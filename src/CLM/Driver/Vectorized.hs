-- | Array-vectorized (Structure-of-Arrays) model state and physics, FOUNDATION.
--
-- The single-column 'CLMState' stores one column's fields as scalars
-- (@t_grnd_col :: Double@, …). True CLM/CTSM (and the Julia port and any GPU
-- backend) instead hold N columns at once as Structure-of-Arrays: each
-- per-column scalar becomes a @VU.Vector Double@ indexed by column, and each
-- per-(column,layer) field is flattened row-major as @c*nlev + j@ (the same
-- layout 'CLM.Infrastructure.SubgridAverage' already assumes, so the @c2g@
-- roll-up interoperates for free).
--
-- This module is the migration-safe foundation for that rewrite. Rather than
-- mutate 'CLMState' in place (which would break every adapter at once), the
-- vectorized state ('CLMStateV') COEXISTS with the scalar one, bridged by
-- 'gather' (['CLMState'] -> SoA) and 'scatter' (overlay SoA back onto a
-- ['CLMState'] base). That bridge gives a free correctness ORACLE: for any
-- vectorized step @fV@ and its scalar counterpart @f@,
--
-- @
--   scatter sts (fV cfg ctx (gather sts))  ==  map (f cfg ctx) sts
-- @
--
-- must hold bit-for-bit. Vectorization therefore changes only STORAGE and
-- ITERATION (SoA arrays + 'VU.zipWith'/'VU.generate'), reusing the existing
-- per-column physics KERNELS unchanged — so each step is bit-identical by
-- construction and verifiable independently. 'CLMStateV' grows one field group
-- at a time as subsystems are migrated; this first slice covers the
-- water-balance and hydrology-drainage steps.
module CLM.Driver.Vectorized
  ( -- * Vectorized state
    CLMStateV(..)
  , gather
  , scatter
    -- * Vectorized physics steps (SoA counterparts of the scalar adapters)
  , waterBalanceStepV
  , hydrologyDrainageStepV
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Driver.CLMDriver       (CLMState(..), TimestepContext(..))
import CLM.Constants.ControlFlags  (CLMDriverConfig)
import CLM.Types.WaterFluxData    (WaterFluxData(..))
import CLM.Types.WaterBalanceData (WaterBalanceData(..))
import CLM.BioGeoPhys.BalanceCheck
  ( waterBalanceCol, WaterBalanceColInput(..), WaterBalanceColOutput(..) )
import CLM.BioGeoPhys.HydrologyDrainage
  ( computeTotalRunoff, TotalRunoffInput(..), TotalRunoffResult(..) )

-- | Structure-of-Arrays model state: one entry per column in every vector.
-- Grows as subsystems are vectorized; this slice carries the fields the
-- water-balance and hydrology-drainage steps read and write.
data CLMStateV = CLMStateV
  { vNumCols       :: !Int                 -- ^ Number of columns (length of every vector below)
  , vqflx_evap_tot :: !(VU.Vector Double)  -- ^ Total evapotranspiration [mm/s] (read)
  , vqflx_surf     :: !(VU.Vector Double)  -- ^ Surface runoff [mm/s] (read+write)
  , vqflx_drain    :: !(VU.Vector Double)  -- ^ Sub-surface drainage [mm/s] (read)
  , vwb_errh2o     :: !(VU.Vector Double)  -- ^ Water conservation error [mm] (write)
  } deriving (Eq, Show)

-- | Project a list of single-column 'CLMState's (column order = list order)
-- into the SoA 'CLMStateV'. Pure; the inverse-on-tracked-fields is 'scatter'.
gather :: [CLMState] -> CLMStateV
gather sts = CLMStateV
  { vNumCols       = length sts
  , vqflx_evap_tot = VU.fromList [ qflx_evap_tot_patch (clmWaterFlux s)    | s <- sts ]
  , vqflx_surf     = VU.fromList [ qflx_surf_col       (clmWaterFlux s)    | s <- sts ]
  , vqflx_drain    = VU.fromList [ qflx_drain_col      (clmWaterFlux s)    | s <- sts ]
  , vwb_errh2o     = VU.fromList [ errh2oOf s                              | s <- sts ]
  }
  where
    errh2oOf s = let v = wb_errh2o_col (clmWaterBalance s)
                 in if VU.null v then 0.0 else v VU.! 0

-- | Overlay the SoA state's tracked fields back onto a per-column base list
-- (one base 'CLMState' per column, same order/length as the 'gather'ed list),
-- leaving every untracked field of each base untouched. Together with 'gather'
-- this is the migration bridge and the equivalence oracle.
scatter :: [CLMState] -> CLMStateV -> [CLMState]
scatter bases v =
  [ s { clmWaterFlux    = (clmWaterFlux s)
          { qflx_evap_tot_patch = vqflx_evap_tot v VU.! i
          , qflx_surf_col       = vqflx_surf     v VU.! i
          , qflx_drain_col      = vqflx_drain    v VU.! i }
      , clmWaterBalance = (clmWaterBalance s)
          { wb_errh2o_col = VU.singleton (vwb_errh2o v VU.! i) }
      }
  | (i, s) <- zip [0 ..] bases ]

-- | Vectorized counterpart of 'CLM.Driver.PhysicsAdapters.waterBalanceStep':
-- reuses the 'waterBalanceCol' kernel across all columns via 'VU.zipWith3' on
-- the SoA arrays. Bit-identical to the per-column scalar step by construction
-- (the per-column input record below mirrors the scalar adapter exactly).
waterBalanceStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
waterBalanceStepV _cfg ctx v =
  let dtime     = tcDtime ctx
      forc_rain = if VU.null (tcForcRain ctx) then 0.0 else tcForcRain ctx VU.! 0
      forc_snow = if VU.null (tcForcSnow ctx) then 0.0 else tcForcSnow ctx VU.! 0
      mk evap surf drain = WaterBalanceColInput
        { wbci_endwb               = 0.0
        , wbci_begwb               = 0.0
        , wbci_forc_rain           = forc_rain
        , wbci_forc_snow           = forc_snow
        , wbci_qflx_flood          = 0.0
        , wbci_qflx_sfc_irrig      = 0.0
        , wbci_qflx_glcice_dyn     = 0.0
        , wbci_qflx_evap_tot       = evap
        , wbci_qflx_surf           = surf
        , wbci_qflx_qrgwl          = 0.0
        , wbci_qflx_drain          = drain
        , wbci_qflx_drain_perch    = 0.0
        , wbci_qflx_ice_runoff     = 0.0
        , wbci_qflx_snwcp_disc_liq = 0.0
        , wbci_qflx_snwcp_disc_ice = 0.0
        , wbci_dtime               = dtime
        , wbci_is_active           = True
        }
      errh2o = VU.zipWith3
        (\evap surf drain -> wbco_errh2o (waterBalanceCol (mk evap surf drain)))
        (vqflx_evap_tot v) (vqflx_surf v) (vqflx_drain v)
  in v { vwb_errh2o = errh2o }

-- | Vectorized counterpart of
-- 'CLM.Driver.PhysicsAdapters.hydrologyDrainageStep': total runoff per column
-- via 'computeTotalRunoff', reusing the kernel across the SoA arrays. The
-- read-then-write of @qflx_surf@ is handled functionally (the new vector is
-- built from the old), so there is no aliasing. Bit-identical to the scalar
-- step (single soil column ⇒ @lun_itype = istsoil@, non-urban).
hydrologyDrainageStepV :: CLMDriverConfig -> TimestepContext -> CLMStateV -> CLMStateV
hydrologyDrainageStepV _cfg _ctx v =
  let surf' = VU.zipWith
        (\drain surf -> trr_qflx_runoff (computeTotalRunoff TotalRunoffInput
          { tri_qflx_drain         = drain
          , tri_qflx_surf          = surf
          , tri_qflx_qrgwl         = 0.0
          , tri_qflx_drain_perched = 0.0
          , tri_lun_itype          = 1      -- istsoil
          , tri_urbpoi             = False
          }))
        (vqflx_drain v) (vqflx_surf v)
  in v { vqflx_surf = surf' }
