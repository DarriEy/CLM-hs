-- | Glacier surface mass balance.
-- Ported from Fortran: src/biogeophys/GlacierSurfaceMassBalanceMod.F90
-- (and cross-checked against CLM.jl src/biogeophys/glacier_surface_mass_balance.jl).
--
-- Computes the fluxes specific to glacier (land-ice, @istice@) columns:
--
--   * HandleIceMelt — in glacier columns, any liquid meltwater in a ground layer
--     is treated as ice melt: accumulated into @qflx_glcice_melt@ and converted
--     back to pure ice by "borrowing" an equal ice mass from below the column
--     (@ice += liq; liq = 0@). This keeps the glacier surface ice-covered while
--     letting the meltwater run off; the borrowing is reconciled in the runoff.
--   * ComputeSurfaceMassBalance — ice growth (@frz@) equals the snow-capping flux
--     wherever snow has persisted long enough (glacial inception) or the column is
--     already @istice@; the net glacial-ice flux is @frz - melt@.
--   * AdjustRunoffTerms — ice melt is added to liquid runoff; ice runoff is
--     reduced by the dynamically-coupled capped snow and (on the uncoupled
--     fraction) by one unit per unit of melt, correcting the accumulation/melt
--     double-counting (conserves mass and energy).
--
-- This is a single-column port. @glc_dyn_runoff_routing@ is the gridcell fraction
-- coupled to a dynamic ice sheet; with no glc2lnd coupling it is 0, reproducing
-- standalone-CLM behaviour exactly.
module CLM.BioGeoPhys.GlacierSurfaceMassBalance
  ( -- * Constants
    istice
  , glcSnowPersistenceMaxDays
  , secspday
    -- * Surface mass balance
  , GlacierSMBInput(..)
  , GlacierSMBOutput(..)
  , glacierSurfaceMassBalance
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Land-ice (glacier) landunit type (Fortran @istice@).
istice :: Int
istice = 4

-- | Default snow-persistence threshold for glacial inception [days]
-- (Fortran @glc_snow_persistence_max_days@).
glcSnowPersistenceMaxDays :: Int
glcSnowPersistenceMaxDays = 7300

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | Inputs for one column's glacier surface mass balance. @h2osoi_*@ are the
-- combined snow+soil layer vectors (length @nlevsno + nlevgrnd@); only the soil
-- layers (indices @nlevsno ..@) participate in ice melt.
data GlacierSMBInput = GlacierSMBInput
  { gsmbi_landunit_itype         :: !Int                -- ^ Column landunit type (glacier = 'istice')
  , gsmbi_do_smb                 :: !Bool               -- ^ Column is in the do-SMB filter
  , gsmbi_h2osoi_liq             :: !(VU.Vector Double) -- ^ Liquid water per layer [kg/m2]
  , gsmbi_h2osoi_ice             :: !(VU.Vector Double) -- ^ Ice per layer [kg/m2]
  , gsmbi_snow_persistence       :: !Double             -- ^ Length of time snow-covered [s]
  , gsmbi_qflx_snwcp_ice         :: !Double             -- ^ Excess solid H2O from snow capping [mm H2O/s]
  , gsmbi_qflx_qrgwl             :: !Double             -- ^ Initial liquid runoff (glacier/wetland/lake) [mm H2O/s]
  , gsmbi_qflx_ice_runoff_snwcp  :: !Double             -- ^ Initial solid runoff from snow capping [mm H2O/s]
  , gsmbi_glc_dyn_runoff_routing :: !Double             -- ^ Gridcell fraction coupled to a dynamic ice sheet (0 = standalone)
  , gsmbi_dtime                  :: !Double             -- ^ Timestep [s]
  } deriving (Show)

-- | Outputs: updated water state plus the glacier flux diagnostics.
data GlacierSMBOutput = GlacierSMBOutput
  { gsmbo_h2osoi_liq                 :: !(VU.Vector Double) -- ^ Updated liquid water (meltwater removed) [kg/m2]
  , gsmbo_h2osoi_ice                 :: !(VU.Vector Double) -- ^ Updated ice (meltwater converted to ice) [kg/m2]
  , gsmbo_qflx_glcice_melt           :: !Double             -- ^ Ice melt, positive definite [mm H2O/s]
  , gsmbo_qflx_glcice_frz            :: !Double             -- ^ Ice growth, positive definite [mm H2O/s]
  , gsmbo_qflx_glcice                :: !Double             -- ^ Net new glacial ice = frz - melt [mm H2O/s]
  , gsmbo_qflx_glcice_dyn_water_flux :: !Double             -- ^ Water-balance term for dynamic routing [mm H2O/s]
  , gsmbo_qflx_qrgwl                 :: !Double             -- ^ Adjusted liquid runoff [mm H2O/s]
  , gsmbo_qflx_ice_runoff_snwcp      :: !Double             -- ^ Adjusted solid runoff [mm H2O/s]
  } deriving (Show)

-- | Run HandleIceMelt -> ComputeSurfaceMassBalance -> AdjustRunoffTerms for one
-- column. Columns outside the do-SMB filter pass through unchanged with zero
-- glacier fluxes.
glacierSurfaceMassBalance :: Int -> Int -> GlacierSMBInput -> GlacierSMBOutput
glacierSurfaceMassBalance nlevsno nlevgrnd inp
  | not (gsmbi_do_smb inp) = GlacierSMBOutput
      { gsmbo_h2osoi_liq                 = gsmbi_h2osoi_liq inp
      , gsmbo_h2osoi_ice                 = gsmbi_h2osoi_ice inp
      , gsmbo_qflx_glcice_melt           = 0.0
      , gsmbo_qflx_glcice_frz            = 0.0
      , gsmbo_qflx_glcice                = 0.0
      , gsmbo_qflx_glcice_dyn_water_flux = 0.0
      , gsmbo_qflx_qrgwl                 = gsmbi_qflx_qrgwl inp
      , gsmbo_qflx_ice_runoff_snwcp      = gsmbi_qflx_ice_runoff_snwcp inp
      }
  | otherwise = GlacierSMBOutput
      { gsmbo_h2osoi_liq                 = liq'
      , gsmbo_h2osoi_ice                 = ice'
      , gsmbo_qflx_glcice_melt           = melt
      , gsmbo_qflx_glcice_frz            = frz
      , gsmbo_qflx_glcice                = frz - melt
      , gsmbo_qflx_glcice_dyn_water_flux = routing * (melt - frz)
      , gsmbo_qflx_qrgwl                 = gsmbi_qflx_qrgwl inp + melt
      , gsmbo_qflx_ice_runoff_snwcp      =
          gsmbi_qflx_ice_runoff_snwcp inp
            - routing * frz
            - (1.0 - routing) * melt
      }
  where
    dt      = gsmbi_dtime inp
    routing = gsmbi_glc_dyn_runoff_routing inp
    isGlacier = gsmbi_landunit_itype inp == istice

    -- HandleIceMelt: convert soil-layer meltwater back to ice (istice only),
    -- accumulating the melt flux. Soil layers are combined indices nlevsno..
    soilIdx = [nlevsno .. nlevsno + nlevgrnd - 1]
    (liq', ice', melt)
      | isGlacier = foldl meltLayer (gsmbi_h2osoi_liq inp, gsmbi_h2osoi_ice inp, 0.0) soilIdx
      | otherwise = (gsmbi_h2osoi_liq inp, gsmbi_h2osoi_ice inp, 0.0)

    meltLayer (l, i, m) jj =
      let lj = l VU.! jj
      in if lj > 0.0
         then ( l VU.// [(jj, 0.0)]
              , i VU.// [(jj, (i VU.! jj) + lj)]
              , m + lj / dt )
         else (l, i, m)

    -- ComputeSurfaceMassBalance: ice growth from snow capping at glacial
    -- inception (long-persistent snow) or on existing glacier columns.
    frz | gsmbi_snow_persistence inp >= fromIntegral glcSnowPersistenceMaxDays * secspday
            || isGlacier = gsmbi_qflx_snwcp_ice inp
        | otherwise      = 0.0
