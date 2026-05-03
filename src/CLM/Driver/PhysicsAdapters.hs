-- | Adapter functions that plug pure physics modules into the CLM pipeline.
--
-- Each adapter extracts relevant fields from CLMState, builds the physics
-- module's input type, calls the pure function, and packs results back.
--
-- Ported from Julia: soil_temperature.jl (hs computation),
--                    clm_driver.jl (pipeline wiring)
module CLM.Driver.PhysicsAdapters
  ( -- * Heat source term computation (used by soil temperature)
    HeatSourceTerms(..)
  , computeHeatSourceTerms
    -- * Soil temperature adapter
  , soilTemperatureStep
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants
  ( nlevsno, nlevgrnd, nlevsoi, tfrz, sb, denh2o, denice
  , cpice, cpliq, hfus, hvap, hsub )
import CLM.BioGeoPhys.SoilTemperature
  ( SoilTempInput(..), SoilTempOutput(..)
  , solveSoilTemperature, SnowThermalCond(..) )

-- ============================================================================
-- Heat source terms
-- ============================================================================

-- | Column-level heat source terms for soil temperature solver.
-- Computed from weighted patch-level surface fluxes.
data HeatSourceTerms = HeatSourceTerms
  { hst_hs_top   :: !Double  -- ^ Top-layer heat source [W/m2]
  , hst_dhsdT    :: !Double  -- ^ d(hs)/d(T_grnd) [W/m2/K]
  , hst_hs_soil  :: !Double  -- ^ Soil heat source [W/m2]
  , hst_hs_h2osfc :: !Double -- ^ Surface water heat source [W/m2]
  , hst_sabg_lyr  :: !(VU.Vector Double) -- ^ Absorbed solar per layer [W/m2]
  } deriving (Show)

-- | Compute column-level heat source terms from patch-level fluxes.
-- This replicates the internal computation in Julia's soil_temperature!
-- (lines 706-858).
--
-- For each patch on the column, we compute eflx_gnet and accumulate
-- weighted contributions to hs, dhsdT, hs_soil, hs_h2osfc.
computeHeatSourceTerms
  :: Double          -- ^ t_grnd [K]
  -> Double          -- ^ emg (emissivity)
  -> Double          -- ^ forc_lwrad [W/m2]
  -> Double          -- ^ htvp (latent heat: hvap or hsub) [J/kg]
  -> Int             -- ^ snl (snow layers, <= 0)
  -> VU.Vector Double -- ^ t_soisno (full snow+soil, length nlevsno+nlevgrnd)
  -> Double          -- ^ t_h2osfc [K]
  -> Double          -- ^ frac_sno_eff
  -- Per-patch arrays (only patches on this column)
  -> [(Double, Double, Double, Double, Double, Double,
       Double, Double, Double, Double, Double,
       VU.Vector Double)]
     -- ^ List of (wtcol, sabg, sabg_soil, sabg_snow,
     --            dlrad, cgrnd, eflx_sh_grnd, eflx_sh_snow,
     --            eflx_sh_soil, eflx_sh_h2osfc,
     --            qflx_evap_soi, sabg_lyr_patch)
     --   for each non-urban patch on this column
  -> HeatSourceTerms
computeHeatSourceTerms t_grnd emg forc_lwrad htvp snl t_soisno t_h2osfc
                        _frac_sno_eff patches =
  let joff = nlevsno - 1  -- 0-based offset
      -- Longwave emission terms
      lwrad_emit       = emg * sb * t_grnd ** 4
      dlwrad_emit      = 4.0 * emg * sb * t_grnd ** 3
      lyr_top          = snl + 1
      t_top_snow       = t_soisno VU.! (lyr_top + joff)
      t_top_soil       = t_soisno VU.! (joff + 1)  -- soil layer 1
      lwrad_emit_snow  = emg * sb * t_top_snow ** 4
      lwrad_emit_soil  = emg * sb * t_top_soil ** 4
      lwrad_emit_h2osfc = emg * sb * t_h2osfc ** 4

      -- Accumulate patch contributions
      nlyr_sabg = nlevsno + 1  -- sabg_lyr has nlevsno+1 entries
      zero_sabg = VU.replicate nlyr_sabg 0.0

      accum (hs_acc, dhsdT_acc, hs_soil_acc, hs_h2osfc_acc, sabg_lyr_acc)
            (wt, sabg, sabg_soil, sabg_snow, dlrad, cgrnd,
             eflx_sh_grnd, eflx_sh_snow, eflx_sh_soil, eflx_sh_h2osfc,
             qflx_evap_soi, sabg_lyr_p) =
        let -- Ground net flux
            eflx_gnet = sabg + dlrad + emg * forc_lwrad - lwrad_emit
                       - (eflx_sh_grnd + qflx_evap_soi * htvp)
            -- Snow/soil/h2osfc decomposition
            eflx_gnet_soil = sabg_soil + dlrad + emg * forc_lwrad
                           - lwrad_emit_soil
                           - (eflx_sh_soil + qflx_evap_soi * htvp)
                           -- Note: simplified, uses total qflx for soil
            eflx_gnet_h2osfc = sabg_soil + dlrad + emg * forc_lwrad
                             - lwrad_emit_h2osfc
                             - (eflx_sh_h2osfc + qflx_evap_soi * htvp)

            -- Top layer version using sabg_lyr
            eflx_gnet_top = (sabg_lyr_p VU.! 0) + dlrad + emg * forc_lwrad
                          - lwrad_emit
                          - (eflx_sh_grnd + qflx_evap_soi * htvp)

            dgnetdT = -cgrnd - dlwrad_emit

            -- Weighted accumulation
            hs_acc'       = hs_acc + eflx_gnet_top * wt
            dhsdT_acc'    = dhsdT_acc + dgnetdT * wt
            hs_soil_acc'  = hs_soil_acc + eflx_gnet_soil * wt
            hs_h2osfc_acc'= hs_h2osfc_acc + eflx_gnet_h2osfc * wt

            -- Accumulate sabg_lyr weighted by patch
            sabg_lyr_acc' = VU.zipWith (+) sabg_lyr_acc
                            (VU.map (* wt) sabg_lyr_p)

        in (hs_acc', dhsdT_acc', hs_soil_acc', hs_h2osfc_acc', sabg_lyr_acc')

      (hs_total, dhsdT_total, hs_soil_total, hs_h2osfc_total, sabg_lyr_total) =
        foldl accum (0.0, 0.0, 0.0, 0.0, zero_sabg) patches

  in HeatSourceTerms
    { hst_hs_top    = hs_total
    , hst_dhsdT     = dhsdT_total
    , hst_hs_soil   = hs_soil_total
    , hst_hs_h2osfc = hs_h2osfc_total
    , hst_sabg_lyr  = sabg_lyr_total
    }

-- ============================================================================
-- Soil temperature pipeline adapter
-- ============================================================================

-- | Run soil temperature solver for a single column.
-- Takes pre-computed heat source terms and column state.
soilTemperatureStep
  :: HeatSourceTerms  -- ^ Heat source terms from computeHeatSourceTerms
  -> Int              -- ^ snl (snow layer count, <= 0)
  -> VU.Vector Double -- ^ t_soisno (snow+soil temperatures)
  -> Double           -- ^ t_grnd
  -> Double           -- ^ t_h2osfc
  -> VU.Vector Double -- ^ h2osoi_liq
  -> VU.Vector Double -- ^ h2osoi_ice
  -> VU.Vector Double -- ^ dz
  -> VU.Vector Double -- ^ z
  -> VU.Vector Double -- ^ zi
  -> VU.Vector Double -- ^ watsat (soil-only, len nlevgrnd)
  -> VU.Vector Double -- ^ bsw
  -> VU.Vector Double -- ^ sucsat
  -> VU.Vector Double -- ^ tkmg
  -> VU.Vector Double -- ^ tkdry
  -> VU.Vector Double -- ^ csol
  -> VU.Vector Double -- ^ tksatu
  -> Int              -- ^ nbedrock
  -> Double           -- ^ h2osno_no_layers
  -> Double           -- ^ h2osfc
  -> Double           -- ^ frac_sno_eff
  -> Double           -- ^ frac_h2osfc
  -> Double           -- ^ snow_depth
  -> Double           -- ^ dtime
  -> SoilTempOutput
soilTemperatureStep hst snl tSoisno tGrnd tH2osfc
                    h2oLiq h2oIce dz z zi
                    watsat bsw sucsat tkmg tkdry csol tksatu
                    nbedrock h2osnoNL h2osfc fracSE fracH2o
                    snowDep dtime =
  solveSoilTemperature SoilTempInput
    { sti_snl              = snl
    , sti_t_soisno         = tSoisno
    , sti_t_grnd           = tGrnd
    , sti_t_h2osfc         = tH2osfc
    , sti_h2osoi_liq       = h2oLiq
    , sti_h2osoi_ice       = h2oIce
    , sti_dz               = dz
    , sti_z                = z
    , sti_zi               = zi
    , sti_watsat           = watsat
    , sti_bsw              = bsw
    , sti_sucsat           = sucsat
    , sti_tkmg             = tkmg
    , sti_tkdry            = tkdry
    , sti_csol             = csol
    , sti_tksatu           = tksatu
    , sti_nbedrock         = nbedrock
    , sti_h2osno_no_layers = h2osnoNL
    , sti_h2osfc           = h2osfc
    , sti_frac_sno_eff     = fracSE
    , sti_frac_h2osfc      = fracH2o
    , sti_snow_depth       = snowDep
    , sti_hs_top           = hst_hs_top hst
    , sti_dhsdT            = hst_dhsdT hst
    , sti_hs_soil          = hst_hs_soil hst
    , sti_hs_h2osfc        = hst_hs_h2osfc hst
    , sti_sabg_lyr         = hst_sabg_lyr hst
    , sti_eflx_bot         = 0.0
    , sti_dtime            = dtime
    , sti_snowCondMethod   = Jordan1991
    }
