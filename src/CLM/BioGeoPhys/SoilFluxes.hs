{-# LANGUAGE BangPatterns #-}
-- | Soil heat and water flux calculations.
-- Fortran: SoilFluxesMod.F90
--
-- Provides:
--   * Ground temperature correction (tinc)
--   * Sensible/latent heat flux corrections
--   * Evaporation partitioning (liquid/solid)
--   * Snow evaporation constraint
--   * Ground heat flux and energy balance error (errsoi)
--   * Outgoing longwave radiation
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SoilFluxes
  ( -- * Input/output records
    SoilFluxesInput(..)
  , SoilFluxesResult(..)
    -- * Science functions
  , computeGroundTempDiff
  , correctFluxes
  , partitionEvaporation
  , constrainSnowEvaporation
  , computeGroundHeatFlux
  , computeErrSoi
  , computeOutgoingLW
  , soilFluxes
  ) where

import CLM.Constants.PhysicalConstants (tfrz, sb, hvap, nlevsno, nlevsoi, nlevgrnd)

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Number of urban levels
nlevurb :: Int
nlevurb = 5

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Comprehensive input for soil fluxes (single patch)
data SoilFluxesInput = SoilFluxesInput
  { -- Column-level
    sfi_snl              :: !Int              -- ^ Number of snow layers (negative)
  , sfi_frac_sno_eff     :: !Double           -- ^ Effective snow fraction
  , sfi_frac_h2osfc      :: !Double           -- ^ Surface water fraction
  , sfi_t_grnd           :: !Double           -- ^ Ground temperature [K]
  , sfi_t_h2osfc         :: !Double           -- ^ Surface water temperature [K]
  , sfi_t_h2osfc_bef     :: !Double           -- ^ Previous surface water temperature [K]
  , sfi_c_h2osfc         :: !Double           -- ^ Surface water heat capacity
  , sfi_xmf              :: !Double           -- ^ Phase change heat flux
  , sfi_xmf_h2osfc       :: !Double           -- ^ Surface water phase change
  , sfi_emg              :: !Double           -- ^ Ground emissivity
  , sfi_forc_lwrad       :: !Double           -- ^ Downward longwave [W/m2]
  , sfi_htvp             :: !Double           -- ^ Latent heat of vaporization [J/kg]
  , sfi_t_ssbef          :: !(VU.Vector Double)  -- ^ Pre-step temperature (snow+soil)
  , sfi_t_soisno         :: !(VU.Vector Double)  -- ^ Current temperature (snow+soil)
  , sfi_fact             :: !(VU.Vector Double)  -- ^ Temperature factor (snow+soil)
  , sfi_h2osoi_liq       :: !(VU.Vector Double)  -- ^ Liquid water (snow+soil)
  , sfi_h2osoi_ice       :: !(VU.Vector Double)  -- ^ Ice water (snow+soil)
    -- Patch-level
  , sfi_frac_veg_nosno   :: !Int              -- ^ Vegetation fraction flag
  , sfi_eflx_sh_grnd     :: !Double           -- ^ Sensible heat from ground [W/m2]
  , sfi_eflx_sh_veg      :: !Double           -- ^ Sensible heat from vegetation [W/m2]
  , sfi_eflx_sh_stem     :: !Double           -- ^ Sensible heat from stem [W/m2]
  , sfi_cgrnds           :: !Double           -- ^ Ground SH conductance
  , sfi_cgrndl           :: !Double           -- ^ Ground LH conductance
  , sfi_qflx_evap_soi    :: !Double           -- ^ Soil evaporation [mm/s]
  , sfi_qflx_evap_veg    :: !Double           -- ^ Vegetation evaporation [mm/s]
  , sfi_qflx_tran_veg    :: !Double           -- ^ Transpiration [mm/s]
  , sfi_qflx_ev_snow     :: !Double           -- ^ Snow evaporation [mm/s]
  , sfi_qflx_ev_soil     :: !Double           -- ^ Soil bare evaporation [mm/s]
  , sfi_qflx_ev_h2osfc   :: !Double           -- ^ Surface water evaporation [mm/s]
  , sfi_sabg_soil         :: !Double           -- ^ Absorbed solar on soil [W/m2]
  , sfi_sabg_snow         :: !Double           -- ^ Absorbed solar on snow [W/m2]
  , sfi_sabg              :: !Double           -- ^ Total absorbed solar [W/m2]
  , sfi_dlrad             :: !Double           -- ^ Downward LW from canopy [W/m2]
  , sfi_ulrad             :: !Double           -- ^ Upward LW from canopy [W/m2]
  , sfi_eflx_lwrad_net    :: !Double           -- ^ Net LW radiation [W/m2]
    -- Urban/landunit
  , sfi_is_urban          :: !Bool
  , sfi_is_soil_or_crop   :: !Bool
  , sfi_col_itype         :: !Int              -- ^ Column type
    -- Flags for urban extras
  , sfi_eflx_wasteheat    :: !Double
  , sfi_eflx_heat_from_ac :: !Double
  , sfi_eflx_traffic      :: !Double
  , sfi_eflx_ventilation  :: !Double
  , sfi_eflx_building_heat_errsoi :: !Double
  , sfi_eflx_h2osfc_to_snow :: !Double
    -- Time
  , sfi_dtime             :: !Double
  } deriving (Show)

-- | Result of soil fluxes computation
data SoilFluxesResult = SoilFluxesResult
  { sfr_eflx_sh_grnd       :: !Double  -- ^ Updated sensible heat from ground [W/m2]
  , sfr_eflx_sh_tot        :: !Double  -- ^ Total sensible heat [W/m2]
  , sfr_qflx_evap_soi      :: !Double  -- ^ Updated soil evaporation [mm/s]
  , sfr_qflx_evap_tot      :: !Double  -- ^ Total evaporation [mm/s]
  , sfr_qflx_evap_can      :: !Double  -- ^ Canopy evaporation [mm/s]
  , sfr_eflx_lh_tot        :: !Double  -- ^ Total latent heat [W/m2]
  , sfr_eflx_lh_vege       :: !Double  -- ^ Veg canopy evaporation LH [W/m2]
  , sfr_eflx_lh_vegt       :: !Double  -- ^ Veg transpiration LH [W/m2]
  , sfr_eflx_lh_grnd       :: !Double  -- ^ Ground evaporation LH [W/m2]
  , sfr_eflx_soil_grnd     :: !Double  -- ^ Ground heat flux [W/m2]
  , sfr_eflx_lwrad_out     :: !Double  -- ^ Outgoing LW radiation [W/m2]
  , sfr_eflx_lwrad_net     :: !Double  -- ^ Net LW radiation [W/m2]
  , sfr_errsoi             :: !Double  -- ^ Soil energy balance error [W/m2]
  , sfr_qflx_ev_snow       :: !Double  -- ^ Updated snow evaporation [mm/s]
  , sfr_qflx_ev_soil       :: !Double  -- ^ Updated soil evaporation [mm/s]
  , sfr_qflx_ev_h2osfc     :: !Double  -- ^ Updated h2osfc evaporation [mm/s]
  , sfr_qflx_liqevap       :: !Double  -- ^ Liquid evaporation from top layer [mm/s]
  , sfr_qflx_solidevap     :: !Double  -- ^ Solid evaporation from top layer [mm/s]
  , sfr_qflx_soliddew      :: !Double  -- ^ Solid dew deposition [mm/s]
  , sfr_qflx_liqdew        :: !Double  -- ^ Liquid dew deposition [mm/s]
  , sfr_t_skin             :: !Double  -- ^ Skin temperature [K]
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute temperature difference for flux corrections.
-- Ported from Loop 1 in SoilFluxes.
computeGroundTempDiff :: SoilFluxesInput -> Double
computeGroundTempDiff inp =
  let snl_val = sfi_snl inp
      fse = sfi_frac_sno_eff inp
      fh2 = sfi_frac_h2osfc inp
      tss = sfi_t_ssbef inp
      t_h2osfc_bef = sfi_t_h2osfc_bef inp
      t_grnd_val = sfi_t_grnd inp
      joff = nlevsno
      t_grnd0 = if snl_val < 0
                then fse * vix tss (snl_val + 1 + joff)
                   + (1.0 - fse - fh2) * vix tss (1 + joff)
                   + fh2 * t_h2osfc_bef
                else (1.0 - fh2) * vix tss (1 + joff)
                   + fh2 * t_h2osfc_bef
  in t_grnd_val - t_grnd0

-- | Correct sensible and latent heat fluxes for temperature change.
-- Ported from Loop 2 in SoilFluxes.
correctFluxes
  :: Double  -- ^ tinc
  -> Double  -- ^ cgrnds
  -> Double  -- ^ cgrndl
  -> Double  -- ^ eflx_sh_grnd
  -> Double  -- ^ qflx_evap_soi
  -> (Double, Double)  -- ^ (eflx_sh_grnd', qflx_evap_soi')
correctFluxes tinc cgrnds cgrndl sh_grnd evap_soi =
  ( sh_grnd + tinc * cgrnds
  , evap_soi + tinc * cgrndl
  )

-- | Partition evaporation into liquid and solid components.
-- Ported from Loop 3 in SoilFluxes.
partitionEvaporation
  :: Bool    -- ^ is_urban
  -> Double  -- ^ t_grnd
  -> Double  -- ^ qflx_ev_snow
  -> Double  -- ^ qflx_evap_soi (for urban)
  -> Double  -- ^ h2o_liq (top layer)
  -> Double  -- ^ h2o_ice (top layer)
  -> (Double, Double, Double, Double)  -- ^ (liqevap, solidevap, soliddew, liqdew)
partitionEvaporation isUrban t_grnd_val ev_snow evap_soi h2o_liq h2o_ice =
  let ev_val = if isUrban then evap_soi else ev_snow
  in if ev_val >= 0.0
     then let liqevap = if (h2o_liq + h2o_ice) > 0.0
                        then max (ev_val * h2o_liq / (h2o_liq + h2o_ice)) 0.0
                        else 0.0
              solidevap = ev_val - liqevap
          in (liqevap, solidevap, 0.0, 0.0)
     else if t_grnd_val < tfrz
          then (0.0, 0.0, abs ev_val, 0.0)
          else (0.0, 0.0, 0.0, abs ev_val)

-- | Constrain evaporation from snow to available moisture.
-- Ported from Loop 4 in SoilFluxes.
constrainSnowEvaporation
  :: Int     -- ^ j_top (Fortran top layer index)
  -> Double  -- ^ frac_sno_eff
  -> Double  -- ^ frac_h2osfc
  -> Double  -- ^ dtime
  -> Double  -- ^ htvp
  -> Double  -- ^ h2o_liq
  -> Double  -- ^ h2o_ice
  -> Double  -- ^ qflx_ev_snow
  -> Double  -- ^ qflx_evap_soi
  -> Double  -- ^ eflx_sh_grnd
  -> Double  -- ^ qflx_liqevap
  -> Double  -- ^ qflx_solidevap
  -> Bool    -- ^ is_urban
  -> Int     -- ^ col_itype
  -> (Double, Double, Double, Double, Double)
     -- ^ (ev_snow', evap_soi', sh_grnd', liqevap', solidevap')
constrainSnowEvaporation j_top fse fh2 dtime_val htvp_val
                         h2o_liq h2o_ice ev_snow evap_soi sh_grnd
                         liqevap solidevap _isUrban _colType =
  let -- Snow layer constraint
      (ev_snow1, evap_soi1, sh_grnd1, liqevap1, solidevap1) =
        if j_top < 1 && ev_snow > (h2o_ice + h2o_liq) / (fse * dtime_val)
        then let evlim = (h2o_ice + h2o_liq) / (fse * dtime_val)
                 demand = ev_snow
             in ( evlim
                , evap_soi - fse * (demand - evlim)
                , sh_grnd + fse * (demand - evlim) * htvp_val
                , max (h2o_liq / (fse * dtime_val)) 0.0
                , max (h2o_ice / (fse * dtime_val)) 0.0
                )
        else (ev_snow, evap_soi, sh_grnd, liqevap, solidevap)

      -- Solid evaporation constraint for top soil layer
      (liqevap2, solidevap2) =
        if j_top == 1 && fh2 < 1.0
        then let evlim_ice = h2o_ice / (dtime_val * (1.0 - fh2))
             in if solidevap1 >= evlim_ice
                then ( liqevap1 + (solidevap1 - evlim_ice)
                     , evlim_ice
                     )
                else (liqevap1, solidevap1)
        else (liqevap1, solidevap1)

  in (ev_snow1, evap_soi1, sh_grnd1, liqevap2, solidevap2)

-- | Compute ground heat flux (non-urban).
-- Ported from Loop 5 in SoilFluxes.
computeGroundHeatFlux
  :: SoilFluxesInput
  -> Double  -- ^ tinc
  -> Double  -- ^ t_grnd0 (computed from t_ssbef)
  -> Double  -- ^ eflx_sh_grnd (corrected)
  -> Double  -- ^ qflx_evap_soi (corrected)
  -> Double  -- ^ ground heat flux [W/m2]
computeGroundHeatFlux inp tinc t_grnd0 sh_grnd evap_soi =
  let fse = sfi_frac_sno_eff inp
      fh2 = sfi_frac_h2osfc inp
      tss = sfi_t_ssbef inp
      joff = nlevsno
      snl_val = sfi_snl inp
      t_h2osfc_bef = sfi_t_h2osfc_bef inp

      lw_grnd = fse * vix tss (snl_val + 1 + joff) ** 4
              + (1.0 - fse - fh2) * vix tss (1 + joff) ** 4
              + fh2 * t_h2osfc_bef ** 4

  in ((1.0 - fse) * sfi_sabg_soil inp + fse * sfi_sabg_snow inp)
   + sfi_dlrad inp
   + fromIntegral (1 - sfi_frac_veg_nosno inp) * sfi_emg inp * sfi_forc_lwrad inp
   - sfi_emg inp * sb * lw_grnd
   - sfi_emg inp * sb * t_grnd0 ** 3 * (4.0 * tinc)
   - (sh_grnd + evap_soi * sfi_htvp inp)

-- | Compute soil energy balance error.
-- Ported from Loop 6 in SoilFluxes.
computeErrSoi
  :: SoilFluxesInput
  -> Double  -- ^ eflx_soil_grnd
  -> Double  -- ^ errsoi [W/m2]
computeErrSoi inp eflx_soil_grnd_val =
  let dtime_val = sfi_dtime inp
      base = eflx_soil_grnd_val
           - sfi_xmf inp - sfi_xmf_h2osfc inp
           - sfi_frac_h2osfc inp
             * (sfi_t_h2osfc inp - sfi_t_h2osfc_bef inp)
             * (sfi_c_h2osfc inp / dtime_val)
           + sfi_eflx_h2osfc_to_snow inp

      -- Add building heat for urban wall/roof columns
      base2 = if sfi_col_itype inp `elem` [61, 62, 65]  -- ICOL_SUNWALL, ICOL_SHADEWALL, ICOL_ROOF
              then base + sfi_eflx_building_heat_errsoi inp
              else base

      -- Subtract layer heat storage changes
      joff = nlevsno
      fse = sfi_frac_sno_eff inp
      t_soisno_v = sfi_t_soisno inp
      t_ssbef_v = sfi_t_ssbef inp
      fact_v = sfi_fact inp

      isUrbanWallRoof = sfi_col_itype inp `elem` [61, 62, 65]
      nlev = if isUrbanWallRoof then nlevurb else nlevgrnd

      snl_val = sfi_snl inp

      -- Snow layer contributions
      errSnow = sum [ fse * (vix t_soisno_v (jf + joff) - vix t_ssbef_v (jf + joff))
                        / vix fact_v (jf + joff)
                    | jf <- [snl_val + 1 .. 0]
                    , jf >= snl_val + 1, jf < 1
                    ]

      -- Soil layer contributions
      errSoil = sum [ (vix t_soisno_v (jf + joff) - vix t_ssbef_v (jf + joff))
                        / vix fact_v (jf + joff)
                    | jf <- [1 .. nlev]
                    ]

  in base2 - errSnow - errSoil

-- | Compute outgoing longwave radiation.
-- Ported from Loop 7 in SoilFluxes.
computeOutgoingLW
  :: SoilFluxesInput
  -> Double  -- ^ tinc
  -> Double  -- ^ t_grnd0
  -> Double  -- ^ eflx_lwrad_out [W/m2]
computeOutgoingLW inp tinc t_grnd0 =
  let fse = sfi_frac_sno_eff inp
      fh2 = sfi_frac_h2osfc inp
      tss = sfi_t_ssbef inp
      joff = nlevsno
      snl_val = sfi_snl inp
      t_h2osfc_bef = sfi_t_h2osfc_bef inp

      lw_grnd = fse * vix tss (snl_val + 1 + joff) ** 4
              + (1.0 - fse - fh2) * vix tss (1 + joff) ** 4
              + fh2 * t_h2osfc_bef ** 4

  in sfi_ulrad inp
   + fromIntegral (1 - sfi_frac_veg_nosno inp) * (1.0 - sfi_emg inp) * sfi_forc_lwrad inp
   + fromIntegral (1 - sfi_frac_veg_nosno inp) * sfi_emg inp * sb * lw_grnd
   + 4.0 * sfi_emg inp * sb * t_grnd0 ** 3 * tinc

-- | Full soil fluxes computation for a single patch.
-- Ported from SoilFluxes in SoilFluxesMod.F90
soilFluxes :: SoilFluxesInput -> SoilFluxesResult
soilFluxes inp =
  let -- Step 1: Temperature difference
      tinc = computeGroundTempDiff inp
      joff = nlevsno
      snl_val = sfi_snl inp
      fse = sfi_frac_sno_eff inp
      fh2 = sfi_frac_h2osfc inp

      -- Compute t_grnd0
      t_grnd0 = if snl_val < 0
                then fse * vix (sfi_t_ssbef inp) (snl_val + 1 + joff)
                   + (1.0 - fse - fh2) * vix (sfi_t_ssbef inp) (1 + joff)
                   + fh2 * sfi_t_h2osfc_bef inp
                else (1.0 - fh2) * vix (sfi_t_ssbef inp) (1 + joff)
                   + fh2 * sfi_t_h2osfc_bef inp

      -- Step 2: Correct fluxes
      (sh_grnd1, evap_soi1) = correctFluxes tinc
                                 (sfi_cgrnds inp) (sfi_cgrndl inp)
                                 (sfi_eflx_sh_grnd inp) (sfi_qflx_evap_soi inp)

      -- Update ev_snow/ev_soil/ev_h2osfc for non-urban
      ev_snow1 = if sfi_is_urban inp then sfi_qflx_ev_snow inp
                 else sfi_qflx_ev_snow inp + tinc * sfi_cgrndl inp
      ev_soil1 = if sfi_is_urban inp then 0.0
                 else sfi_qflx_ev_soil inp + tinc * sfi_cgrndl inp
      ev_h2osfc1 = if sfi_is_urban inp then 0.0
                   else sfi_qflx_ev_h2osfc inp + tinc * sfi_cgrndl inp

      -- Step 3: Partition evaporation
      j_jl = snl_val + 1 + joff
      h2o_liq_top = vix (sfi_h2osoi_liq inp) j_jl
      h2o_ice_top = vix (sfi_h2osoi_ice inp) j_jl

      (liqevap0, solidevap0, soliddew0, liqdew0) =
        partitionEvaporation (sfi_is_urban inp) (sfi_t_grnd inp)
          ev_snow1 evap_soi1 h2o_liq_top h2o_ice_top

      -- Step 4: Constrain snow evaporation
      j_top = snl_val + 1
      (ev_snow2, evap_soi2, sh_grnd2, liqevap1, solidevap1) =
        constrainSnowEvaporation j_top fse fh2 (sfi_dtime inp) (sfi_htvp inp)
          h2o_liq_top h2o_ice_top ev_snow1 evap_soi1 sh_grnd1
          liqevap0 solidevap0 (sfi_is_urban inp) (sfi_col_itype inp)

      -- Step 5: Ground heat flux and total fluxes
      eflx_soil_grnd_val = if not (sfi_is_urban inp)
                           then computeGroundHeatFlux inp tinc t_grnd0 sh_grnd2 evap_soi2
                           else sfi_sabg inp + sfi_dlrad inp
                              - sfi_eflx_lwrad_net inp
                              - 4.0 * sfi_emg inp * sb * t_grnd0 ** 3 * tinc
                              - (sh_grnd2 + evap_soi2 * sfi_htvp inp
                                 + sfi_qflx_tran_veg inp * hvap)
                              + sfi_eflx_wasteheat inp + sfi_eflx_heat_from_ac inp
                              + sfi_eflx_traffic inp + sfi_eflx_ventilation inp

      sh_tot = sfi_eflx_sh_veg inp + sh_grnd2
             + if not (sfi_is_urban inp) then sfi_eflx_sh_stem inp else 0.0
      evap_tot = sfi_qflx_evap_veg inp + evap_soi2
      lh_tot = hvap * sfi_qflx_evap_veg inp + sfi_htvp inp * evap_soi2

      evap_can = sfi_qflx_evap_veg inp - sfi_qflx_tran_veg inp
      lh_vege = evap_can * hvap
      lh_vegt = sfi_qflx_tran_veg inp * hvap
      lh_grnd = evap_soi2 * sfi_htvp inp

      -- Step 6: Energy balance error
      errsoi_val = computeErrSoi inp eflx_soil_grnd_val

      -- Step 7: Outgoing LW
      lwrad_out = if not (sfi_is_urban inp)
                  then computeOutgoingLW inp tinc t_grnd0
                  else sfi_eflx_lwrad_net inp + sfi_forc_lwrad inp
                     + 4.0 * sfi_emg inp * sb * t_grnd0 ** 3 * tinc

      lwrad_net = lwrad_out - sfi_forc_lwrad inp

      -- Skin temperature for bare ground
      lw_grnd = fse * vix (sfi_t_ssbef inp) (snl_val + 1 + joff) ** 4
              + (1.0 - fse - fh2) * vix (sfi_t_ssbef inp) (1 + joff) ** 4
              + fh2 * sfi_t_h2osfc_bef inp ** 4
      t_skin_val = if sfi_frac_veg_nosno inp == 0 && not (sfi_is_urban inp)
                   then lw_grnd ** 0.25
                   else if sfi_is_urban inp
                        then vix (sfi_t_soisno inp) (snl_val + 1 + joff)
                        else 0.0  -- not set for vegetated non-urban

  in SoilFluxesResult
       { sfr_eflx_sh_grnd      = sh_grnd2
       , sfr_eflx_sh_tot       = sh_tot
       , sfr_qflx_evap_soi     = evap_soi2
       , sfr_qflx_evap_tot     = evap_tot
       , sfr_qflx_evap_can     = evap_can
       , sfr_eflx_lh_tot       = lh_tot
       , sfr_eflx_lh_vege      = lh_vege
       , sfr_eflx_lh_vegt      = lh_vegt
       , sfr_eflx_lh_grnd      = lh_grnd
       , sfr_eflx_soil_grnd    = eflx_soil_grnd_val
       , sfr_eflx_lwrad_out    = lwrad_out
       , sfr_eflx_lwrad_net    = lwrad_net
       , sfr_errsoi            = errsoi_val
       , sfr_qflx_ev_snow      = ev_snow2
       , sfr_qflx_ev_soil      = ev_soil1
       , sfr_qflx_ev_h2osfc    = ev_h2osfc1
       , sfr_qflx_liqevap      = liqevap1
       , sfr_qflx_solidevap    = solidevap1
       , sfr_qflx_soliddew     = soliddew0
       , sfr_qflx_liqdew       = liqdew0
       , sfr_t_skin            = t_skin_val
       }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe 0-based vector index
vix :: VU.Vector Double -> Int -> Double
vix v i = v `VU.unsafeIndex` i
