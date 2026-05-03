{-# LANGUAGE BangPatterns #-}
-- | Water and energy balance checking.
-- Fortran: BalanceCheckMod.F90
-- Julia:   src/biogeophys/balance_check.jl
--
-- Accumulates numerical truncation errors of the water and energy balance.
-- The error for energy balance:
--   error = |Net radiation - dU/dt - SH - LH|
-- The error for water balance:
--   error = |precipitation - dW/dt - evaporation - runoff|
--
-- All functions are pure.
module CLM.BioGeoPhys.BalanceCheck
  ( -- * Initialization
    BalanceCheckConfig(..)
  , balanceCheckInit
    -- * Water balance
  , WaterBalanceColInput(..)
  , WaterBalanceColOutput(..)
  , waterBalanceCol
  , WaterBalanceGrcInput(..)
  , waterBalanceGrc
    -- * Snow balance
  , SnowBalanceInput(..)
  , SnowBalanceOutput(..)
  , snowBalance
    -- * Energy balance
  , EnergyBalanceInput(..)
  , EnergyBalanceOutput(..)
  , energyBalance
    -- * Constants
  , balanceCheckSkipSize
  , h2oWarningThresh
  , energyWarningThresh
  , balanceErrorThresh
  , spval
  ) where

-- No VU import needed; per-column functions work on scalars

-- =========================================================================
-- Constants
-- =========================================================================

-- | Time steps to skip the balance check at startup [s]
balanceCheckSkipSize :: Double
balanceCheckSkipSize = 3600.0

-- | Warning threshold for water balance [mm]
h2oWarningThresh :: Double
h2oWarningThresh = 1.0e-9

-- | Warning threshold for energy balance [W/m2]
energyWarningThresh :: Double
energyWarningThresh = 1.0e-7

-- | Error threshold for stopping [mm or W/m2]
balanceErrorThresh :: Double
balanceErrorThresh = 1.0e-5

-- | Special value sentinel
spval :: Double
spval = 1.0e36

-- =========================================================================
-- Initialization
-- =========================================================================

-- | Balance check configuration.
data BalanceCheckConfig = BalanceCheckConfig
  { bcc_skip_steps :: !Int  -- ^ Number of timesteps to skip at startup
  } deriving (Show)

-- | Initialize balance check. Returns config with skip_steps computed.
balanceCheckInit :: Double -> BalanceCheckConfig
balanceCheckInit dtime = BalanceCheckConfig
  { bcc_skip_steps = max 2 (round (balanceCheckSkipSize / dtime)) + 1
  }

-- =========================================================================
-- Column-level water balance
-- =========================================================================

-- | Input for column-level water balance check.
data WaterBalanceColInput = WaterBalanceColInput
  { wbci_endwb           :: !Double  -- ^ End-of-step water balance [kg/m2]
  , wbci_begwb           :: !Double  -- ^ Begin-of-step water balance [kg/m2]
  , wbci_forc_rain       :: !Double  -- ^ Rain rate [kg/m2/s]
  , wbci_forc_snow       :: !Double  -- ^ Snow rate [kg/m2/s]
  , wbci_qflx_flood      :: !Double  -- ^ Flood rate [kg/m2/s]
  , wbci_qflx_sfc_irrig  :: !Double  -- ^ Surface irrigation [kg/m2/s]
  , wbci_qflx_glcice_dyn :: !Double  -- ^ Glacier ice dyn flux [kg/m2/s]
  , wbci_qflx_evap_tot   :: !Double  -- ^ Total evaporation [kg/m2/s]
  , wbci_qflx_surf       :: !Double  -- ^ Surface runoff [kg/m2/s]
  , wbci_qflx_qrgwl      :: !Double  -- ^ Groundwater recharge/loss [kg/m2/s]
  , wbci_qflx_drain      :: !Double  -- ^ Sub-surface drainage [kg/m2/s]
  , wbci_qflx_drain_perch :: !Double -- ^ Perched drainage [kg/m2/s]
  , wbci_qflx_ice_runoff :: !Double  -- ^ Ice runoff [kg/m2/s]
  , wbci_qflx_snwcp_disc_liq :: !Double -- ^ Discarded snow cap liq [kg/m2/s]
  , wbci_qflx_snwcp_disc_ice :: !Double -- ^ Discarded snow cap ice [kg/m2/s]
  , wbci_dtime           :: !Double  -- ^ Timestep [s]
  , wbci_is_active       :: !Bool    -- ^ Is column active?
  } deriving (Show)

-- | Output for column-level water balance check.
data WaterBalanceColOutput = WaterBalanceColOutput
  { wbco_errh2o :: !Double  -- ^ Water balance error [mm]
  } deriving (Show)

-- | Compute column-level water balance error.
waterBalanceCol :: WaterBalanceColInput -> WaterBalanceColOutput
waterBalanceCol inp
  | not (wbci_is_active inp) = WaterBalanceColOutput { wbco_errh2o = 0.0 }
  | otherwise = WaterBalanceColOutput
      { wbco_errh2o = wbci_endwb inp - wbci_begwb inp
          - (wbci_forc_rain inp
           + wbci_forc_snow inp
           + wbci_qflx_flood inp
           + wbci_qflx_sfc_irrig inp
           + wbci_qflx_glcice_dyn inp
           - wbci_qflx_evap_tot inp
           - wbci_qflx_surf inp
           - wbci_qflx_qrgwl inp
           - wbci_qflx_drain inp
           - wbci_qflx_drain_perch inp
           - wbci_qflx_ice_runoff inp
           - wbci_qflx_snwcp_disc_liq inp
           - wbci_qflx_snwcp_disc_ice inp) * wbci_dtime inp
      }

-- =========================================================================
-- Gridcell-level water balance
-- =========================================================================

-- | Input for gridcell-level water balance check.
data WaterBalanceGrcInput = WaterBalanceGrcInput
  { wbgi_endwb_grc        :: !Double
  , wbgi_begwb_grc        :: !Double
  , wbgi_forc_rain_grc    :: !Double
  , wbgi_forc_snow_grc    :: !Double
  , wbgi_forc_flood_grc   :: !Double
  , wbgi_qflx_sfc_irrig_grc :: !Double
  , wbgi_qflx_glcice_dyn_grc :: !Double
  , wbgi_qflx_evap_tot_grc :: !Double
  , wbgi_qflx_surf_grc    :: !Double
  , wbgi_qflx_qrgwl_grc   :: !Double
  , wbgi_qflx_drain_grc   :: !Double
  , wbgi_qflx_drain_perch_grc :: !Double
  , wbgi_qflx_ice_runoff_grc :: !Double
  , wbgi_qflx_snwcp_disc_liq_grc :: !Double
  , wbgi_qflx_snwcp_disc_ice_grc :: !Double
  , wbgi_dtime             :: !Double
  } deriving (Show)

-- | Compute gridcell-level water balance error.
waterBalanceGrc :: WaterBalanceGrcInput -> Double
waterBalanceGrc inp =
  wbgi_endwb_grc inp - wbgi_begwb_grc inp
    - (wbgi_forc_rain_grc inp
     + wbgi_forc_snow_grc inp
     + wbgi_forc_flood_grc inp
     + wbgi_qflx_sfc_irrig_grc inp
     + wbgi_qflx_glcice_dyn_grc inp
     - wbgi_qflx_evap_tot_grc inp
     - wbgi_qflx_surf_grc inp
     - wbgi_qflx_qrgwl_grc inp
     - wbgi_qflx_drain_grc inp
     - wbgi_qflx_drain_perch_grc inp
     - wbgi_qflx_ice_runoff_grc inp
     - wbgi_qflx_snwcp_disc_liq_grc inp
     - wbgi_qflx_snwcp_disc_ice_grc inp) * wbgi_dtime inp

-- =========================================================================
-- Snow balance
-- =========================================================================

-- | Input for snow balance check at a single column.
data SnowBalanceInput = SnowBalanceInput
  { sbi_h2osno_total    :: !Double  -- ^ Current total snow water [kg/m2]
  , sbi_h2osno_old      :: !Double  -- ^ Previous total snow water [kg/m2]
  , sbi_snl             :: !Int     -- ^ Number of snow layers (<= 0)
  , sbi_is_active       :: !Bool    -- ^ Is column active?
  , sbi_dtime           :: !Double  -- ^ Timestep [s]
    -- Snow sources
  , sbi_qflx_prec_grnd  :: !Double  -- ^ Precipitation reaching ground [kg/m2/s]
  , sbi_qflx_soliddew   :: !Double  -- ^ Solid dew to top layer [kg/m2/s]
  , sbi_qflx_liqdew     :: !Double  -- ^ Liquid dew to top layer [kg/m2/s]
  , sbi_qflx_snow_grnd  :: !Double  -- ^ Snow reaching ground [kg/m2/s]
  , sbi_qflx_liq_grnd   :: !Double  -- ^ Liquid reaching ground [kg/m2/s]
  , sbi_qflx_snow_h2osfc :: !Double -- ^ Snow to surface water [kg/m2/s]
  , sbi_qflx_h2osfc_to_ice :: !Double -- ^ Surface water to ice [kg/m2/s]
  , sbi_frac_sno_eff    :: !Double  -- ^ Effective snow fraction
    -- Snow sinks
  , sbi_qflx_solidevap  :: !Double  -- ^ Solid evaporation [kg/m2/s]
  , sbi_qflx_liqevap    :: !Double  -- ^ Liquid evaporation [kg/m2/s]
  , sbi_qflx_snow_drain :: !Double  -- ^ Snow drainage [kg/m2/s]
  , sbi_qflx_snwcp_ice  :: !Double  -- ^ Snow capping ice [kg/m2/s]
  , sbi_qflx_snwcp_liq  :: !Double  -- ^ Snow capping liquid [kg/m2/s]
  , sbi_qflx_snwcp_disc_ice :: !Double
  , sbi_qflx_snwcp_disc_liq :: !Double
  , sbi_qflx_sl_top_soil :: !Double -- ^ Snow to top soil [kg/m2/s]
    -- Column/landunit type flags
  , sbi_is_soil_or_crop :: !Bool    -- ^ ISTSOIL, ISTCROP, ISTWET, ISTICE, or pervious road
  , sbi_is_lake         :: !Bool    -- ^ Is this a lake column?
  } deriving (Show)

-- | Output of snow balance check.
data SnowBalanceOutput = SnowBalanceOutput
  { sbo_errh2osno    :: !Double  -- ^ Snow balance error [mm]
  , sbo_snow_sources :: !Double  -- ^ Total snow sources [kg/m2/s]
  , sbo_snow_sinks   :: !Double  -- ^ Total snow sinks [kg/m2/s]
  } deriving (Show)

-- | Compute snow balance error for a single column.
snowBalance :: SnowBalanceInput -> SnowBalanceOutput
snowBalance inp
  | not (sbi_is_active inp) = SnowBalanceOutput 0.0 0.0 0.0
  | sbi_snl inp >= 0 = SnowBalanceOutput 0.0 0.0 0.0  -- no snow layers
  | otherwise =
      let fse = sbi_frac_sno_eff inp
          dt  = sbi_dtime inp

          -- Base sources/sinks (general case)
          (!sources, !sinks)
            | sbi_is_soil_or_crop inp =
                ( (sbi_qflx_snow_grnd inp - sbi_qflx_snow_h2osfc inp)
                  + fse * (sbi_qflx_liq_grnd inp + sbi_qflx_soliddew inp
                           + sbi_qflx_liqdew inp)
                  + sbi_qflx_h2osfc_to_ice inp
                , fse * (sbi_qflx_solidevap inp + sbi_qflx_liqevap inp)
                  + sbi_qflx_snwcp_ice inp + sbi_qflx_snwcp_liq inp
                  + sbi_qflx_snwcp_disc_ice inp + sbi_qflx_snwcp_disc_liq inp
                  + sbi_qflx_snow_drain inp + sbi_qflx_sl_top_soil inp
                )
            | sbi_is_lake inp =
                ( sbi_qflx_snow_grnd inp
                  + fse * (sbi_qflx_liq_grnd inp + sbi_qflx_soliddew inp
                           + sbi_qflx_liqdew inp)
                , fse * (sbi_qflx_solidevap inp + sbi_qflx_liqevap inp)
                  + sbi_qflx_snwcp_ice inp + sbi_qflx_snwcp_liq inp
                  + sbi_qflx_snwcp_disc_ice inp + sbi_qflx_snwcp_disc_liq inp
                  + sbi_qflx_snow_drain inp + sbi_qflx_sl_top_soil inp
                )
            | otherwise =
                ( sbi_qflx_prec_grnd inp + sbi_qflx_soliddew inp
                  + sbi_qflx_liqdew inp
                , sbi_qflx_solidevap inp + sbi_qflx_liqevap inp
                  + sbi_qflx_snow_drain inp + sbi_qflx_snwcp_ice inp
                  + sbi_qflx_snwcp_liq inp
                  + sbi_qflx_snwcp_disc_ice inp + sbi_qflx_snwcp_disc_liq inp
                  + sbi_qflx_sl_top_soil inp
                )

          !errh2osno = (sbi_h2osno_total inp - sbi_h2osno_old inp)
                     - (sources - sinks) * dt

      in SnowBalanceOutput
           { sbo_errh2osno    = errh2osno
           , sbo_snow_sources = sources
           , sbo_snow_sinks   = sinks
           }

-- =========================================================================
-- Energy balance
-- =========================================================================

-- | Input for energy balance check at a single patch.
data EnergyBalanceInput = EnergyBalanceInput
  { ebi_fsa             :: !Double  -- ^ Absorbed solar radiation [W/m2]
  , ebi_fsr             :: !Double  -- ^ Reflected solar radiation [W/m2]
  , ebi_sabv            :: !Double  -- ^ Absorbed solar by vegetation [W/m2]
  , ebi_sabg            :: !Double  -- ^ Absorbed solar by ground [W/m2]
  , ebi_sabg_chk        :: !Double  -- ^ sabg for checking [W/m2]
  , ebi_forc_solad1     :: !Double  -- ^ Direct solar band 1 [W/m2]
  , ebi_forc_solad2     :: !Double  -- ^ Direct solar band 2 [W/m2]
  , ebi_forc_solai1     :: !Double  -- ^ Diffuse solar band 1 [W/m2]
  , ebi_forc_solai2     :: !Double  -- ^ Diffuse solar band 2 [W/m2]
  , ebi_forc_lwrad      :: !Double  -- ^ Incoming longwave [W/m2]
  , ebi_eflx_lwrad_out  :: !Double  -- ^ Outgoing longwave [W/m2]
  , ebi_eflx_lwrad_net  :: !Double  -- ^ Net longwave [W/m2]
  , ebi_eflx_sh_tot     :: !Double  -- ^ Total sensible heat [W/m2]
  , ebi_eflx_lh_tot     :: !Double  -- ^ Total latent heat [W/m2]
  , ebi_eflx_soil_grnd  :: !Double  -- ^ Ground heat flux [W/m2]
  , ebi_dhsdt_canopy    :: !Double  -- ^ Canopy heat storage change [W/m2]
  , ebi_is_urban        :: !Bool    -- ^ Is urban patch?
  , ebi_is_active       :: !Bool    -- ^ Is patch active?
    -- Urban-specific
  , ebi_eflx_wasteheat  :: !Double
  , ebi_eflx_heat_from_ac :: !Double
  , ebi_eflx_traffic    :: !Double
  , ebi_eflx_ventilation :: !Double
  } deriving (Show)

-- | Output of energy balance check at a single patch.
data EnergyBalanceOutput = EnergyBalanceOutput
  { ebo_errsol :: !Double  -- ^ Solar radiation balance error [W/m2]
  , ebo_errlon :: !Double  -- ^ Longwave balance error [W/m2]
  , ebo_errseb :: !Double  -- ^ Surface energy balance error [W/m2]
  , ebo_netrad :: !Double  -- ^ Net radiation [W/m2]
  } deriving (Show)

-- | Compute energy balance errors for a single patch.
energyBalance :: EnergyBalanceInput -> EnergyBalanceOutput
energyBalance inp
  | not (ebi_is_active inp) = EnergyBalanceOutput 0.0 0.0 0.0 0.0
  | otherwise = EnergyBalanceOutput
      { ebo_errsol = errsol_v
      , ebo_errlon = errlon_v
      , ebo_errseb = errseb_v
      , ebo_netrad = ebi_fsa inp - ebi_eflx_lwrad_net inp
      }
  where
    -- Solar
    errsol_v
      | ebi_is_urban inp = spval
      | otherwise = ebi_fsa inp + ebi_fsr inp
          - (ebi_forc_solad1 inp + ebi_forc_solad2 inp
           + ebi_forc_solai1 inp + ebi_forc_solai2 inp)

    -- Longwave
    errlon_v
      | ebi_is_urban inp = spval
      | otherwise = ebi_eflx_lwrad_out inp - ebi_eflx_lwrad_net inp
          - ebi_forc_lwrad inp

    -- Surface energy balance
    errseb_v
      | ebi_is_urban inp =
          ebi_sabv inp + ebi_sabg inp
          - ebi_eflx_lwrad_net inp
          - ebi_eflx_sh_tot inp - ebi_eflx_lh_tot inp
          - ebi_eflx_soil_grnd inp
          + ebi_eflx_wasteheat inp + ebi_eflx_heat_from_ac inp
          + ebi_eflx_traffic inp + ebi_eflx_ventilation inp
      | otherwise =
          ebi_sabv inp + ebi_sabg_chk inp + ebi_forc_lwrad inp
          - ebi_eflx_lwrad_out inp
          - ebi_eflx_sh_tot inp - ebi_eflx_lh_tot inp
          - ebi_eflx_soil_grnd inp - ebi_dhsdt_canopy inp
