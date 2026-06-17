-- | Water diagnostic bulk data structures.
-- Fortran: WaterDiagnosticBulkType.F90 — snow diagnostics, fractions, humidity.
module CLM.Types.WaterDiagnosticBulkData
  ( WaterDiagnosticBulkData(..)
  , defaultWaterDiagnosticBulkData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column, patch, landunit, and gridcell level water diagnostics (SoA layout).
-- Preserves Fortran variable names with @wdiag_@ prefix.
data WaterDiagnosticBulkData = WaterDiagnosticBulkData
  { -- Parent fields (WaterDiagnosticType) — column 1D
    wdiag_snowice_col                 :: !(VU.Vector Double) -- ^ Average snow ice lens
  , wdiag_snowliq_col                 :: !(VU.Vector Double) -- ^ Average snow liquid water
  , wdiag_total_plant_stored_h2o_col  :: !(VU.Vector Double) -- ^ Water bound in plants [kg/m^2]
  , wdiag_h2osoi_liqice_10cm_col      :: !(VU.Vector Double) -- ^ Liquid + ice in top 10cm [kg/m^2]
  , wdiag_qg_snow_col                 :: !(VU.Vector Double) -- ^ Ground specific humidity (snow) [kg/kg]
  , wdiag_qg_soil_col                 :: !(VU.Vector Double) -- ^ Ground specific humidity (soil) [kg/kg]
  , wdiag_qg_h2osfc_col               :: !(VU.Vector Double) -- ^ Ground specific humidity (surface water) [kg/kg]
  , wdiag_qg_col                      :: !(VU.Vector Double) -- ^ Ground specific humidity [kg/kg]
    -- Parent fields — patch 1D
  , wdiag_h2ocan_patch                :: !(VU.Vector Double) -- ^ Total canopy water (liq+ice) [mm]
  , wdiag_q_ref2m_patch               :: !(VU.Vector Double) -- ^ 2m surface specific humidity [kg/kg]
    -- Parent fields — landunit 1D
  , wdiag_qaf_lun                     :: !(VU.Vector Double) -- ^ Urban canopy air specific humidity [kg/kg]
    -- Parent fields — gridcell 1D
  , wdiag_tws_grc                     :: !(VU.Vector Double) -- ^ Total water storage [mm]
    -- Bulk-specific column 1D
  , wdiag_h2osno_total_col            :: !(VU.Vector Double) -- ^ Total snow water [mm]
  , wdiag_snow_depth_col              :: !(VU.Vector Double) -- ^ Snow height of covered area [m]
  , wdiag_snow_5day_col               :: !(VU.Vector Double) -- ^ 5-day avg snow height [m]
  , wdiag_snowdp_col                  :: !(VU.Vector Double) -- ^ Area-averaged snow height [m]
  , wdiag_snomelt_accum_col           :: !(VU.Vector Double) -- ^ Accumulated snow melt for z0m [m H2O]
  , wdiag_h2osoi_liq_tot_col          :: !(VU.Vector Double) -- ^ Vertically summed liquid water [kg/m^2]
  , wdiag_h2osoi_ice_tot_col          :: !(VU.Vector Double) -- ^ Vertically summed ice lens [kg/m^2]
  , wdiag_exice_subs_tot_col          :: !(VU.Vector Double) -- ^ Total subsidence from excess ice melt [m]
  , wdiag_exice_vol_tot_col           :: !(VU.Vector Double) -- ^ Averaged volumetric excess ice [m^3/m^3]
  , wdiag_snw_rds_top_col             :: !(VU.Vector Double) -- ^ Snow grain radius (top layer) [microns]
  , wdiag_snow_persist_col            :: !(VU.Vector Double) -- ^ Mass-weighted snow persistence for albedo [s]
  , wdiag_h2osno_top_col              :: !(VU.Vector Double) -- ^ Top-layer mass of snow [kg]
  , wdiag_sno_liq_top_col             :: !(VU.Vector Double) -- ^ Snow liquid water fraction (mass), top layer
  , wdiag_dqgdT_col                   :: !(VU.Vector Double) -- ^ d(qg)/dT
    -- Fractions (column 1D)
  , wdiag_frac_sno_col                :: !(VU.Vector Double) -- ^ Fraction of ground covered by snow (0-1)
  , wdiag_frac_sno_eff_col            :: !(VU.Vector Double) -- ^ Effective fraction of ground covered by snow
  , wdiag_frac_h2osfc_col             :: !(VU.Vector Double) -- ^ Fractional area with surface water > 0
  , wdiag_frac_h2osfc_nosnow_col      :: !(VU.Vector Double) -- ^ Fractional area with surface water (no snow)
  , wdiag_wf_col                      :: !(VU.Vector Double) -- ^ Soil water as frac of whc, top 0.05m (0-1)
  , wdiag_wf2_col                     :: !(VU.Vector Double) -- ^ Soil water as frac of whc, top 0.17m (0-1)
    -- Summed fluxes (column 1D)
  , wdiag_qflx_prec_grnd_col          :: !(VU.Vector Double) -- ^ Water onto ground incl canopy runoff [mm/s]
    -- Column 2D (flattened)
  , wdiag_snow_layer_unity_col        :: !(VU.Vector Double) -- ^ Unity values for snow layers
  , wdiag_bw_col                      :: !(VU.Vector Double) -- ^ Partial density of water in snow [kg/m^3]
  , wdiag_air_vol_col                 :: !(VU.Vector Double) -- ^ Air filled porosity
  , wdiag_h2osoi_liqvol_col           :: !(VU.Vector Double) -- ^ Volumetric liquid water content [v/v]
  , wdiag_swe_old_col                 :: !(VU.Vector Double) -- ^ Initial snow water
  , wdiag_exice_subs_col              :: !(VU.Vector Double) -- ^ Per-layer subsidence from excess ice [m]
  , wdiag_exice_vol_col               :: !(VU.Vector Double) -- ^ Per-layer volumetric excess ice [m^3/m^3]
  , wdiag_snw_rds_col                 :: !(VU.Vector Double) -- ^ Snow grain radius per layer [microns]
  , wdiag_frac_iceold_col             :: !(VU.Vector Double) -- ^ Fraction of ice relative to total water
    -- Bulk-specific patch 1D
  , wdiag_iwue_ln_patch               :: !(VU.Vector Double) -- ^ Intrinsic water use efficiency at noon
  , wdiag_vpd_ref2m_patch             :: !(VU.Vector Double) -- ^ 2m vapor pressure deficit [Pa]
  , wdiag_rh_ref2m_patch              :: !(VU.Vector Double) -- ^ 2m relative humidity [%]
  , wdiag_rh_ref2m_r_patch            :: !(VU.Vector Double) -- ^ 2m relative humidity - rural [%]
  , wdiag_rh_ref2m_u_patch            :: !(VU.Vector Double) -- ^ 2m relative humidity - urban [%]
  , wdiag_rh_af_patch                 :: !(VU.Vector Double) -- ^ Fractional humidity of canopy air
  , wdiag_rh10_af_patch               :: !(VU.Vector Double) -- ^ 10-day mean fractional humidity of canopy air
  , wdiag_fwet_patch                  :: !(VU.Vector Double) -- ^ Canopy fraction that is wet (0-1)
  , wdiag_fcansno_patch               :: !(VU.Vector Double) -- ^ Canopy fraction snow covered (0-1)
  , wdiag_fdry_patch                  :: !(VU.Vector Double) -- ^ Canopy fraction green and dry
  , wdiag_qflx_prec_intr_patch        :: !(VU.Vector Double) -- ^ Interception of precipitation [mm/s]
    -- Landunit 1D
  , wdiag_stream_water_depth_lun      :: !(VU.Vector Double) -- ^ Depth of water in streams [m]
  } deriving (Show)

defaultWaterDiagnosticBulkData :: WaterDiagnosticBulkData
defaultWaterDiagnosticBulkData = WaterDiagnosticBulkData
  { wdiag_snowice_col                 = VU.empty
  , wdiag_snowliq_col                 = VU.empty
  , wdiag_total_plant_stored_h2o_col  = VU.empty
  , wdiag_h2osoi_liqice_10cm_col      = VU.empty
  , wdiag_qg_snow_col                 = VU.empty
  , wdiag_qg_soil_col                 = VU.empty
  , wdiag_qg_h2osfc_col               = VU.empty
  , wdiag_qg_col                      = VU.empty
  , wdiag_h2ocan_patch                = VU.empty
  , wdiag_q_ref2m_patch               = VU.empty
  , wdiag_qaf_lun                     = VU.empty
  , wdiag_tws_grc                     = VU.empty
  , wdiag_h2osno_total_col            = VU.empty
  , wdiag_snow_depth_col              = VU.empty
  , wdiag_snow_5day_col               = VU.empty
  , wdiag_snowdp_col                  = VU.empty
  , wdiag_snomelt_accum_col           = VU.empty
  , wdiag_h2osoi_liq_tot_col          = VU.empty
  , wdiag_h2osoi_ice_tot_col          = VU.empty
  , wdiag_exice_subs_tot_col          = VU.empty
  , wdiag_exice_vol_tot_col           = VU.empty
  , wdiag_snw_rds_top_col             = VU.empty
  , wdiag_snow_persist_col            = VU.empty
  , wdiag_h2osno_top_col              = VU.empty
  , wdiag_sno_liq_top_col             = VU.empty
  , wdiag_dqgdT_col                   = VU.empty
  , wdiag_frac_sno_col                = VU.empty
  , wdiag_frac_sno_eff_col            = VU.empty
  , wdiag_frac_h2osfc_col             = VU.empty
  , wdiag_frac_h2osfc_nosnow_col      = VU.empty
  , wdiag_wf_col                      = VU.empty
  , wdiag_wf2_col                     = VU.empty
  , wdiag_qflx_prec_grnd_col          = VU.empty
  , wdiag_snow_layer_unity_col        = VU.empty
  , wdiag_bw_col                      = VU.empty
  , wdiag_air_vol_col                 = VU.empty
  , wdiag_h2osoi_liqvol_col           = VU.empty
  , wdiag_swe_old_col                 = VU.empty
  , wdiag_exice_subs_col              = VU.empty
  , wdiag_exice_vol_col               = VU.empty
  , wdiag_snw_rds_col                 = VU.empty
  , wdiag_frac_iceold_col             = VU.empty
  , wdiag_iwue_ln_patch               = VU.empty
  , wdiag_vpd_ref2m_patch             = VU.empty
  , wdiag_rh_ref2m_patch              = VU.empty
  , wdiag_rh_ref2m_r_patch            = VU.empty
  , wdiag_rh_ref2m_u_patch            = VU.empty
  , wdiag_rh_af_patch                 = VU.empty
  , wdiag_rh10_af_patch               = VU.empty
  , wdiag_fwet_patch                  = VU.empty
  , wdiag_fcansno_patch               = VU.empty
  , wdiag_fdry_patch                  = VU.empty
  , wdiag_qflx_prec_intr_patch        = VU.empty
  , wdiag_stream_water_depth_lun      = VU.empty
  }
