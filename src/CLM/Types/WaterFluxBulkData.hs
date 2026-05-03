-- | Water flux bulk data structures.
-- Fortran: WaterFluxBulkType.F90 — bulk water fluxes extending WaterFluxData.
module CLM.Types.WaterFluxBulkData
  ( WaterFluxBulkData(..)
  , defaultWaterFluxBulkData
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Types.WaterFluxData (WaterFluxData(..), defaultWaterFluxData)

-- | Bulk water fluxes, composing the parent 'WaterFluxData' with
-- bulk-specific patch and column level fields (SoA layout).
-- Preserves Fortran variable names with @wfbulk_@ prefix.
data WaterFluxBulkData = WaterFluxBulkData
  { -- Parent water flux fields (composition)
    wfbulk_wf                         :: !WaterFluxData
    -- Bulk-specific patch 1D
  , wfbulk_qflx_snowindunload_patch   :: !(VU.Vector Double) -- ^ Canopy snow wind unloading [mm/s]
  , wfbulk_qflx_snotempunload_patch   :: !(VU.Vector Double) -- ^ Canopy snow temp unloading [mm/s]
  , wfbulk_qflx_ev_snow_patch         :: !(VU.Vector Double) -- ^ Evaporation from snow [mm/s]
  , wfbulk_qflx_ev_soil_patch         :: !(VU.Vector Double) -- ^ Evaporation from soil [mm/s]
  , wfbulk_qflx_ev_h2osfc_patch       :: !(VU.Vector Double) -- ^ Evaporation from surface water [mm/s]
  , wfbulk_qflx_hydr_redist_patch     :: !(VU.Vector Double) -- ^ Hydraulic redistribution [mm/s]
    -- Bulk-specific column 1D
  , wfbulk_qflx_phs_neg_col           :: !(VU.Vector Double) -- ^ Sum of negative hydraulic redistribution [mm/s]
  , wfbulk_qflx_ev_snow_col           :: !(VU.Vector Double) -- ^ Evaporation from snow [mm/s]
  , wfbulk_qflx_ev_soil_col           :: !(VU.Vector Double) -- ^ Evaporation from soil [mm/s]
  , wfbulk_qflx_ev_h2osfc_col         :: !(VU.Vector Double) -- ^ Evaporation from surface water [mm/s]
  , wfbulk_qflx_sat_excess_surf_col   :: !(VU.Vector Double) -- ^ Surface runoff due to saturated surface [mm/s]
  , wfbulk_qflx_infl_excess_col       :: !(VU.Vector Double) -- ^ Infiltration excess runoff [mm/s]
  , wfbulk_qflx_infl_excess_surf_col  :: !(VU.Vector Double) -- ^ Surface runoff due to infiltration excess [mm/s]
  , wfbulk_qflx_h2osfc_surf_col       :: !(VU.Vector Double) -- ^ Surface water runoff [mm/s]
  , wfbulk_qflx_in_soil_col           :: !(VU.Vector Double) -- ^ Surface input to soil [mm/s]
  , wfbulk_qflx_in_soil_limited_col   :: !(VU.Vector Double) -- ^ Surface input to soil, limited [mm/s]
  , wfbulk_qflx_h2osfc_drain_col      :: !(VU.Vector Double) -- ^ Bottom drainage from h2osfc [mm/s]
  , wfbulk_qflx_top_soil_to_h2osfc_col :: !(VU.Vector Double) -- ^ Portion of qflx_top_soil going to h2osfc [mm/s]
  , wfbulk_qflx_in_h2osfc_col         :: !(VU.Vector Double) -- ^ Total surface input to h2osfc [mm/s]
  , wfbulk_qflx_deficit_col           :: !(VU.Vector Double) -- ^ Water deficit to keep non-negative liquid [mm]
  , wfbulk_AnnET                      :: !(VU.Vector Double) -- ^ Annual average ET flux [mm/s]
    -- Bulk-specific column 2D (flattened)
  , wfbulk_qflx_adv_col               :: !(VU.Vector Double) -- ^ Advective flux across layer interfaces [mm/s]
  , wfbulk_qflx_rootsoi_col           :: !(VU.Vector Double) -- ^ Root-soil water exchange [mm/s]
  , wfbulk_qflx_snomelt_lyr_col       :: !(VU.Vector Double) -- ^ Snow melt in each layer [mm/s]
  , wfbulk_qflx_drain_vr_col          :: !(VU.Vector Double) -- ^ Liquid water lost as drainage [m/timestep]
  } deriving (Show)

defaultWaterFluxBulkData :: WaterFluxBulkData
defaultWaterFluxBulkData = WaterFluxBulkData
  { wfbulk_wf                         = defaultWaterFluxData
  , wfbulk_qflx_snowindunload_patch   = VU.empty
  , wfbulk_qflx_snotempunload_patch   = VU.empty
  , wfbulk_qflx_ev_snow_patch         = VU.empty
  , wfbulk_qflx_ev_soil_patch         = VU.empty
  , wfbulk_qflx_ev_h2osfc_patch       = VU.empty
  , wfbulk_qflx_hydr_redist_patch     = VU.empty
  , wfbulk_qflx_phs_neg_col           = VU.empty
  , wfbulk_qflx_ev_snow_col           = VU.empty
  , wfbulk_qflx_ev_soil_col           = VU.empty
  , wfbulk_qflx_ev_h2osfc_col         = VU.empty
  , wfbulk_qflx_sat_excess_surf_col   = VU.empty
  , wfbulk_qflx_infl_excess_col       = VU.empty
  , wfbulk_qflx_infl_excess_surf_col  = VU.empty
  , wfbulk_qflx_h2osfc_surf_col       = VU.empty
  , wfbulk_qflx_in_soil_col           = VU.empty
  , wfbulk_qflx_in_soil_limited_col   = VU.empty
  , wfbulk_qflx_h2osfc_drain_col      = VU.empty
  , wfbulk_qflx_top_soil_to_h2osfc_col = VU.empty
  , wfbulk_qflx_in_h2osfc_col         = VU.empty
  , wfbulk_qflx_deficit_col           = VU.empty
  , wfbulk_AnnET                      = VU.empty
  , wfbulk_qflx_adv_col               = VU.empty
  , wfbulk_qflx_rootsoi_col           = VU.empty
  , wfbulk_qflx_snomelt_lyr_col       = VU.empty
  , wfbulk_qflx_drain_vr_col          = VU.empty
  }
