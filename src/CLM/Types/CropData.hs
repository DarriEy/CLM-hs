-- | Crop model data structures.
-- Fortran: CropType.F90 — phenology, sowing/harvest tracking, GDD accumulators.
module CLM.Types.CropData
  ( CropData(..)
  , defaultCropData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Crop model state variables (SoA layout).
-- Preserves Fortran variable names with @crop_@ prefix.
data CropData = CropData
  { -- Config
    crop_baset_mapping           :: !String  -- ^ Base temperature mapping mode
  , crop_baset_latvary_intercept :: !Double  -- ^ Latitude-varying baset intercept
  , crop_baset_latvary_slope     :: !Double  -- ^ Latitude-varying baset slope
    -- Patch-level integer
  , crop_nyrs_crop_active_patch  :: !(VU.Vector Int)    -- ^ Years this crop patch has been active
  , crop_harvdate_patch          :: !(VU.Vector Int)    -- ^ Most recent harvest date
  , crop_sowing_reason_patch     :: !(VU.Vector Int)    -- ^ Reason for most recent sowing
  , crop_sowing_count            :: !(VU.Vector Int)    -- ^ Number of sowing events this year
  , crop_harvest_count           :: !(VU.Vector Int)    -- ^ Number of harvest events this year
    -- Patch-level real
  , crop_fertnitro_patch         :: !(VU.Vector Double) -- ^ Fertilizer nitrogen [gN/m^2/yr]
  , crop_gddtsoi_patch           :: !(VU.Vector Double) -- ^ GDD from planting (top soil) [ddays]
  , crop_vf_patch                :: !(VU.Vector Double) -- ^ Vernalization factor for cereal
  , crop_cphase_patch            :: !(VU.Vector Double) -- ^ Phenology phase
  , crop_latbaset_patch          :: !(VU.Vector Double) -- ^ Latitude-varying baset for hui [deg C]
  , crop_hui_patch               :: !(VU.Vector Double) -- ^ Heat unit index [ddays]
  , crop_gddaccum_patch          :: !(VU.Vector Double) -- ^ GDD from planting (air) [ddays]
  , crop_gdd20_baseline_patch    :: !(VU.Vector Double) -- ^ GDD20 baseline [ddays]
  , crop_gdd20_season_start_patch :: !(VU.Vector Double) -- ^ GDD20 season start [day of year]
  , crop_gdd20_season_end_patch  :: !(VU.Vector Double) -- ^ GDD20 season end [day of year]
    -- Patch-level 2D integer (flattened: patch * mxsowings)
  , crop_rx_swindow_starts_thisyr_patch :: !(VU.Vector Int)    -- ^ Prescribed sowing window starts
  , crop_rx_swindow_ends_thisyr_patch   :: !(VU.Vector Int)    -- ^ Prescribed sowing window ends
    -- Patch-level 2D real (flattened: patch * mxsowings)
  , crop_rx_cultivar_gdds_thisyr_patch  :: !(VU.Vector Double) -- ^ Cultivar GDD targets
  , crop_sdates_thisyr_patch            :: !(VU.Vector Double) -- ^ Actual sowing dates this year
  , crop_swindow_starts_thisyr_patch    :: !(VU.Vector Double) -- ^ Sowing window starts this year
  , crop_swindow_ends_thisyr_patch      :: !(VU.Vector Double) -- ^ Sowing window ends this year
  , crop_sowing_reason_thisyr_patch     :: !(VU.Vector Double) -- ^ Reason for each sowing this year
    -- Patch-level 2D real (flattened: patch * mxharvests)
  , crop_sdates_perharv_patch           :: !(VU.Vector Double) -- ^ Sowing dates for harvested crops
  , crop_syears_perharv_patch           :: !(VU.Vector Double) -- ^ Sowing years for harvested crops
  , crop_hdates_thisyr_patch            :: !(VU.Vector Double) -- ^ Harvest dates this year
  , crop_gddaccum_thisyr_patch          :: !(VU.Vector Double) -- ^ Accumulated GDD at harvest
  , crop_hui_thisyr_patch               :: !(VU.Vector Double) -- ^ Accumulated HUI at harvest
  , crop_sowing_reason_perharv_patch    :: !(VU.Vector Double) -- ^ Sowing reason for harvested crops
  , crop_harvest_reason_thisyr_patch    :: !(VU.Vector Double) -- ^ Reason for each harvest this year
  } deriving (Show)

defaultCropData :: CropData
defaultCropData = CropData
  { crop_baset_mapping           = "constant"
  , crop_baset_latvary_intercept = 12.0
  , crop_baset_latvary_slope     = 0.4
  , crop_nyrs_crop_active_patch  = VU.empty
  , crop_harvdate_patch          = VU.empty
  , crop_sowing_reason_patch     = VU.empty
  , crop_sowing_count            = VU.empty
  , crop_harvest_count           = VU.empty
  , crop_fertnitro_patch         = VU.empty
  , crop_gddtsoi_patch           = VU.empty
  , crop_vf_patch                = VU.empty
  , crop_cphase_patch            = VU.empty
  , crop_latbaset_patch          = VU.empty
  , crop_hui_patch               = VU.empty
  , crop_gddaccum_patch          = VU.empty
  , crop_gdd20_baseline_patch    = VU.empty
  , crop_gdd20_season_start_patch = VU.empty
  , crop_gdd20_season_end_patch  = VU.empty
  , crop_rx_swindow_starts_thisyr_patch = VU.empty
  , crop_rx_swindow_ends_thisyr_patch   = VU.empty
  , crop_rx_cultivar_gdds_thisyr_patch  = VU.empty
  , crop_sdates_thisyr_patch            = VU.empty
  , crop_swindow_starts_thisyr_patch    = VU.empty
  , crop_swindow_ends_thisyr_patch      = VU.empty
  , crop_sowing_reason_thisyr_patch     = VU.empty
  , crop_sdates_perharv_patch           = VU.empty
  , crop_syears_perharv_patch           = VU.empty
  , crop_hdates_thisyr_patch            = VU.empty
  , crop_gddaccum_thisyr_patch          = VU.empty
  , crop_hui_thisyr_patch               = VU.empty
  , crop_sowing_reason_perharv_patch    = VU.empty
  , crop_harvest_reason_thisyr_patch    = VU.empty
  }
