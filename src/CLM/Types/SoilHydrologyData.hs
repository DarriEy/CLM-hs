-- | Soil hydrology data structures.
-- Fortran: SoilHydrologyType.F90 — water table, ice fraction, VIC parameters.
module CLM.Types.SoilHydrologyData
  ( SoilHydrologyData(..)
  , defaultSoilHydrologyData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column-level soil hydrology state (SoA layout).
-- Preserves Fortran variable names with @sh_@ prefix.
data SoilHydrologyData = SoilHydrologyData
  { sh_h2osfcflag           :: !Int              -- ^ Surface water active flag
    -- NON-VIC state
  , sh_num_substeps_col     :: !(VU.Vector Double) -- ^ Adaptive timestep counter
  , sh_frost_table_col      :: !(VU.Vector Double) -- ^ Frost table depth [m]
  , sh_zwt_col              :: !(VU.Vector Double) -- ^ Water table depth [m]
  , sh_zwts_col             :: !(VU.Vector Double) -- ^ Shallower water table depth [m]
  , sh_zwt_perched_col      :: !(VU.Vector Double) -- ^ Perched water table depth [m]
  , sh_qcharge_col          :: !(VU.Vector Double) -- ^ Aquifer recharge rate [mm/s]
  , sh_icefrac_col          :: !(VU.Vector Double) -- ^ Ice fraction (ncols * nlevgrnd, flattened)
  , sh_h2osfc_thresh_col    :: !(VU.Vector Double) -- ^ Surface water percolation threshold
  , sh_xs_urban_col         :: !(VU.Vector Double) -- ^ Excess soil water above urban ponding limit
    -- VIC parameters
  , sh_hkdepth_col          :: !(VU.Vector Double) -- ^ VIC decay factor [m]
  , sh_b_infil_col          :: !(VU.Vector Double) -- ^ VIC b infiltration parameter
  , sh_ds_col               :: !(VU.Vector Double) -- ^ VIC fraction of Dsmax for nonlinear baseflow
  , sh_dsmax_col            :: !(VU.Vector Double) -- ^ VIC max baseflow velocity [mm/day]
  , sh_Wsvic_col            :: !(VU.Vector Double) -- ^ VIC fraction of max soil moisture for nonlinear baseflow
  , sh_porosity_col         :: !(VU.Vector Double) -- ^ VIC porosity (ncols * nlayer, flattened)
  , sh_vic_clm_fract_col    :: !(VU.Vector Double) -- ^ VIC fraction of VIC layers in CLM layers (3D flattened)
  , sh_depth_col            :: !(VU.Vector Double) -- ^ VIC layer depth (ncols * nlayert, flattened)
  , sh_c_param_col          :: !(VU.Vector Double) -- ^ VIC baseflow exponent
  , sh_expt_col             :: !(VU.Vector Double) -- ^ VIC pore-size distribution parameter (ncols * nlayer, flattened)
  , sh_ksat_col             :: !(VU.Vector Double) -- ^ VIC saturated conductivity (ncols * nlayer, flattened)
  , sh_phi_s_col            :: !(VU.Vector Double) -- ^ VIC soil moisture diffusion param (ncols * nlayer, flattened)
  , sh_moist_col            :: !(VU.Vector Double) -- ^ VIC soil moisture (ncols * nlayert, flattened)
  , sh_moist_vol_col        :: !(VU.Vector Double) -- ^ VIC volumetric soil moisture (ncols * nlayert, flattened)
  , sh_max_moist_col        :: !(VU.Vector Double) -- ^ VIC max layer moist + ice (ncols * nlayer, flattened)
  , sh_top_moist_col        :: !(VU.Vector Double) -- ^ VIC soil moisture in top layers
  , sh_top_max_moist_col    :: !(VU.Vector Double) -- ^ VIC max soil moisture in top layers
  , sh_top_ice_col          :: !(VU.Vector Double) -- ^ VIC ice in top layers
  , sh_top_moist_limited_col :: !(VU.Vector Double) -- ^ VIC limited soil moisture in top layers
  , sh_ice_col              :: !(VU.Vector Double) -- ^ VIC soil ice (ncols * nlayert, flattened)
  } deriving (Show)

defaultSoilHydrologyData :: SoilHydrologyData
defaultSoilHydrologyData = SoilHydrologyData
  { sh_h2osfcflag           = 1
  , sh_num_substeps_col     = VU.empty
  , sh_frost_table_col      = VU.empty
  , sh_zwt_col              = VU.empty
  , sh_zwts_col             = VU.empty
  , sh_zwt_perched_col      = VU.empty
  , sh_qcharge_col          = VU.empty
  , sh_icefrac_col          = VU.empty
  , sh_h2osfc_thresh_col    = VU.empty
  , sh_xs_urban_col         = VU.empty
  , sh_hkdepth_col          = VU.empty
  , sh_b_infil_col          = VU.empty
  , sh_ds_col               = VU.empty
  , sh_dsmax_col            = VU.empty
  , sh_Wsvic_col            = VU.empty
  , sh_porosity_col         = VU.empty
  , sh_vic_clm_fract_col    = VU.empty
  , sh_depth_col            = VU.empty
  , sh_c_param_col          = VU.empty
  , sh_expt_col             = VU.empty
  , sh_ksat_col             = VU.empty
  , sh_phi_s_col            = VU.empty
  , sh_moist_col            = VU.empty
  , sh_moist_vol_col        = VU.empty
  , sh_max_moist_col        = VU.empty
  , sh_top_moist_col        = VU.empty
  , sh_top_max_moist_col    = VU.empty
  , sh_top_ice_col          = VU.empty
  , sh_top_moist_limited_col = VU.empty
  , sh_ice_col              = VU.empty
  }
