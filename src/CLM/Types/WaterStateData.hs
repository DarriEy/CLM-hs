-- | Water state variables.
-- Fortran: WaterStateBulkType
module CLM.Types.WaterStateData
  ( WaterStateData(..)
  , defaultWaterStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column-level water state (SoA layout).
data WaterStateData = WaterStateData
  { h2osoi_liq_col   :: !(VU.Vector Double)  -- ^ Liquid water in each layer [kg/m²]
  , h2osoi_ice_col   :: !(VU.Vector Double)  -- ^ Ice in each layer [kg/m²]
  , h2osoi_vol_col   :: !(VU.Vector Double)  -- ^ Volumetric soil water (soil-only) [m³/m³]
  , h2osno_col       :: !Double              -- ^ Snow water equivalent [kg/m²]
  , h2osfc_col       :: !Double              -- ^ Surface water [kg/m²]
  , h2ocan_patch     :: !Double              -- ^ Canopy water [kg/m²]
  , liqcan_patch     :: !Double              -- ^ Liquid water on canopy [kg/m²]
  , snocan_patch     :: !Double              -- ^ Snow water on canopy [kg/m²]
  , h2ocan_patch_vec :: !(VU.Vector Double)  -- ^ Patch-level canopy water [kg/m²]
  , liqcan_patch_vec :: !(VU.Vector Double)  -- ^ Patch-level canopy liquid [kg/m²]
  , snocan_patch_vec :: !(VU.Vector Double)  -- ^ Patch-level canopy snow [kg/m²]
  } deriving (Show)

defaultWaterStateData :: WaterStateData
defaultWaterStateData = WaterStateData
  { h2osoi_liq_col = VU.empty
  , h2osoi_ice_col = VU.empty
  , h2osoi_vol_col = VU.empty
  , h2osno_col     = 0.0
  , h2osfc_col     = 0.0
  , h2ocan_patch   = 0.0
  , liqcan_patch   = 0.0
  , snocan_patch   = 0.0
  , h2ocan_patch_vec = VU.empty
  , liqcan_patch_vec = VU.empty
  , snocan_patch_vec = VU.empty
  }
