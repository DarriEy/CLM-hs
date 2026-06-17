-- | Water flux variables.
-- Fortran: WaterFluxBulkType
module CLM.Types.WaterFluxData
  ( WaterFluxData(..)
  , defaultWaterFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column/patch-level water fluxes.
data WaterFluxData = WaterFluxData
  { qflx_evap_tot_patch  :: !Double  -- ^ Total evapotranspiration [mm/s]
  , qflx_evap_grnd_col   :: !Double  -- ^ Ground evaporation [mm/s]
  , qflx_tran_veg_patch  :: !Double  -- ^ Transpiration [mm/s]
  , qflx_rain_grnd_col   :: !Double  -- ^ Rain reaching ground [mm/s]
  , qflx_snow_grnd_col   :: !Double  -- ^ Snow reaching ground [mm/s]
  , qflx_surf_col        :: !Double  -- ^ Surface runoff [mm/s]
  , qflx_drain_col       :: !Double  -- ^ Subsurface drainage [mm/s]
  , qflx_evap_tot_patch_vec  :: !(VU.Vector Double)  -- ^ Patch total evapotranspiration [mm/s]
  , qflx_evap_grnd_patch_vec :: !(VU.Vector Double)  -- ^ Patch ground evaporation [mm/s]
  , qflx_tran_veg_patch_vec  :: !(VU.Vector Double)  -- ^ Patch transpiration [mm/s]
  } deriving (Show)

defaultWaterFluxData :: WaterFluxData
defaultWaterFluxData = WaterFluxData
  { qflx_evap_tot_patch = 0.0
  , qflx_evap_grnd_col  = 0.0
  , qflx_tran_veg_patch = 0.0
  , qflx_rain_grnd_col  = 0.0
  , qflx_snow_grnd_col  = 0.0
  , qflx_surf_col       = 0.0
  , qflx_drain_col      = 0.0
  , qflx_evap_tot_patch_vec  = VU.empty
  , qflx_evap_grnd_patch_vec = VU.empty
  , qflx_tran_veg_patch_vec  = VU.empty
  }
