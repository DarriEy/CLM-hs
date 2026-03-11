-- | Water flux variables.
-- Fortran: WaterFluxBulkType
module CLM.Types.WaterFluxData
  ( WaterFluxData(..)
  , defaultWaterFluxData
  ) where

-- | Column/patch-level water fluxes.
data WaterFluxData = WaterFluxData
  { qflx_evap_tot_patch  :: !Double  -- ^ Total evapotranspiration [mm/s]
  , qflx_evap_grnd_col   :: !Double  -- ^ Ground evaporation [mm/s]
  , qflx_tran_veg_patch  :: !Double  -- ^ Transpiration [mm/s]
  , qflx_rain_grnd_col   :: !Double  -- ^ Rain reaching ground [mm/s]
  , qflx_snow_grnd_col   :: !Double  -- ^ Snow reaching ground [mm/s]
  , qflx_surf_col        :: !Double  -- ^ Surface runoff [mm/s]
  , qflx_drain_col       :: !Double  -- ^ Subsurface drainage [mm/s]
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
  }
