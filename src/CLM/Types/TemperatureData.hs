-- | Temperature state variables.
-- Fortran: TemperatureType
module CLM.Types.TemperatureData
  ( TemperatureData(..)
  , defaultTemperatureData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column-level temperature state (SoA layout).
data TemperatureData = TemperatureData
  { t_soisno_col  :: !(VU.Vector Double)  -- ^ Soil/snow layer temperatures [K]
  , t_grnd_col    :: !Double              -- ^ Ground surface temperature [K]
  , t_h2osfc_col  :: !Double              -- ^ Surface water temperature [K]
  , t_ref2m_patch :: !Double              -- ^ 2-m reference temperature [K]
  , t_veg_patch   :: !Double              -- ^ Vegetation temperature [K]
  } deriving (Show)

defaultTemperatureData :: TemperatureData
defaultTemperatureData = TemperatureData
  { t_soisno_col  = VU.empty
  , t_grnd_col    = 274.0
  , t_h2osfc_col  = 274.0
  , t_ref2m_patch = 283.0
  , t_veg_patch   = 283.0
  }
