-- | Water state bulk data structures.
-- Fortran: WaterStateBulkType.F90 — bulk water state extending WaterStateData.
module CLM.Types.WaterStateBulkData
  ( WaterStateBulkData(..)
  , defaultWaterStateBulkData
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Types.WaterStateData (WaterStateData(..), defaultWaterStateData)

-- | Bulk water state, composing the parent 'WaterStateData' with
-- bulk-specific column-level fields (SoA layout).
-- Preserves Fortran variable names with @wsbulk_@ prefix.
data WaterStateBulkData = WaterStateBulkData
  { -- Parent water state fields (composition)
    wsbulk_ws                   :: !WaterStateData
    -- Bulk-specific column 1D
  , wsbulk_snow_persistence_col :: !(VU.Vector Double) -- ^ Length of time ground has had non-zero snow [s]
  , wsbulk_int_snow_col         :: !(VU.Vector Double) -- ^ Integrated snowfall [mm H2O]
  } deriving (Show)

defaultWaterStateBulkData :: WaterStateBulkData
defaultWaterStateBulkData = WaterStateBulkData
  { wsbulk_ws                   = defaultWaterStateData
  , wsbulk_snow_persistence_col = VU.empty
  , wsbulk_int_snow_col         = VU.empty
  }
