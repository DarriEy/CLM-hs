-- | Water info base types.
-- Fortran: WaterInfoBaseType.F90 — information describing a given water instance.
module CLM.Types.WaterInfoBase
  ( WaterInfoBase(..)
  , defaultWaterInfoBulk
  , defaultWaterInfoIsotope
  ) where

-- | Information about a water instance (bulk or tracer).
-- In Julia/Fortran this is an abstract type with subtypes; here we use
-- a sum type with tagged constructors.
data WaterInfoBase
  = WaterInfoBulk
    { wib_ratio :: !Double  -- ^ Ratio (always 1.0 for bulk)
    }
  | WaterInfoIsotope
    { wib_tracer_name                  :: !String  -- ^ Tracer name
    , wib_ratio                        :: !Double  -- ^ Ratio
    , wib_included_in_consistency_check :: !Bool   -- ^ Included in consistency check
    , wib_communicated_with_coupler    :: !Bool    -- ^ Communicated with coupler
    }
  deriving (Show)

defaultWaterInfoBulk :: WaterInfoBase
defaultWaterInfoBulk = WaterInfoBulk { wib_ratio = 1.0 }

defaultWaterInfoIsotope :: WaterInfoBase
defaultWaterInfoIsotope = WaterInfoIsotope
  { wib_tracer_name                  = ""
  , wib_ratio                        = 1.0
  , wib_included_in_consistency_check = False
  , wib_communicated_with_coupler    = False
  }
