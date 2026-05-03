-- | Master water container.
-- Fortran: WaterType.F90 — holds all water-related data for bulk water and tracers.
module CLM.Types.WaterData
  ( WaterData(..)
  , defaultWaterData
  ) where

-- | Master water container. References to sub-instances are kept at the Haskell
-- call site rather than embedded here (to avoid circular module imports).
-- This record holds the index bounds and parameters for iterating over
-- bulk + tracer instances.
data WaterData = WaterData
  { wd_bulk_and_tracers_beg :: !Int  -- ^ First index for bulk & tracers (1)
  , wd_bulk_and_tracers_end :: !Int  -- ^ Last index for bulk & tracers
  , wd_tracers_beg          :: !Int  -- ^ First index for just tracers (2)
  , wd_tracers_end          :: !Int  -- ^ Last index for just tracers
  , wd_i_bulk               :: !Int  -- ^ Index of bulk in bulk_and_tracers
  , wd_enable_consistency_checks :: !Bool -- ^ Enable consistency checks
  , wd_enable_isotopes      :: !Bool -- ^ Enable isotope tracers
  , wd_bulk_tracer_index    :: !Int  -- ^ Index of tracer replicating bulk (-1 if none)
  } deriving (Show)

defaultWaterData :: WaterData
defaultWaterData = WaterData
  { wd_bulk_and_tracers_beg = 1
  , wd_bulk_and_tracers_end = 1
  , wd_tracers_beg          = 2
  , wd_tracers_end          = 1
  , wd_i_bulk               = 1
  , wd_enable_consistency_checks = False
  , wd_enable_isotopes      = False
  , wd_bulk_tracer_index    = -1
  }
