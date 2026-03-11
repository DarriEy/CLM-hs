-- | Filter/mask utilities for column and patch loops.
-- Replaces Fortran integer filter arrays with Bool vectors.
module CLM.Infrastructure.Filters
  ( FilterSet(..)
  , defaultFilterSet
  , maskToIndices
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Set of masks for different surface types.
data FilterSet = FilterSet
  { maskSoil     :: !(VU.Vector Bool)  -- ^ Soil columns
  , maskLake     :: !(VU.Vector Bool)  -- ^ Lake columns
  , maskUrban    :: !(VU.Vector Bool)  -- ^ Urban columns
  , maskSnowpack :: !(VU.Vector Bool)  -- ^ Columns with active snowpack
  , maskExposedVeg :: !(VU.Vector Bool) -- ^ Patches with exposed vegetation
  } deriving (Show)

defaultFilterSet :: FilterSet
defaultFilterSet = FilterSet
  { maskSoil       = VU.empty
  , maskLake       = VU.empty
  , maskUrban      = VU.empty
  , maskSnowpack   = VU.empty
  , maskExposedVeg = VU.empty
  }

-- | Convert a Bool mask to a vector of active indices (for interop).
maskToIndices :: VU.Vector Bool -> VU.Vector Int
maskToIndices mask =
  VU.map fst $ VU.filter snd $ VU.indexed mask
