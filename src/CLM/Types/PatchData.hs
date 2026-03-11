-- | Patch-level (PFT) data structures.
module CLM.Types.PatchData
  ( PatchData(..)
  , defaultPatchData
  ) where

-- | Patch-level vegetation and surface properties.
data PatchData = PatchData
  { pftIndex         :: !Int      -- ^ PFT type index (0 = bare ground)
  , elai             :: !Double   -- ^ Exposed one-sided LAI
  , esai             :: !Double   -- ^ Exposed one-sided SAI
  , fracVegNosno     :: !Int      -- ^ Fraction of veg that is not snow-covered (0 or 1)
  , htop             :: !Double   -- ^ Canopy top height [m]
  , hbot             :: !Double   -- ^ Canopy bottom height [m]
  } deriving (Show)

defaultPatchData :: PatchData
defaultPatchData = PatchData
  { pftIndex     = 0
  , elai         = 0.0
  , esai         = 0.0
  , fracVegNosno = 0
  , htop         = 0.0
  , hbot         = 0.0
  }
