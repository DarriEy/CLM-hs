-- | Lake state data structures.
-- Fortran: LakeStateType.F90 — ice fractions, eddy conductivity, friction velocity.
module CLM.Types.LakeStateData
  ( LakeStateData(..)
  , defaultLakeStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column and patch level lake state (SoA layout).
-- Preserves Fortran variable names with @lake_@ prefix.
data LakeStateData = LakeStateData
  { -- Time constant variables (from surface data)
    lake_lakefetch_col        :: !(VU.Vector Double) -- ^ Lake fetch [m]
  , lake_etal_col             :: !(VU.Vector Double) -- ^ Lake extinction coefficient [1/m]
    -- Time varying variables — column 1D
  , lake_lake_raw_col         :: !(VU.Vector Double) -- ^ Aerodynamic resistance for moisture [s/m]
  , lake_ks_col               :: !(VU.Vector Double) -- ^ Coefficient for eddy diffusivity decay with depth
  , lake_ws_col               :: !(VU.Vector Double) -- ^ Surface friction velocity [m/s]
  , lake_ust_lake_col         :: !(VU.Vector Double) -- ^ Friction velocity [m/s]
  , lake_betaprime_col        :: !(VU.Vector Double) -- ^ Effective beta
  , lake_savedtke1_col        :: !(VU.Vector Double) -- ^ Top level eddy conductivity from previous timestep [W/mK]
  , lake_lake_icefrac_col     :: !(VU.Vector Double) -- ^ Mass fraction of lake layer frozen (ncols * nlevlak, flattened)
  , lake_lake_icefracsurf_col :: !(VU.Vector Double) -- ^ Mass fraction of surface lake layer frozen
  , lake_lake_icethick_col    :: !(VU.Vector Double) -- ^ Ice thickness [m]
  , lake_lakeresist_col       :: !(VU.Vector Double) -- ^ Resistance for grnd_ch4_cond [s/m]
    -- Time varying variables — patch 1D
  , lake_ram1_lake_patch      :: !(VU.Vector Double) -- ^ Aerodynamical resistance [s/m]
  } deriving (Show)

defaultLakeStateData :: LakeStateData
defaultLakeStateData = LakeStateData
  { lake_lakefetch_col        = VU.empty
  , lake_etal_col             = VU.empty
  , lake_lake_raw_col         = VU.empty
  , lake_ks_col               = VU.empty
  , lake_ws_col               = VU.empty
  , lake_ust_lake_col         = VU.empty
  , lake_betaprime_col        = VU.empty
  , lake_savedtke1_col        = VU.empty
  , lake_lake_icefrac_col     = VU.empty
  , lake_lake_icefracsurf_col = VU.empty
  , lake_lake_icethick_col    = VU.empty
  , lake_lakeresist_col       = VU.empty
  , lake_ram1_lake_patch      = VU.empty
  }
