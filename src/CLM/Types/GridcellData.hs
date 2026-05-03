-- | Gridcell-level data structures.
-- Fortran: GridcellType.F90 — topological mapping, daylength, landunit indices.
module CLM.Types.GridcellData
  ( GridcellData(..)
  , defaultGridcellData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Gridcell-level data (SoA layout).
-- Preserves Fortran variable names with @grc_@ prefix.
data GridcellData = GridcellData
  { grc_area              :: !(VU.Vector Double) -- ^ Total land area [km^2]
  , grc_lat               :: !(VU.Vector Double) -- ^ Latitude [radians]
  , grc_lon               :: !(VU.Vector Double) -- ^ Longitude [radians]
  , grc_latdeg            :: !(VU.Vector Double) -- ^ Latitude [degrees]
  , grc_londeg            :: !(VU.Vector Double) -- ^ Longitude [degrees]
  , grc_nbedrock          :: !(VU.Vector Int)    -- ^ Index of uppermost bedrock layer
  , grc_max_dayl          :: !(VU.Vector Double) -- ^ Maximum daylength [s]
  , grc_dayl              :: !(VU.Vector Double) -- ^ Daylength [s]
  , grc_prev_dayl         :: !(VU.Vector Double) -- ^ Previous timestep daylength [s]
  , grc_landunit_indices  :: !(VU.Vector Int)    -- ^ Landunit indices (max_lunit * ngridcells, flattened)
  } deriving (Show)

defaultGridcellData :: GridcellData
defaultGridcellData = GridcellData
  { grc_area             = VU.empty
  , grc_lat              = VU.empty
  , grc_lon              = VU.empty
  , grc_latdeg           = VU.empty
  , grc_londeg           = VU.empty
  , grc_nbedrock         = VU.empty
  , grc_max_dayl         = VU.empty
  , grc_dayl             = VU.empty
  , grc_prev_dayl        = VU.empty
  , grc_landunit_indices = VU.empty
  }
