-- | Column-level data structures.
-- Fortran: clm_varcon / column_varcon — column geometry and properties.
module CLM.Types.ColumnData
  ( ColumnData(..)
  , defaultColumnData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column-level geometry, soil properties, and topography.
-- Uses Structure-of-Arrays layout matching Fortran for GPU friendliness.
data ColumnData = ColumnData
  { -- Vertical geometry (snow+soil, dim = nlevsno + nlevgrnd per column)
    colZ        :: !(VU.Vector Double)   -- ^ Layer midpoint depths [m]
  , colDz       :: !(VU.Vector Double)   -- ^ Layer thicknesses [m]
  , colZi       :: !(VU.Vector Double)   -- ^ Interface depths [m]
    -- Soil properties (soil-only, dim = nlevsoi per column)
  , watsat      :: !(VU.Vector Double)   -- ^ Saturated volumetric water content
  , bsw         :: !(VU.Vector Double)   -- ^ Clapp-Hornberger exponent
  , hksat       :: !(VU.Vector Double)   -- ^ Saturated hydraulic conductivity [mm/s]
  , sucsat      :: !(VU.Vector Double)   -- ^ Saturated matric potential [mm]
    -- Scalars per column
  , zii         :: !Double               -- ^ Convective boundary layer height [m]
  , lakedepth   :: !Double               -- ^ Lake depth [m]
  } deriving (Show)

defaultColumnData :: ColumnData
defaultColumnData = ColumnData
  { colZ      = VU.empty
  , colDz     = VU.empty
  , colZi     = VU.empty
  , watsat    = VU.empty
  , bsw       = VU.empty
  , hksat     = VU.empty
  , sucsat    = VU.empty
  , zii       = 1000.0
  , lakedepth = 0.0
  }
