-- | Column-level constants and utility functions.
-- Ported from: src/main/column_varcon.F90
-- Preserves Fortran variable names for traceability.
module CLM.Constants.ColumnConstants
  ( -- * Urban column type indices
    icol_roof
  , icol_sunwall
  , icol_shadewall
  , icol_road_imperv
  , icol_road_perv
    -- * Utility functions
  , isHydrologicallyActive
  , iceClassToColItype
  , colItypeToIceClass
  ) where

import CLM.Constants.LandunitConstants
  ( istsoil, istcrop, istice, istdlak, istwet
  , isturb_min, isturb_max
  )

-- | Urban roof column type
icol_roof :: Int
icol_roof = isturb_min * 10 + 1  -- 71

-- | Urban sunlit wall column type
icol_sunwall :: Int
icol_sunwall = isturb_min * 10 + 2  -- 72

-- | Urban shaded wall column type
icol_shadewall :: Int
icol_shadewall = isturb_min * 10 + 3  -- 73

-- | Urban impervious road column type
icol_road_imperv :: Int
icol_road_imperv = isturb_min * 10 + 4  -- 74

-- | Urban pervious road column type
icol_road_perv :: Int
icol_road_perv = isturb_min * 10 + 5  -- 75

-- | Determine whether a column is hydrologically active.
-- Soil, crop, ice, lake, wetland, and pervious road columns are active.
-- Urban walls, roofs, and impervious roads are not.
isHydrologicallyActive
  :: Int   -- ^ Column itype
  -> Int   -- ^ Landunit itype
  -> Bool
isHydrologicallyActive colItype lunItype
  | lunItype == istsoil = True
  | lunItype == istcrop = True
  | lunItype == istice  = True
  | lunItype == istdlak = True
  | lunItype == istwet  = True
  | lunItype >= isturb_min && lunItype <= isturb_max
    = colItype == icol_road_perv
  | otherwise = False

-- | Convert a glacier ice class (1..maxpatch_glc) to a column itype.
iceClassToColItype :: Int -> Int
iceClassToColItype = id

-- | Convert a column itype back to a glacier ice class.
colItypeToIceClass :: Int -> Int
colItypeToIceClass = id
