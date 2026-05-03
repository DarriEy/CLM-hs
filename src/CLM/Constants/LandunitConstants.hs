-- | Landunit-level constants and utility functions.
-- Ported from: src/main/landunit_varcon.F90
-- Preserves Fortran variable names for traceability.
module CLM.Constants.LandunitConstants
  ( -- * Landunit type indices
    istsoil
  , istcrop
  , istocn
  , istice
  , istdlak
  , istwet
  , isturb_min
  , isturb_tbd
  , isturb_hd
  , isturb_md
  , isturb_max
  , max_lunit
  , numurbl
    -- * Landunit names
  , landunitNames
    -- * Utility functions
  , landunitIsSpecial
  ) where

import qualified Data.Vector as V

-- | Vegetated or bare soil
istsoil :: Int
istsoil = 1

-- | Crop
istcrop :: Int
istcrop = 2

-- | Ocean (unused in land model)
istocn :: Int
istocn = 3

-- | Land ice (glacier)
istice :: Int
istice = 4

-- | Deep lake
istdlak :: Int
istdlak = 5

-- | Wetland
istwet :: Int
istwet = 6

-- | Minimum urban type index
isturb_min :: Int
isturb_min = 7

-- | Urban tall building district
isturb_tbd :: Int
isturb_tbd = 7

-- | Urban high density
isturb_hd :: Int
isturb_hd = 8

-- | Urban medium density
isturb_md :: Int
isturb_md = 9

-- | Maximum urban type index
isturb_max :: Int
isturb_max = 9

-- | Maximum number of landunit types
max_lunit :: Int
max_lunit = 9

-- | Number of urban landunit types
numurbl :: Int
numurbl = isturb_max - isturb_min + 1

-- | Landunit names (1-based index matching landunit type)
landunitNames :: V.Vector String
landunitNames = V.fromList
  [ "vegetated_or_bare_soil"  -- 1
  , "crop"                     -- 2
  , "ocean"                    -- 3
  , "landice"                  -- 4
  , "deep_lake"                -- 5
  , "wetland"                  -- 6
  , "urban_tbd"                -- 7
  , "urban_hd"                 -- 8
  , "urban_md"                 -- 9
  ]

-- | Returns 'True' if the landunit type is \"special\" (not vegetated soil or crop).
-- Special landunits: ocean, ice, lake, wetland, urban.
landunitIsSpecial :: Int -> Bool
landunitIsSpecial ltype
  | ltype < 1 || ltype > max_lunit = error $ "Invalid landunit type: " ++ show ltype
  | otherwise = ltype /= istsoil && ltype /= istcrop
