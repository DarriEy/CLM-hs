-- | Low-level subgrid construction: add landunits, columns, patches,
-- and compute/validate the subgrid pointer hierarchy.
-- Ported from Julia: src/infrastructure/init_subgrid.jl
-- Fortran: src/main/initSubgridMod.F90
--
-- All functions are pure: they take subgrid data records and return
-- updated records. No IO or mutation.
module CLM.Infrastructure.InitSubgrid
  ( -- * Subgrid data types
    BoundsType(..)
  , GridcellData(..)
  , LandunitData(..)
  , SubgridColumnData(..)
  , SubgridPatchData(..)
    -- * Construction defaults
  , defaultBounds
  , defaultGridcellData
  , defaultLandunitData
  , defaultSubgridColumnData
  , defaultSubgridPatchData
    -- * Adding entities
  , addLandunit
  , addColumn
  , addPatch
    -- * Pointer computation and validation
  , clmPtrsCompdown
  , clmPtrsCheck
    -- * Landunit type constants
  , istsoil, istcrop, istocn, istice, istdlak, istwet
  , isturb_min, isturb_tbd, isturb_hd, isturb_md, isturb_max
  , maxLunit
    -- * Column type constants
  , icolRoof, icolSunwall, icolShadewall, icolRoadImperv, icolRoadPerv
    -- * Special values
  , spval, ispval, noveg
    -- * Landunit classification helpers
  , landunitIsSpecial
  , isHydrologicallyActive
  , iceClassToColItype
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!))

-- ============================================================================
-- Constants (from landunit_varcon.jl, column_varcon.jl, varcon.jl)
-- ============================================================================

istsoil, istcrop, istocn, istice, istdlak, istwet :: Int
istsoil = 1
istcrop = 2
istocn  = 3
istice  = 4
istdlak = 5
istwet  = 6

isturb_min, isturb_tbd, isturb_hd, isturb_md, isturb_max :: Int
isturb_min = 7
isturb_tbd = 7
isturb_hd  = 8
isturb_md  = 9
isturb_max = 9

maxLunit :: Int
maxLunit = 9

icolRoof, icolSunwall, icolShadewall, icolRoadImperv, icolRoadPerv :: Int
icolRoof       = isturb_min * 10 + 1  -- 71
icolSunwall    = isturb_min * 10 + 2  -- 72
icolShadewall  = isturb_min * 10 + 3  -- 73
icolRoadImperv = isturb_min * 10 + 4  -- 74
icolRoadPerv   = isturb_min * 10 + 5  -- 75

spval :: Double
spval = 1.0e36

ispval :: Int
ispval = -9999

noveg :: Int
noveg = 0

-- ============================================================================
-- Bounds
-- ============================================================================

-- | Index bounds for each subgrid level (1-based, inclusive).
data BoundsType = BoundsType
  { begg :: !Int, endg :: !Int  -- ^ Gridcell range
  , begl :: !Int, endl :: !Int  -- ^ Landunit range
  , begc :: !Int, endc :: !Int  -- ^ Column range
  , begp :: !Int, endp :: !Int  -- ^ Patch range
  } deriving (Show, Eq)

defaultBounds :: BoundsType
defaultBounds = BoundsType 1 1 1 1 1 1 1 1

-- ============================================================================
-- Gridcell data
-- ============================================================================

-- | Gridcell-level properties.
data GridcellData = GridcellData
  { grcArea              :: !(Vector Double)  -- ^ Gridcell area [km^2]
  , grcLatdeg            :: !(Vector Double)  -- ^ Latitude [degrees]
  , grcLondeg            :: !(Vector Double)  -- ^ Longitude [degrees]
  , grcLat               :: !(Vector Double)  -- ^ Latitude [radians]
  , grcLon               :: !(Vector Double)  -- ^ Longitude [radians]
  , grcLandunitIndices   :: !(Vector Int)     -- ^ Landunit indices (maxLunit * ngrc), row-major
  } deriving (Show)

defaultGridcellData :: Int -> GridcellData
defaultGridcellData ngrc = GridcellData
  { grcArea            = VU.replicate ngrc 0.0
  , grcLatdeg          = VU.replicate ngrc 0.0
  , grcLondeg          = VU.replicate ngrc 0.0
  , grcLat             = VU.replicate ngrc 0.0
  , grcLon             = VU.replicate ngrc 0.0
  , grcLandunitIndices = VU.replicate (maxLunit * ngrc) ispval
  }

-- | Get landunit index for gridcell g (1-based) and landunit type lt (1-based).
getLandunitIndex :: GridcellData -> Int -> Int -> Int
getLandunitIndex grc lt g = grcLandunitIndices grc ! ((g - 1) * maxLunit + (lt - 1))

-- | Set landunit index, returning updated vector.
setLandunitIndex :: Vector Int -> Int -> Int -> Int -> Vector Int
setLandunitIndex vec lt g val =
  vec VU.// [((g - 1) * maxLunit + (lt - 1), val)]

-- ============================================================================
-- Landunit data
-- ============================================================================

-- | Landunit-level properties and pointers.
data LandunitData = LandunitData
  { lunGridcell  :: !(Vector Int)     -- ^ Parent gridcell index
  , lunWtgcell   :: !(Vector Double)  -- ^ Weight relative to gridcell
  , lunItype     :: !(Vector Int)     -- ^ Landunit type
  , lunIfspecial :: !(Vector Bool)    -- ^ Special landunit flag
  , lunGlcpoi    :: !(Vector Bool)    -- ^ Glacier point flag
  , lunLakpoi    :: !(Vector Bool)    -- ^ Lake point flag
  , lunUrbpoi    :: !(Vector Bool)    -- ^ Urban point flag
  , lunCanyonHwr :: !(Vector Double)  -- ^ Canyon height-to-width ratio (urban)
  , lunActive    :: !(Vector Bool)    -- ^ Active flag
  , lunColi      :: !(Vector Int)     -- ^ First column index
  , lunColf      :: !(Vector Int)     -- ^ Last column index
  , lunNcolumns  :: !(Vector Int)     -- ^ Number of columns
  , lunPatchi    :: !(Vector Int)     -- ^ First patch index
  , lunPatchf    :: !(Vector Int)     -- ^ Last patch index
  , lunNpatches  :: !(Vector Int)     -- ^ Number of patches
  } deriving (Show)

defaultLandunitData :: Int -> LandunitData
defaultLandunitData nl = LandunitData
  { lunGridcell  = VU.replicate nl 0
  , lunWtgcell   = VU.replicate nl 0.0
  , lunItype     = VU.replicate nl 0
  , lunIfspecial = VU.replicate nl False
  , lunGlcpoi    = VU.replicate nl False
  , lunLakpoi    = VU.replicate nl False
  , lunUrbpoi    = VU.replicate nl False
  , lunCanyonHwr = VU.replicate nl 0.0
  , lunActive    = VU.replicate nl True
  , lunColi      = VU.replicate nl 0
  , lunColf      = VU.replicate nl 0
  , lunNcolumns  = VU.replicate nl 0
  , lunPatchi    = VU.replicate nl 0
  , lunPatchf    = VU.replicate nl 0
  , lunNpatches  = VU.replicate nl 0
  }

-- ============================================================================
-- Column data (subgrid-level, distinct from CLM.Types.ColumnData)
-- ============================================================================

-- | Column-level subgrid pointers and classification.
data SubgridColumnData = SubgridColumnData
  { colLandunit            :: !(Vector Int)     -- ^ Parent landunit index
  , colGridcell            :: !(Vector Int)     -- ^ Parent gridcell index
  , colWtlunit             :: !(Vector Double)  -- ^ Weight relative to landunit
  , colWtgcell             :: !(Vector Double)  -- ^ Weight relative to gridcell
  , colItype               :: !(Vector Int)     -- ^ Column type
  , colLunItype            :: !(Vector Int)     -- ^ Parent landunit type
  , colTypeIsDynamic       :: !(Vector Bool)    -- ^ Dynamic type flag
  , colHydrologicallyActive :: !(Vector Bool)   -- ^ Hydrologically active flag
  , colUrbpoi              :: !(Vector Bool)    -- ^ Urban point flag
  , colActive              :: !(Vector Bool)    -- ^ Active flag
  , colPatchi              :: !(Vector Int)     -- ^ First patch index
  , colPatchf              :: !(Vector Int)     -- ^ Last patch index
  , colNpatches            :: !(Vector Int)     -- ^ Number of patches
  } deriving (Show)

defaultSubgridColumnData :: Int -> SubgridColumnData
defaultSubgridColumnData nc = SubgridColumnData
  { colLandunit             = VU.replicate nc 0
  , colGridcell             = VU.replicate nc 0
  , colWtlunit              = VU.replicate nc 0.0
  , colWtgcell              = VU.replicate nc 0.0
  , colItype                = VU.replicate nc 0
  , colLunItype             = VU.replicate nc 0
  , colTypeIsDynamic        = VU.replicate nc False
  , colHydrologicallyActive = VU.replicate nc False
  , colUrbpoi               = VU.replicate nc False
  , colActive               = VU.replicate nc True
  , colPatchi               = VU.replicate nc 0
  , colPatchf               = VU.replicate nc 0
  , colNpatches             = VU.replicate nc 0
  }

-- ============================================================================
-- Patch data (subgrid-level)
-- ============================================================================

-- | Patch-level subgrid pointers and classification.
data SubgridPatchData = SubgridPatchData
  { pchColumn   :: !(Vector Int)     -- ^ Parent column index
  , pchLandunit :: !(Vector Int)     -- ^ Parent landunit index
  , pchGridcell :: !(Vector Int)     -- ^ Parent gridcell index
  , pchWtcol    :: !(Vector Double)  -- ^ Weight relative to column
  , pchWtlunit  :: !(Vector Double)  -- ^ Weight relative to landunit
  , pchWtgcell  :: !(Vector Double)  -- ^ Weight relative to gridcell
  , pchItype    :: !(Vector Int)     -- ^ Patch (PFT) type
  , pchMxy      :: !(Vector Int)     -- ^ PFT index for soil/crop (1-based)
  , pchActive   :: !(Vector Bool)    -- ^ Active flag
  } deriving (Show)

defaultSubgridPatchData :: Int -> SubgridPatchData
defaultSubgridPatchData np = SubgridPatchData
  { pchColumn   = VU.replicate np 0
  , pchLandunit = VU.replicate np 0
  , pchGridcell = VU.replicate np 0
  , pchWtcol    = VU.replicate np 0.0
  , pchWtlunit  = VU.replicate np 0.0
  , pchWtgcell  = VU.replicate np 0.0
  , pchItype    = VU.replicate np 0
  , pchMxy      = VU.replicate np ispval
  , pchActive   = VU.replicate np True
  }

-- ============================================================================
-- Landunit classification helpers
-- ============================================================================

-- | True if the landunit type is "special" (not soil or crop).
landunitIsSpecial :: Int -> Bool
landunitIsSpecial ltype = ltype /= istsoil && ltype /= istcrop

-- | True if a column is hydrologically active.
isHydrologicallyActive :: Int -> Int -> Bool
isHydrologicallyActive ctype luntype =
  luntype == istsoil || luntype == istcrop ||
  (luntype >= isturb_min && luntype <= isturb_max &&
   (ctype == icolRoadPerv || ctype == icolRoadImperv))

-- | Convert glacier elevation class (1-based) to column itype.
iceClassToColItype :: Int -> Int
iceClassToColItype m = istice * 100 + m

-- ============================================================================
-- Adding entities (pure, returning updated records and next index)
-- ============================================================================

-- | Add a landunit. Returns (updated LandunitData, new landunit index).
-- The new landunit is placed at index (li + 1).
addLandunit :: LandunitData -> Int -> Int -> Int -> Double -> (LandunitData, Int)
addLandunit lun li gi ltype wtgcell = (lun', l)
  where
    l = li + 1
    ix = l - 1  -- 0-based for VU
    lun' = lun
      { lunGridcell  = lunGridcell lun  VU.// [(ix, gi)]
      , lunWtgcell   = lunWtgcell lun   VU.// [(ix, wtgcell)]
      , lunItype     = lunItype lun     VU.// [(ix, ltype)]
      , lunIfspecial = lunIfspecial lun VU.// [(ix, landunitIsSpecial ltype)]
      , lunGlcpoi    = lunGlcpoi lun    VU.// [(ix, ltype == istice)]
      , lunLakpoi    = lunLakpoi lun    VU.// [(ix, ltype == istdlak)]
      , lunUrbpoi    = lunUrbpoi lun    VU.// [(ix, ltype >= isturb_min && ltype <= isturb_max)]
      }

-- | Add a column. Returns (updated SubgridColumnData, new column index).
addColumn :: SubgridColumnData -> LandunitData -> Int -> Int -> Int -> Double -> Bool
          -> (SubgridColumnData, Int)
addColumn col lun ci li ctype wtlunit typeIsDynamic = (col', c)
  where
    c = ci + 1
    ix = c - 1
    liIx = li - 1
    col' = col
      { colLandunit             = colLandunit col VU.// [(ix, li)]
      , colGridcell             = colGridcell col VU.// [(ix, lunGridcell lun ! liIx)]
      , colWtlunit              = colWtlunit col VU.// [(ix, wtlunit)]
      , colItype                = colItype col VU.// [(ix, ctype)]
      , colLunItype             = colLunItype col VU.// [(ix, lunItype lun ! liIx)]
      , colTypeIsDynamic        = colTypeIsDynamic col VU.// [(ix, typeIsDynamic)]
      , colHydrologicallyActive = colHydrologicallyActive col VU.//
          [(ix, isHydrologicallyActive ctype (lunItype lun ! liIx))]
      , colUrbpoi               = colUrbpoi col VU.// [(ix, lunUrbpoi lun ! liIx)]
      }

-- | Add a patch. Returns (updated SubgridPatchData, new patch index).
-- natpft_lb is the lower bound for natural PFT numbering (typically 0).
addPatch :: SubgridPatchData -> SubgridColumnData -> LandunitData
         -> Int -> Int -> Int -> Double -> Int
         -> (SubgridPatchData, Int)
addPatch pch col lun pi ci ptype wtcol natpftLb = (pch', p)
  where
    p = pi + 1
    ix = p - 1
    ciIx = ci - 1
    li = colLandunit col ! ciIx
    liIx = li - 1
    lunType = lunItype lun ! liIx
    lbOffset = 1 - natpftLb
    mxyVal = if lunType == istsoil || lunType == istcrop
             then ptype + lbOffset
             else ispval
    pch' = pch
      { pchColumn   = pchColumn pch   VU.// [(ix, ci)]
      , pchLandunit = pchLandunit pch VU.// [(ix, li)]
      , pchGridcell = pchGridcell pch VU.// [(ix, colGridcell col ! ciIx)]
      , pchWtcol    = pchWtcol pch    VU.// [(ix, wtcol)]
      , pchItype    = pchItype pch    VU.// [(ix, ptype)]
      , pchMxy      = pchMxy pch      VU.// [(ix, mxyVal)]
      }

-- ============================================================================
-- Pointer computation (pure)
-- ============================================================================

-- | Fill in down-pointers from up-pointers.
-- Computes col.patchi/patchf, lun.patchi/patchf/coli/colf,
-- and grc.landunit_indices.
clmPtrsCompdown :: BoundsType -> GridcellData -> LandunitData
                -> SubgridColumnData -> SubgridPatchData
                -> (GridcellData, LandunitData, SubgridColumnData)
clmPtrsCompdown bounds grc0 lun0 col0 pch = (grc3, lun3, col2)
  where
    -- Pass 1: Patch -> Column and Patch -> Landunit down-pointers
    (col1, lun1) = foldl patchPass (col0, lun0) [begp bounds .. endp bounds]
    patchPass (colAcc, lunAcc) p =
      let c = pchColumn pch ! (p - 1)
          l = pchLandunit pch ! (p - 1)
          -- Update column patchi/patchf
          colAcc' = colAcc
            { colPatchi = if colPatchi colAcc ! (c - 1) == 0 || p < colPatchi colAcc ! (c - 1)
                          then colPatchi colAcc VU.// [(c - 1, p)]
                          else colPatchi colAcc
            , colPatchf = colPatchf colAcc VU.// [(c - 1, p)]
            }
          colAcc'' = colAcc'
            { colNpatches = colNpatches colAcc' VU.//
                [(c - 1, colPatchf colAcc'' ! (c - 1) - colPatchi colAcc'' ! (c - 1) + 1)]
            }
          -- Update landunit patchi/patchf
          lunAcc' = lunAcc
            { lunPatchi = if lunPatchi lunAcc ! (l - 1) == 0 || p < lunPatchi lunAcc ! (l - 1)
                          then lunPatchi lunAcc VU.// [(l - 1, p)]
                          else lunPatchi lunAcc
            , lunPatchf = lunPatchf lunAcc VU.// [(l - 1, p)]
            }
          lunAcc'' = lunAcc'
            { lunNpatches = lunNpatches lunAcc' VU.//
                [(l - 1, lunPatchf lunAcc'' ! (l - 1) - lunPatchi lunAcc'' ! (l - 1) + 1)]
            }
      in  (colAcc'', lunAcc'')

    -- Pass 2: Column -> Landunit down-pointers
    lun2 = foldl colPass lun1 [begc bounds .. endc bounds]
    colPass lunAcc c =
      let l = colLandunit col0 ! (c - 1)
          lunAcc' = lunAcc
            { lunColi = if lunColi lunAcc ! (l - 1) == 0 || c < lunColi lunAcc ! (l - 1)
                        then lunColi lunAcc VU.// [(l - 1, c)]
                        else lunColi lunAcc
            , lunColf = lunColf lunAcc VU.// [(l - 1, c)]
            }
          lunAcc'' = lunAcc'
            { lunNcolumns = lunNcolumns lunAcc' VU.//
                [(l - 1, lunColf lunAcc'' ! (l - 1) - lunColi lunAcc'' ! (l - 1) + 1)]
            }
      in  lunAcc''

    lun3 = lun2

    -- Pass 3: Landunit -> Gridcell
    col2 = col1
    grcIndices0 = VU.replicate (maxLunit * (endg bounds - begg bounds + 1)) ispval
    grc1 = grc0 { grcLandunitIndices = grcIndices0 }
    grc3 = foldl lunPass grc1 [begl bounds .. endl bounds]
    lunPass grcAcc l =
      let ltype = lunItype lun2 ! (l - 1)
          g = lunGridcell lun2 ! (l - 1)
      in  grcAcc { grcLandunitIndices =
                      setLandunitIndex (grcLandunitIndices grcAcc) ltype g l }

-- ============================================================================
-- Pointer validation (pure)
-- ============================================================================

-- | Validate the subgrid hierarchy. Returns Nothing if valid,
-- or Just errorMessage if invalid.
clmPtrsCheck :: BoundsType -> GridcellData -> LandunitData
             -> SubgridColumnData -> SubgridPatchData
             -> Maybe String
clmPtrsCheck bounds grc lun col pch =
  -- Check gridcell -> landunit indices
  let grcErrors = [ "clmPtrsCheck: g index range error at g=" ++ show g ++
                     ", ltype=" ++ show lt ++ ", l=" ++ show l
                   | g <- [begg bounds .. endg bounds]
                   , lt <- [1 .. maxLunit]
                   , let l = getLandunitIndex grc lt g
                   , l /= ispval
                   , l < begl bounds || l > endl bounds
                   ]
      lunErrors = [ "clmPtrsCheck: l range error at l=" ++ show l
                  | l <- [begl bounds .. endl bounds]
                  , let g = lunGridcell lun ! (l-1)
                  , g < begg bounds || g > endg bounds
                  ]
      colErrors = [ "clmPtrsCheck: c range error at c=" ++ show c
                  | c <- [begc bounds .. endc bounds]
                  , let g = colGridcell col ! (c-1)
                  , g < begg bounds || g > endg bounds
                  ]
      pchErrors = [ "clmPtrsCheck: p range error at p=" ++ show p
                  | p <- [begp bounds .. endp bounds]
                  , let g = pchGridcell pch ! (p-1)
                  , g < begg bounds || g > endg bounds
                  ]
      allErrors = grcErrors ++ lunErrors ++ colErrors ++ pchErrors
  in  case allErrors of
        []    -> Nothing
        (e:_) -> Just e
