-- | Domain decomposition types and functions.
-- Fortran: decompMod.F90
-- Bounds, clump info, processor info, and subgrid index queries.
module CLM.Infrastructure.Decomp
  ( -- * Subgrid level constants
    SubgridLevel(..)
    -- * Bounds level constants
  , BoundsLevel(..)
    -- * Types
  , BoundsType(..)
  , defaultBounds
  , ProcessorType(..)
  , defaultProcessor
  , ClumpType(..)
  , defaultClump
  , DecompData(..)
  , defaultDecomp
    -- * Queries
  , getBeg
  , getEnd
  , getClumpBounds
  , getProcBounds
  , getProcTotal
  , getProcGlobal
  , getProcClumps
  , getSubgridLevelGsize
  ) where

import qualified Data.Vector as V

-- | Subgrid hierarchy levels.
data SubgridLevel
  = SubgridUnspecified
  | SubgridLndGrid
  | SubgridGridcell
  | SubgridLandunit
  | SubgridColumn
  | SubgridPatch
  | SubgridCohort
  deriving (Eq, Show, Ord, Enum)

-- | Whether bounds are for a processor or a clump.
data BoundsLevel
  = BoundsProc
  | BoundsClump
  deriving (Eq, Show)

-- | Subgrid bounds for processor or clump decomposition.
-- Ported from @bounds_type@ in @decompMod.F90@.
data BoundsType = BoundsType
  { begg       :: !Int    -- ^ Beginning gridcell index
  , endg       :: !Int    -- ^ Ending gridcell index
  , begl       :: !Int    -- ^ Beginning landunit index
  , endl       :: !Int    -- ^ Ending landunit index
  , begc       :: !Int    -- ^ Beginning column index
  , endc       :: !Int    -- ^ Ending column index
  , begp       :: !Int    -- ^ Beginning patch index
  , endp       :: !Int    -- ^ Ending patch index
  , begCohort  :: !Int    -- ^ Beginning cohort index
  , endCohort  :: !Int    -- ^ Ending cohort index
  , boundsLevel :: !BoundsLevel
  , clumpIndex :: !Int    -- ^ Clump index (if clump level, else -1)
  } deriving (Show)

defaultBounds :: BoundsType
defaultBounds = BoundsType 0 0 0 0 0 0 0 0 0 0 BoundsProc (-1)

-- | Per-processor decomposition information.
-- Ported from @processor_type@ in @decompMod.F90@.
data ProcessorType = ProcessorType
  { procNclumps  :: !Int        -- ^ Number of clumps for this processor
  , procCid      :: !(V.Vector Int) -- ^ Clump indices
  , procNcells   :: !Int
  , procNlunits  :: !Int
  , procNcols    :: !Int
  , procNpatches :: !Int
  , procNCohorts :: !Int
  , procBegg     :: !Int
  , procEndg     :: !Int
  , procBegl     :: !Int
  , procEndl     :: !Int
  , procBegc     :: !Int
  , procEndc     :: !Int
  , procBegp     :: !Int
  , procEndp     :: !Int
  , procBegCohort :: !Int
  , procEndCohort :: !Int
  } deriving (Show)

defaultProcessor :: ProcessorType
defaultProcessor = ProcessorType 0 V.empty 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Per-clump decomposition information.
-- Ported from @clump_type@ in @decompMod.F90@.
data ClumpType = ClumpType
  { clumpOwner    :: !Int
  , clumpNcells   :: !Int
  , clumpNlunits  :: !Int
  , clumpNcols    :: !Int
  , clumpNpatches :: !Int
  , clumpNCohorts :: !Int
  , clumpBegg     :: !Int
  , clumpEndg     :: !Int
  , clumpBegl     :: !Int
  , clumpEndl     :: !Int
  , clumpBegc     :: !Int
  , clumpEndc     :: !Int
  , clumpBegp     :: !Int
  , clumpEndp     :: !Int
  , clumpBegCohort :: !Int
  , clumpEndCohort :: !Int
  } deriving (Show)

defaultClump :: ClumpType
defaultClump = ClumpType 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- | Global decomposition state.
-- Ported from module-level variables in @decompMod.F90@.
data DecompData = DecompData
  { decompProcinfo   :: !ProcessorType
  , decompClumps     :: !(V.Vector ClumpType)
  , decompNclumps    :: !Int           -- ^ Total clumps across all processors
  , decompNumg       :: !Int           -- ^ Total gridcells
  , decompNuml       :: !Int           -- ^ Total landunits
  , decompNumc       :: !Int           -- ^ Total columns
  , decompNump       :: !Int           -- ^ Total patches
  , decompNumCohort  :: !Int           -- ^ Total cohorts
  , decompLdomainNs  :: !Int           -- ^ Domain size
  } deriving (Show)

defaultDecomp :: DecompData
defaultDecomp = DecompData defaultProcessor V.empty 0 0 0 0 0 0 0

-- | Get beginning bound for a given subgrid level.
getBeg :: BoundsType -> SubgridLevel -> Maybe Int
getBeg b SubgridGridcell = Just (begg b)
getBeg b SubgridLandunit = Just (begl b)
getBeg b SubgridColumn   = Just (begc b)
getBeg b SubgridPatch    = Just (begp b)
getBeg b SubgridCohort   = Just (begCohort b)
getBeg _ _               = Nothing

-- | Get ending bound for a given subgrid level.
getEnd :: BoundsType -> SubgridLevel -> Maybe Int
getEnd b SubgridGridcell = Just (endg b)
getEnd b SubgridLandunit = Just (endl b)
getEnd b SubgridColumn   = Just (endc b)
getEnd b SubgridPatch    = Just (endp b)
getEnd b SubgridCohort   = Just (endCohort b)
getEnd _ _               = Nothing

-- | Determine clump bounds for processor clump index @n@ (1-based).
-- Ported from @get_clump_bounds@ in @decompMod.F90@.
getClumpBounds :: DecompData -> Int -> BoundsType
getClumpBounds dd n =
  let pi_ = decompProcinfo dd
      cid = procCid pi_ V.! (n - 1)
      clump = decompClumps dd V.! (cid - 1)
  in BoundsType
    { begg       = clumpBegg clump      - procBegg pi_      + 1
    , endg       = clumpEndg clump      - procBegg pi_      + 1
    , begl       = clumpBegl clump      - procBegl pi_      + 1
    , endl       = clumpEndl clump      - procBegl pi_      + 1
    , begc       = clumpBegc clump      - procBegc pi_      + 1
    , endc       = clumpEndc clump      - procBegc pi_      + 1
    , begp       = clumpBegp clump      - procBegp pi_      + 1
    , endp       = clumpEndp clump      - procBegp pi_      + 1
    , begCohort  = clumpBegCohort clump - procBegCohort pi_ + 1
    , endCohort  = clumpEndCohort clump - procBegCohort pi_ + 1
    , boundsLevel = BoundsClump
    , clumpIndex = n
    }

-- | Retrieve processor bounds (1-based, relative to processor).
-- Ported from @get_proc_bounds@ in @decompMod.F90@.
getProcBounds :: DecompData -> BoundsType
getProcBounds dd =
  let pi_ = decompProcinfo dd
  in BoundsType
    { begg       = 1
    , endg       = procEndg pi_       - procBegg pi_      + 1
    , begl       = 1
    , endl       = procEndl pi_       - procBegl pi_      + 1
    , begc       = 1
    , endc       = procEndc pi_       - procBegc pi_      + 1
    , begp       = 1
    , endp       = procEndp pi_       - procBegp pi_      + 1
    , begCohort  = 1
    , endCohort  = procEndCohort pi_  - procBegCohort pi_ + 1
    , boundsLevel = BoundsProc
    , clumpIndex = -1
    }

-- | Count subgrid entities owned by a given process.
-- Returns (ncells, nlunits, ncols, npatches, nCohorts).
getProcTotal :: DecompData -> Int -> (Int, Int, Int, Int, Int)
getProcTotal dd pid =
  let owned = V.filter (\c -> clumpOwner c == pid) (decompClumps dd)
      sumF f = V.foldl' (\acc c -> acc + f c) 0 owned
  in ( sumF clumpNcells
     , sumF clumpNlunits
     , sumF clumpNcols
     , sumF clumpNpatches
     , sumF clumpNCohorts
     )

-- | Return global counts: (ng, nl, nc, np, nCohorts).
getProcGlobal :: DecompData -> (Int, Int, Int, Int, Int)
getProcGlobal dd =
  ( decompNumg dd
  , decompNuml dd
  , decompNumc dd
  , decompNump dd
  , decompNumCohort dd
  )

-- | Return the number of clumps for this processor.
getProcClumps :: DecompData -> Int
getProcClumps dd = procNclumps (decompProcinfo dd)

-- | Get 1D global size from subgrid level.
getSubgridLevelGsize :: DecompData -> SubgridLevel -> Maybe Int
getSubgridLevelGsize dd SubgridLndGrid  = Just (decompLdomainNs dd)
getSubgridLevelGsize dd SubgridGridcell = Just (decompNumg dd)
getSubgridLevelGsize dd SubgridLandunit = Just (decompNuml dd)
getSubgridLevelGsize dd SubgridColumn   = Just (decompNumc dd)
getSubgridLevelGsize dd SubgridPatch    = Just (decompNump dd)
getSubgridLevelGsize dd SubgridCohort   = Just (decompNumCohort dd)
getSubgridLevelGsize _  _              = Nothing
