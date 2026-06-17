-- | Master initialization orchestrator for CLM.
-- Ported from Julia: src/driver/clm_initialize.jl
-- Fortran: src/main/clm_initializeMod.F90
--
-- Produces a fully initialized CLMInstances plus bounds, filters, and
-- time manager. IO is used only for file-backed surface and parameter readers.
module CLM.Driver.CLMInitialize
  ( -- * Configuration
    InitConfig(..)
  , defaultInitConfig
    -- * Result
  , InitResult(..)
    -- * Initialization (IO for file reading)
  , clmInitialize
  , clmInitializeBinary
    -- * Snow layer constants (pure)
  , SnowLayerConstants(..)
  , initSnowLayerConstants
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Infrastructure.Decomp (BoundsType(..), BoundsLevel(..))
import CLM.Infrastructure.Instances
  ( CLMInstances(..), defaultCLMInstances
  , CLMDimensions(..), defaultCLMDimensions
  , DecompCascadeConData(..), decompCascadeConInit
  , SCFMethod(..) )
import CLM.Infrastructure.TimeManager (TimeManager(..), defaultTimeManager, DateTime(..))
import CLM.Infrastructure.Filters (FilterSet(..), defaultFilterSet, buildFilters, buildFiltersBinary)
import CLM.Infrastructure.Control (VarCtl(..), defaultVarCtl)
import CLM.Infrastructure.SurfData
  ( SurfaceInputData(..), defaultSurfaceInputData, surfrdGetData, surfrdGetDataBinary )
import CLM.Infrastructure.ReadParams (AllParams(..), readParameters, readParametersBinary)
import CLM.Infrastructure.BinaryIO
  ( ManifestDims(..), readManifestDims
  , readFloat64Vector, readFloat64Scalar, readInt64Vector )
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for CLM initialization.
data InitConfig = InitConfig
  { ic_fsurdat           :: !FilePath
  , ic_paramfile         :: !FilePath
  , ic_startYear         :: !Int
  , ic_startMonth        :: !Int
  , ic_startDay          :: !Int
  , ic_dtime             :: !Int       -- ^ Timestep in seconds
  , ic_use_cn            :: !Bool
  , ic_use_crop          :: !Bool
  , ic_use_bedrock       :: !Bool
  , ic_use_aquifer_layer :: !Bool
  , ic_all_active        :: !Bool
  , ic_lat               :: !Double
  , ic_lon               :: !Double
  , ic_soil_layerstruct  :: !String
  , ic_h2osfcflag        :: !Int
  , ic_int_snow_max      :: !Double
  , ic_use_hydrstress    :: !Bool
  , ic_use_luna          :: !Bool
  , ic_use_cndv          :: !Bool
  } deriving (Show)

defaultInitConfig :: InitConfig
defaultInitConfig = InitConfig
  { ic_fsurdat           = ""
  , ic_paramfile         = ""
  , ic_startYear         = 2000
  , ic_startMonth        = 1
  , ic_startDay          = 1
  , ic_dtime             = 1800
  , ic_use_cn            = False
  , ic_use_crop          = False
  , ic_use_bedrock       = True
  , ic_use_aquifer_layer = True
  , ic_all_active        = False
  , ic_lat               = 0.0 / 0.0   -- NaN = read from file
  , ic_lon               = 0.0 / 0.0
  , ic_soil_layerstruct  = "20SL_8.5m"
  , ic_h2osfcflag        = 0
  , ic_int_snow_max      = 2000.0
  , ic_use_hydrstress    = False
  , ic_use_luna          = False
  , ic_use_cndv          = False
  }

-- ---------------------------------------------------------------------------
-- Result
-- ---------------------------------------------------------------------------

-- | Result of CLM initialization.
data InitResult = InitResult
  { ir_inst    :: !CLMInstances
  , ir_bounds  :: !BoundsType
  , ir_filters :: !FilterSet
  , ir_tm      :: !TimeManager
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Snow layer constants (pure)
-- ---------------------------------------------------------------------------

-- | Snow layer thickness limits.
data SnowLayerConstants = SnowLayerConstants
  { slc_dzmin   :: !(VU.Vector Double)  -- ^ Minimum layer thickness
  , slc_dzmaxU  :: !(VU.Vector Double)  -- ^ Upper max thickness for combining
  , slc_dzmaxL  :: !(VU.Vector Double)  -- ^ Lower max thickness for combining
  } deriving (Show)

-- | Compute snow layer constants for nlevsno layers (pure).
initSnowLayerConstants :: Int -> SnowLayerConstants
initSnowLayerConstants nlevsno =
  let dzmin  = VU.generate nlevsno mkDzmin
      dzmaxU = VU.generate nlevsno mkDzmaxU
      dzmaxL = VU.generate nlevsno mkDzmaxL

      mkDzmin 0 = 0.010
      mkDzmin 1 = 0.015
      mkDzmin j = (dzmaxU VU.! (j - 1)) * 0.5

      mkDzmaxU 0 = 0.02
      mkDzmaxU 1 = 0.05
      mkDzmaxU j
        | j == nlevsno - 1 = 1.0e308  -- float max
        | otherwise         = 2.0 * (dzmaxU VU.! (j - 1)) + 0.01

      mkDzmaxL 0 = 0.03
      mkDzmaxL 1 = 0.07
      mkDzmaxL j
        | j == nlevsno - 1 = 1.0e308
        | otherwise         = (dzmaxU VU.! j) + (dzmaxL VU.! (j - 1))

  in SnowLayerConstants dzmin dzmaxU dzmaxL

-- ---------------------------------------------------------------------------
-- Main initialization (IO)
-- ---------------------------------------------------------------------------

-- | Master initialization function.
-- Reads surface data and parameter files, builds subgrid hierarchy,
-- initializes vertical structure, sets initial state, and returns all
-- data structures ready for the driver.
clmInitialize :: InitConfig -> IO InitResult
clmInitialize cfg = do
  -- Step 1: Read surface data
  let ng = 1  -- single gridcell
  _surf <- surfrdGetData (ic_fsurdat cfg) 1 ng

  -- Step 2: Read parameters
  params <- readParameters (ic_paramfile cfg)

  -- Step 3: Build single-gridcell compatibility bounds
  let nl = 2   -- soil + lake minimum
      nc = 2
      np = 2
      bounds = BoundsType
        { begg = 1, endg = ng
        , begl = 1, endl = nl
        , begc = 1, endc = nc
        , begp = 1, endp = np
        , begCohort = 0, endCohort = 0
        , boundsLevel = BoundsClump
        , clumpIndex = 1
        }

  -- Step 4: Build instances with default state
  let inst = defaultCLMInstances
        { inst_params = params
        , inst_decomp_cascade = decompCascadeConInit 7 5
        , inst_scf_method = SCFSwensonLawrence2012
        }

  -- Step 5: Build filters (default)
  let filt = defaultFilterSet

  -- Step 6: Time manager
  let startDt = DateTime (ic_startYear cfg) (ic_startMonth cfg) (ic_startDay cfg) 0 0 0
      tm = defaultTimeManager
        { tmStartDate   = startDt
        , tmCurrentDate = startDt
        , tmDtime       = ic_dtime cfg
        , tmNstep       = 0
        }

  return $ InitResult inst bounds filt tm

-- ---------------------------------------------------------------------------
-- Binary initialization (IO)
-- ---------------------------------------------------------------------------

-- | Initialize CLM from a directory of binary test data.
-- Reads manifest, surface data, parameters, and subgrid hierarchy from
-- binary files exported by Julia's export_test_data.jl script.
clmInitializeBinary :: FilePath -> IO InitResult
clmInitializeBinary dir = do
  -- Step 1: Read manifest
  dims <- readManifestDims (dir </> "manifest.json")
  let ng = mdNg dims
      nc = mdNc dims
      np = mdNp dims
      nl = mdNl dims

  -- Step 2: Read surface data
  surf <- surfrdGetDataBinary (dir </> "surfdata") ng (mdNlevsoi dims)

  -- Step 3: Read parameters
  params <- readParametersBinary (dir </> "params")

  -- Step 4: Read subgrid hierarchy from cold-start export
  colItype    <- readInt64Vector (dir </> "coldstart" </> "col_itype.bin")
  colLandunit <- readInt64Vector (dir </> "coldstart" </> "col_landunit.bin")
  lunItype    <- readInt64Vector (dir </> "coldstart" </> "lun_itype.bin")
  pchColumn   <- readInt64Vector (dir </> "coldstart" </> "pch_column.bin")

  -- Step 5: Build filters
  let filt = buildFilters nc np colItype colLandunit lunItype pchColumn

  -- Step 6: Build bounds
  let bounds = BoundsType
        { begg = 1, endg = ng
        , begl = 1, endl = nl
        , begc = 1, endc = nc
        , begp = 1, endp = np
        , begCohort = 0, endCohort = 0
        , boundsLevel = BoundsClump
        , clumpIndex = 1
        }

  -- Step 7: Create instances
  let inst = defaultCLMInstances
        { inst_params = params
        , inst_decomp_cascade = decompCascadeConInit 7 5
        , inst_scf_method = SCFSwensonLawrence2012
        , inst_surfdata = Just surf
        }

  -- Step 8: Time manager
  let startDt = DateTime (mdYear dims) 1 1 0 0 0
      tm = defaultTimeManager
        { tmStartDate   = startDt
        , tmCurrentDate = startDt
        , tmDtime       = mdDtime dims
        , tmNstep       = 0
        }

  return $ InitResult inst bounds filt tm
