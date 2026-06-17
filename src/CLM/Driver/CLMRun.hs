-- | Top-level simulation loop.
-- Ported from Julia: src/driver/clm_run.jl
-- Orchestrates initialization, forcing, driver, and output.
module CLM.Driver.CLMRun
  ( -- * Run configuration
    RunConfig(..)
  , defaultRunConfig
    -- * Driver configuration
  , CLMDriverConfig(..)
  , defaultCLMDriverConfig
    -- * Pure time-loop helpers
  , shouldAdvance
  , isSimComplete
  , StepFlags(..)
  , computeStepFlags
    -- * IO entry point
  , clmRun
  ) where

import CLM.Infrastructure.TimeManager
  ( TimeManager(..), DateTime(..), advanceTimestep
  , isBegCurrDay, isEndCurrDay, isBegCurrYear, getNstep )
import CLM.Infrastructure.Instances (CLMInstances(..), defaultCLMInstances)
import CLM.Infrastructure.Decomp (BoundsType(..))
import CLM.Infrastructure.Filters (FilterSet(..))
import CLM.Driver.CLMInitialize (InitConfig(..), defaultInitConfig, clmInitialize, InitResult(..))
import CLM.Driver.PipelineRunner
  ( runPipeline, PipelineConfig(..), defaultPipelineConfig, writeDailyCSV )
import System.FilePath ((</>))

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Configuration for the simulation loop.
data RunConfig = RunConfig
  { rc_fsurdat           :: !FilePath
  , rc_paramfile         :: !FilePath
  , rc_fforcing          :: !FilePath
  , rc_fhistory          :: !FilePath
  , rc_startYear         :: !Int
  , rc_startMonth        :: !Int
  , rc_startDay          :: !Int
  , rc_endYear           :: !Int
  , rc_endMonth          :: !Int
  , rc_endDay            :: !Int
  , rc_dtime             :: !Int       -- ^ Timestep in seconds
  , rc_use_cn            :: !Bool
  , rc_use_bedrock       :: !Bool
  , rc_use_aquifer_layer :: !Bool
  , rc_h2osfcflag        :: !Int
  , rc_verbose           :: !Bool
  , rc_frestart          :: !FilePath
  , rc_ffortran_restart  :: !FilePath
  , rc_baseflow_scalar   :: !Double
  , rc_int_snow_max      :: !Double
  , rc_use_hydrstress    :: !Bool
  , rc_stomatalcond_mtd  :: !Int       -- ^ 1=BB1987, 2=Medlyn2011
  , rc_use_luna          :: !Bool
  , rc_irrigate          :: !Bool
  , rc_use_voc           :: !Bool
  , rc_use_cndv          :: !Bool
  } deriving (Show)

defaultRunConfig :: RunConfig
defaultRunConfig = RunConfig
  { rc_fsurdat           = ""
  , rc_paramfile         = ""
  , rc_fforcing          = ""
  , rc_fhistory          = "clm_history.nc"
  , rc_startYear         = 2000
  , rc_startMonth        = 1
  , rc_startDay          = 1
  , rc_endYear           = 2000
  , rc_endMonth          = 2
  , rc_endDay            = 1
  , rc_dtime             = 1800
  , rc_use_cn            = False
  , rc_use_bedrock       = True
  , rc_use_aquifer_layer = True
  , rc_h2osfcflag        = 0
  , rc_verbose           = True
  , rc_frestart          = ""
  , rc_ffortran_restart  = ""
  , rc_baseflow_scalar   = 1.0e-2
  , rc_int_snow_max      = 2000.0
  , rc_use_hydrstress    = False
  , rc_stomatalcond_mtd  = 2     -- Medlyn2011
  , rc_use_luna          = False
  , rc_irrigate          = False
  , rc_use_voc           = False
  , rc_use_cndv          = False
  }

-- ---------------------------------------------------------------------------
-- Driver configuration
-- ---------------------------------------------------------------------------

-- | Configuration flags passed to the physics driver each timestep.
data CLMDriverConfig = CLMDriverConfig
  { drc_use_cn            :: !Bool
  , drc_use_aquifer_layer :: !Bool
  , drc_irrigate          :: !Bool
  , drc_use_voc           :: !Bool
  , drc_use_cndv          :: !Bool
  } deriving (Show)

defaultCLMDriverConfig :: CLMDriverConfig
defaultCLMDriverConfig = CLMDriverConfig False True False False False

-- ---------------------------------------------------------------------------
-- Pure time-loop helpers
-- ---------------------------------------------------------------------------

-- | Check whether the simulation should advance (current date < end date).
shouldAdvance :: DateTime -> DateTime -> Bool
shouldAdvance current end = dtToOrd current < dtToOrd end
  where
    dtToOrd dt = (dtYear dt, dtMonth dt, dtDay dt, dtHour dt, dtMinute dt, dtSecond dt)

-- | Check whether the simulation is complete.
isSimComplete :: DateTime -> DateTime -> Bool
isSimComplete current end = not (shouldAdvance current end)

-- | Time flags computed at the beginning of each step.
data StepFlags = StepFlags
  { sf_firstStep   :: !Bool
  , sf_begDay      :: !Bool
  , sf_endDay      :: !Bool
  , sf_begYear     :: !Bool
  , sf_doAlb       :: !Bool  -- ^ Compute albedo this step
  } deriving (Show)

-- | Compute time flags for the current step.
computeStepFlags :: TimeManager -> StepFlags
computeStepFlags tm = StepFlags
  { sf_firstStep = tmNstep tm == 1
  , sf_begDay    = isBegCurrDay tm
  , sf_endDay    = isEndCurrDay tm
  , sf_begYear   = isBegCurrYear tm
  , sf_doAlb     = True  -- compute albedo every step for simplicity
  }

-- ---------------------------------------------------------------------------
-- Main simulation loop
-- ---------------------------------------------------------------------------

-- | Run a complete CLM simulation from initialization through time integration.
-- This is the top-level entry point for offline CLM simulations.
clmRun :: RunConfig -> IO CLMInstances
clmRun rc = do
  let initCfg = defaultInitConfig
        { ic_fsurdat           = rc_fsurdat rc
        , ic_paramfile         = rc_paramfile rc
        , ic_startYear         = rc_startYear rc
        , ic_startMonth        = rc_startMonth rc
        , ic_startDay          = rc_startDay rc
        , ic_dtime             = rc_dtime rc
        , ic_use_cn            = rc_use_cn rc
        , ic_use_bedrock       = rc_use_bedrock rc
        , ic_use_aquifer_layer = rc_use_aquifer_layer rc
        , ic_h2osfcflag        = rc_h2osfcflag rc
        , ic_int_snow_max      = rc_int_snow_max rc
        , ic_use_hydrstress    = rc_use_hydrstress rc
        , ic_use_luna          = rc_use_luna rc
        , ic_use_cndv          = rc_use_cndv rc
        }
  initRes <- clmInitialize initCfg

  let pipeCfg = defaultPipelineConfig
        { pcDtime     = fromIntegral (rc_dtime rc)
        , pcNdays     = runLengthDays rc
        , pcDataDir   = if null (rc_fforcing rc) then pcDataDir defaultPipelineConfig else rc_fforcing rc
        , pcVerbose   = rc_verbose rc
        , pcUseCN     = rc_use_cn rc
        , pcOutputCSV = rc_fhistory rc
        }

  dailies <- runPipeline pipeCfg
  if null (rc_fhistory rc)
    then return ()
    else writeDailyCSV (rc_fhistory rc) dailies

  return $ ir_inst initRes

runLengthDays :: RunConfig -> Int
runLengthDays rc =
  max 1 $
    dateOrdinal (DateTime (rc_endYear rc) (rc_endMonth rc) (rc_endDay rc) 0 0 0)
    - dateOrdinal (DateTime (rc_startYear rc) (rc_startMonth rc) (rc_startDay rc) 0 0 0)

dateOrdinal :: DateTime -> Int
dateOrdinal dt =
  (dtYear dt * 365) + monthOffset (dtMonth dt) + dtDay dt
  where
    monthOffset m = sum (map monthDays [1 .. m - 1])
    monthDays  1 = 31
    monthDays  2 = 28
    monthDays  3 = 31
    monthDays  4 = 30
    monthDays  5 = 31
    monthDays  6 = 30
    monthDays  7 = 31
    monthDays  8 = 31
    monthDays  9 = 30
    monthDays 10 = 31
    monthDays 11 = 30
    monthDays 12 = 31
    monthDays _  = 30
