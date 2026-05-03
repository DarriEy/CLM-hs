-- | CLMInstances container aggregating all model state.
-- Ported from Julia: src/infrastructure/instances.jl
-- Fortran: src/main/clm_instMod.F90
module CLM.Infrastructure.Instances
  ( -- * Instance container
    CLMInstances(..)
  , defaultCLMInstances
    -- * Initialization dimensions
  , CLMDimensions(..)
  , defaultCLMDimensions
    -- * Decomposition cascade configuration
  , DecompCascadeConData(..)
  , defaultDecompCascadeConData
  , decompCascadeConInit
    -- * SCF method tag
  , SCFMethod(..)
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

import CLM.Infrastructure.Decomp (BoundsType(..))
import CLM.Infrastructure.Topo (TopoData(..), defaultTopoData)
import CLM.Infrastructure.ReadParams (AllParams(..), defaultAllParams)
import CLM.Infrastructure.SurfData (SurfaceInputData(..), defaultSurfaceInputData)
import CLM.Types.TemperatureData (TemperatureData(..), defaultTemperatureData)
import CLM.Types.EnergyFluxData (EnergyFluxData(..), defaultEnergyFluxData)
import CLM.Types.CanopyStateData (CanopyStateData(..), defaultCanopyStateData)
import CLM.Types.SoilStateData (SoilStateData(..), defaultSoilStateData)
import CLM.Types.FrictionVelocityData (FrictionVelocityData(..), defaultFrictionVelocityData)
import CLM.Types.LakeStateData (LakeStateData(..), defaultLakeStateData)
import CLM.Types.Atm2LndData (Atm2LndData(..), defaultAtm2LndData)
import CLM.Types.WaterStateBulkData (WaterStateBulkData(..), defaultWaterStateBulkData)
import CLM.Types.WaterFluxBulkData (WaterFluxBulkData(..), defaultWaterFluxBulkData)
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..), defaultWaterDiagnosticBulkData)
import CLM.Types.SolarAbsorbedData (SolarAbsorbedData(..), defaultSolarAbsorbedData)

-- ---------------------------------------------------------------------------
-- Dimension configuration
-- ---------------------------------------------------------------------------

-- | Grid dimensions for CLM initialization.
data CLMDimensions = CLMDimensions
  { dim_ng                           :: !Int  -- ^ Number of gridcells
  , dim_nl                           :: !Int  -- ^ Number of landunits
  , dim_nc                           :: !Int  -- ^ Number of columns
  , dim_np                           :: !Int  -- ^ Number of patches
  , dim_nlevdecomp_full              :: !Int  -- ^ Full decomposition levels
  , dim_ndecomp_pools                :: !Int  -- ^ Decomposition pools
  , dim_ndecomp_cascade_transitions  :: !Int  -- ^ Cascade transitions
  } deriving (Show)

defaultCLMDimensions :: CLMDimensions
defaultCLMDimensions = CLMDimensions 1 1 1 1 10 7 5

-- ---------------------------------------------------------------------------
-- Decomposition cascade configuration
-- ---------------------------------------------------------------------------

data DecompCascadeConData = DecompCascadeConData
  { dcc_cascade_donor_pool             :: !(VU.Vector Int)
  , dcc_cascade_receiver_pool          :: !(VU.Vector Int)
  , dcc_floating_cn_ratio_decomp_pools :: !(VU.Vector Bool)
  , dcc_is_litter                      :: !(VU.Vector Bool)
  , dcc_is_soil                        :: !(VU.Vector Bool)
  , dcc_is_cwd                         :: !(VU.Vector Bool)
  , dcc_initial_cn_ratio               :: !(VU.Vector Double)
  , dcc_initial_stock                  :: !(VU.Vector Double)
  , dcc_is_metabolic                   :: !(VU.Vector Bool)
  , dcc_is_cellulose                   :: !(VU.Vector Bool)
  , dcc_is_lignin                      :: !(VU.Vector Bool)
  , dcc_spinup_factor                  :: !(VU.Vector Double)
  , dcc_decomp_pool_name_restart       :: !(V.Vector String)
  , dcc_decomp_pool_name_history       :: !(V.Vector String)
  , dcc_decomp_pool_name_long          :: !(V.Vector String)
  , dcc_decomp_pool_name_short         :: !(V.Vector String)
  , dcc_cascade_step_name              :: !(V.Vector String)
  } deriving (Show)

defaultDecompCascadeConData :: DecompCascadeConData
defaultDecompCascadeConData = DecompCascadeConData
  { dcc_cascade_donor_pool = VU.empty
  , dcc_cascade_receiver_pool = VU.empty
  , dcc_floating_cn_ratio_decomp_pools = VU.empty
  , dcc_is_litter = VU.empty
  , dcc_is_soil = VU.empty
  , dcc_is_cwd = VU.empty
  , dcc_initial_cn_ratio = VU.empty
  , dcc_initial_stock = VU.empty
  , dcc_is_metabolic = VU.empty
  , dcc_is_cellulose = VU.empty
  , dcc_is_lignin = VU.empty
  , dcc_spinup_factor = VU.empty
  , dcc_decomp_pool_name_restart = V.empty
  , dcc_decomp_pool_name_history = V.empty
  , dcc_decomp_pool_name_long = V.empty
  , dcc_decomp_pool_name_short = V.empty
  , dcc_cascade_step_name = V.empty
  }

-- | Initialize decomposition cascade arrays with given sizes.
decompCascadeConInit :: Int -> Int -> DecompCascadeConData
decompCascadeConInit nPools nTransitions = DecompCascadeConData
  { dcc_cascade_donor_pool     = VU.replicate nTransitions 0
  , dcc_cascade_receiver_pool  = VU.replicate nTransitions 0
  , dcc_floating_cn_ratio_decomp_pools = VU.replicate nPools False
  , dcc_is_litter              = VU.replicate nPools False
  , dcc_is_soil                = VU.replicate nPools False
  , dcc_is_cwd                 = VU.replicate nPools False
  , dcc_initial_cn_ratio       = VU.replicate nPools 0.0
  , dcc_initial_stock          = VU.replicate nPools 0.0
  , dcc_is_metabolic           = VU.replicate nPools False
  , dcc_is_cellulose           = VU.replicate nPools False
  , dcc_is_lignin              = VU.replicate nPools False
  , dcc_spinup_factor          = VU.replicate nPools 1.0
  , dcc_decomp_pool_name_restart = V.replicate nPools ""
  , dcc_decomp_pool_name_history = V.replicate nPools ""
  , dcc_decomp_pool_name_long   = V.replicate nPools ""
  , dcc_decomp_pool_name_short  = V.replicate nPools ""
  , dcc_cascade_step_name       = V.replicate nTransitions ""
  }

-- ---------------------------------------------------------------------------
-- SCF method tag
-- ---------------------------------------------------------------------------

-- | Snow cover fraction method discriminator.
data SCFMethod = SCFSwensonLawrence2012 | SCFNiuYang2007
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- CLMInstances container
-- ---------------------------------------------------------------------------

-- | Container holding all CLM data type instances.
-- Corresponds to the module-level variables declared in clm_instMod.F90.
data CLMInstances = CLMInstances
  { -- Grid hierarchy
    inst_temperature         :: !TemperatureData
  , inst_energyflux          :: !EnergyFluxData
  , inst_canopystate         :: !CanopyStateData
  , inst_soilstate           :: !SoilStateData
  , inst_frictionvel         :: !FrictionVelocityData
  , inst_lakestate           :: !LakeStateData
  , inst_solarabs            :: !SolarAbsorbedData
  , inst_atm2lnd             :: !Atm2LndData
  , inst_waterstatebulk      :: !WaterStateBulkData
  , inst_waterfluxbulk       :: !WaterFluxBulkData
  , inst_waterdiagbulk       :: !WaterDiagnosticBulkData
    -- Topography
  , inst_topo                :: !TopoData
    -- Parameters
  , inst_params              :: !AllParams
    -- Decomposition cascade
  , inst_decomp_cascade      :: !DecompCascadeConData
    -- SCF method
  , inst_scf_method          :: !SCFMethod
    -- Surface data (for monthly phenology re-reads)
  , inst_surfdata            :: !(Maybe SurfaceInputData)
  } deriving (Show)

defaultCLMInstances :: CLMInstances
defaultCLMInstances = CLMInstances
  { inst_temperature    = defaultTemperatureData
  , inst_energyflux     = defaultEnergyFluxData
  , inst_canopystate    = defaultCanopyStateData
  , inst_soilstate      = defaultSoilStateData
  , inst_frictionvel    = defaultFrictionVelocityData
  , inst_lakestate      = defaultLakeStateData
  , inst_solarabs       = defaultSolarAbsorbedData
  , inst_atm2lnd        = defaultAtm2LndData
  , inst_waterstatebulk = defaultWaterStateBulkData
  , inst_waterfluxbulk  = defaultWaterFluxBulkData
  , inst_waterdiagbulk  = defaultWaterDiagnosticBulkData
  , inst_topo           = defaultTopoData
  , inst_params         = defaultAllParams
  , inst_decomp_cascade = defaultDecompCascadeConData
  , inst_scf_method     = SCFSwensonLawrence2012
  , inst_surfdata       = Nothing
  }
