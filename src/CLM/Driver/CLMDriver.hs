-- | Main CLM timestep driver.
-- Ported from Julia: src/driver/clm_driver.jl
-- Fortran: clm_driver.F90
--
-- Orchestrates the physics calling sequence:
--   phenology -> albedo -> canopy fluxes -> soil temp -> hydrology -> snow -> history
--
-- Since most physics modules are stubs, this module defines the driver as a
-- pipeline of typed function slots. The type signatures specify the interface
-- contract even when implementations are not yet wired.
module CLM.Driver.CLMDriver
  ( -- * Driver state
    CLMDriverState(..)
  , defaultDriverState
    -- * Timestep context
  , TimestepContext(..)
  , defaultTimestepContext
    -- * Physics pipeline types
  , PhysicsStep
  , PhysicsPipeline(..)
  , defaultPhysicsPipeline
    -- * Main driver
  , clmDrv
    -- * Driver sub-phases
  , clmDrvInit
  , clmDrvPatch2Col
    -- * CLM model state (aggregate)
  , CLMState(..)
  , defaultCLMState
    -- * Re-exported types for convenience
  , module CLM.Types.ColumnData
  , module CLM.Types.PatchData
  , module CLM.Types.LandunitData
  , module CLM.Types.GridcellData
  , module CLM.Types.TemperatureData
  , module CLM.Types.WaterStateData
  , module CLM.Types.WaterFluxData
  , module CLM.Types.WaterFluxBulkData
  , module CLM.Types.WaterStateBulkData
  , module CLM.Types.WaterDiagnosticBulkData
  , module CLM.Types.WaterBalanceData
  , module CLM.Types.EnergyFluxData
  , module CLM.Types.SolarAbsorbedData
  , module CLM.Types.CanopyStateData
  , module CLM.Types.FrictionVelocityData
  , module CLM.Types.SoilStateData
  , module CLM.Types.SoilHydrologyData
  , module CLM.Types.LakeStateData
  , module CLM.Types.Atm2LndData
  , module CLM.Types.Lnd2AtmData
  , module CLM.Types.UrbanParamsData
    -- * Utilities
  , computeSpecificHumidity
  , writeDiagnostic
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.ControlFlags
  ( CLMDriverConfig(..) )
import CLM.Types.ColumnData      (ColumnData(..), defaultColumnData)
import CLM.Types.PatchData       (PatchData(..), defaultPatchData)
import CLM.Types.LandunitData    (LandunitData(..), defaultLandunitData)
import CLM.Types.GridcellData    (GridcellData(..), defaultGridcellData)
import CLM.Types.TemperatureData (TemperatureData(..), defaultTemperatureData)
import CLM.Types.WaterStateData  (WaterStateData(..), defaultWaterStateData)
import CLM.Types.WaterFluxData   (WaterFluxData(..), defaultWaterFluxData)
import CLM.Types.WaterFluxBulkData (WaterFluxBulkData(..), defaultWaterFluxBulkData)
import CLM.Types.WaterStateBulkData (WaterStateBulkData(..), defaultWaterStateBulkData)
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..), defaultWaterDiagnosticBulkData)
import CLM.Types.WaterBalanceData (WaterBalanceData(..), defaultWaterBalanceData)
import CLM.Types.EnergyFluxData  (EnergyFluxData(..), defaultEnergyFluxData)
import CLM.Types.SolarAbsorbedData (SolarAbsorbedData(..), defaultSolarAbsorbedData)
import CLM.Types.CanopyStateData (CanopyStateData(..), defaultCanopyStateData)
import CLM.Types.FrictionVelocityData (FrictionVelocityData(..), defaultFrictionVelocityData)
import CLM.Types.SoilStateData   (SoilStateData(..), defaultSoilStateData)
import CLM.Types.SoilHydrologyData (SoilHydrologyData(..), defaultSoilHydrologyData)
import CLM.Types.LakeStateData   (LakeStateData(..), defaultLakeStateData)
import CLM.Types.Atm2LndData     (Atm2LndData(..), defaultAtm2LndData)
import CLM.Types.Lnd2AtmData     (Lnd2AtmData(..), defaultLnd2AtmData)
import CLM.Types.UrbanParamsData (UrbanParamsData(..), defaultUrbanParamsData)
import CLM.Infrastructure.Filters (FilterSet(..), defaultFilterSet)

-- ============================================================================
-- Driver state (mutable across timesteps in the imperative version;
-- here threaded through the pipeline).
-- ============================================================================

-- | Driver-level state tracked across timesteps.
data CLMDriverState = CLMDriverState
  { dsNstep       :: !Int     -- ^ Current timestep number
  , dsYear        :: !Int     -- ^ Current year
  , dsMonth       :: !Int     -- ^ Current month (1-12)
  , dsDay         :: !Int     -- ^ Current day of month
  , dsSec         :: !Int     -- ^ Seconds within current day
  } deriving (Show, Eq)

defaultDriverState :: CLMDriverState
defaultDriverState = CLMDriverState
  { dsNstep = 0
  , dsYear  = 2002
  , dsMonth = 1
  , dsDay   = 1
  , dsSec   = 0
  }

-- ============================================================================
-- Timestep context (read-only inputs for a single timestep)
-- ============================================================================

-- | Read-only context for a single CLM timestep.
data TimestepContext = TimestepContext
  { tcDoAlb          :: !Bool     -- ^ Compute surface albedo this step?
  , tcDtime          :: !Double   -- ^ Timestep length [s]
  , tcNextswCday     :: !Double   -- ^ Calendar day for next shortwave step
  , tcDeclinP1       :: !Double   -- ^ Solar declination for next step [rad]
  , tcDeclin         :: !Double   -- ^ Solar declination for current step [rad]
  , tcObliqr         :: !Double   -- ^ Earth obliquity [rad]
  , tcIsFirstStep    :: !Bool
  , tcIsBegCurrDay   :: !Bool
  , tcIsEndCurrDay   :: !Bool
  , tcIsBegCurrYear  :: !Bool
  -- Atmospheric forcing (column-level, downscaled)
  , tcForcT          :: !(VU.Vector Double)  -- ^ Air temperature [K]
  , tcForcTh         :: !(VU.Vector Double)  -- ^ Potential temperature [K]
  , tcForcQ          :: !(VU.Vector Double)  -- ^ Specific humidity [kg/kg]
  , tcForcPbot       :: !(VU.Vector Double)  -- ^ Surface pressure [Pa]
  , tcForcRho        :: !(VU.Vector Double)  -- ^ Air density [kg/m^3]
  , tcForcRain       :: !(VU.Vector Double)  -- ^ Rainfall rate [mm/s]
  , tcForcSnow       :: !(VU.Vector Double)  -- ^ Snowfall rate [mm/s]
  , tcForcLwrad      :: !(VU.Vector Double)  -- ^ Downward longwave [W/m^2]
  , tcForcSolad      :: !(VU.Vector Double)  -- ^ Direct solar by band [W/m^2]
  , tcForcSolai      :: !(VU.Vector Double)  -- ^ Diffuse solar by band [W/m^2]
  , tcForcWind       :: !(VU.Vector Double)  -- ^ Wind speed [m/s]
  } deriving (Show)

defaultTimestepContext :: TimestepContext
defaultTimestepContext = TimestepContext
  { tcDoAlb         = True
  , tcDtime         = 1800.0
  , tcNextswCday    = 1.0
  , tcDeclinP1      = 0.0
  , tcDeclin        = 0.0
  , tcObliqr        = 0.4091
  , tcIsFirstStep   = False
  , tcIsBegCurrDay  = False
  , tcIsEndCurrDay  = False
  , tcIsBegCurrYear = False
  , tcForcT         = VU.empty
  , tcForcTh        = VU.empty
  , tcForcQ         = VU.empty
  , tcForcPbot      = VU.empty
  , tcForcRho       = VU.empty
  , tcForcRain      = VU.empty
  , tcForcSnow      = VU.empty
  , tcForcLwrad     = VU.empty
  , tcForcSolad     = VU.empty
  , tcForcSolai     = VU.empty
  , tcForcWind      = VU.empty
  }

-- ============================================================================
-- Aggregate CLM model state
-- ============================================================================

-- | Aggregate model state threaded through the physics pipeline.
-- In the imperative Julia/Fortran version these are mutable fields on
-- CLMInstances; here they form a product type that is transformed
-- by each physics step.
data CLMState = CLMState
  { -- Subgrid structure
    clmColumn      :: !ColumnData
  , clmPatch       :: !PatchData
  , clmLandunit    :: !LandunitData
  , clmGridcell    :: !GridcellData
    -- Temperature and energy
  , clmTemp        :: !TemperatureData
  , clmEnergyFlux  :: !EnergyFluxData
  , clmSolarAbs    :: !SolarAbsorbedData
    -- Water state and fluxes
  , clmWaterState  :: !WaterStateData
  , clmWaterFlux   :: !WaterFluxData
  , clmWaterFluxBulk :: !WaterFluxBulkData
  , clmWaterStateBulk :: !WaterStateBulkData
  , clmWaterDiagBulk :: !WaterDiagnosticBulkData
  , clmWaterBalance :: !WaterBalanceData
    -- Surface and canopy
  , clmCanopyState :: !CanopyStateData
  , clmFrictionVel :: !FrictionVelocityData
    -- Soil
  , clmSoilState   :: !SoilStateData
  , clmSoilHydro   :: !SoilHydrologyData
    -- Lake
  , clmLakeState   :: !LakeStateData
    -- Atmosphere coupling
  , clmAtm2Lnd     :: !Atm2LndData
  , clmLnd2Atm     :: !Lnd2AtmData
    -- Urban
  , clmUrbanParams :: !UrbanParamsData
    -- Filters and snow layer count
  , clmFilters     :: !FilterSet
  , clmSnl         :: !Int              -- ^ Number of snow layers (0 to -nlevsno)
  } deriving (Show)

defaultCLMState :: CLMState
defaultCLMState = CLMState
  { clmColumn       = defaultColumnData
  , clmPatch        = defaultPatchData
  , clmLandunit     = defaultLandunitData
  , clmGridcell     = defaultGridcellData
  , clmTemp         = defaultTemperatureData
  , clmEnergyFlux   = defaultEnergyFluxData
  , clmSolarAbs     = defaultSolarAbsorbedData
  , clmWaterState   = defaultWaterStateData
  , clmWaterFlux    = defaultWaterFluxData
  , clmWaterFluxBulk  = defaultWaterFluxBulkData
  , clmWaterStateBulk = defaultWaterStateBulkData
  , clmWaterDiagBulk  = defaultWaterDiagnosticBulkData
  , clmWaterBalance   = defaultWaterBalanceData
  , clmCanopyState  = defaultCanopyStateData
  , clmFrictionVel  = defaultFrictionVelocityData
  , clmSoilState    = defaultSoilStateData
  , clmSoilHydro    = defaultSoilHydrologyData
  , clmLakeState    = defaultLakeStateData
  , clmAtm2Lnd      = defaultAtm2LndData
  , clmLnd2Atm      = defaultLnd2AtmData
  , clmUrbanParams  = defaultUrbanParamsData
  , clmFilters      = defaultFilterSet
  , clmSnl          = 0
  }

-- ============================================================================
-- Physics step type and pipeline
-- ============================================================================

-- | A single physics step: given config, context, and current state,
-- produce updated state.  Pure function signature.
type PhysicsStep = CLMDriverConfig -> TimestepContext -> CLMState -> CLMState

-- | The full physics pipeline: an ordered collection of named steps.
-- Each field holds a function with the 'PhysicsStep' signature.
-- Stub implementations are identity (no-op); real implementations
-- are plugged in as they are ported.
data PhysicsPipeline = PhysicsPipeline
  { -- Phase 1: Initialization
    ppDrvInit              :: !PhysicsStep
    -- Phase 2: Surface radiation
  , ppCanopyInterception   :: !PhysicsStep
  , ppHandleNewSnow        :: !PhysicsStep
  , ppSurfaceRadiation     :: !PhysicsStep
    -- Phase 3: Pre-flux calculations
  , ppPreFluxCalcs         :: !PhysicsStep
  , ppSurfaceHumidity      :: !PhysicsStep
    -- Phase 4: Fluxes
  , ppBaregroundFluxes     :: !PhysicsStep
  , ppCanopyFluxes         :: !PhysicsStep
  , ppLakeFluxes           :: !PhysicsStep
    -- Phase 5: Temperatures
  , ppSoilTemperature      :: !PhysicsStep
  , ppLakeTemperature      :: !PhysicsStep
  , ppSoilFluxes           :: !PhysicsStep
    -- Phase 6: Hydrology
  , ppSnowWater            :: !PhysicsStep
  , ppSoilHydrology        :: !PhysicsStep
  , ppWaterTable           :: !PhysicsStep
    -- Phase 7: Snow management
  , ppSnowCompaction       :: !PhysicsStep
  , ppSnowLayerCombine     :: !PhysicsStep
  , ppSnowLayerDivide      :: !PhysicsStep
  , ppSnowAging            :: !PhysicsStep
    -- Phase 8: Hydrology drainage
  , ppHydrologyDrainage    :: !PhysicsStep
    -- Phase 9: Balance and diagnostics
  , ppWaterBalance         :: !PhysicsStep
  , ppEnergyBalance        :: !PhysicsStep
    -- Phase 10: Albedo for next step
  , ppSurfaceAlbedo        :: !PhysicsStep
  }

-- | Identity physics step (no-op stub).
idStep :: PhysicsStep
idStep _cfg _ctx st = st

-- | Default pipeline: all steps are no-ops.
defaultPhysicsPipeline :: PhysicsPipeline
defaultPhysicsPipeline = PhysicsPipeline
  { ppDrvInit            = idStep
  , ppCanopyInterception = idStep
  , ppHandleNewSnow      = idStep
  , ppSurfaceRadiation   = idStep
  , ppPreFluxCalcs       = idStep
  , ppSurfaceHumidity    = idStep
  , ppBaregroundFluxes   = idStep
  , ppCanopyFluxes       = idStep
  , ppLakeFluxes         = idStep
  , ppSoilTemperature    = idStep
  , ppLakeTemperature    = idStep
  , ppSoilFluxes         = idStep
  , ppSnowWater          = idStep
  , ppSoilHydrology      = idStep
  , ppWaterTable         = idStep
  , ppSnowCompaction     = idStep
  , ppSnowLayerCombine   = idStep
  , ppSnowLayerDivide    = idStep
  , ppSnowAging          = idStep
  , ppHydrologyDrainage  = idStep
  , ppWaterBalance       = idStep
  , ppEnergyBalance      = idStep
  , ppSurfaceAlbedo      = idStep
  }

-- ============================================================================
-- Driver initialization (clm_drv_init)
-- ============================================================================

-- | Initialize driver variables from previous timestep.
-- Sets frac_veg_nosno, computes ice fraction of snow layers,
-- resets bottom heat flux.
--
-- Corresponds to clm_drv_init in clm_driver.F90.
clmDrvInit :: CLMDriverConfig -> TimestepContext -> CLMState -> CLMState
clmDrvInit _cfg _ctx st = st
  { clmEnergyFlux = (clmEnergyFlux st)
      { eflx_soil_grnd_col = 0.0 }  -- Reset bottom heat flux
  -- In full implementation:
  -- 1. Reset intracellular CO2 parameters (cisun_z, cisha_z = -999)
  -- 2. Set frac_veg_nosno from frac_veg_nosno_alb (patch level)
  -- 3. Compute frac_iceold from h2osoi_liq/h2osoi_ice for snow layers
  }

-- ============================================================================
-- Patch to column averaging (clm_drv_patch2col)
-- ============================================================================

-- | Average patch-level fluxes to column level.
-- In the single-column/single-patch case this is identity.
-- With multiple patches per column, computes weighted averages
-- for evaporation, transpiration, and other flux fields.
--
-- Corresponds to clm_drv_patch2col in clm_driver.F90.
clmDrvPatch2Col :: CLMState -> CLMState
clmDrvPatch2Col st = st
  -- In full implementation: weighted average of patch fluxes → column fluxes
  -- for qflx_ev_snow, qflx_ev_soil, qflx_ev_h2osfc,
  -- qflx_evap_soi, qflx_evap_tot, qflx_tran_veg, etc.

-- ============================================================================
-- Main driver: clm_drv
-- ============================================================================

-- | Main CLM driver — single timestep of the physics pipeline.
--
-- Executes the full CLM physics calling sequence:
--
-- 1. Driver initialization (save previous-step state)
-- 2. Canopy interception and throughfall
-- 3. New snow handling
-- 4. Surface radiation (two-stream + SNICAR)
-- 5. Pre-flux calculations (roughness, stability)
-- 6. Surface humidity
-- 7. Bare ground fluxes
-- 8. Canopy fluxes (Monin-Obukhov + stomatal resistance)
-- 9. Lake fluxes
-- 10. Soil temperature (Crank-Nicolson diffusion + phase change)
-- 11. Lake temperature
-- 12. Soil/surface fluxes for new ground temperature
-- 13. Patch-to-column averaging
-- 14. Snow water (percolation, sublimation, melt)
-- 15. Soil hydrology (Richards equation)
-- 16. Water table
-- 17. Snow compaction
-- 18. Snow layer combine/divide
-- 19. Snow aging (grain growth)
-- 20. Hydrology drainage
-- 21. Water balance check
-- 22. Energy balance check
-- 23. Surface albedo for next step (if doAlb)
--
-- Each step is a slot in the 'PhysicsPipeline'. Stub implementations
-- pass state through unchanged; real implementations are plugged in
-- as modules are ported.
--
-- Returns updated (CLMDriverState, CLMState).
clmDrv
  :: CLMDriverConfig
  -> PhysicsPipeline
  -> TimestepContext
  -> CLMDriverState
  -> CLMState
  -> (CLMDriverState, CLMState)
clmDrv cfg pipeline ctx drvState st0 =
  let
    -- Apply each phase in sequence
    apply step = step cfg ctx

    -- Phase 1: Driver init
    st1  = apply (ppDrvInit pipeline) st0

    -- Phase 2: Hydrology stage 1
    st2  = apply (ppCanopyInterception pipeline) st1
    st3  = apply (ppHandleNewSnow pipeline) st2

    -- Phase 3: Surface radiation
    st4  = apply (ppSurfaceRadiation pipeline) st3

    -- Phase 4: Pre-flux calculations
    st5  = apply (ppPreFluxCalcs pipeline) st4
    st6  = apply (ppSurfaceHumidity pipeline) st5

    -- Phase 5: Determine fluxes
    st7  = apply (ppBaregroundFluxes pipeline) st6
    st8  = apply (ppCanopyFluxes pipeline) st7
    st9  = apply (ppLakeFluxes pipeline) st8

    -- Phase 6: Determine temperatures
    st10 = apply (ppSoilTemperature pipeline) st9
    st11 = apply (ppLakeTemperature pipeline) st10
    st12 = apply (ppSoilFluxes pipeline) st11

    -- Phase 6b: Patch to column averaging
    st13 = clmDrvPatch2Col st12

    -- Phase 7: Hydrology stage 2 (snow water, soil water, water table)
    st14 = apply (ppSnowWater pipeline) st13
    st15 = apply (ppSoilHydrology pipeline) st14
    st16 = apply (ppWaterTable pipeline) st15

    -- Phase 8: Snow management
    st17 = apply (ppSnowCompaction pipeline) st16
    st18 = apply (ppSnowLayerCombine pipeline) st17
    st19 = apply (ppSnowLayerDivide pipeline) st18
    st20 = apply (ppSnowAging pipeline) st19

    -- Phase 9: Drainage
    st21 = apply (ppHydrologyDrainage pipeline) st20

    -- Phase 10: Balance checks
    st22 = apply (ppWaterBalance pipeline) st21
    st23 = apply (ppEnergyBalance pipeline) st22

    -- Phase 11: Albedo for next step
    st24 = if tcDoAlb ctx
           then apply (ppSurfaceAlbedo pipeline) st23
           else st23

    -- Advance driver state
    drvState' = advanceDriverState drvState (tcDtime ctx)
  in
    (drvState', st24)

-- | Advance the driver state by one timestep.
advanceDriverState :: CLMDriverState -> Double -> CLMDriverState
advanceDriverState ds dtime =
  let newSec = dsSec ds + round dtime
      (extraDays, remainSec) = newSec `divMod` (86400 :: Int)
      -- Simplified date advance (does not handle month/year rollover properly;
      -- a real implementation would use a calendar module)
      newDay = dsDay ds + extraDays
  in  ds { dsNstep = dsNstep ds + 1
         , dsSec   = remainSec
         , dsDay   = newDay
         }

-- ============================================================================
-- Specific humidity computation (utility)
-- ============================================================================

-- | Compute specific humidity from vapor pressure and surface pressure.
-- q = 0.622 * e / max(p - 0.378 * e, 1.0)
--
-- Used to derive column-level specific humidity from gridcell-level
-- forc_vp and column-level forc_pbot.
computeSpecificHumidity
  :: Double   -- ^ Vapor pressure [Pa]
  -> Double   -- ^ Surface pressure [Pa]
  -> Double   -- ^ Specific humidity [kg/kg]
computeSpecificHumidity vp pbot =
  0.622 * vp / max (pbot - 0.378 * vp) 1.0

-- ============================================================================
-- Write diagnostic (pure version returns a message string)
-- ============================================================================

-- | Generate a diagnostic message for the completed timestep.
writeDiagnostic :: Int -> String
writeDiagnostic nstep = "clm: completed timestep " ++ show nstep
