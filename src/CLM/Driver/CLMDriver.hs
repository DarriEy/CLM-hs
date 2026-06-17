-- | Main CLM timestep driver.
-- Ported from Julia: src/driver/clm_driver.jl
-- Fortran: clm_driver.F90
--
-- Orchestrates the physics calling sequence:
--   phenology -> albedo -> canopy fluxes -> soil temp -> hydrology -> snow -> history
--
-- The driver is represented as a pipeline of typed function slots. The type
-- signatures specify the interface contract for both default and wired runs.
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
  , clmDrvBoundaries
  , BoundarySnapshots(..)
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
  , tcForcHgt        :: !Double              -- ^ Forcing reference height [m]
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
  , tcForcHgt       = 30.0
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
    -- CN Biogeochemistry state
  , clmCNActive    :: !Bool             -- ^ Whether CN biogeochemistry is active
  , clmLeafC       :: !Double           -- ^ Leaf carbon pool (gC/m2)
  , clmFrootC      :: !Double           -- ^ Fine root carbon pool (gC/m2)
  , clmLiveStemC   :: !Double           -- ^ Live stem carbon pool (gC/m2)
  , clmDeadStemC   :: !Double           -- ^ Dead stem carbon pool (gC/m2)
  , clmCPool       :: !Double           -- ^ Transient carbon pool (gC/m2)
  , clmGPP         :: !Double           -- ^ Gross primary production (gC/m2/s)
  , clmNPP         :: !Double           -- ^ Net primary production (gC/m2/s)
  , clmHR          :: !Double           -- ^ Heterotrophic respiration (gC/m2/s)
  , clmNEE         :: !Double           -- ^ Net ecosystem exchange (gC/m2/s)
  , clmSoilOrgC    :: !Double           -- ^ Soil organic carbon (gC/m2)
  , clmLitterC     :: !Double           -- ^ Litter carbon (gC/m2)
  , clmSMINN       :: !Double           -- ^ Soil mineral nitrogen (gN/m2)
  , clmLeafN       :: !Double           -- ^ Leaf nitrogen pool (gN/m2)
  , clmFPG         :: !Double           -- ^ Fraction of potential growth [0,1]
    -- Calibration parameters (injected by SiteCalibration, read by hydrology)
  , clmP_baseflow_scalar :: !Double
  , clmP_fff        :: !Double      -- ^ TOPMODEL decay factor
  , clmP_fmax       :: !Double      -- ^ Max fractional saturated area
  , clmP_e_ice      :: !Double      -- ^ Ice impedance factor
  , clmP_n_baseflow :: !Double      -- ^ Baseflow exponent
  , clmP_n_melt_coef :: !Double     -- ^ Snowmelt coefficient
  , clmP_interception_frac :: !Double
  , clmP_sno_z0mv   :: !Double      -- ^ Snow roughness length
  , clmP_route_k    :: !Double      -- ^ Routing residence time
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
  , clmCNActive     = False
  , clmLeafC        = 0.0
  , clmFrootC       = 0.0
  , clmLiveStemC    = 0.0
  , clmDeadStemC    = 0.0
  , clmCPool        = 0.0
  , clmGPP          = 0.0
  , clmNPP          = 0.0
  , clmHR           = 0.0
  , clmNEE          = 0.0
  , clmSoilOrgC     = 0.0
  , clmLitterC      = 0.0
  , clmSMINN        = 0.0
  , clmLeafN        = 0.0
  , clmFPG          = 1.0
  , clmP_baseflow_scalar = 0.01
  , clmP_fff        = 0.5
  , clmP_fmax       = 0.5
  , clmP_e_ice      = 6.0
  , clmP_n_baseflow = 1.0
  , clmP_n_melt_coef = 200.0
  , clmP_interception_frac = 0.5
  , clmP_sno_z0mv   = 0.002
  , clmP_route_k    = 20.0
  }

-- ============================================================================
-- Physics step type and pipeline
-- ============================================================================

-- | A single physics step: given config, context, and current state,
-- produce updated state.  Pure function signature.
type PhysicsStep = CLMDriverConfig -> TimestepContext -> CLMState -> CLMState

-- | The full physics pipeline: an ordered collection of named steps.
-- Each field holds a function with the 'PhysicsStep' signature.
-- Default implementations preserve state. Wired implementations are supplied
-- by the physics adapter layer.
data PhysicsPipeline = PhysicsPipeline
  { -- Phase 0: Pre-physics
    ppDayLength            :: !PhysicsStep
  , ppPhenology            :: !PhysicsStep
  , ppActiveLayer          :: !PhysicsStep
    -- Phase 1: Initialization
  , ppDrvInit              :: !PhysicsStep
    -- Phase 2: Canopy hydrology
  , ppCanopyInterception   :: !PhysicsStep
  , ppHandleNewSnow        :: !PhysicsStep
  , ppFracH2oSfc           :: !PhysicsStep
    -- Phase 3: Surface radiation
  , ppSurfaceRadiation     :: !PhysicsStep
    -- Phase 4: Pre-flux calculations
  , ppPreFluxCalcs         :: !PhysicsStep
  , ppSoilEvapResistance   :: !PhysicsStep
  , ppSurfaceHumidity      :: !PhysicsStep
    -- Phase 5: Fluxes
  , ppBaregroundFluxes     :: !PhysicsStep
  , ppCanopyFluxes         :: !PhysicsStep
  , ppLakeFluxes           :: !PhysicsStep
  , ppUrbanFluxes          :: !PhysicsStep
    -- Phase 6: Temperatures
  , ppSoilTemperature      :: !PhysicsStep
  , ppLakeTemperature      :: !PhysicsStep
  , ppSoilFluxes           :: !PhysicsStep
    -- Phase 7: Hydrology stage 2
  , ppSnowWater            :: !PhysicsStep
  , ppSoilHydrology        :: !PhysicsStep
  , ppWaterTable           :: !PhysicsStep
    -- Phase 8: Snow management
  , ppSnowCompaction       :: !PhysicsStep
  , ppSnowLayerCombine     :: !PhysicsStep
  , ppSnowLayerDivide      :: !PhysicsStep
  , ppSnowAging            :: !PhysicsStep
    -- Phase 8b: Biogeochemistry (CN mode only)
  , ppCNPreDrainage        :: !PhysicsStep
  , ppCNPostDrainage       :: !PhysicsStep
  , ppCNBalanceCheck       :: !PhysicsStep
    -- Phase 9: Hydrology drainage
  , ppHydrologyDrainage    :: !PhysicsStep
    -- Phase 10: Balance and diagnostics
  , ppWaterBalance         :: !PhysicsStep
  , ppEnergyBalance        :: !PhysicsStep
    -- Phase 11: Albedo for next step
  , ppSurfaceAlbedo        :: !PhysicsStep
  }

-- | Identity physics step for an unwired pipeline slot.
idStep :: PhysicsStep
idStep _cfg _ctx st = st

-- | Default pipeline: all steps preserve state until a caller supplies adapters.
defaultPhysicsPipeline :: PhysicsPipeline
defaultPhysicsPipeline = PhysicsPipeline
  { ppDayLength          = idStep
  , ppPhenology          = idStep
  , ppActiveLayer        = idStep
  , ppDrvInit            = idStep
  , ppCanopyInterception = idStep
  , ppHandleNewSnow      = idStep
  , ppFracH2oSfc         = idStep
  , ppSurfaceRadiation   = idStep
  , ppPreFluxCalcs       = idStep
  , ppSoilEvapResistance = idStep
  , ppSurfaceHumidity    = idStep
  , ppBaregroundFluxes   = idStep
  , ppCanopyFluxes       = idStep
  , ppLakeFluxes         = idStep
  , ppUrbanFluxes        = idStep
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
  , ppCNPreDrainage      = idStep
  , ppCNPostDrainage     = idStep
  , ppCNBalanceCheck     = idStep
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
clmDrvPatch2Col st =
  let ef = clmEnergyFlux st
      wf = clmWaterFlux st
      temp = clmTemp st
      cs = clmCanopyState st
      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (eflx_sh_tot_patch_vec ef)
          , VU.length (eflx_lh_tot_patch_vec ef)
          , VU.length (eflx_sh_grnd_patch_vec ef)
          , VU.length (cgrnds_patch_vec ef)
          , VU.length (cgrndl_patch_vec ef)
          , VU.length (cgrnd_patch_vec ef)
          , VU.length (dlrad_patch_vec ef)
          , VU.length (ulrad_patch_vec ef)
          , VU.length (eflx_lwrad_out_patch_vec ef)
          , VU.length (eflx_lwrad_net_patch_vec ef)
          , VU.length (qflx_evap_tot_patch_vec wf)
          , VU.length (qflx_evap_grnd_patch_vec wf)
          , VU.length (qflx_tran_veg_patch_vec wf)
          , VU.length (t_ref2m_patch_vec temp)
          , VU.length (t_veg_patch_vec temp)
          ]
      safeVec vec fallback p =
        if p >= 0 && p < VU.length vec then vec VU.! p else fallback
      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeVec (cstate_patch_wtgcell cs) 0.0 p
      weightSum = max 1.0e-12 (sum [ patchWeight p | p <- [0 .. patchCount - 1] ])
      weighted vec fallback =
        sum
          [ (patchWeight p / weightSum) * safeVec vec fallback p
          | p <- [0 .. patchCount - 1]
          ]
      shTot = weighted (eflx_sh_tot_patch_vec ef) (eflx_sh_tot_patch ef)
      lhTot = weighted (eflx_lh_tot_patch_vec ef) (eflx_lh_tot_patch ef)
      shGrnd = weighted (eflx_sh_grnd_patch_vec ef) (eflx_sh_grnd_patch ef)
      sabv = weighted (sabv_patch_vec ef) (sabv_patch ef)
      sabg = weighted (sabg_patch_vec ef) (sabg_patch ef)
      fsa = weighted (fsa_patch_vec ef) (fsa_patch ef)
      cgrnds = weighted (cgrnds_patch_vec ef) (cgrnds_patch ef)
      cgrndl = weighted (cgrndl_patch_vec ef) (cgrndl_patch ef)
      cgrnd = weighted (cgrnd_patch_vec ef) (cgrnd_patch ef)
      dlrad = weighted (dlrad_patch_vec ef) (dlrad_patch ef)
      ulrad = weighted (ulrad_patch_vec ef) (ulrad_patch ef)
      lwradOut = weighted (eflx_lwrad_out_patch_vec ef) (eflx_lwrad_out_patch ef)
      lwradNet = weighted (eflx_lwrad_net_patch_vec ef) (eflx_lwrad_net_patch ef)
      evapTot = weighted (qflx_evap_tot_patch_vec wf) (qflx_evap_tot_patch wf)
      evapGrnd = weighted (qflx_evap_grnd_patch_vec wf) (qflx_evap_grnd_col wf)
      tranVeg = weighted (qflx_tran_veg_patch_vec wf) (qflx_tran_veg_patch wf)
      tRef = weighted (t_ref2m_patch_vec temp) (t_ref2m_patch temp)
      tVeg = weighted (t_veg_patch_vec temp) (t_veg_patch temp)
      ef' = ef
        { eflx_sh_tot_patch = shTot
        , eflx_lh_tot_patch = lhTot
        , eflx_sh_grnd_patch = shGrnd
        , eflx_soil_grnd_col = shGrnd
        , sabv_patch = sabv
        , sabg_patch = sabg
        , fsa_patch = fsa
        , cgrnds_patch = cgrnds
        , cgrndl_patch = cgrndl
        , cgrnd_patch = cgrnd
        , dlrad_patch = dlrad
        , ulrad_patch = ulrad
        , eflx_lwrad_out_patch = lwradOut
        , eflx_lwrad_net_patch = lwradNet
        }
      wf' = wf
        { qflx_evap_tot_patch = evapTot
        , qflx_evap_grnd_col = evapGrnd
        , qflx_tran_veg_patch = tranVeg
        }
      temp' = temp
        { t_ref2m_patch = tRef
        , t_veg_patch = tVeg
        }
  in st { clmEnergyFlux = ef', clmWaterFlux = wf', clmTemp = temp' }

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
-- Each step is a slot in the 'PhysicsPipeline'. The default pipeline preserves
-- state, and production runs install the wired adapter set.
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
  let (drvState', snaps) = clmDrvBoundaries cfg pipeline ctx drvState st0
  in (drvState', bsFinal snaps)

-- | Intermediate CLMState snapshots captured at the Fortran instrumentation
-- boundaries, for per-module parity diffing. Purely a diagnostic projection of
-- the same physics call sequence 'clmDrv' runs — call order is unchanged.
data BoundarySnapshots = BoundarySnapshots
  { bsAfterCanopyFluxes      :: !CLMState  -- ^ after flux phase (st9b)
  , bsAfterSoilTemperature   :: !CLMState  -- ^ after soil temperature (st10)
  , bsAfterSoilFluxes        :: !CLMState  -- ^ after soil/surface fluxes (st12)
  , bsAfterHydrologyNoDrain  :: !CLMState  -- ^ after water table (st16)
  , bsFinal                  :: !CLMState  -- ^ end of step (st24)
  }

-- | Like 'clmDrv' but also returns the intermediate boundary snapshots.
clmDrvBoundaries
  :: CLMDriverConfig
  -> PhysicsPipeline
  -> TimestepContext
  -> CLMDriverState
  -> CLMState
  -> (CLMDriverState, BoundarySnapshots)
clmDrvBoundaries cfg pipeline ctx drvState st0 =
  let
    -- Apply each phase in sequence
    apply step = step cfg ctx

    -- Phase 0: Pre-physics (daylength, phenology, active layer)
    st_dl = apply (ppDayLength pipeline) st0
    st_ph = apply (ppPhenology pipeline) st_dl
    st_al = apply (ppActiveLayer pipeline) st_ph

    -- Phase 1: Driver init
    st1  = apply (ppDrvInit pipeline) st_al

    -- Phase 2: Canopy hydrology
    st2  = apply (ppCanopyInterception pipeline) st1
    st3  = apply (ppHandleNewSnow pipeline) st2
    st3b = apply (ppFracH2oSfc pipeline) st3

    -- Phase 3: Surface radiation
    st4  = apply (ppSurfaceRadiation pipeline) st3b

    -- Phase 4: Pre-flux calculations
    st5  = apply (ppPreFluxCalcs pipeline) st4
    st5b = apply (ppSoilEvapResistance pipeline) st5
    st6  = apply (ppSurfaceHumidity pipeline) st5b

    -- Phase 5: Determine fluxes
    st7  = apply (ppBaregroundFluxes pipeline) st6
    st8  = apply (ppCanopyFluxes pipeline) st7
    st9  = apply (ppLakeFluxes pipeline) st8
    st9b = apply (ppUrbanFluxes pipeline) st9

    -- Phase 6: Determine temperatures
    st10 = apply (ppSoilTemperature pipeline) st9b
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

    -- Phase 8b: CN biogeochemistry pre-drainage
    st20b = apply (ppCNPreDrainage pipeline) st20

    -- Phase 9: Drainage
    st21 = apply (ppHydrologyDrainage pipeline) st20b

    -- Phase 9b: CN biogeochemistry post-drainage
    st21b = apply (ppCNPostDrainage pipeline) st21

    -- Phase 10: Balance checks
    st22 = apply (ppWaterBalance pipeline) st21b
    st23 = apply (ppEnergyBalance pipeline) st22
    st23b = apply (ppCNBalanceCheck pipeline) st23

    -- Phase 11: Albedo for next step
    st24 = if tcDoAlb ctx
           then apply (ppSurfaceAlbedo pipeline) st23b
           else st23b

    -- Advance driver state
    drvState' = advanceDriverState drvState (tcDtime ctx)
  in
    (drvState', BoundarySnapshots
      { bsAfterCanopyFluxes     = st9b
      , bsAfterSoilTemperature  = st10
      , bsAfterSoilFluxes       = st12
      , bsAfterHydrologyNoDrain = st16
      , bsFinal                 = st24
      })

-- | Advance the driver state by one timestep.
advanceDriverState :: CLMDriverState -> Double -> CLMDriverState
advanceDriverState ds dtime =
  let newSec = dsSec ds + round dtime
      (extraDays, remainSec) = newSec `divMod` (86400 :: Int)
      (newYear, newMonth, newDay) =
        advanceYmd (dsYear ds) (dsMonth ds) (dsDay ds) extraDays
  in  ds { dsNstep = dsNstep ds + 1
         , dsSec   = remainSec
         , dsYear  = newYear
         , dsMonth = newMonth
         , dsDay   = newDay
         }

advanceYmd :: Int -> Int -> Int -> Int -> (Int, Int, Int)
advanceYmd y m d n
  | n <= 0 = (y, m, d)
  | d + n <= daysInMonth m = (y, m, d + n)
  | otherwise =
      let n' = n - (daysInMonth m - d + 1)
          (y', m') = if m == 12 then (y + 1, 1) else (y, m + 1)
      in advanceYmd y' m' 1 n'

daysInMonth :: Int -> Int
daysInMonth  1 = 31
daysInMonth  2 = 28
daysInMonth  3 = 31
daysInMonth  4 = 30
daysInMonth  5 = 31
daysInMonth  6 = 30
daysInMonth  7 = 31
daysInMonth  8 = 31
daysInMonth  9 = 30
daysInMonth 10 = 31
daysInMonth 11 = 30
daysInMonth 12 = 31
daysInMonth _  = 30

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
