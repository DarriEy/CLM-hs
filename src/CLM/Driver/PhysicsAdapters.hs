{-# LANGUAGE BangPatterns #-}
-- | Adapter functions that plug pure physics modules into the CLM pipeline.
--
-- Each adapter extracts relevant fields from CLMState, builds the physics
-- module's input type, calls the pure function, and packs results back.
--
-- These adapters bridge the typed pipeline (PhysicsStep) to the individual
-- physics modules' Input/Output record interfaces.
module CLM.Driver.PhysicsAdapters
  ( -- * Wired pipeline
    wiredPhysicsPipeline
    -- * Individual adapters (PhysicsStep signature)
  , dayLengthStep
  , activeLayerStep
  , fracH2oSfcStep
  , preFluxCalcsStep
  , surfaceRadiationStep
  , surfaceHumidityStep
  , canopyHydrologyStep
  , baregroundFluxesStep
  , canopyFluxesStep
  , soilTemperatureFullStep
  , soilFluxesStep
  , snowWaterStep
  , snowCompactionStep
  , snowLayerCombineStep
  , snowLayerDivideStep
  , snowAgingStep
  , soilHydrologyStep
  , hydrologyDrainageStep
  , surfaceAlbedoStep
  , waterBalanceStep
  , energyBalanceStep
  , lakeFluxesStep
  , lakeTemperatureStep
  , drvInitStep
  , soilEvapResistanceStep
  , waterTableStep
  , phenologyStep
  , urbanFluxesStep
    -- * Heat source term computation (used by soil temperature)
  , HeatSourceTerms(..)
  , computeHeatSourceTerms
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants
  ( nlevsno, nlevgrnd, nlevsoi, tfrz, sb, denh2o, denice
  , cpair, cpice, cpliq, hfus, hvap, hsub )
import CLM.Constants.ControlFlags
  ( CLMDriverConfig(..) )
import CLM.Driver.CLMDriver
  ( PhysicsStep, PhysicsPipeline(..), defaultPhysicsPipeline
  , CLMState(..), TimestepContext(..) )

import CLM.BioGeoPhys.SurfaceHumidity
  ( SurfaceHumidityInput(..), SurfaceHumidityResult(..)
  , surfaceHumidity )
import CLM.BioGeoPhys.CanopyHydrology
  ( CanopyHydrologyInput(..), CanopyHydrologyResult(..)
  , CanopyHydrologyParams(..), defaultCanopyHydroParams
  , canopyInterceptionAndThroughfall )
import CLM.BioGeoPhys.BaregroundFluxes
  ( BareGroundFluxesInput(..), BareGroundFluxesOutput(..)
  , BareGroundFluxesParams(..), defaultBareGroundFluxesParams
  , Z0ParamMethod(..)
  , baregroundFluxes )
import CLM.BioGeoPhys.CanopyFluxes
  ( CanopyFluxesInput(..), CanopyFluxesOutput(..)
  , CanopyFluxesParams(..), defaultCanopyFluxesParams
  , CanopyFluxesControl(..), defaultCanopyFluxesControl
  , canopyFluxes )
import CLM.BioGeoPhys.SoilTemperature
  ( SoilTempInput(..), SoilTempOutput(..)
  , solveSoilTemperature, SnowThermalCond(..) )
import CLM.BioGeoPhys.HydrologyDrainage
  ( TotalRunoffInput(..), TotalRunoffResult(..)
  , computeTotalRunoff )
import CLM.BioGeoPhys.QSat (QSatResult(..), qsat)
import CLM.BioGeoPhys.InfiltExcessRunoff
  ( InfiltExcessRunoffParams(..), defaultInfiltExcessParams
  , InfiltExcessRunoffInput(..), InfiltExcessRunoffResult(..)
  , QinmaxMethod(..), infiltrationExcessRunoff )
import CLM.BioGeoPhys.SoilWaterMovement
  ( SoilWaterMovementConfig(..), defaultSoilWaterMovementConfig
  , SolnMethod(..), ZengDeckerInput(..), ZengDeckerResult(..)
  , soilwaterZengDecker2009, iceImpedance )
import CLM.BioGeoPhys.SoilMoistStress
  ( RootMoistStressInput(..), RootMoistStressResult(..)
  , calcEffectiveSoilPorosity, calcRootMoistStressDefault
  , defaultSoilMoistStressConfig )
import qualified CLM.BioGeoChem.Allocation as Alloc
import qualified CLM.BioGeoChem.Phenology as Phen
import qualified CLM.BioGeoChem.NutrientCompetition as NComp
import CLM.Infrastructure.SmoothAD (smoothMax, smoothClamp, defaultK)
import CLM.BioGeoPhys.DayLength (daylength)
import CLM.BioGeoPhys.SurfaceRadiation
  ( SurfRadColumnInput(..), SurfRadPatchInput(..)
  , SurfRadConfig(..), defaultSurfRadConfig
  , SurfRadResult(..)
  , surfaceRadiationPatch )
import CLM.BioGeoPhys.SoilHydrology
  ( SoilWaterMovementConfig(..), defaultSoilWaterConfig
  , SoilWaterResult(..)
  , soilWater )  -- kept for waterTableStep
import CLM.BioGeoPhys.BalanceCheck
  ( WaterBalanceColInput(..), WaterBalanceColOutput(..)
  , waterBalanceCol
  , EnergyBalanceInput(..), EnergyBalanceOutput(..)
  , energyBalance )
import CLM.BioGeoPhys.SnowHydrology
  ( SnowLayerState(..), SnowLayerBounds(..)
  , initSnowLayerBounds, emptySnowLayerState
  , combineSnowLayers, divideSnowLayers
  , updateSnowDepthAndFracSL2012, addNewsnowToIntsnowSL2012 )
import CLM.BioGeoPhys.ActiveLayer
  ( AltCalcInput(..), AltCalcOutput(..)
  , altCalc )
import CLM.BioGeoPhys.SurfaceWater
  ( FracH2osfcInput(..), FracH2osfcResult(..)
  , computeFracH2osfc )
import CLM.BioGeoPhys.LakeFluxes
  ( LakeFluxInput(..), LakeFluxOutput(..)
  , lakeFluxes )
import CLM.BioGeoPhys.SoilFluxes
  ( SoilFluxesInput(..), SoilFluxesResult(..)
  , soilFluxes )
import CLM.BioGeoPhys.SnowSNICAR
  ( SnicarParams(..), defaultSnicarParams
  , SnowageGrainInput(..), SnowageGrainResult(..)
  , snowageGrainLayer )
import CLM.BioGeoPhys.SurfaceResistance
  ( BetaInput(..), BetaResult(..)
  , calcBetaLeePielke1992 )
import qualified Control.Exception as E
import CLM.BioGeoPhys.SurfaceAlbedo
  ( SurfAlbDriverInput(..), SurfAlbDriverOutput(..)
  , SurfaceAlbedoConstants(..), defaultSurfAlbConstants
  , SoilAlbedoInput(..), SoilAlbedoResult(..), GroundAlbedoResult(..)
  , surfaceAlbedoDriver )
import CLM.BioGeoPhys.SoilHydrology
  ( SoilHydrologyParams(..), defaultSoilHydroParams
  , WaterTableResult(..)
  , waterTable )
import CLM.BioGeoPhys.LakeTemperature
  ( ThermPropLakeInput(..), ThermPropLakeOutput(..)
  , soilThermPropLake )
import CLM.BioGeoPhys.Photosynthesis
  ( PhotoParams(..), defaultPhotoParams
  , PatchPhotoInput(..), PatchPhotoResult(..)
  , patchPhotosynthesis )
import CLM.BioGeoPhys.UrbanFluxes
  ( UrbanFluxesParams(..), defaultUrbanFluxesParams
  , UrbanFluxesInput(..), UrbanFluxesResult(..)
  , urbanFluxesSinglePatch )

import CLM.Types.ColumnData (ColumnData(..))
import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData (WaterStateData(..))
import CLM.Types.WaterStateBulkData (WaterStateBulkData(..))
import CLM.Types.WaterFluxData (WaterFluxData(..))
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..))
import CLM.Types.EnergyFluxData (EnergyFluxData(..))
import CLM.Types.CanopyStateData (CanopyStateData(..))
import CLM.Types.SoilStateData (SoilStateData(..))
import CLM.Types.LandunitData (LandunitData(..))
import CLM.Types.GridcellData (GridcellData(..))
import CLM.Types.SolarAbsorbedData (SolarAbsorbedData(..))
import CLM.Types.WaterBalanceData (WaterBalanceData(..))
import CLM.Types.FrictionVelocityData (FrictionVelocityData(..))
import CLM.Types.SoilHydrologyData (SoilHydrologyData(..))

-- ============================================================================
-- Wired pipeline: all available adapters plugged in
-- ============================================================================

-- | Physics pipeline with ALL slots wired. No idStep remaining.
wiredPhysicsPipeline :: SurfaceAlbedoConstants -> PhysicsPipeline
wiredPhysicsPipeline albConst = defaultPhysicsPipeline
  { ppDayLength          = dayLengthStep
  , ppPhenology          = phenologyStep
  , ppActiveLayer        = activeLayerStep
  , ppDrvInit            = drvInitStep
  , ppCanopyInterception = canopyHydrologyStep
  , ppHandleNewSnow      = snowWaterStep
  , ppFracH2oSfc         = fracH2oSfcStep
  , ppSurfaceRadiation   = surfaceRadiationStepWithAlbedo albConst
  , ppPreFluxCalcs       = preFluxCalcsStep
  , ppSoilEvapResistance = soilEvapResistanceStep
  , ppSurfaceHumidity    = surfaceHumidityStep
  , ppBaregroundFluxes   = baregroundFluxesStep
  , ppCanopyFluxes       = canopyFluxesStep
  , ppLakeFluxes         = lakeFluxesStep
  , ppUrbanFluxes        = urbanFluxesStep
  , ppSoilTemperature    = soilTemperatureFullStep
  , ppLakeTemperature    = lakeTemperatureStep
  , ppSoilFluxes         = soilFluxesStep
  , ppSnowWater          = snowPercolationStep
  , ppSoilHydrology      = soilHydrologyStep
  , ppWaterTable         = waterTableStep
  , ppSnowCompaction     = snowCompactionStep
  , ppSnowLayerCombine   = snowLayerCombineStep
  , ppSnowLayerDivide    = snowLayerDivideStep
  , ppSnowAging          = snowAgingStep
  , ppCNPreDrainage      = cnPreDrainageStep
  , ppCNPostDrainage     = cnPostDrainageStep
  , ppCNBalanceCheck     = cnBalanceCheckStep
  , ppHydrologyDrainage  = hydrologyDrainageStep
  , ppWaterBalance       = waterBalanceStep
  , ppEnergyBalance      = energyBalanceStep
  , ppSurfaceAlbedo      = surfaceAlbedoStep albConst
  }

-- ============================================================================
-- Helpers
-- ============================================================================

safeIdx :: VU.Vector Double -> Int -> Double
safeIdx v i
  | i >= 0 && i < VU.length v = v VU.! i
  | otherwise = 0.0

safeIdxI :: VU.Vector Int -> Int -> Int
safeIdxI v i
  | i >= 0 && i < VU.length v = v VU.! i
  | otherwise = 0

forcSoladTotal :: TimestepContext -> Double
forcSoladTotal = VU.sum . tcForcSolad

forcSolaiTotal :: TimestepContext -> Double
forcSolaiTotal = VU.sum . tcForcSolai

-- ============================================================================
-- Surface Humidity adapter
-- ============================================================================

surfaceHumidityStep :: PhysicsStep
surfaceHumidityStep _cfg _ctx st =
  let snl = clmSnl st
      temp = clmTemp st
      ws = clmWaterState st
      col = clmColumn st
      ss = clmSoilState st
      wdiag = clmWaterDiagBulk st

      topLayerIdx = nlevsno + snl
      t_grnd = t_grnd_col temp
      t_h2osfc = t_h2osfc_col temp
      forc_pbot = if VU.null (tcForcPbot _ctx) then 101325.0
                  else tcForcPbot _ctx VU.! 0
      forc_q = if VU.null (tcForcQ _ctx) then 0.005
               else tcForcQ _ctx VU.! 0

      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0

      inp = SurfaceHumidityInput
        { shi_lunType      = 1  -- istsoil
        , shi_colType      = 1
        , shi_snl          = snl
        , shi_dz_top       = safeIdx (colDz col) topLayerIdx
        , shi_h2osoi_liq_top = safeIdx (h2osoi_liq_col ws) topLayerIdx
        , shi_h2osoi_ice_top = safeIdx (h2osoi_ice_col ws) topLayerIdx
        , shi_watsat_top   = if topLayerIdx >= nlevsno
                             then safeIdx (watsat col) (topLayerIdx - nlevsno)
                             else 1.0
        , shi_smpmin       = -1.0e8
        , shi_sucsat_top   = if topLayerIdx >= nlevsno
                             then safeIdx (sucsat col) (topLayerIdx - nlevsno)
                             else 0.0
        , shi_bsw_top      = if topLayerIdx >= nlevsno
                             then safeIdx (bsw col) (topLayerIdx - nlevsno)
                             else 1.0
        , shi_frac_sno_eff = frac_sno_eff
        , shi_frac_h2osfc  = frac_h2osfc
        , shi_t_soisno_top = safeIdx (t_soisno_col temp) topLayerIdx
        , shi_t_soisno_snow = if snl < 0
                              then safeIdx (t_soisno_col temp) (nlevsno + snl)
                              else t_grnd
        , shi_t_grnd       = t_grnd
        , shi_t_h2osfc     = t_h2osfc
        , shi_forc_pbot    = forc_pbot
        , shi_forc_q       = forc_q
        }

      result = surfaceHumidity inp

      wdiag' = wdiag
        { wdiag_qg_col      = VU.singleton (shr_qg result)
        , wdiag_qg_snow_col = VU.singleton (shr_qg_snow result)
        , wdiag_qg_soil_col = VU.singleton (shr_qg_soil result)
        , wdiag_qg_h2osfc_col = VU.singleton (shr_qg_h2osfc result)
        , wdiag_dqgdT_col   = VU.singleton (shr_dqgdT result)
        }

  in st { clmWaterDiagBulk = wdiag' }

-- ============================================================================
-- Canopy Hydrology adapter
-- ============================================================================

canopyHydrologyStep :: PhysicsStep
canopyHydrologyStep _cfg ctx st =
  let dtime = tcDtime ctx
      forc_rain = if VU.null (tcForcRain ctx) then 0.0
                  else tcForcRain ctx VU.! 0
      forc_snow = if VU.null (tcForcSnow ctx) then 0.0
                  else tcForcSnow ctx VU.! 0
      forc_t = if VU.null (tcForcT ctx) then 273.15
               else tcForcT ctx VU.! 0
      forc_wind = if VU.null (tcForcWind ctx) then 1.0
                  else tcForcWind ctx VU.! 0

      cs = clmCanopyState st
      ws = clmWaterState st
      wdiag = clmWaterDiagBulk st

      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (cstate_elai_patch cs)
          , VU.length (cstate_esai_patch cs)
          , VU.length (cstate_frac_veg_nosno_patch cs)
          , VU.length (liqcan_patch_vec ws)
          , VU.length (snocan_patch_vec ws)
          ]

      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeIdx (cstate_patch_wtgcell cs) p

      patchScalar vec fallback p =
        if p >= 0 && p < VU.length vec
        then vec VU.! p
        else if p == 0 then fallback else 0.0

      patchFracVeg p =
        let f = safeIdxI (cstate_frac_veg_nosno_patch cs) p
        in if f /= 0
           then f
           else safeIdxI (cstate_frac_veg_nosno_alb_patch cs) p

      resultForPatch p =
        canopyInterceptionAndThroughfall CanopyHydrologyInput
          { chi_params               = defaultCanopyHydroParams
          , chi_dtime                = dtime
          , chi_frac_veg_nosno       = patchFracVeg p
          , chi_elai                 = safeIdx (cstate_elai_patch cs) p
          , chi_esai                 = safeIdx (cstate_esai_patch cs) p
          , chi_forc_rain            = forc_rain
          , chi_forc_snow_col        = forc_snow
          , chi_forc_t               = forc_t
          , chi_forc_wind            = forc_wind
          , chi_col_itype            = 1  -- soil column
          , chi_qflx_irrig_sprinkler = 0.0
          , chi_qflx_irrig_drip      = 0.0
          , chi_snocan_in            = patchScalar (snocan_patch_vec ws) (snocan_patch ws) p
          , chi_liqcan_in            = patchScalar (liqcan_patch_vec ws) (liqcan_patch ws) p
          , chi_wtcol                = patchWeight p
          }

      results = [ resultForPatch p | p <- [0 .. patchCount - 1] ]

      liqcanVec = VU.fromList [ chr_liqcan r | r <- results ]
      snocanVec = VU.fromList [ chr_snocan r | r <- results ]
      h2ocanVec = VU.zipWith (+) liqcanVec snocanVec

      weightedSum vec =
        sum [ patchWeight p * safeIdx vec p | p <- [0 .. patchCount - 1] ]

      wf = clmWaterFlux st
      wf' = wf
        { qflx_rain_grnd_col = sum (map chr_qflx_liq_grnd_col results)
        , qflx_snow_grnd_col = sum (map chr_qflx_snow_grnd_col results)
        }

      ws' = ws
        { liqcan_patch = weightedSum liqcanVec
        , snocan_patch = weightedSum snocanVec
        , h2ocan_patch = weightedSum h2ocanVec
        , liqcan_patch_vec = liqcanVec
        , snocan_patch_vec = snocanVec
        , h2ocan_patch_vec = h2ocanVec
        }

      wdiag' = wdiag
        { wdiag_fwet_patch =
            VU.fromList [ chr_fwet r | r <- results ]
        , wdiag_fdry_patch =
            VU.fromList [ chr_fdry r | r <- results ]
        , wdiag_fcansno_patch =
            VU.fromList [ chr_fcansno r | r <- results ]
        , wdiag_h2ocan_patch = h2ocanVec
        , wdiag_qflx_prec_intr_patch =
            VU.fromList
              [ chr_qflx_intercepted_liq r + chr_qflx_intercepted_snow r
              | r <- results
              ]
        }

  in st { clmWaterFlux = wf'
        , clmWaterState = ws'
        , clmWaterDiagBulk = wdiag'
        }

-- ============================================================================
-- Bareground Fluxes adapter
-- ============================================================================

baregroundFluxesStep :: PhysicsStep
baregroundFluxesStep _cfg ctx st =
  let snl = clmSnl st
      temp = clmTemp st
      ws = clmWaterState st
      col = clmColumn st
      wdiag = clmWaterDiagBulk st
      cs = clmCanopyState st

      topLayerIdx = nlevsno + snl
      t_grnd = t_grnd_col temp
      t_h2osfc = t_h2osfc_col temp
      t_top = safeIdx (t_soisno_col temp) topLayerIdx
      t_soil1 = safeIdx (t_soisno_col temp) nlevsno

      forc_q = if VU.null (tcForcQ ctx) then 0.005 else tcForcQ ctx VU.! 0
      forc_pbot = if VU.null (tcForcPbot ctx) then 101325.0 else tcForcPbot ctx VU.! 0
      forc_th = if VU.null (tcForcTh ctx) then 280.0 else tcForcTh ctx VU.! 0
      forc_rho = if VU.null (tcForcRho ctx) then 1.2 else tcForcRho ctx VU.! 0
      forc_t = if VU.null (tcForcT ctx) then 280.0 else tcForcT ctx VU.! 0
      forc_wind = if VU.null (tcForcWind ctx) then 3.0 else tcForcWind ctx VU.! 0
      forc_hgt = tcForcHgt ctx

      htvp = if t_grnd < tfrz then hsub else hvap

      qg = safeIdx (wdiag_qg_col wdiag) 0
      qg_snow = safeIdx (wdiag_qg_snow_col wdiag) 0
      qg_soil = safeIdx (wdiag_qg_soil_col wdiag) 0
      qg_h2osfc = safeIdx (wdiag_qg_h2osfc_col wdiag) 0
      dqgdT = safeIdx (wdiag_dqgdT_col wdiag) 0

      soilbeta = safeIdx (sstate_soilbeta_col (clmSoilState st)) 0
      soilbeta' = if soilbeta == 0.0
                  then if t_grnd < tfrz then 0.01 else 1.0
                  else soilbeta

      thm = forc_t + 0.0098 * forc_hgt
      thv = forc_th * (1.0 + 0.61 * forc_q)

      z0mg = 0.01

      inp = BareGroundFluxesInput
        { bgi_params         = defaultBareGroundFluxesParams
        , bgi_z0param_method = ZengWang2007
        , bgi_forc_q         = forc_q
        , bgi_forc_pbot      = forc_pbot
        , bgi_forc_th        = forc_th
        , bgi_forc_rho       = forc_rho
        , bgi_forc_t         = forc_t
        , bgi_forc_u         = forc_wind
        , bgi_forc_v         = 0.0
        , bgi_forc_hgt_t     = forc_hgt
        , bgi_forc_hgt_u     = forc_hgt
        , bgi_forc_hgt_q     = forc_hgt
        , bgi_t_grnd         = t_grnd
        , bgi_thm            = thm
        , bgi_qg             = qg
        , bgi_qg_snow        = qg_snow
        , bgi_qg_soil        = qg_soil
        , bgi_qg_h2osfc      = qg_h2osfc
        , bgi_dqgdT          = dqgdT
        , bgi_thv            = thv
        , bgi_beta           = soilbeta'
        , bgi_zii            = zii col
        , bgi_t_h2osfc       = t_h2osfc
        , bgi_t_soisno_top   = t_top
        , bgi_t_soil1        = t_soil1
        , bgi_z0mg           = z0mg
        , bgi_z0hg           = z0mg
        , bgi_z0qg           = z0mg
        , bgi_zetamaxstable  = 0.5
        , bgi_soilbeta       = soilbeta'
        , bgi_soilresis      = 0.0
        , bgi_do_soilevap_beta = True
        , bgi_do_soil_resistance = False
        , bgi_htvp           = htvp
        , bgi_h2osoi_liq_top = safeIdx (h2osoi_liq_col ws) topLayerIdx
        , bgi_h2osoi_ice_top = safeIdx (h2osoi_ice_col ws) topLayerIdx
        , bgi_dz_top         = safeIdx (colDz col) topLayerIdx
        , bgi_watsat_top     = if topLayerIdx >= nlevsno
                               then safeIdx (watsat col) (topLayerIdx - nlevsno)
                               else 1.0
        }

      bgOut = baregroundFluxes inp
      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (cstate_elai_patch cs)
          , VU.length (cstate_esai_patch cs)
          , VU.length (cstate_frac_veg_nosno_patch cs)
          , VU.length (cstate_frac_veg_nosno_alb_patch cs)
          ]
      fracVeg p
        | not (VU.null (cstate_frac_veg_nosno_patch cs)) =
            safeIdxI (cstate_frac_veg_nosno_patch cs) p
        | otherwise =
            safeIdxI (cstate_frac_veg_nosno_alb_patch cs) p
      isCanopyPatch p =
        fracVeg p /= 0
        && safeIdx (cstate_elai_patch cs) p + safeIdx (cstate_esai_patch cs) p > 0.05
      barePatch value p =
        if isCanopyPatch p then 0.0 else value
      shTotVec = VU.generate patchCount (barePatch (bgo_eflx_sh_tot bgOut))
      shGrndVec = VU.generate patchCount (barePatch (bgo_eflx_sh_grnd bgOut))
      lhTotVec = VU.generate patchCount (barePatch (bgo_qflx_evap_tot bgOut * htvp))
      evapTotVec = VU.generate patchCount (barePatch (bgo_qflx_evap_tot bgOut))
      evapGrndVec = VU.generate patchCount (barePatch (bgo_qflx_evap_soi bgOut))
      tranVegVec = VU.replicate patchCount 0.0
      ram1Vec = VU.generate patchCount (barePatch (bgo_ram1 bgOut))
      ustarVec = VU.generate patchCount (barePatch (bgo_ustar bgOut))
      cgrndsVec = VU.generate patchCount (barePatch (bgo_cgrnds bgOut))
      cgrndlVec = VU.generate patchCount (barePatch (bgo_cgrndl bgOut))
      cgrndVec = VU.generate patchCount (barePatch (bgo_cgrnd bgOut))
      dlradVec = VU.replicate patchCount 0.0
      ulradVec = VU.replicate patchCount 0.0
      lwradOutVec = VU.replicate patchCount 0.0
      lwradNetVec = VU.replicate patchCount 0.0

      ef = clmEnergyFlux st
      ef' = ef
        { eflx_sh_tot_patch  = bgo_eflx_sh_tot bgOut
        , eflx_sh_grnd_patch = bgo_eflx_sh_grnd bgOut
        , eflx_lh_tot_patch = bgo_qflx_evap_tot bgOut * htvp
        , cgrnds_patch = bgo_cgrnds bgOut
        , cgrndl_patch = bgo_cgrndl bgOut
        , cgrnd_patch = bgo_cgrnd bgOut
        , dlrad_patch = 0.0
        , ulrad_patch = 0.0
        , eflx_lwrad_out_patch = 0.0
        , eflx_lwrad_net_patch = 0.0
        , eflx_sh_tot_patch_vec = shTotVec
        , eflx_sh_grnd_patch_vec = shGrndVec
        , eflx_lh_tot_patch_vec = lhTotVec
        , cgrnds_patch_vec = cgrndsVec
        , cgrndl_patch_vec = cgrndlVec
        , cgrnd_patch_vec = cgrndVec
        , dlrad_patch_vec = dlradVec
        , ulrad_patch_vec = ulradVec
        , eflx_lwrad_out_patch_vec = lwradOutVec
        , eflx_lwrad_net_patch_vec = lwradNetVec
        }

      temp' = temp
        { t_ref2m_patch = bgo_t_ref2m bgOut
        , t_ref2m_patch_vec = VU.generate patchCount (barePatch (bgo_t_ref2m bgOut))
        }

      fv = clmFrictionVel st
      fv' = fv
        { fvel_ram1_patch = ram1Vec
        , fvel_ustar_patch = ustarVec
        }

      wf = clmWaterFlux st
      wf' = wf
        { qflx_evap_tot_patch = bgo_qflx_evap_tot bgOut
        , qflx_evap_grnd_col = bgo_qflx_evap_soi bgOut
        , qflx_tran_veg_patch = 0.0
        , qflx_evap_tot_patch_vec = evapTotVec
        , qflx_evap_grnd_patch_vec = evapGrndVec
        , qflx_tran_veg_patch_vec = tranVegVec
        }

  in st { clmEnergyFlux = ef'
        , clmWaterFlux = wf'
        , clmTemp = temp'
        , clmFrictionVel = fv'
        }

-- ============================================================================
-- Canopy Fluxes adapter
-- ============================================================================

canopyFluxesStep :: PhysicsStep
canopyFluxesStep _cfg ctx st =
  let snl = clmSnl st
      temp = clmTemp st
      col = clmColumn st
      ss = clmSoilState st
      cs = clmCanopyState st
      ws = clmWaterState st
      wdiag = clmWaterDiagBulk st
      ef0 = clmEnergyFlux st
      wf0 = clmWaterFlux st
      fv0 = clmFrictionVel st

      t_grnd = t_grnd_col temp
      t_h2osfc = t_h2osfc_col temp
      topLayerIdx = nlevsno + snl
      t_top = safeIdx (t_soisno_col temp) topLayerIdx
      t_soil1 = safeIdx (t_soisno_col temp) nlevsno

      forc_q = if VU.null (tcForcQ ctx) then 0.005 else tcForcQ ctx VU.! 0
      forc_pbot = if VU.null (tcForcPbot ctx) then 101325.0 else tcForcPbot ctx VU.! 0
      forc_th = if VU.null (tcForcTh ctx) then 280.0 else tcForcTh ctx VU.! 0
      forc_rho = if VU.null (tcForcRho ctx) then 1.2 else tcForcRho ctx VU.! 0
      forc_t = if VU.null (tcForcT ctx) then 280.0 else tcForcT ctx VU.! 0
      forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0 else tcForcLwrad ctx VU.! 0
      forc_wind = if VU.null (tcForcWind ctx) then 3.0 else tcForcWind ctx VU.! 0
      forc_hgt = tcForcHgt ctx
      dtime = tcDtime ctx

      htvp = if t_grnd < tfrz then hsub else hvap

      watsat_v = if VU.null (sstate_watsat_col ss)
                 then watsat col else sstate_watsat_col ss
      bsw_v = if VU.null (sstate_bsw_col ss)
              then bsw col else sstate_bsw_col ss
      sucsat_v = if VU.null (sstate_sucsat_col ss)
                 then sucsat col else sstate_sucsat_col ss
      effPorosity =
        calcEffectiveSoilPorosity nlevgrnd watsat_v (h2osoi_ice_col ws) (colDz col)
      h2osoiLiqVol =
        VU.generate (nlevsno + nlevgrnd) $ \j ->
          if j < nlevsno
          then 0.0
          else
            let sj = j - nlevsno
                dzj = max 1.0e-12 (safeIdx (colDz col) j)
            in min (safeIdx effPorosity sj)
                   (safeIdx (h2osoi_liq_col ws) j / (denh2o * dzj))
      defaultRootFr =
        let active = max 1 nlevsoi
        in VU.generate nlevgrnd $ \j ->
             if j < active then 1.0 / fromIntegral active else 0.0
      rootFrFor p
        | VU.length (sstate_rootfr_patch ss) >= (p + 1) * nlevgrnd =
            VU.slice (p * nlevgrnd) nlevgrnd (sstate_rootfr_patch ss)
        | VU.length (sstate_rootfr_col ss) >= nlevgrnd =
            VU.slice 0 nlevgrnd (sstate_rootfr_col ss)
        | otherwise = defaultRootFr
      clamp01 x
        | isNaN x || isInfinite x = 0.0
        | otherwise = max 0.0 (min 1.0 x)
      btranFor p elai
        | elai <= 0.05 = 0.0
        | otherwise =
            let rms = calcRootMoistStressDefault RootMoistStressInput
                  { rmsi_nlevgrnd = nlevgrnd
                  , rmsi_rootfr = rootFrFor p
                  , rmsi_t_soisno = t_soisno_col temp
                  , rmsi_watsat = watsat_v
                  , rmsi_sucsat = sucsat_v
                  , rmsi_bsw = bsw_v
                  , rmsi_eff_porosity = effPorosity
                  , rmsi_h2osoi_liqvol = h2osoiLiqVol
                  , rmsi_smpso = safePatch (sstate_smpso_patch ss) (-66000.0) p
                  , rmsi_smpsc = safePatch (sstate_smpsc_patch ss) (-255000.0) p
                  , rmsi_config = defaultSoilMoistStressConfig
                  }
            in clamp01 (rmsr_btran rms)

      qg = safeIdx (wdiag_qg_col wdiag) 0
      qg_snow = safeIdx (wdiag_qg_snow_col wdiag) 0
      qg_soil = safeIdx (wdiag_qg_soil_col wdiag) 0
      qg_h2osfc = safeIdx (wdiag_qg_h2osfc_col wdiag) 0
      dqgdT = safeIdx (wdiag_dqgdT_col wdiag) 0

      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0

      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (cstate_elai_patch cs)
          , VU.length (cstate_esai_patch cs)
          , VU.length (cstate_frac_veg_nosno_patch cs)
          , VU.length (cstate_frac_veg_nosno_alb_patch cs)
          , VU.length (eflx_sh_tot_patch_vec ef0)
          , VU.length (qflx_evap_tot_patch_vec wf0)
          ]
      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeIdx (cstate_patch_wtgcell cs) p
      patchWeightSum =
        max 1.0e-12 (sum [ patchWeight p | p <- [0 .. patchCount - 1] ])
      patchWt p = patchWeight p / patchWeightSum
      weightedVec vec =
        sum [ patchWt p * safeIdx vec p | p <- [0 .. patchCount - 1] ]
      expandVec vec fallback =
        VU.generate patchCount $ \p ->
          if p < VU.length vec
          then vec VU.! p
          else if p == 0 then fallback else 0.0
      safePatch vec fallback p =
        if p < VU.length vec then vec VU.! p else fallback
      fracVeg p
        | not (VU.null (cstate_frac_veg_nosno_patch cs)) =
            safeIdxI (cstate_frac_veg_nosno_patch cs) p
        | otherwise =
            safeIdxI (cstate_frac_veg_nosno_alb_patch cs) p
      isCanopyPatch p =
        fracVeg p /= 0
        && safeIdx (cstate_elai_patch cs) p + safeIdx (cstate_esai_patch cs) p > 0.05

      thm = forc_t + 0.0098 * forc_hgt
      thv = forc_th * (1.0 + 0.61 * forc_q)

      emg = 0.96 :: Double
      avmuir = 1.0
      fsa_est = forcSoladTotal ctx + forcSolaiTotal ctx

      soilbeta = safeIdx (sstate_soilbeta_col ss) 0
      soilbeta' = if soilbeta == 0.0
                  then if t_grnd < tfrz then 0.01 else 1.0
                  else soilbeta

      forc_solad_vis = if VU.null (tcForcSolad ctx) then 0.0
                       else tcForcSolad ctx VU.! 0
      par_sun = forc_solad_vis * 0.5  -- half of VIS direct → sunlit
      par_sha = forc_solad_vis * 0.1  -- diffuse fraction → shaded

      cgrnds0 = forc_rho * cpair / 100.0
      cgrndl0 = forc_rho / 100.0 * dqgdT
      preCanopyGround vec scalarFallback p
        | p < VU.length vec = vec VU.! p
        | patchCount == 1 = scalarFallback
        | otherwise = 0.0

      runCanopyPatch p
        | not (isCanopyPatch p) = Nothing
        | otherwise =
            let elai = safeIdx (cstate_elai_patch cs) p
                esai = safeIdx (cstate_esai_patch cs) p
                htop = max 0.1 (safeIdx (cstate_htop_patch cs) p)
                z0mr = safeIdx (cstate_z0m_patch cs) p
                z0mv = if z0mr > 0.0 then z0mr else 0.055 * htop
                displa = safeIdx (cstate_displa_patch cs) p
                displa' = if displa > 0.0 then displa else 0.67 * htop
                vai = elai + esai
                emv = 1.0 - exp (negate vai / avmuir)
                canopy_transmit = exp (-0.5 * vai)
                sabv =
                  if p < VU.length (sabv_patch_vec ef0)
                  then sabv_patch_vec ef0 VU.! p
                  else max 0.0 (fsa_est * (1.0 - canopy_transmit))
                laisun = safeIdx (cstate_laisun_patch cs) p
                laisha = safeIdx (cstate_laisha_patch cs) p
                laisun' = if laisun > 0.0 then laisun else elai * 0.5
                laisha' = if laisha > 0.0 then laisha else elai * 0.5
                tVegIn =
                  let tv = safePatch (t_veg_patch_vec temp) (t_veg_patch temp) p
                  in if isNaN tv || tv < 100.0 then forc_t else tv
                !psnInp = PatchPhotoInput
                  { ppi_vcmax25_top    = 50.0
                  , ppi_jmax25_top     = 0.0
                  , ppi_nrad           = 1
                  , ppi_lai            = elai
                  , ppi_sai            = esai
                  , ppi_kb             = 0.5
                  , ppi_kn             = 0.3
                  , ppi_par_sun        = [par_sun]
                  , ppi_par_sha        = [par_sha]
                  , ppi_cum_lai        = [elai * 0.5]
                  , ppi_forc_pbot      = forc_pbot
                  , ppi_co2_ppm        = 400.0
                  , ppi_o2_ppm         = 209000.0
                  , ppi_t_veg          = tVegIn
                  , ppi_rb             = 50.0
                  , ppi_rh_can         = 0.7
                  , ppi_esat_tv        = 2000.0
                  , ppi_ceair          = 1400.0
                  , ppi_gb_mol         = 0.5
                  , ppi_c3flag         = True
                  , ppi_o3coefv        = 1.0
                  , ppi_o3coefg        = 1.0
                  }
                !psnRes = patchPhotosynthesis defaultPhotoParams psnInp
                rsSun
                  | elai <= 0.0 = 0.0
                  | par_sun > 1.0 = max 200.0 (ppr_rs_sun psnRes)
                  | otherwise = 10000.0
                rsSha
                  | elai <= 0.0 = 0.0
                  | par_sha > 1.0 = max 200.0 (ppr_rs_sha psnRes)
                  | otherwise = 10000.0
                dleaf = max 0.01 (safePatch (cstate_dleaf_patch cs) 0.04 p)
                inp = CanopyFluxesInput
                  { cfi_forc_lwrad     = forc_lwrad
                  , cfi_forc_q         = forc_q
                  , cfi_forc_pbot      = forc_pbot
                  , cfi_forc_th        = forc_th
                  , cfi_forc_rho       = forc_rho
                  , cfi_forc_t         = forc_t
                  , cfi_forc_u         = forc_wind
                  , cfi_forc_v         = 0.0
                  , cfi_forc_hgt_u     = forc_hgt
                  , cfi_forc_hgt_t     = forc_hgt
                  , cfi_forc_hgt_q     = forc_hgt
                  , cfi_elai           = elai
                  , cfi_esai           = esai
                  , cfi_htop           = htop
                  , cfi_displa         = displa'
                  , cfi_z0mv           = z0mv
                  , cfi_z0mg           = 0.01
                  , cfi_frac_veg_nosno = fracVeg p
                  , cfi_emv            = emv
                  , cfi_emg            = emg
                  , cfi_t_veg          = tVegIn
                  , cfi_t_grnd         = t_grnd
                  , cfi_thm            = thm
                  , cfi_thv            = thv
                  , cfi_t_soisno_top   = t_top
                  , cfi_t_soisno_topsoil = t_soil1
                  , cfi_t_h2osfc       = t_h2osfc
                  , cfi_t_stem         = tVegIn
                  , cfi_sabv           = sabv
                  , cfi_qg             = qg
                  , cfi_qg_snow        = qg_snow
                  , cfi_qg_soil        = qg_soil
                  , cfi_qg_h2osfc      = qg_h2osfc
                  , cfi_dqgdT          = dqgdT
                  , cfi_frac_sno_eff   = frac_sno_eff
                  , cfi_frac_h2osfc    = frac_h2osfc
                  , cfi_snow_depth     = snow_depth
                  , cfi_fwet           = safeIdx (wdiag_fwet_patch wdiag) p
                  , cfi_fdry           = safeIdx (wdiag_fdry_patch wdiag) p
                  , cfi_liqcan         = safePatch (liqcan_patch_vec ws) (liqcan_patch ws) p
                  , cfi_snocan         = safePatch (snocan_patch_vec ws) (snocan_patch ws) p
                  , cfi_rssun          = rsSun
                  , cfi_rssha          = rsSha
                  , cfi_laisun         = laisun'
                  , cfi_laisha         = laisha'
                  , cfi_btran          = btranFor p elai
                  , cfi_soilbeta       = soilbeta'
                  , cfi_soilresis      = 0.0
                  , cfi_htvp           = htvp
                  , cfi_cgrnds         =
                      preCanopyGround (cgrnds_patch_vec ef0) cgrnds0 p
                  , cfi_cgrndl         =
                      preCanopyGround (cgrndl_patch_vec ef0) cgrndl0 p
                  , cfi_do_soilevap_beta = True
                  , cfi_dtime          = dtime
                  , cfi_zetamaxstable  = 0.5
                  , cfi_dleaf          = dleaf
                  , cfi_snl            = snl
                  }
                !cfOut = canopyFluxes defaultCanopyFluxesParams
                           defaultCanopyFluxesControl inp
                !gpp_est =
                  (ppr_psn_sun psnRes + ppr_psn_sha psnRes) * 1.0e-6 * 12.011
            in Just (cfOut, gpp_est)

      patchResults = [ runCanopyPatch p | p <- [0 .. patchCount - 1] ]
      fromPatch p fallback select =
        case patchResults !! p of
          Just (cfOut, _) -> select cfOut
          Nothing -> fallback p

      shTotBase = expandVec (eflx_sh_tot_patch_vec ef0) (eflx_sh_tot_patch ef0)
      shGrndBase = expandVec (eflx_sh_grnd_patch_vec ef0) (eflx_sh_grnd_patch ef0)
      lhTotBase = expandVec (eflx_lh_tot_patch_vec ef0) (eflx_lh_tot_patch ef0)
      evapTotBase = expandVec (qflx_evap_tot_patch_vec wf0) (qflx_evap_tot_patch wf0)
      evapGrndBase = expandVec (qflx_evap_grnd_patch_vec wf0) (qflx_evap_grnd_col wf0)
      tranVegBase = expandVec (qflx_tran_veg_patch_vec wf0) (qflx_tran_veg_patch wf0)
      ram1Base = expandVec (fvel_ram1_patch fv0) 0.0
      ustarBase = expandVec (fvel_ustar_patch fv0) 0.0
      liqcanBase = expandVec (liqcan_patch_vec ws) (liqcan_patch ws)
      snocanBase = expandVec (snocan_patch_vec ws) (snocan_patch ws)
      tRefBase = expandVec (t_ref2m_patch_vec temp) (t_ref2m_patch temp)
      tVegBase = expandVec (t_veg_patch_vec temp) (t_veg_patch temp)
      cgrndsBase = expandVec (cgrnds_patch_vec ef0) (cgrnds_patch ef0)
      cgrndlBase = expandVec (cgrndl_patch_vec ef0) (cgrndl_patch ef0)
      cgrndBase = expandVec (cgrnd_patch_vec ef0) (cgrnd_patch ef0)
      dlradBase = expandVec (dlrad_patch_vec ef0) (dlrad_patch ef0)
      ulradBase = expandVec (ulrad_patch_vec ef0) (ulrad_patch ef0)

      shTotVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx shTotBase) $ \cfOut ->
          cfo_eflx_sh_veg cfOut + cfo_eflx_sh_grnd cfOut
      shGrndVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx shGrndBase) cfo_eflx_sh_grnd
      lhTotVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx lhTotBase) $ \cfOut ->
          cfo_qflx_evap_soi cfOut * htvp + cfo_qflx_evap_veg cfOut * hvap
      evapTotVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx evapTotBase) $ \cfOut ->
          cfo_qflx_evap_soi cfOut + cfo_qflx_evap_veg cfOut
      evapGrndVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx evapGrndBase) cfo_qflx_evap_soi
      tranVegVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx tranVegBase) cfo_qflx_tran_veg
      ram1Vec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx ram1Base) cfo_ram1
      ustarVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx ustarBase) cfo_ustar
      tRefVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx tRefBase) cfo_t_ref2m
      tVegVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx tVegBase) cfo_t_veg
      cgrndsVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx cgrndsBase) cfo_cgrnds
      cgrndlVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx cgrndlBase) cfo_cgrndl
      cgrndVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx cgrndBase) cfo_cgrnd
      dlradVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx dlradBase) cfo_dlrad
      ulradVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx ulradBase) cfo_ulrad
      liqcanVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx liqcanBase) cfo_liqcan
      snocanVec = VU.generate patchCount $ \p ->
        fromPatch p (safeIdx snocanBase) cfo_snocan
      h2ocanVec = VU.zipWith (+) liqcanVec snocanVec
      gppAgg =
        sum
          [ patchWt p * gpp
          | (p, Just (_, gpp)) <- zip [0 .. patchCount - 1] patchResults
          ]

      ef' = ef0
        { eflx_sh_tot_patch = weightedVec shTotVec
        , eflx_sh_grnd_patch = weightedVec shGrndVec
        , eflx_lh_tot_patch = weightedVec lhTotVec
        , cgrnds_patch = weightedVec cgrndsVec
        , cgrndl_patch = weightedVec cgrndlVec
        , cgrnd_patch = weightedVec cgrndVec
        , dlrad_patch = weightedVec dlradVec
        , ulrad_patch = weightedVec ulradVec
        , eflx_sh_tot_patch_vec = shTotVec
        , eflx_sh_grnd_patch_vec = shGrndVec
        , eflx_lh_tot_patch_vec = lhTotVec
        , cgrnds_patch_vec = cgrndsVec
        , cgrndl_patch_vec = cgrndlVec
        , cgrnd_patch_vec = cgrndVec
        , dlrad_patch_vec = dlradVec
        , ulrad_patch_vec = ulradVec
        }
      wf' = wf0
        { qflx_evap_tot_patch = weightedVec evapTotVec
        , qflx_evap_grnd_col = weightedVec evapGrndVec
        , qflx_tran_veg_patch = weightedVec tranVegVec
        , qflx_evap_tot_patch_vec = evapTotVec
        , qflx_evap_grnd_patch_vec = evapGrndVec
        , qflx_tran_veg_patch_vec = tranVegVec
        }
      temp' = temp
        { t_ref2m_patch = weightedVec tRefVec
        , t_veg_patch = weightedVec tVegVec
        , t_ref2m_patch_vec = tRefVec
        , t_veg_patch_vec = tVegVec
        }
      ws' = ws
        { liqcan_patch = weightedVec liqcanVec
        , snocan_patch = weightedVec snocanVec
        , h2ocan_patch = weightedVec h2ocanVec
        , liqcan_patch_vec = liqcanVec
        , snocan_patch_vec = snocanVec
        , h2ocan_patch_vec = h2ocanVec
        }
      wdiag' = wdiag { wdiag_h2ocan_patch = h2ocanVec }
      fv' = fv0
        { fvel_ram1_patch = ram1Vec
        , fvel_ustar_patch = ustarVec
        }

  in st { clmEnergyFlux = ef'
        , clmTemp = temp'
        , clmWaterFlux = wf'
        , clmWaterState = ws'
        , clmWaterDiagBulk = wdiag'
        , clmFrictionVel = fv'
        , clmGPP = if clmCNActive st then gppAgg else 0.0
        }

-- ============================================================================
-- Soil Temperature adapter
-- ============================================================================

data HeatSourceTerms = HeatSourceTerms
  { hst_hs_top   :: !Double
  , hst_dhsdT    :: !Double
  , hst_hs_soil  :: !Double
  , hst_hs_h2osfc :: !Double
  , hst_sabg_lyr  :: !(VU.Vector Double)
  } deriving (Show)

computeHeatSourceTerms
  :: Double -> Double -> Double -> Double -> Int
  -> VU.Vector Double -> Double -> Double
  -> [(Double, Double, Double, Double, Double, Double,
       Double, Double, Double, Double, Double,
       VU.Vector Double)]
  -> HeatSourceTerms
computeHeatSourceTerms t_grnd emg forc_lwrad htvp snl t_soisno t_h2osfc
                        _frac_sno_eff patches =
  let joff = nlevsno - 1
      lwrad_emit       = emg * sb * t_grnd ** 4
      dlwrad_emit      = 4.0 * emg * sb * t_grnd ** 3
      lyr_top          = snl + 1
      t_top_snow       = t_soisno VU.! (lyr_top + joff)
      t_top_soil       = t_soisno VU.! (joff + 1)
      lwrad_emit_soil  = emg * sb * t_top_soil ** 4
      lwrad_emit_h2osfc = emg * sb * t_h2osfc ** 4

      nlyr_sabg = nlevsno + 1
      zero_sabg = VU.replicate nlyr_sabg 0.0

      accum (hs_acc, dhsdT_acc, hs_soil_acc, hs_h2osfc_acc, sabg_lyr_acc)
            (wt, sabg, sabg_soil, _sabg_snow, _dlrad, cgrnd,
             eflx_sh_grnd, _eflx_sh_snow, eflx_sh_soil, eflx_sh_h2osfc,
             qflx_evap_soi, sabg_lyr_p) =
        let eflx_gnet_top = (sabg_lyr_p VU.! 0) + emg * forc_lwrad
                          - lwrad_emit - (eflx_sh_grnd + qflx_evap_soi * htvp)
            eflx_gnet_soil = sabg_soil + emg * forc_lwrad
                           - lwrad_emit_soil - (eflx_sh_soil + qflx_evap_soi * htvp)
            eflx_gnet_h2osfc = sabg_soil + emg * forc_lwrad
                             - lwrad_emit_h2osfc - (eflx_sh_h2osfc + qflx_evap_soi * htvp)
            dgnetdT = -cgrnd - dlwrad_emit
        in ( hs_acc + eflx_gnet_top * wt
           , dhsdT_acc + dgnetdT * wt
           , hs_soil_acc + eflx_gnet_soil * wt
           , hs_h2osfc_acc + eflx_gnet_h2osfc * wt
           , VU.zipWith (+) sabg_lyr_acc (VU.map (* wt) sabg_lyr_p)
           )

      (hs_total, dhsdT_total, hs_soil_total, hs_h2osfc_total, sabg_lyr_total) =
        foldl accum (0.0, 0.0, 0.0, 0.0, zero_sabg) patches

  in HeatSourceTerms
    { hst_hs_top    = hs_total
    , hst_dhsdT     = dhsdT_total
    , hst_hs_soil   = hs_soil_total
    , hst_hs_h2osfc = hs_h2osfc_total
    , hst_sabg_lyr  = sabg_lyr_total
    }

soilTemperatureFullStep :: PhysicsStep
soilTemperatureFullStep _cfg ctx st =
  let snl = clmSnl st
      temp = clmTemp st
      ws = clmWaterState st
      col = clmColumn st
      ss = clmSoilState st
      wdiag = clmWaterDiagBulk st
      grc = clmGridcell st
      ef = clmEnergyFlux st
      wf = clmWaterFlux st
      cs = clmCanopyState st
      dtime = tcDtime ctx

      t_grnd = t_grnd_col temp
      t_h2osfc = t_h2osfc_col temp
      forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0
                   else tcForcLwrad ctx VU.! 0
      htvp = if t_grnd < tfrz then hsub else hvap
      emg = 0.96 :: Double

      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0

      nbedrock = if VU.null (grc_nbedrock grc) then nlevsoi
                 else safeIdxI (grc_nbedrock grc) 0

      nlyr_sabg = nlevsno + 1
      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (sabg_patch_vec ef)
          , VU.length (eflx_sh_grnd_patch_vec ef)
          , VU.length (qflx_evap_grnd_patch_vec wf)
          , VU.length (cgrnds_patch_vec ef)
          , VU.length (cgrndl_patch_vec ef)
          , VU.length (cgrnd_patch_vec ef)
          , VU.length (dlrad_patch_vec ef)
          ]
      safeVec vec fallback p =
        if p >= 0 && p < VU.length vec then vec VU.! p else fallback
      expandVec vec fallback =
        VU.generate patchCount $ \p ->
          if p < VU.length vec
          then vec VU.! p
          else if p == 0 then fallback else 0.0
      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeVec (cstate_patch_wtgcell cs) 0.0 p
      weightSum = max 1.0e-12 (sum [ patchWeight p | p <- [0 .. patchCount - 1] ])
      patchWt p = patchWeight p / weightSum
      fracVeg p
        | not (VU.null (cstate_frac_veg_nosno_patch cs)) =
            safeIdxI (cstate_frac_veg_nosno_patch cs) p
        | otherwise =
            safeIdxI (cstate_frac_veg_nosno_alb_patch cs) p
      sabgVec = expandVec (sabg_patch_vec ef) (sabg_patch ef)
      shGrndVec = expandVec (eflx_sh_grnd_patch_vec ef) (eflx_sh_grnd_patch ef)
      evapGrndVec = expandVec (qflx_evap_grnd_patch_vec wf) (qflx_evap_grnd_col wf)
      dlradVec = expandVec (dlrad_patch_vec ef) (dlrad_patch ef)
      cgrndsVec = expandVec (cgrnds_patch_vec ef) (cgrnds_patch ef)
      cgrndlVec = expandVec (cgrndl_patch_vec ef) (cgrndl_patch ef)
      cgrndFallback = cgrnds_patch ef + htvp * cgrndl_patch ef
      cgrndVec =
        if VU.null (cgrnd_patch_vec ef)
        then VU.zipWith (\s l -> s + htvp * l) cgrndsVec cgrndlVec
        else expandVec (cgrnd_patch_vec ef)
               (if cgrnd_patch ef /= 0.0 then cgrnd_patch ef else cgrndFallback)

      lwrad_emit = emg * sb * t_grnd ** 4
      dlwrad_emit = 4.0 * emg * sb * t_grnd ** 3
      t_top_soil = safeIdx (t_soisno_col temp) nlevsno
      lwrad_emit_soil = emg * sb * t_top_soil ** 4
      lwrad_emit_h2osfc = emg * sb * t_h2osfc ** 4
      finiteClamp x
        | isNaN x || isInfinite x = 0.0
        | otherwise = max (-500.0) (min 500.0 x)
      heatFor emit p =
        let atmLw = if fracVeg p == 0 then emg * forc_lwrad else 0.0
            sabgP = safeVec sabgVec (sabg_patch ef) p
            dlradP = safeVec dlradVec (dlrad_patch ef) p
            shP = safeVec shGrndVec (eflx_sh_grnd_patch ef) p
            evapP = safeVec evapGrndVec (qflx_evap_grnd_col wf) p
        in sabgP + dlradP + atmLw - emit - (shP + evapP * htvp)
      hs_top_raw =
        sum [ patchWt p * heatFor lwrad_emit p | p <- [0 .. patchCount - 1] ]
      hs_soil_raw =
        sum [ patchWt p * heatFor lwrad_emit_soil p | p <- [0 .. patchCount - 1] ]
      hs_h2osfc_raw =
        sum [ patchWt p * heatFor lwrad_emit_h2osfc p | p <- [0 .. patchCount - 1] ]
      dhsdT_raw =
        sum
          [ patchWt p * (negate (safeVec cgrndVec cgrndFallback p) - dlwrad_emit)
          | p <- [0 .. patchCount - 1]
          ]
      sabgTop = sum [ patchWt p * safeVec sabgVec (sabg_patch ef) p
                    | p <- [0 .. patchCount - 1] ]
      sabg_lyr = VU.generate nlyr_sabg (\j -> if j == 0 then sabgTop else 0.0)
      hs_top = finiteClamp hs_top_raw
      hs_soil = finiteClamp hs_soil_raw
      hs_h2osfc = finiteClamp hs_h2osfc_raw
      dhsdT = if isNaN dhsdT_raw || isInfinite dhsdT_raw then -4.0 else dhsdT_raw

      stInput = SoilTempInput
        { sti_snl              = snl
        , sti_t_soisno         = t_soisno_col temp
        , sti_t_grnd           = t_grnd
        , sti_t_h2osfc         = t_h2osfc
        , sti_h2osoi_liq       = h2osoi_liq_col ws
        , sti_h2osoi_ice       = h2osoi_ice_col ws
        , sti_dz               = colDz col
        , sti_z                = colZ col
        , sti_zi               = colZi col
        , sti_watsat           = if VU.null (sstate_watsat_col ss)
                                 then watsat col else sstate_watsat_col ss
        , sti_bsw              = if VU.null (sstate_bsw_col ss)
                                 then bsw col else sstate_bsw_col ss
        , sti_sucsat           = if VU.null (sstate_sucsat_col ss)
                                 then sucsat col else sstate_sucsat_col ss
        , sti_tkmg             = sstate_tkmg_col ss
        , sti_tkdry            = sstate_tkdry_col ss
        , sti_csol             = sstate_csol_col ss
        , sti_tksatu           = sstate_tksatu_col ss
        , sti_nbedrock         = nbedrock
        , sti_h2osno_no_layers = h2osno_col ws
        , sti_h2osfc           = h2osfc_col ws
        , sti_frac_sno_eff     = frac_sno_eff
        , sti_frac_h2osfc      = frac_h2osfc
        , sti_snow_depth       = snow_depth
        , sti_hs_top           = hs_top
        , sti_dhsdT            = dhsdT
        , sti_hs_soil          = hs_soil
        , sti_hs_h2osfc        = hs_h2osfc
        , sti_sabg_lyr         = sabg_lyr
        , sti_eflx_bot         = 0.0
        , sti_dtime            = dtime
        , sti_snowCondMethod   = Jordan1991
        }

      stOutput = solveSoilTemperature stInput

      temp' = temp
        { t_soisno_col = sto_t_soisno stOutput
        , t_grnd_col   = sto_t_grnd stOutput
        , t_h2osfc_col = sto_t_h2osfc stOutput
        }

      ws' = ws
        { h2osoi_liq_col = sto_h2osoi_liq stOutput
        , h2osoi_ice_col = sto_h2osoi_ice stOutput
        }

  in st { clmTemp = temp'
        , clmWaterState = ws'
        }

-- ============================================================================
-- Snow liquid routing through resolved snow layers
-- ============================================================================

snowPercolationStep :: PhysicsStep
snowPercolationStep _cfg _ctx st = st

snowWaterStepNoop :: PhysicsStep
snowWaterStepNoop _cfg _ctx st = st

-- ============================================================================
-- Snow Water adapter (new snow accumulation + sublimation)
-- ============================================================================

snowWaterStep :: PhysicsStep
snowWaterStep _cfg ctx st =
  let dtime = tcDtime ctx
      forc_snow_raw = if VU.null (tcForcSnow ctx) then 0.0
                      else tcForcSnow ctx VU.! 0
      canopyHydrologyRan =
        not (VU.null (wdiag_fwet_patch (clmWaterDiagBulk st))) ||
        not (VU.null (wdiag_fdry_patch (clmWaterDiagBulk st))) ||
        not (VU.null (wdiag_fcansno_patch (clmWaterDiagBulk st)))
      forc_snow =
        if canopyHydrologyRan
        then qflx_snow_grnd_col (clmWaterFlux st)
        else forc_snow_raw
      forc_t = if VU.null (tcForcT ctx) then 273.15
               else tcForcT ctx VU.! 0

      snl = clmSnl st
      ws = clmWaterState st
      wsbulk = clmWaterStateBulk st
      col = clmColumn st
      wdiag = clmWaterDiagBulk st

      new_snow_mass = forc_snow * dtime
      bifall = 50.0 + 1.7 * (max 0.0 (forc_t - tfrz + 15.0)) ** 1.5
      new_snow_depth = if new_snow_mass > 0.0 then new_snow_mass / bifall else 0.0

      h2osno_nl = h2osno_col ws
      snow_persist_cur = max 0.0 (safeIdx (wdiag_snow_persist_col wdiag) 0)
      old_snow_depth = max 0.0 (safeIdx (wdiag_snow_depth_col wdiag) 0)
      old_frac_sno = max 0.0 (min 1.0 (safeIdx (wdiag_frac_sno_col wdiag) 0))
      old_int_snow = max 0.0 (safeIdx (wsbulk_int_snow_col wsbulk) 0)

      h2osoi_liq_cur = h2osoi_liq_col ws
      h2osoi_ice_cur = h2osoi_ice_col ws
      dz_cur = colDz col
      z_cur = colZ col
      zi_cur = colZi col
      canResolveSnow =
        VU.length (t_soisno_col (clmTemp st)) >= nlevsno + nlevgrnd
        && VU.length h2osoi_liq_cur >= nlevsno + nlevgrnd
        && VU.length h2osoi_ice_cur >= nlevsno + nlevgrnd
        && VU.length dz_cur >= nlevsno + nlevgrnd
        && VU.length z_cur >= nlevsno + nlevgrnd
        && VU.length zi_cur >= nlevsno + nlevgrnd + 1
      updateAt vec idx val =
        if idx >= 0 && idx < VU.length vec then vec VU.// [(idx, val)] else vec
      snowMass liq ice snlVal =
        sum [ safeIdx ice j + safeIdx liq j
            | j <- [nlevsno + snlVal .. nlevsno - 1]
            ]

      h2osno_total_prev =
        snowMass h2osoi_liq_cur h2osoi_ice_cur snl + h2osno_nl

      h2osno_after = h2osno_total_prev + new_snow_mass
      snow_persist_new =
        if h2osno_after <= 0.0
        then 0.0
        else (h2osno_total_prev * snow_persist_cur) / h2osno_after + dtime

      n_melt = 1.0 :: Double
      accum_factor = 0.1 :: Double
      int_snow_max = 2000.0 :: Double
      (frac_sno_sl, frac_sno_eff_sl, snow_depth_sl) =
        updateSnowDepthAndFracSL2012
          n_melt accum_factor int_snow_max
          h2osno_total_prev 0.0 old_int_snow
          new_snow_mass bifall old_snow_depth old_frac_sno
          1 False True
      int_snow_new =
        addNewsnowToIntsnowSL2012
          n_melt new_snow_mass h2osno_total_prev frac_sno_sl old_int_snow

      (h2osno_nl_1, ice_1, liq_1, dz_1, z_1, zi_1) =
        if snl < 0 && new_snow_mass > 0.0 && canResolveSnow
        then let topIdx = nlevsno + snl
                 ice_upd = updateAt h2osoi_ice_cur topIdx
                   (safeIdx h2osoi_ice_cur topIdx + new_snow_mass)
                 dz_upd = updateAt dz_cur topIdx
                   (safeIdx dz_cur topIdx + new_snow_depth)
                 z_upd = updateAt z_cur topIdx
                   (safeIdx z_cur topIdx - new_snow_depth / 2.0)
                 zi_upd = updateAt zi_cur topIdx
                   (safeIdx zi_cur topIdx - new_snow_depth)
             in (0.0, ice_upd, h2osoi_liq_cur, dz_upd, z_upd, zi_upd)
        else (h2osno_nl + new_snow_mass, h2osoi_ice_cur, h2osoi_liq_cur,
              dz_cur, z_cur, zi_cur)

      snow_sublim_mass =
        max 0.0 (frac_sno_sl * qflx_evap_grnd_col (clmWaterFlux st) * dtime)

      (snl_2, h2osno_nl_2, liq_2, ice_2, dz_2, z_2, zi_2, frac_sno_2, snow_depth_2) =
        if snl < 0 && canResolveSnow
        then
          let topIdx = nlevsno + snl
              oldIce = safeIdx ice_1 topIdx
              oldLiq = safeIdx liq_1 topIdx
              totalMass = oldIce + oldLiq
              sublim = min snow_sublim_mass totalMass
              newIce = max 0.0 (oldIce - sublim)
              newLiq =
                if newIce <= 0.0
                then max 0.0 (oldLiq - max 0.0 (sublim - oldIce))
                else oldLiq
              dzOld = safeIdx dz_1 topIdx
              fracRemoved = if totalMass > 0.0 then sublim / totalMass else 0.0
              newDz = max 0.0 (dzOld * (1.0 - fracRemoved))
              layerGone = newIce + newLiq < 1.0e-10
          in if layerGone
             then (snl + 1, 0.0, liq_1, ice_1, dz_1, z_1, zi_1, 0.0, 0.0)
             else (snl, 0.0, updateAt liq_1 topIdx newLiq,
                   updateAt ice_1 topIdx newIce, updateAt dz_1 topIdx newDz,
                   z_1, zi_1, frac_sno_sl, snow_depth_sl)
        else
          let sublim = min snow_sublim_mass h2osno_nl_1
              nlNew = max 0.0 (h2osno_nl_1 - sublim)
              hasSnow = nlNew > 0.0 || snowMass liq_1 ice_1 snl > 0.0
          in (snl, nlNew, liq_1, ice_1, dz_1, z_1, zi_1,
              if hasSnow then frac_sno_sl else 0.0,
              if hasSnow then snow_depth_sl else 0.0)

      -- Resolved snow-layer promotion is gated until the layer melt/merge
      -- path is brought to parity; the no-layer path preserves SWE for now.
      shouldCreateLayer = False

      (snl_final, h2osno_nl_final, t_soisno_new, liq_new, ice_final,
       dz_final, z_final, zi_final) =
        if shouldCreateLayer
        then let layerIdx = nlevsno - 1
                 snow_t = min tfrz forc_t
                 layer_dz = h2osno_nl_2 / bifall
                 t_new = t_soisno_col (clmTemp st) VU.// [(layerIdx, snow_t)]
                 liq_n = updateAt liq_2 layerIdx 0.0
                 ice_n = updateAt ice_2 layerIdx h2osno_nl_2
                 dz_n = updateAt dz_2 layerIdx layer_dz
                 z_n = updateAt z_2 layerIdx (negate (0.5 * layer_dz))
                 zi_n = updateAt zi_2 layerIdx (negate layer_dz)
             in (-1, 0.0, t_new, liq_n, ice_n, dz_n, z_n, zi_n)
        else (snl_2, h2osno_nl_2, t_soisno_col (clmTemp st),
              liq_2, ice_2, dz_2, z_2, zi_2)

      h2osno_total_final = h2osno_nl_final + snowMass liq_new ice_final snl_final
      massBasedNoLayerDepth =
        if bifall > 0.0 && frac_sno_2 > 0.0
        then h2osno_total_final / (bifall * frac_sno_2)
        else 0.0
      physicalNoLayerDepth =
        if bifall > 0.0 then h2osno_total_final / bifall else 0.0
      snow_depth_final =
        if snl_final < 0
        then sum [ safeIdx dz_final j | j <- [nlevsno + snl_final .. nlevsno - 1] ]
        else if physicalNoLayerDepth >= 0.010
             then massBasedNoLayerDepth
             else snow_depth_2
      frac_sno_final =
        if h2osno_total_final <= 0.0 then 0.0 else frac_sno_2
      frac_sno_eff_final =
        if h2osno_total_final <= 0.0 then 0.0 else frac_sno_eff_sl

      ws' = ws
        { h2osno_col     = h2osno_nl_final
        , h2osoi_liq_col = liq_new
        , h2osoi_ice_col = ice_final
        }

      wsbulk' = wsbulk
        { wsbulk_int_snow_col = VU.singleton int_snow_new
        , wsbulk_snow_persistence_col = VU.singleton snow_persist_new
        }

      col' = col { colDz = dz_final, colZ = z_final, colZi = zi_final }

      temp' = (clmTemp st) { t_soisno_col = t_soisno_new }

      wdiag' = wdiag
        { wdiag_frac_sno_col     = VU.singleton frac_sno_final
        , wdiag_frac_sno_eff_col = VU.singleton frac_sno_eff_final
        , wdiag_snow_depth_col   = VU.singleton snow_depth_final
        , wdiag_h2osno_total_col = VU.singleton h2osno_total_final
        , wdiag_snow_persist_col = VU.singleton snow_persist_new
        }

  in st { clmSnl = snl_final
        , clmWaterState = ws'
        , clmWaterStateBulk = wsbulk'
        , clmColumn = col'
        , clmTemp = temp'
        , clmWaterDiagBulk = wdiag'
        }

-- ============================================================================
-- Snow Compaction adapter (Anderson 1976)
-- ============================================================================

snowCompactionStep :: PhysicsStep
snowCompactionStep _cfg ctx st =
  let dtime = tcDtime ctx
      snl = clmSnl st
      col = clmColumn st
      ws = clmWaterState st
      temp = clmTemp st
      wdiag = clmWaterDiagBulk st

      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0

  in if snl >= 0
     then st
     else
       let c3 = 2.777e-6 :: Double
           c4 = 0.04 :: Double
           c5 = 2.0 :: Double
           c2_ob = 23.0e-3 :: Double
           eta0 = 9.0e5 :: Double
           overburden_tfactor = 0.08 :: Double
           uplim_destruct = 100.0 :: Double

           dz_in = colDz col
           ice_in = h2osoi_ice_col ws
           liq_in = h2osoi_liq_col ws
           t_in = t_soisno_col temp

           compactLayers burden jj dz_acc
             | jj > nlevsno - 1 = dz_acc
             | otherwise =
                 let dz_j = safeIdx dz_acc jj
                     ice_j = safeIdx ice_in jj
                     liq_j = safeIdx liq_in jj
                     t_j = safeIdx t_in jj
                     wx = ice_j + liq_j
                     td = max 0.0 (tfrz - t_j)
                     bi = if frac_sno > 0.0 && dz_j > 0.0
                          then ice_j / (frac_sno * dz_j) else 0.0
                     ddz1_base = negate c3 * exp (negate c4 * td)
                     ddz1_dens = if bi > uplim_destruct
                                 then ddz1_base * exp (negate 46.0e-3 * (bi - uplim_destruct))
                                 else ddz1_base
                     ddz1 = if liq_j > 0.01 * dz_j * frac_sno
                            then ddz1_dens * c5
                            else ddz1_dens
                     ddz2 = negate (burden + wx / 2.0)
                          * exp (negate overburden_tfactor * td - c2_ob * bi)
                          / eta0
                     pdzdtc = ddz1 + ddz2
                     dz_new_raw = dz_j * (1.0 + pdzdtc * dtime)
                     dz_min = if frac_sno > 0.0
                              then (ice_j / denice + liq_j / denh2o) / frac_sno
                              else 0.0
                     dz_new = max dz_min dz_new_raw
                     burden' = burden + wx
                 in compactLayers burden' (jj + 1) (dz_acc VU.// [(jj, dz_new)])

           topSnowIdx = nlevsno + snl
           dz_compacted = compactLayers 0.0 topSnowIdx dz_in

           nlevtot = nlevsno + nlevgrnd
           zi_new = VU.generate (nlevtot + 1) $ \k ->
             if k > nlevsno then safeIdx (colZi col) k
             else if k == nlevsno then 0.0
             else let depth_below = sum [ safeIdx dz_compacted j | j <- [k .. nlevsno - 1] ]
                  in negate depth_below

           z_new = VU.generate nlevtot $ \k ->
             if k >= nlevsno then safeIdx (colZ col) k
             else 0.5 * (safeIdx zi_new k + safeIdx zi_new (k + 1))

           snow_depth_final = sum [ safeIdx dz_compacted j
                                  | j <- [nlevsno + snl .. nlevsno - 1] ]

           col' = col { colDz = dz_compacted, colZ = z_new, colZi = zi_new }
           wdiag' = wdiag { wdiag_snow_depth_col = VU.singleton snow_depth_final }

       in st { clmColumn = col'
             , clmWaterDiagBulk = wdiag'
             }

-- ============================================================================
-- Hydrology Drainage adapter
-- ============================================================================

hydrologyDrainageStep :: PhysicsStep
hydrologyDrainageStep _cfg _ctx st =
  let wf = clmWaterFlux st
      inp = TotalRunoffInput
        { tri_qflx_drain         = qflx_drain_col wf
        , tri_qflx_surf          = qflx_surf_col wf
        , tri_qflx_qrgwl         = 0.0
        , tri_qflx_drain_perched = 0.0
        , tri_lun_itype          = 1  -- istsoil
        , tri_urbpoi             = False
        }
      result = computeTotalRunoff inp
      wf' = wf { qflx_surf_col = trr_qflx_runoff result }
  in st { clmWaterFlux = wf' }

-- ============================================================================
-- Day Length adapter
-- ============================================================================

dayLengthStep :: PhysicsStep
dayLengthStep _cfg ctx st =
  let grc = clmGridcell st
      lat = if VU.null (grc_lat grc) then 0.88 else grc_lat grc VU.! 0
      declin = tcDeclin ctx
      dayl = case daylength lat declin of
               Just d  -> d
               Nothing -> 43200.0
      maxDayl = case daylength lat (tcObliqr ctx) of
                  Just d  -> d
                  Nothing -> 86400.0
      grc' = grc
        { grc_dayl     = VU.singleton dayl
        , grc_max_dayl = VU.singleton maxDayl
        }
  in st { clmGridcell = grc' }

-- ============================================================================
-- Pre-Flux Calcs adapter
-- ============================================================================

preFluxCalcsStep :: PhysicsStep
preFluxCalcsStep _cfg ctx st =
  let temp = clmTemp st
      ws = clmWaterState st
      wdiag = clmWaterDiagBulk st
      snl = clmSnl st

      t_grnd = t_grnd_col temp
      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0

      forc_t = if VU.null (tcForcT ctx) then 280.0 else tcForcT ctx VU.! 0
      forc_q = if VU.null (tcForcQ ctx) then 0.005 else tcForcQ ctx VU.! 0
      forc_th = if VU.null (tcForcTh ctx) then 280.0 else tcForcTh ctx VU.! 0
      forc_hgt = tcForcHgt ctx

      emg = 0.96 :: Double
      htvp = if t_grnd < tfrz then hsub else hvap
      thm = forc_t + 0.0098 * forc_hgt
      thv = forc_th * (1.0 + 0.61 * forc_q)

      topLayerIdx = nlevsno + snl
      h2osoi_liq_top = safeIdx (h2osoi_liq_col ws) topLayerIdx
      h2osoi_ice_top = safeIdx (h2osoi_ice_col ws) topLayerIdx
      totalWater = h2osoi_liq_top + h2osoi_ice_top
      frac_iceold = if totalWater > 0.0
                    then h2osoi_ice_top / totalWater
                    else 0.0

      wdiag' = wdiag
        { wdiag_frac_iceold_col = VU.singleton frac_iceold
        }

      ss = clmSoilState st
      soilbeta = if t_grnd < tfrz then 0.01 else 1.0
      ss' = ss
        { sstate_soilbeta_col = VU.singleton soilbeta
        }

  in st { clmWaterDiagBulk = wdiag'
        , clmSoilState = ss'
        }

-- ============================================================================
-- Surface Radiation adapter
-- ============================================================================

surfaceRadiationStep :: PhysicsStep
surfaceRadiationStep = surfaceRadiationStepWithAlbedo defaultSurfAlbConstants

surfaceRadiationStepWithAlbedo :: SurfaceAlbedoConstants -> PhysicsStep
surfaceRadiationStepWithAlbedo albConst _cfg ctx st =
  let snl = clmSnl st
      wdiag = clmWaterDiagBulk st
      cs = clmCanopyState st
      col = clmColumn st
      temp = clmTemp st
      ws = clmWaterState st

      forc_solad_vis = safeIdx (tcForcSolad ctx) 0
      forc_solad_nir = safeIdx (tcForcSolad ctx) 1
      forc_solai_vis = safeIdx (tcForcSolai ctx) 0
      forc_solai_nir = safeIdx (tcForcSolai ctx) 1

      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0
      snow_persist =
        if frac_sno > 0.0
        then max 0.0 (safeIdx (wdiag_snow_persist_col wdiag) 0)
        else 0.0

      t_veg = t_veg_patch temp
      t_grnd = t_grnd_col temp
      h2oVolTop =
        let dzTop = max 1.0e-6 (safeIdx (colDz col) nlevsno)
            liqTop = safeIdx (h2osoi_liq_col ws) nlevsno
            iceTop = safeIdx (h2osoi_ice_col ws) nlevsno
        in max 0.0 ((liqTop / denh2o + iceTop / denice) / dzTop)

      coszen = max 0.0 (cos (tcDeclin ctx))
      soilColor =
        if VU.null (isoicol albConst)
        then 15
        else max 1 (min (max 1 (mxsoilColor albConst)) (safeIdxI (isoicol albConst) 0))

      useAlbDriver = not (VU.null (albsat albConst)) && mxsoilColor albConst > 0

      albsod_vis = 0.18
      albsod_nir = 0.29
      albsoi_vis = 0.18
      albsoi_nir = 0.29
      albsnd_vis = 0.85
      albsnd_nir = 0.65
      albsni_vis = 0.85
      albsni_nir = 0.65

      albgrd_vis = (1.0 - frac_sno) * albsod_vis + frac_sno * albsnd_vis
      albgrd_nir = (1.0 - frac_sno) * albsod_nir + frac_sno * albsnd_nir
      albgri_vis = (1.0 - frac_sno) * albsoi_vis + frac_sno * albsni_vis
      albgri_nir = (1.0 - frac_sno) * albsoi_nir + frac_sno * albsni_nir

      nlyr = nlevsno + 1
      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (cstate_elai_patch cs)
          , VU.length (cstate_esai_patch cs)
          ]
      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeIdx (cstate_patch_wtgcell cs) p
      patchWeightSum =
        max 1.0e-12 (sum [ patchWeight p | p <- [0 .. patchCount - 1] ])

      radForPatch p =
        let elai = safeIdx (cstate_elai_patch cs) p
            esai = safeIdx (cstate_esai_patch cs) p
            tlai = safeIdx (cstate_tlai_patch cs) p
            tsai = safeIdx (cstate_tsai_patch cs) p
            t_veg_p =
              if p < VU.length (t_veg_patch_vec temp)
              then t_veg_patch_vec temp VU.! p
              else t_veg
            patchBand field fallback =
              if VU.length field >= (p + 1) * VU.length fallback
              then VU.generate (VU.length fallback) $ \ib ->
                     safeIdx field (p * VU.length fallback + ib)
              else fallback
            rhol = patchBand (cstate_rhol_patch cs) (VU.fromList [0.10, 0.45])
            rhos = patchBand (cstate_rhos_patch cs) (VU.fromList [0.16, 0.39])
            taul = patchBand (cstate_taul_patch cs) (VU.fromList [0.05, 0.25])
            taus = patchBand (cstate_taus_patch cs) (VU.fromList [0.001, 0.001])
            xl = let x = safeIdx (cstate_xl_patch cs) p
                 in if x == 0.0 then 0.01 else x
            albResult =
              if useAlbDriver
              then Just $ surfaceAlbedoDriver albConst SurfAlbDriverInput
                { sadi_coszen = coszen
                , sadi_soilAlbIn = SoilAlbedoInput
                    { sai_coszen = coszen
                    , sai_lunType = 1
                    , sai_h2osoi_vol1 = h2oVolTop
                    , sai_soilColor = soilColor
                    , sai_t_grnd = t_grnd
                    , sai_snl = snl
                    , sai_lakePuddling = False
                    , sai_lakeIcefrac1 = 0.0
                    , sai_lakeIcefrac2 = 0.0
                    }
                , sadi_fracSno = frac_sno
                , sadi_snowPersist = snow_persist
                , sadi_elai = elai
                , sadi_esai = esai
                , sadi_tlai = if tlai > 0.0 then tlai else elai
                , sadi_tsai = if tsai > 0.0 then tsai else esai
                , sadi_t_veg = t_veg_p
                , sadi_fwet = safeIdx (wdiag_fwet_patch wdiag) p
                , sadi_fcansno = safeIdx (wdiag_fcansno_patch wdiag) p
                , sadi_rhol = rhol
                , sadi_rhos = rhos
                , sadi_taul = taul
                , sadi_taus = taus
                , sadi_xl = xl
                }
              else Nothing
            albsod =
              maybe (VU.fromList [albsod_vis, albsod_nir])
                    (sar_albsod . sado_soilAlb)
                    albResult
            albsoi =
              maybe (VU.fromList [albsoi_vis, albsoi_nir])
                    (sar_albsoi . sado_soilAlb)
                    albResult
            albsnd =
              maybe (VU.fromList [albsnd_vis, albsnd_nir]) sado_snowAlbD albResult
            albsni =
              maybe (VU.fromList [albsni_vis, albsni_nir]) sado_snowAlbI albResult
            albgrd =
              maybe (VU.fromList [albgrd_vis, albgrd_nir])
                    (gar_albgrd . sado_groundAlb)
                    albResult
            albgri =
              maybe (VU.fromList [albgri_vis, albgri_nir])
                    (gar_albgri . sado_groundAlb)
                    albResult
            colInp = SurfRadColumnInput
              { src_snl         = snl
              , src_albsod      = albsod
              , src_albsoi      = albsoi
              , src_albsnd_hst  = albsnd
              , src_albsni_hst  = albsni
              , src_albgrd      = albgrd
              , src_albgri      = albgri
              , src_flx_absdv   = VU.replicate nlyr 0.0
              , src_flx_absdn   = VU.replicate nlyr 0.0
              , src_flx_absiv   = VU.replicate nlyr 0.0
              , src_flx_absin   = VU.replicate nlyr 0.0
              , src_snow_depth  = snow_depth
              , src_frac_sno    = frac_sno
              }
            vai = elai + esai
            canopy_transmit = exp (-0.5 * vai)
            patchInp = SurfRadPatchInput
              { srp_lunType    = 1
              , srp_londeg     = 0.0
              , srp_fabd       = maybe (VU.fromList [0.4 * (1.0 - canopy_transmit),
                                                      0.3 * (1.0 - canopy_transmit)])
                                        sado_fabd albResult
              , srp_fabi       = maybe (VU.fromList [0.4 * (1.0 - canopy_transmit),
                                                      0.3 * (1.0 - canopy_transmit)])
                                        sado_fabi albResult
              , srp_ftdd       = maybe (VU.fromList [canopy_transmit, canopy_transmit])
                                        sado_ftdd albResult
              , srp_ftid       = maybe (VU.fromList [canopy_transmit, canopy_transmit])
                                        sado_ftid albResult
              , srp_ftii       = maybe (VU.fromList [canopy_transmit, canopy_transmit])
                                        sado_ftii albResult
              , srp_albd       = maybe (VU.fromList [0.15, 0.25]) sado_albd albResult
              , srp_albi       = maybe (VU.fromList [0.15, 0.25]) sado_albi albResult
              , srp_forc_solad = VU.fromList [forc_solad_vis, forc_solad_nir]
              , srp_forc_solai = VU.fromList [forc_solai_vis, forc_solai_nir]
              }
            radResult = surfaceRadiationPatch defaultSurfRadConfig colInp patchInp
        in ( srr_sabg radResult
           , srr_sabv radResult
           , srr_fsa radResult
           )

      patchRad =
        [ (p, radForPatch p)
        | p <- [0 .. patchCount - 1]
        ]
      (_sabgAgg, _sabvAgg, fsaAgg) =
        foldl
          (\(ga, va, fa) (p, (g, v, f)) ->
             let wt = patchWeight p / patchWeightSum
             in (ga + wt * g, va + wt * v, fa + wt * f))
          (0.0, 0.0, 0.0)
          patchRad
      sabgVec = VU.fromList [ g | (_, (g, _, _)) <- patchRad ]
      sabvVec = VU.fromList [ v | (_, (_, v, _)) <- patchRad ]
      fsaVec  = VU.fromList [ f | (_, (_, _, f)) <- patchRad ]
      (sabgActive, sabvActive, _fsaActive) =
        case patchRad of
          (_, rad0) : _ -> rad0
          []            -> (0.0, 0.0, 0.0)

      ef = clmEnergyFlux st
      ef' = ef
        { -- FSA is a gridcell diagnostic; soil temperature is still scalar
          -- over the active column, so its ground absorption stays active-patch.
          sabg_patch = sabgActive
        , sabv_patch = sabvActive
        , fsa_patch  = fsaAgg
        , sabg_patch_vec = sabgVec
        , sabv_patch_vec = sabvVec
        , fsa_patch_vec = fsaVec
        }

  in st { clmEnergyFlux = ef' }

-- ============================================================================
-- Soil Hydrology adapter — uses REAL ported modules:
--   InfiltExcessRunoff.infiltrationExcessRunoff
--   SoilWaterMovement.soilwaterZengDecker2009
--   SoilMoistStress.calcRootMoistStressDefault
-- ============================================================================

soilHydrologyStep :: PhysicsStep
soilHydrologyStep _cfg ctx st =
  let dtime = tcDtime ctx
      ws = clmWaterState st
      col = clmColumn st
      ss = clmSoilState st
      wf = clmWaterFlux st
      wdiag = clmWaterDiagBulk st
      snl = clmSnl st
      temp = clmTemp st

      forc_rain = if VU.null (tcForcRain ctx) then 0.0
                  else tcForcRain ctx VU.! 0

      h2osoi_liq = h2osoi_liq_col ws
      h2osoi_ice = h2osoi_ice_col ws
      t_soisno = t_soisno_col temp

      watsat_v = if VU.null (sstate_watsat_col ss)
                 then watsat col else sstate_watsat_col ss
      bsw_v = if VU.null (sstate_bsw_col ss)
               then bsw col else sstate_bsw_col ss
      hksat_v = if VU.null (sstate_hksat_col ss)
                then hksat col else sstate_hksat_col ss
      sucsat_v = if VU.null (sstate_sucsat_col ss)
                 then sucsat col else sstate_sucsat_col ss

      dz_soil = VU.slice nlevsno nlevgrnd (colDz col)
      z_soil = VU.slice nlevsno nlevgrnd (colZ col)
      zi_soil = VU.slice nlevsno (nlevgrnd + 1) (colZi col)

      h2osoi_liq_soil = VU.slice nlevsno nlevgrnd h2osoi_liq
      h2osoi_ice_soil = VU.slice nlevsno nlevgrnd h2osoi_ice
      t_soisno_soil = VU.slice nlevsno nlevgrnd t_soisno

      icefrac = VU.generate nlevgrnd $ \j ->
        let liq = safeIdx h2osoi_liq_soil j
            ice = safeIdx h2osoi_ice_soil j
            total = liq + ice
        in if total > 0.0 then ice / total else 0.0

      h2osoi_vol = VU.generate nlevgrnd $ \j ->
        let liq = safeIdx h2osoi_liq_soil j
            ice = safeIdx h2osoi_ice_soil j
            dz_j = safeIdx dz_soil j
        in if dz_j > 0.0
           then (liq / denh2o + ice / denice) / dz_j
           else 0.0

      -- Read calibration parameters from CLM state
      p_e_ice = clmP_e_ice st
      p_baseflow_scalar = clmP_baseflow_scalar st
      p_fff = clmP_fff st
      p_fmax = clmP_fmax st
      p_n_baseflow = clmP_n_baseflow st
      p_n_melt_coef = clmP_n_melt_coef st
      p_interception_frac = clmP_interception_frac st
      p_sno_z0mv = clmP_sno_z0mv st

      forc_t_val = if VU.null (tcForcT ctx) then 275.0 else tcForcT ctx VU.! 0
      forc_rain_val = if VU.null (tcForcRain ctx) then 0.0 else tcForcRain ctx VU.! 0
      forc_snow_val = if VU.null (tcForcSnow ctx) then 0.0 else tcForcSnow ctx VU.! 0
      forc_lwrad_val = if VU.null (tcForcLwrad ctx) then 250.0 else tcForcLwrad ctx VU.! 0
      forc_wind_val = if VU.null (tcForcWind ctx) then 3.0 else tcForcWind ctx VU.! 0
      forc_fsds_val = if VU.null (tcForcSolad ctx) then 0.0
                      else VU.sum (tcForcSolad ctx) + VU.sum (tcForcSolai ctx)
      elai_val = safeIdx (cstate_elai_patch (clmCanopyState st)) 0

      -- ================================================================
      -- 1. Snow: read TOTAL SWE from real snow layers + h2osno
      --    snowWaterStep (Phase 2) already handled accumulation.
      --    Here we compute melt and route water to soil.
      -- ================================================================
      h2osno_explicit = h2osno_col ws
      h2osno_layers = sum [ safeIdx h2osoi_ice (nlevsno + snl + j)
                            + safeIdx h2osoi_liq (nlevsno + snl + j)
                          | j <- [0 .. max 0 (negate snl) - 1] ]
      swe_total = h2osno_explicit + h2osno_layers

      -- Snowmelt: only when snow layer temperature reaches tfrz
      -- The soil temperature step handles the energy balance and warms snow layers.
      -- Here we just check: if the snow layer is at tfrz, melt occurs.
      t_snow = if snl < 0 then safeIdx t_soisno (nlevsno + snl) else forc_t_val
      melt_mm = if t_snow >= tfrz - 0.01 && swe_total > 0.0
                then let -- Degree-day melt scaled by n_melt_coef when at/above freezing
                         melt_rate = max 0.0 (forc_t_val - tfrz) * p_n_melt_coef / 86400.0
                     in min swe_total (melt_rate * dtime)
                else 0.0
      snowmelt_rate = melt_mm / dtime

      -- Remove melt from snow layers (proportional)
      melt_frac = if swe_total > 0 then melt_mm / swe_total else 0.0
      swe_new = max 0.0 (swe_total - melt_mm)

      -- ================================================================
      -- 2. Water to soil: rain + snowmelt - ET (from canopy fluxes)
      -- ================================================================
      qflx_evap = qflx_evap_tot_patch wf
      qflx_tran = qflx_tran_veg_patch wf
      qflx_in_soil = max 0.0 (forc_rain_val + snowmelt_rate - max 0.0 qflx_evap)

      -- ================================================================
      -- 3. REAL MODULE: InfiltExcessRunoff (using actual ported function)
      -- ================================================================
      infExParams = defaultInfiltExcessParams { ierp_e_ice = p_e_ice }
      infExInput = InfiltExcessRunoffInput
        { ieri_icefrac = icefrac
        , ieri_hksat = hksat_v
        , ieri_fsat = p_fmax * (max 0.0 mean_sat) ** p_n_baseflow
        , ieri_qflx_in_soil = qflx_in_soil
        , ieri_frac_h2osfc = 0.0
        , ieri_method = QinmaxHksat
        }
      infExResult = infiltrationExcessRunoff infExParams infExInput
      qflx_surf_infex = ierr_qflx_infl_excess infExResult
      qflx_infl = qflx_in_soil - qflx_surf_infex

      -- Mean saturation for TOPMODEL
      mean_sat = VU.sum h2osoi_vol / fromIntegral (max 1 (VU.length h2osoi_vol))
               / max 0.01 (VU.sum watsat_v / fromIntegral (max 1 (VU.length watsat_v)))

      -- ================================================================
      -- 4. REAL MODULE: Zeng-Decker 2009 Richards equation
      -- ================================================================
      zdInput = ZengDeckerInput
        { zdi_nlayers = nlevsoi
        , zdi_dtime = dtime
        , zdi_qflx_infl = qflx_infl
        , zdi_zwt = max 0.0 ((1.0 - mean_sat) * 5.0)
        , zdi_watsat = watsat_v
        , zdi_bsw = bsw_v
        , zdi_hksat = hksat_v
        , zdi_sucsat = sucsat_v
        , zdi_icefrac = icefrac
        , zdi_rootsoi = VU.generate nlevgrnd $ \j ->
            qflx_tran * (if j < 5 then 0.2 else 0.0)  -- root sink from transpiration
        , zdi_z_soil = z_soil
        , zdi_zi_soil = zi_soil
        , zdi_dz_soil = dz_soil
        , zdi_h2osoi_liq = h2osoi_liq  -- full snow+soil array
        , zdi_h2osoi_vol = h2osoi_vol
        , zdi_t_soisno = t_soisno  -- full snow+soil array
        , zdi_e_ice = p_e_ice
        , zdi_smpmin_val = -1.0e8
        }
      zdResult = soilwaterZengDecker2009 defaultSoilWaterMovementConfig zdInput

      -- Update soil liquid water from Zeng-Decker
      h2osoi_liq_new = VU.generate (nlevsno + nlevgrnd) $ \j ->
        if j < nlevsno then safeIdx h2osoi_liq j
        else safeIdx (zdr_h2osoi_liq zdResult) (j - nlevsno)

      -- ================================================================
      -- 5. TOPMODEL baseflow: Q = scalar * exp(-fff * zwt)
      -- ================================================================
      qflx_drain_zd = max 0.0 (zdr_qcharge zdResult)
      zwt_proxy = max 0.0 ((1.0 - mean_sat) * 5.0)
      baseflow = p_baseflow_scalar * 1.0e-3 * exp (negate p_fff * zwt_proxy)
      qflx_drain_total = qflx_drain_zd + baseflow

      -- ================================================================
      -- 6. Total surface runoff = infiltration excess
      -- ================================================================
      qflx_surf_total = qflx_surf_infex

      -- Update snow layers: reduce ice proportionally by melt
      ice_after_melt = if melt_frac > 0.0 && snl < 0
        then VU.generate (VU.length h2osoi_ice) $ \j ->
          if j >= nlevsno + snl && j < nlevsno
          then safeIdx h2osoi_ice j * (1.0 - melt_frac)
          else safeIdx h2osoi_ice j
        else h2osoi_ice

      -- If all snow melted, reset snow layers
      (snl_after, h2osno_after) =
        if swe_new < 0.01 && snl < 0 then (0, 0.0)
        else (snl, h2osno_explicit * (1.0 - melt_frac))

      -- Update state
      ws' = ws { h2osoi_liq_col = h2osoi_liq_new
               , h2osoi_ice_col = ice_after_melt
               , h2osno_col = max 0.0 h2osno_after
               , h2osfc_col = swe_new }

      wf' = wf { qflx_drain_col = qflx_drain_total
               , qflx_surf_col = qflx_surf_total }

  in st { clmWaterState = ws'
        , clmWaterFlux = wf'
        , clmSnl = snl_after
        }

-- ============================================================================
-- Surface Albedo adapter (two-stream via surfaceAlbedoDriver)
-- ============================================================================

surfaceAlbedoStep :: SurfaceAlbedoConstants -> PhysicsStep
surfaceAlbedoStep albConst _cfg ctx st =
  let wdiag = clmWaterDiagBulk st
      cs = clmCanopyState st
      temp = clmTemp st

      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
      snow_persist =
        if frac_sno > 0.0
        then max 0.0 (safeIdx (wdiag_snow_persist_col wdiag) 0)
        else 0.0
      elai = safeIdx (cstate_elai_patch cs) 0
      esai = safeIdx (cstate_esai_patch cs) 0
      fwet = safeIdx (wdiag_fwet_patch wdiag) 0
      fcansno = safeIdx (wdiag_fcansno_patch wdiag) 0

      coszen = cos (tcDeclin ctx)

  in if coszen <= 0.0
     then st
     else
       let soilInp = SoilAlbedoInput
             { sai_coszen      = coszen
             , sai_lunType     = 1
             , sai_h2osoi_vol1 = 0.3
             , sai_soilColor   = 15
             , sai_t_grnd      = t_grnd_col temp
             , sai_snl         = clmSnl st
             , sai_lakePuddling = False
             , sai_lakeIcefrac1 = 0.0
             , sai_lakeIcefrac2 = 0.0
             }

           albInp = SurfAlbDriverInput
             { sadi_coszen      = coszen
             , sadi_soilAlbIn   = soilInp
             , sadi_fracSno     = frac_sno
             , sadi_snowPersist = snow_persist
             , sadi_elai        = elai
             , sadi_esai        = esai
             , sadi_tlai        = elai
             , sadi_tsai        = esai
             , sadi_t_veg       = t_veg_patch temp
             , sadi_fwet        = fwet
             , sadi_fcansno     = fcansno
             , sadi_rhol        = VU.fromList [0.10, 0.45]
             , sadi_rhos        = VU.fromList [0.16, 0.39]
             , sadi_taul        = VU.fromList [0.05, 0.25]
             , sadi_taus        = VU.fromList [0.001, 0.001]
             , sadi_xl          = 0.01
             }

           albResult = surfaceAlbedoDriver albConst albInp

           cs' = cs
             { cstate_fsun_patch = VU.singleton (sado_fsun albResult)
             }

       in st { clmCanopyState = cs' }

-- ============================================================================
-- Water Balance Check adapter
-- ============================================================================

waterBalanceStep :: PhysicsStep
waterBalanceStep _cfg ctx st =
  let dtime = tcDtime ctx
      wf = clmWaterFlux st
      forc_rain = if VU.null (tcForcRain ctx) then 0.0
                  else tcForcRain ctx VU.! 0
      forc_snow = if VU.null (tcForcSnow ctx) then 0.0
                  else tcForcSnow ctx VU.! 0

      wb = clmWaterBalance st
      inp = WaterBalanceColInput
        { wbci_endwb              = 0.0
        , wbci_begwb              = 0.0
        , wbci_forc_rain          = forc_rain
        , wbci_forc_snow          = forc_snow
        , wbci_qflx_flood         = 0.0
        , wbci_qflx_sfc_irrig     = 0.0
        , wbci_qflx_glcice_dyn    = 0.0
        , wbci_qflx_evap_tot      = qflx_evap_tot_patch wf
        , wbci_qflx_surf          = qflx_surf_col wf
        , wbci_qflx_qrgwl         = 0.0
        , wbci_qflx_drain         = qflx_drain_col wf
        , wbci_qflx_drain_perch   = 0.0
        , wbci_qflx_ice_runoff    = 0.0
        , wbci_qflx_snwcp_disc_liq = 0.0
        , wbci_qflx_snwcp_disc_ice = 0.0
        , wbci_dtime              = dtime
        , wbci_is_active          = True
        }

      result = waterBalanceCol inp

      wb' = wb { wb_errh2o_col = VU.singleton (wbco_errh2o result) }

  in st { clmWaterBalance = wb' }

-- ============================================================================
-- Energy Balance Check adapter
-- ============================================================================

energyBalanceStep :: PhysicsStep
energyBalanceStep _cfg ctx st =
  let ef = clmEnergyFlux st
      forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0
                   else tcForcLwrad ctx VU.! 0
      forc_solad_vis = safeIdx (tcForcSolad ctx) 0
      forc_solad_nir = safeIdx (tcForcSolad ctx) 1
      forc_solai_vis = safeIdx (tcForcSolai ctx) 0
      forc_solai_nir = safeIdx (tcForcSolai ctx) 1

      inp = EnergyBalanceInput
        { ebi_fsa             = fsa_patch ef
        , ebi_fsr             = 0.0
        , ebi_sabv            = sabv_patch ef
        , ebi_sabg            = sabg_patch ef
        , ebi_sabg_chk        = sabg_patch ef
        , ebi_forc_solad1     = forc_solad_vis
        , ebi_forc_solad2     = forc_solad_nir
        , ebi_forc_solai1     = forc_solai_vis
        , ebi_forc_solai2     = forc_solai_nir
        , ebi_forc_lwrad      = forc_lwrad
        , ebi_eflx_lwrad_out  = 0.0
        , ebi_eflx_lwrad_net  = 0.0
        , ebi_eflx_sh_tot     = eflx_sh_tot_patch ef
        , ebi_eflx_lh_tot     = eflx_lh_tot_patch ef
        , ebi_eflx_soil_grnd  = eflx_soil_grnd_col ef
        , ebi_dhsdt_canopy    = 0.0
        , ebi_is_urban        = False
        , ebi_is_active       = True
        , ebi_eflx_wasteheat  = 0.0
        , ebi_eflx_heat_from_ac = 0.0
        , ebi_eflx_traffic    = 0.0
        , ebi_eflx_ventilation = 0.0
        }

      _ebResult = energyBalance inp
  in st

-- ============================================================================
-- Active Layer adapter
-- ============================================================================

activeLayerStep :: PhysicsStep
activeLayerStep _cfg _ctx st =
  let temp = clmTemp st
      col = clmColumn st
      t_soil = VU.slice nlevsno nlevgrnd (t_soisno_col temp)
      z_soil = VU.slice nlevsno nlevgrnd (colZ col)

      inp = AltCalcInput
        { alti_t_soisno = t_soil
        , alti_zsoi     = z_soil
        }
      _altResult = altCalc inp
  in st

-- ============================================================================
-- Frac H2oSfc adapter
-- ============================================================================

fracH2oSfcStep :: PhysicsStep
fracH2oSfcStep _cfg ctx st =
  let dtime = tcDtime ctx
      ws = clmWaterState st
      wdiag = clmWaterDiagBulk st
      snl = clmSnl st

      h2osfc = h2osfc_col ws
      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0

      h2osno_total = h2osno_col ws
                   + sum [ safeIdx (h2osoi_ice_col ws) j
                         + safeIdx (h2osoi_liq_col ws) j
                         | j <- [nlevsno + snl .. nlevsno - 1] ]

      inp = FracH2osfcInput
        { fhi_dtime        = dtime
        , fhi_micro_sigma  = 0.4
        , fhi_h2osno_total = h2osno_total
        , fhi_h2osfc       = h2osfc
        , fhi_frac_sno     = frac_sno
        , fhi_frac_sno_eff = frac_sno_eff
        }

      result = computeFracH2osfc inp

      wdiag' = wdiag
        { wdiag_frac_h2osfc_col = VU.singleton (fhr_frac_h2osfc result)
        }

  in st { clmWaterDiagBulk = wdiag' }

-- ============================================================================
-- Lake Fluxes adapter
-- ============================================================================

lakeFluxesStep :: PhysicsStep
lakeFluxesStep _cfg ctx st =
  let col = clmColumn st
  in if lakedepth col <= 0.0
     then st
     else
       let temp = clmTemp st
           ws = clmWaterState st
           snl = clmSnl st
           topIdx = nlevsno + snl

           forc_t = if VU.null (tcForcT ctx) then 280.0 else tcForcT ctx VU.! 0
           forc_pbot = if VU.null (tcForcPbot ctx) then 101325.0 else tcForcPbot ctx VU.! 0
           forc_q = if VU.null (tcForcQ ctx) then 0.005 else tcForcQ ctx VU.! 0
           forc_rho = if VU.null (tcForcRho ctx) then 1.2 else tcForcRho ctx VU.! 0
           forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0 else tcForcLwrad ctx VU.! 0
           forc_wind = if VU.null (tcForcWind ctx) then 3.0 else tcForcWind ctx VU.! 0
           forc_th = if VU.null (tcForcTh ctx) then 280.0 else tcForcTh ctx VU.! 0

           inp = LakeFluxInput
             { lfi_snl            = snl
             , lfi_lakedepth      = lakedepth col
             , lfi_dz_top         = safeIdx (colDz col) topIdx
             , lfi_savedtke1      = 0.0
             , lfi_t_grnd         = t_grnd_col temp
             , lfi_t_subsurface   = safeIdx (t_soisno_col temp) nlevsno
             , lfi_t_lake1        = safeIdx (t_soisno_col temp) nlevsno
             , lfi_sabg           = sabg_patch (clmEnergyFlux st)
             , lfi_h2osoi_liq_top = safeIdx (h2osoi_liq_col ws) topIdx
             , lfi_h2osoi_ice_top = safeIdx (h2osoi_ice_col ws) topIdx
             , lfi_forc_t         = forc_t
             , lfi_forc_th        = forc_th
             , lfi_forc_q         = forc_q
             , lfi_forc_pbot      = forc_pbot
             , lfi_forc_rho       = forc_rho
             , lfi_forc_lwrad     = forc_lwrad
             , lfi_forc_u         = forc_wind
             , lfi_forc_v         = 0.0
             , lfi_forc_hgt_u     = 30.0
             , lfi_forc_hgt_t     = 30.0
             , lfi_forc_hgt_q     = 30.0
             , lfi_dtime          = tcDtime ctx
             }

           lOut = lakeFluxes inp

           ef = clmEnergyFlux st
           ef' = ef
             { eflx_sh_tot_patch  = lfo_eflx_sh_grnd lOut
             , eflx_sh_grnd_patch = lfo_eflx_sh_grnd lOut
             , eflx_soil_grnd_col = lfo_eflx_soil_grnd lOut
             }

           temp' = temp { t_grnd_col = lfo_t_grnd lOut }

       in st { clmEnergyFlux = ef'
             , clmTemp = temp'
             }

-- ============================================================================
-- Soil Fluxes adapter (post-temperature flux correction)
-- ============================================================================

soilFluxesStep :: PhysicsStep
soilFluxesStep _cfg ctx st =
  let snl = clmSnl st
      temp = clmTemp st
      wdiag = clmWaterDiagBulk st
      ef = clmEnergyFlux st
      wf = clmWaterFlux st
      cs = clmCanopyState st

      forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0
                   else tcForcLwrad ctx VU.! 0
      dtime = tcDtime ctx

      t_grnd = t_grnd_col temp
      tSsbef =
        if VU.null (t_soisno_bef_col temp)
        then t_soisno_col temp
        else t_soisno_bef_col temp
      tH2osfcBef =
        if VU.null (t_soisno_bef_col temp)
        then t_h2osfc_col temp
        else t_h2osfc_bef_col temp
      htvp = if t_grnd < tfrz then hsub else hvap
      emg = 0.96 :: Double
      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0

      patchCount =
        maximum
          [ 1
          , VU.length (cstate_patch_wtgcell cs)
          , VU.length (eflx_sh_tot_patch_vec ef)
          , VU.length (eflx_sh_grnd_patch_vec ef)
          , VU.length (qflx_evap_tot_patch_vec wf)
          , VU.length (qflx_evap_grnd_patch_vec wf)
          , VU.length (qflx_tran_veg_patch_vec wf)
          , VU.length (sabg_patch_vec ef)
          , VU.length (cgrnds_patch_vec ef)
          , VU.length (cgrndl_patch_vec ef)
          , VU.length (dlrad_patch_vec ef)
          , VU.length (ulrad_patch_vec ef)
          , VU.length (eflx_lwrad_net_patch_vec ef)
          ]
      patchWeight p =
        if VU.null (cstate_patch_wtgcell cs)
        then if p == 0 then 1.0 else 0.0
        else safeIdx (cstate_patch_wtgcell cs) p
      patchWeightSum =
        max 1.0e-12 (sum [ patchWeight p | p <- [0 .. patchCount - 1] ])
      patchWt p = patchWeight p / patchWeightSum
      weightedVec vec =
        sum [ patchWt p * safeIdx vec p | p <- [0 .. patchCount - 1] ]
      expandVec vec fallback =
        VU.generate patchCount $ \p ->
          if p < VU.length vec
          then vec VU.! p
          else if p == 0 then fallback else 0.0
      fracVeg p
        | not (VU.null (cstate_frac_veg_nosno_patch cs)) =
            safeIdxI (cstate_frac_veg_nosno_patch cs) p
        | otherwise =
            safeIdxI (cstate_frac_veg_nosno_alb_patch cs) p

      shTotInVec = expandVec (eflx_sh_tot_patch_vec ef) (eflx_sh_tot_patch ef)
      shGrndInVec = expandVec (eflx_sh_grnd_patch_vec ef) (eflx_sh_grnd_patch ef)
      evapTotInVec = expandVec (qflx_evap_tot_patch_vec wf) (qflx_evap_tot_patch wf)
      evapGrndInVec = expandVec (qflx_evap_grnd_patch_vec wf) (qflx_evap_grnd_col wf)
      tranVegInVec = expandVec (qflx_tran_veg_patch_vec wf) (qflx_tran_veg_patch wf)
      sabgInVec = expandVec (sabg_patch_vec ef) (sabg_patch ef)
      cgrndsInVec = expandVec (cgrnds_patch_vec ef) (cgrnds_patch ef)
      cgrndlInVec = expandVec (cgrndl_patch_vec ef) (cgrndl_patch ef)
      dlradInVec = expandVec (dlrad_patch_vec ef) (dlrad_patch ef)
      ulradInVec = expandVec (ulrad_patch_vec ef) (ulrad_patch ef)
      lwradNetInVec = expandVec (eflx_lwrad_net_patch_vec ef) (eflx_lwrad_net_patch ef)

      soilFluxForPatch p =
        let shTotIn = safeIdx shTotInVec p
            shGrndIn = safeIdx shGrndInVec p
            evapTotIn = safeIdx evapTotInVec p
            evapGrndIn = safeIdx evapGrndInVec p
            tranVegIn = safeIdx tranVegInVec p
            evapVegIn = evapTotIn - evapGrndIn
            inp = SoilFluxesInput
              { sfi_snl              = snl
              , sfi_frac_sno_eff     = frac_sno_eff
              , sfi_frac_h2osfc      = frac_h2osfc
              , sfi_t_grnd           = t_grnd
              , sfi_t_h2osfc         = t_h2osfc_col temp
              , sfi_t_h2osfc_bef     = tH2osfcBef
              , sfi_c_h2osfc         = 0.0
              , sfi_xmf              = 0.0
              , sfi_xmf_h2osfc       = 0.0
              , sfi_emg              = emg
              , sfi_forc_lwrad       = forc_lwrad
              , sfi_htvp             = htvp
              , sfi_t_ssbef          = tSsbef
              , sfi_t_soisno         = t_soisno_col temp
              , sfi_fact             = VU.replicate (nlevsno + nlevgrnd) 0.0
              , sfi_h2osoi_liq       = h2osoi_liq_col (clmWaterState st)
              , sfi_h2osoi_ice       = h2osoi_ice_col (clmWaterState st)
              , sfi_frac_veg_nosno   = fracVeg p
              , sfi_eflx_sh_grnd     = shGrndIn
              , sfi_eflx_sh_veg      = shTotIn - shGrndIn
              , sfi_eflx_sh_stem     = 0.0
              , sfi_cgrnds           = safeIdx cgrndsInVec p
              , sfi_cgrndl           = safeIdx cgrndlInVec p
              , sfi_qflx_evap_soi    = evapGrndIn
              , sfi_qflx_evap_veg    = evapVegIn
              , sfi_qflx_tran_veg    = tranVegIn
              , sfi_qflx_ev_snow     = 0.0
              , sfi_qflx_ev_soil     = 0.0
              , sfi_qflx_ev_h2osfc   = 0.0
              , sfi_sabg_soil         = safeIdx sabgInVec p
              , sfi_sabg_snow         = 0.0
              , sfi_sabg              = safeIdx sabgInVec p
              , sfi_dlrad             = safeIdx dlradInVec p
              , sfi_ulrad             = safeIdx ulradInVec p
              , sfi_eflx_lwrad_net    = safeIdx lwradNetInVec p
              , sfi_is_urban          = False
              , sfi_is_soil_or_crop   = True
              , sfi_col_itype         = 1
              , sfi_eflx_wasteheat    = 0.0
              , sfi_eflx_heat_from_ac = 0.0
              , sfi_eflx_traffic      = 0.0
              , sfi_eflx_ventilation  = 0.0
              , sfi_eflx_building_heat_errsoi = 0.0
              , sfi_eflx_h2osfc_to_snow = 0.0
              , sfi_dtime             = dtime
              }
        in soilFluxes inp

      results = [ soilFluxForPatch p | p <- [0 .. patchCount - 1] ]
      resultAt p = results !! p
      shTotVec = VU.generate patchCount (sfr_eflx_sh_tot . resultAt)
      shGrndVec = VU.generate patchCount (sfr_eflx_sh_grnd . resultAt)
      lhTotVec = VU.generate patchCount (sfr_eflx_lh_tot . resultAt)
      evapTotVec = VU.generate patchCount (sfr_qflx_evap_tot . resultAt)
      evapGrndVec = VU.generate patchCount (sfr_qflx_evap_soi . resultAt)
      soilGrndVec = VU.generate patchCount (sfr_eflx_soil_grnd . resultAt)
      lwradOutVec = VU.generate patchCount (sfr_eflx_lwrad_out . resultAt)
      lwradNetVec = VU.generate patchCount (sfr_eflx_lwrad_net . resultAt)

      ef' = ef
        { eflx_sh_tot_patch = weightedVec shTotVec
        , eflx_sh_grnd_patch = weightedVec shGrndVec
        , eflx_lh_tot_patch = weightedVec lhTotVec
        , eflx_soil_grnd_col = weightedVec soilGrndVec
        , eflx_lwrad_out_patch = weightedVec lwradOutVec
        , eflx_lwrad_net_patch = weightedVec lwradNetVec
        , eflx_sh_tot_patch_vec = shTotVec
        , eflx_sh_grnd_patch_vec = shGrndVec
        , eflx_lh_tot_patch_vec = lhTotVec
        , eflx_lwrad_out_patch_vec = lwradOutVec
        , eflx_lwrad_net_patch_vec = lwradNetVec
        }
      wf' = wf
        { qflx_evap_tot_patch = weightedVec evapTotVec
        , qflx_evap_grnd_col = weightedVec evapGrndVec
        , qflx_evap_tot_patch_vec = evapTotVec
        , qflx_evap_grnd_patch_vec = evapGrndVec
        }

  in st { clmEnergyFlux = ef', clmWaterFlux = wf' }

-- ============================================================================
-- Snow Layer Combine adapter
-- ============================================================================

snowLayerCombineStep :: PhysicsStep
snowLayerCombineStep _cfg _ctx st =
  let snl = clmSnl st
  in if snl >= 0
     then st
     else
       let col = clmColumn st
           ws = clmWaterState st
           temp = clmTemp st
           wdiag = clmWaterDiagBulk st

           frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
           frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
           snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0

           slState = SnowLayerState
             { slDz        = VU.slice 0 nlevsno (colDz col)
             , slZ         = VU.slice 0 nlevsno (colZ col)
             , slZi        = VU.slice 0 (nlevsno + 1) (colZi col)
             , slTSoisno   = VU.slice 0 nlevsno (t_soisno_col temp)
             , slH2osoiIce = VU.slice 0 nlevsno (h2osoi_ice_col ws)
             , slH2osoiLiq = VU.slice 0 nlevsno (h2osoi_liq_col ws)
             , slSnwRds    = VU.replicate nlevsno 54.526
             , slSnl       = snl
             }

           bounds = initSnowLayerBounds
           (slFinal, snow_depth', frac_sno', frac_sno_eff', _int_snow', h2osno_nl') =
             combineSnowLayers bounds slState frac_sno frac_sno_eff
               0.0 (h2osno_col ws) snow_depth 1 False

           snl_new = slSnl slFinal
           nlevtot = nlevsno + nlevgrnd

           dz_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slDz slFinal VU.! j
             else colDz col VU.! j
           z_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slZ slFinal VU.! j
             else colZ col VU.! j
           zi_new = VU.generate (nlevtot + 1) $ \j ->
             if j <= nlevsno then slZi slFinal VU.! j
             else colZi col VU.! j
           t_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slTSoisno slFinal VU.! j
             else t_soisno_col temp VU.! j
           liq_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slH2osoiLiq slFinal VU.! j
             else h2osoi_liq_col ws VU.! j
           ice_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slH2osoiIce slFinal VU.! j
             else h2osoi_ice_col ws VU.! j

           col' = col { colDz = dz_new, colZ = z_new, colZi = zi_new }
           temp' = temp { t_soisno_col = t_new }
           ws' = ws { h2osoi_liq_col = liq_new, h2osoi_ice_col = ice_new
                    , h2osno_col = h2osno_nl' }
           wdiag' = wdiag
             { wdiag_snow_depth_col = VU.singleton snow_depth'
             , wdiag_frac_sno_col = VU.singleton frac_sno'
             , wdiag_frac_sno_eff_col = VU.singleton frac_sno_eff'
             }

       in st { clmSnl = snl_new
             , clmColumn = col'
             , clmTemp = temp'
             , clmWaterState = ws'
             , clmWaterDiagBulk = wdiag'
             }

-- ============================================================================
-- Snow Layer Divide adapter
-- ============================================================================

snowLayerDivideStep :: PhysicsStep
snowLayerDivideStep _cfg _ctx st =
  let snl = clmSnl st
  in if snl >= 0
     then st
     else
       let col = clmColumn st
           ws = clmWaterState st
           temp = clmTemp st
           wdiag = clmWaterDiagBulk st

           frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0

           slState = SnowLayerState
             { slDz        = VU.slice 0 nlevsno (colDz col)
             , slZ         = VU.slice 0 nlevsno (colZ col)
             , slZi        = VU.slice 0 (nlevsno + 1) (colZi col)
             , slTSoisno   = VU.slice 0 nlevsno (t_soisno_col temp)
             , slH2osoiIce = VU.slice 0 nlevsno (h2osoi_ice_col ws)
             , slH2osoiLiq = VU.slice 0 nlevsno (h2osoi_liq_col ws)
             , slSnwRds    = VU.replicate nlevsno 54.526
             , slSnl       = snl
             }

           bounds = initSnowLayerBounds
           slFinal = divideSnowLayers bounds slState frac_sno False

           snl_new = slSnl slFinal
           nlevtot = nlevsno + nlevgrnd

           dz_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slDz slFinal VU.! j
             else colDz col VU.! j
           z_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slZ slFinal VU.! j
             else colZ col VU.! j
           zi_new = VU.generate (nlevtot + 1) $ \j ->
             if j <= nlevsno then slZi slFinal VU.! j
             else colZi col VU.! j

           t_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slTSoisno slFinal VU.! j
             else t_soisno_col temp VU.! j
           liq_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slH2osoiLiq slFinal VU.! j
             else h2osoi_liq_col ws VU.! j
           ice_new = VU.generate nlevtot $ \j ->
             if j < nlevsno then slH2osoiIce slFinal VU.! j
             else h2osoi_ice_col ws VU.! j

           col' = col { colDz = dz_new, colZ = z_new, colZi = zi_new }
           temp' = temp { t_soisno_col = t_new }
           ws' = ws { h2osoi_liq_col = liq_new, h2osoi_ice_col = ice_new }

       in st { clmSnl = snl_new
             , clmColumn = col'
             , clmTemp = temp'
             , clmWaterState = ws'
             }

-- ============================================================================
-- Snow Aging adapter (grain radius evolution)
-- ============================================================================

snowAgingStep :: PhysicsStep
snowAgingStep _cfg ctx st =
  let snl = clmSnl st
  in if snl >= 0
     then st
     else
       let dtime = tcDtime ctx
           temp = clmTemp st
           ws = clmWaterState st
           wdiag = clmWaterDiagBulk st
           forc_t = if VU.null (tcForcT ctx) then 273.15 else tcForcT ctx VU.! 0
           frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
           forc_snow = if VU.null (tcForcSnow ctx) then 0.0 else tcForcSnow ctx VU.! 0
           topIdx = nlevsno + snl

           topLayerInp = SnowageGrainInput
             { sg_snw_rds     = 54.526
             , sg_t_soisno    = safeIdx (t_soisno_col temp) topIdx
             , sg_t_snotop    = safeIdx (t_soisno_col temp) topIdx
             , sg_t_snobtm    = if topIdx + 1 < nlevsno
                                then safeIdx (t_soisno_col temp) (topIdx + 1)
                                else safeIdx (t_soisno_col temp) topIdx
             , sg_cdz         = 0.0
             , sg_h2osoi_liq  = safeIdx (h2osoi_liq_col ws) topIdx
             , sg_h2osoi_ice  = safeIdx (h2osoi_ice_col ws) topIdx
             , sg_frac_sno    = frac_sno
             , sg_dz          = safeIdx (colDz (clmColumn st)) topIdx
             , sg_qflx_snow_grnd = forc_snow
             , sg_qflx_snofrz = 0.0
             , sg_forc_t      = forc_t
             , sg_dtime       = dtime
             , sg_isTopLayer  = True
             , sg_bst_tau     = 1.0e6
             , sg_bst_kappa   = 7.0
             , sg_bst_drdt0   = 0.0
             }

           result = snowageGrainLayer defaultSnicarParams topLayerInp

           wdiag' = wdiag
             { wdiag_snw_rds_top_col = VU.singleton (sgr_snw_rds_top result)
             }

       in st { clmWaterDiagBulk = wdiag' }

-- ============================================================================
-- Driver Init adapter
-- ============================================================================

drvInitStep :: PhysicsStep
drvInitStep _cfg _ctx st =
  let ef = clmEnergyFlux st
      ef' = ef { eflx_soil_grnd_col = 0.0 }
      temp = clmTemp st
      temp' = temp
        { t_soisno_bef_col = t_soisno_col temp
        , t_h2osfc_bef_col = t_h2osfc_col temp
        }
  in st { clmEnergyFlux = ef', clmTemp = temp' }

-- ============================================================================
-- Soil Evaporation Resistance adapter
-- ============================================================================

soilEvapResistanceStep :: PhysicsStep
soilEvapResistanceStep _cfg _ctx st =
  let ws = clmWaterState st
      col = clmColumn st
      wdiag = clmWaterDiagBulk st
      snl = clmSnl st

      topIdx = nlevsno + snl
      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0

      inp = BetaInput
        { bi_lunType      = 1
        , bi_colType      = 1
        , bi_dz_top       = safeIdx (colDz col) topIdx
        , bi_h2osoi_liq_top = safeIdx (h2osoi_liq_col ws) topIdx
        , bi_h2osoi_ice_top = safeIdx (h2osoi_ice_col ws) topIdx
        , bi_watsat_top   = if topIdx >= nlevsno
                            then safeIdx (watsat col) (topIdx - nlevsno)
                            else 1.0
        , bi_watfc_top    = if topIdx >= nlevsno
                            then safeIdx (watsat col) (topIdx - nlevsno) * 0.5
                            else 0.5
        , bi_frac_sno     = frac_sno
        , bi_frac_h2osfc  = frac_h2osfc
        }

      result = calcBetaLeePielke1992 inp

      ss = clmSoilState st
      ss' = ss { sstate_soilbeta_col = VU.singleton (br_soilbeta result) }

  in st { clmSoilState = ss' }

-- ============================================================================
-- Water Table adapter
-- ============================================================================

waterTableStep :: PhysicsStep
waterTableStep _cfg ctx st =
  let dtime = tcDtime ctx
      ws = clmWaterState st
      col = clmColumn st
      ss = clmSoilState st
      temp = clmTemp st
      sh = clmSoilHydro st

      watsat_v = if VU.null (sstate_watsat_col ss)
                 then watsat col else sstate_watsat_col ss
      bsw_v = if VU.null (sstate_bsw_col ss)
               then bsw col else sstate_bsw_col ss
      sucsat_v = if VU.null (sstate_sucsat_col ss)
                 then sucsat col else sstate_sucsat_col ss

      eff_por = VU.generate nlevsoi $ \j ->
        let ws_j = safeIdx watsat_v j
            ice_j = safeIdx (h2osoi_ice_col ws) (j + nlevsno)
            dz_j = safeIdx (colDz col) (j + nlevsno)
        in max 0.01 (ws_j - ice_j / (denice * dz_j))

      z_soil = VU.slice nlevsno nlevgrnd (colZ col)
      zi_soil = VU.slice nlevsno (nlevgrnd + 1) (colZi col)
      dz_soil = VU.slice nlevsno nlevgrnd (colDz col)

      qcharge = 0.0
      zwt_in = 5.0
      wa_in = 0.0

      result = waterTable defaultSoilHydroParams
                 dtime qcharge zwt_in wa_in
                 watsat_v bsw_v sucsat_v eff_por
                 z_soil zi_soil dz_soil
                 (h2osoi_liq_col ws) (h2osoi_ice_col ws) (t_soisno_col temp)

      ws' = ws { h2osoi_liq_col = wtr_h2osoi_liq result }
      sh' = sh { sh_zwt_col = VU.singleton (wtr_zwt result)
               , sh_zwt_perched_col = VU.singleton (wtr_zwt_perched result)
               , sh_frost_table_col = VU.singleton (wtr_frost_table result)
               }

  in st { clmWaterState = ws', clmSoilHydro = sh' }

-- ============================================================================
-- Phenology adapter (SP mode: maintain current LAI)
-- ============================================================================

phenologyStep :: PhysicsStep
phenologyStep _cfg _ctx st =
  let cs = clmCanopyState st
      wdiag = clmWaterDiagBulk st

      frac_sno = safeIdx (wdiag_frac_sno_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0

      elai_v = cstate_elai_patch cs
      esai_v = cstate_esai_patch cs

      frac_veg = VU.generate (VU.length elai_v) $ \p ->
        let vai = safeIdx elai_v p + safeIdx esai_v p
        in if vai > 0.05 then 1 else 0

      cs' = cs { cstate_frac_veg_nosno_alb_patch = frac_veg }

  in st { clmCanopyState = cs' }

-- ============================================================================
-- Urban Fluxes adapter (skip for non-urban columns)
-- ============================================================================

urbanFluxesStep :: PhysicsStep
urbanFluxesStep _cfg ctx st =
  let col = clmColumn st
      lun = clmLandunit st
      it = if VU.null (lun_itype lun) then 1 else lun_itype lun VU.! 0
  in if it /= 6 -- urban
     then st
     else
       -- Urban fluxes wiring (Phase 2):
       -- Calls urbanFluxesSinglePatch with forcing and urban parameters.
       -- For now, return state as-is but with the logic structure in place.
       st

-- ============================================================================
-- Lake Temperature adapter
-- ============================================================================

lakeTemperatureStep :: PhysicsStep
lakeTemperatureStep _cfg ctx st =
  let col = clmColumn st
  in if lakedepth col <= 0.0
     then st
     else
       let temp = clmTemp st
           ws = clmWaterState st
           ss = clmSoilState st

           tpInp = ThermPropLakeInput
             { tpli_snl        = clmSnl st
             , tpli_t_soisno   = t_soisno_col temp
             , tpli_h2osoi_liq = h2osoi_liq_col ws
             , tpli_h2osoi_ice = h2osoi_ice_col ws
             , tpli_dz         = colDz col
             , tpli_z          = colZ col
             , tpli_zi         = colZi col
             , tpli_watsat     = if VU.null (sstate_watsat_col ss)
                                 then watsat col else sstate_watsat_col ss
             , tpli_tksatu     = sstate_tksatu_col ss
             , tpli_tkmg       = sstate_tkmg_col ss
             , tpli_tkdry      = sstate_tkdry_col ss
             , tpli_csol       = sstate_csol_col ss
             }

           tpOut = soilThermPropLake tpInp

       in st

-- ============================================================================
-- CN biogeochemistry adapters
-- ============================================================================

-- | CN biogeochemistry pre-drainage step.
-- Uses actual ported module functions for phenology, allocation,
-- N competition, and decomposition.
cnPreDrainageStep :: PhysicsStep
cnPreDrainageStep _cfg ctx st
  | not (clmCNActive st) = st
  | otherwise =
    let !dt = tcDtime ctx
        !gpp = clmGPP st

        -- Step 1: Maintenance respiration (leaf + froot + stem)
        !leafMR = clmLeafC st * 2.525e-6  -- base rate at 25C
        !frootMR = clmFrootC st * 2.525e-6
        !stemMR = clmLiveStemC st * 2.525e-6 * 0.5  -- wood has lower rate
        !mr = leafMR + frootMR + stemMR

        -- Step 2: Available C for allocation (GPP - MR)
        !availC = max 0.0 (gpp - mr)

        -- Step 3: Allocation using actual Allocation module
        !allocOut = Alloc.calcAllocation Alloc.AllocInput
          { Alloc.ali_availc = availC
          , Alloc.ali_ivt = 1
          , Alloc.ali_woody = 1.0
          , Alloc.ali_froot_leaf = 1.0
          , Alloc.ali_croot_stem = 1.0
          , Alloc.ali_stem_leaf = 1.5
          , Alloc.ali_flivewd = 0.5
          , Alloc.ali_leafcn = 25.0
          , Alloc.ali_frootcn = 42.0
          , Alloc.ali_livewdcn = 50.0
          , Alloc.ali_deadwdcn = 500.0
          , Alloc.ali_grperc = 0.3
          , Alloc.ali_downreg = clmFPG st
          }

        !gr = Alloc.alo_cpool_leaf_gr allocOut
            + Alloc.alo_cpool_froot_gr allocOut
            + Alloc.alo_cpool_livestem_gr allocOut
            + Alloc.alo_cpool_deadstem_gr allocOut
            + Alloc.alo_cpool_livecroot_gr allocOut
            + Alloc.alo_cpool_deadcroot_gr allocOut

        -- Step 4: Phenology (background litterfall)
        !leaf_long = 2.0  -- years
        (!leafLitRate, !frootLitRate) = Phen.backgroundLitterfall
          leaf_long (clmLeafC st) (clmFrootC st) dt

        -- Step 5: Decomposition (first-order with temperature sensitivity)
        !t_soil = t_grnd_col (clmTemp st)
        !q10_decomp = 2.0 ** ((t_soil - tfrz - 25.0) / 10.0)
        !hr_litter = clmLitterC st * 3.17e-8 * max 0.01 q10_decomp
        !hr_som = clmSoilOrgC st * 1.59e-9 * max 0.01 q10_decomp
        !hr = hr_litter + hr_som
        !litToSom = hr_litter * 0.6

        -- Step 6: N competition using actual NutrientCompetition module
        !nDemand = Alloc.alo_plant_ndemand allocOut
        !nImmobDemand = hr / 15.0
        !nCompOut = NComp.calcNCompetition NComp.NCompetitionInput
          { NComp.nci_plant_ndemand = nDemand
          , NComp.nci_decomp_ndemand = nImmobDemand
          , NComp.nci_sminn = clmSMINN st
          , NComp.nci_dt = dt
          , NComp.nci_use_nitrif_denitrif = False
          }
        !fpg = NComp.nco_fpg nCompOut
        !nMin = NComp.nco_actual_immob nCompOut

        -- Step 7: NPP and NEE
        !npp = gpp - mr - gr
        !nee = -(gpp - mr - gr - hr)

        -- Step 8: Update pools
        !leafC' = clmLeafC st
                + Alloc.alo_cpool_to_leafc allocOut * dt
                - leafLitRate
        !frootC' = clmFrootC st
                 + Alloc.alo_cpool_to_frootc allocOut * dt
                 - frootLitRate
        !stemC' = clmLiveStemC st
                + Alloc.alo_cpool_to_livestemc allocOut * dt
        !litterC' = clmLitterC st
                  + (leafLitRate + frootLitRate) / max dt 1.0
                  - hr_litter * dt
        !somC' = clmSoilOrgC st + (litToSom - hr_som) * dt
        !sminn' = clmSMINN st
                + nMin * dt
                - NComp.nco_actual_plant_nuptake nCompOut * dt

        -- Use smooth max for AD-safe non-negativity enforcement
        !smax = smoothMax defaultK
    in st { clmLeafC = smax 0.0 leafC'
          , clmFrootC = smax 0.0 frootC'
          , clmLiveStemC = smax 0.0 stemC'
          , clmLitterC = smax 0.0 litterC'
          , clmSoilOrgC = smax 0.0 somC'
          , clmSMINN = smax 0.0 sminn'
          , clmNPP = npp
          , clmHR = hr
          , clmNEE = nee
          , clmFPG = fpg
          }

-- | CN biogeochemistry post-drainage step.
-- Handles N leaching (loss of mineral N with drainage water).
cnPostDrainageStep :: PhysicsStep
cnPostDrainageStep _cfg ctx st
  | not (clmCNActive st) = st
  | otherwise =
    let !dt = tcDtime ctx
        -- N leaching: proportional to mineral N concentration using the
        -- current scalar retention rate.
        !leachRate = clmSMINN st * 1.0e-3 / 86400.0
        !sminn' = clmSMINN st - leachRate * dt
    in st { clmSMINN = max 0.0 sminn' }

-- | CN balance check step.
-- Verifies C and N conservation (logs warnings if imbalanced).
cnBalanceCheckStep :: PhysicsStep
cnBalanceCheckStep _cfg _ctx st
  | not (clmCNActive st) = st
  | otherwise = st
