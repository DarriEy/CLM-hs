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
  , surfaceHumidityStep
  , canopyHydrologyStep
  , baregroundFluxesStep
  , canopyFluxesStep
  , soilTemperatureFullStep
  , snowWaterStep
  , snowCompactionStep
  , hydrologyDrainageStep
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

import CLM.Types.ColumnData (ColumnData(..))
import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData (WaterStateData(..))
import CLM.Types.WaterFluxData (WaterFluxData(..))
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..))
import CLM.Types.EnergyFluxData (EnergyFluxData(..))
import CLM.Types.CanopyStateData (CanopyStateData(..))
import CLM.Types.SoilStateData (SoilStateData(..))
import CLM.Types.GridcellData (GridcellData(..))
import CLM.Types.FrictionVelocityData (FrictionVelocityData(..))

-- ============================================================================
-- Wired pipeline: all available adapters plugged in
-- ============================================================================

-- | Physics pipeline with all implemented modules wired in.
-- Replaces the default all-idStep pipeline.
wiredPhysicsPipeline :: PhysicsPipeline
wiredPhysicsPipeline = defaultPhysicsPipeline
  { ppSurfaceHumidity   = surfaceHumidityStep
  , ppCanopyInterception = canopyHydrologyStep
  , ppBaregroundFluxes   = baregroundFluxesStep
  , ppCanopyFluxes       = canopyFluxesStep
  , ppSoilTemperature    = soilTemperatureFullStep
  , ppSnowWater          = snowWaterStep
  , ppSnowCompaction     = snowCompactionStep
  , ppHydrologyDrainage  = hydrologyDrainageStep
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

      elai = safeIdx (cstate_elai_patch cs) 0
      esai = safeIdx (cstate_esai_patch cs) 0
      frac_veg = safeIdxI (cstate_frac_veg_nosno_alb_patch cs) 0

      liqcan_in = safeIdx (wdiag_h2ocan_patch wdiag) 0
      snocan_in = 0.0

      inp = CanopyHydrologyInput
        { chi_params               = defaultCanopyHydroParams
        , chi_dtime                = dtime
        , chi_frac_veg_nosno       = frac_veg
        , chi_elai                 = elai
        , chi_esai                 = esai
        , chi_forc_rain            = forc_rain
        , chi_forc_snow_col        = forc_snow
        , chi_forc_t               = forc_t
        , chi_forc_wind            = forc_wind
        , chi_col_itype            = 1  -- soil column
        , chi_qflx_irrig_sprinkler = 0.0
        , chi_qflx_irrig_drip      = 0.0
        , chi_snocan_in            = snocan_in
        , chi_liqcan_in            = liqcan_in
        , chi_wtcol                = 1.0
        }

      result = canopyInterceptionAndThroughfall inp

      wf = clmWaterFlux st
      wf' = wf
        { qflx_rain_grnd_col = chr_qflx_liq_grnd_col result
        , qflx_snow_grnd_col = chr_qflx_snow_grnd_col result
        }

      wdiag' = wdiag
        { wdiag_fwet_patch = VU.singleton (chr_fwet result)
        , wdiag_fdry_patch = VU.singleton (chr_fdry result)
        , wdiag_fcansno_patch = VU.singleton (chr_fcansno result)
        }

  in st { clmWaterFlux = wf'
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
      forc_hgt = 30.0

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

      ef = clmEnergyFlux st
      ef' = ef
        { eflx_sh_tot_patch  = bgo_eflx_sh_tot bgOut
        , eflx_sh_grnd_patch = bgo_eflx_sh_grnd bgOut
        }

      temp' = temp
        { t_ref2m_patch = bgo_t_ref2m bgOut
        }

      fv = clmFrictionVel st
      fv' = fv
        { fvel_ram1_patch = VU.singleton (bgo_ram1 bgOut)
        , fvel_ustar_patch = VU.singleton (bgo_ustar bgOut)
        }

  in st { clmEnergyFlux = ef'
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
      cs = clmCanopyState st
      wdiag = clmWaterDiagBulk st

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
      forc_hgt = 30.0
      dtime = tcDtime ctx

      htvp = if t_grnd < tfrz then hsub else hvap

      qg = safeIdx (wdiag_qg_col wdiag) 0
      qg_snow = safeIdx (wdiag_qg_snow_col wdiag) 0
      qg_soil = safeIdx (wdiag_qg_soil_col wdiag) 0
      qg_h2osfc = safeIdx (wdiag_qg_h2osfc_col wdiag) 0
      dqgdT = safeIdx (wdiag_dqgdT_col wdiag) 0

      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0
      fwet = safeIdx (wdiag_fwet_patch wdiag) 0
      fdry = safeIdx (wdiag_fdry_patch wdiag) 0

      elai = safeIdx (cstate_elai_patch cs) 0
      esai = safeIdx (cstate_esai_patch cs) 0
      htop = max 0.1 (safeIdx (cstate_htop_patch cs) 0)
      frac_veg = safeIdxI (cstate_frac_veg_nosno_alb_patch cs) 0

      thm = forc_t + 0.0098 * forc_hgt
      thv = forc_th * (1.0 + 0.61 * forc_q)

      z0mr = safeIdx (cstate_z0m_patch cs) 0
      z0mv = if z0mr > 0.0 then z0mr else 0.055 * htop
      displa = safeIdx (cstate_displa_patch cs) 0
      displa' = if displa > 0.0 then displa else 0.67 * htop

      emg = 0.96
      avmuir = 1.0
      emv = 1.0 - exp (-(elai + esai) / avmuir)

      vai = elai + esai
      canopy_transmit = exp (-0.5 * vai)
      fsa_est = 100.0  -- simplified radiation estimate
      sabv = max 0.0 (fsa_est * (1.0 - canopy_transmit))

      laisun = safeIdx (cstate_laisun_patch cs) 0
      laisha = safeIdx (cstate_laisha_patch cs) 0
      laisun' = if laisun > 0.0 then laisun else elai * 0.5
      laisha' = if laisha > 0.0 then laisha else elai * 0.5

      soilbeta = safeIdx (sstate_soilbeta_col (clmSoilState st)) 0
      soilbeta' = if soilbeta == 0.0
                  then if t_grnd < tfrz then 0.01 else 1.0
                  else soilbeta

      cgrnds0 = forc_rho * cpair / 100.0
      cgrndl0 = forc_rho / 100.0 * dqgdT

  in if frac_veg == 0 || elai <= 0.05
     then st  -- no canopy: bareground handles it
     else
       let inp = CanopyFluxesInput
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
             , cfi_frac_veg_nosno = frac_veg
             , cfi_emv            = emv
             , cfi_emg            = emg
             , cfi_t_veg          = t_veg_patch temp
             , cfi_t_grnd         = t_grnd
             , cfi_thm            = thm
             , cfi_thv            = thv
             , cfi_t_soisno_top   = t_top
             , cfi_t_soisno_topsoil = t_soil1
             , cfi_t_h2osfc       = t_h2osfc
             , cfi_t_stem         = t_veg_patch temp
             , cfi_sabv           = sabv
             , cfi_qg             = qg
             , cfi_qg_snow        = qg_snow
             , cfi_qg_soil        = qg_soil
             , cfi_qg_h2osfc      = qg_h2osfc
             , cfi_dqgdT          = dqgdT
             , cfi_frac_sno_eff   = frac_sno_eff
             , cfi_frac_h2osfc    = frac_h2osfc
             , cfi_snow_depth     = snow_depth
             , cfi_fwet           = fwet
             , cfi_fdry           = fdry
             , cfi_liqcan         = 0.0
             , cfi_snocan         = 0.0
             , cfi_rssun          = 200.0
             , cfi_rssha          = 200.0
             , cfi_laisun         = laisun'
             , cfi_laisha         = laisha'
             , cfi_btran          = 1.0
             , cfi_soilbeta       = soilbeta'
             , cfi_soilresis      = 0.0
             , cfi_htvp           = htvp
             , cfi_cgrnds         = cgrnds0
             , cfi_cgrndl         = cgrndl0
             , cfi_do_soilevap_beta = True
             , cfi_dtime          = dtime
             , cfi_zetamaxstable  = 0.5
             , cfi_dleaf          = safeIdx (cstate_dleaf_patch cs) 0
             , cfi_snl            = snl
             }

           cfOut = canopyFluxes defaultCanopyFluxesParams
                     defaultCanopyFluxesControl inp

           sh_tot = cfo_eflx_sh_veg cfOut + cfo_eflx_sh_grnd cfOut

           ef = clmEnergyFlux st
           ef' = ef
             { eflx_sh_tot_patch  = sh_tot
             , eflx_sh_grnd_patch = cfo_eflx_sh_grnd cfOut
             }

           temp' = (clmTemp st)
             { t_ref2m_patch = cfo_t_ref2m cfOut
             , t_veg_patch   = cfo_t_veg cfOut
             }

           wf = clmWaterFlux st
           wf' = wf
             { qflx_evap_tot_patch = cfo_qflx_evap_soi cfOut
                                   + cfo_qflx_tran_veg cfOut
             , qflx_tran_veg_patch = cfo_qflx_tran_veg cfOut
             }

       in st { clmEnergyFlux = ef'
             , clmTemp = temp'
             , clmWaterFlux = wf'
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
      dtime = tcDtime ctx

      t_grnd = t_grnd_col temp
      t_h2osfc = t_h2osfc_col temp
      forc_lwrad = if VU.null (tcForcLwrad ctx) then 300.0
                   else tcForcLwrad ctx VU.! 0
      htvp = if t_grnd < tfrz then hsub else hvap
      emg = 0.96

      frac_sno_eff = safeIdx (wdiag_frac_sno_eff_col wdiag) 0
      frac_h2osfc = safeIdx (wdiag_frac_h2osfc_col wdiag) 0
      snow_depth = safeIdx (wdiag_snow_depth_col wdiag) 0

      nbedrock = if VU.null (grc_nbedrock grc) then nlevsoi
                 else safeIdxI (grc_nbedrock grc) 0

      sabg_val = sabg_patch ef
      nlyr_sabg = nlevsno + 1
      sabg_lyr = VU.generate nlyr_sabg (\j -> if j == 0 then sabg_val else 0.0)

      hs_top = eflx_soil_grnd_col ef
      dhsdT = 0.0

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
        , sti_hs_soil          = hs_top
        , sti_hs_h2osfc        = hs_top
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
-- Snow Water adapter (new snow accumulation + sublimation)
-- ============================================================================

snowWaterStep :: PhysicsStep
snowWaterStep _cfg ctx st =
  let dtime = tcDtime ctx
      forc_snow = if VU.null (tcForcSnow ctx) then 0.0
                  else tcForcSnow ctx VU.! 0
      forc_t = if VU.null (tcForcT ctx) then 273.15
               else tcForcT ctx VU.! 0

      snl = clmSnl st
      ws = clmWaterState st
      col = clmColumn st
      wdiag = clmWaterDiagBulk st

      new_snow_mass = forc_snow * dtime
      bifall = 50.0 + 1.7 * (max 0.0 (forc_t - tfrz + 15.0)) ** 1.5
      new_snow_depth = if new_snow_mass > 0.0 then new_snow_mass / bifall else 0.0

      h2osno_nl = h2osno_col ws
      frac_sno_cur = safeIdx (wdiag_frac_sno_col wdiag) 0
      snow_depth_cur = safeIdx (wdiag_snow_depth_col wdiag) 0

      h2osoi_liq_cur = h2osoi_liq_col ws
      h2osoi_ice_cur = h2osoi_ice_col ws
      dz_cur = colDz col

      accum_factor = 0.1 :: Double

      h2osno_total_prev =
        (sum [ safeIdx h2osoi_ice_cur j + safeIdx h2osoi_liq_cur j
             | j <- [nlevsno + snl .. nlevsno - 1] ])
        + h2osno_nl

      frac_sno_new =
        if h2osno_total_prev == 0.0
        then if new_snow_mass > 0.0
             then tanh (accum_factor * new_snow_mass)
             else 0.0
        else if new_snow_mass > 0.0
             then frac_sno_cur + tanh (accum_factor * new_snow_mass) * (1.0 - frac_sno_cur)
             else frac_sno_cur

      snow_depth_new =
        if h2osno_total_prev == 0.0
        then if new_snow_mass > 0.0 && frac_sno_new > 0.0
             then (new_snow_mass / bifall) / frac_sno_new
             else 0.0
        else if new_snow_mass > 0.0 && frac_sno_new > 0.0
             then snow_depth_cur + new_snow_mass / (bifall * frac_sno_new)
             else snow_depth_cur

      (h2osno_nl', ice_new, dz_new) =
        if snl < 0 && new_snow_mass > 0.0
        then let topIdx = nlevsno + snl
                 ice_upd = h2osoi_ice_cur VU.// [(topIdx, safeIdx h2osoi_ice_cur topIdx + new_snow_mass)]
                 dz_add = new_snow_depth
                 dz_upd = dz_cur VU.// [(topIdx, safeIdx dz_cur topIdx + dz_add)]
             in (0.0, ice_upd, dz_upd)
        else (h2osno_nl + new_snow_mass, h2osoi_ice_cur, dz_cur)

      snow_dzmin_1 = 0.010 :: Double
      shouldCreateLayer = snl == 0 && h2osno_nl' > 0.0
                        && frac_sno_new > 0.0
                        && snow_depth_new * frac_sno_new >= snow_dzmin_1

      (snl_final, h2osno_nl_final, t_soisno_new, liq_new, ice_final, dz_final) =
        if shouldCreateLayer
        then let layerIdx = nlevsno - 1
                 snow_t = min tfrz forc_t
                 layer_dz = h2osno_nl' / bifall
                 t_new = t_soisno_col (clmTemp st) VU.// [(layerIdx, snow_t)]
                 liq_n = h2osoi_liq_cur VU.// [(layerIdx, 0.0)]
                 ice_n = ice_new VU.// [(layerIdx, h2osno_nl')]
                 dz_n = dz_new VU.// [(layerIdx, layer_dz)]
             in (-1, 0.0, t_new, liq_n, ice_n, dz_n)
        else (snl, h2osno_nl', t_soisno_col (clmTemp st), h2osoi_liq_cur, ice_new, dz_new)

      ws' = ws
        { h2osno_col     = h2osno_nl_final
        , h2osoi_liq_col = liq_new
        , h2osoi_ice_col = ice_final
        }

      col' = col { colDz = dz_final }

      temp' = (clmTemp st) { t_soisno_col = t_soisno_new }

      wdiag' = wdiag
        { wdiag_frac_sno_col     = VU.singleton frac_sno_new
        , wdiag_frac_sno_eff_col = VU.singleton frac_sno_new
        , wdiag_snow_depth_col   = VU.singleton snow_depth_new
        }

  in st { clmSnl = snl_final
        , clmWaterState = ws'
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
