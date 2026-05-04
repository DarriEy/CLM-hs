{-# LANGUAGE BangPatterns #-}
-- | Pipeline-driven CLM simulation runner.
-- Replaces the monolithic Simulation.hs with one that uses
-- clmDrv + wiredPhysicsPipeline for all physics.
module CLM.Driver.PipelineRunner
  ( -- * Initialization
    initCLMStateFromDir
    -- * Timestep context
  , buildTimestepContext
    -- * Run loop
  , runPipeline
  , PipelineConfig(..)
  , defaultPipelineConfig
    -- * Daily diagnostics
  , DailyDiag(..)
  , zeroDailyDiag
    -- * CSV output
  , writeDailyCSV
  ) where

import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import Control.Monad (when)

import CLM.Constants.PhysicalConstants
  ( nlevsno, nlevgrnd, nlevsoi, tfrz )
import CLM.Constants.ControlFlags
  ( CLMDriverConfig(..), defaultDriverConfig )
import CLM.Driver.CLMDriver
  ( CLMState(..), CLMDriverState(..), TimestepContext(..)
  , PhysicsPipeline(..)
  , defaultCLMState, defaultDriverState, defaultTimestepContext
  , clmDrv )
import CLM.Driver.PhysicsAdapters (wiredPhysicsPipeline)

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

import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readInt64Vector
  , readManifestDims, ManifestDims(..) )
import CLM.Infrastructure.ReadParams
  ( readParametersBinary, AllParams(..) )
import CLM.Infrastructure.ForcingReader
  ( ForcingReaderState(..), forcingReaderInitBinary, readForcingStepPure
  , ForcingTimestep(..)
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity )
import CLM.Infrastructure.Orbital
  ( computeOrbital, defaultOrbitalParams )

-- ============================================================================
-- Configuration
-- ============================================================================

data PipelineConfig = PipelineConfig
  { pcDtime       :: !Double
  , pcNdays       :: !Int
  , pcDataDir     :: !FilePath
  , pcVerbose     :: !Bool
  , pcOutputCSV   :: !FilePath
  } deriving (Show)

defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pcDtime   = 1800.0
  , pcNdays   = 30
  , pcDataDir = "test/data"
  , pcVerbose = True
  , pcOutputCSV = ""
  }

-- ============================================================================
-- CLMState initialization from binary test data
-- ============================================================================

initCLMStateFromDir :: FilePath -> IO (CLMState, ForcingReaderState)
initCLMStateFromDir dir = do
  dims <- readManifestDims (dir </> "manifest.json")
  let nc = mdNc dims
      nlevtot = nlevsno + nlevgrnd

  let extractCol1 :: VU.Vector Double -> Int -> VU.Vector Double
      extractCol1 vec nlev = VU.generate nlev (\j -> vec VU.! (j * nc + 0))
      sanitize v = VU.map (\x -> if isNaN x then 0.0 else x) v

  t_soisno_raw <- readFloat64Vector (dir </> "coldstart" </> "t_soisno.bin")
  h2osoi_liq_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osoi_liq.bin")
  h2osoi_ice_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osoi_ice.bin")
  dz_raw <- readFloat64Vector (dir </> "coldstart" </> "col_dz.bin")
  z_raw <- readFloat64Vector (dir </> "coldstart" </> "col_z.bin")
  zi_raw <- readFloat64Vector (dir </> "coldstart" </> "col_zi.bin")
  watsat_raw <- readFloat64Vector (dir </> "coldstart" </> "watsat.bin")
  bsw_raw <- readFloat64Vector (dir </> "coldstart" </> "bsw.bin")
  sucsat_raw <- readFloat64Vector (dir </> "coldstart" </> "sucsat.bin")
  tkmg_raw <- readFloat64Vector (dir </> "coldstart" </> "tkmg.bin")
  tkdry_raw <- readFloat64Vector (dir </> "coldstart" </> "tkdry.bin")
  csol_raw <- readFloat64Vector (dir </> "coldstart" </> "csol.bin")
  tksatu_raw <- readFloat64Vector (dir </> "coldstart" </> "tksatu.bin")
  t_grnd_raw <- readFloat64Vector (dir </> "coldstart" </> "t_grnd.bin")
  t_h2osfc_raw <- readFloat64Vector (dir </> "coldstart" </> "t_h2osfc.bin")
  h2osfc_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osfc.bin")
  h2osno_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osno.bin")

  elai_raw <- readFloat64Vector (dir </> "coldstart" </> "elai.bin")
  esai_raw <- readFloat64Vector (dir </> "coldstart" </> "esai.bin")
  htop_raw <- readFloat64Vector (dir </> "coldstart" </> "htop.bin")

  forcing <- forcingReaderInitBinary (dir </> "forcing")

  let t_soisno = extractCol1 t_soisno_raw nlevtot
      h2osoi_liq_raw_col = extractCol1 h2osoi_liq_raw nlevtot
      h2osoi_ice_raw_col = extractCol1 h2osoi_ice_raw nlevtot
      h2osoi_liq = VU.generate nlevtot $ \j ->
        if j >= nlevsno && t_soisno VU.! j <= tfrz
        then 0.0
        else h2osoi_liq_raw_col VU.! j
      h2osoi_ice = VU.generate nlevtot $ \j ->
        if j >= nlevsno && t_soisno VU.! j <= tfrz
        then h2osoi_liq_raw_col VU.! j + h2osoi_ice_raw_col VU.! j
        else h2osoi_ice_raw_col VU.! j
      dz = extractCol1 dz_raw nlevtot
      z = extractCol1 z_raw nlevtot
      zi = extractCol1 zi_raw (nlevtot + 1)
      watsat_v = sanitize $ extractCol1 watsat_raw nlevgrnd
      bsw_v = sanitize $ extractCol1 bsw_raw nlevgrnd
      sucsat_v = sanitize $ extractCol1 sucsat_raw nlevgrnd
      tkmg_v = sanitize $ extractCol1 tkmg_raw nlevgrnd
      tkdry_v = sanitize $ extractCol1 tkdry_raw nlevgrnd
      csol_v = sanitize $ extractCol1 csol_raw nlevgrnd
      tksatu_v = sanitize $ extractCol1 tksatu_raw nlevgrnd

      t_grnd = t_grnd_raw VU.! 0
      np = mdNp dims

      frac_veg = VU.generate np $ \p ->
        if elai_raw VU.! p + esai_raw VU.! p > 0.05 then 1 else 0

  let st = defaultCLMState
        { clmColumn = ColumnData
            { colZ = z, colDz = dz, colZi = zi
            , watsat = watsat_v, bsw = bsw_v
            , hksat = VU.replicate nlevgrnd 0.01
            , sucsat = sucsat_v
            , zii = 1000.0, lakedepth = 0.0
            }
        , clmTemp = TemperatureData
            { t_soisno_col = t_soisno
            , t_grnd_col = t_grnd
            , t_h2osfc_col = t_h2osfc_raw VU.! 0
            , t_ref2m_patch = t_grnd
            , t_veg_patch = t_grnd
            }
        , clmWaterState = WaterStateData
            { h2osoi_liq_col = h2osoi_liq
            , h2osoi_ice_col = h2osoi_ice
            , h2osoi_vol_col = VU.replicate nlevgrnd 0.3
            , h2osno_col = h2osno_raw VU.! 0
            , h2osfc_col = h2osfc_raw VU.! 0
            , h2ocan_patch = 0.0
            }
        , clmSoilState = (clmSoilState defaultCLMState)
            { sstate_tkmg_col = tkmg_v
            , sstate_tkdry_col = tkdry_v
            , sstate_csol_col = csol_v
            , sstate_tksatu_col = tksatu_v
            , sstate_watsat_col = watsat_v
            , sstate_bsw_col = bsw_v
            , sstate_sucsat_col = sucsat_v
            , sstate_soilbeta_col = VU.singleton 1.0
            }
        , clmCanopyState = (clmCanopyState defaultCLMState)
            { cstate_elai_patch = elai_raw
            , cstate_esai_patch = esai_raw
            , cstate_htop_patch = htop_raw
            , cstate_frac_veg_nosno_alb_patch = frac_veg
            , cstate_fsun_patch = VU.replicate np 0.5
            , cstate_laisun_patch = VU.map (* 0.5) elai_raw
            , cstate_laisha_patch = VU.map (* 0.5) elai_raw
            , cstate_dleaf_patch = VU.replicate np 0.04
            }
        , clmGridcell = (clmGridcell defaultCLMState)
            { grc_lat = VU.singleton (mdLat dims)
            , grc_lon = VU.singleton (mdLon dims)
            , grc_nbedrock = VU.singleton nlevsoi
            , grc_dayl = VU.singleton 43200.0
            , grc_max_dayl = VU.singleton 86400.0
            }
        , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
            { wdiag_frac_sno_col = VU.singleton 0.0
            , wdiag_frac_sno_eff_col = VU.singleton 0.0
            , wdiag_frac_h2osfc_col = VU.singleton 0.0
            , wdiag_snow_depth_col = VU.singleton 0.0
            , wdiag_qg_col = VU.singleton 0.005
            , wdiag_qg_snow_col = VU.singleton 0.005
            , wdiag_qg_soil_col = VU.singleton 0.005
            , wdiag_qg_h2osfc_col = VU.singleton 0.005
            , wdiag_dqgdT_col = VU.singleton 0.0
            }
        , clmSnl = 0
        }

  return (st, forcing)

-- ============================================================================
-- Timestep context builder
-- ============================================================================

buildTimestepContext
  :: ForcingReaderState
  -> Int       -- ^ step number (1-based)
  -> Double    -- ^ dtime [s]
  -> TimestepContext
buildTimestepContext fr nstep dtime =
  let (fts, _fr') = readForcingStepPure fr (nstep - 1)

      forc_t    = ft_tbot fts
      forc_pbot = ft_psrf fts
      forc_wind = ft_wind fts
      forc_lwrad = ft_flds fts
      forc_fsds = ft_fsds fts
      forc_precip = ft_precip fts
      forc_qbot = ft_qbot fts

      forc_vp  = computeVaporPressureFromQ forc_qbot forc_pbot
      forc_th  = computePotentialTemperature forc_t forc_pbot
      forc_rho = computeAirDensity forc_pbot forc_t forc_vp
      (forc_rain, forc_snow) = partitionPrecip forc_t forc_precip

      forc_solad_vis = forc_fsds * 0.35
      forc_solad_nir = forc_fsds * 0.35
      forc_solai_vis = forc_fsds * 0.15
      forc_solai_nir = forc_fsds * 0.15

      calday = fromIntegral (nstep - 1) * dtime / 86400.0 + 1.0
      (declin, _eccf) = computeOrbital defaultOrbitalParams calday

      stepsPerDay = round (86400.0 / dtime) :: Int

  in defaultTimestepContext
    { tcDoAlb         = True
    , tcDtime         = dtime
    , tcDeclin        = declin
    , tcDeclinP1      = declin
    , tcObliqr        = 0.4091
    , tcIsFirstStep   = nstep == 1
    , tcIsBegCurrDay  = (nstep - 1) `mod` stepsPerDay == 0
    , tcIsEndCurrDay  = nstep `mod` stepsPerDay == 0
    , tcForcT         = VU.singleton forc_t
    , tcForcTh        = VU.singleton forc_th
    , tcForcQ         = VU.singleton forc_qbot
    , tcForcPbot      = VU.singleton forc_pbot
    , tcForcRho       = VU.singleton forc_rho
    , tcForcRain      = VU.singleton forc_rain
    , tcForcSnow      = VU.singleton forc_snow
    , tcForcLwrad     = VU.singleton forc_lwrad
    , tcForcSolad     = VU.fromList [forc_solad_vis, forc_solad_nir]
    , tcForcSolai     = VU.fromList [forc_solai_vis, forc_solai_nir]
    , tcForcWind      = VU.singleton forc_wind
    , tcForcHgt       = 30.0
    }

-- ============================================================================
-- Daily diagnostics
-- ============================================================================

data DailyDiag = DailyDiag
  { dd_t_grnd     :: !Double
  , dd_h2osno     :: !Double
  , dd_snow_depth :: !Double
  , dd_frac_sno   :: !Double
  , dd_eflx_sh    :: !Double
  , dd_eflx_lh    :: !Double
  , dd_count      :: !Int
  } deriving (Show)

zeroDailyDiag :: DailyDiag
zeroDailyDiag = DailyDiag 0 0 0 0 0 0 0

accumDiag :: DailyDiag -> CLMState -> DailyDiag
accumDiag dd st = DailyDiag
  { dd_t_grnd     = dd_t_grnd dd + t_grnd_col (clmTemp st)
  , dd_h2osno     = dd_h2osno dd + h2osno_col (clmWaterState st)
  , dd_snow_depth = dd_snow_depth dd
                  + (if VU.null v then 0.0 else v VU.! 0)
  , dd_frac_sno   = dd_frac_sno dd
                  + (if VU.null fs then 0.0 else fs VU.! 0)
  , dd_eflx_sh    = dd_eflx_sh dd + eflx_sh_tot_patch (clmEnergyFlux st)
  , dd_eflx_lh    = dd_eflx_lh dd + eflx_lh_tot_patch (clmEnergyFlux st)
  , dd_count      = dd_count dd + 1
  }
  where
    v = wdiag_snow_depth_col (clmWaterDiagBulk st)
    fs = wdiag_frac_sno_col (clmWaterDiagBulk st)

avgDiag :: DailyDiag -> DailyDiag
avgDiag dd =
  let n = fromIntegral (dd_count dd)
  in if n <= 0 then dd
     else DailyDiag
       { dd_t_grnd     = dd_t_grnd dd / n
       , dd_h2osno     = dd_h2osno dd / n
       , dd_snow_depth = dd_snow_depth dd / n
       , dd_frac_sno   = dd_frac_sno dd / n
       , dd_eflx_sh    = dd_eflx_sh dd / n
       , dd_eflx_lh    = dd_eflx_lh dd / n
       , dd_count      = dd_count dd
       }

-- ============================================================================
-- Main run loop
-- ============================================================================

runPipeline :: PipelineConfig -> IO [DailyDiag]
runPipeline cfg = do
  let dir = pcDataDir cfg
      dtime = pcDtime cfg
      ndays = pcNdays cfg
      stepsPerDay = round (86400.0 / dtime) :: Int
      totalSteps = ndays * stepsPerDay

  (st0, forcing) <- initCLMStateFromDir dir

  when (pcVerbose cfg) $
    putStrLn $ "Pipeline runner: " ++ show ndays ++ " days, "
            ++ show stepsPerDay ++ " steps/day, dtime=" ++ show dtime ++ "s"

  let drvCfg = defaultDriverConfig

  go st0 defaultDriverState forcing 1 zeroDailyDiag [] totalSteps stepsPerDay drvCfg dtime
  where
    go !st !drvSt !fr !step !dayAcc !results !total !spd !drvCfg !dtime
      | step > total = return (reverse results)
      | otherwise = do
          let ctx = buildTimestepContext fr step dtime
              (!drvSt', !st') = clmDrv drvCfg wiredPhysicsPipeline ctx drvSt st
              dayAcc' = accumDiag dayAcc st'
              isEndDay = step `mod` spd == 0

          return ()

          when (t_grnd_col (clmTemp st') > 400.0 ||
                t_grnd_col (clmTemp st') < 100.0) $
            putStrLn $ "  *** WARNING step " ++ show step
                    ++ ": T_GRND=" ++ show (t_grnd_col (clmTemp st'))

          if isEndDay
            then do
              let avg = avgDiag dayAcc'
                  dayNum = step `div` spd
              when (pcVerbose (defaultPipelineConfig)) $
                putStrLn $ "  Day " ++ show dayNum
                        ++ ": T_GRND=" ++ show (dd_t_grnd avg) ++ " K"
                        ++ ", H2OSNO=" ++ show (dd_h2osno avg) ++ " kg/m2"
                        ++ ", SNOW_DEPTH=" ++ show (dd_snow_depth avg) ++ " m"
              go st' drvSt' fr (step + 1) zeroDailyDiag (avg : results) total spd drvCfg dtime
            else
              go st' drvSt' fr (step + 1) dayAcc' results total spd drvCfg dtime

-- ============================================================================
-- CSV output (matching Julia daily_avg format)
-- ============================================================================

writeDailyCSV :: FilePath -> [DailyDiag] -> IO ()
writeDailyCSV path dailies = do
  let header = "day,T_GRND,EFLX_LH_TOT,EFLX_SH_TOT,H2OSNO,SNOW_DEPTH,FRAC_SNO"
      rows = zipWith mkRow [1::Int ..] dailies
      mkRow d dd = show d
                ++ "," ++ showE (dd_t_grnd dd)
                ++ "," ++ showE (dd_eflx_lh dd)
                ++ "," ++ showE (dd_eflx_sh dd)
                ++ "," ++ showE (dd_h2osno dd)
                ++ "," ++ showE (dd_snow_depth dd)
                ++ "," ++ showE (dd_frac_sno dd)
      showE x = show x
  writeFile path (unlines (header : rows))
  putStrLn $ "Wrote " ++ show (length dailies) ++ " daily averages to " ++ path
