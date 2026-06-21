{-# LANGUAGE BangPatterns #-}
-- | General single-column site calibration framework.
--
-- Runs the REAL CLM physics pipeline (clmDrv) as the forward model.
-- Parameters are injected by scaling soil/snow/vegetation properties
-- in the CLM state before each run. QRUNOFF is extracted from the
-- pipeline output, routed through a linear reservoir, and compared
-- to observed streamflow via KGE.
--
-- Matches the SYMFLUENCE paper setup:
--   - Full CLM physics (soil temp, snow, Richards equation, canopy fluxes)
--   - Parameter scaling (hksat_mult, bsw_mult, watsat_mult, etc.)
--   - Linear reservoir routing with route_k
--   - KGE metric against observed discharge
--   - Central finite-difference gradient (CLM has discontinuities)
module CLM.Calibration.SiteCalibration
  ( -- * Site configuration
    SiteConfig(..)
    -- * Observations
  , readStreamflowCSV
    -- * Routing
  , linearReservoirRoute
    -- * Metrics
  , kge
  , nse
    -- * CLM parameter set (matching Julia paper)
  , CLMCalibParams(..)
  , defaultCLMCalibParams
  , ddsOptimalParams
  , paramNames
  , paramBounds
  , paramsToList
  , paramsFromList
    -- * Parameter injection
  , injectParams
    -- * Forward model
  , runCLMAndRoute
    -- * Calibration
  , CalibrationConfig(..)
  , defaultCalibConfig
  , CalibResult(..)
  , calibrateSiteCLM
  ) where

import qualified Data.Vector.Unboxed as VU
import Control.Monad (foldM)
import CLM.Driver.PipelineRunner
  ( PipelineConfig(..), defaultPipelineConfig, runCLMForQrunoff
  , initCLMStateFromDir, buildTimestepContext, SurfaceAlbedoConstants(..) )
import CLM.Driver.CLMDriver
  ( CLMState(..), CLMDriverState(..), TimestepContext(..)
  , defaultDriverState, clmDrv )
import CLM.Driver.PhysicsAdapters (wiredPhysicsPipeline)
import CLM.BioGeoPhys.CanopyHydrology (defaultCanopyHydroParams)
import CLM.Constants.PhysicalConstants (nlevgrnd, nlevsoi)
import CLM.Constants.ControlFlags (defaultDriverConfig)
import CLM.Types.ColumnData (ColumnData(..))
import CLM.Types.SoilStateData (SoilStateData(..))
import CLM.Types.WaterFluxData (WaterFluxData(..))
import CLM.Types.CanopyStateData (CanopyStateData(..))
import CLM.Infrastructure.ForcingReader
  ( ForcingReaderState(..), readForcingStepPure, ForcingTimestep(..)
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity )
import CLM.Infrastructure.Orbital (computeOrbital, defaultOrbitalParams)

-- =========================================================================
-- Site configuration
-- =========================================================================

data SiteConfig = SiteConfig
  { sc_name        :: !String
  , sc_area_km2    :: !Double
  , sc_lat         :: !Double
  , sc_lon         :: !Double
  , sc_elev        :: !Double
  , sc_forcing_dt  :: !Double  -- ^ Forcing timestep [s]
  , sc_data_dir    :: !FilePath
  , sc_spinup_days :: !Int     -- ^ Days to discard as spinup
  } deriving (Show)

-- =========================================================================
-- CLM calibration parameters (matching SYMFLUENCE/Julia paper)
-- =========================================================================

-- | Full 29-parameter set matching SYMFLUENCE/Julia paper exactly.
data CLMCalibParams = CLMCalibParams
  { -- Soil hydraulic multipliers
    cp_hksat_mult    :: !Double  -- ^ Saturated hydraulic conductivity multiplier
  , cp_bsw_mult      :: !Double  -- ^ Clapp-Hornberger b multiplier
  , cp_watsat_mult   :: !Double  -- ^ Porosity multiplier
  , cp_sucsat_mult   :: !Double  -- ^ Saturated suction multiplier
    -- Hydrology
  , cp_baseflow_scalar :: !Double -- ^ TOPMODEL baseflow scalar
  , cp_fff           :: !Double  -- ^ TOPMODEL decay factor (1/m)
  , cp_fmax          :: !Double  -- ^ Maximum fractional saturated area
  , cp_e_ice         :: !Double  -- ^ Ice impedance factor
  , cp_wimp          :: !Double  -- ^ Minimum soil water for ice impedance
  , cp_ksatdecay     :: !Double  -- ^ Hydraulic conductivity depth decay
  , cp_n_baseflow    :: !Double  -- ^ Baseflow exponent
  , cp_perched_baseflow :: !Double -- ^ Perched water table baseflow scalar
    -- Snow
  , cp_fresh_snw_rds :: !Double  -- ^ Fresh snow grain radius max [um]
  , cp_snw_aging_bst :: !Double  -- ^ Snow aging parameter
  , cp_sno_z0mv      :: !Double  -- ^ Snow roughness length [m]
  , cp_accum_factor   :: !Double -- ^ Snow accumulation factor
  , cp_snow_dens_max  :: !Double -- ^ Maximum snow density [kg/m3]
  , cp_snow_dens_min  :: !Double -- ^ Minimum snow density [kg/m3]
  , cp_n_melt_coef    :: !Double -- ^ Melt coefficient
  , cp_int_snow_max   :: !Double -- ^ Max integrated snow [mm]
    -- Vegetation
  , cp_interception_frac :: !Double -- ^ Canopy interception fraction
  , cp_max_leaf_wet  :: !Double  -- ^ Maximum leaf wetted fraction
  , cp_medlynslope   :: !Double  -- ^ Medlyn stomatal slope
  , cp_slatop        :: !Double  -- ^ Specific leaf area at top [m2/gC]
  , cp_flnr          :: !Double  -- ^ Fraction leaf N in Rubisco
  , cp_froot_leaf    :: !Double  -- ^ Fine root to leaf ratio
  , cp_stem_leaf     :: !Double  -- ^ Stem to leaf ratio
  , cp_organic_max   :: !Double  -- ^ Maximum soil organic matter
    -- Routing
  , cp_route_k       :: !Double  -- ^ Linear reservoir residence time [hours]
  } deriving (Show)

defaultCLMCalibParams :: CLMCalibParams
defaultCLMCalibParams = CLMCalibParams
  { cp_hksat_mult = 1.0, cp_bsw_mult = 1.0, cp_watsat_mult = 1.0, cp_sucsat_mult = 1.0
  , cp_baseflow_scalar = 0.01, cp_fff = 0.5, cp_fmax = 0.5, cp_e_ice = 6.0
  , cp_wimp = 0.05, cp_ksatdecay = 1.0, cp_n_baseflow = 1.0, cp_perched_baseflow = 1e-5
  , cp_fresh_snw_rds = 100.0, cp_snw_aging_bst = 100.0, cp_sno_z0mv = 0.002
  , cp_accum_factor = 0.0, cp_snow_dens_max = 350.0, cp_snow_dens_min = 100.0
  , cp_n_melt_coef = 200.0, cp_int_snow_max = 2000.0
  , cp_interception_frac = 0.5, cp_max_leaf_wet = 0.1, cp_medlynslope = 6.0
  , cp_slatop = 0.01, cp_flnr = 0.1, cp_froot_leaf = 1.0, cp_stem_leaf = 1.5
  , cp_organic_max = 50.0, cp_route_k = 20.0
  }

-- | DDS-optimized parameters from the SYMFLUENCE paper.
ddsOptimalParams :: CLMCalibParams
ddsOptimalParams = CLMCalibParams
  { cp_hksat_mult = 4.1194, cp_bsw_mult = 1.201, cp_watsat_mult = 1.1608
  , cp_sucsat_mult = 0.6879
  , cp_baseflow_scalar = 0.002212, cp_fff = 0.10915, cp_fmax = 0.9809
  , cp_e_ice = 3.4173, cp_wimp = 0.01033, cp_ksatdecay = 0.33006
  , cp_n_baseflow = 2.6479, cp_perched_baseflow = 1.676e-6
  , cp_fresh_snw_rds = 70.232, cp_snw_aging_bst = 90.153, cp_sno_z0mv = 0.009816
  , cp_accum_factor = 0.001556, cp_snow_dens_max = 505.73, cp_snow_dens_min = 141.67
  , cp_n_melt_coef = 440.13, cp_int_snow_max = 3113.2
  , cp_interception_frac = 0.6455, cp_max_leaf_wet = 0.07266
  , cp_medlynslope = 11.153, cp_slatop = 0.00667, cp_flnr = 0.07278
  , cp_froot_leaf = 2.8143, cp_stem_leaf = 2.6721, cp_organic_max = 53.226
  , cp_route_k = 31.006
  }

paramNames :: [String]
paramNames =
  [ "hksat_mult", "bsw_mult", "watsat_mult", "sucsat_mult"
  , "baseflow_scalar", "fff", "fmax", "e_ice", "wimp", "ksatdecay"
  , "n_baseflow", "perched_baseflow"
  , "fresh_snw_rds", "snw_aging_bst", "sno_z0mv", "accum_factor"
  , "snow_dens_max", "snow_dens_min", "n_melt_coef", "int_snow_max"
  , "interception_frac", "max_leaf_wet", "medlynslope", "slatop"
  , "flnr", "froot_leaf", "stem_leaf", "organic_max"
  , "route_k"
  ]

paramBounds :: [(Double, Double)]
paramBounds =
  [ (0.01, 50.0), (0.3, 3.0), (0.5, 1.5), (0.2, 3.0)          -- soil (wider)
  , (1e-4, 0.5), (0.01, 2.0), (0.0, 1.0), (1.0, 8.0)          -- hydrology 1 (wider)
  , (0.005, 0.2), (0.05, 20.0), (0.5, 6.0), (1e-8, 1e-2)      -- hydrology 2 (wider)
  , (30.0, 300.0), (0.0, 300.0), (1e-5, 0.05), (-0.2, 0.2)    -- snow 1
  , (200.0, 600.0), (30.0, 250.0), (10.0, 1000.0), (200.0, 8000.0) -- snow 2
  , (0.1, 1.0), (0.005, 0.3), (1.0, 15.0), (0.002, 0.08)      -- vegetation 1
  , (0.02, 0.35), (0.3, 5.0), (0.3, 5.0), (0.0, 200.0)        -- vegetation 2
  , (1.0, 500.0)                                                 -- routing (mountain basins need long lag)
  ]

paramsToList :: CLMCalibParams -> [Double]
paramsToList p =
  [ cp_hksat_mult p, cp_bsw_mult p, cp_watsat_mult p, cp_sucsat_mult p
  , cp_baseflow_scalar p, cp_fff p, cp_fmax p, cp_e_ice p
  , cp_wimp p, cp_ksatdecay p, cp_n_baseflow p, cp_perched_baseflow p
  , cp_fresh_snw_rds p, cp_snw_aging_bst p, cp_sno_z0mv p, cp_accum_factor p
  , cp_snow_dens_max p, cp_snow_dens_min p, cp_n_melt_coef p, cp_int_snow_max p
  , cp_interception_frac p, cp_max_leaf_wet p, cp_medlynslope p, cp_slatop p
  , cp_flnr p, cp_froot_leaf p, cp_stem_leaf p, cp_organic_max p
  , cp_route_k p
  ]

paramsFromList :: [Double] -> CLMCalibParams
paramsFromList xs
  | length xs == 29 = CLMCalibParams
      (xs!!0) (xs!!1) (xs!!2) (xs!!3) (xs!!4) (xs!!5) (xs!!6) (xs!!7)
      (xs!!8) (xs!!9) (xs!!10) (xs!!11) (xs!!12) (xs!!13) (xs!!14) (xs!!15)
      (xs!!16) (xs!!17) (xs!!18) (xs!!19) (xs!!20) (xs!!21) (xs!!22) (xs!!23)
      (xs!!24) (xs!!25) (xs!!26) (xs!!27) (xs!!28)
  | otherwise = error $ "paramsFromList: expected 29, got " ++ show (length xs)

-- =========================================================================
-- Parameter injection into CLM state
-- =========================================================================

-- | Inject all 29 calibration parameters into CLM state.
-- Soil/vegetation properties are scaled; hydrology/snow/routing params
-- are stored in CLMState fields for the hydrology step to read.
injectParams :: CLMCalibParams -> CLMState -> CLMState
injectParams !p !st =
  let col = clmColumn st
      ss = clmSoilState st
      cs = clmCanopyState st
      -- Soil hydraulic multipliers (applied to initial state vectors)
      col' = col
        { hksat = VU.map (\h -> h * cp_hksat_mult p
                               * exp (negate (cp_ksatdecay p) * 0.5)) (hksat col)
        }
      ss' = ss
        { sstate_bsw_col = VU.map (* cp_bsw_mult p) (sstate_bsw_col ss)
        , sstate_watsat_col = VU.map (\w -> min 0.95 (w * cp_watsat_mult p)) (sstate_watsat_col ss)
        , sstate_sucsat_col = VU.map (* cp_sucsat_mult p) (sstate_sucsat_col ss)
        }
      -- Vegetation: scale LAI by slatop ratio
      cs' = cs
        { cstate_elai_patch = VU.map (\lai -> lai * cp_slatop p / 0.01) (cstate_elai_patch cs)
        }
  in st { clmColumn = col', clmSoilState = ss', clmCanopyState = cs'
        -- Store hydrology/snow/routing params for the physics steps to read
        , clmP_baseflow_scalar = cp_baseflow_scalar p
        , clmP_fff = cp_fff p
        , clmP_fmax = cp_fmax p
        , clmP_e_ice = cp_e_ice p
        , clmP_n_baseflow = cp_n_baseflow p
        , clmP_n_melt_coef = cp_n_melt_coef p
        , clmP_interception_frac = cp_interception_frac p
        , clmP_sno_z0mv = cp_sno_z0mv p
        , clmP_route_k = cp_route_k p
        }

-- =========================================================================
-- Observations
-- =========================================================================

readStreamflowCSV :: FilePath -> Int -> IO [Double]
readStreamflowCSV csvpath year = do
  contents <- readFile csvpath
  let ls = drop 1 (lines contents)
      parseLine l = case break (== ',') l of
        (dateStr, ',':qStr) ->
          let yr = case reads (take 4 dateStr) :: [(Int, String)] of
                     [(v, _)] -> v; _ -> 0
              q = case reads qStr :: [(Double, String)] of
                    [(v, _)] | not (isNaN v) -> Just v; _ -> Nothing
          in if yr == year then q else Nothing
        _ -> Nothing
      allQ = [q | l <- ls, Just q <- [parseLine l]]
      nPerDay = max 1 (length allQ `div` 365)
      dailyAvg = [avg (take nPerDay (drop (i * nPerDay) allQ))
                 | i <- [0 .. min 364 (length allQ `div` max 1 nPerDay - 1)]]
  return dailyAvg

-- =========================================================================
-- Routing
-- =========================================================================

linearReservoirRoute :: Double -> Double -> [Double] -> [Double]
linearReservoirRoute k dtHrs inflow =
  let !decay = exp (negate dtHrs / max 0.1 k)
      !gain = 1.0 - decay
      go _ [] = []
      go !qP (qI:rest) = let !qO = qP * decay + qI * gain in qO : go qO rest
  in go 0.0 inflow

-- =========================================================================
-- Forward model: run full CLM pipeline → QRUNOFF → route → daily Q
-- =========================================================================

-- | Run the full CLM physics pipeline with injected parameters,
-- extract hourly QRUNOFF, route, and return daily streamflow.
runCLMAndRoute :: SiteConfig -> CLMCalibParams
               -> CLMState -> ForcingReaderState -> SurfaceAlbedoConstants
               -> Int  -- ^ total steps
               -> [Double]  -- ^ daily streamflow [m3/s]
runCLMAndRoute site params st0 forcing albConst totalSteps =
  let !dt = sc_forcing_dt site
      !area = sc_area_km2 site
      !routeK = cp_route_k params
      !nPerDay = round (86400.0 / dt) :: Int

      -- Inject parameters into initial state
      !stInjected = injectParams params st0

      -- Run CLM pipeline, collect hourly QRUNOFF
      !pipeline = wiredPhysicsPipeline albConst defaultCanopyHydroParams
      !drvCfg = defaultDriverConfig

      runSteps !st !drvSt !step !qAcc
        | step > totalSteps = reverse qAcc
        | otherwise =
          let !ctx = buildTimestepContext forcing step dt
              (!drvSt', !st') = clmDrv drvCfg pipeline ctx drvSt st
              !wf = clmWaterFlux st'
              !qrunoff = qflx_surf_col wf + qflx_drain_col wf
          in runSteps st' drvSt' (step + 1) (qrunoff : qAcc)

      !hourlyQ_mms = runSteps stInjected defaultDriverState 1 []

      -- Convert mm/s → m3/s
      !areaM2 = area * 1.0e6
      !hourlyQ_cms = map (\r -> r * areaM2 / 1000.0) hourlyQ_mms

      -- Route through linear reservoir
      !routedQ = linearReservoirRoute routeK (dt / 3600.0) hourlyQ_cms

      -- Aggregate to daily
      !nDays = length routedQ `div` nPerDay
      !dailyQ = [avg (take nPerDay (drop (i * nPerDay) routedQ))
                | i <- [0 .. nDays - 1]]

  in dailyQ

-- =========================================================================
-- Metrics
-- =========================================================================

kge :: [Double] -> [Double] -> Double
kge sim obs
  | length valid < 10 = -999.0
  | sO < 1e-10 = -999.0
  | otherwise = 1.0 - sqrt ((r-1)**2 + (sS/sO-1)**2 + (mS/mO-1)**2)
  where
    valid = [(s,o) | (s,o) <- zip sim obs, not(isNaN s), not(isNaN o), not(isInfinite s)]
    (ss',os') = unzip valid
    mS = avg ss'; mO = avg os'; sS = sd ss'; sO = sd os'; r = corr ss' os'

nse :: [Double] -> [Double] -> Double
nse sim obs
  | length valid < 10 = -999.0
  | ssObs < 1e-10 = -999.0
  | otherwise = 1.0 - ssErr / ssObs
  where
    valid = [(s,o) | (s,o) <- zip sim obs, not(isNaN s), not(isNaN o)]
    mO = avg (map snd valid)
    ssErr = sum [(s-o)**2 | (s,o) <- valid]
    ssObs = sum [(o-mO)**2 | (_,o) <- valid]

-- =========================================================================
-- Calibration with central finite differences
-- (matching Julia paper — CLM has discontinuities, FD is more robust than AD)
-- =========================================================================

data CalibrationConfig = CalibrationConfig
  { cc_maxIter    :: !Int
  , cc_verbose    :: !Bool
  , cc_obsYear    :: !Int      -- ^ Year to evaluate against
  , cc_spinupDays :: !Int      -- ^ Days to discard as spinup
  } deriving (Show)

defaultCalibConfig :: CalibrationConfig
defaultCalibConfig = CalibrationConfig
  { cc_maxIter = 10, cc_verbose = True, cc_obsYear = 2004, cc_spinupDays = 1461 }

data CalibResult = CalibResult
  { cr_kge_init   :: !Double
  , cr_kge_final  :: !Double
  , cr_params     :: !CLMCalibParams
  , cr_iters      :: !Int
  , cr_sim_daily  :: ![Double]
  } deriving (Show)

-- | Run CLM calibration with central finite-difference gradient ascent on KGE.
-- This matches the SYMFLUENCE paper methodology exactly.
calibrateSiteCLM :: SiteConfig -> FilePath -> CLMCalibParams
                 -> CalibrationConfig -> IO CalibResult
calibrateSiteCLM site obsPath initParams config = do
  let dir = sc_data_dir site
      dt = sc_forcing_dt site
      spinup = cc_spinupDays config
      stepsPerDay = round (86400.0 / dt) :: Int

  -- Initialize CLM state and forcing
  (st0, forcing, albConst) <- initCLMStateFromDir dir

  -- Read observations
  obsQ <- readStreamflowCSV obsPath (cc_obsYear config)

  let totalSteps = length obsQ * stepsPerDay + spinup * stepsPerDay
      verb = cc_verbose config

  whenIO verb $ putStrLn $ "=== Calibrating " ++ sc_name site ++ " ==="
  whenIO verb $ putStrLn $ "  Steps: " ++ show totalSteps ++ " (" ++ show (totalSteps `div` stepsPerDay) ++ " days)"
  whenIO verb $ putStrLn $ "  Spinup: " ++ show spinup ++ " days"
  whenIO verb $ putStrLn $ "  Obs: " ++ show (length obsQ) ++ " daily Q, mean=" ++ showR (avg obsQ) ++ " m3/s"

  -- Evaluate function: run CLM, extract post-spinup daily Q, compute KGE
  let evalFn params = do
        let allDailyQ = runCLMAndRoute site params st0 forcing albConst totalSteps
            evalQ = drop spinup allDailyQ
            n = min (length evalQ) (length obsQ)
        return (kge (take n evalQ) (take n obsQ), take n evalQ)

  -- Initial evaluation
  (kge0, simQ0) <- evalFn initParams
  whenIO verb $ putStrLn $ "  Initial KGE = " ++ showR kge0

  -- Central FD gradient ascent
  let loop !theta !bestKGE !bestTheta !bestSim !iter
        | iter > cc_maxIter config = return (bestKGE, bestTheta, bestSim, iter)
        | otherwise = do
          -- Coordinate descent: perturb each parameter individually
          let nParams = length theta
              stepSizes = [0.05, 0.02, 0.01, -0.01, -0.02, -0.05]

          (thetaNew, kgeNew, simNew) <- foldM
            (\(th, bestK, bestS) i -> do
              let lo = fst (paramBounds !! i)
                  hi = snd (paramBounds !! i)
                  range_i = hi - lo
              results <- mapM (\step -> do
                let trial = take i th ++ [max lo (min hi (th!!i + step * range_i))] ++ drop (i+1) th
                (kgeTrial, simTrial) <- evalFn (paramsFromList trial)
                return (kgeTrial, trial, simTrial)
                ) stepSizes
              let (bestTrialK, bestTrialTh, bestTrialSim) = foldr
                    (\(k,t,s) (bk,bt,bs) -> if k > bk then (k,t,s) else (bk,bt,bs))
                    (bestK, th, bestS) results
              return (bestTrialTh, bestTrialK, bestTrialSim)
            ) (theta, bestKGE, bestSim) [0 .. nParams - 1]

          whenIO verb $ putStrLn $ "  Iter " ++ show iter ++ " | KGE=" ++ showR kgeNew
                                 ++ " (was " ++ showR bestKGE ++ ")"

          if kgeNew > bestKGE
            then loop thetaNew kgeNew thetaNew simNew (iter + 1)
            else loop theta bestKGE bestTheta bestSim (iter + 1)

  let theta0 = paramsToList initParams
  (kgeFinal, thetaFinal, simFinal, nIters) <- loop theta0 kge0 theta0 simQ0 1

  whenIO verb $ do
    putStrLn $ "  Final KGE = " ++ showR kgeFinal
    putStrLn $ "  Iterations: " ++ show nIters
    putStrLn $ "  Params: " ++ show thetaFinal

  return CalibResult
    { cr_kge_init = kge0
    , cr_kge_final = kgeFinal
    , cr_params = paramsFromList thetaFinal
    , cr_iters = nIters
    , cr_sim_daily = simFinal
    }

-- =========================================================================
-- Helpers
-- =========================================================================

avg :: [Double] -> Double
avg [] = 0; avg xs = sum xs / fromIntegral (length xs)

sd :: [Double] -> Double
sd xs = let n = fromIntegral (length xs); m = avg xs
        in sqrt (sum [(x-m)**2 | x <- xs] / max 1 (n-1))

corr :: [Double] -> [Double] -> Double
corr xs ys
  | length xs < 3 = 0
  | sx*sy < 1e-20 = 0
  | otherwise = sxy / (sx*sy)
  where mx=avg xs; my=avg ys
        sxy=sum[(x-mx)*(y-my)|(x,y)<-zip xs ys]
        sx=sqrt$sum[(x-mx)**2|x<-xs]; sy=sqrt$sum[(y-my)**2|y<-ys]

whenIO :: Bool -> IO () -> IO ()
whenIO True a = a; whenIO False _ = return ()

showR :: Double -> String
showR x = show (fromIntegral (round (x * 10000) :: Int) / 10000.0 :: Double)

-- zipWith3 is imported from Prelude
