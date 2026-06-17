import Test.Hspec
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)

import CLM.Constants.PhysicalConstants
import CLM.Constants.ControlFlags (defaultDriverConfig)
import CLM.Infrastructure.Tridiagonal (tridiagonalSolve)
import CLM.Infrastructure.Filters (maskToIndices)
import CLM.Driver.CLMDriver
  ( CLMState(..), TimestepContext(..), defaultCLMState
  , defaultTimestepContext, clmDrvPatch2Col )
import CLM.Driver.PhysicsAdapters
  ( canopyFluxesStep, canopyHydrologyStep, snowWaterStep )
import CLM.Driver.PipelineRunner
  ( PipelineConfig(..), defaultPipelineConfig, initCLMStateFromDir
  , runPipeline, DailyDiag(..), runCLMForQrunoff )
import CLM.Types.ColumnData (ColumnData(..), defaultColumnData)
import CLM.Types.WaterStateData (WaterStateData(..), defaultWaterStateData)
import CLM.Types.WaterFluxData (WaterFluxData(..), defaultWaterFluxData)
import CLM.Types.EnergyFluxData (EnergyFluxData(..), defaultEnergyFluxData)
import CLM.Types.WaterDiagnosticBulkData
  ( WaterDiagnosticBulkData(..), defaultWaterDiagnosticBulkData )
import CLM.Types.CanopyStateData (CanopyStateData(..), defaultCanopyStateData)
import CLM.Types.TemperatureData
  ( TemperatureData(..), defaultTemperatureData )
import CLM.Types.SoilStateData (SoilStateData(..), defaultSoilStateData)

-- Modules under test
import CLM.BioGeoPhys.Photosynthesis
import CLM.BioGeoPhys.CanopyHydrology
import CLM.BioGeoPhys.QSat (QSatResult(..), qsat)
import CLM.BioGeoPhys.SoilWaterMovement
import CLM.BioGeoPhys.SnowSNICAR
import CLM.BioGeoPhys.Irrigation
import CLM.BioGeoPhys.SurfaceResistance
import CLM.BioGeoPhys.RootBioPhys
import CLM.BioGeoPhys.DryDepVelocity (weselySeason, pftToWesely)
import CLM.BioGeoPhys.BalanceCheck
import CLM.BioGeoPhys.SurfaceRadiation (LongwaveInput(..), LongwaveResult(..), longwaveRadiation, netRadiation)
import CLM.BioGeoPhys.Aerosol
import CLM.BioGeoPhys.HillslopeHydrology
import CLM.BioGeoChem.Methane
import CLM.BioGeoChem.Phenology
import CLM.BioGeoChem.Allocation
import CLM.BioGeoChem.NutrientCompetition
import CLM.BioGeoChem.CIsoFlux
import CLM.BioGeoChem.DustEmission
import CLM.BioGeoChem.CNProducts
import CLM.BioGeoChem.VOCEmission
import CLM.BioGeoChem.FUN
import CLM.BioGeoChem.CNDV
import CLM.BioGeoChem.CNDriver (centuryDecomp, mimicsDecomp)
import CLM.Infrastructure.SmoothAD
import CLM.Calibration.ADSensitivity
import CLM.Calibration.FluxnetReader (generateSyntheticFluxnet, FluxnetTimestep(..), FluxnetForcing(..), FluxnetTarget(..))
import CLM.Calibration.Optimize
import CLM.Infrastructure.NetCDF
import CLM.Infrastructure.HistoryWriter (ReferenceRow(..), readReferenceCSV)
import CLM.Calibration.SiteCalibration
  ( SiteConfig(..), readStreamflowCSV, kge, nse, linearReservoirRoute
  , CLMCalibParams(..), defaultCLMCalibParams, ddsOptimalParams
  , injectParams, runCLMAndRoute
  , CalibrationConfig(..), defaultCalibConfig, calibrateSiteCLM )
import CLM.Calibration.FortranParity
  ( bgcDumpDir, bgcSteps, dumpPath, baselineReport, identityReport, FieldDiff(..) )
import Numeric.AD (grad)

-- Production-source markers that indicate unfinished porting work.
-- The port-completion audit intentionally fails while any remain.
portDebtMarkers :: [String]
portDebtMarkers =
  [ "placeholder"
  , "stub"
  , "todo"
  , "no-op"
  , "not yet"
  , "simplified"
  ]

hsFilesUnder :: FilePath -> IO [FilePath]
hsFilesUnder root = do
  exists <- doesDirectoryExist root
  if not exists
    then return []
    else do
      names <- listDirectory root
      fmap concat $ mapM visit names
  where
    visit name = do
      let path = root </> name
      isDir <- doesDirectoryExist path
      if isDir
        then hsFilesUnder path
        else if takeExtension path == ".hs"
             then return [path]
             else return []

findPortDebt :: IO [String]
findPortDebt = do
  files <- fmap concat $ mapM hsFilesUnder ["src", "app"]
  fmap concat $ mapM scanFile files
  where
    scanFile path = do
      content <- readFile path
      let numbered = zip [1 :: Int ..] (lines content)
      return
        [ path ++ ":" ++ show lineNo ++ ": " ++ trim line
        | (lineNo, line) <- numbered
        , lineHasPortDebt line
        ]

    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

lineHasPortDebt :: String -> Bool
lineHasPortDebt line =
  let lowerLine = map toLower line
      tokens = words $ map tokenChar lowerLine
  in "not yet" `isInfixOf` lowerLine
     || any (`elem` tokens)
          [ "placeholder", "placeholders"
          , "stub", "stubs", "stubbed"
          , "todo", "todos"
          , "no-op"
          , "simplified"
          ]
  where
    tokenChar c
      | isAlphaNum c || c == '-' = c
      | otherwise = ' '

trajectoryParityTolerances :: [(String, Double)]
trajectoryParityTolerances =
  [ ("T_GRND", 0.10)       -- K, daily mean ground temperature
  , ("FSA", 1.00)          -- W/m2 absorbed shortwave
  , ("EFLX_LH_TOT", 0.50) -- W/m2
  , ("EFLX_SH_TOT", 0.50) -- W/m2
  , ("H2OSNO", 1.0e-3)    -- kg/m2
  , ("SNOW_DEPTH", 1.0e-3)
  , ("FRAC_SNO", 1.0e-4)
  ]

dailyDiagValue :: String -> DailyDiag -> Maybe Double
dailyDiagValue field dd =
  case field of
    "T_GRND"      -> Just (dd_t_grnd dd)
    "FSA"         -> Just (dd_fsa dd)
    "EFLX_LH_TOT" -> Just (dd_eflx_lh dd)
    "EFLX_SH_TOT" -> Just (dd_eflx_sh dd)
    "H2OSNO"      -> Just (dd_h2osno dd)
    "SNOW_DEPTH"  -> Just (dd_snow_depth dd)
    "FRAC_SNO"    -> Just (dd_frac_sno dd)
    _             -> Nothing

dailyParityFailures :: [ReferenceRow] -> [DailyDiag] -> [String]
dailyParityFailures refs dailies =
  concat
    [ compareDay day dd
    | (day, dd) <- zip [1 :: Int ..] dailies
    ]
  where
    refByDay = Map.fromList [(rr_day rr, rr_values rr) | rr <- refs]

    compareDay day dd =
      case Map.lookup day refByDay of
        Nothing -> ["missing Julia reference day " ++ show day]
        Just refMap -> concatMap (compareField day dd refMap) trajectoryParityTolerances

    compareField day dd refMap (field, tol) =
      case (Map.lookup field refMap, dailyDiagValue field dd) of
        (Just expected, Just actual)
          | abs (actual - expected) <= tol -> []
          | otherwise ->
              [ "day " ++ show day ++ " " ++ field
                ++ ": expected " ++ show expected
                ++ ", actual " ++ show actual
                ++ ", abs diff " ++ show (abs (actual - expected))
                ++ " > tolerance " ++ show tol
              ]
        (Nothing, _) -> ["missing Julia reference field " ++ field]
        (_, Nothing) -> ["missing Haskell diagnostic field " ++ field]

main :: IO ()
main = hspec $ do

  -- =====================================================================
  -- Port completion audit
  -- =====================================================================
  describe "Port completion audit" $ do
    it "keeps the tracked checklist in the repository" $ do
      doesFileExist "docs/PORT_COMPLETION_CHECKLIST.md" `shouldReturn` True
      checklist <- fmap (map toLower) (readFile "docs/PORT_COMPLETION_CHECKLIST.md")
      mapM_ (\marker -> checklist `shouldSatisfy` (marker `isInfixOf`)) portDebtMarkers

    it "has no unfinished placeholder/stub markers in production source" $ do
      debt <- findPortDebt
      debt `shouldBe` []

  -- =====================================================================
  -- Constants
  -- =====================================================================
  describe "PhysicalConstants" $ do
    it "freezing point is 273.15 K" $
      tfrz `shouldBe` 273.15

    it "latent heat of sublimation = hvap + hfus" $
      hsub `shouldBe` (hvap + hfus)

    it "grid dimensions are positive" $ do
      nlevsoi `shouldSatisfy` (> 0)
      nlevgrnd `shouldSatisfy` (> nlevsoi)
      nlevsno `shouldSatisfy` (> 0)

  -- =====================================================================
  -- Tridiagonal Solver
  -- =====================================================================
  describe "Tridiagonal solver" $ do
    it "solves a simple 3x3 system" $ do
      let a = VU.fromList [0.0, -1.0, -1.0]
          b = VU.fromList [2.0,  2.0,  2.0]
          c = VU.fromList [-1.0, -1.0, 0.0]
          r = VU.fromList [1.0,  0.0,  1.0]
          x = tridiagonalSolve a b c r
      VU.length x `shouldBe` 3
      abs (x VU.! 0 - 1.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 1 - 1.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 2 - 1.0) `shouldSatisfy` (< 1e-12)

    it "solves identity system" $ do
      let a = VU.fromList [0.0, 0.0, 0.0]
          b = VU.fromList [1.0, 1.0, 1.0]
          c = VU.fromList [0.0, 0.0, 0.0]
          r = VU.fromList [3.0, 7.0, 2.0]
          x = tridiagonalSolve a b c r
      abs (x VU.! 0 - 3.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 1 - 7.0) `shouldSatisfy` (< 1e-12)
      abs (x VU.! 2 - 2.0) `shouldSatisfy` (< 1e-12)

  -- =====================================================================
  -- Filters
  -- =====================================================================
  describe "Filters" $ do
    it "maskToIndices returns correct indices" $ do
      let mask = VU.fromList [True, False, True, False, True]
      maskToIndices mask `shouldBe` VU.fromList [0, 2, 4]

    it "empty mask gives empty indices" $
      VU.length (maskToIndices (VU.fromList [False, False, False])) `shouldBe` 0

  -- =====================================================================
  -- QSat
  -- =====================================================================
  describe "QSat" $ do
    it "saturation vapor pressure at 273.15K ~ 611 Pa" $ do
      let r = qsat tfrz 101325.0
      abs (qsr_es r - 611.0) `shouldSatisfy` (< 5.0)

    it "specific humidity increases with temperature" $ do
      let r1 = qsat 280.0 101325.0
          r2 = qsat 300.0 101325.0
      qsr_qs r2 `shouldSatisfy` (> qsr_qs r1)

  -- =====================================================================
  -- Photosynthesis
  -- =====================================================================
  describe "Photosynthesis" $ do
    it "quadratic solver returns correct root" $ do
      let (r1, r2) = quadraticSolve 1.0 (-3.0) 2.0
      abs (r1 - 2.0) `shouldSatisfy` (< 1e-10)
      abs (r2 - 1.0) `shouldSatisfy` (< 1e-10)

    it "temperature response ft > 0 for reasonable T" $
      ftPhoto 298.15 65330.0 `shouldSatisfy` (> 0.0)

    it "temperature response fth > 0" $
      fth25Photo 72000.0 200000.0 `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- SoilWaterMovement
  -- =====================================================================
  describe "SoilWaterMovement" $ do
    it "ice impedance is 1.0 when icefrac=0" $
      abs (iceImpedance 0.0 6.0 - 1.0) `shouldSatisfy` (< 1e-12)

    it "ice impedance decreases with ice fraction" $
      iceImpedance 0.5 6.0 `shouldSatisfy` (< 1.0)

    it "Clapp-Hornberger hk at saturation equals hksat" $ do
      let (hk, _) = clappHornbergerHk 1.0 1.0 10.0 4.0
      abs (hk - 10.0) `shouldSatisfy` (< 1e-6)

    it "Clapp-Hornberger hk decreases with drying" $ do
      let (hk1, _) = clappHornbergerHk 0.8 1.0 10.0 4.0
          (hk2, _) = clappHornbergerHk 0.3 1.0 10.0 4.0
      hk1 `shouldSatisfy` (> hk2)

    it "equilibrium water content is higher below water table" $
      equilibriumWaterContent 1000.0 0.0 500.0 0.4 5.5 200.0
        `shouldSatisfy` (> 0.0)

    it "equilibrium water content < porosity above water table" $
      equilibriumWaterContent 500.0 0.0 1000.0 0.4 5.5 200.0
        `shouldSatisfy` (< 0.4)

  -- =====================================================================
  -- SnowSNICAR
  -- =====================================================================
  describe "SnowSNICAR" $ do
    it "fresh snow radius is positive and finite at 253K" $ do
      let rds = freshSnowRadius defaultSnicarParams 253.0
      rds `shouldSatisfy` (> 0.0)
      rds `shouldSatisfy` (< 500.0)

    it "fresh snow radius increases at warmer temperatures" $ do
      let cold = freshSnowRadius defaultSnicarParams 240.0
          warm = freshSnowRadius defaultSnicarParams 270.0
      warm `shouldSatisfy` (> cold)

    it "delta-Eddington layer albedo in [0,1] for typical snow" $ do
      let de = deltaEddingtonLayer 2.0 0.999 0.86 0.5
      dep_rdif_a de `shouldSatisfy` (>= 0.0)
      dep_rdif_a de `shouldSatisfy` (<= 1.0)
      dep_rdir de `shouldSatisfy` (<= 1.5)

    it "thicker optical depth → higher albedo" $ do
      let de1 = deltaEddingtonLayer 0.5 0.999 0.86 0.5
          de2 = deltaEddingtonLayer 5.0 0.999 0.86 0.5
      dep_rdif_a de2 `shouldSatisfy` (> dep_rdif_a de1)

  -- =====================================================================
  -- Canopy hydrology adapter
  -- =====================================================================
  describe "Canopy hydrology adapter" $ do
    let canopyCtx = defaultTimestepContext
          { tcDtime = 1800.0
          , tcForcRain = VU.singleton 0.0
          , tcForcSnow = VU.singleton 2.0e-3
          , tcForcT = VU.singleton 268.15
          , tcForcWind = VU.singleton 0.0
          }
        canopyState = defaultCLMState
          { clmCanopyState = defaultCanopyStateData
              { cstate_patch_wtgcell = VU.fromList [0.5, 0.5]
              , cstate_frac_veg_nosno_patch = VU.fromList [0, 1]
              , cstate_frac_veg_nosno_alb_patch = VU.fromList [0, 1]
              , cstate_elai_patch = VU.fromList [0.0, 2.5]
              , cstate_esai_patch = VU.fromList [0.0, 1.0]
              }
          , clmWaterState = defaultWaterStateData
              { liqcan_patch_vec = VU.fromList [0.0, 0.0]
              , snocan_patch_vec = VU.fromList [0.0, 0.0]
              , h2ocan_patch_vec = VU.fromList [0.0, 0.0]
              }
          }
        snowRate = 2.0e-3
        vegVAI = 3.5
        fpisnow = 1.0 - exp (-0.5 * vegVAI)

    it "aggregates patch-weighted snow throughfall and keeps patch canopy snow state" $ do
      let st' = canopyHydrologyStep defaultDriverConfig canopyCtx canopyState
          expectedSnowGrnd =
            0.5 * snowRate + 0.5 * snowRate * (1.0 - fpisnow)
          expectedVegSnocan = 1800.0 * snowRate * fpisnow
          ws = clmWaterState st'
          wdiag = clmWaterDiagBulk st'
      abs (qflx_snow_grnd_col (clmWaterFlux st') - expectedSnowGrnd)
        `shouldSatisfy` (< 1.0e-12)
      VU.length (snocan_patch_vec ws) `shouldBe` 2
      abs (snocan_patch_vec ws VU.! 0) `shouldSatisfy` (< 1.0e-12)
      abs (snocan_patch_vec ws VU.! 1 - expectedVegSnocan)
        `shouldSatisfy` (< 1.0e-12)
      abs (snocan_patch ws - 0.5 * expectedVegSnocan)
        `shouldSatisfy` (< 1.0e-12)
      VU.length (wdiag_h2ocan_patch wdiag) `shouldBe` 2
      abs (wdiag_h2ocan_patch wdiag VU.! 1 - expectedVegSnocan)
        `shouldSatisfy` (< 1.0e-12)

  -- =====================================================================
  -- Canopy flux adapter
  -- =====================================================================
  describe "Canopy flux adapter" $ do
    let nlevtot = nlevsno + nlevgrnd
        rootfr =
          VU.generate nlevgrnd $ \j ->
            if j < nlevsoi then 1.0 / fromIntegral nlevsoi else 0.0
        soilVec x = VU.replicate nlevgrnd x
        layerVec x = VU.replicate nlevtot x
        baseFluxState cgrndsCan cgrndlCan = defaultCLMState
          { clmSnl = 0
          , clmColumn = defaultColumnData
              { colDz = layerVec 0.1
              , watsat = soilVec 0.45
              , bsw = soilVec 5.0
              , sucsat = soilVec 100.0
              , zii = 1000.0
              }
          , clmTemp = defaultTemperatureData
              { t_soisno_col = layerVec 272.0
              , t_grnd_col = 272.0
              , t_h2osfc_col = 274.0
              , t_veg_patch = 258.0
              , t_veg_patch_vec = VU.fromList [258.0, 258.0]
              }
          , clmSoilState = defaultSoilStateData
              { sstate_watsat_col = soilVec 0.45
              , sstate_bsw_col = soilVec 5.0
              , sstate_sucsat_col = soilVec 100.0
              , sstate_soilbeta_col = VU.singleton 0.8
              , sstate_rootfr_patch = VU.concat [rootfr, rootfr]
              , sstate_smpso_patch = VU.fromList [-66000.0, -66000.0]
              , sstate_smpsc_patch = VU.fromList [-255000.0, -255000.0]
              }
          , clmWaterState = defaultWaterStateData
              { h2osoi_liq_col =
                  VU.generate nlevtot $ \j -> if j < nlevsno then 0.0 else 35.0
              , h2osoi_ice_col = layerVec 0.0
              , liqcan_patch_vec = VU.fromList [0.0, 0.0]
              , snocan_patch_vec = VU.fromList [0.0, 0.0]
              , h2ocan_patch_vec = VU.fromList [0.0, 0.0]
              }
          , clmWaterDiagBulk = defaultWaterDiagnosticBulkData
              { wdiag_qg_col = VU.singleton 0.002
              , wdiag_qg_snow_col = VU.singleton 0.002
              , wdiag_qg_soil_col = VU.singleton 0.002
              , wdiag_qg_h2osfc_col = VU.singleton 0.002
              , wdiag_dqgdT_col = VU.singleton 3.0e-4
              , wdiag_frac_sno_eff_col = VU.singleton 0.0
              , wdiag_frac_h2osfc_col = VU.singleton 0.0
              , wdiag_snow_depth_col = VU.singleton 0.0
              , wdiag_fwet_patch = VU.fromList [0.0, 0.0]
              , wdiag_fdry_patch = VU.fromList [1.0, 1.0]
              }
          , clmCanopyState = defaultCanopyStateData
              { cstate_patch_wtgcell = VU.fromList [0.5, 0.5]
              , cstate_frac_veg_nosno_patch = VU.fromList [0, 1]
              , cstate_frac_veg_nosno_alb_patch = VU.fromList [0, 1]
              , cstate_elai_patch = VU.fromList [0.0, 1.5]
              , cstate_esai_patch = VU.fromList [0.0, 0.3]
              , cstate_htop_patch = VU.fromList [0.0, 10.0]
              , cstate_z0m_patch = VU.fromList [0.0, 0.5]
              , cstate_displa_patch = VU.fromList [0.0, 6.7]
              , cstate_laisun_patch = VU.fromList [0.0, 0.75]
              , cstate_laisha_patch = VU.fromList [0.0, 0.75]
              , cstate_dleaf_patch = VU.fromList [0.04, 0.04]
              }
          , clmEnergyFlux = defaultEnergyFluxData
              { sabv_patch_vec = VU.fromList [0.0, 10.0]
              , cgrnds_patch_vec = VU.fromList [7.0, cgrndsCan]
              , cgrndl_patch_vec = VU.fromList [1.0e-40, cgrndlCan]
              , cgrnd_patch_vec = VU.fromList [7.0, cgrndsCan]
              }
          }
        canopyCtx = defaultTimestepContext
          { tcDtime = 1800.0
          , tcForcT = VU.singleton 258.0
          , tcForcTh = VU.singleton 258.0
          , tcForcQ = VU.singleton 0.002
          , tcForcPbot = VU.singleton 90000.0
          , tcForcRho = VU.singleton 1.05
          , tcForcLwrad = VU.singleton 235.0
          , tcForcWind = VU.singleton 4.0
          , tcForcHgt = 30.0
          }

    it "passes patch-level pre-canopy ground conductance into canopy fluxes" $ do
      let st0 = canopyFluxesStep defaultDriverConfig canopyCtx (baseFluxState 0.0 0.0)
          st1 = canopyFluxesStep defaultDriverConfig canopyCtx (baseFluxState 10.0 2.0e-6)
          ef0 = clmEnergyFlux st0
          ef1 = clmEnergyFlux st1
          dcgrnds = cgrnds_patch_vec ef1 VU.! 1 - cgrnds_patch_vec ef0 VU.! 1
          dcgrndl = cgrndl_patch_vec ef1 VU.! 1 - cgrndl_patch_vec ef0 VU.! 1
      abs (dcgrnds - 10.0) `shouldSatisfy` (< 1.0e-12)
      abs (dcgrndl - 2.0e-6) `shouldSatisfy` (< 1.0e-15)

  -- =====================================================================
  -- CanopyHydrology
  -- =====================================================================
  describe "CanopyHydrology" $ do
    it "uses the wind unloading timescale instead of unloading all canopy snow" $ do
      let snocan = 10.0
          wind = 5.0
          out = bulkFluxSnowUnloading SnowUnloadingInput
            { sui_frac_veg_nosno = 1
            , sui_snocan = snocan
            , sui_forc_t = 268.15
            , sui_forc_wind = wind
            , sui_dtime = 1800.0
            , sui_params = defaultCanopyHydroParams
            }
          expectedWindUnload =
            snocan * wind / chp_snowcan_unload_wind_fact defaultCanopyHydroParams
      abs (sur_qflx_snowindunload out - expectedWindUnload)
        `shouldSatisfy` (< 1.0e-15)
      sur_qflx_snow_unload out `shouldSatisfy` (< snocan / 1800.0)

  -- =====================================================================
  -- Snow water adapter
  -- =====================================================================
  describe "Snow water adapter" $ do
    let snowCtx snowRate = defaultTimestepContext
          { tcDtime = 1800.0
          , tcForcSnow = VU.singleton snowRate
          , tcForcT = VU.singleton 268.15
          }
        snowState swe persist = defaultCLMState
          { clmWaterState = defaultWaterStateData
              { h2osno_col = swe }
          , clmWaterDiagBulk = defaultWaterDiagnosticBulkData
              { wdiag_frac_sno_col = VU.singleton (if swe > 0.0 then 0.3 else 0.0)
              , wdiag_frac_sno_eff_col = VU.singleton (if swe > 0.0 then 0.3 else 0.0)
              , wdiag_snow_depth_col = VU.singleton (swe / 100.0)
              , wdiag_snow_persist_col = VU.singleton persist
              }
          , clmSnl = 0
          }
        snowPersist st =
          let v = wdiag_snow_persist_col (clmWaterDiagBulk st)
          in if VU.null v then 0.0 else v VU.! 0

    it "ages no-layer snow while the snowpack remains" $ do
      let st' = snowWaterStep defaultDriverConfig (snowCtx 0.0)
                  (snowState 10.0 86400.0)
      abs (snowPersist st' - 88200.0) `shouldSatisfy` (< 1.0e-9)

    it "dilutes snow persistence when fresh snow arrives" $ do
      let st' = snowWaterStep defaultDriverConfig (snowCtx (10.0 / 1800.0))
                  (snowState 10.0 86400.0)
      abs (snowPersist st' - 45000.0) `shouldSatisfy` (< 1.0e-9)

    it "accumulates Swenson-Lawrence no-layer snow depth incrementally" $ do
      let oldSwe = 0.1
          newSnow = 0.1
          oldFrac = 0.1
          oldDepth = 0.2
          forcT = 268.15
          bifall = 50.0 + 1.7 * (max 0.0 (forcT - tfrz + 15.0)) ** 1.5
          expectedFrac = oldFrac + tanh (0.1 * newSnow) * (1.0 - oldFrac)
          expectedDepth = oldDepth + newSnow / (bifall * expectedFrac)
          st0 = (snowState oldSwe 0.0)
            { clmWaterDiagBulk = (clmWaterDiagBulk (snowState oldSwe 0.0))
                { wdiag_frac_sno_col = VU.singleton oldFrac
                , wdiag_frac_sno_eff_col = VU.singleton oldFrac
                , wdiag_snow_depth_col = VU.singleton oldDepth
                }
            }
          st' = snowWaterStep defaultDriverConfig (snowCtx (newSnow / 1800.0)) st0
          wdiag = clmWaterDiagBulk st'
      abs (wdiag_frac_sno_col wdiag VU.! 0 - expectedFrac)
        `shouldSatisfy` (< 1.0e-12)
      abs (wdiag_snow_depth_col wdiag VU.! 0 - expectedDepth)
        `shouldSatisfy` (< 1.0e-12)

    it "resets snow persistence when no snow remains" $ do
      let st' = snowWaterStep defaultDriverConfig (snowCtx 0.0)
                  (snowState 0.0 86400.0)
      snowPersist st' `shouldBe` 0.0

    it "uses canopy snow-throughfall once canopy hydrology has run" $ do
      let st0 = (snowState 0.0 0.0)
            { clmWaterFlux = defaultWaterFluxData
                { qflx_snow_grnd_col = 1.0e-3 }
            , clmWaterDiagBulk = (clmWaterDiagBulk (snowState 0.0 0.0))
                { wdiag_fwet_patch = VU.singleton 0.0
                , wdiag_fdry_patch = VU.singleton 1.0
                , wdiag_fcansno_patch = VU.singleton 0.0
                }
            }
          st' = snowWaterStep defaultDriverConfig (snowCtx 2.0e-3) st0
      abs (h2osno_col (clmWaterState st') - 1.8) `shouldSatisfy` (< 1.0e-12)

  -- =====================================================================
  -- Driver patch-to-column aggregation
  -- =====================================================================
  describe "Driver patch-to-column aggregation" $ do
    it "uses patch weights for energy and water flux vectors" $ do
      let st0 = defaultCLMState
            { clmCanopyState = defaultCanopyStateData
                { cstate_patch_wtgcell = VU.fromList [0.25, 0.75] }
            , clmEnergyFlux = defaultEnergyFluxData
                { eflx_sh_tot_patch_vec = VU.fromList [10.0, 30.0]
                , eflx_lh_tot_patch_vec = VU.fromList [2.0, 6.0]
                , eflx_sh_grnd_patch_vec = VU.fromList [4.0, 8.0]
                , sabg_patch_vec = VU.fromList [100.0, 200.0]
                , sabv_patch_vec = VU.fromList [5.0, 9.0]
                , fsa_patch_vec = VU.fromList [105.0, 209.0]
                , cgrnds_patch_vec = VU.fromList [1.0, 3.0]
                , cgrndl_patch_vec = VU.fromList [0.1, 0.3]
                , cgrnd_patch_vec = VU.fromList [4.0, 8.0]
                , dlrad_patch_vec = VU.fromList [20.0, 40.0]
                , ulrad_patch_vec = VU.fromList [30.0, 50.0]
                , eflx_lwrad_out_patch_vec = VU.fromList [300.0, 340.0]
                , eflx_lwrad_net_patch_vec = VU.fromList [10.0, 30.0]
                }
            , clmWaterFlux = defaultWaterFluxData
                { qflx_evap_tot_patch_vec = VU.fromList [1.0e-4, 3.0e-4]
                , qflx_evap_grnd_patch_vec = VU.fromList [2.0e-5, 6.0e-5]
                , qflx_tran_veg_patch_vec = VU.fromList [4.0e-5, 8.0e-5]
                }
            }
          st' = clmDrvPatch2Col st0
          ef = clmEnergyFlux st'
          wf = clmWaterFlux st'
      eflx_sh_tot_patch ef `shouldBe` 25.0
      eflx_lh_tot_patch ef `shouldBe` 5.0
      eflx_sh_grnd_patch ef `shouldBe` 7.0
      sabg_patch ef `shouldBe` 175.0
      sabv_patch ef `shouldBe` 8.0
      fsa_patch ef `shouldBe` 183.0
      cgrnds_patch ef `shouldBe` 2.5
      abs (cgrndl_patch ef - 0.25) `shouldSatisfy` (< 1.0e-15)
      cgrnd_patch ef `shouldBe` 7.0
      dlrad_patch ef `shouldBe` 35.0
      ulrad_patch ef `shouldBe` 45.0
      eflx_lwrad_out_patch ef `shouldBe` 330.0
      eflx_lwrad_net_patch ef `shouldBe` 25.0
      abs (qflx_evap_tot_patch wf - 2.5e-4) `shouldSatisfy` (< 1.0e-15)
      abs (qflx_evap_grnd_col wf - 5.0e-5) `shouldSatisfy` (< 1.0e-15)
      abs (qflx_tran_veg_patch wf - 7.0e-5) `shouldSatisfy` (< 1.0e-15)

  -- =====================================================================
  -- Methane
  -- =====================================================================
  describe "Methane" $ do
    it "Q10 factor is 1.0 at base temperature" $
      abs (ch4ProdQ10Factor 1.5 295.0 295.0 - 1.0) `shouldSatisfy` (< 1e-10)

    it "Q10 factor increases with temperature" $
      ch4ProdQ10Factor 1.5 295.0 305.0 `shouldSatisfy` (> 1.0)

    it "Michaelis-Menten oxidation rate is zero with zero CH4" $
      abs (ch4OxidRate 0.01 0.0 0.005 0.01 0.02) `shouldSatisfy` (< 1e-15)

    it "ebullition triggers above threshold" $
      ch4EbullitionCheck 0.5 0.15 300.0 `shouldBe` True

    it "no ebullition below threshold" $
      ch4EbullitionCheck 0.001 0.15 280.0 `shouldBe` False

    it "CH4 diffusivity is positive for unsaturated soil" $ do
      let (d_ch4, d_o2) = ch4Diffusivity defaultCH4Params 0.4 0.2 280.0
      d_ch4 `shouldSatisfy` (> 0.0)
      d_o2 `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- Phenology
  -- =====================================================================
  describe "Phenology" $ do
    it "evergreen phenology produces background litterfall" $ do
      let st = phenologyInit defaultPhenologyParams 1800.0
          (leafLit, frootLit) = evergreenPhenology st 2.0 100.0 50.0
      leafLit `shouldSatisfy` (> 0.0)
      frootLit `shouldSatisfy` (> 0.0)

    it "seasonal onset triggers when GDD exceeds threshold" $ do
      let st = phenologyInit defaultPhenologyParams 1800.0
          pp = defaultPatchPhenology { pph_dormant_flag = True, pph_onset_gdd = 100.0 }
          pp' = seasonalDecidOnset st pp 50000.0 280.0 0.0 0.2
      pph_onset_flag pp' `shouldBe` True

    it "critical daylength increases at lower latitudes" $ do
      let dl60 = seasonalCriticalDaylength defaultPhenologyParams 60.0
          dl40 = seasonalCriticalDaylength defaultPhenologyParams 40.0
      dl40 `shouldSatisfy` (< dl60)

  -- =====================================================================
  -- Allocation
  -- =====================================================================
  describe "Allocation" $ do
    it "allocation sums to available C (modulo growth resp)" $ do
      let alloc = calcAllocation AllocInput
            { ali_availc = 1.0e-5, ali_ivt = 1, ali_woody = 1.0
            , ali_froot_leaf = 1.0, ali_croot_stem = 1.0
            , ali_stem_leaf = 1.5, ali_flivewd = 0.5
            , ali_leafcn = 25.0, ali_frootcn = 42.0
            , ali_livewdcn = 50.0, ali_deadwdcn = 500.0
            , ali_grperc = 0.3, ali_downreg = 1.0
            }
          cTotal = alo_cpool_to_leafc alloc + alo_cpool_to_frootc alloc
                 + alo_cpool_to_livestemc alloc + alo_cpool_to_deadstemc alloc
                 + alo_cpool_to_livecrootc alloc + alo_cpool_to_deadcrootc alloc
          grTotal = alo_cpool_leaf_gr alloc + alo_cpool_froot_gr alloc
                  + alo_cpool_livestem_gr alloc + alo_cpool_deadstem_gr alloc
                  + alo_cpool_livecroot_gr alloc + alo_cpool_deadcroot_gr alloc
      abs (cTotal + grTotal - 1.0e-5) `shouldSatisfy` (< 1e-15)

    it "N demand is positive when C is allocated" $
      alo_plant_ndemand (calcAllocation AllocInput
        { ali_availc = 1.0e-5, ali_ivt = 1, ali_woody = 0.0
        , ali_froot_leaf = 1.0, ali_croot_stem = 0.0
        , ali_stem_leaf = 0.0, ali_flivewd = 0.0
        , ali_leafcn = 25.0, ali_frootcn = 42.0
        , ali_livewdcn = 50.0, ali_deadwdcn = 500.0
        , ali_grperc = 0.3, ali_downreg = 1.0
        }) `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- NutrientCompetition
  -- =====================================================================
  describe "NutrientCompetition" $ do
    it "fpi=fpg=1 when supply exceeds demand" $ do
      let out = calcNCompetition NCompetitionInput
            { nci_plant_ndemand = 1.0e-8, nci_decomp_ndemand = 1.0e-8
            , nci_sminn = 1.0, nci_dt = 1800.0
            , nci_use_nitrif_denitrif = False }
      abs (nco_fpi out - 1.0) `shouldSatisfy` (< 1e-10)
      abs (nco_fpg out - 1.0) `shouldSatisfy` (< 1e-10)

    it "N-limited: fpi < 1 and fpg < 1" $ do
      let out = calcNCompetition NCompetitionInput
            { nci_plant_ndemand = 1.0, nci_decomp_ndemand = 1.0
            , nci_sminn = 0.001, nci_dt = 1800.0
            , nci_use_nitrif_denitrif = False }
      nco_fpi out `shouldSatisfy` (< 1.0)
      nco_fpg out `shouldSatisfy` (< 1.0)

    it "dynamic stem:leaf ratio increases with NPP" $ do
      let low  = calcDynamicStemLeaf DynamicStemLeaf { dsl_stem_leaf_flag = -1.0, dsl_annsum_npp = 100.0 }
          high = calcDynamicStemLeaf DynamicStemLeaf { dsl_stem_leaf_flag = -1.0, dsl_annsum_npp = 1000.0 }
      high `shouldSatisfy` (> low)

  -- =====================================================================
  -- CIsoFlux
  -- =====================================================================
  describe "CIsoFlux" $ do
    it "C13 discrimination factor > 1 for C3 plants" $
      photosyntheticDiscrimination C13 True 0.7 `shouldSatisfy` (> 1.0)

    it "C14 discrimination is squared C13" $ do
      let d13 = photosyntheticDiscrimination C13 True 0.7
          d14 = photosyntheticDiscrimination C14 True 0.7
      abs (d14 - d13 ** 2) `shouldSatisfy` (< 1e-10)

    it "C14 decay factor < 1 for positive timestep" $
      c14DecayFactor 1800.0 `shouldSatisfy` (< 1.0)

    it "C14 decay factor is near 1 for short timestep" $
      abs (c14DecayFactor 1.0 - 1.0) `shouldSatisfy` (< 1e-10)

  -- =====================================================================
  -- DustEmission
  -- =====================================================================
  describe "DustEmission" $ do
    it "overlap factor is non-negative" $
      calcOverlapFactor 0.832e-6 2.1 0.036 0.1e-6 1.0e-6 `shouldSatisfy` (>= 0.0)

    it "overlap factor is bounded by mass fraction" $
      calcOverlapFactor 0.832e-6 2.1 0.036 0.1e-6 10.0e-6 `shouldSatisfy` (<= 0.036)

    it "LAI sheltering reduces to 1 with no vegetation" $
      abs (calcLAIShelter 0.0 - 1.0) `shouldSatisfy` (< 1e-10)

    it "LAI sheltering decreases with vegetation" $
      calcLAIShelter 3.0 `shouldSatisfy` (< 1.0)

    it "threshold friction velocity increases with soil moisture" $ do
      let dry = calcThreshFricVel 0.3 0.0 0.01
          wet = calcThreshFricVel 0.3 0.05 0.01
      wet `shouldSatisfy` (> dry)

  -- =====================================================================
  -- VOCEmission (MEGAN)
  -- =====================================================================
  describe "VOCEmission" $ do
    it "gamma_P is 1 when LDF=0 (light-independent)" $
      abs (getGammaP 500.0 200.0 180.0 0.0 - 1.0) `shouldSatisfy` (< 1e-10)

    it "gamma_L increases with LAI up to a point" $ do
      let gL1 = getGammaL 1.0
          gL4 = getGammaL 4.0
      gL4 `shouldSatisfy` (> gL1)

    it "gamma_SM is 0 for completely dry soil" $
      abs (getGammaSM 0.0) `shouldSatisfy` (< 1e-10)

    it "gamma_SM is 1 for saturated soil" $
      abs (getGammaSM 1.0 - 1.0) `shouldSatisfy` (< 1e-10)

    it "CO2 inhibition reduces isoprene at high CO2" $
      getGammaC 800.0 `shouldSatisfy` (< getGammaC 400.0)

  -- =====================================================================
  -- CNProducts
  -- =====================================================================
  describe "CNProducts" $ do
    it "product pool decays over time" $ do
      let st = CNProductsState 100.0 1000.0 5000.0
          fl = CNProductsFluxes 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
          out = productPoolUpdate ProductUpdateInput { pui_state = st, pui_fluxes = fl, pui_dt = 86400.0 }
          st' = puo_state out
      cps_cropprod1 st' `shouldSatisfy` (< 100.0)
      cps_prod10 st' `shouldSatisfy` (< 1000.0)
      cps_prod100 st' `shouldSatisfy` (< 5000.0)

    it "product pool gains from harvest" $ do
      let st = CNProductsState 0.0 0.0 0.0
          fl = CNProductsFluxes 0.0 0.0 0.0 0.0 0.0 1.0e-5 0.0 0.0
          out = productPoolUpdate ProductUpdateInput { pui_state = st, pui_fluxes = fl, pui_dt = 86400.0 }
      cps_cropprod1 (puo_state out) `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- FUN
  -- =====================================================================
  describe "FUN" $ do
    it "fixation cost is bigCost for non-fixers" $
      funCostFix 0 1.0 0.1 50.0 0.5 (-1.0) 20.0 `shouldBe` bigCost

    it "fixation cost is finite for fixers" $
      funCostFix 1 1.0 0.1 50.0 0.5 (-1.0) 20.0 `shouldSatisfy` (< bigCost)

    it "active uptake cost decreases with more N available" $ do
      let low  = funCostActive 0.001 1.0 1.0 100.0 0.5 1e-20
          high = funCostActive 0.1   1.0 1.0 100.0 0.5 1e-20
      high `shouldSatisfy` (< low)

  -- =====================================================================
  -- CNDV
  -- =====================================================================
  describe "CNDV" $ do
    it "FPC from LAI follows 1-exp(-0.5*LAI)" $ do
      abs (calcFPCInd 0.0) `shouldSatisfy` (< 1e-10)
      abs (calcFPCInd 10.0 - (1.0 - exp (-5.0))) `shouldSatisfy` (< 1e-10)

    it "establishment requires positive NPP" $ do
      let out = calcEstablishment EstablishmentInput
            { esi_present = True, esi_prec365 = 0.001
            , esi_tcold = 5.0, esi_twarm = 25.0, esi_gdd = 1000.0
            , esi_tcmax = 20.0, esi_tcmin = -15.0, esi_twmax = 35.0
            , esi_gddmin = 500.0, esi_is_woody = True
            , esi_annsum_npp = 0.0 }
      eso_estab out `shouldBe` False

  -- =====================================================================
  -- Irrigation
  -- =====================================================================
  describe "Irrigation" $ do
    it "irrigation steps per day rounds up" $ do
      calcIrrigNstepsPerDay 14400 1800 `shouldBe` 8
      calcIrrigNstepsPerDay 14400 3600 `shouldBe` 4

    it "no irrigation when not irrigated PFT" $
      pointNeedsCheckForIrrig defaultIrrigationParams 2.0 False 21600 1800
        `shouldBe` False

    it "deficit is zero when soil is wet" $ do
      let layers = V.fromList [IrrigDeficitInput
            { idi_z = 0.1, idi_dz = 0.2, idi_t_soisno = 280.0
            , idi_h2osoi_liq = 100.0, idi_eff_porosity = 0.4
            , idi_relsat_wp = 0.1, idi_relsat_target = 0.5
            , idi_nbedrock = 10 }]
          result = calcDeficit defaultIrrigationParams layers
      idr_deficit result `shouldSatisfy` (<= 1e-6)

  -- =====================================================================
  -- SurfaceRadiation (longwave)
  -- =====================================================================
  describe "SurfaceRadiation (longwave)" $ do
    it "outgoing LW > 0 for T > 0K" $ do
      let r = longwaveRadiation LongwaveInput
            { lwi_forc_lwrad = 300.0, lwi_t_grnd = 260.0
            , lwi_t_veg = 265.0, lwi_emv = 0.97
            , lwi_emg = 0.96, lwi_frac_veg = 0.8 }
      lwr_eflx_lwrad_out r `shouldSatisfy` (> 0.0)

    it "net radiation = fsa - lwnet" $
      abs (netRadiation 200.0 50.0 - 150.0) `shouldSatisfy` (< 1e-10)

  -- =====================================================================
  -- HillslopeHydrology
  -- =====================================================================
  describe "HillslopeHydrology" $ do
    it "stream outflow is zero with no water" $ do
      let scp = StreamChannelProps 1.0 2.0 0.01 100.0 1.0
      sor_volumetric_streamflow (hillslopeStreamOutflow scp 0.0 1800.0)
        `shouldSatisfy` (<= 0.0)

    it "lateral flow is zero when water tables are equal" $ do
      let out = calcLateralSubsurfaceFlow LateralFlowInput
            { lfi_zwt_this = 2.0, lfi_zwt_neighbor = 2.0
            , lfi_distance = 100.0, lfi_width = 10.0
            , lfi_transmissivity = 0.001, lfi_area_this = 1000.0
            , lfi_dt = 1800.0 }
      abs (lfo_qflx_lateral out) `shouldSatisfy` (< 1e-15)

    it "lateral flow goes downhill (from shallow to deep WT)" $ do
      let out = calcLateralSubsurfaceFlow LateralFlowInput
            { lfi_zwt_this = 1.0, lfi_zwt_neighbor = 3.0
            , lfi_distance = 100.0, lfi_width = 10.0
            , lfi_transmissivity = 0.001, lfi_area_this = 1000.0
            , lfi_dt = 1800.0 }
      lfo_qflx_lateral out `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- Aerosol
  -- =====================================================================
  describe "Aerosol" $ do
    it "deposition to snow increases BC mass" $ do
      let dep = defaultAerosolDepFluxes
            { ad_flx_bc_dep_phi = 1.0e-10, ad_flx_bc_dep_pho = 5.0e-11 }
          lyr = defaultAerosolLayer
          lyr' = aerosolDepositionToSnow dep lyr 1800.0
      al_mss_bcphi lyr' `shouldSatisfy` (> 0.0)
      al_mss_bcpho lyr' `shouldSatisfy` (> 0.0)

    it "washout reduces aerosol mass" $ do
      let lyr = defaultAerosolLayer { al_mss_bcphi = 1.0e-6 }
          out = aerosolWashout AerosolWashoutInput
            { awi_layer = lyr, awi_qflx_percolation = 0.001
            , awi_h2osoi_liq = 5.0, awi_h2osoi_ice = 20.0
            , awi_dt = 1800.0
            , awi_scvng_fct_bc = 0.2, awi_scvng_fct_oc = 0.2
            , awi_scvng_fct_dst = 0.02 }
      al_mss_bcphi (awo_layer_updated out) `shouldSatisfy` (< 1.0e-6)

  -- =====================================================================
  -- RootBioPhys
  -- =====================================================================
  describe "RootBioPhys" $ do
    it "root fractions sum to ~1.0" $ do
      let zi = VU.fromList [0.02, 0.06, 0.12, 0.20, 0.32, 0.50, 0.75, 1.04, 1.36, 1.70]
          z  = VU.fromList [0.01, 0.04, 0.09, 0.16, 0.26, 0.41, 0.62, 0.90, 1.20, 1.53]
          dz = VU.fromList [0.02, 0.04, 0.06, 0.08, 0.12, 0.18, 0.25, 0.29, 0.32, 0.34]
          rf = computeRootFr RootFrInput
            { rfi_method = Zeng2001Root, rfi_nlevsoi = 10, rfi_nlevgrnd = 10
            , rfi_nbedrock = 10, rfi_is_fates = False
            , rfi_roota_par = 7.0, rfi_rootb_par = 2.0
            , rfi_rootprof_beta = 0.95
            , rfi_col_zi = zi, rfi_col_z = z, rfi_col_dz = dz }
      abs (VU.sum rf - 1.0) `shouldSatisfy` (< 0.05)

    it "all root fractions are non-negative" $ do
      let zi = VU.fromList [0.02, 0.06, 0.12, 0.20, 0.32]
          z  = VU.fromList [0.01, 0.04, 0.09, 0.16, 0.26]
          dz = VU.fromList [0.02, 0.04, 0.06, 0.08, 0.12]
          rf = computeRootFr RootFrInput
            { rfi_method = Zeng2001Root, rfi_nlevsoi = 5, rfi_nlevgrnd = 5
            , rfi_nbedrock = 5, rfi_is_fates = False
            , rfi_roota_par = 7.0, rfi_rootb_par = 2.0
            , rfi_rootprof_beta = 0.95
            , rfi_col_zi = zi, rfi_col_z = z, rfi_col_dz = dz }
      VU.all (>= 0.0) rf `shouldBe` True

  -- =====================================================================
  -- CNDriver constants
  -- =====================================================================
  -- =====================================================================
  -- SmoothAD
  -- =====================================================================
  describe "SmoothAD" $ do
    it "smoothMin approximates min for well-separated values" $
      abs (smoothMin defaultK 3.0 7.0 - 3.0) `shouldSatisfy` (< 1e-10)

    it "smoothMax approximates max for well-separated values" $
      abs (smoothMax defaultK 3.0 7.0 - 7.0) `shouldSatisfy` (< 1e-10)

    it "smoothMin is symmetric" $
      abs (smoothMin defaultK 5.0 2.0 - smoothMin defaultK 2.0 5.0) `shouldSatisfy` (< 1e-15)

    it "smoothClamp keeps value in bounds" $ do
      abs (smoothClamp defaultK 0.0 1.0 0.5 - 0.5) `shouldSatisfy` (< 1e-10)
      abs (smoothClamp defaultK 0.0 1.0 (-1.0) - 0.0) `shouldSatisfy` (< 1e-10)
      abs (smoothClamp defaultK 0.0 1.0 2.0 - 1.0) `shouldSatisfy` (< 1e-10)

    it "smoothHeaviside is ~0 for negative, ~1 for positive" $ do
      smoothHeaviside defaultK (-5.0) `shouldSatisfy` (< 1e-10)
      abs (smoothHeaviside defaultK 5.0 - 1.0) `shouldSatisfy` (< 1e-10)
      abs (smoothHeaviside defaultK 0.0 - 0.5) `shouldSatisfy` (< 1e-10)

    it "smoothAbs approximates abs for nonzero values" $ do
      abs (smoothAbs defaultK 3.0 - 3.0) `shouldSatisfy` (< 1e-10)
      abs (smoothAbs defaultK (-3.0) - 3.0) `shouldSatisfy` (< 1e-10)

    it "smoothIfElse selects correct branch" $ do
      abs (smoothIfElse defaultK 5.0 10.0 20.0 - 10.0) `shouldSatisfy` (< 1e-10)
      abs (smoothIfElse defaultK (-5.0) 10.0 20.0 - 20.0) `shouldSatisfy` (< 1e-10)

    it "smooth functions are continuous at transition" $ do
      let x1 = smoothMin defaultK 1.0 (1.0 + 1e-8)
          x2 = smoothMin defaultK 1.0 (1.0 - 1e-8)
      abs (x1 - x2) `shouldSatisfy` (< 1e-6)

  -- =====================================================================
  -- Automatic Differentiation
  -- =====================================================================
  describe "AD Sensitivity" $ do
    it "CN timestep produces finite outputs" $ do
      let params = defaultCNParams :: CNParams Double
          state0 = defaultCNState :: CNState Double
          (state', gpp, npp, nee, hr) = cnTimestep params state0 1800.0 200.0 280.0 3.0
      not (isNaN gpp) `shouldBe` True
      not (isNaN nee) `shouldBe` True
      cs_leafc state' `shouldSatisfy` (> 0.0)

    it "Jacobian has correct dimensions (6 outputs x 9 params)" $ do
      let state0 = defaultCNState :: CNState Double
          jac = computeSensitivities state0 1800.0 200.0 280.0 3.0
      length jac `shouldBe` 6
      all (\row -> length row == 9) jac `shouldBe` True

    it "Jacobian entries are finite (no NaN/Inf)" $ do
      let state0 = defaultCNState :: CNState Double
          jac = computeSensitivities state0 1800.0 200.0 280.0 3.0
      all (all (\x -> not (isNaN x) && not (isInfinite x))) jac `shouldBe` True

    it "Jacobian is non-trivial (not all zeros)" $ do
      let state0 = defaultCNState :: CNState Double
          jac = computeSensitivities state0 1800.0 200.0 280.0 3.0
      any (any (/= 0.0)) jac `shouldBe` True

    it "gradient of simple cost is finite and non-zero" $ do
      let state0 = defaultCNState :: CNState Double
          -- Simple cost: squared deviation of NEE from target after one step
          -- Written polymorphically so ad can differentiate through it
          costFn :: (Floating a, Ord a) => [a] -> a
          costFn paramList =
            let params = cnParamsFromList paramList
                st0 = defaultCNState
                (_, _, _, nee, _) = cnTimestep params st0 1800.0 200.0 280.0 3.0
                target = -2.0e-6
            in (nee - target) * (nee - target)
          gradient = computeGradient costFn (defaultCNParams :: CNParams Double)
      length gradient `shouldBe` 9
      all (\x -> not (isNaN x) && not (isInfinite x)) gradient `shouldBe` True
      any (/= 0.0) gradient `shouldBe` True

    it "leaf longevity sensitivity: longer leaf life -> more leaf C" $ do
      let state0 = defaultCNState :: CNState Double
          jac = computeSensitivities state0 1800.0 200.0 280.0 3.0
          dLeafC_dLeafLong = head (head jac)
      dLeafC_dLeafLong `shouldSatisfy` (> 0.0)

    it "Q10 sensitivity is non-zero (soil C responds to Q10)" $ do
      let state0 = defaultCNState :: CNState Double
          jac = computeSensitivities state0 1800.0 200.0 280.0 3.0
          dSoilC_dQ10 = (jac !! 4) !! 3
      abs dSoilC_dQ10 `shouldSatisfy` (> 0.0)

  -- =====================================================================
  -- FluxNET + Calibration end-to-end
  -- =====================================================================
  describe "FluxNET Calibration" $ do
    it "synthetic FluxNET data has correct length" $ do
      let data_ = generateSyntheticFluxnet 96 285.0 500.0
      length data_ `shouldBe` 96

    it "synthetic data has positive GPP during daytime" $ do
      let data_ = generateSyntheticFluxnet 48 285.0 500.0
          dayGPP = [ft_gpp (fts_target ts) | ts <- data_,
                    ff_sw_in (fts_forcing ts) > 10.0,
                    not (isNaN (ft_gpp (fts_target ts)))]
      length dayGPP `shouldSatisfy` (> 0)
      all (> 0.0) dayGPP `shouldBe` True

    it "gradient descent reduces objective on synthetic data" $ do
      let synth = generateSyntheticFluxnet 48 285.0 500.0
          driverPairs = [(ff_sw_in (fts_forcing ts), ff_ta (fts_forcing ts))
                        | ts <- synth]
          obsVals = [ft_nee (fts_target ts) | ts <- synth]

          mkCost :: (Floating a, Ord a) => [a] -> a
          mkCost theta =
            let params = cnParamsFromList theta
                go _st [] _ acc = acc
                go st ((par,tsoil):ds) (o:os) acc =
                  let (st', _, _, nee, _) = cnTimestep params st 1800.0
                        (realToFrac par) (realToFrac tsoil) 3.0
                      err = (nee - realToFrac o) * (nee - realToFrac o)
                  in go st' ds os (acc + err)
                go _ _ _ acc = acc
            in go defaultCNState driverPairs obsVals 0.0

          theta0 = cnParamsToList (defaultCNParams :: CNParams Double)
          f0 = mkCost theta0 :: Double
          gradFn t = grad mkCost t
          result = gradientDescent (mkCost :: [Double] -> Double) gradFn theta0
                     20 1e-12 1e-15 100.0 False

      cr_iterations result `shouldSatisfy` (> 0)
      cr_objective result `shouldSatisfy` (<= f0 * 1.01)

    it "gradient descent trajectory is monotonically decreasing" $ do
      let synth = generateSyntheticFluxnet 24 290.0 400.0
          driverPairs = [(ff_sw_in (fts_forcing ts), ff_ta (fts_forcing ts))
                        | ts <- synth]
          obsVals = [ft_nee (fts_target ts) | ts <- synth]

          mkCost :: (Floating a, Ord a) => [a] -> a
          mkCost theta =
            let params = cnParamsFromList theta
                go _st [] _ acc = acc
                go st ((par,tsoil):ds) (o:os) acc =
                  let (st', _, _, nee, _) = cnTimestep params st 1800.0
                        (realToFrac par) (realToFrac tsoil) 3.0
                      err = (nee - realToFrac o) * (nee - realToFrac o)
                  in go st' ds os (acc + err)
                go _ _ _ acc = acc
            in go defaultCNState driverPairs obsVals 0.0

          theta0 = cnParamsToList (defaultCNParams :: CNParams Double)
          gradFn t = grad mkCost t
          result = gradientDescent (mkCost :: [Double] -> Double) gradFn theta0
                     15 1e-12 1e-15 100.0 False
          objs = map (\(_, o, _) -> o) (cr_trajectory result)

      length objs `shouldSatisfy` (> 1)
      -- Each step should not increase the objective (Armijo condition)
      all (\(a, b) -> b <= a + 1e-10) (zip objs (tail objs)) `shouldBe` True

  -- =====================================================================
  -- NetCDF reading
  -- =====================================================================
  describe "NetCDF reader" $ do
    it "reads Bow at Banff forcing file" $ do
      let ncpath = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped/data/forcing/CLM_input/clmforc.2002.nc"
      result <- ncOpen ncpath
      case result of
        Left err -> pendingWith ("NetCDF open failed: " ++ err)
        Right nc -> do
          -- Read TBOT
          tbot_r <- ncReadDouble1D nc "TBOT"
          case tbot_r of
            Left err -> do
              ncClose nc
              expectationFailure ("TBOT read failed: " ++ err)
            Right tbot -> do
              VU.length tbot `shouldSatisfy` (> 8000)
              let tmin = VU.minimum tbot
                  tmax = VU.maximum tbot
              tmin `shouldSatisfy` (> 200.0)  -- physically reasonable
              tmax `shouldSatisfy` (< 320.0)
              -- Read dimension
              timeLen <- ncDimLen nc "time"
              case timeLen of
                Left _ -> return ()
                Right n -> n `shouldSatisfy` (> 8000)
              ncClose nc

  -- =====================================================================
  -- Bow at Banff site calibration (real data)
  -- =====================================================================
  describe "Bow at Banff CLM calibration" $ do
    let obsPath = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped/data/observations/streamflow/preprocessed/Bow_at_Banff_lumped_streamflow_processed.csv"
        bowSite = SiteConfig
          { sc_name = "Bow_at_Banff"
          , sc_area_km2 = 2210.0
          , sc_lat = 51.36
          , sc_lon = -116.01
          , sc_elev = 1400.0
          , sc_forcing_dt = 3600.0
          , sc_data_dir = "test/data_bow"
          , sc_spinup_days = 0
          }

    it "CLM pipeline produces QRUNOFF on Bow at Banff data" $ do
      hasData <- doesDirectoryExist "test/data_bow/coldstart"
      if not hasData
        then pendingWith "Bow at Banff data not available"
        else do
          qrunoff <- runCLMForQrunoff defaultPipelineConfig
            { pcDataDir = "test/data_bow", pcNdays = 30
            , pcDtime = 3600.0, pcVerbose = False }
          length qrunoff `shouldSatisfy` (> 700)
          all (\q -> not (isNaN q) && not (isInfinite q)) qrunoff `shouldBe` True

    it "reads Bow at Banff observed streamflow" $ do
      hasObs <- doesFileExist obsPath
      if not hasObs
        then pendingWith "Obs file not available"
        else do
          obsQ <- readStreamflowCSV obsPath 2004
          length obsQ `shouldSatisfy` (> 300)
          let meanQ = sum obsQ / fromIntegral (length obsQ)
          meanQ `shouldSatisfy` (> 10.0)  -- mean Q > 10 m3/s

    it "KGE and routing functions work correctly" $ do
      let sim = [10,20,30,20,10,15,25,35,25,15,12,22]
          obs = [12,18,28,22,11,14,24,33,23,14,11,20]
      kge sim obs `shouldSatisfy` (> 0.5)
      let routed = linearReservoirRoute 10.0 1.0 [0,0,100,0,0,0,0,0,0,0]
      length routed `shouldBe` 10
      maximum routed `shouldSatisfy` (< 100.0)

  describe "CNDriver" $ do
    it "decomposition method constants are distinct" $
      centuryDecomp `shouldSatisfy` (/= mimicsDecomp)

  -- =====================================================================
  -- Pipeline integration (vs Julia reference)
  -- =====================================================================
  describe "Pipeline initialization" $ do
    it "seeds patch vegetation temperature from cold-start data" $ do
      hasData <- doesFileExist "test/data/coldstart/t_veg.bin"
      if not hasData
        then pendingWith "cold-start vegetation temperature not available"
        else do
          (st, _, _) <- initCLMStateFromDir "test/data"
          let tVeg = t_veg_patch_vec (clmTemp st)
          VU.length tVeg `shouldSatisfy` (> 0)
          abs (tVeg VU.! 0 - 283.0) `shouldSatisfy` (< 1.0e-12)

  describe "Pipeline integration (vs Julia reference)" $ do
    it "runs 10 days without crashing" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data", pcNdays = 10, pcVerbose = False }
          length dailies `shouldBe` 10

    it "Day 1 T_GRND matches Julia reference within 0.10K" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data", pcNdays = 1, pcVerbose = False }
          abs (dd_t_grnd (head dailies) - 259.806905) `shouldSatisfy` (<= 0.10)

    it "matches Julia daily reference for core trajectory fields" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      hasRef <- doesFileExist "test/data/julia_daily_avg.csv"
      if not hasData
        then pendingWith "test/data not available"
        else if not hasRef
          then pendingWith "Julia daily reference not available"
          else do
            let ndays = 10
            dailies <- runPipeline defaultPipelineConfig
              { pcDataDir = "test/data", pcNdays = ndays, pcVerbose = False }
            refs <- readReferenceCSV "test/data/julia_daily_avg.csv"
            dailyParityFailures refs dailies `shouldBe` []

    it "CN mode: pools remain positive and stable for 10 days" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data", pcNdays = 10, pcVerbose = False
            , pcUseCN = True }
          length dailies `shouldBe` 10
          -- GPP should be non-negative (may be zero in winter)
          all (\d -> dd_gpp d >= 0.0) dailies `shouldBe` True
          -- Leaf C should remain positive (initialized at 200 gC/m2)
          all (\d -> dd_leafc d > 50.0) dailies `shouldBe` True
          -- Soil organic C should remain positive (initialized at 8000 gC/m2)
          all (\d -> dd_soilorgc d > 5000.0) dailies `shouldBe` True
          -- NEE should be finite
          all (\d -> not (isNaN (dd_nee d)) && not (isInfinite (dd_nee d))) dailies `shouldBe` True
          -- Pools should not blow up (< 2x initial)
          all (\d -> dd_leafc d < 400.0 && dd_soilorgc d < 16000.0) dailies `shouldBe` True

    it "T_GRND stays in physical range (200-320K) for 30 days" $ do
      hasData <- doesDirectoryExist "test/data/coldstart"
      if not hasData
        then pendingWith "test/data not available"
        else do
          dailies <- runPipeline defaultPipelineConfig
            { pcDataDir = "test/data", pcNdays = 30, pcVerbose = False }
          all (\t -> t > 200.0 && t < 320.0) (map dd_t_grnd dailies) `shouldBe` True

  -- =====================================================================
  -- Fortran parity (Phase 0 baseline harness) — gated on dump presence
  -- =====================================================================
  describe "Fortran parity (Phase 0 baseline, gated)" $ do
    it "injects before_step, runs one clmDrv step, diffs vs after_hydrologydrainage" $ do
      have <- doesFileExist (dumpPath bgcDumpDir "before_step" (head bgcSteps))
      if not have
        then pendingWith "Fortran reference dumps not present on this machine"
        else do
          _ <- identityReport "test/data"
          diffs <- baselineReport "test/data"
          -- The harness must produce diffs for the mappable registry fields and
          -- stay finite. This is the measurement baseline: per-field PASS/FAIL is
          -- printed above; we do NOT assert parity here (that is Phase 1 work).
          not (null diffs) `shouldBe` True
          all (\fd -> not (isNaN (fdAbs fd)) && not (isInfinite (fdAbs fd))) diffs
            `shouldBe` True
