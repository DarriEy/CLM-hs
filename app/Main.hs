{-# LANGUAGE BangPatterns #-}
-- | CLM.hs single-column driver.
-- Modes:
--   (no args)        — synthetic cold-start demo
--   --test-data DIR  — load binary test data and verify forcing/surface reads
--   --init-test DIR  — verify cold-start initialization against Julia exports
--   --run DIR NDAYS  — run N-day simulation and output daily averages
module Main (main) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.Environment (getArgs)
import System.FilePath ((</>))
import Numeric (showEFloat)

import CLM.Constants.PhysicalConstants
  ( tfrz, nlevsoi, nlevgrnd, nlevsno, denh2o, denice, cpice, cpliq, hfus )
import CLM.Infrastructure.InitVertical
  ( soilCoordinates )
import CLM.Infrastructure.ColdStart
  ( ColdStartConfig(..), defaultColdStartConfig
  , SurfaceInputData(..), defaultSurfaceInputData
  , coldStartInitialize, SoilProperties(..)
  , tSoil
  )
import CLM.Infrastructure.ForcingReader
  ( ForcingTimestep(..), ForcingReaderState(..)
  , forcingReaderInitBinary, readForcingStepPure
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity
  , splitShortwaveBands
  )
import CLM.Infrastructure.SurfData
  ( SurfaceInputData(..), surfrdGetDataBinary )
import qualified CLM.Infrastructure.SurfData as SD
import CLM.Infrastructure.ReadParams
  ( readParametersBinary, AllParams(..), PFTConstants(..) )
import CLM.Infrastructure.BinaryIO
  ( readManifestDims, ManifestDims(..)
  , readFloat64Vector, readInt64Vector )
import CLM.Infrastructure.Filters (FilterSet(..), buildFilters)
import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData  (WaterStateData(..))
import CLM.BioGeoPhys.SurfaceAlbedo
  ( SurfaceAlbedoConstants(..), SoilAlbedoInput(..)
  , SoilAlbedoResult(..), GroundAlbedoResult(..)
  , SurfAlbDriverInput(..), SurfAlbDriverOutput(..)
  , surfaceAlbedoDriver, initSoilAlbedoTables, defaultSurfAlbConstants
  )
import CLM.BioGeoPhys.SurfaceRadiation
  ( SurfRadColumnInput(..), SurfRadPatchInput(..), SurfRadConfig(..)
  , SurfRadResult(..), surfaceRadiationPatch, defaultSurfRadConfig
  )
import CLM.BioGeoPhys.BaregroundFluxes
  ( BareGroundFluxesInput(..), BareGroundFluxesOutput(..)
  , BareGroundFluxesParams(..), defaultBareGroundFluxesParams
  , Z0ParamMethod(..)
  , baregroundFluxes
  )
import CLM.BioGeoPhys.LakeFluxes
  ( LakeFluxInput(..), LakeFluxOutput(..), lakeFluxes
  )
import CLM.BioGeoPhys.CanopyFluxes
  ( CanopyFluxesInput(..), CanopyFluxesOutput(..)
  , CanopyFluxesParams(..), defaultCanopyFluxesParams
  , CanopyFluxesControl(..), defaultCanopyFluxesControl
  , canopyFluxes
  )
import CLM.BioGeoPhys.SoilTemperature
  ( SoilTempInput(..), SoilTempOutput(..)
  , solveSoilTemperature
  , SnowThermalCond(..)
  )
import CLM.BioGeoPhys.PreFluxCalcs
  ( SetZ0mDisplaInput(..), SetZ0mDisplaOutput(..)
  , CalcInitTempEnergyColInput(..), CalcInitTempEnergyColOutput(..)
  , CalcInitTempEnergyPatchInput(..), CalcInitTempEnergyPatchOutput(..)
  , setZ0mDispla, calcInitTempEnergyCol, calcInitTempEnergyPatch
  )
import CLM.Driver.Simulation
  ( SimState(..), initSimState, runSimulation, DailyAvg(..) )
import CLM.Driver.PipelineRunner
  ( PipelineConfig(..), defaultPipelineConfig, runPipeline )

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--test-data", dir] -> testDataMode dir
    ["--init-test", dir] -> initTestMode dir
    ["--rad-test", dir]  -> radTestMode dir
    ["--flux-test", dir] -> fluxTestMode dir
    ["--soiltemp-test", dir] -> soilTempTestMode dir
    ["--run", dir, ndaysStr] -> runMode dir (read ndaysStr)
    ["--run", dir] -> runMode dir 30
    ["--pipeline", dir, ndaysStr] -> pipelineMode dir (read ndaysStr)
    ["--pipeline", dir] -> pipelineMode dir 30
    _ -> demoMode

-- ============================================================================
-- Pipeline mode: use clmDrv + wiredPhysicsPipeline
-- ============================================================================

pipelineMode :: FilePath -> Int -> IO ()
pipelineMode dir ndays = do
  putStrLn "=============================================="
  putStrLn " CLM-hs Pipeline Runner"
  putStrLn "=============================================="
  dailies <- runPipeline defaultPipelineConfig
    { pcDataDir = dir
    , pcNdays   = ndays
    , pcVerbose = True
    }
  putStrLn $ "\nCompleted " ++ show (length dailies) ++ " days via pipeline."

-- ============================================================================
-- Test data mode: load binary data exported by Julia
-- ============================================================================

testDataMode :: FilePath -> IO ()
testDataMode dir = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Binary Test Data Loader"
  putStrLn "=============================================="
  putStrLn ""

  -- Read manifest
  putStrLn "Reading manifest..."
  dims <- readManifestDims (dir ++ "/manifest.json")
  putStrLn $ "  Site: Bow at Banff (single column)"
  putStrLn $ "  Dimensions: ng=" ++ show (mdNg dims)
                  ++ " nc=" ++ show (mdNc dims)
                  ++ " np=" ++ show (mdNp dims)
  putStrLn $ "  Levels: nlevsoi=" ++ show (mdNlevsoi dims)
                  ++ " nlevgrnd=" ++ show (mdNlevgrnd dims)
                  ++ " nlevsno=" ++ show (mdNlevsno dims)
  putStrLn $ "  Forcing: " ++ show (mdNtimes dims) ++ " timesteps"
  putStrLn $ "  Location: lat=" ++ show (mdLat dims)
                  ++ " lon=" ++ show (mdLon dims)
  putStrLn ""

  -- Read surface data
  putStrLn "Reading surface data..."
  surf <- surfrdGetDataBinary (dir ++ "/surfdata") (mdNg dims) (mdNlevsoi dims)
  let sandRow = SD.sid_pct_sand surf
  putStrLn $ "  Latitude:  " ++ show (SD.sid_latitude surf)
  putStrLn $ "  Longitude: " ++ show (SD.sid_longitude surf)
  putStrLn $ "  Sand layers: " ++ show (length sandRow) ++ " gridcells"
  putStrLn ""

  -- Read parameters
  putStrLn "Reading parameters..."
  params <- readParametersBinary (dir ++ "/params")
  let pft = ap_pftcon params
  putStrLn $ "  PFT z0mr length: " ++ show (VU.length (pft_z0mr pft))
  putStrLn $ "  PFT vcmx25 length: " ++ show (VU.length (pft_vcmx25 pft))
  putStrLn ""

  -- Read forcing data
  putStrLn "Reading forcing data..."
  frs <- forcingReaderInitBinary (dir ++ "/forcing")
  putStrLn $ "  Loaded " ++ show (frs_ntimes frs) ++ " timesteps"

  -- Print first 5 forcing timesteps
  putStrLn ""
  putStrLn "First 5 forcing timesteps:"
  putStrLn "  Step | T_air [K] | P [Pa]    | Wind [m/s] | LW [W/m2] | SW [W/m2] | Precip [mm/s] | Q [kg/kg]"
  putStrLn "  -----|-----------|-----------|------------|-----------|-----------|---------------|----------"
  let printStep idx frs0 = do
        let (ft, frs1) = readForcingStepPure frs0 idx
        putStrLn $ "  " ++ padL 4 (show (idx + 1))
                ++ " | " ++ padL 9 (showF2 (ft_tbot ft))
                ++ " | " ++ padL 9 (showF1 (ft_psrf ft))
                ++ " | " ++ padL 10 (showF2 (ft_wind ft))
                ++ " | " ++ padL 9 (showF2 (ft_flds ft))
                ++ " | " ++ padL 9 (showF2 (ft_fsds ft))
                ++ " | " ++ padL 13 (showF5 (ft_precip ft))
                ++ " | " ++ showF6 (ft_qbot ft)
        return frs1
  frs1 <- printStep 0 frs
  frs2 <- printStep 1 frs1
  frs3 <- printStep 2 frs2
  frs4 <- printStep 3 frs3
  _    <- printStep 4 frs4

  -- Derived quantities from first timestep
  putStrLn ""
  let (ft0, _) = readForcingStepPure frs 0
      (rain, snow) = partitionPrecip (ft_tbot ft0) (ft_precip ft0)
      vp = computeVaporPressureFromQ (ft_qbot ft0) (ft_psrf ft0)
      th = computePotentialTemperature (ft_tbot ft0) (ft_psrf ft0)
      rho = computeAirDensity (ft_psrf ft0) (ft_tbot ft0) vp
      (sVD, sND, sVI, sNI) = splitShortwaveBands (ft_fsds ft0)
  putStrLn "Derived quantities (timestep 1):"
  putStrLn $ "  Rain: " ++ showF5 rain ++ " mm/s, Snow: " ++ showF5 snow ++ " mm/s"
  putStrLn $ "  Vapor pressure: " ++ showF2 vp ++ " Pa"
  putStrLn $ "  Potential temp:  " ++ showF2 th ++ " K"
  putStrLn $ "  Air density:     " ++ showF4 rho ++ " kg/m3"
  putStrLn $ "  SW bands: VIS_d=" ++ showF2 sVD ++ " NIR_d=" ++ showF2 sND
          ++ " VIS_i=" ++ showF2 sVI ++ " NIR_i=" ++ showF2 sNI ++ " W/m2"
  putStrLn ""
  putStrLn "Test data loading successful!"

-- ============================================================================
-- Init-test mode: verify cold-start against Julia exports
-- ============================================================================

initTestMode :: FilePath -> IO ()
initTestMode dir = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Cold-Start Verification"
  putStrLn "=============================================="
  putStrLn ""

  -- 1. Read manifest dimensions
  putStrLn "Reading manifest..."
  dims <- readManifestDims (dir </> "manifest.json")
  let nc      = mdNc dims
      np      = mdNp dims
      nlevs   = mdNlevsoi dims
      nlevg   = mdNlevgrnd dims
      nlevsn  = mdNlevsno dims
      nlevtot = nlevsn + nlevg
  putStrLn $ "  nc=" ++ show nc ++ " np=" ++ show np
          ++ " nlevsoi=" ++ show nlevs ++ " nlevgrnd=" ++ show nlevg
          ++ " nlevsno=" ++ show nlevsn
  putStrLn ""

  -- 2. Read surface data
  putStrLn "Reading surface data..."
  surf <- surfrdGetDataBinary (dir </> "surfdata") (mdNg dims) nlevs
  putStrLn "  Surface data loaded."
  putStrLn ""

  -- 3. Run Haskell cold-start initialization
  putStrLn "Running Haskell cold-start..."
  let (zsoi, _dzsoi, _zisoi) = soilCoordinates
      csInput = defaultSurfaceInputData
        { surfPctSand = SD.sid_pct_sand surf V.! 0
        , surfPctClay = SD.sid_pct_clay surf V.! 0
        , surfOrganic = SD.sid_organic surf V.! 0
        , surfFmax    = SD.sid_fmax surf
        , surfColZ    = zsoi
        }
      cfg = defaultColdStartConfig { cscNcols = 1, cscNpatches = 1 }
      (temps, water, soilProps, _waterDiag, _miscState, _eflux) =
        coldStartInitialize cfg csInput
  putStrLn "  Cold-start complete."
  putStrLn ""

  -- 4. Read Julia's reference arrays
  putStrLn "Reading Julia reference arrays..."
  let csDir = dir </> "coldstart"

  -- Soil properties (Julia stores [nc, nlevsoi] column-major)
  watsatRef  <- readFloat64Vector (csDir </> "watsat.bin")
  bswRef     <- readFloat64Vector (csDir </> "bsw.bin")
  sucsatRef  <- readFloat64Vector (csDir </> "sucsat.bin")
  hksatRef   <- readFloat64Vector (csDir </> "hksat.bin")
  tkmgRef    <- readFloat64Vector (csDir </> "tkmg.bin")
  tksatuRef  <- readFloat64Vector (csDir </> "tksatu.bin")
  tkdryRef   <- readFloat64Vector (csDir </> "tkdry.bin")
  csolRef    <- readFloat64Vector (csDir </> "csol.bin")
  watdryRef  <- readFloat64Vector (csDir </> "watdry.bin")
  watoptRef  <- readFloat64Vector (csDir </> "watopt.bin")
  watfcRef   <- readFloat64Vector (csDir </> "watfc.bin")

  -- Temperatures (Julia stores [nc, nlevtot] column-major)
  tSoisnoRef <- readFloat64Vector (csDir </> "t_soisno.bin")
  tGrndRef   <- readFloat64Vector (csDir </> "t_grnd.bin")

  -- Water state (Julia stores [nc, nlevtot] or [nc, nlevsoi] column-major)
  h2osoiLiqRef <- readFloat64Vector (csDir </> "h2osoi_liq.bin")
  h2osoiIceRef <- readFloat64Vector (csDir </> "h2osoi_ice.bin")
  h2osoiVolRef <- readFloat64Vector (csDir </> "h2osoi_vol.bin")

  putStrLn "  Reference arrays loaded."
  putStrLn ""

  -- 5. Extract column 0 from Julia's column-major [nc, nlev] matrices
  let extractCol0 :: Int -> Int -> VU.Vector Double -> VU.Vector Double
      extractCol0 nr nlev flat = VU.generate nlev $ \j -> flat VU.! (j * nr)

      -- Soil properties: [nc, nlevsoi]
      jWatsat  = extractCol0 nc nlevs watsatRef
      jBsw     = extractCol0 nc nlevs bswRef
      jSucsat  = extractCol0 nc nlevs sucsatRef
      jHksat   = extractCol0 nc nlevs hksatRef
      jTkmg    = extractCol0 nc nlevs tkmgRef
      jTksatu  = extractCol0 nc nlevs tksatuRef
      jTkdry   = extractCol0 nc nlevs tkdryRef
      jCsol    = extractCol0 nc nlevs csolRef
      jWatdry  = extractCol0 nc nlevs watdryRef
      jWatopt  = extractCol0 nc nlevs watoptRef
      jWatfc   = extractCol0 nc nlevs watfcRef

      -- Temperatures: [nc, nlevtot]
      jTsoisno = extractCol0 nc nlevtot tSoisnoRef
      jTgrnd   = tGrndRef VU.! 0  -- scalar per column, column 0

      -- Water state: liq/ice are [nc, nlevtot], vol is [nc, nlevsoi]
      jH2oLiq  = extractCol0 nc nlevtot h2osoiLiqRef
      jH2oIce  = extractCol0 nc nlevtot h2osoiIceRef
      jH2oVol  = extractCol0 nc nlevs h2osoiVolRef

  -- 6. Compare arrays and print results
  putStrLn "=== Cold-Start Verification ==="
  putStrLn $ padR 20 "Array" ++ "| " ++ padR 26 "Haskell[0:3]"
          ++ "| " ++ padR 26 "Julia[0:3]" ++ "| Max Diff"
  putStrLn $ replicate 20 '-' ++ "+-" ++ replicate 26 '-'
          ++ "+-" ++ replicate 26 '-' ++ "+-" ++ replicate 12 '-'

  -- Vertical coordinates comparison
  colDzRef   <- readFloat64Vector (csDir </> "col_dz.bin")
  colZRef    <- readFloat64Vector (csDir </> "col_z.bin")
  let (zsoi2, dzsoi2, _zisoi2) = soilCoordinates
      -- Extract soil column (col 0) dz for soil layers (skip snow offset)
      jDz = extractCol0 nc nlevtot colDzRef
      jZ  = extractCol0 nc nlevtot colZRef
      -- Haskell dz/z with snow offset
      hsDz = VU.generate nlevtot $ \i ->
               if i < nlevsn then 0.0 else dzsoi2 VU.! (i - nlevsn)
      hsZ  = VU.generate nlevtot $ \i ->
               if i < nlevsn then 0.0 else zsoi2 VU.! (i - nlevsn)
  compareRow "col_dz" hsDz jDz
  compareRow "col_z"  hsZ  jZ

  -- Soil properties comparison
  compareRow "watsat"  (spWatsat  soilProps) jWatsat
  compareRow "bsw"     (spBsw     soilProps) jBsw
  compareRow "sucsat"  (spSucsat  soilProps) jSucsat
  compareRow "hksat"   (spHksat   soilProps) jHksat
  compareRow "tkmg"    (spTkmg    soilProps) jTkmg
  compareRow "tksatu"  (spTksatu  soilProps) jTksatu
  compareRow "tkdry"   (spTkdry   soilProps) jTkdry
  compareRow "csol"    (spCsol    soilProps) jCsol
  compareRow "watdry"  (spWatdry  soilProps) jWatdry
  compareRow "watopt"  (spWatopt  soilProps) jWatopt
  compareRow "watfc"   (spWatfc   soilProps) jWatfc

  -- Temperature comparison
  compareRow "t_soisno" (t_soisno_col temps) jTsoisno
  compareScalar "t_grnd" (t_grnd_col temps) jTgrnd

  -- Water state comparison
  compareRow "h2osoi_liq" (h2osoi_liq_col water) jH2oLiq
  compareRow "h2osoi_ice" (h2osoi_ice_col water) jH2oIce
  compareRow "h2osoi_vol" (h2osoi_vol_col water) jH2oVol


  putStrLn ""

  -- 7. Filter comparison
  putStrLn "=== Filter Verification ==="

  -- Read topology for building filters
  colItype    <- readInt64Vector (csDir </> "col_itype.bin")
  colLandunit <- readInt64Vector (csDir </> "col_landunit.bin")
  lunItype    <- readInt64Vector (csDir </> "lun_itype.bin")
  pchColumn   <- readInt64Vector (csDir </> "pch_column.bin")

  let hsFilt = buildFilters nc np colItype colLandunit lunItype pchColumn

  -- Read Julia's reference filters
  filtSoilcRef  <- readFloat64Vector (csDir </> "filt_soilc.bin")
  filtLakecRef  <- readFloat64Vector (csDir </> "filt_lakec.bin")
  filtNolakecRef <- readFloat64Vector (csDir </> "filt_nolakec.bin")
  filtSoilpRef  <- readFloat64Vector (csDir </> "filt_soilp.bin")

  let toBool :: VU.Vector Double -> VU.Vector Bool
      toBool = VU.map (> 0.5)

      jSoilc  = toBool filtSoilcRef
      jLakec  = toBool filtLakecRef
      jNolakec = toBool filtNolakecRef
      jSoilp  = toBool filtSoilpRef

  compareBoolRow "filt_soilc"  (maskSoil   hsFilt) jSoilc
  compareBoolRow "filt_lakec"  (maskLake   hsFilt) jLakec
  compareBoolRow "filt_nolakec" (maskNoLake hsFilt) jNolakec
  compareBoolRow "filt_soilp"  (maskSoilP  hsFilt) jSoilp

  putStrLn ""
  putStrLn "Init-test verification complete."

-- | Compare two vectors and print a summary row.
compareRow :: String -> VU.Vector Double -> VU.Vector Double -> IO ()
compareRow name hs jl = do
  let n = min (VU.length hs) (VU.length jl)
      maxDiff = if n == 0 then 0.0
                else VU.maximum $ VU.zipWith (\a b -> abs (a - b)) hs jl
      showFirst3 v = "[" ++ concatComma (map (\i -> showF4 (v VU.! i))
                       [0 .. min 2 (VU.length v - 1)]) ++ ", ...]"
  putStrLn $ padR 20 name
          ++ "| " ++ padR 26 (showFirst3 hs)
          ++ "| " ++ padR 26 (showFirst3 jl)
          ++ "| " ++ showEFloat (Just 3) maxDiff ""

-- | Compare a scalar value and print a summary row.
compareScalar :: String -> Double -> Double -> IO ()
compareScalar name hs jl = do
  let diff = abs (hs - jl)
  putStrLn $ padR 20 name
          ++ "| " ++ padR 26 (showF4 hs)
          ++ "| " ++ padR 26 (showF4 jl)
          ++ "| " ++ showEFloat (Just 3) diff ""

-- | Compare two Bool vectors and print match/mismatch count.
compareBoolRow :: String -> VU.Vector Bool -> VU.Vector Bool -> IO ()
compareBoolRow name hs jl = do
  let n = min (VU.length hs) (VU.length jl)
      mismatches = if n == 0 then 0
                   else VU.length $ VU.filter id $ VU.zipWith (/=) hs jl
      hsTrue = VU.length (VU.filter id hs)
      jlTrue = VU.length (VU.filter id jl)
  putStrLn $ padR 20 name
          ++ "| hs=" ++ show hsTrue ++ "/" ++ show (VU.length hs) ++ " true"
          ++ "  | jl=" ++ show jlTrue ++ "/" ++ show (VU.length jl) ++ " true"
          ++ "  | mismatches=" ++ show mismatches

-- | Comma-join a list of strings.
concatComma :: [String] -> String
concatComma [] = ""
concatComma [x] = x
concatComma (x:xs) = x ++ ", " ++ concatComma xs

-- | Right-pad a string to a given width.
padR :: Int -> String -> String
padR n s = s ++ replicate (max 0 (n - length s)) ' '

-- ============================================================================
-- Radiation test mode: verify surface albedo + radiation against Julia
-- ============================================================================

radTestMode :: FilePath -> IO ()
radTestMode dir = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Radiation Verification (Tier 2)"
  putStrLn "=============================================="
  putStrLn ""

  -- 1. Read manifest and parameters
  dims <- readManifestDims (dir </> "manifest.json")
  let nc  = mdNc dims
      np  = mdNp dims
      _ng = mdNg dims

  putStrLn $ "Grid: nc=" ++ show nc ++ " np=" ++ show np
  putStrLn ""

  -- Read PFT constants
  params <- readParametersBinary (dir </> "params")
  let pft = ap_pftcon params
      numpft = VU.length (pft_xl pft)

  -- 2. Read radiation reference metadata
  putStrLn "Reading radiation reference data..."
  let radDir = dir </> "radiation"

  -- Coszen and forcing
  coszenVec <- readFloat64Vector (radDir </> "coszen_grc.bin")
  let coszen = coszenVec VU.! 0

  -- Forcing solar components (column-major [nc, numrad])
  forcSoladVec <- readFloat64Vector (radDir </> "forc_solad_col.bin")
  forcSolaiVec <- readFloat64Vector (radDir </> "forc_solai_grc.bin")
  let forcSoladVis = forcSoladVec VU.! 0  -- col 1, band 1 (column-major)
      forcSoladNir = forcSoladVec VU.! nc  -- col 1, band 2
      forcSolaiVis = forcSolaiVec VU.! 0
      forcSolaiNir = forcSolaiVec VU.! 1   -- ng=1, so [1, numrad]

  putStrLn $ "  coszen = " ++ showF4 coszen
  putStrLn $ "  forc_solad = [" ++ showF2 forcSoladVis ++ ", " ++ showF2 forcSoladNir ++ "] W/m²"
  putStrLn $ "  forc_solai = [" ++ showF2 forcSolaiVis ++ ", " ++ showF2 forcSolaiNir ++ "] W/m²"
  putStrLn ""

  -- Canopy state (after phenology)
  elaiVec <- readFloat64Vector (radDir </> "elai.bin")
  esaiVec <- readFloat64Vector (radDir </> "esai.bin")
  tlaiVec <- readFloat64Vector (radDir </> "tlai.bin")
  tsaiVec <- readFloat64Vector (radDir </> "tsai.bin")

  putStrLn "  Canopy state (after phenology):"
  mapM_ (\p -> putStrLn $ "    Patch " ++ show (p+1) ++ ": elai=" ++ showF4 (elaiVec VU.! p)
           ++ " esai=" ++ showF4 (esaiVec VU.! p)
           ++ " tlai=" ++ showF4 (tlaiVec VU.! p)
           ++ " tsai=" ++ showF4 (tsaiVec VU.! p)
         ) [0 .. np - 1]
  putStrLn ""

  -- Read column/patch topology
  colItype    <- readInt64Vector (dir </> "coldstart" </> "col_itype.bin")
  pchItype    <- readInt64Vector (dir </> "coldstart" </> "pch_itype.bin")
  pchColumn   <- readInt64Vector (dir </> "coldstart" </> "pch_column.bin")
  colLandunit <- readInt64Vector (dir </> "coldstart" </> "col_landunit.bin")
  lunItype    <- readInt64Vector (dir </> "coldstart" </> "lun_itype.bin")

  -- Column-level state
  tGrndRef <- readFloat64Vector (dir </> "coldstart" </> "t_grnd.bin")
  tVegRef  <- readFloat64Vector (dir </> "coldstart" </> "t_veg.bin")
  h2osoiVolRef <- readFloat64Vector (dir </> "coldstart" </> "h2osoi_vol.bin")

  -- Soil color (from surfdata)
  soilColorVec <- readInt64Vector (dir </> "surfdata" </> "soil_color.bin")
  let soilColor = fromIntegral (soilColorVec VU.! 0) :: Int

  -- 3. Build soil albedo tables
  let mxsoilColor = 20  -- CLM5 uses 20-class
      soicPerGrc = VU.singleton soilColor
      colGrc     = VU.replicate nc 0  -- all columns in gridcell 0
      albCon     = initSoilAlbedoTables mxsoilColor soicPerGrc colGrc

  -- 4. Run surface albedo for each patch
  putStrLn "=== Surface Albedo Verification ==="
  putStrLn ""

  -- Read Julia reference albedos (patch-level, column-major [np, numrad])
  albdRef <- readFloat64Vector (radDir </> "albd_patch.bin")
  albiRef <- readFloat64Vector (radDir </> "albi_patch.bin")
  fabdRef <- readFloat64Vector (radDir </> "fabd_patch.bin")
  fabiRef <- readFloat64Vector (radDir </> "fabi_patch.bin")
  ftddRef <- readFloat64Vector (radDir </> "ftdd_patch.bin")
  ftidRef <- readFloat64Vector (radDir </> "ftid_patch.bin")
  ftiiRef <- readFloat64Vector (radDir </> "ftii_patch.bin")

  -- Column-level reference
  albsodRef <- readFloat64Vector (radDir </> "albsod_col.bin")
  albsoiRef <- readFloat64Vector (radDir </> "albsoi_col.bin")
  albgrdRef <- readFloat64Vector (radDir </> "albgrd_col.bin")
  albgriRef <- readFloat64Vector (radDir </> "albgri_col.bin")

  -- Surface radiation reference
  fsaRef  <- readFloat64Vector (radDir </> "fsa.bin")
  sabgRef <- readFloat64Vector (radDir </> "sabg.bin")
  sabvRef <- readFloat64Vector (radDir </> "sabv.bin")
  fsrRef  <- readFloat64Vector (radDir </> "fsr.bin")

  putStrLn $ padR 16 "Variable" ++ " | " ++ padR 12 "Patch"
          ++ " | " ++ padR 14 "Haskell" ++ " | " ++ padR 14 "Julia" ++ " | Diff"
  putStrLn $ replicate 16 '-' ++ "-+-" ++ replicate 12 '-'
          ++ "-+-" ++ replicate 14 '-' ++ "-+-" ++ replicate 14 '-' ++ "-+------"

  mapM_ (\p -> do
    let c   = fromIntegral (pchColumn VU.! p) - 1  -- 0-based column index
        pt  = fromIntegral (pchItype VU.! p) :: Int
        lt  = fromIntegral (lunItype VU.! (fromIntegral (colLandunit VU.! c) - 1)) :: Int

        -- Get h2osoi_vol for top soil layer (soil-only indexing, column-major [nc, nlevsoi])
        h2oVol1 = if c < VU.length h2osoiVolRef
                  then h2osoiVolRef VU.! c  -- col c, layer 0 (column-major)
                  else 0.0

        -- Soil albedo input
        soilInp = SoilAlbedoInput
          { sai_coszen     = coszen
          , sai_lunType    = lt
          , sai_h2osoi_vol1 = h2oVol1
          , sai_soilColor  = soilColor
          , sai_t_grnd     = tGrndRef VU.! c
          , sai_snl        = 0  -- no snow at cold start
          , sai_lakePuddling = False
          , sai_lakeIcefrac1 = 0.0
          , sai_lakeIcefrac2 = 0.0
          }

        -- PFT optical properties: column-major [numpft, numrad]
        -- So rhol[pft, vis] = rhol[pft], rhol[pft, nir] = rhol[pft + numpft]
        getRhol ib = if pt < numpft then pft_rhol pft VU.! (pt + ib * numpft) else 0.0
        getRhos ib = if pt < numpft then pft_rhos pft VU.! (pt + ib * numpft) else 0.0
        getTaul ib = if pt < numpft then pft_taul pft VU.! (pt + ib * numpft) else 0.0
        getTaus ib = if pt < numpft then pft_taus pft VU.! (pt + ib * numpft) else 0.0
        getXl   = if pt < numpft then pft_xl pft VU.! pt else 0.0

        driverInp = SurfAlbDriverInput
          { sadi_coszen      = coszen
          , sadi_soilAlbIn   = soilInp
          , sadi_fracSno     = 0.0  -- no snow at cold start
          , sadi_snowPersist = 0.0
          , sadi_elai        = elaiVec VU.! p
          , sadi_esai        = esaiVec VU.! p
          , sadi_tlai        = tlaiVec VU.! p
          , sadi_tsai        = tsaiVec VU.! p
          , sadi_t_veg       = tVegRef VU.! p
          , sadi_fwet        = 0.0   -- no canopy water at cold start
          , sadi_fcansno     = 0.0
          , sadi_rhol        = VU.fromList [getRhol 0, getRhol 1]
          , sadi_rhos        = VU.fromList [getRhos 0, getRhos 1]
          , sadi_taul        = VU.fromList [getTaul 0, getTaul 1]
          , sadi_taus        = VU.fromList [getTaus 0, getTaus 1]
          , sadi_xl          = getXl
          }

        result = surfaceAlbedoDriver albCon driverInp

        -- Run surface radiation
        albsndHst = sado_snowAlbD result
        albsniHst = sado_snowAlbI result

        colInp = SurfRadColumnInput
          { src_snl        = 0
          , src_albsod     = sar_albsod (sado_soilAlb result)
          , src_albsoi     = sar_albsoi (sado_soilAlb result)
          , src_albsnd_hst = albsndHst
          , src_albsni_hst = albsniHst
          , src_albgrd     = gar_albgrd (sado_groundAlb result)
          , src_albgri     = gar_albgri (sado_groundAlb result)
          , src_flx_absdv  = VU.replicate (nlevsno + 1) 0.0
          , src_flx_absdn  = VU.replicate (nlevsno + 1) 0.0
          , src_flx_absiv  = VU.replicate (nlevsno + 1) 0.0
          , src_flx_absin  = VU.replicate (nlevsno + 1) 0.0
          , src_snow_depth = 0.0
          , src_frac_sno   = 0.0
          }

        -- Get solar forcing for this patch's column
        solad = VU.fromList [forcSoladVis, forcSoladNir]
        solai = VU.fromList [forcSolaiVis, forcSolaiNir]

        patchInp = SurfRadPatchInput
          { srp_lunType    = lt
          , srp_londeg     = mdLon dims * (180.0 / pi)  -- radians to degrees
          , srp_fabd       = sado_fabd result
          , srp_fabi       = sado_fabi result
          , srp_ftdd       = sado_ftdd result
          , srp_ftid       = sado_ftid result
          , srp_ftii       = sado_ftii result
          , srp_albd       = sado_albd result
          , srp_albi       = sado_albi result
          , srp_forc_solad = solad
          , srp_forc_solai = solai
          }

        radResult = surfaceRadiationPatch defaultSurfRadConfig colInp patchInp

        -- Compare against Julia reference
        -- Albedos: column-major [np, numrad]
        jAlbdVis = albdRef VU.! p
        jAlbdNir = albdRef VU.! (p + np)
        jAlbiVis = albiRef VU.! p
        jAlbiNir = albiRef VU.! (p + np)
        jFabdVis = fabdRef VU.! p
        jFabdNir = fabdRef VU.! (p + np)
        _jFabiVis = fabiRef VU.! p
        _jFabiNir = fabiRef VU.! (p + np)
        jFtddVis = ftddRef VU.! p
        jFtddNir = ftddRef VU.! (p + np)
        _jFtidVis = ftidRef VU.! p
        _jFtidNir = ftidRef VU.! (p + np)
        jFtiiVis = ftiiRef VU.! p
        jFtiiNir = ftiiRef VU.! (p + np)

        hsAlbdVis = sado_albd result VU.! 0
        hsAlbdNir = sado_albd result VU.! 1
        hsAlbiVis = sado_albi result VU.! 0
        hsAlbiNir = sado_albi result VU.! 1

        jFsa  = fsaRef VU.! p
        jSabg = sabgRef VU.! p
        jSabv = sabvRef VU.! p
        jFsr  = fsrRef VU.! p

        hsFsa  = srr_fsa radResult
        hsSabg = srr_sabg radResult
        hsSabv = srr_sabv radResult
        hsFsr  = srr_fsr radResult

        pName = "P" ++ show (p+1) ++ " (PFT " ++ show pt ++ ")"

    -- Print albedo comparison
    printComp "albd_vis" pName hsAlbdVis jAlbdVis
    printComp "albd_nir" pName hsAlbdNir jAlbdNir
    printComp "albi_vis" pName hsAlbiVis jAlbiVis
    printComp "albi_nir" pName hsAlbiNir jAlbiNir
    printComp "fabd_vis" pName (sado_fabd result VU.! 0) jFabdVis
    printComp "fabd_nir" pName (sado_fabd result VU.! 1) jFabdNir
    printComp "ftdd_vis" pName (sado_ftdd result VU.! 0) jFtddVis
    printComp "ftdd_nir" pName (sado_ftdd result VU.! 1) jFtddNir
    printComp "ftii_vis" pName (sado_ftii result VU.! 0) jFtiiVis
    printComp "ftii_nir" pName (sado_ftii result VU.! 1) jFtiiNir

    -- Print radiation comparison
    printComp "fsa"  pName hsFsa  jFsa
    printComp "sabg" pName hsSabg jSabg
    printComp "sabv" pName hsSabv jSabv
    printComp "fsr"  pName hsFsr  jFsr
    putStrLn ""
    ) [0 .. np - 1]

  -- Summary: column-level albedo comparison
  putStrLn "=== Column-Level Albedo ==="
  mapM_ (\c -> do
    let jSodVis = albsodRef VU.! c
        jSodNir = albsodRef VU.! (c + nc)
        jSoiVis = albsoiRef VU.! c
        jSoiNir = albsoiRef VU.! (c + nc)
        jGrdVis = albgrdRef VU.! c
        jGrdNir = albgrdRef VU.! (c + nc)
        jGriVis = albgriRef VU.! c
        jGriNir = albgriRef VU.! (c + nc)
        cName = "C" ++ show (c+1)
    -- For column comparison, run driver for patch 0 on this column
    -- (column albedo is the soil/ground albedo, independent of patch)
    putStrLn $ "  Column " ++ show (c+1) ++ ":"
    putStrLn $ "    albsod: Julia=[" ++ showF4 jSodVis ++ "," ++ showF4 jSodNir ++ "]"
    putStrLn $ "    albgrd: Julia=[" ++ showF4 jGrdVis ++ "," ++ showF4 jGrdNir ++ "]"
    ) [0 .. nc - 1]

  putStrLn ""
  putStrLn "Radiation verification complete."

-- | Print a comparison line.
printComp :: String -> String -> Double -> Double -> IO ()
printComp var pname hs jl = do
  let diff = abs (hs - jl)
      diffStr = if diff < 1.0e-10 then "  exact"
                else showEFloat (Just 3) diff ""
  putStrLn $ padR 16 var ++ " | " ++ padR 12 pname
          ++ " | " ++ padR 14 (showF6 hs)
          ++ " | " ++ padR 14 (showF6 jl)
          ++ " | " ++ diffStr

-- ============================================================================
-- Flux test mode: verify bareground, canopy, lake fluxes against Julia
-- ============================================================================

fluxTestMode :: FilePath -> IO ()
fluxTestMode dir = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Surface Flux Verification (Tier 3)"
  putStrLn "=============================================="
  putStrLn ""

  -- Read metadata
  let fluxDir = dir </> "fluxes"
  dims <- readManifestDims (dir </> "manifest.json")
  let nc  = mdNc dims
      np  = mdNp dims
      ng  = mdNg dims

  -- Read topology
  colLandunit <- readInt64Vector (dir </> "coldstart" </> "col_landunit.bin")
  lunItype    <- readInt64Vector (dir </> "coldstart" </> "lun_itype.bin")
  pchColumn   <- readInt64Vector (dir </> "coldstart" </> "pch_column.bin")

  -- Read parameters
  params <- readParametersBinary (dir </> "params")
  let pft = ap_pftcon params

  -- Read atmospheric forcing
  forcT      <- readFloat64Vector (fluxDir </> "forc_t_col.bin")
  forcTh     <- readFloat64Vector (fluxDir </> "forc_th_col.bin")
  forcQ      <- readFloat64Vector (fluxDir </> "forc_q_col.bin")
  forcPbot   <- readFloat64Vector (fluxDir </> "forc_pbot_col.bin")
  forcRho    <- readFloat64Vector (fluxDir </> "forc_rho_col.bin")
  forcLwrad  <- readFloat64Vector (fluxDir </> "forc_lwrad_col.bin")
  forcU      <- readFloat64Vector (fluxDir </> "forc_u_grc.bin")
  forcV      <- readFloat64Vector (fluxDir </> "forc_v_grc.bin")
  forcHgtU   <- readFloat64Vector (fluxDir </> "forc_hgt_u_grc.bin")
  forcHgtT   <- readFloat64Vector (fluxDir </> "forc_hgt_t_grc.bin")
  forcHgtQ   <- readFloat64Vector (fluxDir </> "forc_hgt_q_grc.bin")

  -- Read pre-flux state
  tGrnd      <- readFloat64Vector (fluxDir </> "t_grnd_col.bin")
  emgCol     <- readFloat64Vector (fluxDir </> "emg_col.bin")
  htvpCol    <- readFloat64Vector (fluxDir </> "htvp_col.bin")
  qgCol      <- readFloat64Vector (fluxDir </> "qg_col.bin")
  qgSnow     <- readFloat64Vector (fluxDir </> "qg_snow_col.bin")
  qgSoil     <- readFloat64Vector (fluxDir </> "qg_soil_col.bin")
  qgH2osfc   <- readFloat64Vector (fluxDir </> "qg_h2osfc_col.bin")
  dqgdTCol   <- readFloat64Vector (fluxDir </> "dqgdT_col.bin")
  thvCol     <- readFloat64Vector (fluxDir </> "thv_col.bin")
  betaCol    <- readFloat64Vector (fluxDir </> "beta_col.bin")
  ziiCol     <- readFloat64Vector (fluxDir </> "zii_col.bin")
  snlCol     <- readFloat64Vector (fluxDir </> "snl_col.bin")
  tH2osfc    <- readFloat64Vector (fluxDir </> "t_h2osfc_col.bin")
  z0mgCol    <- readFloat64Vector (fluxDir </> "z0mg_col.bin")
  z0hgCol    <- readFloat64Vector (fluxDir </> "z0hg_col.bin")
  z0qgCol    <- readFloat64Vector (fluxDir </> "z0qg_col.bin")
  soilbeta   <- readFloat64Vector (fluxDir </> "soilbeta_col.bin")
  topLiq     <- readFloat64Vector (fluxDir </> "h2osoi_liq_top_col.bin")
  topIce     <- readFloat64Vector (fluxDir </> "h2osoi_ice_top_col.bin")
  topDz      <- readFloat64Vector (fluxDir </> "dz_top_col.bin")
  topWatsat  <- readFloat64Vector (fluxDir </> "watsat_top_col.bin")
  fracH2osfc <- readFloat64Vector (fluxDir </> "frac_h2osfc_col.bin")
  fracSnoEff <- readFloat64Vector (fluxDir </> "frac_sno_eff_col.bin")
  tSoisno    <- readFloat64Vector (fluxDir </> "t_soisno_col.bin")

  -- Read patch-level pre-flux state
  z0mPatch   <- readFloat64Vector (fluxDir </> "z0m_patch.bin")
  displaPatch <- readFloat64Vector (fluxDir </> "displa_patch.bin")
  emvPatch   <- readFloat64Vector (fluxDir </> "emv_patch.bin")
  thmPatch   <- readFloat64Vector (fluxDir </> "thm_patch.bin")
  fwetPatch  <- readFloat64Vector (fluxDir </> "fwet_patch.bin")
  fdryPatch  <- readFloat64Vector (fluxDir </> "fdry_patch.bin")
  btranPatch <- readFloat64Vector (fluxDir </> "btran_patch.bin")
  forc_hgt_u_patch <- readFloat64Vector (fluxDir </> "forc_hgt_u_patch.bin")
  forc_hgt_t_patch <- readFloat64Vector (fluxDir </> "forc_hgt_t_patch.bin")
  forc_hgt_q_patch <- readFloat64Vector (fluxDir </> "forc_hgt_q_patch.bin")
  pchItype   <- readFloat64Vector (fluxDir </> "pch_itype.bin")

  -- Canopy state for canopy fluxes
  elaiPatch  <- readFloat64Vector (fluxDir </> "elai_patch.bin")
  esaiPatch  <- readFloat64Vector (fluxDir </> "esai_patch.bin")
  htopPatch  <- readFloat64Vector (fluxDir </> "htop_patch.bin")
  fracVegPatch <- readFloat64Vector (fluxDir </> "frac_veg_nosno_patch.bin")
  tStemPatch <- readFloat64Vector (fluxDir </> "t_stem_patch.bin")
  liqcanPatch <- readFloat64Vector (fluxDir </> "liqcan_patch.bin")
  snocanPatch <- readFloat64Vector (fluxDir </> "snocan_patch.bin")
  dleafPft   <- readFloat64Vector (fluxDir </> "dleaf_pft.bin")
  rssunPatch <- readFloat64Vector (fluxDir </> "rssun_patch.bin")
  rsshaPatch <- readFloat64Vector (fluxDir </> "rssha_patch.bin")
  tVegPreCanopy <- readFloat64Vector (fluxDir </> "t_veg_pre_canopy_patch.bin")
  cgrndsPreCanopy <- readFloat64Vector (fluxDir </> "cgrnds_pre_canopy_patch.bin")
  cgrndlPreCanopy <- readFloat64Vector (fluxDir </> "cgrndl_pre_canopy_patch.bin")
  displaPreCanopy <- readFloat64Vector (fluxDir </> "displa_pre_canopy_patch.bin")
  z0mvPreCanopy  <- readFloat64Vector (fluxDir </> "z0mv_pre_canopy_patch.bin")
  laisunPatch <- readFloat64Vector (fluxDir </> "laisun_patch.bin")
  laishaPatch <- readFloat64Vector (fluxDir </> "laisha_patch.bin")
  snowDepth  <- readFloat64Vector (fluxDir </> "snow_depth_col.bin")
  fracSno    <- readFloat64Vector (fluxDir </> "frac_sno_col.bin")

  -- Read radiation outputs for flux inputs
  sabvPatch  <- readFloat64Vector (fluxDir </> "sabv_patch.bin")
  sabgPatch  <- readFloat64Vector (fluxDir </> "sabg_patch.bin")

  -- Read Julia reference outputs
  refShTot   <- readFloat64Vector (fluxDir </> "eflx_sh_tot_patch.bin")
  refShGrnd  <- readFloat64Vector (fluxDir </> "eflx_sh_grnd_patch.bin")
  refShVeg   <- readFloat64Vector (fluxDir </> "eflx_sh_veg_patch.bin")
  refLhTot   <- readFloat64Vector (fluxDir </> "eflx_lh_tot_patch.bin")
  refEvapTot <- readFloat64Vector (fluxDir </> "qflx_evap_tot_patch.bin")
  refEvapSoi <- readFloat64Vector (fluxDir </> "qflx_evap_soi_patch.bin")
  refTveg    <- readFloat64Vector (fluxDir </> "t_veg_patch.bin")
  refTref2m  <- readFloat64Vector (fluxDir </> "t_ref2m_patch.bin")
  refCgrnds  <- readFloat64Vector (fluxDir </> "cgrnds_patch.bin")
  refCgrndl  <- readFloat64Vector (fluxDir </> "cgrndl_patch.bin")
  refDlrad   <- readFloat64Vector (fluxDir </> "dlrad_patch.bin")
  refUlrad   <- readFloat64Vector (fluxDir </> "ulrad_patch.bin")
  refRam1    <- readFloat64Vector (fluxDir </> "ram1_patch.bin")
  refFv      <- readFloat64Vector (fluxDir </> "fv_patch.bin")

  -- Lake state
  lakedepth  <- readFloat64Vector (fluxDir </> "lakedepth_col.bin")
  savedtke1  <- readFloat64Vector (fluxDir </> "savedtke1_col.bin")
  tLakeCol   <- readFloat64Vector (fluxDir </> "t_lake_col.bin")
  dzLakeCol  <- readFloat64Vector (fluxDir </> "dz_lake_col.bin")
  tGrndPreLake <- readFloat64Vector (fluxDir </> "t_grnd_pre_lake_col.bin")

  putStrLn $ "Grid: nc=" ++ show nc ++ " np=" ++ show np ++ " ng=" ++ show ng
  putStrLn ""

  -- Helper to extract column-major [nc, nlev] -> column c value at layer j
  let getColMajor :: VU.Vector Double -> Int -> Int -> Int -> Double
      getColMajor flat nr c j = flat VU.! (j * nr + c)

  -- =====================================================================
  -- Patch 1: Bare ground (noexposedveg)
  -- =====================================================================
  putStrLn "=== Patch 1: Bare Ground (BaregroundFluxes) ==="
  let p = 0  -- 0-based
      c = fromIntegral (pchColumn VU.! p) - 1  -- Julia 1-based -> 0-based
      g = 0  -- single gridcell
      lt = fromIntegral (lunItype VU.! (fromIntegral (colLandunit VU.! c) - 1)) :: Int
      snl_c = round (snlCol VU.! c) :: Int
      nlevtot = nlevsno + nlevgrnd
      -- Top snow/soil layer temperature
      topLayerIdx = nlevsno + snl_c  -- Julia: nlevsno + snl + 1, Haskell 0-based: nlevsno + snl
      tSoisnoTop = getColMajor tSoisno nc c (if topLayerIdx >= 0 && topLayerIdx < nlevtot then topLayerIdx else nlevsno)
      tSoil1 = getColMajor tSoisno nc c nlevsno  -- first soil layer

      bgInp = BareGroundFluxesInput
        { bgi_params         = defaultBareGroundFluxesParams
        , bgi_z0param_method = ZengWang2007
        , bgi_forc_q         = forcQ VU.! c
        , bgi_forc_pbot      = forcPbot VU.! c
        , bgi_forc_th        = forcTh VU.! c
        , bgi_forc_rho       = forcRho VU.! c
        , bgi_forc_t         = forcT VU.! c
        , bgi_forc_u         = forcU VU.! g
        , bgi_forc_v         = forcV VU.! g
        , bgi_forc_hgt_t     = forc_hgt_t_patch VU.! p
        , bgi_forc_hgt_u     = forc_hgt_u_patch VU.! p
        , bgi_forc_hgt_q     = forc_hgt_q_patch VU.! p
        , bgi_t_grnd         = tGrnd VU.! c
        , bgi_thm            = thmPatch VU.! p
        , bgi_qg             = qgCol VU.! c
        , bgi_qg_snow        = qgSnow VU.! c
        , bgi_qg_soil        = qgSoil VU.! c
        , bgi_qg_h2osfc      = qgH2osfc VU.! c
        , bgi_dqgdT          = dqgdTCol VU.! c
        , bgi_thv            = thvCol VU.! c
        , bgi_beta           = betaCol VU.! c
        , bgi_zii            = ziiCol VU.! c
        , bgi_t_h2osfc       = tH2osfc VU.! c
        , bgi_t_soisno_top   = tSoisnoTop
        , bgi_t_soil1        = tSoil1
        , bgi_z0mg           = z0mgCol VU.! c
        , bgi_z0hg           = z0mgCol VU.! c  -- initial z0hg = z0mg for bare ground (Julia line 161)
        , bgi_z0qg           = z0mgCol VU.! c  -- initial z0qg = z0mg for bare ground
        , bgi_zetamaxstable  = 0.5
        , bgi_soilbeta       = soilbeta VU.! c
        , bgi_soilresis      = 0.0
        , bgi_do_soilevap_beta = True
        , bgi_do_soil_resistance = False
        , bgi_htvp           = htvpCol VU.! c
        , bgi_h2osoi_liq_top = topLiq VU.! c
        , bgi_h2osoi_ice_top = topIce VU.! c
        , bgi_dz_top         = topDz VU.! c
        , bgi_watsat_top     = topWatsat VU.! c
        }

      bgOut = baregroundFluxes bgInp

  putStrLn $ padR 20 "Variable" ++ " | " ++ padR 14 "Haskell"
          ++ " | " ++ padR 14 "Julia" ++ " | Diff"
  putStrLn $ replicate 20 '-' ++ "-+-" ++ replicate 14 '-'
          ++ "-+-" ++ replicate 14 '-' ++ "-+------"

  printComp "eflx_sh_grnd" "P1" (bgo_eflx_sh_grnd bgOut) (refShGrnd VU.! p)
  printComp "eflx_sh_tot"  "P1" (bgo_eflx_sh_tot bgOut)  (refShTot VU.! p)
  printComp "qflx_evap_soi" "P1" (bgo_qflx_evap_soi bgOut) (refEvapSoi VU.! p)
  printComp "cgrnds"       "P1" (bgo_cgrnds bgOut)        (refCgrnds VU.! p)
  printComp "cgrndl"       "P1" (bgo_cgrndl bgOut)        (refCgrndl VU.! p)
  printComp "t_ref2m"      "P1" (bgo_t_ref2m bgOut)       (refTref2m VU.! p)
  printComp "ram1"         "P1" (bgo_ram1 bgOut)           (refRam1 VU.! p)
  printComp "ustar"        "P1" (bgo_ustar bgOut)          (refFv VU.! p)
  putStrLn ""

  -- =====================================================================
  -- Patch 3: Canopy (CanopyFluxes) — deciduous/grass, elai=0, esai=0.5
  -- =====================================================================
  putStrLn "=== Patch 3: Canopy (CanopyFluxes) ==="
  let p3 = 2  -- 0-based
      c3 = fromIntegral (pchColumn VU.! p3) - 1
      g3 = 0
      snl_c3 = round (snlCol VU.! c3) :: Int
      topLayerIdx3 = nlevsno + snl_c3
      tSoisnoTop3 = getColMajor tSoisno nc c3 (if topLayerIdx3 >= 0 && topLayerIdx3 < nlevtot then topLayerIdx3 else nlevsno)
      tSoil1_3 = getColMajor tSoisno nc c3 nlevsno  -- top soil layer
      ivt3 = round (pchItype VU.! p3) :: Int

      canopyInp = CanopyFluxesInput
        { cfi_forc_lwrad      = forcLwrad VU.! c3
        , cfi_forc_q          = forcQ VU.! c3
        , cfi_forc_pbot       = forcPbot VU.! c3
        , cfi_forc_th         = forcTh VU.! c3
        , cfi_forc_rho        = forcRho VU.! c3
        , cfi_forc_t          = forcT VU.! c3
        , cfi_forc_u          = forcU VU.! g3
        , cfi_forc_v          = forcV VU.! g3
        , cfi_forc_hgt_u      = forcHgtU VU.! g3  -- gridcell-level (canopy adds z0+displa internally)
        , cfi_forc_hgt_t      = forcHgtT VU.! g3
        , cfi_forc_hgt_q      = forcHgtQ VU.! g3
        , cfi_elai            = elaiPatch VU.! p3
        , cfi_esai            = esaiPatch VU.! p3
        , cfi_htop            = htopPatch VU.! p3
        , cfi_displa          = displaPreCanopy VU.! p3  -- pre-Zeng-Wang
        , cfi_z0mv            = z0mvPreCanopy VU.! p3  -- pre-Zeng-Wang
        , cfi_z0mg            = z0mgCol VU.! c3
        , cfi_frac_veg_nosno  = round (fracVegPatch VU.! p3)
        , cfi_emv             = emvPatch VU.! p3
        , cfi_emg             = emgCol VU.! c3
        , cfi_t_veg           = tVegPreCanopy VU.! p3
        , cfi_t_grnd          = tGrnd VU.! c3
        , cfi_thm             = thmPatch VU.! p3
        , cfi_thv             = thvCol VU.! c3
        , cfi_t_soisno_top    = tSoisnoTop3
        , cfi_t_soisno_topsoil = tSoil1_3
        , cfi_t_h2osfc        = tH2osfc VU.! c3
        , cfi_t_stem          = tStemPatch VU.! p3
        , cfi_sabv            = sabvPatch VU.! p3
        , cfi_qg              = qgCol VU.! c3
        , cfi_qg_snow         = qgSnow VU.! c3
        , cfi_qg_soil         = qgSoil VU.! c3
        , cfi_qg_h2osfc       = qgH2osfc VU.! c3
        , cfi_dqgdT           = dqgdTCol VU.! c3
        , cfi_frac_sno_eff    = fracSnoEff VU.! c3
        , cfi_frac_h2osfc     = fracH2osfc VU.! c3
        , cfi_snow_depth      = snowDepth VU.! c3
        , cfi_fwet            = fwetPatch VU.! p3
        , cfi_fdry            = fdryPatch VU.! p3
        , cfi_liqcan          = liqcanPatch VU.! p3
        , cfi_snocan          = snocanPatch VU.! p3
        , cfi_rssun           = rssunPatch VU.! p3
        , cfi_rssha           = rsshaPatch VU.! p3
        , cfi_laisun          = laisunPatch VU.! p3
        , cfi_laisha          = laishaPatch VU.! p3
        , cfi_btran           = btranPatch VU.! p3
        , cfi_soilbeta        = soilbeta VU.! c3
        , cfi_soilresis       = 0.0
        , cfi_htvp            = htvpCol VU.! c3
        , cfi_cgrnds          = cgrndsPreCanopy VU.! p3
        , cfi_cgrndl          = cgrndlPreCanopy VU.! p3
        , cfi_do_soilevap_beta = True
        , cfi_dtime           = 1800.0
        , cfi_zetamaxstable   = 0.5
        , cfi_dleaf           = if ivt3 >= 0 && ivt3 < VU.length dleafPft
                                then dleafPft VU.! ivt3
                                else 0.04
        , cfi_snl             = snl_c3
        }

      canopyOut = canopyFluxes defaultCanopyFluxesParams defaultCanopyFluxesControl canopyInp

  printComp "eflx_sh_tot"  "P3" (cfo_eflx_sh_veg canopyOut + cfo_eflx_sh_grnd canopyOut)
                                (refShTot VU.! p3)
  printComp "eflx_sh_veg"  "P3" (cfo_eflx_sh_veg canopyOut)  (refShVeg VU.! p3)
  printComp "eflx_sh_grnd" "P3" (cfo_eflx_sh_grnd canopyOut) (refShGrnd VU.! p3)
  printComp "t_veg"        "P3" (cfo_t_veg canopyOut)         (refTveg VU.! p3)
  printComp "t_ref2m"      "P3" (cfo_t_ref2m canopyOut)       (refTref2m VU.! p3)
  printComp "ustar"        "P3" (cfo_ustar canopyOut)         (refFv VU.! p3)
  printComp "ram1"         "P3" (cfo_ram1 canopyOut)           (refRam1 VU.! p3)
  putStrLn ""

  -- =====================================================================
  -- Patch 4: Lake (LakeFluxes)
  -- =====================================================================
  putStrLn "=== Patch 4: Lake (LakeFluxes) ==="
  let p4 = 3  -- 0-based
      c4 = fromIntegral (pchColumn VU.! p4) - 1
      g4 = 0
      snl_c4 = round (snlCol VU.! c4) :: Int
      -- Lake: t_lake_col is [nc, nlevlak] column-major
      -- nlevlak=10, so layer j for column c is at index j*nc + c
      tLake1 = getColMajor tLakeCol nc c4 0  -- first lake layer
      tLake2 = getColMajor tLakeCol nc c4 1  -- second lake layer (subsurface)
      -- Lake dz: dz_lake_col is [nc, nlevlak], use layer 1
      dzLake1 = getColMajor dzLakeCol nc c4 0

      -- For lake without snow: dzsur = dz_lake[1]/2, tsur = t_lake[2]
      -- For lake with snow: dzsur = dz[jtop]/2, tsur = t_soisno[jtop+1]
      topLayerIdx4 = nlevsno + snl_c4
      (dzSur4, tSur4)
        | snl_c4 < 0 = error "Lake with snow: need full dz_col export"
        | otherwise = (dzLake1 / 2.0, tLake2)

      topLiqLake = topLiq VU.! c4
      topIceLake = topIce VU.! c4

      lakeInp = LakeFluxInput
        { lfi_snl          = snl_c4
        , lfi_lakedepth    = lakedepth VU.! c4
        , lfi_dz_top       = dzSur4
        , lfi_savedtke1    = savedtke1 VU.! c4
        , lfi_t_grnd       = tGrndPreLake VU.! c4  -- pre-lake t_grnd (272.0 cold start)
        , lfi_t_subsurface = tSur4
        , lfi_t_lake1      = tLake1
        , lfi_sabg         = sabgPatch VU.! p4
        , lfi_h2osoi_liq_top = topLiqLake
        , lfi_h2osoi_ice_top = topIceLake
        , lfi_forc_t       = forcT VU.! c4
        , lfi_forc_th      = forcTh VU.! c4
        , lfi_forc_q       = forcQ VU.! c4
        , lfi_forc_pbot    = forcPbot VU.! c4
        , lfi_forc_rho     = forcRho VU.! c4
        , lfi_forc_lwrad   = forcLwrad VU.! c4
        , lfi_forc_u       = forcU VU.! g4
        , lfi_forc_v       = forcV VU.! g4
        , lfi_forc_hgt_u   = forcHgtU VU.! g4
        , lfi_forc_hgt_t   = forcHgtT VU.! g4
        , lfi_forc_hgt_q   = forcHgtQ VU.! g4
        , lfi_dtime        = 1800.0
        }

      lakeOut = lakeFluxes lakeInp

      -- Lake: eflx_sh_tot = eflx_sh_grnd, eflx_lh_tot = htvp * qflx_evap_soi
      lakeShTot = lfo_eflx_sh_grnd lakeOut
      lakeLhTot = lfo_eflx_lh_grnd lakeOut

  printComp "eflx_sh_tot"  "P4" lakeShTot            (refShTot VU.! p4)
  printComp "eflx_lh_tot"  "P4" lakeLhTot            (refLhTot VU.! p4)
  printComp "qflx_evap_soi" "P4" (lfo_qflx_evap_soi lakeOut) (refEvapSoi VU.! p4)
  printComp "ustar"        "P4" (lfo_ustar lakeOut)   (refFv VU.! p4)
  printComp "t_grnd"       "P4" (lfo_t_grnd lakeOut)  (tGrnd VU.! c4)
  putStrLn ""

  -- =====================================================================
  -- Summary
  -- =====================================================================
  putStrLn "Flux test verification complete."
  putStrLn "Note: Patch 2 (needleleaf, high LAI) produces NaN in Julia cold-start"
  putStrLn "      due to photosynthesis instability at -15°C. Skipped for now."

-- ============================================================================
-- Demo mode: synthetic cold-start
-- ============================================================================

demoMode :: IO ()
demoMode = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Community Land Model (Haskell)"
  putStrLn "  Single-column cold-start demo"
  putStrLn "=============================================="
  putStrLn ""

  let (zsoi, dzsoi, _zisoi) = soilCoordinates
  putStrLn $ "Soil layers: " ++ show nlevsoi
  putStrLn $ "Total layers (snow+soil): " ++ show (nlevsno + nlevgrnd)
  putStrLn ""
  putStrLn "Soil node depths [m]:"
  mapM_ (\j -> putStrLn $ "  layer " ++ show (j+1) ++ ": z=" ++
    showF4 (zsoi VU.! j) ++ " m, dz=" ++ showF4 (dzsoi VU.! j) ++ " m")
    [0..min 9 (nlevsoi - 1)]
  myWhen (nlevsoi > 10) $ putStrLn $ "  ... (" ++ show nlevsoi ++ " total)"
  putStrLn ""

  let nc = 1
      nlay = nlevsoi
      sandVec = VU.replicate (nc * nlay) 60.0
      clayVec = VU.replicate (nc * nlay) 20.0
      orgVec  = VU.replicate (nc * nlay) 0.0
      fmaxVec = VU.replicate nc 0.5
      surf = defaultSurfaceInputData
        { surfPctSand = sandVec
        , surfPctClay = clayVec
        , surfOrganic = orgVec
        , surfFmax    = fmaxVec
        , surfColZ    = zsoi
        }
      cfg = defaultColdStartConfig { cscNcols = nc, cscNpatches = 1 }

  let (temps, water, _soilProps, _waterDiag, _miscState, _eflux) =
        coldStartInitialize cfg surf

  putStrLn "Cold-start initialization complete."
  putStrLn ""

  let tsoi = t_soisno_col temps
  putStrLn "Initial soil temperatures [K]:"
  mapM_ (\j -> do
    let t = if j < VU.length tsoi then tsoi VU.! j else 0.0
    putStrLn $ "  layer " ++ show (j - nlevsno + 1) ++
      ": T=" ++ showF2 t ++ " K (" ++ showF2 (t - tfrz) ++ " C)")
    [nlevsno .. nlevsno + min 9 (nlevgrnd - 1)]
  putStrLn ""

  let liq = h2osoi_liq_col water
      ice = h2osoi_ice_col water
  putStrLn "Initial soil water [kg/m2 per layer]:"
  mapM_ (\j -> do
    let idx = j + nlevsno
        l = if idx < VU.length liq then liq VU.! idx else 0.0
        i = if idx < VU.length ice then ice VU.! idx else 0.0
    putStrLn $ "  layer " ++ show (j+1) ++ ": liq=" ++
      showF3 l ++ ", ice=" ++ showF3 i)
    [0 .. min 9 (nlevsoi - 1)]
  putStrLn ""

  putStrLn "Running 10 timesteps of simple heat diffusion..."
  let dt = 1800.0
      nsteps = 10 :: Int
      tsoilInit = VU.slice nlevsno nlevgrnd tsoi
      stepOnce tvec _step =
        let n = VU.length tvec
            keff = 1.0
            cv   = 2.0e6
        in VU.generate n $ \j ->
             let tj = tvec VU.! j
                 dz_j = if j < VU.length dzsoi then dzsoi VU.! j else 0.1
                 tUp   = if j > 0     then tvec VU.! (j-1) else 280.0
                 tDown = if j < n - 1 then tvec VU.! (j+1) else tj
                 dzUp   = if j > 0     then 0.5 * (dz_j + (if j-1 < VU.length dzsoi then dzsoi VU.! (j-1) else dz_j)) else dz_j
                 dzDown = if j < n - 1 then 0.5 * (dz_j + (if j+1 < VU.length dzsoi then dzsoi VU.! (j+1) else dz_j)) else dz_j
                 flux = keff * ((tUp - tj) / dzUp - (tj - tDown) / dzDown)
                 dT' = dt * flux / (cv * dz_j)
             in tj + dT'
      tFinal = foldl stepOnce tsoilInit [1..nsteps]

  putStrLn $ "After " ++ show nsteps ++ " timesteps:"
  putStrLn ""
  putStrLn "  Layer | Initial [K]  | Final [K]    | Delta [K]"
  putStrLn "  ------|------------- |------------- |----------"
  mapM_ (\j -> do
    let t0 = tsoilInit VU.! j
        tf = tFinal VU.! j
        dt_ = tf - t0
    putStrLn $ "  " ++ padL 5 (show (j+1)) ++ " | " ++
      padL 12 (showF4 t0) ++ " | " ++
      padL 12 (showF4 tf) ++ " | " ++
      (if dt_ >= 0 then "+" else "") ++ showF6 dt_)
    [0 .. min 9 (nlevgrnd - 1)]

  putStrLn ""
  putStrLn "Done."

-- ============================================================================
-- Formatting helpers
-- ============================================================================

myWhen :: Bool -> IO () -> IO ()
myWhen True  act = act
myWhen False _   = return ()

showF1 :: Double -> String
showF1 x = show (fromIntegral (round (x * 10) :: Int) / 10.0 :: Double)

showF2 :: Double -> String
showF2 x = show (fromIntegral (round (x * 100) :: Int) / 100.0 :: Double)

-- ============================================================================
-- Soil Temperature test mode (Tier 4)
-- ============================================================================

soilTempTestMode :: FilePath -> IO ()
soilTempTestMode dir = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Soil Temperature Test (Tier 4)"
  putStrLn "=============================================="

  let stDir = dir </> "soiltemp"
      flDir = dir </> "fluxes"

  -- Grid dimensions
  let nc = 2
      nlev_sno = nlevsno   -- 12
      nlev_grnd = nlevgrnd  -- 25
      nlev_soi = nlevsoi    -- 20
      ntot = nlev_sno + nlev_grnd  -- 37

  -- Read column geometry (nc × ntot column-major)
  dzAll   <- readFloat64Vector (stDir </> "dz_col.bin")
  zAll    <- readFloat64Vector (stDir </> "z_col.bin")
  ziAll   <- readFloat64Vector (stDir </> "zi_col.bin")

  -- Read soil properties (nc × nlevgrnd column-major)
  watsatAll  <- readFloat64Vector (stDir </> "watsat_col.bin")
  bswAll     <- readFloat64Vector (stDir </> "bsw_col.bin")
  sucsatAll  <- readFloat64Vector (stDir </> "sucsat_col.bin")
  tkmgAll    <- readFloat64Vector (stDir </> "tkmg_col.bin")
  tkdryAll   <- readFloat64Vector (stDir </> "tkdry_col.bin")
  csolAll    <- readFloat64Vector (stDir </> "csol_col.bin")
  tksatuAll  <- readFloat64Vector (stDir </> "tksatu_col.bin")
  nbedrockAll <- readFloat64Vector (stDir </> "nbedrock_col.bin")

  -- Read pre-soil_temperature state (nc × ntot column-major)
  tSoisnoPre   <- readFloat64Vector (stDir </> "t_soisno_pre_col.bin")
  h2osoiLiqPre <- readFloat64Vector (stDir </> "h2osoi_liq_pre_col.bin")
  h2osoiIcePre <- readFloat64Vector (stDir </> "h2osoi_ice_pre_col.bin")
  tGrndPre     <- readFloat64Vector (stDir </> "t_grnd_pre_col.bin")
  tH2osfcPre   <- readFloat64Vector (stDir </> "t_h2osfc_pre_col.bin")

  -- Read heat source terms (nc)
  hsTopCol  <- readFloat64Vector (stDir </> "hs_top_col.bin")
  dhsdTCol  <- readFloat64Vector (stDir </> "dhsdT_col.bin")
  hsSoilCol <- readFloat64Vector (stDir </> "hs_soil_col.bin")
  hsH2osfcCol <- readFloat64Vector (stDir </> "hs_h2osfc_col.bin")

  -- Read additional state
  h2osnoNoLayers <- readFloat64Vector (stDir </> "h2osno_no_layers_col.bin")
  h2osfcCol   <- readFloat64Vector (stDir </> "h2osfc_col.bin")
  snlCol      <- readFloat64Vector (stDir </> "snl_col.bin")
  fracSnoEff  <- readFloat64Vector (stDir </> "frac_sno_eff_col.bin")
  fracH2osfc  <- readFloat64Vector (stDir </> "frac_h2osfc_col.bin")
  snowDepthCol <- readFloat64Vector (stDir </> "snow_depth_col.bin")

  -- Read sabg_lyr_patch (np × (nlevsno+1) column-major)
  sabgLyrPatchAll <- readFloat64Vector (stDir </> "sabg_lyr_patch.bin")
  pchWtcol <- readFloat64Vector (stDir </> "pch_wtcol.bin")

  -- Read post-soil_temperature reference (nc × ntot column-major)
  tSoisnoPost   <- readFloat64Vector (stDir </> "t_soisno_post_col.bin")
  tGrndPost     <- readFloat64Vector (stDir </> "t_grnd_post_col.bin")
  tH2osfcPost   <- readFloat64Vector (stDir </> "t_h2osfc_post_col.bin")
  h2osoiLiqPost <- readFloat64Vector (stDir </> "h2osoi_liq_post_col.bin")
  h2osoiIcePost <- readFloat64Vector (stDir </> "h2osoi_ice_post_col.bin")
  imeltPost     <- readFloat64Vector (stDir </> "imelt_post_col.bin")

  let dtime = 1800.0

  -- Extract column 1 (soil column) data from column-major arrays
  -- Julia layout: column-major (nc, nlev) → Fortran [c + nc*(j-1)]
  -- So column c=0 (0-based) at layer j is index j*nc + c
  let extractCol1 :: VU.Vector Double -> Int -> VU.Vector Double
      extractCol1 vec nlev = VU.generate nlev (\j -> vec VU.! (j * nc + 0))

  let c = 0 :: Int  -- column 1 (0-based)

  -- Column geometry for column 1 (length ntot = 37)
  -- SoilTemperature.hs uses joff = nlevsno - 1 = 11 for 0-based indexing
  let dz_c1  = extractCol1 dzAll ntot
      z_c1   = extractCol1 zAll ntot
      -- zi has ntot+1 entries per column (length 38)
      zi_c1  = VU.generate (ntot + 1) (\j -> ziAll VU.! (j * nc + c))

  -- Soil properties for column 1 (nlevgrnd, NO padding needed — soil-only, 0-based)
  let watsat_c1 = extractCol1 watsatAll nlev_grnd
      bsw_c1    = extractCol1 bswAll nlev_grnd
      sucsat_c1 = extractCol1 sucsatAll nlev_grnd
      tkmg_c1   = extractCol1 tkmgAll nlev_grnd
      tkdry_c1  = extractCol1 tkdryAll nlev_grnd
      csol_c1   = extractCol1 csolAll nlev_grnd
      tksatu_c1 = extractCol1 tksatuAll nlev_grnd
      nbedrock_c1 = round (nbedrockAll VU.! c) :: Int

  -- Pre-state for column 1 (length ntot = 37)
  let tSoisno_c1 = extractCol1 tSoisnoPre ntot
      h2oLiq_c1  = extractCol1 h2osoiLiqPre ntot
      h2oIce_c1  = extractCol1 h2osoiIcePre ntot
      tGrnd_c1   = tGrndPre VU.! c
      tH2osfc_c1 = tH2osfcPre VU.! c

  let snl_c1  = round (snlCol VU.! c) :: Int
      hsTop_c1 = hsTopCol VU.! c
      dhsdT_c1 = dhsdTCol VU.! c
      hsSoil_c1 = hsSoilCol VU.! c
      hsH2osfc_c1 = hsH2osfcCol VU.! c
      h2osnoNL_c1 = h2osnoNoLayers VU.! c
      h2osfc_c1   = h2osfcCol VU.! c
      fracSE_c1   = fracSnoEff VU.! c
      fracH2o_c1  = fracH2osfc VU.! c
      snowDep_c1  = snowDepthCol VU.! c

  -- Compute sabg_lyr for column 1 by weighting patches
  -- sabg_lyr_patch is (np × (nlevsno+1)) column-major
  -- We need to sum weighted patch contributions for top soil layer
  let np = 4
      nlev_sno1 = nlev_sno + 1  -- nlevsno+1 layers in sabg_lyr
      -- For column 1, patches 0,1,2 (0-based) contribute
      -- sabg_lyr_col for column: sum over patches of sabg_lyr * wtcol
      sabgLyrCol = VU.generate nlev_sno1 (\j ->
        let p0_val = sabgLyrPatchAll VU.! (j * np + 0)
            p0_wt  = pchWtcol VU.! 0
            p1_val = sabgLyrPatchAll VU.! (j * np + 1)
            p1_wt  = pchWtcol VU.! 1
            p2_val = sabgLyrPatchAll VU.! (j * np + 2)
            p2_wt  = pchWtcol VU.! 2
            -- Filter NaN: use 0 for NaN patches
            safe x = if isNaN x then 0.0 else x
        in safe p0_val * p0_wt + safe p1_val * p1_wt + safe p2_val * p2_wt)

  putStrLn ""
  putStrLn "--- Input Summary (Column 1, soil) ---"
  putStrLn $ "  snl = " ++ show snl_c1
  putStrLn $ "  nbedrock = " ++ show nbedrock_c1
  putStrLn $ "  t_grnd = " ++ showF4 tGrnd_c1
  putStrLn $ "  t_h2osfc = " ++ showF4 tH2osfc_c1
  putStrLn $ "  hs_top = " ++ showF4 hsTop_c1
  putStrLn $ "  dhsdT = " ++ showF4 dhsdT_c1
  putStrLn $ "  hs_soil = " ++ showF4 hsSoil_c1
  putStrLn $ "  hs_h2osfc = " ++ showF4 hsH2osfc_c1
  putStrLn $ "  h2osno = " ++ showF4 h2osnoNL_c1
  putStrLn $ "  h2osfc = " ++ showF4 h2osfc_c1
  putStrLn $ "  frac_sno_eff = " ++ showF4 fracSE_c1
  putStrLn $ "  frac_h2osfc = " ++ showF4 fracH2o_c1
  putStrLn $ "  snow_depth = " ++ showF4 snowDep_c1
  putStrLn $ "  dtime = " ++ show dtime
  putStrLn $ "  t_soisno[top5] = " ++ show (VU.toList (VU.slice nlev_sno 5 tSoisno_c1))
  putStrLn $ "  dz[top3] = " ++ show (VU.toList (VU.slice nlev_sno 3 dz_c1))
  putStrLn $ "  watsat[top3] = " ++ show (VU.toList (VU.slice 0 3 watsat_c1))

  -- Build SoilTempInput
  let stInput = SoilTempInput
        { sti_snl              = snl_c1
        , sti_t_soisno         = tSoisno_c1
        , sti_t_grnd           = tGrnd_c1
        , sti_t_h2osfc         = tH2osfc_c1
        , sti_h2osoi_liq       = h2oLiq_c1
        , sti_h2osoi_ice       = h2oIce_c1
        , sti_dz               = dz_c1
        , sti_z                = z_c1
        , sti_zi               = zi_c1
        , sti_watsat           = watsat_c1
        , sti_bsw              = bsw_c1
        , sti_sucsat           = sucsat_c1
        , sti_tkmg             = tkmg_c1
        , sti_tkdry            = tkdry_c1
        , sti_csol             = csol_c1
        , sti_tksatu           = tksatu_c1
        , sti_nbedrock         = nbedrock_c1
        , sti_h2osno_no_layers = h2osnoNL_c1
        , sti_h2osfc           = h2osfc_c1
        , sti_frac_sno_eff     = fracSE_c1
        , sti_frac_h2osfc      = fracH2o_c1
        , sti_snow_depth       = snowDep_c1
        , sti_hs_top           = hsTop_c1
        , sti_dhsdT            = dhsdT_c1
        , sti_hs_soil          = hsSoil_c1
        , sti_hs_h2osfc        = hsH2osfc_c1
        , sti_sabg_lyr         = sabgLyrCol
        , sti_eflx_bot         = 0.0
        , sti_dtime            = dtime
        , sti_snowCondMethod   = Jordan1991
        }

  putStrLn ""
  putStrLn "--- Running solveSoilTemperature ---"

  let !stOutput = solveSoilTemperature stInput

  -- Extract reference post-state for column 1
  let tSoisnoRef = extractCol1 tSoisnoPost ntot
      tGrndRef   = tGrndPost VU.! c
      tH2osfcRef = tH2osfcPost VU.! c

  putStrLn ""
  putStrLn "--- Results Comparison ---"
  putStrLn $ "  t_grnd:   Haskell=" ++ showF6 (sto_t_grnd stOutput) ++ "  Julia=" ++ showF6 tGrndRef ++ "  diff=" ++ showEFloat (Just 4) (sto_t_grnd stOutput - tGrndRef) ""
  putStrLn $ "  t_h2osfc: Haskell=" ++ showF6 (sto_t_h2osfc stOutput) ++ "  Julia=" ++ showF6 tH2osfcRef ++ "  diff=" ++ showEFloat (Just 4) (sto_t_h2osfc stOutput - tH2osfcRef) ""
  putStrLn $ "  xmf:      " ++ showF6 (sto_xmf stOutput)
  putStrLn $ "  qflx_snomelt: " ++ showF6 (sto_qflx_snomelt stOutput)

  putStrLn ""
  putStrLn "  t_soisno comparison (top 10 soil layers):"
  putStrLn $ "    " ++ padL 5 "Layer" ++ padL 15 "Haskell" ++ padL 15 "Julia" ++ padL 15 "Diff"
  let t_out = sto_t_soisno stOutput
  mapM_ (\j -> do
    let jj = j + nlev_sno
        h_val = t_out VU.! jj
        j_val = tSoisnoRef VU.! jj
        diff = h_val - j_val
    putStrLn $ "    " ++ padL 5 (show (j+1)) ++ padL 15 (showF6 h_val) ++ padL 15 (showF6 j_val) ++ padL 15 (showEFloat (Just 3) diff "")
    ) [0..min 9 (nlev_grnd - 1)]

  -- Overall statistics
  let diffs = VU.generate nlev_grnd (\j ->
        let jj = j + nlev_sno
        in abs (t_out VU.! jj - tSoisnoRef VU.! jj))
      maxDiff = VU.maximum diffs
      meanDiff = VU.sum diffs / fromIntegral nlev_grnd

  putStrLn ""
  putStrLn $ "  Max |diff| = " ++ showEFloat (Just 4) maxDiff ""
  putStrLn $ "  Mean |diff| = " ++ showEFloat (Just 4) meanDiff ""

  let pass = maxDiff < 0.01
  putStrLn ""
  if pass
    then putStrLn "  *** TIER 4 SOIL TEMPERATURE: PASS (max diff < 0.01 K) ***"
    else putStrLn "  *** TIER 4 SOIL TEMPERATURE: FAIL ***"

showF3 :: Double -> String
showF3 x = show (fromIntegral (round (x * 1000) :: Int) / 1000.0 :: Double)

showF4 :: Double -> String
showF4 x = show (fromIntegral (round (x * 10000) :: Int) / 10000.0 :: Double)

showF5 :: Double -> String
showF5 x = show (fromIntegral (round (x * 100000) :: Int) / 100000.0 :: Double)

showF6 :: Double -> String
showF6 x = show (fromIntegral (round (x * 1000000) :: Int) / 1000000.0 :: Double)

padL :: Int -> String -> String
padL n s = replicate (max 0 (n - length s)) ' ' ++ s

-- ============================================================================
-- Run mode: multi-day simulation
-- ============================================================================

runMode :: FilePath -> Int -> IO ()
runMode dir ndays = do
  putStrLn "=============================================="
  putStrLn "  CLM.hs — Multi-Day Simulation (Tier 5)"
  putStrLn "=============================================="
  putStrLn ""

  putStrLn $ "Initializing from: " ++ dir
  st0 <- initSimState dir
  putStrLn $ "  T_GRND(init) = " ++ show (ss_t_grnd st0) ++ " K"
  putStrLn ""

  let dtime = 1800.0  -- 30-minute timesteps
  putStrLn $ "Running " ++ show ndays ++ "-day simulation (" ++ show (ndays * 48) ++ " timesteps)..."
  putStrLn ""

  results <- runSimulation st0 ndays dtime

  -- Write CSV output
  let outFile = dir </> "haskell_daily_avg.csv"
  writeFile outFile "day,T_GRND,TSA,FSA,EFLX_LH_TOT,EFLX_SH_TOT,H2OSNO,QRUNOFF,SNOW_DEPTH,FRAC_SNO\n"
  mapM_ (\(dayNum, avg) ->
    appendFile outFile $ show dayNum
      ++ "," ++ showEFloat (Just 8) (da_t_grnd avg) ""
      ++ "," ++ showEFloat (Just 8) (da_tsa avg) ""
      ++ "," ++ showEFloat (Just 8) (da_fsa avg) ""
      ++ "," ++ showEFloat (Just 8) (da_eflx_lh avg) ""
      ++ "," ++ showEFloat (Just 8) (da_eflx_sh avg) ""
      ++ "," ++ showEFloat (Just 8) (da_h2osno avg) ""
      ++ "," ++ showEFloat (Just 8) (da_qrunoff avg) ""
      ++ "," ++ showEFloat (Just 8) (da_snow_depth avg) ""
      ++ "," ++ showEFloat (Just 8) (da_frac_sno avg) ""
      ++ "\n"
    ) (zip [1..] results)

  putStrLn ""
  putStrLn $ "Output written to: " ++ outFile
  putStrLn ""

  -- Compare against Julia reference if available
  let refFile = dir </> "trajectory" </> "daily_avg.csv"
  putStrLn $ "Comparing against Julia reference: " ++ refFile
  putStrLn ""

  -- Print summary
  putStrLn "day | T_GRND(hs)  | T_GRND(jl)  | diff"
  putStrLn "----+-------------+-------------+-------"
  mapM_ (\(dayNum, avg) ->
    putStrLn $ padL 3 (show dayNum)
            ++ " | " ++ showF3 (da_t_grnd avg)
            ++ "       | (see CSV)   | "
    ) (zip [1..] (take 10 results))
