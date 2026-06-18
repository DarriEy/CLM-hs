{-# LANGUAGE BangPatterns #-}
-- | Fortran-parity harness (Phase 0, hardened).
--
-- Mirrors the CLM.jl single-step boundary-injection methodology:
--
--   1. Read a Fortran @before_step@ restart-format dump (per-boundary
--      instrumentation snapshot, single column / 3 patches).
--   2. Inject that exact Fortran state into a fresh CLM-hs 'CLMState', and build
--      the matching atmospheric forcing for the step.
--   3. Run exactly ONE 'clmDrvBoundaries' timestep, capturing the intermediate
--      state at each Fortran instrumentation boundary.
--   4. Diff each field against the dump for ITS boundary, NaN-aware, against a
--      fixed tolerance table.
--
-- Re-injecting every step measures per-step *translation* error, not
-- compounded drift, so a failing field points at the responsible module.
--
-- Forcing: the bgc_ref_summer dumps carry FORC_T_G / FORC_PBOT_G /
-- FORC_LWRAD_G / FORC_RAIN_G / FORC_SNOW_G directly (already rain/snow-split by
-- Fortran). The missing atmospheric inputs — shortwave (FSDS), wind, and
-- specific humidity (QBOT) — are read from the Bow forcing file
-- 'bowForcingFile' (hourly, 2003) at the dump's timestamp. Solar geometry
-- (declination) is derived from the calendar day; 'identityReport' confirms the
-- injected state round-trips the dump before any physics runs.
module CLM.Calibration.FortranParity
  ( -- * Reference data
    bgcDumpDir
  , bowForcingFile
  , bgcSteps
  , dumpPath
  , boundaries
    -- * Harness
  , ParityHarness(..)
  , initParityHarness
  , injectBeforeStep
  , runOneStepBoundaries
    -- * Diffing
  , Kind(..)
  , Boundary(..)
  , FieldDiff(..)
  , registry
  , reldiff
  , absdiff
  , compareToDumps
    -- * Reports
  , identityReport
  , baselineReport
  ) where

import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Text.Printf (printf)
import Control.Monad (forM, forM_)
import Data.List (foldl', nub, minimumBy)
import Data.Ord (comparing)

import CLM.Infrastructure.NetCDF
  ( NcFile, ncOpen, ncClose, ncHasVar, ncReadDouble1D )
import CLM.Constants.ControlFlags (defaultDriverConfig)
import CLM.Driver.CLMDriver
  ( CLMState(..), TimestepContext(..)
  , BoundarySnapshots(..)
  , defaultDriverState, defaultTimestepContext, clmDrvBoundaries )
import CLM.Driver.PhysicsAdapters (wiredPhysicsPipeline)
import CLM.Driver.PipelineRunner
  ( initCLMStateFromDir, SurfaceAlbedoConstants )
import CLM.Infrastructure.ForcingReader
  ( computeVaporPressureFromQ, computePotentialTemperature, computeAirDensity
  , splitShortwaveBandsCLMNCEP )
import CLM.Infrastructure.Orbital (computeOrbital, defaultOrbitalParams)

import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData (WaterStateData(..))
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..))
import CLM.Types.SoilHydrologyData (SoilHydrologyData(..))
import CLM.Types.SoilStateData (SoilStateData(..))
import CLM.Types.EnergyFluxData (EnergyFluxData(..))
import CLM.Types.CanopyStateData (CanopyStateData(..))
import CLM.Types.ColumnData (ColumnData(..))
import CLM.Types.GridcellData (GridcellData(..))

-- ============================================================================
-- Reference data locations
-- ============================================================================

-- | Directory holding the 28-step summer-window per-boundary dumps.
bgcDumpDir :: FilePath
bgcDumpDir =
  "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/clm_bgc_spinup/bgc_ref_summer"

-- | Bow-at-Banff hourly atmospheric forcing. The bgc spinup (whose dumps drive
-- this harness) cycles a SINGLE forcing year — 2002 — confirmed in the run's
-- @atm.log@ (@shr_stream_getCalendar ... clmforc.2002.nc@). The 2002 file's
-- clear-sky-shaped FSDS reproduces the dump's absorbed solar (SABG/SABV) after
-- the solar-zenith downscaling below; the 2003 file is a different (cloudier)
-- climatology and does NOT match the dumps.
bowForcingFile :: FilePath
bowForcingFile =
  "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped/data/forcing/CLM_input/clmforc.2002.nc"

-- | The 28 contiguous summer timesteps that were instrumented.
bgcSteps :: [Int]
bgcSteps = [1757845 .. 1757872]

-- | Per-boundary dump names, in driver order.
boundaries :: [String]
boundaries =
  [ "before_step", "after_canopyfluxes", "after_soiltemperature"
  , "after_soilfluxes", "after_hydrologynodrainage", "after_hydrologydrainage"
  , "after_ecosysdyn_predrain", "after_competition" ]

-- | Path to a specific dump: @dumpPath dir boundary nstep@.
dumpPath :: FilePath -> String -> Int -> FilePath
dumpPath dir boundary nstep =
  dir </> ("pdump_" ++ boundary ++ "_n" ++ show nstep ++ ".nc")

-- ============================================================================
-- Low-level dump reading
-- ============================================================================

withDump :: FilePath -> (NcFile -> IO a) -> IO a
withDump path act = do
  r <- ncOpen path
  case r of
    Left e   -> error ("FortranParity: cannot open " ++ path ++ ": " ++ e)
    Right nc -> do a <- act nc; ncClose nc; return a

-- | Read a named variable as a flat vector, 'Nothing' if absent.
getVec :: NcFile -> String -> IO (Maybe (VU.Vector Double))
getVec nc name = do
  has <- ncHasVar nc name
  if not has
    then return Nothing
    else either (const Nothing) Just <$> ncReadDouble1D nc name

-- | Read a scalar (first element) with a fallback.
getScalar :: NcFile -> String -> Double -> IO Double
getScalar nc name def =
  maybe def (\v -> if VU.null v then def else v VU.! 0) <$> getVec nc name

-- ============================================================================
-- Forcing
-- ============================================================================

-- | Cumulative days before each month in a no-leap (365-day) year.
cumDaysNoLeap :: [Int]
cumDaysNoLeap = [0,31,59,90,120,151,181,212,243,273,304,334]

-- | Day-of-year (1-based) for a (month, day) pair, no-leap calendar.
dayOfYear :: Int -> Int -> Int
dayOfYear m d = (cumDaysNoLeap !! (max 0 (min 11 (m - 1)))) + d

-- | Read FSDS / WIND / QBOT from the Bow forcing file, calibrating the time
-- index against the dump's air temperature. T is present in BOTH the dump
-- (@FORC_T_G@) and the forcing file (@TBOT@); CLM forcing is interval-averaged
-- / one-step-lagged, so the nominal @(doy,hour)@ index can be off by one. We
-- search a small window around the nominal index and pick the record whose
-- TBOT best matches the dump's exact forcing temperature, then read the
-- (undumped) shortwave/wind/humidity from that aligned record.
readForcingCalibrated :: Int -> Double -> IO (Maybe (Double, Double, Double))
readForcingCalibrated idx0 targetT = do
  r <- ncOpen bowForcingFile
  case r of
    Left _   -> return Nothing
    Right nc -> do
      mtbot <- getVec nc "TBOT"
      mfsds <- getVec nc "FSDS"
      mwind <- getVec nc "WIND"
      mqbot <- getVec nc "QBOT"
      ncClose nc
      return $ do
        tbot <- mtbot; fsds <- mfsds; wind <- mwind; qbot <- mqbot
        let n = minimum [VU.length tbot, VU.length fsds, VU.length wind, VU.length qbot]
            cands = [ i | d <- [-2 .. 2 :: Int], let i = idx0 + d, i >= 0, i < n ]
        if null cands
          then Nothing
          else
            -- Instantaneous fields (T, wind, humidity) align at the
            -- temperature-matched index @best@. Shortwave FSDS in the file is an
            -- INTERVAL AVERAGE: FSDS[i] is the mean over the hour ENDING at
            -- timestamp i (verified: FSDS turns non-zero exactly at the sunrise
            -- hour). The model step's instantaneous solar geometry time is
            -- t_inst = calday - dtime/86400 (see 'injectBeforeStep'); that hour
            -- is the START of the bucket whose timestamp is @best@, so the
            -- governing average is FSDS[best] (interval [t_inst, t_inst+dtime]).
            -- The caller applies the shr_orb_cosz solar-zenith downscaling
            --   FSDS_inst = FSDS_avg * cosz_inst / mean(max(0,cosz)) .
            let best = minimumBy (comparing (\i -> abs (tbot VU.! i - targetT))) cands
            in Just (fsds VU.! best, wind VU.! best, qbot VU.! best)

-- | Solar zenith cosine via shr_orb_cosz (share/src/shr_orb_mod.F90 lines
-- 155-156): @cosz = sin(lat)sin(declin) - cos(lat)cos(declin)cos(2pi*frac+lon)@,
-- with @frac@ the UTC day fraction. lat/lon in radians.
shrOrbCosz :: Double -> Double -> Double -> Double -> Double
shrOrbCosz lat lon declin calday =
  let frac = calday - fromIntegral (floor calday :: Int)
  in sin lat * sin declin - cos lat * cos declin * cos (frac * 2.0 * pi + lon)

-- | datm solar-zenith downscaling factor for an interval-averaged FSDS.
--
-- CLM's data atmosphere disaggregates an interval-mean shortwave to the model
-- instant via @FSDS_inst = FSDS_avg * cosz_inst / mean(max(0,cosz))@, where the
-- mean is taken over the FSDS averaging interval (CLMNCEP/coszen mode). Here
-- @tInst@ is the model step's instantaneous solar-geometry time
-- (calday - dtime/86400, calibrated to reproduce the dump @coszen@ to ~1e-4),
-- and the averaging interval is the forcing timestep CENTERED on @tInst@,
-- i.e. @[tInst - dtime/2, tInst + dtime/2]@. This was selected by minimising
-- the residual against the dump's bare-patch-implied FSDS over the 28-step
-- window (centered interval: ~13 W/m² RMS; one-sided intervals: 60-77 W/m²).
-- The result is clamped (cap the up-scale, zero below the horizon) to stay
-- finite at dawn/dusk.
solarSwScale
  :: Double  -- ^ latitude  [rad]
  -> Double  -- ^ longitude [rad]
  -> Double  -- ^ declination [rad]
  -> Double  -- ^ dtime [s]
  -> Double  -- ^ instantaneous geometry time (calday - dtime/86400)
  -> Double  -- ^ instantaneous cosz (dump ground truth)
  -> Double
solarSwScale lat lon declin dtime tInst coszInst
  | coszInst <= 1.0e-4 = 0.0
  | coszBar  <= 1.0e-4 = 0.0
  | otherwise          = min 4.0 (coszInst / coszBar)
  where
    dDay  = dtime / 86400.0
    nsub  = 120 :: Int
    t0    = tInst - 0.5 * dDay   -- forcing interval centered on tInst
    coszBar =
      let s = sum [ max 0.0 (shrOrbCosz lat lon declin
                              (t0 + dDay * (fromIntegral k + 0.5) / fromIntegral nsub))
                  | k <- [0 .. nsub - 1] ]
      in s / fromIntegral nsub

-- ============================================================================
-- Harness state
-- ============================================================================

-- | Static structure shared across all injected steps.
data ParityHarness = ParityHarness
  { phBase :: !CLMState                 -- ^ Bow structure + statics (test/data)
  , phAlb  :: !SurfaceAlbedoConstants   -- ^ Albedo tables for the wired pipeline
  }

-- | Build the static structure once from the test/data Bow cold-start.
initParityHarness :: FilePath -> IO ParityHarness
initParityHarness testDataDir = do
  (st, _forcing, alb) <- initCLMStateFromDir testDataDir
  return (ParityHarness st alb)

-- | Overlay only the first @length new@ entries of a base vector.
overlayFirst :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
overlayFirst base new
  | VU.null new = base
  | otherwise   = VU.imap (\i b -> if i < VU.length new then new VU.! i else b) base

-- | Inject a @before_step@ dump onto the base structure and build the matching
-- forcing context for the one-step run.
injectBeforeStep :: ParityHarness -> FilePath -> IO (CLMState, TimestepContext)
injectBeforeStep h path = do
  -- ---- read everything we need from the dump -----------------------------
  (st, ymd, tod, stepSec, coszen) <- withDump path $ \nc -> do
    let base = phBase h
    t_soisno   <- maybe (t_soisno_col (clmTemp base)) id <$> getVec nc "T_SOISNO"
    h2osoi_liq <- maybe (h2osoi_liq_col (clmWaterState base)) id <$> getVec nc "H2OSOI_LIQ"
    h2osoi_ice <- maybe (h2osoi_ice_col (clmWaterState base)) id <$> getVec nc "H2OSOI_ICE"
    t_grnd     <- getScalar nc "T_GRND"   (t_grnd_col (clmTemp base))
    h2osfc     <- getScalar nc "H2OSFC"   (h2osfc_col (clmWaterState base))
    h2osno     <- getScalar nc "H2OSNO"   (h2osno_col (clmWaterState base))
    zwt        <- getScalar nc "ZWT"        2.0
    zwt_perch  <- getScalar nc "ZWT_PERCH"  zwt
    snow_depth <- getScalar nc "SNOW_DEPTH" 0.0
    frac_sno   <- getScalar nc "frac_sno"   0.0
    frac_sno_e <- getScalar nc "frac_sno_eff" frac_sno
    snl        <- round <$> getScalar nc "SNLSNO" 0.0
    mt_veg <- getVec nc "T_VEG"
    melai  <- getVec nc "elai"
    mtlai  <- getVec nc "tlai"
    mesai  <- getVec nc "esai"
    mtsai  <- getVec nc "tsai"
    mwatsat <- getVec nc "WATSAT_P"
    mbsw    <- getVec nc "BSW_P"
    msucsat <- getVec nc "SUCSAT_P"
    mhksat  <- getVec nc "HKSAT_P"
    -- Exact Fortran per-layer thermal conductivity (levtot ordering, snow-first
    -- then soil) — injected into the heat solve like the hydraulic params, since
    -- the bgc-run soil texture differs from the test/data base and cannot be
    -- reconstructed from surfdata.
    mthk    <- getVec nc "THK_C"
    ymd'     <- round <$> getScalar nc "timemgr_rst_curr_ymd"  22020715
    tod'     <- round <$> getScalar nc "timemgr_rst_curr_tod"  46800
    stepSec' <- getScalar nc "timemgr_rst_step_sec" 1800
    coszen'  <- getScalar nc "coszen" =<< getScalar nc "coszen_grc" 0.0

    let baseT  = clmTemp base
        baseW  = clmWaterState base
        baseSH = clmSoilHydro base
        baseWD = clmWaterDiagBulk base
        baseC  = clmColumn base
        baseSS = clmSoilState base
        baseCS = clmCanopyState base
        tvegVec0 = t_veg_patch_vec baseT
        withSoil v = maybe v id
        st' = base
          { clmTemp = baseT
              { t_soisno_col      = t_soisno
              , t_soisno_bef_col  = t_soisno
              , t_grnd_col        = t_grnd
              , t_veg_patch       = maybe (t_veg_patch baseT)
                                      (\v -> if VU.null v then t_veg_patch baseT else v VU.! 0) mt_veg
              , t_veg_patch_vec   = maybe tvegVec0 (overlayFirst tvegVec0) mt_veg
              }
          , clmWaterState = baseW
              { h2osoi_liq_col = h2osoi_liq
              , h2osoi_ice_col = h2osoi_ice
              , h2osno_col     = h2osno
              , h2osfc_col     = h2osfc
              }
          , clmSoilHydro = baseSH
              { sh_zwt_col         = VU.singleton zwt
              , sh_zwts_col        = VU.singleton zwt
              , sh_zwt_perched_col = VU.singleton zwt_perch
              }
          , clmWaterDiagBulk = baseWD
              { wdiag_snow_depth_col   = VU.singleton snow_depth
              , wdiag_frac_sno_col     = VU.singleton frac_sno
              , wdiag_frac_sno_eff_col = VU.singleton frac_sno_e
              }
          , clmColumn = baseC
              { watsat = withSoil (watsat baseC) mwatsat
              , bsw    = withSoil (bsw baseC)    mbsw
              , sucsat = withSoil (sucsat baseC) msucsat
              , hksat  = withSoil (hksat baseC)  mhksat
              }
          , clmSoilState = baseSS
              { sstate_watsat_col = withSoil (sstate_watsat_col baseSS) mwatsat
              , sstate_bsw_col    = withSoil (sstate_bsw_col baseSS)    mbsw
              , sstate_sucsat_col = withSoil (sstate_sucsat_col baseSS) msucsat
              , sstate_thk_override_col = maybe VU.empty id mthk
              }
          , clmCanopyState = baseCS
              { cstate_elai_patch = maybe (cstate_elai_patch baseCS) (overlayFirst (cstate_elai_patch baseCS)) melai
              , cstate_tlai_patch = maybe (cstate_tlai_patch baseCS) (overlayFirst (cstate_tlai_patch baseCS)) mtlai
              , cstate_esai_patch = maybe (cstate_esai_patch baseCS) (overlayFirst (cstate_esai_patch baseCS)) mesai
              , cstate_tsai_patch = maybe (cstate_tsai_patch baseCS) (overlayFirst (cstate_tsai_patch baseCS)) mtsai
              }
          , clmSnl = snl
          }
    -- forcing inputs that the dump DOES carry (Fortran-exact)
    return (st', ymd', tod', stepSec', coszen')

  -- ---- atmospheric forcing: dump for T/pbot/lw/rain/snow, file for sw/wind/q
  (forc_t, forc_pbot, forc_lw, forc_rain, forc_snow) <- withDump path $ \nc -> do
    a <- getScalar nc "FORC_T_G"     283.0
    b <- getScalar nc "FORC_PBOT_G"  78000.0
    c <- getScalar nc "FORC_LWRAD_G" 300.0
    d <- getScalar nc "FORC_RAIN_G"  0.0
    e <- getScalar nc "FORC_SNOW_G"  0.0
    return (a, b, c, d, e)

  let month = (ymd `div` 100) `mod` 100
      day   = ymd `mod` 100
      hour  = tod `div` 3600
      idx   = (dayOfYear month day - 1) * 24 + hour
      calday = fromIntegral (dayOfYear month day) + fromIntegral tod / 86400.0
      (declin, _eccf) = computeOrbital defaultOrbitalParams calday
      dtime  = if stepSec > 0 then stepSec else 1800.0

  mfrc <- readForcingCalibrated idx forc_t
  let (fsdsAvg, wind, forc_q) = case mfrc of
        Just (s, w, q) -> (s, w, q)
        Nothing        -> (0.0, 2.0, 0.005)   -- forcing file absent
      -- Solar-zenith downscaling of the interval-averaged FSDS to the model
      -- instant. The instantaneous geometry time tInst = calday - dtime/86400
      -- reproduces the dump @coszen@ (cz_inst) to ~1e-4; we use the dump value
      -- itself as the ground-truth instantaneous cosine.
      grc   = clmGridcell (phBase h)
      lat   = if VU.null (grc_lat grc) then 0.8964 else grc_lat grc VU.! 0
      lon   = if VU.null (grc_lon grc) then 0.0    else grc_lon grc VU.! 0
      tInst = calday - dtime / 86400.0
      swScale = solarSwScale lat lon declin dtime tInst coszen
      fsds  = fsdsAvg * swScale
      (solad_vis, solad_nir, solai_vis, solai_nir) = splitShortwaveBandsCLMNCEP fsds
      forc_vp  = computeVaporPressureFromQ forc_q forc_pbot
      forc_th  = computePotentialTemperature forc_t forc_pbot
      forc_rho = computeAirDensity forc_pbot forc_t forc_vp
      ctx = defaultTimestepContext
        { tcDoAlb       = True
        , tcDtime       = dtime
        , tcNextswCday  = calday + dtime / 86400.0
        , tcDeclin      = declin
        , tcDeclinP1    = declin
        , tcObliqr      = 0.409
        , tcIsFirstStep = False
        , tcForcT       = VU.singleton forc_t
        , tcForcTh      = VU.singleton forc_th
        , tcForcQ       = VU.singleton forc_q
        , tcForcPbot    = VU.singleton forc_pbot
        , tcForcRho     = VU.singleton forc_rho
        , tcForcRain    = VU.singleton forc_rain
        , tcForcSnow    = VU.singleton forc_snow
        , tcForcLwrad   = VU.singleton forc_lw
        , tcForcSolad   = VU.fromList [solad_vis, solad_nir]
        , tcForcSolai   = VU.fromList [solai_vis, solai_nir]
        , tcForcWind    = VU.singleton wind
        , tcForcHgt     = 30.0
        }
  return (st, ctx)

-- | Inject → one 'clmDrvBoundaries' step → boundary snapshots.
runOneStepBoundaries :: ParityHarness -> (CLMState, TimestepContext) -> BoundarySnapshots
runOneStepBoundaries h (st, ctx) =
  snd (clmDrvBoundaries defaultDriverConfig (wiredPhysicsPipeline (phAlb h))
                        ctx defaultDriverState st)

-- ============================================================================
-- Field registry + diffing
-- ============================================================================

-- | Shape of a registry field.
data Kind = Col1d | Col2d | Patch deriving (Eq, Show)

-- | Instrumentation boundary a field should be compared at.
data Boundary
  = AfterCanopyFluxes
  | AfterSoilTemperature
  | AfterSoilFluxes
  | AfterHydrologyNoDrain
  | AfterHydrologyDrainage
  deriving (Eq, Show)

-- | The dump-file boundary name for a 'Boundary'.
boundaryDumpName :: Boundary -> String
boundaryDumpName b = case b of
  AfterCanopyFluxes      -> "after_canopyfluxes"
  AfterSoilTemperature   -> "after_soiltemperature"
  AfterSoilFluxes        -> "after_soilfluxes"
  AfterHydrologyNoDrain  -> "after_hydrologynodrainage"
  AfterHydrologyDrainage -> "after_hydrologydrainage"

-- | The captured snapshot for a 'Boundary'.
boundarySnapshot :: Boundary -> BoundarySnapshots -> CLMState
boundarySnapshot b = case b of
  AfterCanopyFluxes      -> bsAfterCanopyFluxes
  AfterSoilTemperature   -> bsAfterSoilTemperature
  AfterSoilFluxes        -> bsAfterSoilFluxes
  AfterHydrologyNoDrain  -> bsAfterHydrologyNoDrain
  AfterHydrologyDrainage -> bsFinal

-- | (Fortran var name, shape, boundary, model getter, abs tolerance).
registry :: [(String, Kind, Boundary, CLMState -> [Double], Double)]
registry =
  [ ("T_GRND",     Col1d, AfterSoilTemperature,   \s -> [t_grnd_col (clmTemp s)],                      0.20)
  , ("T_SOISNO",   Col2d, AfterSoilTemperature,   \s -> VU.toList (t_soisno_col (clmTemp s)),          0.20)
  , ("T_VEG",      Patch, AfterCanopyFluxes,      \s -> VU.toList (t_veg_patch_vec (clmTemp s)),       1.20)
  , ("SABV_P",     Patch, AfterCanopyFluxes,      \s -> VU.toList (sabv_patch_vec (clmEnergyFlux s)),  5.0)
  , ("SABG_P",     Patch, AfterCanopyFluxes,      \s -> VU.toList (sabg_patch_vec (clmEnergyFlux s)),  5.0)
  , ("H2OSOI_LIQ", Col2d, AfterHydrologyNoDrain,  \s -> VU.toList (h2osoi_liq_col (clmWaterState s)),  0.05)
  , ("H2OSOI_ICE", Col2d, AfterHydrologyNoDrain,  \s -> VU.toList (h2osoi_ice_col (clmWaterState s)),  0.05)
  , ("H2OSFC",     Col1d, AfterHydrologyNoDrain,  \s -> [h2osfc_col (clmWaterState s)],                1.0e-3)
  , ("ZWT",        Col1d, AfterHydrologyDrainage, \s -> headList (sh_zwt_col (clmSoilHydro s)),         0.02)
  , ("ZWT_PERCH",  Col1d, AfterHydrologyDrainage, \s -> headList (sh_zwt_perched_col (clmSoilHydro s)), 0.05)
  , ("SNOW_DEPTH", Col1d, AfterHydrologyDrainage, \s -> headList (wdiag_snow_depth_col (clmWaterDiagBulk s)), 1.0e-3)
  , ("frac_sno",   Col1d, AfterHydrologyDrainage, \s -> headList (wdiag_frac_sno_col (clmWaterDiagBulk s)),   1.0e-3)
  ]
  where headList v = if VU.null v then [] else [v VU.! 0]

-- | Result of comparing one field against a dump.
data FieldDiff = FieldDiff
  { fdName     :: !String
  , fdBoundary :: !String
  , fdAbs      :: !Double
  , fdRel      :: !Double
  , fdNpts     :: !Int
  , fdTol      :: !Double
  , fdPass     :: !Bool
  } deriving (Show)

-- | NaN-aware max relative diff: |a-b| / (1 + max(|a|,|b|)).
reldiff :: [Double] -> [Double] -> Double
reldiff as bs = foldl' step 0.0 (zip as bs)
  where step m (a, b)
          | isNaN a && isNaN b = m
          | otherwise = max m (abs (a - b) / (1.0 + max (abs a) (abs b)))

-- | NaN-aware max absolute diff.
absdiff :: [Double] -> [Double] -> Double
absdiff as bs = foldl' step 0.0 (zip as bs)
  where step m (a, b)
          | isNaN a && isNaN b = m
          | otherwise = max m (abs (a - b))

-- | Compare boundary snapshots against the matching @after_<boundary>@ dumps,
-- one comparison per registry field at its own boundary.
compareToDumps :: BoundarySnapshots -> FilePath -> Int -> IO [FieldDiff]
compareToDumps snaps dir nstep =
  fmap concat $ forM (nub (map (\(_,_,b,_,_) -> b) registry)) $ \bnd -> do
    let path = dumpPath dir (boundaryDumpName bnd) nstep
        st   = boundarySnapshot bnd snaps
        flds = [ (n, g, tol) | (n, _, b, g, tol) <- registry, b == bnd ]
    exists <- doesFileExist path
    if not exists then return [] else withDump path $ \nc ->
      fmap concat $ forM flds $ \(name, getter, tol) -> do
        mdump <- getVec nc name
        case mdump of
          Nothing -> return []
          Just dv -> do
            let dl = VU.toList dv
                ml = getter st
                n  = min (length dl) (length ml)
            if n == 0 then return [] else do
              let a = absdiff (take n ml) (take n dl)
                  r = reldiff (take n ml) (take n dl)
              return [FieldDiff name (boundaryDumpName bnd) a r n tol (a <= tol)]

-- ============================================================================
-- Identity report (injection fidelity, no physics)
-- ============================================================================

-- | Inject @before_step@ and compare the INJECTED state to the same dump with
-- NO step run. Should be ~0 for every injected field; non-zero means a
-- layer/scaling bug in the injector, not physics.
identityReport :: FilePath -> IO [FieldDiff]
identityReport testDataDir = do
  h <- initParityHarness testDataDir
  steps <- filterExisting bgcSteps "before_step"
  if null steps then putStrLn "FortranParity identity: no dumps." >> return [] else do
    let n0 = head steps
        path = dumpPath bgcDumpDir "before_step" n0
    (st, _ctx) <- injectBeforeStep h path
    diffs <- withDump path $ \nc ->
      fmap concat $ forM registry $ \(name, _k, _b, getter, tol) -> do
        mdump <- getVec nc name
        case mdump of
          Nothing -> return []
          Just dv -> do
            let dl = VU.toList dv
                ml = getter st
                nn = min (length dl) (length ml)
            if nn == 0 then return [] else do
              let a = absdiff (take nn ml) (take nn dl)
                  r = reldiff (take nn ml) (take nn dl)
              -- identity tolerance is tight: injected fields must round-trip
              return [FieldDiff name "before_step" a r nn 1.0e-6 (a <= 1.0e-6)]
    putStrLn ""
    printf "=== Injection identity (before_step n%d, no physics) ===\n" n0
    putStrLn (printf "%-12s %12s %12s  %s" "field" "absdiff" "reldiff" "result")
    forM_ diffs $ \fd ->
      putStrLn (printf "%-12s %12.4e %12.4e  %s" (fdName fd) (fdAbs fd) (fdRel fd)
                  (if fdPass fd then "OK" else "INJECT-BUG?"))
    return diffs

-- ============================================================================
-- Baseline report
-- ============================================================================

filterExisting :: [Int] -> String -> IO [Int]
filterExisting steps bnd =
  fmap (map fst . filter snd)
    (forM steps (\n -> (,) n <$> doesFileExist (dumpPath bgcDumpDir bnd n)))

-- | Run the per-boundary harness over the window and print (1) the first-step
-- table and (2) the 28-step max per field. Returns the aggregated diffs.
baselineReport :: FilePath -> IO [FieldDiff]
baselineReport testDataDir = do
  h <- initParityHarness testDataDir
  steps <- filterExisting bgcSteps "before_step"
  if null steps
    then do putStrLn "FortranParity baseline: no dumps present, skipping."
            return []
    else do
      perStep <- forM steps $ \n -> do
        inj <- injectBeforeStep h (dumpPath bgcDumpDir "before_step" n)
        let snaps = runOneStepBoundaries h inj
        compareToDumps snaps bgcDumpDir n

      putStrLn ""
      printf "=== Fortran parity baseline — first step n%d (per-boundary) ===\n" (head steps)
      putStrLn (printf "%-12s %-26s %12s %12s %10s  %s" "field" "boundary" "absdiff" "reldiff" "tol" "result")
      forM_ (head perStep) $ \fd ->
        putStrLn (printf "%-12s %-26s %12.4e %12.4e %10.2e  %s"
                    (fdName fd) (fdBoundary fd) (fdAbs fd) (fdRel fd) (fdTol fd)
                    (if fdPass fd then "PASS" else "FAIL"))

      let agg = aggregate (concat perStep)
      putStrLn ""
      printf "=== %d-step window max ===\n" (length steps)
      putStrLn (printf "%-12s %-26s %12s %12s %10s  %s" "field" "boundary" "maxAbs" "maxRel" "tol" "result")
      forM_ agg $ \fd ->
        putStrLn (printf "%-12s %-26s %12.4e %12.4e %10.2e  %s"
                    (fdName fd) (fdBoundary fd) (fdAbs fd) (fdRel fd) (fdTol fd)
                    (if fdPass fd then "PASS" else "FAIL"))
      return agg

-- | Worst (max abs, max rel) per field name across all comparisons.
aggregate :: [FieldDiff] -> [FieldDiff]
aggregate fds =
  [ FieldDiff name (boundaryDumpName b)
              (maximum (map fdAbs g)) (maximum (map fdRel g))
              (maximum (map fdNpts g)) tol (maximum (map fdAbs g) <= tol)
  | (name, _, b, _, tol) <- registry
  , let g = filter ((== name) . fdName) fds
  , not (null g)
  ]
