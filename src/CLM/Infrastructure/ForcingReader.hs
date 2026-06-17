-- | Atmospheric forcing data structures and pure transforms.
-- Ported from Julia: src/infrastructure/forcing_reader.jl
-- No Fortran equivalent (CESM coupler provides forcings).
--
-- Binary I/O reads forcing data exported by Julia's export_test_data.jl:
--   forcing/tbot.bin, forcing/psrf.bin, forcing/wind.bin, forcing/flds.bin,
--   forcing/fsds.bin, forcing/precip.bin, forcing/qbot.bin
-- Each file contains ntimes Float64 values in little-endian order.
module CLM.Infrastructure.ForcingReader
  ( -- * Data types
    ForcingTimestep(..)
  , defaultForcingTimestep
  , ForcingReaderState(..)
  , defaultForcingReaderState
    -- * Pure forcing processing
  , partitionPrecip
  , computeVaporPressureFromQ
  , computeVaporPressureFromRH
  , computePotentialTemperature
  , computeAirDensity
  , applyForcingDefaults
  , splitShortwaveBands
    -- * Binary I/O
  , forcingReaderInit
  , forcingReaderInitBinary
  , readForcingStep
  , readForcingStepPure
  , forcingReaderClose
    -- * Constants
  , defaultRefHeight
  , defaultCO2Partial
  , defaultO2Partial
  ) where

import qualified Data.Vector.Unboxed as VU
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

import CLM.Constants.PhysicalConstants (tfrz, rair, cpair)
import CLM.Infrastructure.BinaryIO (readFloat64Vector)
import CLM.Infrastructure.NetCDF
  ( NcFile, ncOpen, ncClose, ncHasVar, ncReadDouble1D )

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

defaultRefHeight :: Double
defaultRefHeight = 30.0  -- meters

defaultCO2Partial :: Double
defaultCO2Partial = 40.0  -- Pa (~400 ppm at sea level)

defaultO2Partial :: Double
defaultO2Partial = 20900.0  -- Pa (~20.9% of 100 kPa)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

-- | One timestep of atmospheric forcing data (gridcell level).
data ForcingTimestep = ForcingTimestep
  { ft_tbot    :: !Double  -- ^ Temperature [K]
  , ft_psrf    :: !Double  -- ^ Surface pressure [Pa]
  , ft_wind    :: !Double  -- ^ Wind speed [m/s]
  , ft_flds    :: !Double  -- ^ Downward longwave radiation [W/m2]
  , ft_fsds    :: !Double  -- ^ Downward shortwave radiation [W/m2]
  , ft_precip  :: !Double  -- ^ Total precipitation [mm/s]
  , ft_qbot    :: !Double  -- ^ Specific humidity [kg/kg] (or -1 if RH used)
  , ft_rh      :: !Double  -- ^ Relative humidity [%] (or -1 if QBOT used)
  } deriving (Show)

defaultForcingTimestep :: ForcingTimestep
defaultForcingTimestep = ForcingTimestep
  { ft_tbot   = 270.0
  , ft_psrf   = 85000.0
  , ft_wind   = 3.0
  , ft_flds   = 250.0
  , ft_fsds   = 0.0
  , ft_precip = 0.0
  , ft_qbot   = 0.003
  , ft_rh     = -1.0
  }

-- | State of the forcing reader.
-- When initialized from binary files, holds all forcing data in memory.
data ForcingReaderState = ForcingReaderState
  { frs_timeIndex :: !Int
  , frs_ntimes    :: !Int
  , frs_filepath  :: !String
  -- In-memory forcing arrays (populated by forcingReaderInitBinary)
  , frs_tbot      :: !(VU.Vector Double)
  , frs_psrf      :: !(VU.Vector Double)
  , frs_wind      :: !(VU.Vector Double)
  , frs_flds      :: !(VU.Vector Double)
  , frs_fsds      :: !(VU.Vector Double)
  , frs_precip    :: !(VU.Vector Double)
  , frs_qbot      :: !(VU.Vector Double)
  } deriving (Show)

defaultForcingReaderState :: ForcingReaderState
defaultForcingReaderState = ForcingReaderState
  { frs_timeIndex = 0
  , frs_ntimes    = 0
  , frs_filepath  = ""
  , frs_tbot      = VU.empty
  , frs_psrf      = VU.empty
  , frs_wind      = VU.empty
  , frs_flds      = VU.empty
  , frs_fsds      = VU.empty
  , frs_precip    = VU.empty
  , frs_qbot      = VU.empty
  }

-- ---------------------------------------------------------------------------
-- Pure forcing processing
-- ---------------------------------------------------------------------------

-- | Partition total precipitation into rain and snow based on temperature.
partitionPrecip
  :: Double           -- ^ temperature [K]
  -> Double           -- ^ total precipitation [mm/s]
  -> (Double, Double) -- ^ (rain, snow) [mm/s]
partitionPrecip tbot precip
  | precip < 0.0 = (0.0, 0.0)
  | tbot > tfrz  = (precip, 0.0)
  | otherwise     = (0.0, precip)

-- | Compute vapor pressure from specific humidity and pressure.
-- e = q * p / (0.622 + 0.378 * q)
computeVaporPressureFromQ :: Double -> Double -> Double
computeVaporPressureFromQ qbot pbot =
  qbot * pbot / (0.622 + 0.378 * qbot)

-- | Compute vapor pressure from relative humidity and temperature.
-- Uses Tetens formula for saturation vapor pressure.
computeVaporPressureFromRH :: Double -> Double -> Double
computeVaporPressureFromRH rhPct tbot =
  let rh = rhPct / 100.0
      tc = tbot - tfrz
      esat = 611.0 * exp (17.27 * tc / (tc + 237.3))
  in rh * esat

-- | Compute potential temperature.
-- th = T * (100000 / pbot) ^ (Rd / cp)
computePotentialTemperature :: Double -> Double -> Double
computePotentialTemperature tbot pbot =
  tbot * (100000.0 / pbot) ** (rair / cpair)

-- | Compute air density from equation of state.
-- rho = (p - 0.378 * e) / (Rd * T)
computeAirDensity :: Double -> Double -> Double -> Double
computeAirDensity pbot tbot vp =
  (pbot - 0.378 * vp) / (rair * tbot)

-- | Split total shortwave into VIS/NIR direct/diffuse bands.
-- Returns (solad_vis, solad_nir, solai_vis, solai_nir).
splitShortwaveBands :: Double -> (Double, Double, Double, Double)
splitShortwaveBands fsds =
  ( fsds * 0.50  -- VIS direct
  , fsds * 0.20  -- NIR direct
  , fsds * 0.20  -- VIS diffuse
  , fsds * 0.10  -- NIR diffuse
  )

-- | Apply default values for reference heights, CO2, O2.
-- Returns (forc_hgt, forc_pco2, forc_po2) with defaults applied.
applyForcingDefaults
  :: Double           -- ^ current forc_hgt
  -> Double           -- ^ current forc_pco2
  -> Double           -- ^ current forc_po2
  -> (Double, Double, Double)
applyForcingDefaults hgt pco2 po2 =
  ( if hgt  <= 0.0 then defaultRefHeight  else hgt
  , if pco2 <= 0.0 then defaultCO2Partial else pco2
  , if po2  <= 0.0 then defaultO2Partial  else po2
  )

-- ---------------------------------------------------------------------------
-- Binary I/O
-- ---------------------------------------------------------------------------

-- | Initialize forcing reader from either an exported binary directory or NetCDF file.
forcingReaderInit :: FilePath -> IO ForcingReaderState
forcingReaderInit fp
  | null fp = return defaultForcingReaderState
  | otherwise = do
      isDir <- doesDirectoryExist fp
      if isDir
        then forcingReaderInitBinary fp
        else forcingReaderInitNetCDF fp

-- | Initialize forcing reader from a directory of binary files.
-- Reads all forcing data into memory for fast access.
--
-- Expected files in the directory:
--   tbot.bin, psrf.bin, wind.bin, flds.bin, fsds.bin, precip.bin, qbot.bin
forcingReaderInitBinary :: FilePath -> IO ForcingReaderState
forcingReaderInitBinary dir = do
  tbot   <- readFloat64Vector (dir </> "tbot.bin")
  psrf   <- readFloat64Vector (dir </> "psrf.bin")
  wind   <- readFloat64Vector (dir </> "wind.bin")
  flds   <- readFloat64Vector (dir </> "flds.bin")
  fsds   <- readFloat64Vector (dir </> "fsds.bin")
  precip <- readFloat64Vector (dir </> "precip.bin")
  qbot   <- readFloat64Vector (dir </> "qbot.bin")
  let n = VU.length tbot
  return $! ForcingReaderState
    { frs_timeIndex = 0
    , frs_ntimes    = n
    , frs_filepath  = dir
    , frs_tbot      = tbot
    , frs_psrf      = psrf
    , frs_wind      = wind
    , frs_flds      = flds
    , frs_fsds      = fsds
    , frs_precip    = precip
    , frs_qbot      = qbot
    }

-- | Read one forcing timestep from binary data.
-- The time index is 0-based. Returns updated state with incremented index.
readForcingStep :: ForcingReaderState -> Int -> IO (ForcingTimestep, ForcingReaderState)
readForcingStep frs targetIdx =
  return $ readForcingStepPure frs targetIdx

-- | Pure version: extract one timestep from in-memory forcing arrays.
readForcingStepPure :: ForcingReaderState -> Int -> (ForcingTimestep, ForcingReaderState)
readForcingStepPure frs targetIdx =
  let idx = if targetIdx >= 0 && targetIdx < frs_ntimes frs
            then targetIdx
            else frs_timeIndex frs
      ft = if VU.null (frs_tbot frs)
           then defaultForcingTimestep
           else ForcingTimestep
             { ft_tbot   = frs_tbot   frs VU.! idx
             , ft_psrf   = frs_psrf   frs VU.! idx
             , ft_wind   = frs_wind   frs VU.! idx
             , ft_flds   = frs_flds   frs VU.! idx
             , ft_fsds   = frs_fsds   frs VU.! idx
             , ft_precip = frs_precip frs VU.! idx
             , ft_qbot   = frs_qbot   frs VU.! idx
             , ft_rh     = -1.0
             }
      frs' = frs { frs_timeIndex = idx + 1 }
  in (ft, frs')

-- | Close forcing reader. Data-backed readers keep arrays in memory, so there is
-- no open handle after initialization.
forcingReaderClose :: ForcingReaderState -> IO ()
forcingReaderClose _ = return ()

forcingReaderInitNetCDF :: FilePath -> IO ForcingReaderState
forcingReaderInitNetCDF fp = do
  opened <- ncOpen fp
  case opened of
    Left err -> ioError (userError err)
    Right nc -> do
      tbot <- readRequired nc ["TBOT", "tbot"]
      psrf <- readRequired nc ["PSRF", "psrf", "PBOT", "pbot"]
      wind <- readRequired nc ["WIND", "wind"]
      flds <- readRequired nc ["FLDS", "flds"]
      fsds <- readRequired nc ["FSDS", "fsds"]
      precip <- readRequired nc ["PRECTmms", "PRECIP", "precip"]
      qbot <- readRequired nc ["QBOT", "qbot"]
      ncClose nc
      let n = minimum (map VU.length [tbot, psrf, wind, flds, fsds, precip, qbot])
          trim = VU.take n
      return $! ForcingReaderState
        { frs_timeIndex = 0
        , frs_ntimes    = n
        , frs_filepath  = fp
        , frs_tbot      = trim tbot
        , frs_psrf      = trim psrf
        , frs_wind      = trim wind
        , frs_flds      = trim flds
        , frs_fsds      = trim fsds
        , frs_precip    = trim precip
        , frs_qbot      = trim qbot
        }

readRequired :: NcFile -> [String] -> IO (VU.Vector Double)
readRequired nc names = do
  found <- firstExisting names
  case found of
    Nothing -> ioError (userError ("missing NetCDF variable; tried " ++ show names))
    Just name -> do
      vals <- ncReadDouble1D nc name
      case vals of
        Left err -> ioError (userError err)
        Right v  -> return v
  where
    firstExisting [] = return Nothing
    firstExisting (name:rest) = do
      exists <- ncHasVar nc name
      if exists then return (Just name) else firstExisting rest
