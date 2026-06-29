-- | FATES Global variables and utility functions
module CLM.BioGeoChem.FATES.Globals
  ( fatesLog
  , fatesGlobalVerbose
  , fatesEndrun
  , fatesWarn
  ) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- | Returns log file descriptor unit number
fatesLog :: Int
fatesLog = 6

-- | Verbose logging flag
fatesGlobalVerbose :: Bool
fatesGlobalVerbose = False

-- | End FATES run on error
fatesEndrun :: String -> IO a
fatesEndrun msg = do
  hPutStrLn stderr $ "ENDRUN: " ++ msg
  exitFailure

-- | Print a warning message
fatesWarn :: Int -> String -> IO ()
fatesWarn _id msg = do
  hPutStrLn stderr $ "FATES Warning: " ++ msg
