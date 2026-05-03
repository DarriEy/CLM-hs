-- | History output data structures and pure accumulation logic.
-- Ported from Julia: src/infrastructure/history_writer.jl
-- All aggregation/accumulation is pure. CSV writing for comparison output.
module CLM.Infrastructure.HistoryWriter
  ( -- * Field definitions
    HistFieldDef(..)
  , SubgridLevel(..)
    -- * Accumulator state
  , HistAccumulator(..)
  , defaultHistAccumulator
    -- * Pure accumulation
  , accumulateStep
  , computeDailyAverage
  , resetAccumulator
    -- * Gridcell aggregation
  , aggregateToGridcell
    -- * Writer state (IO)
  , HistoryWriterState(..)
  , defaultHistoryWriterState
    -- * IO
  , historyWriterInit
  , historyWriterInitCSV
  , historyWriteStep
  , historyWriteCSVRow
  , historyWriterClose
    -- * CSV reference reader
  , readReferenceCSV
  , ReferenceRow(..)
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as Map
import System.IO (Handle, hPutStrLn, hFlush, hClose, openFile, IOMode(..))
import Data.List (intercalate)
import Text.Read (readMaybe)

-- ---------------------------------------------------------------------------
-- Field definitions
-- ---------------------------------------------------------------------------

-- | Subgrid level at which a field is natively defined.
data SubgridLevel = GridcellLevel | ColumnLevel | PatchLevel
  deriving (Show, Eq)

-- | Definition of a single history output field.
data HistFieldDef = HistFieldDef
  { hfd_name      :: !String
  , hfd_long_name :: !String
  , hfd_units     :: !String
  , hfd_level     :: !SubgridLevel
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- Accumulator state (pure)
-- ---------------------------------------------------------------------------

-- | Accumulator for daily-average history output.
data HistAccumulator = HistAccumulator
  { ha_accum      :: !(Map.Map String (VU.Vector Double))  -- ^ Sum per field
  , ha_accumCount :: !Int                                   -- ^ Number of steps accumulated
  , ha_timeIndex  :: !Int                                   -- ^ Current output time index
  } deriving (Show)

defaultHistAccumulator :: HistAccumulator
defaultHistAccumulator = HistAccumulator Map.empty 0 0

-- ---------------------------------------------------------------------------
-- Gridcell aggregation (pure)
-- ---------------------------------------------------------------------------

-- | Aggregate a field from patch or column level to gridcell using area weights.
aggregateToGridcell
  :: SubgridLevel         -- ^ field's native level
  -> Int                  -- ^ ng (number of gridcells)
  -> VU.Vector Int        -- ^ entity -> gridcell mapping (1-based)
  -> VU.Vector Double     -- ^ entity -> wtgcell
  -> VU.Vector Double     -- ^ raw field data
  -> VU.Vector Double     -- ^ gridcell-aggregated values
aggregateToGridcell level ng entityGridcell entityWtgcell rawData =
  case level of
    GridcellLevel ->
      VU.generate ng $ \g ->
        if g < VU.length rawData
        then let v = rawData VU.! g
             in if isFiniteD v then v else fillValue
        else fillValue

    _ ->
      let n = min (VU.length rawData)
                  (min (VU.length entityGridcell) (VU.length entityWtgcell))

          accumPairs =
            [ (entityGridcell VU.! i - 1, (val * wt, wt))
            | i <- [0 .. n - 1]
            , let val = rawData VU.! i
                  wt  = entityWtgcell VU.! i
            , isFiniteD val
            , entityGridcell VU.! i >= 1
            , entityGridcell VU.! i <= ng
            ]

          sumVals = VU.accum (+) (VU.replicate ng 0.0)
                    [(g, v) | (g, (v, _)) <- accumPairs]
          sumWts  = VU.accum (+) (VU.replicate ng 0.0)
                    [(g, w) | (g, (_, w)) <- accumPairs]

      in VU.generate ng $ \g ->
           let w = sumWts VU.! g
           in if w > 0.0
              then (sumVals VU.! g) / w
              else fillValue

fillValue :: Double
fillValue = -9999.0

isFiniteD :: Double -> Bool
isFiniteD x = not (isNaN x) && not (isInfinite x)

-- ---------------------------------------------------------------------------
-- Pure accumulation
-- ---------------------------------------------------------------------------

-- | Accumulate one timestep of gridcell-aggregated values.
accumulateStep
  :: [(String, VU.Vector Double)]  -- ^ (field_name, gridcell_values)
  -> HistAccumulator
  -> HistAccumulator
accumulateStep fieldValues acc =
  let addField m (name, vals) =
        Map.alter (\prev -> case prev of
          Nothing   -> Just vals
          Just prev' -> Just $ VU.zipWith addNonFill prev' vals
          ) name m
      m' = foldl addField (ha_accum acc) fieldValues
  in acc { ha_accum = m', ha_accumCount = ha_accumCount acc + 1 }
  where
    addNonFill a b = if b /= fillValue then a + b else a

-- | Compute daily average from accumulated values. Returns averaged field map.
computeDailyAverage :: HistAccumulator -> Map.Map String (VU.Vector Double)
computeDailyAverage acc
  | ha_accumCount acc <= 0 = ha_accum acc
  | otherwise =
      let n = fromIntegral (ha_accumCount acc) :: Double
      in Map.map (VU.map (/ n)) (ha_accum acc)

-- | Reset accumulator for the next day.
resetAccumulator :: HistAccumulator -> HistAccumulator
resetAccumulator acc =
  acc { ha_accum = Map.map (VU.map (const 0.0)) (ha_accum acc)
      , ha_accumCount = 0
      , ha_timeIndex = ha_timeIndex acc + 1
      }

-- ---------------------------------------------------------------------------
-- IO: CSV writer
-- ---------------------------------------------------------------------------

-- | Writer state for CSV output.
data HistoryWriterState = HistoryWriterState
  { hws_filepath :: !String
  , hws_fields   :: ![HistFieldDef]
  , hws_accum    :: !HistAccumulator
  , hws_handle   :: !(Maybe Handle)
  }

instance Show HistoryWriterState where
  show hws = "HistoryWriterState { filepath=" ++ hws_filepath hws
          ++ ", fields=" ++ show (length (hws_fields hws))
          ++ ", accum=" ++ show (hws_accum hws) ++ " }"

defaultHistoryWriterState :: HistoryWriterState
defaultHistoryWriterState = HistoryWriterState "" [] defaultHistAccumulator Nothing

-- | Legacy placeholder: create history output file (no-op).
historyWriterInit :: FilePath -> [HistFieldDef] -> Int -> IO HistoryWriterState
historyWriterInit fp fields _ng =
  return $ HistoryWriterState fp fields defaultHistAccumulator Nothing

-- | Initialize a CSV history writer.
-- Writes a header row with field names.
historyWriterInitCSV :: FilePath -> [String] -> IO HistoryWriterState
historyWriterInitCSV fp fieldNames = do
  h <- openFile fp WriteMode
  hPutStrLn h $ "day," ++ intercalate "," fieldNames
  hFlush h
  let fields = map (\n -> HistFieldDef n n "" GridcellLevel) fieldNames
  return $ HistoryWriterState fp fields defaultHistAccumulator (Just h)

-- | Legacy placeholder: write one timestep.
historyWriteStep :: HistoryWriterState -> Bool -> IO HistoryWriterState
historyWriteStep hws _isEndDay = return hws

-- | Write one row to the CSV file.
-- @dayIndex@ is the 1-based day number.
-- @values@ is a map of field_name -> gridcell value (first gridcell only).
historyWriteCSVRow :: HistoryWriterState -> Int -> Map.Map String Double -> IO ()
historyWriteCSVRow hws dayIdx values =
  case hws_handle hws of
    Nothing -> return ()
    Just h -> do
      let fieldNames = map hfd_name (hws_fields hws)
          vals = map (\n -> case Map.lookup n values of
                              Just v  -> showDouble v
                              Nothing -> showDouble fillValue
                     ) fieldNames
      hPutStrLn h $ show dayIdx ++ "," ++ intercalate "," vals
      hFlush h

-- | Close the CSV history writer.
historyWriterClose :: HistoryWriterState -> IO ()
historyWriterClose hws =
  case hws_handle hws of
    Nothing -> return ()
    Just h  -> hClose h

showDouble :: Double -> String
showDouble x = let s = show x
               in if 'e' `elem` s || 'E' `elem` s then s
                  else s  -- Haskell show already handles this

-- ---------------------------------------------------------------------------
-- CSV reference reader
-- ---------------------------------------------------------------------------

-- | A row from the Julia reference CSV.
data ReferenceRow = ReferenceRow
  { rr_day    :: !Int
  , rr_values :: !(Map.Map String Double)
  } deriving (Show)

-- | Read a Julia reference CSV file (julia_daily_avg.csv).
-- Returns list of rows with field name -> value maps.
readReferenceCSV :: FilePath -> IO [ReferenceRow]
readReferenceCSV fp = do
  content <- readFile fp
  let lns = lines content
  case lns of
    [] -> return []
    (header:dataLines) ->
      let fieldNames = drop 1 $ splitOn ',' header  -- skip "day"
          parseRow line =
            let parts = splitOn ',' line
            in case parts of
              (dayStr:valStrs) ->
                case readMaybe dayStr of
                  Just d ->
                    let vals = zip fieldNames (map parseDouble valStrs)
                    in Just $ ReferenceRow d (Map.fromList vals)
                  Nothing -> Nothing
              _ -> Nothing
      in return $ concatMap (maybe [] (:[]) . parseRow) dataLines

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn c s  =
  let (pre, rest) = break (== c) s
  in pre : case rest of
             []    -> []
             (_:t) -> splitOn c t

parseDouble :: String -> Double
parseDouble s = case readMaybe s of
  Just d  -> d
  Nothing -> fillValue
