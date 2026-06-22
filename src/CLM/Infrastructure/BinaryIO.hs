-- | Binary I/O utilities for reading raw Float64/Int64 arrays from files.
-- Used by ForcingReader, SurfData, and ReadParams to read data exported
-- from Julia's export_test_data.jl script.
--
-- File format: raw little-endian Float64 or Int64 values, no header.
module CLM.Infrastructure.BinaryIO
  ( -- * Reading
    readFloat64Vector
  , readFloat64Scalar
  , readInt64Vector
  , readFloat64Matrix
    -- * Writing
  , writeFloat64Vector
    -- * JSON manifest
  , ManifestDims(..)
  , readManifestDims
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get (runGet, getDoublele, getInt64le)
import Data.Binary.Put (runPut, putDoublele)
import qualified Data.Vector.Unboxed as VU

-- | Write an unboxed vector as raw little-endian Float64 values (no header),
-- matching the format 'readFloat64Vector' expects. Round-trips exactly.
writeFloat64Vector :: FilePath -> VU.Vector Double -> IO ()
writeFloat64Vector fp v =
  BL.writeFile fp $ runPut (mapM_ putDoublele (VU.toList v))

-- ---------------------------------------------------------------------------
-- Binary reading
-- ---------------------------------------------------------------------------

-- | Read a file of raw little-endian Float64 values into an unboxed vector.
readFloat64Vector :: FilePath -> IO (VU.Vector Double)
readFloat64Vector fp = do
  bs <- BL.readFile fp
  let n = fromIntegral (BL.length bs) `div` 8
      vals = runGet (sequence (replicate n getDoublele)) bs
  return $! VU.fromList vals

-- | Read a single Float64 scalar from a binary file.
readFloat64Scalar :: FilePath -> IO Double
readFloat64Scalar fp = do
  bs <- BL.readFile fp
  return $! runGet getDoublele bs

-- | Read a file of raw little-endian Int64 values into an unboxed vector.
readInt64Vector :: FilePath -> IO (VU.Vector Int)
readInt64Vector fp = do
  bs <- BL.readFile fp
  let n = fromIntegral (BL.length bs) `div` 8
      vals = runGet (sequence (replicate n (fromIntegral <$> getInt64le))) bs
  return $! VU.fromList vals

-- | Read a Float64 matrix stored in column-major order.
-- Returns (nrows, ncols, flat vector in row-major order for Haskell).
readFloat64Matrix :: FilePath -> Int -> Int -> IO (VU.Vector Double)
readFloat64Matrix fp nrows ncols = do
  v <- readFloat64Vector fp
  -- Julia stores column-major; we convert to row-major
  -- v[col * nrows + row] -> result[row * ncols + col]
  let result = VU.generate (nrows * ncols) $ \i ->
        let row = i `div` ncols
            col = i `mod` ncols
        in v VU.! (col * nrows + row)
  return $! result

-- ---------------------------------------------------------------------------
-- JSON manifest (minimal parser for dimension extraction)
-- ---------------------------------------------------------------------------

-- | Key dimensions from the manifest.json file.
data ManifestDims = ManifestDims
  { mdNg       :: !Int
  , mdNl       :: !Int
  , mdNc       :: !Int
  , mdNp       :: !Int
  , mdNlevsoi  :: !Int
  , mdNlevgrnd :: !Int
  , mdNlevsno  :: !Int
  , mdNlevtot  :: !Int
  , mdNtimes   :: !Int
  , mdNumpft   :: !Int
  , mdDtime    :: !Int
  , mdYear     :: !Int
  , mdLat      :: !Double
  , mdLon      :: !Double
  } deriving (Show)

-- | Read manifest dimensions from a JSON file.
-- Uses simple string parsing to avoid aeson dependency.
readManifestDims :: FilePath -> IO ManifestDims
readManifestDims fp = do
  content <- readFile fp
  let getInt key = parseJsonInt key content
      getDbl key = parseJsonDouble key content
  return $ ManifestDims
    { mdNg       = getInt "ng"
    , mdNl       = getInt "nl"
    , mdNc       = getInt "nc"
    , mdNp       = getInt "np"
    , mdNlevsoi  = getInt "nlevsoi"
    , mdNlevgrnd = getInt "nlevgrnd"
    , mdNlevsno  = getInt "nlevsno"
    , mdNlevtot  = getInt "nlevtot"
    , mdNtimes   = getInt "ntimes"
    , mdNumpft   = getInt "numpft"
    , mdDtime    = getInt "dtime"
    , mdYear     = getInt "year"
    , mdLat      = getDbl "lat"
    , mdLon      = getDbl "lon"
    }

-- Simple JSON value extraction (avoids aeson dependency)
parseJsonInt :: String -> String -> Int
parseJsonInt key content =
  case findJsonValue key content of
    Just s  -> read (takeWhile (\c -> c /= ',' && c /= '}' && c /= '\n') (dropWhile (== ' ') s))
    Nothing -> error $ "Key not found in manifest: " ++ key

parseJsonDouble :: String -> String -> Double
parseJsonDouble key content =
  case findJsonValue key content of
    Just s  -> read (takeWhile (\c -> c /= ',' && c /= '}' && c /= '\n') (dropWhile (== ' ') s))
    Nothing -> error $ "Key not found in manifest: " ++ key

findJsonValue :: String -> String -> Maybe String
findJsonValue key content =
  let needle = "\"" ++ key ++ "\""
      go [] = Nothing
      go s@(_:rest)
        | take (length needle) s == needle =
            let afterKey = drop (length needle) s
                afterColon = dropWhile (\c -> c == ' ' || c == ':') afterKey
            in Just afterColon
        | otherwise = go rest
  in go content
