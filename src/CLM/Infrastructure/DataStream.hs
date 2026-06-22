{-# LANGUAGE BangPatterns #-}
-- | External data streams with linear time interpolation.
--
-- Fortran reference: @src/cpl/.../mct/.../*StreamMod.F90@ and the shared
-- @shr_stream_*@ utilities, which read a time series of forcing fields from
-- disk (NetCDF) and interpolate them to the model time. In CLM these streams
-- drive time-varying boundary inputs such as nitrogen deposition, aerosol
-- deposition, and the LAI/SAI streams for the satellite-phenology mode.
--
-- This module provides the *math* core of that machinery: a sorted series of
-- @(time, value)@ knots and a pure linear interpolation to an arbitrary model
-- time, with clamping (constant extrapolation) outside the knot range. The
-- physics callers (e.g. the N-deposition path) can build a 'DataStream' from a
-- NetCDF time axis + value array (via "CLM.Infrastructure.NetCDF") or from an
-- in-memory list; the interpolation itself is deterministic and fully
-- unit-testable without any external dataset.
--
-- HONESTY: no external stream dataset or a Fortran @shr_stream@ reference is
-- bundled here, so only the interpolation arithmetic is validated (exact at
-- knots, exact midpoints, clamping outside range). This is NOT a claim of
-- bit-for-bit parity with the Fortran stream reader's calendar handling.
module CLM.Infrastructure.DataStream
  ( -- * Stream type
    DataStream
  , mkDataStream
  , streamKnots
  , streamLength
    -- * Interpolation
  , interpStream
    -- * Convenience: build from a NetCDF-style (time, value) pair of arrays
  , dataStreamFromVectors
    -- * N-deposition stream helper
  , constantStream
  , nDepRateAt
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.List (sortBy)
import Data.Ord (comparing)

-- | A time series of @(time, value)@ knots, sorted ascending by time.
--
-- The invariant (sorted) is established by the smart constructor
-- 'mkDataStream'; do not construct the value directly.
newtype DataStream = DataStream { unDataStream :: [(Double, Double)] }
  deriving (Show, Eq)

-- | Build a stream from an unordered list of @(time, value)@ knots.
-- The knots are sorted ascending by time.
mkDataStream :: [(Double, Double)] -> DataStream
mkDataStream = DataStream . sortBy (comparing fst)

-- | The sorted knots backing a stream.
streamKnots :: DataStream -> [(Double, Double)]
streamKnots = unDataStream

-- | Number of knots in the stream.
streamLength :: DataStream -> Int
streamLength = length . unDataStream

-- | Build a stream from parallel time/value vectors (e.g. a NetCDF time axis
-- and the corresponding value array). The two vectors are zipped to the length
-- of the shorter one and then sorted by time.
dataStreamFromVectors :: VU.Vector Double -> VU.Vector Double -> DataStream
dataStreamFromVectors ts vs =
  mkDataStream (zip (VU.toList ts) (VU.toList vs))

-- | Linearly interpolate the stream value at model time @t@.
--
-- Behaviour:
--
--   * empty stream            -> 'Nothing'
--   * @t@ at or below the first knot time -> first knot value (clamped)
--   * @t@ at or above the last knot time  -> last knot value (clamped)
--   * @t@ between two knots    -> linear interpolation
--   * @t@ exactly at a knot     -> that knot's value (no rounding error)
--
-- Returns 'Just' the interpolated value, or 'Nothing' for an empty stream.
interpStream :: DataStream -> Double -> Maybe Double
interpStream (DataStream knots) t =
  case knots of
    []            -> Nothing
    [(_, v)]      -> Just v
    ((t0, v0) : _)
      | t <= t0   -> Just v0                       -- clamp below range
      | otherwise -> Just (go knots)
  where
    -- Walk the (sorted) knots looking for the bracketing pair.
    go ((ta, va) : rest@((tb, vb) : _))
      | t <= ta            = va                    -- defensive (shouldn't hit)
      | t == tb            = vb                    -- exact at upper knot
      | t < tb             =
          let !dt = tb - ta
          in if dt <= 0.0
               then va                             -- degenerate: identical times
               else va + (vb - va) * (t - ta) / dt
      | otherwise          = go rest
    go [(_, v)]            = v                      -- clamp above range
    go []                  = 0.0                    -- unreachable (>=2 knots)

-- ============================================================================
-- N-deposition stream helper
-- ============================================================================

-- | A degenerate single-knot stream that returns @v@ at every model time.
-- Used as the non-breaking default for the N-deposition path: a constant
-- deposition rate is exactly a constant stream.
constantStream :: Double -> DataStream
constantStream v = DataStream [(0.0, v)]

-- | Nitrogen-deposition rate at model time @t@, driven by a stream.
--
-- If the stream is non-empty, the interpolated value is returned; otherwise the
-- supplied @fallback@ constant rate is used. This is the wiring point for the
-- N-deposition path: passing 'constantStream' (or an empty stream + a constant
-- fallback) reproduces the previous constant-rate behaviour exactly, while a
-- multi-knot stream (e.g. an annual N-deposition series read from NetCDF)
-- drives a time-varying rate.
nDepRateAt
  :: DataStream  -- ^ deposition-rate stream [gN/m^2/s] vs model time
  -> Double      -- ^ fallback constant rate [gN/m^2/s] (used if stream empty)
  -> Double      -- ^ model time
  -> Double
nDepRateAt stream fallback t =
  case interpStream stream t of
    Just v  -> v
    Nothing -> fallback
