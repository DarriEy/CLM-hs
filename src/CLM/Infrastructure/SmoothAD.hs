{-# LANGUAGE BangPatterns #-}
-- | Smooth AD primitives for automatic differentiation compatibility.
--
-- Provides C-infinity approximations to min/max/clamp/abs/heaviside/ifelse
-- using LogSumExp and sigmoid functions. When AD is not active, the exact
-- (non-smooth) versions are used for performance and accuracy.
--
-- The smoothing parameter k controls sharpness: larger k = closer to
-- exact but sharper gradient. k=50 gives ~1e-22 approximation error.
--
-- Ported from Julia: src/infrastructure/smooth_ad.jl
module CLM.Infrastructure.SmoothAD
  ( -- * Smooth mode control
    SmoothMode(..)
    -- * Smooth primitives
  , smoothMin
  , smoothMax
  , smoothClamp
  , smoothHeaviside
  , smoothAbs
  , smoothIfElse
    -- * Default sharpness
  , defaultK
  ) where

-- | Smooth evaluation mode.
data SmoothMode
  = SmoothAuto    -- ^ Smooth only when AD types detected (default)
  | SmoothAlways  -- ^ Always use smooth approximations
  | SmoothNever   -- ^ Always use exact functions
  deriving (Show, Eq)

-- | Default sharpness parameter. k=50 gives ~1e-22 max error.
defaultK :: Double
defaultK = 50.0

-- | Smooth approximation to @min(a, b)@ using LogSumExp.
--
-- @smooth_min(a,b) = -(log(exp(-k*a) + exp(-k*b))) / k@
--
-- Numerically stable via max-shift to prevent overflow.
smoothMin :: Double  -- ^ k (sharpness, default 50)
          -> Double  -- ^ a
          -> Double  -- ^ b
          -> Double
smoothMin !k !a !b =
  let !ak = negate k * a
      !bk = negate k * b
      !m = max ak bk
  in negate (m + log (exp (ak - m) + exp (bk - m))) / k
{-# INLINE smoothMin #-}

-- | Smooth approximation to @max(a, b)@ using LogSumExp.
--
-- @smooth_max(a,b) = (log(exp(k*a) + exp(k*b))) / k@
smoothMax :: Double  -- ^ k (sharpness)
          -> Double  -- ^ a
          -> Double  -- ^ b
          -> Double
smoothMax !k !a !b =
  let !ak = k * a
      !bk = k * b
      !m = max ak bk
  in (m + log (exp (ak - m) + exp (bk - m))) / k
{-# INLINE smoothMax #-}

-- | Smooth approximation to @clamp x lo hi@.
--
-- Composition: @smooth_max(lo, smooth_min(x, hi))@
smoothClamp :: Double  -- ^ k
            -> Double  -- ^ lo
            -> Double  -- ^ hi
            -> Double  -- ^ x
            -> Double
smoothClamp !k !lo !hi !x =
  smoothMax k lo (smoothMin k x hi)
{-# INLINE smoothClamp #-}

-- | Smooth Heaviside step function (sigmoid).
--
-- @smooth_heaviside(x) = 1 / (1 + exp(-k*x))@
--
-- Returns ~0 for x << 0, ~1 for x >> 0, 0.5 at x=0.
smoothHeaviside :: Double  -- ^ k
                -> Double  -- ^ x
                -> Double
smoothHeaviside !k !x = 1.0 / (1.0 + exp (negate k * x))
{-# INLINE smoothHeaviside #-}

-- | Smooth approximation to @abs(x)@.
--
-- @smooth_abs(x) = x * tanh(k*x)@
--
-- Equals |x| everywhere except near 0, where the derivative
-- transitions smoothly from -1 to +1.
smoothAbs :: Double  -- ^ k
          -> Double  -- ^ x
          -> Double
smoothAbs !k !x = x * tanh (k * x)
{-# INLINE smoothAbs #-}

-- | Smooth conditional: @if x >= 0 then a else b@.
--
-- @smooth_ifelse(x, a, b) = a * h + b * (1 - h)@
-- where @h = sigmoid(k*x)@.
--
-- Common usage: @smoothIfElse k (t - tfrz) unfrozenVal frozenVal@
smoothIfElse :: Double  -- ^ k
             -> Double  -- ^ x (condition variable)
             -> Double  -- ^ a (value when x >= 0)
             -> Double  -- ^ b (value when x < 0)
             -> Double
smoothIfElse !k !x !a !b =
  let !h = smoothHeaviside k x
  in a * h + b * (1.0 - h)
{-# INLINE smoothIfElse #-}
