{-# LANGUAGE BangPatterns #-}
-- | FATES Numerical Integration and Driver Routines
-- Ported from FatesIntegratorsMod.F90
module CLM.BioGeoChem.FATES.Driver
  ( rkf45
  , euler
  , RKF45Result(..)
  ) where

import qualified Data.Vector.Unboxed as VU

-- | RKF45 output signature
data RKF45Result = RKF45Result
  { rkfYout  :: !(VU.Vector Double)
  , rkfOptDx :: !Double
  , rkfPass  :: !Bool
  } deriving (Show, Eq)

-- | Runge-Kutta-Fehlberg 4/5 order adaptive explicit integration.
-- Maps to RKF45 in FatesIntegratorsMod.F90.
rkf45
  :: (VU.Vector Double -> VU.Vector Bool -> Double -> VU.Vector Double -> VU.Vector Double) -- ^ DerivFunction
  -> VU.Vector Double      -- ^ Y dependent variable array
  -> VU.Vector Bool        -- ^ Ymask logical mask
  -> Double                -- ^ dx step size
  -> Double                -- ^ x independent variable
  -> Double                -- ^ max_err allowable error
  -> VU.Vector Double      -- ^ param_array
  -> RKF45Result
rkf45 derivFunction !y !ymask !dx !x !maxErr !paramArray =
  let !len = VU.length y
      !k0 = derivFunction y ymask x paramArray
      
      -- 1st Step
      !y1 = VU.generate len (\i -> (y VU.! i) + dx * (f1_0 * (k0 VU.! i)))
      !x1 = x + t1 * dx
      !k1 = derivFunction y1 ymask x1 paramArray
      
      -- 2nd Step
      !y2 = VU.generate len (\i -> (y VU.! i) + dx * (f2_0 * (k0 VU.! i) + f2_1 * (k1 VU.! i)))
      !x2 = x + t2 * dx
      !k2 = derivFunction y2 ymask x2 paramArray
      
      -- 3rd Step
      !y3 = VU.generate len (\i -> (y VU.! i) + dx * (f3_0 * (k0 VU.! i) + f3_1 * (k1 VU.! i) + f3_2 * (k2 VU.! i)))
      !x3 = x + t3 * dx
      !k3 = derivFunction y3 ymask x3 paramArray
      
      -- 4th Step
      !y4 = VU.generate len (\i -> (y VU.! i) + dx * (f4_0 * (k0 VU.! i) + f4_1 * (k1 VU.! i) + f4_2 * (k2 VU.! i) + f4_3 * (k3 VU.! i)))
      !x4 = x + t4 * dx
      !k4 = derivFunction y4 ymask x4 paramArray
      
      -- 5th Step
      !y5 = VU.generate len (\i -> (y VU.! i) + dx * (f5_0 * (k0 VU.! i) + f5_1 * (k1 VU.! i) + f5_2 * (k2 VU.! i) + f5_3 * (k3 VU.! i) + f5_4 * (k4 VU.! i)))
      !x5 = x + t5 * dx
      !k5 = derivFunction y5 ymask x5 paramArray
      
      -- Evaluate 4th order
      !y4th = VU.generate len (\i -> (y VU.! i) + dx * (y_0 * (k0 VU.! i) + y_2 * (k2 VU.! i) + y_3 * (k3 VU.! i) + y_4 * (k4 VU.! i)))
      
      -- Evaluate 5th order
      !y5th = VU.generate len (\i -> (y VU.! i) + dx * (z_0 * (k0 VU.! i) + z_2 * (k2 VU.! i) + z_3 * (k3 VU.! i) + z_4 * (k4 VU.! i) + z_5 * (k5 VU.! i)))
      
      -- Maximum absolute error
      !err45 = if len == 0
                 then 0.0
                 else VU.maximum (VU.generate len (\i -> abs ((y5th VU.! i) - (y4th VU.! i))))
      
      !optDx = dx * max minStepFraction (0.840896 * (maxErr / max err45 (0.00001 * maxErr)) ** 0.25)
      !pass = err45 <= maxErr
  in RKF45Result y5th optDx pass
  where
    minStepFraction = 0.25
    
    t1   = 1.0/4.0
    f1_0 = 1.0/4.0
    
    t2   = 3.0/8.0
    f2_0 = 3.0/32.0
    f2_1 = 9.0/32.0
    
    t3   = 12.0/13.0
    f3_0 = 1932.0/2197.0
    f3_1 = -7200.0/2197.0
    f3_2 = 7296.0/2197.0
    
    t4   = 1.0
    f4_0 = 439.0/216.0
    f4_1 = -8.0
    f4_2 = 3680.0/513.0
    f4_3 = -845.0/4104.0
    
    t5   = 0.5
    f5_0 = -8.0/27.0
    f5_1 = 2.0
    f5_2 = -3544.0/2565.0
    f5_3 = 1859.0/4104.0
    f5_4 = -11.0/40.0
    
    y_0 = 25.0/216.0
    y_2 = 1408.0/2565.0
    y_3 = 2197.0/4104.0
    y_4 = -1.0/5.0
    
    z_0 = 16.0/135.0
    z_2 = 6656.0/12825.0
    z_3 = 28561.0/56430.0
    z_4 = -9.0/50.0
    z_5 = 2.0/55.0

-- | Simple Euler integration step.
-- Maps to Euler in FatesIntegratorsMod.F90.
euler
  :: (VU.Vector Double -> VU.Vector Bool -> Double -> VU.Vector Double -> VU.Vector Double) -- ^ DerivFunction
  -> VU.Vector Double      -- ^ Y dependent variable array
  -> VU.Vector Bool        -- ^ Ymask logical mask
  -> Double                -- ^ dx step size
  -> Double                -- ^ x independent variable
  -> VU.Vector Double      -- ^ param_array
  -> VU.Vector Double      -- ^ Yout
euler derivFunction !y !ymask !dx !x !paramArray =
  let !len = VU.length y
      !dYdx = derivFunction y ymask x paramArray
  in VU.generate len (\i -> (y VU.! i) + dx * (dYdx VU.! i))
