{-# LANGUAGE BangPatterns #-}
-- | Gradient descent optimization for CLM calibration.
--
-- Built-in optimizer with backtracking line search (Armijo condition).
-- No external optimization library dependencies.
--
-- Ported from Julia: src/calibration/optimize.jl
module CLM.Calibration.Optimize
  ( -- * Result type
    CalibrationResult(..)
    -- * Optimizer
  , gradientDescent
    -- * Parameter transforms
  , ParamTransform(..)
  , transformParam
  , inverseTransformParam
  , defaultTheta
    -- * Helpers
  , gradientNorm
  , clipGradient
  ) where

-- | Result of calibration optimization.
data CalibrationResult = CalibrationResult
  { cr_theta      :: ![Double]              -- ^ Optimized parameter vector
  , cr_objective  :: !Double                -- ^ Final objective value
  , cr_trajectory :: ![(Int, Double, [Double])] -- ^ (iter, obj, theta)
  , cr_converged  :: !Bool
  , cr_iterations :: !Int
  } deriving (Show)

-- | Parameter transform: maps unconstrained theta to physical space.
data ParamTransform
  = PTIdentity !Double            -- ^ val = default * theta (theta=1 at default)
  | PTLog !Double                 -- ^ val = default * exp(theta) (theta=0 at default)
  | PTLogit !Double !Double !Double -- ^ val = lo + (hi-lo)*sigmoid(theta), with default
  deriving (Show)

-- | Transform unconstrained theta to physical parameter value.
transformParam :: ParamTransform -> Double -> Double
transformParam (PTIdentity def) theta = def * theta
transformParam (PTLog def) theta = def * exp theta
transformParam (PTLogit lo hi _def) theta =
  let s = 1.0 / (1.0 + exp (negate theta))
  in lo + (hi - lo) * s

-- | Inverse: physical value to unconstrained theta.
inverseTransformParam :: ParamTransform -> Double -> Double
inverseTransformParam (PTIdentity def) val = val / def
inverseTransformParam (PTLog def) val = log (val / def)
inverseTransformParam (PTLogit lo hi _def) val =
  let p = max 1e-6 (min (1.0 - 1e-6) ((val - lo) / (hi - lo)))
  in log (p / (1.0 - p))

-- | Default theta value (reproduces the default parameter).
defaultTheta :: ParamTransform -> Double
defaultTheta (PTIdentity _) = 1.0
defaultTheta (PTLog _) = 0.0
defaultTheta (PTLogit lo hi def) =
  let p = max 1e-6 (min (1.0 - 1e-6) ((def - lo) / (hi - lo)))
  in log (p / (1.0 - p))

-- | L2 norm of gradient.
gradientNorm :: [Double] -> Double
gradientNorm g = sqrt (sum (map (\x -> x * x) g))

-- | Clip gradient to maximum norm.
clipGradient :: Double -> [Double] -> [Double]
clipGradient maxNorm g =
  let !norm = gradientNorm g
  in if norm > maxNorm
     then map (* (maxNorm / norm)) g
     else g

-- | Gradient descent with backtracking line search (Armijo condition).
--
-- @objectiveFn theta@ returns the scalar cost.
-- @gradientFn theta@ returns the gradient vector.
gradientDescent
  :: ([Double] -> Double)       -- ^ Objective function
  -> ([Double] -> [Double])     -- ^ Gradient function
  -> [Double]                   -- ^ Initial theta
  -> Int                        -- ^ Max iterations
  -> Double                     -- ^ Gradient tolerance (stop when |g| < gtol)
  -> Double                     -- ^ Function tolerance (stop when |f_new - f_old| < ftol)
  -> Double                     -- ^ Max gradient norm (clip above this)
  -> Bool                       -- ^ Verbose
  -> CalibrationResult
gradientDescent objFn gradFn theta0 maxIter gtol ftol maxGrad verbose =
  let !f0 = objFn theta0
      initTraj = [(0, f0, theta0)]
  in go theta0 f0 0 initTraj
  where
    go !theta !fPrev !iter !traj
      | iter >= maxIter =
        CalibrationResult theta fPrev (reverse traj) False iter

      | otherwise =
        let -- Compute and clip gradient
            !rawGrad = gradFn theta
            !g = clipGradient maxGrad rawGrad
            !gNorm = gradientNorm g

        in if gNorm < gtol
           then CalibrationResult theta fPrev (reverse traj) True iter
           else
             -- Backtracking line search (Armijo)
             let !alpha0 = 1.0
                 !c_armijo = 1e-4
                 !rho = 0.5
                 !dirDeriv = negate (sum (map (\x -> x * x) g))

                 lineSearch !alpha
                   | alpha < 1e-12 = (alpha, objFn (step alpha))
                   | otherwise =
                     let !fNew = objFn (step alpha)
                         !sufficient = fNew <= fPrev + c_armijo * alpha * dirDeriv
                     in if sufficient
                        then (alpha, fNew)
                        else lineSearch (rho * alpha)

                 step a = zipWith (\ti gi -> ti - a * gi) theta g

                 (!alphaFinal, !fNew) = lineSearch alpha0
                 !thetaNew = step alphaFinal
                 !fDiff = abs (fNew - fPrev)
                 !newTraj = (iter + 1, fNew, thetaNew) : traj

             in if fDiff < ftol
                then CalibrationResult thetaNew fNew (reverse newTraj) True (iter + 1)
                else go thetaNew fNew (iter + 1) newTraj
