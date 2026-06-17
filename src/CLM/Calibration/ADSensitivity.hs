{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
-- | Automatic differentiation for CLM parameter sensitivity.
--
-- Uses the @ad@ library (forward-mode AD) to compute gradients of
-- CLM output variables (GPP, NEE, soil temperature) with respect to
-- model parameters (leaf longevity, Q10, allocation fractions, etc.).
--
-- The key insight: since our CN physics functions are pure and use
-- standard Haskell numeric operations, the @ad@ library can differentiate
-- through them automatically by replacing Double with Dual numbers.
--
-- This module provides:
--   * A differentiable CN timestep function
--   * Parameter sensitivity computation (Jacobian)
--   * Gradient of a scalar cost function w.r.t. parameters
module CLM.Calibration.ADSensitivity
  ( -- * Parameter vector
    CNParams(..)
  , defaultCNParams
  , cnParamsToList
  , cnParamsFromList
    -- * Differentiable CN timestep
  , CNState(..)
  , defaultCNState
  , cnTimestep
    -- * Sensitivity analysis
  , computeSensitivities
  , computeGradient
    -- * Cost function
  , CostFunction
  , rmseCost
  ) where

import Numeric.AD (grad, jacobian)

-- =========================================================================
-- CN Parameters (the quantities we want to differentiate w.r.t.)
-- =========================================================================

data CNParams a = CNParams
  { cp_leaf_long    :: !a  -- ^ Leaf longevity (years)
  , cp_froot_leaf   :: !a  -- ^ Fine root to leaf allocation ratio
  , cp_stem_leaf    :: !a  -- ^ Stem to leaf allocation ratio
  , cp_q10_hr       :: !a  -- ^ Q10 for heterotrophic respiration
  , cp_litter_tau   :: !a  -- ^ Litter turnover time (years)
  , cp_som_tau      :: !a  -- ^ SOM turnover time (years)
  , cp_mr_base      :: !a  -- ^ Base maintenance respiration rate (gC/gC/s)
  , cp_grperc       :: !a  -- ^ Growth respiration fraction
  , cp_lue          :: !a  -- ^ Light use efficiency (gC/J PAR)
  } deriving (Show)

defaultCNParams :: Fractional a => CNParams a
defaultCNParams = CNParams
  { cp_leaf_long  = 2.0
  , cp_froot_leaf = 1.0
  , cp_stem_leaf  = 1.5
  , cp_q10_hr     = 2.0
  , cp_litter_tau = 1.0
  , cp_som_tau    = 20.0
  , cp_mr_base    = 2.525e-6
  , cp_grperc     = 0.3
  , cp_lue        = 1.5e-6
  }

cnParamsToList :: CNParams a -> [a]
cnParamsToList p =
  [ cp_leaf_long p, cp_froot_leaf p, cp_stem_leaf p
  , cp_q10_hr p, cp_litter_tau p, cp_som_tau p
  , cp_mr_base p, cp_grperc p, cp_lue p
  ]

cnParamsFromList :: [a] -> CNParams a
cnParamsFromList [a,b,c,d,e,f,g,h,i] = CNParams a b c d e f g h i
cnParamsFromList _ = error "cnParamsFromList: expected 9 elements"

-- =========================================================================
-- CN State (pools)
-- =========================================================================

data CNState a = CNState
  { cs_leafc    :: !a
  , cs_frootc   :: !a
  , cs_stemc    :: !a
  , cs_litterc  :: !a
  , cs_soilorgc :: !a
  , cs_sminn    :: !a
  } deriving (Show)

defaultCNState :: Fractional a => CNState a
defaultCNState = CNState
  { cs_leafc    = 200.0
  , cs_frootc   = 150.0
  , cs_stemc    = 500.0
  , cs_litterc  = 300.0
  , cs_soilorgc = 8000.0
  , cs_sminn    = 5.0
  }

cnStateToList :: CNState a -> [a]
cnStateToList s =
  [cs_leafc s, cs_frootc s, cs_stemc s, cs_litterc s, cs_soilorgc s, cs_sminn s]

-- =========================================================================
-- Differentiable CN timestep
-- =========================================================================

-- | A single CN timestep, written in terms of generic Floating operations
-- so that @ad@ can differentiate through it.
--
-- Takes parameters, current state, and environmental drivers.
-- Returns updated state + diagnostic fluxes (GPP, NPP, NEE, HR).
cnTimestep :: (Floating a, Ord a)
           => CNParams a
           -> CNState a
           -> a          -- ^ dt (seconds)
           -> a          -- ^ par (PAR, W/m2)
           -> a          -- ^ t_soil (soil temperature, K)
           -> a          -- ^ lai (leaf area index)
           -> (CNState a, a, a, a, a)  -- ^ (state', gpp, npp, nee, hr)
cnTimestep !params !state !dt !par !t_soil !lai =
  let tfrz_a = 273.15

      -- GPP: light use efficiency model
      !gpp = cp_lue params * par * smoothMaxG 0.0 (lai / 4.0)
           * smoothMaxG 0.0 (1.0 - (tfrz_a - 5.0 - t_soil) / 10.0)

      -- Maintenance respiration
      !mr = cp_mr_base params * (cs_leafc state + cs_frootc state + cs_stemc state * 0.5)

      -- Available C
      !availC = smoothMaxG 0.0 (gpp - mr)

      -- Growth respiration
      !gr = availC * cp_grperc params / (1.0 + cp_grperc params)

      -- Allocation (allometric)
      !f_leaf = 1.0
      !f_froot = cp_froot_leaf params
      !f_stem = cp_stem_leaf params
      !total_allom = f_leaf + f_froot + f_stem
      !c_for_growth = availC / (1.0 + cp_grperc params)
      !aleaf = c_for_growth * f_leaf / total_allom
      !afroot = c_for_growth * f_froot / total_allom
      !astem = c_for_growth * f_stem / total_allom

      -- Litterfall
      !secspyear = 365.0 * 86400.0
      !leafLitRate = cs_leafc state / (cp_leaf_long params * secspyear)
      !frootLitRate = cs_frootc state / (cp_leaf_long params * 0.75 * secspyear)

      -- Decomposition with Q10 temperature sensitivity
      !q10 = cp_q10_hr params ** ((t_soil - tfrz_a - 25.0) / 10.0)
      !q10_safe = smoothMaxG 0.01 q10
      !hr_litter = cs_litterc state / (cp_litter_tau params * secspyear) * q10_safe
      !hr_som = cs_soilorgc state / (cp_som_tau params * secspyear) * q10_safe
      !hr = hr_litter + hr_som
      !litToSom = hr_litter * 0.6

      -- N mineralization
      !nMin = hr / 15.0

      -- NPP and NEE
      !npp = gpp - mr - gr
      !nee = negate (gpp - mr - gr - hr)

      -- Update pools
      !leafC' = smoothMaxG 0.0 (cs_leafc state + (aleaf - leafLitRate) * dt)
      !frootC' = smoothMaxG 0.0 (cs_frootc state + (afroot - frootLitRate) * dt)
      !stemC' = smoothMaxG 0.0 (cs_stemc state + astem * dt)
      !litterC' = smoothMaxG 0.0 (cs_litterc state + (leafLitRate + frootLitRate - hr_litter) * dt)
      !somC' = smoothMaxG 0.0 (cs_soilorgc state + (litToSom - hr_som) * dt)
      !sminn' = smoothMaxG 0.0 (cs_sminn state + nMin * dt)

      !state' = CNState leafC' frootC' stemC' litterC' somC' sminn'

  in (state', gpp, npp, nee, hr)

-- | Smooth max for generic Floating types (AD-compatible).
smoothMaxG :: (Floating a, Ord a) => a -> a -> a
smoothMaxG a b =
  let k = 50.0
      ak = k * a
      bk = k * b
      m = if ak > bk then ak else bk
  in (m + log (exp (ak - m) + exp (bk - m))) / k
{-# INLINE smoothMaxG #-}

-- =========================================================================
-- Sensitivity analysis
-- =========================================================================

-- | Compute the Jacobian of CN state after one timestep w.r.t. parameters.
-- Returns a matrix: rows = output state variables, cols = parameters.
computeSensitivities
  :: CNState Double  -- ^ Initial state
  -> Double          -- ^ dt
  -> Double          -- ^ par
  -> Double          -- ^ t_soil
  -> Double          -- ^ lai
  -> [[Double]]      -- ^ Jacobian matrix (6 outputs x 9 params)
computeSensitivities state0 dt par t_soil lai =
  jacobian f (cnParamsToList (defaultCNParams :: CNParams Double))
  where
    f :: (Floating a, Ord a) => [a] -> [a]
    f paramList =
      let params = cnParamsFromList paramList
          state = CNState
            { cs_leafc = realToFrac (cs_leafc state0)
            , cs_frootc = realToFrac (cs_frootc state0)
            , cs_stemc = realToFrac (cs_stemc state0)
            , cs_litterc = realToFrac (cs_litterc state0)
            , cs_soilorgc = realToFrac (cs_soilorgc state0)
            , cs_sminn = realToFrac (cs_sminn state0)
            }
          (state', _gpp, _npp, _nee, _hr) = cnTimestep params state
            (realToFrac dt) (realToFrac par) (realToFrac t_soil) (realToFrac lai)
      in cnStateToList state'

-- =========================================================================
-- Gradient computation
-- =========================================================================

type CostFunction = CNParams Double -> Double

-- | Compute the gradient of a scalar cost function w.r.t. CN parameters.
-- The cost function must be written in terms of generic Floating operations.
computeGradient
  :: (forall a. (Floating a, Ord a) => [a] -> a)  -- ^ cost as function of param list
  -> CNParams Double  -- ^ Parameter values
  -> [Double]         -- ^ Gradient (9 elements)
computeGradient costFn params =
  grad costFn (cnParamsToList params)

-- | RMSE cost function: run N timesteps, compare NEE against observations.
rmseCost :: [(Double, Double, Double)]  -- ^ [(par, t_soil, lai)] per timestep
         -> [Double]                    -- ^ observed NEE per timestep
         -> Double                      -- ^ dt
         -> CNParams Double             -- ^ parameters
         -> Double                      -- ^ RMSE
rmseCost drivers obsNEE dt params =
  let nsteps = length drivers
      go _state [] [] !acc = acc
      go state ((par, tsoil, lai):ds) (obs:os) !acc =
        let (state', _gpp, _npp, nee, _hr) = cnTimestep params state dt par tsoil lai
            !err = (nee - obs) ** 2
        in go state' ds os (acc + err)
      go _ _ _ !acc = acc
      !sse = go (defaultCNState :: CNState Double) drivers obsNEE 0.0
  in sqrt (sse / fromIntegral nsteps)
