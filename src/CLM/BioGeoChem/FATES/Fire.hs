{-# LANGUAGE BangPatterns #-}
-- | FATES Fuel and Fire Dynamics
-- Ported from FatesFuelMod.F90 & FatesFuelClassesMod.F90
module CLM.BioGeoChem.FATES.Fire
  ( -- * Fuel Classes
    numFuelClasses
  , idxTwigs
  , idxSmallBranches
  , idxLargeBranches
  , idxTrunks
  , idxDeadLeaves
  , idxLiveGrass
  
  -- * Calculations
  , calculateFuelMoistureNesterov
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Total number of fuel classes
numFuelClasses :: Int
numFuelClasses = 6

-- | 0-based indices for fuel classes
idxTwigs, idxSmallBranches, idxLargeBranches, idxTrunks, idxDeadLeaves, idxLiveGrass :: Int
idxTwigs         = 0
idxSmallBranches = 1
idxLargeBranches = 2
idxTrunks        = 3
idxDeadLeaves    = 4
idxLiveGrass     = 5

-- | Update fuel moisture using the Nesterov Index.
-- Maps to CalculateFuelMoistureNesterov in FatesFuelMod.F90.
calculateFuelMoistureNesterov
  :: VU.Vector Double -- ^ sav_fuel surface area to volume ratio of all fuel types [/cm] (length 6)
  -> Double           -- ^ drying_ratio
  -> Double           -- ^ NI Nesterov Index
  -> VU.Vector Double -- ^ moisture of litter [m3/m3] (length 6)
calculateFuelMoistureNesterov !savFuel !dryingRatio !ni =
  VU.generate numFuelClasses $ \i ->
    let !alphaFmc = if i == idxLiveGrass
                      then (savFuel VU.! idxTwigs) / dryingRatio
                      else (savFuel VU.! i) / dryingRatio
    in exp (-1.0 * alphaFmc * ni)
