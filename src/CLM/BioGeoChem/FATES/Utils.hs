{-# LANGUAGE BangPatterns #-}
-- | FATES General Numerical Utilities
-- Ported from FatesUtilsMod.F90
module CLM.BioGeoChem.FATES.Utils
  ( quadraticRootsNSWC
  , quadraticRootsSridharachary
  ) where

import CLM.BioGeoChem.FATES.Constants (nearzero)

-- | Solve quadratic equation ax^2 + bx + c = 0 using NSWC library algorithm.
-- Avoids overflow. Returns Left error message if imaginary roots are detected.
-- Maps to QuadraticRootsNSWC in FatesUtilsMod.F90.
quadraticRootsNSWC :: Double -> Double -> Double -> Either String (Double, Double)
quadraticRootsNSWC !a !b !c
  | abs a < nearzero =
      let !r2 = if b /= 0.0 then -c / b else 0.0
      in Right (0.0, r2)
  | abs c > nearzero =
      let !b1 = b / 2.0
      in if abs b1 < abs c
           then
             let !e = if c < 0.0 then -a else a
                 !e' = b1 * (b1 / abs c) - e
                 !d = sqrt (abs e') * sqrt (abs c)
             in if e' < 0.0
                  then Left "error, imaginary roots detected in quadratic solve"
                  else
                    let !d' = if b1 >= 0.0 then -d else d
                        !r1 = (-b1 + d') / a
                        !r2 = if r1 /= 0.0 then (c / r1) / a else 0.0
                    in Right (r1, r2)
           else
             let !e = 1.0 - (a / b1) * (c / b1)
                 !d = sqrt (abs e) * abs b1
             in if e < 0.0
                  then Left "error, imaginary roots detected in quadratic solve"
                  else
                    let !d' = if b1 >= 0.0 then -d else d
                        !r1 = (-b1 + d') / a
                        !r2 = if r1 /= 0.0 then (c / r1) / a else 0.0
                    in Right (r1, r2)
  | otherwise =
      Right (-b / a, 0.0)

-- | Solve quadratic equation ax^2 + bx + c = 0 using Sridharachary (Standard formula).
-- Returns Left error message if imaginary roots are detected.
-- Maps to QuadraticRootsSridharachary in FatesUtilsMod.F90.
quadraticRootsSridharachary :: Double -> Double -> Double -> Either String (Double, Double)
quadraticRootsSridharachary !a !b !c
  | abs a < nearzero =
      let !r2 = if abs b > nearzero then -c / b else 0.0
      in Right (0.0, r2)
  | otherwise =
      let !d = b * b - 4.0 * a * c
          !das = sqrt (abs d)
      in if d > nearzero
           then Right ((-b + das) / (2.0 * a), (-b - das) / (2.0 * a))
           else if abs d <= nearzero
                  then let !r = -b / (2.0 * a) in Right (r, r)
                  else Left "error, imaginary roots detected in quadratic solve"
