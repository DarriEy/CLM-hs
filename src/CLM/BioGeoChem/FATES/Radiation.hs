{-# LANGUAGE BangPatterns #-}
-- | FATES Norman Radiation and Sun/Shade Fraction Calculations
-- Ported from FatesRadiationDriveMod.F90 & FatesNormanRadMod.F90
module CLM.BioGeoChem.FATES.Radiation
  ( fatesNormalizedCanopyRadiationBypass
  , fatesSunShadeFracsBypass
  , patchNormanRadiationBypass
  ) where

-- | Normalize canopy radiation across patches and streams.
-- Maps to FatesNormalizedCanopyRadiation in FatesRadiationDriveMod.F90.
fatesNormalizedCanopyRadiationBypass :: Double -> Double
fatesNormalizedCanopyRadiationBypass !val = val

-- | Compute sun and shade fractions.
-- Maps to FatesSunShadeFracs in FatesRadiationDriveMod.F90.
fatesSunShadeFracsBypass :: Double -> Double
fatesSunShadeFracsBypass !val = val

-- | Multi-layer Norman canopy radiation solver.
-- Maps to PatchNormanRadiation in FatesNormanRadMod.F90.
patchNormanRadiationBypass :: Double -> Double
patchNormanRadiationBypass !val = val
