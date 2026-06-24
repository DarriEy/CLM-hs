{-# LANGUAGE BangPatterns #-}
-- | FATES Photosynthesis and Respiration Dynamics
-- Ported from FatesPlantRespPhotosynthMod.F90
module CLM.BioGeoChem.FATES.Vegetation
  ( lowStorageMaintRespReduction
  ) where

import CLM.BioGeoChem.FATES.Constants (nearzero)

-- | Reduce maintenance respiration rates when storage pool is low.
-- Maps to lowstorage_maintresp_reduction in FatesPlantRespPhotosynthMod.F90.
lowStorageMaintRespReduction
  :: Double -- ^ frac: ratio of storage to target leaf biomass
  -> Double -- ^ maintresp_reduction_curvature for the PFT
  -> Double -- ^ maintresp_reduction_intercept for the PFT
  -> Double -- ^ maintresp_reduction_factor [0-1]
lowStorageMaintRespReduction !frac !curvature !intercept
  | frac < 1.0 =
      if abs (curvature - 1.0) > nearzero
        then (1.0 - intercept) + intercept * (1.0 - curvature ** frac) / (1.0 - curvature)
        else (1.0 - intercept) + intercept * frac
  | otherwise = 1.0
