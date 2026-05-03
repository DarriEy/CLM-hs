-- | Initialize vertical components of column data (snow/soil layer z, dz, zi).
-- Fortran: initVerticalMod.F90
-- Computes global soil/lake coordinate arrays and per-column vertical structure.
module CLM.Infrastructure.InitVertical
  ( -- * Global coordinate arrays
    soilCoordinates
  , lakeCoordinates
    -- * Per-column initialization
  , setStandardSoil
    -- * Bedrock
  , hasBedrock
  , findSoilLayerContainingDepth
    -- * Layer class constants
  , levgrndClassStandard
  , levgrndClassDeepBedrock
  , levgrndClassShallowBedrock
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (nlevsoi, nlevgrnd)

-- | Layer class constants (from setSoilLayerClass).
levgrndClassStandard, levgrndClassDeepBedrock, levgrndClassShallowBedrock :: Int
levgrndClassStandard        = 1
levgrndClassDeepBedrock     = 2
levgrndClassShallowBedrock  = 3

-- | Compute global soil coordinate arrays (zsoi, dzsoi, zisoi).
-- CLM5 "20SL_8.5m" configuration with piecewise-linear layer thicknesses.
-- Ported from Julia: src/constants/varcon.jl (varcon_init!)
--
-- Returns (zsoi, dzsoi, zisoi) where:
--   zsoi  has length nlevgrnd  (layer midpoints, metres below surface)
--   dzsoi has length nlevgrnd  (layer thicknesses)
--   zisoi has length nlevgrnd+1 (interface depths; index 0 = surface = 0.0)
soilCoordinates :: (VU.Vector Double, VU.Vector Double, VU.Vector Double)
soilCoordinates =
  let -- Layer thicknesses: piecewise-linear within 3 tiers + bedrock
      -- j is 0-based here, Julia uses 1-based
      dzsoi = VU.generate nlevgrnd $ \j ->
        if j < min 4 nlevsoi
          -- Tier 1: dz = (j+1) * 0.02  (0.02, 0.04, 0.06, 0.08 m)
          then fromIntegral (j + 1) * 0.02
        else if j < min 13 nlevsoi
          -- Tier 2: dz = dz[3] + (j+1-4) * 0.04  (0.12, 0.16, ..., 0.44 m)
          then dz4 + fromIntegral (j + 1 - 4) * 0.04
        else if j < nlevsoi
          -- Tier 3: dz = dz[12] + (j+1-13) * 0.10  (0.54, 0.64, ..., 1.14 m)
          then dz13 + fromIntegral (j + 1 - 13) * 0.10
        else
          -- Bedrock layers: exponential thickening
          let k = fromIntegral (j - nlevsoi + 1) :: Double
          in  dz20 + (k * 25.0) ** 1.5 / 100.0

      -- Tier boundary thicknesses (Julia 1-based: dz[4], dz[13], dz[20])
      dz4  = 4.0 * 0.02  -- = 0.08 m
      dz13 = dz4 + (13 - 4) * 0.04  -- = 0.08 + 0.36 = 0.44 m
      dz20 = dz13 + (min 20 nlevsoi' - 13) * 0.10  -- = 0.44 + 0.70 = 1.14 m
      nlevsoi' = fromIntegral nlevsoi :: Double

      -- Interface depths: cumulative sum of layer thicknesses
      -- zisoi[0] = 0.0, zisoi[j+1] = zisoi[j] + dzsoi[j]
      zisoi = VU.generate (nlevgrnd + 1) $ \j ->
        if j == 0 then 0.0
        else VU.sum (VU.take j dzsoi)

      -- Node depths: layer midpoints
      -- zsoi[j] = 0.5 * (zisoi[j] + zisoi[j+1])
      zsoi = VU.generate nlevgrnd $ \j ->
        0.5 * (zisoi VU.! j + zisoi VU.! (j + 1))

  in (zsoi, dzsoi, zisoi)

-- | Default lake layer coordinates (10 levels, standard CLM5 spacing).
-- Returns (zlak, dzlak) where each has length nlevlak.
lakeCoordinates :: Int -> (VU.Vector Double, VU.Vector Double)
lakeCoordinates nlevlak =
  let -- Standard CLM5 lake layer depths (10 layers)
      defaultDepths = VU.fromList
        [0.05, 0.6, 2.1, 4.6, 8.1, 12.6, 18.6, 25.6, 34.325, 44.775]
      defaultThick = VU.fromList
        [0.1, 1.0, 2.0, 3.0, 4.0, 5.0, 7.0, 7.0, 10.45, 10.45]

      nDefault = min nlevlak (VU.length defaultDepths)
      zlak  = VU.take nDefault defaultDepths
      dzlak = VU.take nDefault defaultThick
  in (zlak, dzlak)

-- | Copy global soil coordinates into a column's vertical structure.
-- Returns (z_col, dz_col, zi_col) vectors for the column's soil layers,
-- including the snow offset.
--
-- @joff@: snow layer offset (= nlevsno).
-- The returned vectors are sized (nlevsno + nlevgrnd) for z/dz,
-- and (nlevsno + nlevgrnd + 1) for zi.
setStandardSoil
  :: Int               -- ^ joff (nlevsno)
  -> Int               -- ^ nlevgrnd
  -> VU.Vector Double  -- ^ zsoi  (length nlevgrnd)
  -> VU.Vector Double  -- ^ dzsoi (length nlevgrnd)
  -> VU.Vector Double  -- ^ zisoi (length nlevgrnd+1)
  -> (VU.Vector Double, VU.Vector Double, VU.Vector Double)
     -- ^ (z, dz, zi) for one column
setStandardSoil joff nlev zsoi dzsoi zisoi =
  let totalZ  = joff + nlev
      totalZi = joff + nlev + 1

      z  = VU.generate totalZ  $ \i ->
             if i >= joff then zsoi  VU.! (i - joff) else 0.0

      dz = VU.generate totalZ  $ \i ->
             if i >= joff then dzsoi VU.! (i - joff) else 0.0

      zi = VU.generate totalZi $ \i ->
             if i >= joff then zisoi VU.! (i - joff) else 0.0

  in (z, dz, zi)

-- | Returns True if the given column type includes bedrock layers.
-- Ported from @hasBedrock@ in @initVerticalMod.F90@.
hasBedrock
  :: Int   -- ^ Column type (col_itype)
  -> Int   -- ^ Landunit type (lun_itype)
  -> Int   -- ^ ISTICE constant
  -> Int   -- ^ ISTURB_MIN constant
  -> Int   -- ^ ISTURB_MAX constant
  -> Int   -- ^ ICOL_ROAD_PERV constant
  -> Bool
hasBedrock colItype lunItype istice isturbMin isturbMax icolRoadPerv
  | lunItype == istice = False
  | lunItype >= isturbMin && lunItype <= isturbMax = colItype == icolRoadPerv
  | otherwise = True

-- | Find the soil layer (0-based) that contains the given depth (m).
-- Uses interface depths from 'soilCoordinates'.
findSoilLayerContainingDepth :: VU.Vector Double -> Double -> Maybe Int
findSoilLayerContainingDepth zisoi depth
  | depth <= zisoi VU.! 0 = Nothing  -- above top of soil
  | otherwise = go 0
  where
    nlevels = VU.length zisoi - 1
    go i
      | i >= nlevels = Nothing  -- below bottom
      | depth <= zisoi VU.! (i + 1) = Just i
      | otherwise = go (i + 1)
