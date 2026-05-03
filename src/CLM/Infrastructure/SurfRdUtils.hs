-- | Weight normalization and collapse utilities for surface data reading.
-- Ported from: src/main/surfrdUtilsMod.F90
-- All functions are pure.
module CLM.Infrastructure.SurfRdUtils
  ( -- * Weight checks
    checkSumsEqual1
  , SumCheckError(..)
    -- * Normalization
  , renormalize
    -- * Landunit operations
  , applyConvertOceanToLand
  , collapseIndividualLunits
  , collapseToDominant
    -- * Constants
  , sumTo1Tol
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import Data.List (sortBy)
import Data.Ord (Down(..))

import CLM.Constants.LandunitConstants
  ( istsoil, istcrop, istocn, istice, istdlak, istwet
  , isturb_tbd, isturb_hd, isturb_md, max_lunit )

-- | Default tolerance for weight sum checks.
sumTo1Tol :: Double
sumTo1Tol = 1.0e-6

-- ---------------------------------------------------------------------------
-- Weight checks
-- ---------------------------------------------------------------------------

-- | Error from weight sum validation.
data SumCheckError = SumCheckError
  { sceRow    :: !Int
  , sceSum    :: !Double
  , sceTarget :: !Double
  } deriving (Show, Eq)

-- | Check that each row of a 2D weight array sums to the target (default 1.0).
-- Returns a list of errors (empty on success).
checkSumsEqual1
  :: Double                       -- ^ tolerance
  -> Maybe (VU.Vector Double)     -- ^ per-row targets (Nothing = 1.0 for all)
  -> V.Vector (VU.Vector Double)  -- ^ weight matrix (rows)
  -> [SumCheckError]
checkSumsEqual1 tol mTargets rows =
  [ SumCheckError n s target
  | n <- [0 .. V.length rows - 1]
  , let s = VU.sum (rows V.! n)
        target = case mTargets of
          Nothing -> 1.0
          Just ts -> ts VU.! n
  , abs (s - target) > tol
  ]

-- ---------------------------------------------------------------------------
-- Normalization
-- ---------------------------------------------------------------------------

-- | Renormalize each row so it sums to the target.
-- Rows that sum to zero are unchanged.
renormalize :: Double -> V.Vector (VU.Vector Double) -> V.Vector (VU.Vector Double)
renormalize target = V.map renormRow
  where
    renormRow row =
      let s = VU.sum row
      in if s /= 0.0
         then VU.map (* (target / s)) row
         else row

-- ---------------------------------------------------------------------------
-- Ocean-to-land conversion
-- ---------------------------------------------------------------------------

-- | Move ocean weight to natveg (ISTSOIL).
applyConvertOceanToLand :: V.Vector (VU.Vector Double) -> V.Vector (VU.Vector Double)
applyConvertOceanToLand = V.map convertRow
  where
    convertRow row =
      let oceanWt = row VU.! (istocn - 1)
          soilWt  = row VU.! (istsoil - 1)
      in row VU.// [ (istsoil - 1, soilWt + oceanWt)
                    , (istocn  - 1, 0.0) ]

-- ---------------------------------------------------------------------------
-- Collapse small landunits
-- ---------------------------------------------------------------------------

-- | Thresholds for collapsing individual landunits (as percentages).
data CollapseThresholds = CollapseThresholds
  { ctSoil    :: !Double
  , ctCrop    :: !Double
  , ctGlacier :: !Double
  , ctLake    :: !Double
  , ctWetland :: !Double
  , ctUrban   :: !Double
  } deriving (Show)

-- | Remove landunits below user-defined percentage thresholds.
-- If all landunits get removed for a gridcell, keep the largest.
-- Thresholds are in percentages; internally converted to fractions.
collapseIndividualLunits
  :: Double  -- ^ toosmall_soil (%)
  -> Double  -- ^ toosmall_crop (%)
  -> Double  -- ^ toosmall_glacier (%)
  -> Double  -- ^ toosmall_lake (%)
  -> Double  -- ^ toosmall_wetland (%)
  -> Double  -- ^ toosmall_urban (%)
  -> V.Vector (VU.Vector Double)  -- ^ wt_lunit
  -> V.Vector (VU.Vector Double)
collapseIndividualLunits tsSoil tsCrop tsGlacier tsLake tsWetland tsUrban wtLunit =
  let threshold = VU.replicate max_lunit 0.0 VU.//
        [ (istsoil - 1,    tsSoil / 100.0)
        , (istcrop - 1,    tsCrop / 100.0)
        , (istice  - 1,    tsGlacier / 100.0)
        , (istdlak - 1,    tsLake / 100.0)
        , (istwet  - 1,    tsWetland / 100.0)
        , (isturb_tbd - 1, tsUrban / 100.0)
        , (isturb_hd  - 1, tsUrban / 100.0)
        , (isturb_md  - 1, tsUrban / 100.0)
        ]

      collapseRow row =
        let -- Zero out entries below threshold, keeping residuals
            pairs = VU.generate (VU.length row) $ \m ->
              let w = row VU.! m
                  t = threshold VU.! m
              in if w > 0.0 && w <= t then (0.0, w) else (w, 0.0)
            cleaned  = VU.map fst pairs
            residual = VU.map snd pairs
            s = VU.sum cleaned
        in if s == 0.0
           then -- Keep largest residual
             let maxIdx = VU.maxIndex residual
             in cleaned VU.// [(maxIdx, residual VU.! maxIdx)]
           else cleaned

      collapsed = V.map collapseRow wtLunit
  in renormalize 1.0 collapsed

-- ---------------------------------------------------------------------------
-- Collapse to dominant
-- ---------------------------------------------------------------------------

-- | Collapse to top N dominant landunits per gridcell.
collapseToDominant
  :: Int                          -- ^ n_dominant
  -> V.Vector (VU.Vector Double)  -- ^ weight matrix
  -> V.Vector (VU.Vector Double)
collapseToDominant nDom weights
  | nDom <= 0 = weights
  | otherwise  = V.map collapseRow weights
  where
    collapseRow row =
      let ncols = VU.length row
      in if nDom >= ncols then row
         else let indexed = zip [0..] (VU.toList row)
                  sorted  = take nDom $ sortBy (\a b -> compare (Down (snd a)) (Down (snd b))) indexed
                  topIdxs = map fst sorted
                  wtTotal = VU.sum row
                  wtDom   = sum (map snd sorted)
                  scale   = if wtDom > 0.0 && wtTotal > 0.0
                            then wtTotal / wtDom else 1.0
                  row0 = VU.replicate ncols 0.0
              in row0 VU.// [(i, (row VU.! i) * scale) | i <- topIdxs]
