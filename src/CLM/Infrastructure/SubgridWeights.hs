-- | Subgrid weight computation, active flag setting, and weight validation.
-- Ported from: src/main/subgridWeightsMod.F90
-- All functions are pure: they take subgrid data and produce updated vectors.
module CLM.Infrastructure.SubgridWeights
  ( -- * Weight computation
    computeHigherOrderWeights
    -- * Active flag computation
  , computeActive
    -- * Weight validation
  , checkWeights
  , WeightError(..)
    -- * Utility
  , getLandunitWeight
    -- * Result types
  , HigherOrderWeights(..)
  , ActiveFlags(..)
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.LandunitConstants
  ( istsoil, istdlak, isturb_min, isturb_max )

-- ---------------------------------------------------------------------------
-- Result types
-- ---------------------------------------------------------------------------

-- | Updated weights from 'computeHigherOrderWeights'.
data HigherOrderWeights = HigherOrderWeights
  { howColWtgcell   :: !(VU.Vector Double)  -- ^ col%wtgcell
  , howPatchWtlunit :: !(VU.Vector Double)  -- ^ patch%wtlunit
  , howPatchWtgcell :: !(VU.Vector Double)  -- ^ patch%wtgcell
  } deriving (Show)

-- | Active flags for landunits, columns, and patches.
data ActiveFlags = ActiveFlags
  { afLunActive   :: !(VU.Vector Bool)
  , afColActive   :: !(VU.Vector Bool)
  , afPatchActive :: !(VU.Vector Bool)
  } deriving (Show)

-- ---------------------------------------------------------------------------
-- computeHigherOrderWeights
-- ---------------------------------------------------------------------------

-- | Compute col%wtgcell, patch%wtlunit, patch%wtgcell from base weights.
-- Given patch%wtcol, col%wtlunit, lun%wtgcell, and linkage arrays.
computeHigherOrderWeights
  :: (Int, Int)           -- ^ (begc, endc) column range
  -> (Int, Int)           -- ^ (begp, endp) patch range
  -> VU.Vector Int        -- ^ col%landunit (c -> l)
  -> VU.Vector Double     -- ^ col%wtlunit
  -> VU.Vector Double     -- ^ lun%wtgcell
  -> VU.Vector Int        -- ^ patch%column (p -> c)
  -> VU.Vector Double     -- ^ patch%wtcol
  -> HigherOrderWeights
computeHigherOrderWeights (begc, endc) (begp, endp)
                          colLandunit colWtlunit lunWtgcell
                          patchColumn patchWtcol =
  let -- col%wtgcell
      colWtg = VU.generate (VU.length colWtlunit) $ \c ->
        if c >= begc - 1 && c <= endc - 1
        then let l = colLandunit VU.! c - 1
             in  (colWtlunit VU.! c) * (lunWtgcell VU.! l)
        else 0.0

      -- patch%wtlunit
      pWtl = VU.generate (VU.length patchWtcol) $ \p ->
        if p >= begp - 1 && p <= endp - 1
        then let c = patchColumn VU.! p - 1
             in  (patchWtcol VU.! p) * (colWtlunit VU.! c)
        else 0.0

      -- patch%wtgcell
      pWtg = VU.generate (VU.length patchWtcol) $ \p ->
        if p >= begp - 1 && p <= endp - 1
        then let c = patchColumn VU.! p - 1
             in  (patchWtcol VU.! p) * (colWtg VU.! c)
        else 0.0

  in HigherOrderWeights colWtg pWtl pWtg

-- ---------------------------------------------------------------------------
-- computeActive
-- ---------------------------------------------------------------------------

-- | Compute active flags for landunits, columns, and patches.
computeActive
  :: Bool                 -- ^ all_active flag
  -> (Int, Int)           -- ^ (begl, endl)
  -> (Int, Int)           -- ^ (begc, endc)
  -> (Int, Int)           -- ^ (begp, endp)
  -> VU.Vector Double     -- ^ lun%wtgcell
  -> VU.Vector Int        -- ^ lun%itype
  -> VU.Vector Int        -- ^ col%landunit
  -> VU.Vector Double     -- ^ col%wtlunit
  -> VU.Vector Int        -- ^ patch%column
  -> VU.Vector Double     -- ^ patch%wtcol
  -> ActiveFlags
computeActive allActive (begl, endl) (begc, endc) (begp, endp)
              lunWtgcell lunItype colLandunit colWtlunit patchColumn patchWtcol =
  let -- Landunit active
      lunActive = VU.generate (VU.length lunWtgcell) $ \l ->
        if allActive then True
        else if l < begl - 1 || l > endl - 1 then False
        else let wt = lunWtgcell VU.! l
                 it = lunItype VU.! l
             in  wt > 0.0
                 || it == istsoil
                 || it == istdlak
                 || (it >= isturb_min && it <= isturb_max)

      -- Column active
      colActive = VU.generate (VU.length colWtlunit) $ \c ->
        if allActive then True
        else if c < begc - 1 || c > endc - 1 then False
        else let l = colLandunit VU.! c - 1
                 la = lunActive VU.! l
                 it = lunItype VU.! l
             in  (la && colWtlunit VU.! c > 0.0)
                 || (la && it >= isturb_min && it <= isturb_max)

      -- Patch active
      patchActive = VU.generate (VU.length patchWtcol) $ \p ->
        if allActive then True
        else if p < begp - 1 || p > endp - 1 then False
        else let c = patchColumn VU.! p - 1
             in  (colActive VU.! c) && (patchWtcol VU.! p > 0.0)

  in ActiveFlags lunActive colActive patchActive

-- ---------------------------------------------------------------------------
-- checkWeights
-- ---------------------------------------------------------------------------

-- | Weight validation error.
data WeightError = WeightError
  { weContext :: !String
  , weSum     :: !Double
  } deriving (Show, Eq)

-- | Check that subgrid weights sum to 1.0 within tolerance.
-- Returns a list of errors (empty on success).
checkWeights
  :: Bool                 -- ^ active_only
  -> Double               -- ^ tolerance (e.g. 1e-12)
  -> (Int, Int)           -- ^ (begc, endc)
  -> (Int, Int)           -- ^ (begp, endp)
  -> VU.Vector Bool       -- ^ col%active
  -> VU.Vector Bool       -- ^ patch%active
  -> VU.Vector Int        -- ^ patch%column
  -> VU.Vector Double     -- ^ patch%wtcol
  -> [WeightError]
checkWeights activeOnly tol (begc, endc) (begp, endp)
             colActive patchActive patchColumn patchWtcol =
  let nc = endc
      -- Sum patch weights on columns
      sumwtcol0 = VU.replicate nc 0.0
      sumwtcol = VU.accum (+) sumwtcol0
        [ (patchColumn VU.! p - 1, patchWtcol VU.! p)
        | p <- [begp - 1 .. endp - 1]
        , not activeOnly || (patchActive VU.! p)
        ]
      -- Check each column
      errs = [ WeightError ("patch-on-col at c=" ++ show (c+1)) (sumwtcol VU.! c)
             | c <- [begc - 1 .. endc - 1]
             , not (checkWtOk (sumwtcol VU.! c) activeOnly (colActive VU.! c) tol)
             ]
  in errs

-- | Check if a weight sum is acceptable.
checkWtOk :: Double -> Bool -> Bool -> Double -> Bool
checkWtOk s activeOnly iAmActive tol
  | activeOnly && iAmActive     = abs (s - 1.0) <= tol
  | activeOnly && not iAmActive = s == 0.0 || abs (s - 1.0) <= tol
  | otherwise                   = abs (s - 1.0) <= tol

-- ---------------------------------------------------------------------------
-- getLandunitWeight
-- ---------------------------------------------------------------------------

-- | Get weight of a landunit type on a gridcell. Returns 0 if not present.
-- @landunitIndices@ is a 2D lookup (ltype, g) flattened row-major.
getLandunitWeight
  :: Int                  -- ^ max_lunit (number of landunit types)
  -> VU.Vector Int        -- ^ landunit_indices[ltype, g] (ISPVAL = -1 means absent)
  -> VU.Vector Double     -- ^ lun%wtgcell
  -> Int                  -- ^ gridcell g (1-based)
  -> Int                  -- ^ landunit type ltype (1-based)
  -> Double
getLandunitWeight maxLunit lunIndices lunWtgcell g ltype =
  let idx = (ltype - 1) * 1 + (g - 1)  -- simplified for single gridcell
      l   = if idx >= 0 && idx < VU.length lunIndices
            then lunIndices VU.! idx
            else -1
  in if l < 0 then 0.0 else lunWtgcell VU.! (l - 1)
