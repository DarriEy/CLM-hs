-- | Subgrid averaging utilities: p2c, p2l, p2g, c2l, c2g, l2g.
-- Ported from Julia: src/infrastructure/subgrid_ave.jl
-- Fortran: src/main/subgridAveMod.F90
--
-- All functions are pure. They take index bounds, subgrid data, and
-- input arrays, and return output arrays. Uses Data.Vector.Unboxed.
--
-- Scale types:
--   p2c: "unity"
--   c2l: "unity", "urbanf", "urbans"
--   l2g: "unity", "natveg", "veg", "ice", "nonurb", "lake", "veg_plus_lake"
module CLM.Infrastructure.SubgridAverage
  ( -- * Patch to Column
    p2c1d
  , p2c2d
  , p2c1dFilter
  , p2c2dFilter
    -- * Patch to Landunit
  , p2l1d
  , p2l2d
    -- * Patch to Gridcell
  , p2g1d
  , p2g2d
    -- * Column to Landunit
  , c2l1d
  , c2l2d
    -- * Column to Gridcell
  , c2g1d
  , c2g2d
    -- * Landunit to Gridcell
  , l2g1d
  , l2g2d
    -- * Scale type helpers
  , ScaleType(..)
  , C2LScaleType(..)
  , L2GScaleType(..)
  , createScaleL2gLookup
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!), (//))

import CLM.Infrastructure.InitSubgrid
  ( BoundsType(..)
  , LandunitData(..)
  , SubgridColumnData(..)
  , SubgridPatchData(..)
  , spval, maxLunit
  , istsoil, istcrop, istice, istdlak
  , isturb_min, isturb_max
  , icolRoof, icolSunwall, icolShadewall, icolRoadImperv, icolRoadPerv
  )

-- ============================================================================
-- Scale type enumerations
-- ============================================================================

-- | Patch-to-column scale type (currently only unity).
data ScaleType = ScaleUnity deriving (Show, Eq)

-- | Column-to-landunit scale type.
data C2LScaleType = C2LUnity | C2LUrbanF | C2LUrbanS deriving (Show, Eq)

-- | Landunit-to-gridcell scale type.
data L2GScaleType
  = L2GUnity
  | L2GNatveg
  | L2GVeg
  | L2GIce
  | L2GNonurb
  | L2GLake
  | L2GVegPlusLake
  deriving (Show, Eq)

-- ============================================================================
-- Scale lookup creation
-- ============================================================================

-- | Create a lookup vector of length maxLunit with scale factors per landunit type.
-- A value of spval means that landunit type is excluded from averages.
createScaleL2gLookup :: L2GScaleType -> Vector Double
createScaleL2gLookup L2GUnity       = VU.replicate maxLunit 1.0
createScaleL2gLookup L2GNatveg      = VU.replicate maxLunit spval // [(istsoil - 1, 1.0)]
createScaleL2gLookup L2GVeg         = VU.replicate maxLunit spval //
                                         [(istsoil - 1, 1.0), (istcrop - 1, 1.0)]
createScaleL2gLookup L2GIce         = VU.replicate maxLunit spval // [(istice - 1, 1.0)]
createScaleL2gLookup L2GNonurb      =
  let base = VU.replicate maxLunit 1.0
  in  base // [(i - 1, spval) | i <- [isturb_min .. isturb_max]]
createScaleL2gLookup L2GLake        = VU.replicate maxLunit spval // [(istdlak - 1, 1.0)]
createScaleL2gLookup L2GVegPlusLake = VU.replicate maxLunit spval //
                                         [(istsoil - 1, 1.0), (istcrop - 1, 1.0),
                                          (istdlak - 1, 1.0)]

-- | Build per-landunit scale factors for l2g averaging.
buildScaleL2g :: L2GScaleType -> BoundsType -> LandunitData -> Vector Double
buildScaleL2g scaleType bounds lun =
  let lookup_ = createScaleL2gLookup scaleType
  in  VU.generate (endl bounds) $ \ix ->
        let l = ix + 1
        in  if l >= begl bounds && l <= endl bounds
            then lookup_ ! (lunItype lun ! ix - 1)
            else spval

-- | Build per-column scale factors for c2l averaging.
buildScaleC2l :: C2LScaleType -> BoundsType -> SubgridColumnData -> LandunitData -> Vector Double
buildScaleC2l C2LUnity bounds _col _lun =
  VU.generate (endc bounds) $ \ix ->
    let c = ix + 1
    in  if c >= begc bounds && c <= endc bounds then 1.0 else spval
buildScaleC2l C2LUrbanF bounds col lun =
  VU.generate (endc bounds) $ \ix ->
    let c = ix + 1
    in  if c >= begc bounds && c <= endc bounds
        then let l = colLandunit col ! ix
                 isUrb = lunUrbpoi lun ! (l - 1)
                 ct = colItype col ! ix
                 hwr = lunCanyonHwr lun ! (l - 1)
             in  if isUrb
                 then if ct == icolSunwall || ct == icolShadewall
                      then 3.0 * hwr
                      else if ct == icolRoadPerv || ct == icolRoadImperv
                           then 3.0
                           else 1.0  -- roof
                 else 1.0
        else spval
buildScaleC2l C2LUrbanS bounds col lun =
  VU.generate (endc bounds) $ \ix ->
    let c = ix + 1
    in  if c >= begc bounds && c <= endc bounds
        then let l = colLandunit col ! ix
                 isUrb = lunUrbpoi lun ! (l - 1)
                 ct = colItype col ! ix
                 hwr = lunCanyonHwr lun ! (l - 1)
             in  if isUrb
                 then if ct == icolSunwall || ct == icolShadewall
                      then (3.0 * hwr) / (2.0 * hwr + 1.0)
                      else if ct == icolRoadPerv || ct == icolRoadImperv
                           then 3.0 / (2.0 * hwr + 1.0)
                           else 1.0
                 else 1.0
        else spval

-- ============================================================================
-- Weighted averaging helper
-- ============================================================================

-- | Weighted average accumulator.
-- Given an input vector, weights, scale factors, and a mapping from source
-- to destination index, produce the averaged output vector.
--
-- This is the core averaging pattern used by all p2c/c2l/l2g variants.
weightedAvg1d
  :: Int              -- ^ Output vector size
  -> (Int, Int)       -- ^ (beg, end) range of output indices (1-based)
  -> [(Int, Double)]  -- ^ List of (dest_index_1based, weighted_value) contributions
  -> [(Int, Double)]  -- ^ List of (dest_index_1based, weight) contributions
  -> Vector Double
weightedAvg1d nout (beg, end) vals wts =
  let spvec = VU.replicate nout spval
      sumwt = VU.replicate nout (0.0 :: Double)

      -- Accumulate
      (accVec, accWt) = foldl accum (spvec, sumwt) (zip vals wts)
      accum (v, w) ((di, wval), (_, ww)) =
        let ix = di - 1
            curW = w ! ix
            curV = v ! ix
            v' = if curW == 0.0
                 then v // [(ix, wval)]
                 else v // [(ix, curV + wval)]
            w' = w // [(ix, curW + ww)]
        in  (v', w')

      -- Normalize
      result = VU.generate nout $ \ix ->
        let i = ix + 1
        in  if i >= beg && i <= end
            then let sw = accWt ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "weightedAvg1d: sumwt > 1.0 at index " ++ show i
                     else if sw /= 0.0
                          then accVec ! ix / sw
                          else spval
            else spval
  in  result

-- ============================================================================
-- p2c: Patch to Column
-- ============================================================================

-- | Patch-to-column averaging (1D). Pure function returning column-level vector.
p2c1d :: Vector Double -> BoundsType -> ScaleType -> SubgridPatchData -> Vector Double
p2c1d parr bounds _scaleType pch =
  let nc = endc bounds
      initC = VU.replicate nc spval
      initW = VU.replicate nc (0.0 :: Double)
      -- Accumulate over active patches
      (accC, accW) = VU.ifoldl' step (initC, initW) (pchActive pch)
      step (cv, wv) ix isActive =
        let p = ix + 1
        in  if p >= begp bounds && p <= endp bounds && isActive
               && pchWtcol pch ! ix /= 0.0
               && parr ! ix /= spval
            then let c = pchColumn pch ! ix
                     cix = c - 1
                     wt = pchWtcol pch ! ix
                     curW = wv ! cix
                     curV = cv ! cix
                     cv' = if curW == 0.0
                           then cv // [(cix, parr ! ix * wt)]
                           else cv // [(cix, curV + parr ! ix * wt)]
                     wv' = wv // [(cix, curW + wt)]
                 in  (cv', wv')
            else (cv, wv)
      -- Normalize
      result = VU.generate nc $ \ix ->
        let c = ix + 1
        in  if c >= begc bounds && c <= endc bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "p2c1d: sumwt > 1.0 at c=" ++ show c
                     else if sw /= 0.0
                          then accC ! ix / sw
                          else spval
            else spval
  in  result

-- | Patch-to-column averaging (2D). Returns column-level matrix as flat vector
-- (nc * num2d), row-major.
p2c2d :: Vector Double -> BoundsType -> Int -> ScaleType -> SubgridPatchData -> Vector Double
p2c2d parr bounds num2d scaleType pch =
  let nc = endc bounds
  in  VU.concat [ p2c1d (extractLevel parr (endp bounds) j) bounds scaleType pch
                | j <- [0 .. num2d - 1] ]
  where
    np = endp bounds
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))

-- | Patch-to-column averaging with column mask (1D).
p2c1dFilter :: Vector Double -> Vector Bool -> SubgridColumnData -> SubgridPatchData -> Vector Double
p2c1dFilter patcharr maskC col pch =
  VU.imap (\ix active ->
    if active
    then let c = ix + 1
             pi_ = colPatchi col ! ix
             pf_ = colPatchf col ! ix
             val = sum [ patcharr ! (p - 1) * (pchWtcol pch ! (p - 1))
                       | p <- [pi_ .. pf_]
                       , pchActive pch ! (p - 1)
                       ]
         in  val
    else 0.0
  ) maskC

-- | Patch-to-column averaging with column mask (2D).
-- patcharr is flat (np * nlev), row-major. Returns flat (nc * nlev).
p2c2dFilter :: Vector Double -> Int -> Vector Bool -> SubgridColumnData -> SubgridPatchData -> Vector Double
p2c2dFilter patcharr nlev maskC col pch =
  let nc = VU.length maskC
  in  VU.generate (nc * nlev) $ \idx ->
        let (ix, j) = idx `divMod` nlev
            active = maskC ! ix
        in  if active
            then let pi_ = colPatchi col ! ix
                     pf_ = colPatchf col ! ix
                 in  sum [ patcharr ! ((p - 1) * nlev + j) * (pchWtcol pch ! (p - 1))
                         | p <- [pi_ .. pf_]
                         , pchActive pch ! (p - 1)
                         ]
            else 0.0

-- ============================================================================
-- p2l: Patch to Landunit
-- ============================================================================

-- | Patch-to-landunit averaging (1D).
p2l1d :: Vector Double -> BoundsType -> ScaleType -> C2LScaleType
      -> SubgridPatchData -> SubgridColumnData -> LandunitData -> Vector Double
p2l1d parr bounds _p2cScale c2lScale pch col lun =
  let nl = endl bounds
      scaleC2l = buildScaleC2l c2lScale bounds col lun
      initL = VU.replicate nl spval
      initW = VU.replicate nl (0.0 :: Double)
      (accL, accW) = VU.ifoldl' step (initL, initW) (pchActive pch)
      step (lv, wv) ix isActive =
        let p = ix + 1
        in  if p >= begp bounds && p <= endp bounds && isActive
               && pchWtlunit pch ! ix /= 0.0
            then let c = pchColumn pch ! ix
                     scC = scaleC2l ! (c - 1)
                 in  if parr ! ix /= spval && scC /= spval
                     then let l = pchLandunit pch ! ix
                              lix = l - 1
                              wt = pchWtlunit pch ! ix
                              curW = wv ! lix
                              curV = lv ! lix
                              lv' = if curW == 0.0
                                    then lv // [(lix, parr ! ix * scC * wt)]
                                    else lv // [(lix, curV + parr ! ix * scC * wt)]
                              wv' = wv // [(lix, curW + wt)]
                          in  (lv', wv')
                     else (lv, wv)
            else (lv, wv)
      result = VU.generate nl $ \ix ->
        let l = ix + 1
        in  if l >= begl bounds && l <= endl bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "p2l1d: sumwt > 1.0 at l=" ++ show l
                     else if sw /= 0.0
                          then accL ! ix / sw
                          else spval
            else spval
  in  result

-- | Patch-to-landunit averaging (2D). Returns flat (nl * num2d), level-major.
p2l2d :: Vector Double -> BoundsType -> Int -> ScaleType -> C2LScaleType
      -> SubgridPatchData -> SubgridColumnData -> LandunitData -> Vector Double
p2l2d parr bounds num2d p2cScale c2lScale pch col lun =
  let np = endp bounds
  in  VU.concat [ p2l1d (extractLevel parr np j) bounds p2cScale c2lScale pch col lun
                | j <- [0 .. num2d - 1] ]
  where
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))

-- ============================================================================
-- p2g: Patch to Gridcell
-- ============================================================================

-- | Patch-to-gridcell averaging (1D).
p2g1d :: Vector Double -> BoundsType -> ScaleType -> C2LScaleType -> L2GScaleType
      -> SubgridPatchData -> SubgridColumnData -> LandunitData -> Vector Double
p2g1d parr bounds _p2cScale c2lScale l2gScale pch col lun =
  let ng = endg bounds
      scaleC2l = buildScaleC2l c2lScale bounds col lun
      scaleL2g = buildScaleL2g l2gScale bounds lun
      initG = VU.replicate ng spval
      initW = VU.replicate ng (0.0 :: Double)
      (accG, accW) = VU.ifoldl' step (initG, initW) (pchActive pch)
      step (gv, wv) ix isActive =
        let p = ix + 1
        in  if p >= begp bounds && p <= endp bounds && isActive
               && pchWtgcell pch ! ix /= 0.0
            then let c = pchColumn pch ! ix
                     l = pchLandunit pch ! ix
                     scC = scaleC2l ! (c - 1)
                     scL = scaleL2g ! (l - 1)
                 in  if parr ! ix /= spval && scC /= spval && scL /= spval
                     then let g = pchGridcell pch ! ix
                              gix = g - 1
                              wt = pchWtgcell pch ! ix
                              curW = wv ! gix
                              curV = gv ! gix
                              gv' = if curW == 0.0
                                    then gv // [(gix, parr ! ix * scC * scL * wt)]
                                    else gv // [(gix, curV + parr ! ix * scC * scL * wt)]
                              wv' = wv // [(gix, curW + wt)]
                          in  (gv', wv')
                     else (gv, wv)
            else (gv, wv)
      result = VU.generate ng $ \ix ->
        let g = ix + 1
        in  if g >= begg bounds && g <= endg bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "p2g1d: sumwt > 1.0 at g=" ++ show g
                     else if sw /= 0.0
                          then accG ! ix / sw
                          else spval
            else spval
  in  result

-- | Patch-to-gridcell averaging (2D).
p2g2d :: Vector Double -> BoundsType -> Int -> ScaleType -> C2LScaleType -> L2GScaleType
      -> SubgridPatchData -> SubgridColumnData -> LandunitData -> Vector Double
p2g2d parr bounds num2d p2cScale c2lScale l2gScale pch col lun =
  let np = endp bounds
  in  VU.concat [ p2g1d (extractLevel parr np j) bounds p2cScale c2lScale l2gScale pch col lun
                | j <- [0 .. num2d - 1] ]
  where
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))

-- ============================================================================
-- c2l: Column to Landunit
-- ============================================================================

-- | Column-to-landunit averaging (1D).
c2l1d :: Vector Double -> BoundsType -> C2LScaleType
      -> SubgridColumnData -> LandunitData -> Bool -> Vector Double
c2l1d carr bounds c2lScale col lun includeInactive =
  let nl = endl bounds
      scaleC2l = buildScaleC2l c2lScale bounds col lun
      initL = VU.replicate nl spval
      initW = VU.replicate nl (0.0 :: Double)
      (accL, accW) = VU.ifoldl' step (initL, initW) (colActive col)
      step (lv, wv) ix isActive =
        let c = ix + 1
        in  if c >= begc bounds && c <= endc bounds
               && (isActive || includeInactive)
               && colWtlunit col ! ix /= 0.0
            then let scC = scaleC2l ! ix
                 in  if carr ! ix /= spval && scC /= spval
                     then let l = colLandunit col ! ix
                              lix = l - 1
                              wt = colWtlunit col ! ix
                              curW = wv ! lix
                              curV = lv ! lix
                              lv' = if curW == 0.0
                                    then lv // [(lix, carr ! ix * scC * wt)]
                                    else lv // [(lix, curV + carr ! ix * scC * wt)]
                              wv' = wv // [(lix, curW + wt)]
                          in  (lv', wv')
                     else (lv, wv)
            else (lv, wv)
      result = VU.generate nl $ \ix ->
        let l = ix + 1
        in  if l >= begl bounds && l <= endl bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "c2l1d: sumwt > 1.0 at l=" ++ show l
                     else if sw /= 0.0
                          then accL ! ix / sw
                          else spval
            else spval
  in  result

-- | Column-to-landunit averaging (2D).
c2l2d :: Vector Double -> BoundsType -> Int -> C2LScaleType
      -> SubgridColumnData -> LandunitData -> Vector Double
c2l2d carr bounds num2d c2lScale col lun =
  let nc = endc bounds
  in  VU.concat [ c2l1d (extractLevel carr nc j) bounds c2lScale col lun False
                | j <- [0 .. num2d - 1] ]
  where
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))

-- ============================================================================
-- c2g: Column to Gridcell
-- ============================================================================

-- | Column-to-gridcell averaging (1D).
c2g1d :: Vector Double -> BoundsType -> C2LScaleType -> L2GScaleType
      -> SubgridColumnData -> LandunitData -> Vector Double
c2g1d carr bounds c2lScale l2gScale col lun =
  let ng = endg bounds
      scaleC2l = buildScaleC2l c2lScale bounds col lun
      scaleL2g = buildScaleL2g l2gScale bounds lun
      initG = VU.replicate ng spval
      initW = VU.replicate ng (0.0 :: Double)
      (accG, accW) = VU.ifoldl' step (initG, initW) (colActive col)
      step (gv, wv) ix isActive =
        let c = ix + 1
        in  if c >= begc bounds && c <= endc bounds && isActive
               && colWtgcell col ! ix /= 0.0
            then let l = colLandunit col ! ix
                     scC = scaleC2l ! ix
                     scL = scaleL2g ! (l - 1)
                 in  if carr ! ix /= spval && scC /= spval && scL /= spval
                     then let g = colGridcell col ! ix
                              gix = g - 1
                              wt = colWtgcell col ! ix
                              curW = wv ! gix
                              curV = gv ! gix
                              gv' = if curW == 0.0
                                    then gv // [(gix, carr ! ix * scC * scL * wt)]
                                    else gv // [(gix, curV + carr ! ix * scC * scL * wt)]
                              wv' = wv // [(gix, curW + wt)]
                          in  (gv', wv')
                     else (gv, wv)
            else (gv, wv)
      result = VU.generate ng $ \ix ->
        let g = ix + 1
        in  if g >= begg bounds && g <= endg bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "c2g1d: sumwt > 1.0 at g=" ++ show g
                     else if sw /= 0.0
                          then accG ! ix / sw
                          else spval
            else spval
  in  result

-- | Column-to-gridcell averaging (2D).
c2g2d :: Vector Double -> BoundsType -> Int -> C2LScaleType -> L2GScaleType
      -> SubgridColumnData -> LandunitData -> Vector Double
c2g2d carr bounds num2d c2lScale l2gScale col lun =
  let nc = endc bounds
  in  VU.concat [ c2g1d (extractLevel carr nc j) bounds c2lScale l2gScale col lun
                | j <- [0 .. num2d - 1] ]
  where
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))

-- ============================================================================
-- l2g: Landunit to Gridcell
-- ============================================================================

-- | Landunit-to-gridcell averaging (1D).
l2g1d :: Vector Double -> BoundsType -> L2GScaleType -> LandunitData -> Vector Double
l2g1d larr bounds l2gScale lun =
  let ng = endg bounds
      scaleL2g = buildScaleL2g l2gScale bounds lun
      initG = VU.replicate ng spval
      initW = VU.replicate ng (0.0 :: Double)
      (accG, accW) = VU.ifoldl' step (initG, initW) (lunActive lun)
      step (gv, wv) ix isActive =
        let l = ix + 1
        in  if l >= begl bounds && l <= endl bounds && isActive
               && lunWtgcell lun ! ix /= 0.0
            then let scL = scaleL2g ! ix
                 in  if larr ! ix /= spval && scL /= spval
                     then let g = lunGridcell lun ! ix
                              gix = g - 1
                              wt = lunWtgcell lun ! ix
                              curW = wv ! gix
                              curV = gv ! gix
                              gv' = if curW == 0.0
                                    then gv // [(gix, larr ! ix * scL * wt)]
                                    else gv // [(gix, curV + larr ! ix * scL * wt)]
                              wv' = wv // [(gix, curW + wt)]
                          in  (gv', wv')
                     else (gv, wv)
            else (gv, wv)
      result = VU.generate ng $ \ix ->
        let g = ix + 1
        in  if g >= begg bounds && g <= endg bounds
            then let sw = accW ! ix
                 in  if sw > 1.0 + 1.0e-6
                     then error $ "l2g1d: sumwt > 1.0 at g=" ++ show g
                     else if sw /= 0.0
                          then accG ! ix / sw
                          else spval
            else spval
  in  result

-- | Landunit-to-gridcell averaging (2D).
l2g2d :: Vector Double -> BoundsType -> Int -> L2GScaleType -> LandunitData -> Vector Double
l2g2d larr bounds num2d l2gScale lun =
  let nl = endl bounds
  in  VU.concat [ l2g1d (extractLevel larr nl j) bounds l2gScale lun
                | j <- [0 .. num2d - 1] ]
  where
    extractLevel v n j = VU.generate n (\i -> v ! (i * num2d + j))
