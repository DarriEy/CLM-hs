-- | Satellite phenology (SP) ecosystem dynamics: prescribed LAI/SAI.
-- Ported from Julia: src/biogeochem/satellite_phenology.jl
-- Fortran: src/biogeochem/SatellitePhenologyMod.F90
--
-- Pure functions for interpolating monthly vegetation data and
-- adjusting LAI/SAI for snow burial. The satellite LAI/SAI/height fields
-- are supplied to this module as pre-loaded monthly arrays (via the
-- prescribed-phenology inputs); 'readMonthlyVegetation' and
-- 'readAnnualVegetation' perform the in-scope array-to-patch transformation.
-- The external NetCDF stream reader that fetches those monthly arrays from
-- disk lives outside this single-column scope.
module CLM.BioGeoPhys.SatellitePhenology
  ( -- * Interpolation state
    SatellitePhenologyState(..)
  , initSatellitePhenology
    -- * Main SP dynamics
  , satellitePhenology
    -- * Monthly interpolation
  , interpMonthlyVeg
  , InterpResult(..)
    -- * Data reading interfaces (pure transformation)
  , readMonthlyVegetation
  , readAnnualVegetation
    -- * Constants
  , ndaypm
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!))

-- ============================================================================
-- Constants
-- ============================================================================

-- | Days per month (non-leap year).
ndaypm :: Vector Int
ndaypm = VU.fromList [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

-- ============================================================================
-- Interpolation state
-- ============================================================================

-- | Interpolation state for satellite phenology monthly vegetation data.
-- Contains the saved month index, time weights, and two-month arrays
-- for LAI, SAI, and vegetation heights.
data SatellitePhenologyState = SatellitePhenologyState
  { spInterpMonths1 :: !Int              -- ^ Saved month index (-999 = uninitialized)
  , spTimwt         :: !(Double, Double) -- ^ Time weights for month 1 and month 2
  , spMlai2t        :: !(Vector Double)  -- ^ LAI for interpolation (np * 2), patch-major
  , spMsai2t        :: !(Vector Double)  -- ^ SAI for interpolation (np * 2), patch-major
  , spMhvt2t        :: !(Vector Double)  -- ^ Top vegetation height (np * 2), patch-major
  , spMhvb2t        :: !(Vector Double)  -- ^ Bottom vegetation height (np * 2), patch-major
  } deriving (Show)

-- | Initialize satellite phenology state for np patches.
initSatellitePhenology :: Int -> SatellitePhenologyState
initSatellitePhenology np = SatellitePhenologyState
  { spInterpMonths1 = -999
  , spTimwt         = (0.0, 0.0)
  , spMlai2t        = VU.replicate (np * 2) (0.0 / 0.0)  -- NaN
  , spMsai2t        = VU.replicate (np * 2) (0.0 / 0.0)
  , spMhvt2t        = VU.replicate (np * 2) (0.0 / 0.0)
  , spMhvb2t        = VU.replicate (np * 2) (0.0 / 0.0)
  }

-- ============================================================================
-- Canopy state fields (input/output for SP)
-- ============================================================================

-- | Canopy state fields updated by satellite phenology.
data CanopyStateFields = CanopyStateFields
  { csfTlai              :: !(Vector Double)  -- ^ Total LAI per patch
  , csfTsai              :: !(Vector Double)  -- ^ Total SAI per patch
  , csfElai              :: !(Vector Double)  -- ^ Exposed LAI per patch
  , csfEsai              :: !(Vector Double)  -- ^ Exposed SAI per patch
  , csfHtop              :: !(Vector Double)  -- ^ Canopy top height [m]
  , csfHbot              :: !(Vector Double)  -- ^ Canopy bottom height [m]
  , csfFracVegNosnoAlb   :: !(Vector Int)     -- ^ Fraction veg not snow-covered (0 or 1)
  } deriving (Show)

-- ============================================================================
-- SP configuration
-- ============================================================================

-- | Configuration for satellite phenology computation.
data SPConfig = SPConfig
  { spcNoveg              :: !Int   -- ^ Bare ground PFT index (typically 0)
  , spcNbrdlfDcdBrlShrub  :: !Int   -- ^ Max PFT index for short vegetation (11)
  , spcUseLaiStreams       :: !Bool  -- ^ Use LAI streams instead of interpolation
  , spcUseFatesSp          :: !Bool  -- ^ FATES-SP mode
  } deriving (Show)

-- ============================================================================
-- Main SP dynamics (pure)
-- ============================================================================

-- | Ecosystem dynamics: phenology, vegetation.
-- Calculates leaf areas (tlai, elai), stem areas (tsai, esai) and height (htop).
-- Interpolates monthly vegetation data and adjusts LAI/SAI for snow burial.
--
-- Returns updated canopy state fields.
satellitePhenology
  :: SatellitePhenologyState
  -> CanopyStateFields
  -> Vector Double      -- ^ frac_sno_col (snow cover fraction per column)
  -> Vector Double      -- ^ snow_depth_col [m]
  -> Vector Int          -- ^ patch itype
  -> Vector Int          -- ^ patch -> column mapping (1-based)
  -> Vector Bool         -- ^ patch mask
  -> (Int, Int)          -- ^ patch bounds (beg, end), 1-based
  -> SPConfig
  -> CanopyStateFields
satellitePhenology sp csf fracSno snowDepth pchItype pchCol maskP (begp, endp_) cfg =
  let (tw1, tw2) = spTimwt sp
      np = endp_ - begp + 1

      -- Process each patch
      processP ix =
        let p = ix + 1
            active = p >= begp && p <= endp_ && maskP ! ix
        in  if active
            then let c = pchCol ! ix - 1  -- 0-based column index
                     pt = pchItype ! ix

                     -- Interpolate
                     tlaiNew = if not (spcUseLaiStreams cfg)
                               then tw1 * (spMlai2t sp ! (ix * 2))
                                    + tw2 * (spMlai2t sp ! (ix * 2 + 1))
                               else csfTlai csf ! ix
                     tsaiNew = tw1 * (spMsai2t sp ! (ix * 2))
                               + tw2 * (spMsai2t sp ! (ix * 2 + 1))
                     htopNew = tw1 * (spMhvt2t sp ! (ix * 2))
                               + tw2 * (spMhvt2t sp ! (ix * 2 + 1))
                     hbotNew = tw1 * (spMhvb2t sp ! (ix * 2))
                               + tw2 * (spMhvb2t sp ! (ix * 2 + 1))

                     -- Snow burial fraction
                     fb = if pt > spcNoveg cfg && pt <= spcNbrdlfDcdBrlShrub cfg
                          then let ol = min (max (snowDepth ! c - hbotNew) 0.0)
                                            (htopNew - hbotNew)
                               in  1.0 - ol / max 1.0e-6 (htopNew - hbotNew)
                          else 1.0 - (max (min (snowDepth ! c)
                                              (max 0.05 (htopNew * 0.8))) 0.0
                                     / max 0.05 (htopNew * 0.8))

                     fSno = fracSno ! c

                     -- Exposed LAI/SAI weighted by snow cover
                     (elaiNew, esaiNew, fvna) =
                       if not (spcUseFatesSp cfg)
                       then let e_lai = max (tlaiNew * (1.0 - fSno) + tlaiNew * fb * fSno) 0.0
                                e_sai = max (tsaiNew * (1.0 - fSno) + tsaiNew * fb * fSno) 0.0
                                el = if e_lai < 0.05 then 0.0 else e_lai
                                es = if e_sai < 0.05 then 0.0 else e_sai
                                fv = if (el + es) >= 0.05 then 1 else 0
                            in  (el, es, fv)
                       else (csfElai csf ! ix, csfEsai csf ! ix,
                             csfFracVegNosnoAlb csf ! ix)

                 in  (tlaiNew, tsaiNew, elaiNew, esaiNew, htopNew, hbotNew, fvna)
            else ( csfTlai csf ! ix, csfTsai csf ! ix
                 , csfElai csf ! ix, csfEsai csf ! ix
                 , csfHtop csf ! ix, csfHbot csf ! ix
                 , csfFracVegNosnoAlb csf ! ix )

      npatches = VU.length (csfTlai csf)
      results = map processP [0 .. npatches - 1]
      (tlais, tsais, elais, esais, htops, hbots, fvnas) = unzip7 results
  in  CanopyStateFields
        { csfTlai            = VU.fromList tlais
        , csfTsai            = VU.fromList tsais
        , csfElai            = VU.fromList elais
        , csfEsai            = VU.fromList esais
        , csfHtop            = VU.fromList htops
        , csfHbot            = VU.fromList hbots
        , csfFracVegNosnoAlb = VU.fromList fvnas
        }

-- | Unzip a list of 7-tuples.
unzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7 [] = ([],[],[],[],[],[],[])
unzip7 ((a,b,c,d,e,f,g):xs) =
  let (as,bs,cs,ds,es,fs,gs) = unzip7 xs
  in  (a:as, b:bs, c:cs, d:ds, e:es, f:fs, g:gs)

-- ============================================================================
-- Monthly interpolation (pure)
-- ============================================================================

-- | Result of monthly vegetation interpolation scheduling.
data InterpResult = InterpResult
  { irMonths    :: !(Int, Int)  -- ^ (month1, month2) to interpolate between
  , irNeedsRead :: !Bool        -- ^ True if new data should be read
  , irState     :: !SatellitePhenologyState  -- ^ Updated state
  } deriving (Show)

-- | Determine time weights for monthly vegetation data interpolation.
-- Returns the months to interpolate and whether new data needs reading.
interpMonthlyVeg
  :: SatellitePhenologyState
  -> Int    -- ^ Current month (1-12)
  -> Int    -- ^ Current day (1-31)
  -> InterpResult
interpMonthlyVeg sp kmo kda =
  let t = (fromIntegral kda - 0.5) / fromIntegral (ndaypm ! (kmo - 1))
      it1 = floor (t + 0.5) :: Int
      it2 = it1 + 1
      months1_raw = kmo + it1 - 1
      months2_raw = kmo + it2 - 1
      months1 = if months1_raw < 1 then 12 else months1_raw
      months2 = if months2_raw > 12 then 1 else months2_raw
      tw1 = fromIntegral it1 + 0.5 - t
      tw2 = 1.0 - tw1
      needsRead = spInterpMonths1 sp /= months1
      sp' = sp { spTimwt = (tw1, tw2)
               , spInterpMonths1 = months1 }
  in  InterpResult
        { irMonths    = (months1, months2)
        , irNeedsRead = needsRead
        , irState     = sp'
        }

-- ============================================================================
-- Data reading (pure transformation)
-- ============================================================================

-- | Read monthly vegetation data for two consecutive months.
-- Takes the monthly data arrays (flat: ngrc * npft * 12, gridcell-major)
-- and fills the SP interpolation arrays.
-- Returns updated SP state and mlaidiff per patch.
readMonthlyVegetation
  :: SatellitePhenologyState
  -> Vector Double     -- ^ Monthly LAI (ngrc * (maxveg+1) * 12)
  -> Vector Double     -- ^ Monthly SAI
  -> Vector Double     -- ^ Monthly height top
  -> Vector Double     -- ^ Monthly height bot
  -> Int               -- ^ Number of gridcells
  -> Int               -- ^ maxveg + 1 (number of PFT slots)
  -> (Int, Int)        -- ^ (month1, month2)
  -> Vector Int        -- ^ Patch -> gridcell (1-based)
  -> Vector Int        -- ^ Patch itype
  -> (Int, Int)        -- ^ Patch bounds (beg, end)
  -> Int               -- ^ noveg index
  -> (SatellitePhenologyState, Vector Double)  -- ^ (updated state, mlaidiff)
readMonthlyVegetation sp mLai mSai mHtop mHbot _ngrc npftSlots months pchGrc pchIt (begp, endp_) noveg_ =
  let np = VU.length pchGrc
      nMonths = 12

      -- Look up value from monthly array: monthly[g, l+1, mk]
      -- Flat index: ((g-1) * npftSlots + l) * nMonths + (mk - 1)
      lookupMonthly arr g l mk =
        arr ! (((g - 1) * npftSlots + l) * nMonths + (mk - 1))

      (m1, m2) = months

      -- Build new interpolation arrays (np * 2, patch-major)
      mlai = VU.generate (np * 2) $ \idx ->
        let (pix, k) = idx `divMod` 2
            mk = if k == 0 then m1 else m2
            p = pix + 1
        in  if p >= begp && p <= endp_ && pchIt ! pix /= noveg_
            then let g = pchGrc ! pix
                     l = pchIt ! pix
                 in  lookupMonthly mLai g l mk
            else 0.0

      msai = VU.generate (np * 2) $ \idx ->
        let (pix, k) = idx `divMod` 2
            mk = if k == 0 then m1 else m2
            p = pix + 1
        in  if p >= begp && p <= endp_ && pchIt ! pix /= noveg_
            then lookupMonthly mSai (pchGrc ! pix) (pchIt ! pix) mk
            else 0.0

      mhvt = VU.generate (np * 2) $ \idx ->
        let (pix, k) = idx `divMod` 2
            mk = if k == 0 then m1 else m2
            p = pix + 1
        in  if p >= begp && p <= endp_ && pchIt ! pix /= noveg_
            then lookupMonthly mHtop (pchGrc ! pix) (pchIt ! pix) mk
            else 0.0

      mhvb = VU.generate (np * 2) $ \idx ->
        let (pix, k) = idx `divMod` 2
            mk = if k == 0 then m1 else m2
            p = pix + 1
        in  if p >= begp && p <= endp_ && pchIt ! pix /= noveg_
            then lookupMonthly mHbot (pchGrc ! pix) (pchIt ! pix) mk
            else 0.0

      sp' = sp { spMlai2t = mlai, spMsai2t = msai
               , spMhvt2t = mhvt, spMhvb2t = mhvb }

      -- mlaidiff = mlai2t(:,1) - mlai2t(:,2)
      mlaidiff = VU.generate np $ \pix ->
        mlai ! (pix * 2) - mlai ! (pix * 2 + 1)

  in  (sp', mlaidiff)

-- | Read 12 months of LAI data for dry deposition.
-- Takes monthly LAI array and patch metadata, returns annlai (np * 12).
readAnnualVegetation
  :: Vector Double     -- ^ Monthly LAI (ngrc * (maxveg+1) * 12)
  -> Int               -- ^ npftSlots (maxveg + 1)
  -> Vector Int        -- ^ Patch -> gridcell (1-based)
  -> Vector Int        -- ^ Patch itype
  -> (Int, Int)        -- ^ Patch bounds (beg, end)
  -> Int               -- ^ noveg index
  -> Vector Double     -- ^ annlai (np * 12), patch-major
readAnnualVegetation mLai npftSlots pchGrc pchIt (begp, endp_) noveg_ =
  let np = VU.length pchGrc
  in  VU.generate (np * 12) $ \idx ->
        let (pix, k) = idx `divMod` 12
            p = pix + 1
        in  if p >= begp && p <= endp_ && pchIt ! pix /= noveg_
            then let g = pchGrc ! pix
                     l = pchIt ! pix
                 in  mLai ! (((g - 1) * npftSlots + l) * 12 + k)
            else 0.0
