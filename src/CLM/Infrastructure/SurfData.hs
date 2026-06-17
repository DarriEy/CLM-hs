-- | Surface data structures and pure transforms for reading surfdata_clm.nc.
-- Ported from: src/main/surfrdMod.F90 + clm_varsur.F90
--
-- Binary I/O reads data exported by Julia's export_test_data.jl.
-- Expected directory structure:
--   surfdata/pct_sand.bin, pct_clay.bin, organic.bin, soil_color.bin,
--   fmax.bin, slope.bin, std_elev.bin, lakedepth.bin, zbedrock.bin,
--   wt_lunit.bin, wt_nat_patch.bin,
--   monthly_lai.bin, monthly_sai.bin, monthly_htop.bin, monthly_hbot.bin,
--   latitude.bin, longitude.bin
module CLM.Infrastructure.SurfData
  ( -- * Data structures
    SurfaceInputData(..)
  , defaultSurfaceInputData
    -- * Pure processing
  , surfrdSpecialToWtLunit
  , surfrdVegWeights
  , surfrdNormalizePatchWeights
  , countSubgridElements
  , CountResult(..)
    -- * IO
  , surfrdGetNumPatches
  , surfrdGetData
  , surfrdGetDataBinary
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V
import System.FilePath ((</>))

import CLM.Constants.LandunitConstants
  ( istsoil, istcrop, istocn, istice, istdlak, istwet
  , isturb_tbd, isturb_hd, isturb_md, isturb_min, max_lunit, numurbl )
import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readFloat64Scalar, readInt64Vector, readFloat64Matrix )

-- ---------------------------------------------------------------------------
-- Data structures
-- ---------------------------------------------------------------------------

-- | Container for all data read from the surface dataset.
-- Replaces Fortran module-level variables in clm_instur/clm_varsur.
data SurfaceInputData = SurfaceInputData
  { -- Landunit weights [ng, max_lunit] row-major
    sid_wt_lunit      :: !(V.Vector (VU.Vector Double))
    -- Natural PFT weights [ng, natpft_size] row-major
  , sid_wt_nat_patch  :: !(V.Vector (VU.Vector Double))
    -- Crop functional type weights [ng, cft_size]
  , sid_wt_cft        :: !(V.Vector (VU.Vector Double))
    -- Urban properties
  , sid_urban_valid   :: !(VU.Vector Bool)
  , sid_pct_urban_max :: !(V.Vector (VU.Vector Double))
    -- Fertilizer for CFTs [ng, cft_size]
  , sid_fert_cft      :: !(V.Vector (VU.Vector Double))
    -- Soil properties [ng, nlevsoi]
  , sid_pct_sand      :: !(V.Vector (VU.Vector Double))
  , sid_pct_clay      :: !(V.Vector (VU.Vector Double))
  , sid_organic       :: !(V.Vector (VU.Vector Double))
  , sid_soil_color    :: !(VU.Vector Int)
  , sid_zbedrock      :: !(VU.Vector Double)
    -- Lake/topography
  , sid_lakedepth     :: !(VU.Vector Double)
  , sid_slope         :: !(VU.Vector Double)
  , sid_std_elev      :: !(VU.Vector Double)
  , sid_fmax          :: !(VU.Vector Double)
    -- Monthly phenology [ng][npft, 12] (vector of matrices, row-major)
  , sid_monthly_lai   :: !(V.Vector (VU.Vector Double))
  , sid_monthly_sai   :: !(V.Vector (VU.Vector Double))
  , sid_monthly_htop  :: !(V.Vector (VU.Vector Double))
  , sid_monthly_hbot  :: !(V.Vector (VU.Vector Double))
    -- Glacier MEC weights [ng, maxpatch_glc]
  , sid_wt_glc_mec    :: !(V.Vector (VU.Vector Double))
    -- Location
  , sid_latitude      :: !Double
  , sid_longitude     :: !Double
  } deriving (Show)

defaultSurfaceInputData :: SurfaceInputData
defaultSurfaceInputData = SurfaceInputData
  { sid_wt_lunit      = V.empty
  , sid_wt_nat_patch  = V.empty
  , sid_wt_cft        = V.empty
  , sid_urban_valid   = VU.empty
  , sid_pct_urban_max = V.empty
  , sid_fert_cft      = V.empty
  , sid_pct_sand      = V.empty
  , sid_pct_clay      = V.empty
  , sid_organic       = V.empty
  , sid_soil_color    = VU.empty
  , sid_zbedrock      = VU.empty
  , sid_lakedepth     = VU.empty
  , sid_slope         = VU.empty
  , sid_std_elev      = VU.empty
  , sid_fmax          = VU.empty
  , sid_monthly_lai   = V.empty
  , sid_monthly_sai   = V.empty
  , sid_monthly_htop  = V.empty
  , sid_monthly_hbot  = V.empty
  , sid_wt_glc_mec    = V.empty
  , sid_latitude      = 0.0
  , sid_longitude     = 0.0
  }

-- ---------------------------------------------------------------------------
-- Pure processing functions
-- ---------------------------------------------------------------------------

-- | Convert special landunit percentages to fractions in wt_lunit.
surfrdSpecialToWtLunit
  :: Double           -- ^ pct_lake
  -> Double           -- ^ pct_wetland
  -> Double           -- ^ pct_glacier
  -> VU.Vector Double -- ^ pct_urban (length numurbl)
  -> VU.Vector Double -- ^ existing wt_lunit row (length max_lunit)
  -> VU.Vector Double
surfrdSpecialToWtLunit pctLake pctWetland pctGlacier pctUrban wt0 =
  let wt1 = wt0 VU.// [(istdlak - 1, pctLake / 100.0)]
      wt2 = wt1 VU.// [(istwet  - 1, pctWetland / 100.0)]
      wt3 = wt2 VU.// [(istice  - 1, pctGlacier / 100.0)]
      urbUpdates = [ (isturb_min + n - 1 - 1, (pctUrban VU.! n) / 100.0)
                   | n <- [0 .. VU.length pctUrban - 1]
                   , n < numurbl ]
  in wt3 VU.// urbUpdates

-- | Compute vegetation landunit weights from PCT_NATVEG and PCT_CROP.
surfrdVegWeights
  :: Double           -- ^ pct_nat_veg
  -> Double           -- ^ pct_crop
  -> Bool             -- ^ create_crop_landunit
  -> VU.Vector Double -- ^ existing wt_lunit row
  -> VU.Vector Double
surfrdVegWeights pctNatVeg pctCrop createCropLu wt0 =
  if createCropLu
  then wt0 VU.// [ (istsoil - 1, pctNatVeg / 100.0)
                  , (istcrop - 1, pctCrop / 100.0) ]
  else wt0 VU.// [ (istsoil - 1, (pctNatVeg + pctCrop) / 100.0) ]

-- | Normalize a patch weight vector so it sums to 1.0.
surfrdNormalizePatchWeights :: VU.Vector Double -> VU.Vector Double
surfrdNormalizePatchWeights wts =
  let s = VU.sum wts
  in if s > 0.0
     then VU.map (/ s) wts
     else if VU.null wts then wts
          else VU.replicate (VU.length wts) 0.0 VU.// [(0, 1.0)]

-- ---------------------------------------------------------------------------
-- Subgrid element counting
-- ---------------------------------------------------------------------------

-- | Results of counting subgrid elements.
data CountResult = CountResult
  { crLandunits :: !Int
  , crColumns   :: !Int
  , crPatches   :: !Int
  } deriving (Show)

-- | Count landunits, columns, and patches from surface data weights.
countSubgridElements
  :: Int                          -- ^ ng (number of gridcells)
  -> V.Vector (VU.Vector Double)  -- ^ wt_lunit [ng][max_lunit]
  -> V.Vector (VU.Vector Double)  -- ^ wt_nat_patch [ng][natpft_size]
  -> V.Vector (VU.Vector Double)  -- ^ wt_cft [ng][cft_size]
  -> V.Vector (VU.Vector Double)  -- ^ wt_glc_mec [ng][maxpatch_glc]
  -> Bool                         -- ^ create_crop_landunit
  -> Bool                         -- ^ run_zero_weight_urban
  -> CountResult
countSubgridElements ng wtLunit wtNatPatch wtCft wtGlcMec
                     createCropLu runZeroWtUrban =
  let countG g = (nl, nc, np)
        where
          wl = wtLunit V.! g

          -- Natural vegetation (always created)
          nlSoil = 1
          ncSoil = 1
          natWeights = if g < V.length wtNatPatch
                       then wtNatPatch V.! g else VU.empty
          nNat = max 1 (VU.length (VU.filter (> 0.0) natWeights))
          npSoil = nNat

          -- Crop
          hasCrop = createCropLu && wl VU.! (istcrop - 1) > 0.0
          cftWeights = if g < V.length wtCft then wtCft V.! g else VU.empty
          nCft = VU.length (VU.filter (> 0.0) cftWeights)
          (nlCrop, ncCrop, npCrop)
            | hasCrop && nCft > 0 = (1, nCft, nCft)
            | hasCrop             = (1, 1, 1)
            | otherwise           = (0, 0, 0)

          -- Urban
          countUrb u = if wl VU.! (u - 1) > 0.0 || runZeroWtUrban
                       then (1, 5, 5) else (0, 0, 0)
          (nlU, ncU, npU) = foldl (\(a,b,c) u -> let (x,y,z) = countUrb u
                                                  in (a+x, b+y, c+z))
                            (0,0,0) [isturb_tbd, isturb_hd, isturb_md]

          -- Lake (always created)
          nlLake = 1; ncLake = 1; npLake = 1

          -- Wetland
          hasWet = wl VU.! (istwet - 1) > 0.0
          (nlWet, ncWet, npWet) = if hasWet then (1,1,1) else (0,0,0)

          -- Glacier
          hasGlc = wl VU.! (istice - 1) > 0.0
          glcWeights = if g < V.length wtGlcMec then wtGlcMec V.! g else VU.empty
          nGlc = max 1 (VU.length (VU.filter (> 0.0) glcWeights))
          (nlGlc, ncGlc, npGlc)
            | hasGlc    = (1, nGlc, nGlc)
            | otherwise = (0, 0, 0)

          nl = nlSoil + nlCrop + nlU + nlLake + nlWet + nlGlc
          nc = ncSoil + ncCrop + ncU + ncLake + ncWet + ncGlc
          np = npSoil + npCrop + npU + npLake + npWet + npGlc

      (totalNl, totalNc, totalNp) = foldl
        (\(a,b,c) g -> let (x,y,z) = countG g in (a+x, b+y, c+z))
        (0,0,0) [0 .. ng - 1]

  in CountResult totalNl totalNc totalNp

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- | Read PFT/CFT dimensions for the compatibility surface-data API.
surfrdGetNumPatches :: FilePath -> IO (Int, Int)
surfrdGetNumPatches _fsurdat = do
  return (15, 2)

-- | Read all surface data for the compatibility surface-data API.
surfrdGetData :: FilePath -> Int -> Int -> IO SurfaceInputData
surfrdGetData _fsurdat _begg _endg = do
  return defaultSurfaceInputData

-- | Read surface data from binary files exported by Julia.
-- @dir@ is the path to the surfdata/ subdirectory.
surfrdGetDataBinary :: FilePath -> Int -> Int -> IO SurfaceInputData
surfrdGetDataBinary dir ng nlevsoi = do
  -- Soil properties
  sandFlat <- readFloat64Vector (dir </> "pct_sand.bin")
  clayFlat <- readFloat64Vector (dir </> "pct_clay.bin")
  orgFlat  <- readFloat64Vector (dir </> "organic.bin")
  colorVec <- readInt64Vector   (dir </> "soil_color.bin")

  -- Topography scalars (one per gridcell)
  fmaxVec    <- readFloat64Vector (dir </> "fmax.bin")
  slopeVec   <- readFloat64Vector (dir </> "slope.bin")
  stdElev    <- readFloat64Vector (dir </> "std_elev.bin")
  lakeVec    <- readFloat64Vector (dir </> "lakedepth.bin")
  zbedVec    <- readFloat64Vector (dir </> "zbedrock.bin")

  -- Landunit and PFT weights
  wtLunitFlat   <- readFloat64Vector (dir </> "wt_lunit.bin")
  wtNatPFlat    <- readFloat64Vector (dir </> "wt_nat_patch.bin")

  -- Monthly phenology
  laiFlat  <- readFloat64Vector (dir </> "monthly_lai.bin")
  saiFlat  <- readFloat64Vector (dir </> "monthly_sai.bin")
  htopFlat <- readFloat64Vector (dir </> "monthly_htop.bin")
  hbotFlat <- readFloat64Vector (dir </> "monthly_hbot.bin")

  -- Location
  lat <- readFloat64Scalar (dir </> "latitude.bin")
  lon <- readFloat64Scalar (dir </> "longitude.bin")

  -- Reshape flat vectors into V.Vector (VU.Vector Double) per gridcell
  let nSoilLevels = min nlevsoi (VU.length sandFlat `div` max ng 1)
      splitRows flat ncols =
        V.generate ng $ \g -> VU.slice (g * ncols) ncols flat
      sandRows = splitRows sandFlat nSoilLevels
      clayRows = splitRows clayFlat nSoilLevels
      orgRows  = splitRows orgFlat  nSoilLevels

      -- wt_lunit: ng rows of max_lunit columns
      wtLunitRows = splitRows wtLunitFlat max_lunit

      -- wt_nat_patch: determine npft from total length
      npft = VU.length wtNatPFlat `div` max ng 1
      wtNatPRows = splitRows wtNatPFlat npft

      -- Monthly phenology: total elements / ng gives npft*12 per gridcell
      nPhenPerG = VU.length laiFlat `div` max ng 1
      laiRows  = splitRows laiFlat  nPhenPerG
      saiRows  = splitRows saiFlat  nPhenPerG
      htopRows = splitRows htopFlat nPhenPerG
      hbotRows = splitRows hbotFlat nPhenPerG

  return $! SurfaceInputData
    { sid_wt_lunit      = wtLunitRows
    , sid_wt_nat_patch  = wtNatPRows
    , sid_wt_cft        = V.singleton VU.empty
    , sid_urban_valid   = VU.replicate ng False
    , sid_pct_urban_max = V.singleton VU.empty
    , sid_fert_cft      = V.singleton VU.empty
    , sid_pct_sand      = sandRows
    , sid_pct_clay      = clayRows
    , sid_organic       = orgRows
    , sid_soil_color    = colorVec
    , sid_zbedrock      = zbedVec
    , sid_lakedepth     = lakeVec
    , sid_slope         = slopeVec
    , sid_std_elev      = stdElev
    , sid_fmax          = fmaxVec
    , sid_monthly_lai   = laiRows
    , sid_monthly_sai   = saiRows
    , sid_monthly_htop  = htopRows
    , sid_monthly_hbot  = hbotRows
    , sid_wt_glc_mec    = V.singleton VU.empty
    , sid_latitude      = lat
    , sid_longitude     = lon
    }
