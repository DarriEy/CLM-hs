-- | Filter/mask utilities for column and patch loops.
-- Replaces Fortran integer filter arrays with Bool vectors.
module CLM.Infrastructure.Filters
  ( FilterSet(..)
  , defaultFilterSet
  , maskToIndices
  , buildFilters
  , buildFiltersBinary
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.LandunitConstants (istsoil, istcrop, istdlak, isturb_tbd, isturb_hd, isturb_md)
import CLM.Infrastructure.BinaryIO (readFloat64Vector)
import System.FilePath ((</>))

-- | Set of masks for different surface types.
data FilterSet = FilterSet
  { maskSoil       :: !(VU.Vector Bool)  -- ^ Soil columns
  , maskLake       :: !(VU.Vector Bool)  -- ^ Lake columns
  , maskNoLake     :: !(VU.Vector Bool)  -- ^ Non-lake columns
  , maskUrban      :: !(VU.Vector Bool)  -- ^ Urban columns
  , maskSnowpack   :: !(VU.Vector Bool)  -- ^ Columns with active snowpack
  , maskExposedVeg :: !(VU.Vector Bool)  -- ^ Patches with exposed vegetation
  , maskSoilP      :: !(VU.Vector Bool)  -- ^ Soil patches (patch-level)
  } deriving (Show)

defaultFilterSet :: FilterSet
defaultFilterSet = FilterSet
  { maskSoil       = VU.empty
  , maskLake       = VU.empty
  , maskNoLake     = VU.empty
  , maskUrban      = VU.empty
  , maskSnowpack   = VU.empty
  , maskExposedVeg = VU.empty
  , maskSoilP      = VU.empty
  }

-- | Convert a Bool mask to a vector of active indices (for interop).
maskToIndices :: VU.Vector Bool -> VU.Vector Int
maskToIndices mask =
  VU.map fst $ VU.filter snd $ VU.indexed mask

-- | Build a complete FilterSet from subgrid topology.
buildFilters
  :: Int                  -- ^ nc (number of columns)
  -> Int                  -- ^ np (number of patches)
  -> VU.Vector Int        -- ^ col_itype (column type, length nc)
  -> VU.Vector Int        -- ^ col_landunit (column -> landunit index, 1-based, length nc)
  -> VU.Vector Int        -- ^ lun_itype (landunit type, length nl)
  -> VU.Vector Int        -- ^ pch_column (patch -> column index, 1-based, length np)
  -> FilterSet
buildFilters _nc _np _col_itype col_landunit lun_itype pch_column =
  let -- Helper: get landunit type for column c (0-based index)
      colLunType c =
        let li = col_landunit VU.! c  -- 1-based landunit index
        in  lun_itype VU.! (li - 1)

      -- Column-level masks
      mSoil  = VU.generate (VU.length col_landunit) $ \c ->
        let lt = colLunType c in lt == istsoil || lt == istcrop

      mLake  = VU.generate (VU.length col_landunit) $ \c ->
        colLunType c == istdlak

      mNoLake = VU.map not mLake

      mUrban = VU.generate (VU.length col_landunit) $ \c ->
        let lt = colLunType c in lt == isturb_tbd || lt == isturb_hd || lt == isturb_md

      mSnowpack = VU.replicate (VU.length col_landunit) False

      -- Patch-level masks: check if the patch's column is a soil column
      mExposedVeg = VU.generate (VU.length pch_column) $ \p ->
        let ci = pch_column VU.! p  -- 1-based column index
            lt = colLunType (ci - 1)
        in  lt == istsoil || lt == istcrop

      mSoilP = mExposedVeg  -- same as maskExposedVeg for now

  in FilterSet
    { maskSoil       = mSoil
    , maskLake       = mLake
    , maskNoLake     = mNoLake
    , maskUrban      = mUrban
    , maskSnowpack   = mSnowpack
    , maskExposedVeg = mExposedVeg
    , maskSoilP      = mSoilP
    }

-- | Build a FilterSet by reading exported binary filter files (for verification).
-- Each file contains Float64 values; > 0.5 is treated as True.
buildFiltersBinary
  :: FilePath  -- ^ coldstart directory
  -> Int       -- ^ nc (number of columns)
  -> Int       -- ^ np (number of patches)
  -> IO FilterSet
buildFiltersBinary dir nc np = do
  soilc   <- readBoolVector (dir </> "filt_soilc.bin") nc
  lakec   <- readBoolVector (dir </> "filt_lakec.bin") nc
  nolakec <- readBoolVector (dir </> "filt_nolakec.bin") nc
  soilp   <- readBoolVector (dir </> "filt_soilp.bin") np
  return FilterSet
    { maskSoil       = soilc
    , maskLake       = lakec
    , maskNoLake     = nolakec
    , maskUrban      = VU.replicate nc False   -- not exported
    , maskSnowpack   = VU.replicate nc False   -- not exported
    , maskExposedVeg = soilp                    -- same as soilp for now
    , maskSoilP      = soilp
    }

-- | Read a Float64 binary vector and convert to Bool (> 0.5 = True).
readBoolVector :: FilePath -> Int -> IO (VU.Vector Bool)
readBoolVector fp _n = do
  v <- readFloat64Vector fp
  return $ VU.map (> 0.5) v
