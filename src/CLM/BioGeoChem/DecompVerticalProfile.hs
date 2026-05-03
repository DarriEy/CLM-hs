{-# LANGUAGE BangPatterns #-}
-- | Vertical profiles for distributing soil and litter C and N.
-- Fortran: SoilBiogeochemVerticalProfileMod.F90
-- Julia:   src/biogeochem/decomp_vertical_profile.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompVerticalProfile
  ( -- * Constants
    surfprofExp
    -- * Main computation
  , VertProfileInput(..)
  , VertProfileOutput(..)
  , soilBGCVerticalProfile
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Steepness of surface profile (1/e-folding depth) [1/m].
surfprofExp :: Double
surfprofExp = 10.0

-- | Input to vertical profile computation.
data VertProfileInput = VertProfileInput
  { vpi_nc               :: !Int
  , vpi_np               :: !Int
  , vpi_nlevdecomp       :: !Int
  , vpi_nlevdecomp_full  :: !Int
  , vpi_mask_soilc       :: !(VU.Vector Bool)
  , vpi_mask_vegp        :: !(VU.Vector Bool)
  , vpi_dzsoi_decomp     :: !(VU.Vector Double)  -- ^ (nlevdecomp)
  , vpi_zsoi             :: !(VU.Vector Double)   -- ^ (nlevdecomp)
  , vpi_crootfr          :: !(VU.Vector Double)   -- ^ (np * nlevdecomp)
  , vpi_altmax_lastyear_indx :: !(VU.Vector Int)  -- ^ (nc)
  , vpi_patch_column     :: !(VU.Vector Int)      -- ^ (np) patch-to-column mapping
  , vpi_patch_wtcol      :: !(VU.Vector Double)   -- ^ (np) patch weight on column
  , vpi_use_bedrock      :: !Bool
  , vpi_zmin_bedrock     :: !Double
  } deriving (Show)

-- | Output of vertical profile computation.
data VertProfileOutput = VertProfileOutput
  { vpo_leaf_prof    :: !(VU.Vector Double)  -- ^ (np * nlevdecomp_full)
  , vpo_froot_prof   :: !(VU.Vector Double)
  , vpo_croot_prof   :: !(VU.Vector Double)
  , vpo_stem_prof    :: !(VU.Vector Double)
  , vpo_nfixation_prof :: !(VU.Vector Double)  -- ^ (nc * nlevdecomp_full)
  , vpo_ndep_prof    :: !(VU.Vector Double)
  } deriving (Show)

-- | 2D index helper for patch arrays (np * nlev)
idxP :: Int -> Int -> Int -> Int
idxP np p j = p + np * j
{-# INLINE idxP #-}

-- | 2D index helper for column arrays (nc * nlev)
idxC :: Int -> Int -> Int -> Int
idxC nc c j = c + nc * j
{-# INLINE idxC #-}

-- | Calculate vertical profiles for C and N distribution.
soilBGCVerticalProfile :: VertProfileInput -> VertProfileOutput
soilBGCVerticalProfile inp =
  let !nc   = vpi_nc inp
      !np   = vpi_np inp
      !nlev = vpi_nlevdecomp inp
      !nlevFull = vpi_nlevdecomp_full inp
      maskC = vpi_mask_soilc inp
      maskP = vpi_mask_vegp inp
      dzsoi = vpi_dzsoi_decomp inp
      zsoi  = vpi_zsoi inp
      rootfr = vpi_crootfr inp
      altIdx = vpi_altmax_lastyear_indx inp
      pCol  = vpi_patch_column inp
      pWt   = vpi_patch_wtcol inp

      -- Surface profile
      surface_prof = VU.generate nlev $ \j ->
        let sp = exp (negate surfprofExp * zsoi VU.! j) / (dzsoi VU.! j)
        in if vpi_use_bedrock inp && zsoi VU.! j > vpi_zmin_bedrock inp then 0.0 else sp

      -- cinput_rootfr per patch
      cinput_rootfr = VU.generate (np * nlev) $ \ix ->
        let j = ix `div` np; p = ix `mod` np
        in if not (maskP VU.! p) then 0.0
           else (rootfr VU.! idxP np p j) / (dzsoi VU.! j)

      -- Patch profiles
      patchProfs = VU.generate (np * nlevFull * 4) $ \ix ->
        let profType = ix `div` (np * nlevFull)
            rem1 = ix `mod` (np * nlevFull)
            j = rem1 `div` np; p = rem1 `mod` np
        in if not (maskP VU.! p) || j >= nlev then 0.0
           else let c = pCol VU.! p
                    jmax = min (max (altIdx VU.! c) 1) nlev
                    rootfrTot = sum [ cinput_rootfr VU.! idxP np p jj * dzsoi VU.! jj | jj <- [0..jmax-1] ]
                    surfTot = sum [ surface_prof VU.! jj * dzsoi VU.! jj | jj <- [0..jmax-1] ]
                in if altIdx VU.! c > 0 && rootfrTot > 0.0 && surfTot > 0.0
                   then if j < jmax
                        then case profType of
                          0 -> surface_prof VU.! j / surfTot      -- leaf
                          1 -> cinput_rootfr VU.! idxP np p j / rootfrTot  -- froot
                          2 -> cinput_rootfr VU.! idxP np p j / rootfrTot  -- croot
                          3 -> surface_prof VU.! j / surfTot      -- stem
                          _ -> 0.0
                        else 0.0
                   else if j == 0
                        then 1.0 / dzsoi VU.! 0
                        else 0.0

      extractProf t = VU.generate (np * nlevFull) $ \ix ->
        patchProfs VU.! (t * np * nlevFull + ix)

      leaf_prof  = extractProf 0
      froot_prof = extractProf 1
      croot_prof = extractProf 2
      stem_prof  = extractProf 3

      -- Column profiles: aggregate rootfr from patches
      col_cinput_rootfr = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in sum [ cinput_rootfr VU.! idxP np p j * pWt VU.! p
               | p <- [0..np-1], maskP VU.! p, pCol VU.! p == c ]

      -- Nfix and Ndep profiles
      colProfs = VU.generate (nc * nlevFull * 2) $ \ix ->
        let profType = ix `div` (nc * nlevFull)
            rem1 = ix `mod` (nc * nlevFull)
            j = rem1 `div` nc; c = rem1 `mod` nc
        in if not (maskC VU.! c) || j >= nlev then 0.0
           else let jmax = min (max (altIdx VU.! c) 1) nlev
                    rootfrTot = sum [ col_cinput_rootfr VU.! idxC nc c jj * dzsoi VU.! jj | jj <- [0..jmax-1] ]
                    surfTot = sum [ surface_prof VU.! jj * dzsoi VU.! jj | jj <- [0..jmax-1] ]
                in if altIdx VU.! c > 0 && rootfrTot > 0.0 && surfTot > 0.0
                   then if j < jmax
                        then case profType of
                          0 -> col_cinput_rootfr VU.! idxC nc c j / rootfrTot  -- nfix
                          1 -> surface_prof VU.! j / surfTot                    -- ndep
                          _ -> 0.0
                        else 0.0
                   else if j == 0
                        then 1.0 / dzsoi VU.! 0
                        else 0.0

      nfixProf = VU.generate (nc * nlevFull) $ \ix ->
        colProfs VU.! ix
      ndepProf = VU.generate (nc * nlevFull) $ \ix ->
        colProfs VU.! (nc * nlevFull + ix)

  in VertProfileOutput
    { vpo_leaf_prof = leaf_prof
    , vpo_froot_prof = froot_prof
    , vpo_croot_prof = croot_prof
    , vpo_stem_prof = stem_prof
    , vpo_nfixation_prof = nfixProf
    , vpo_ndep_prof = ndepProf
    }
