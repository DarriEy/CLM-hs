{-# LANGUAGE BangPatterns #-}
-- | Maintenance respiration fluxes for coupled carbon-nitrogen code (CN).
-- Fortran: CNMRespMod.F90
-- Julia:   src/biogeochem/maint_resp.jl
--
-- All functions are pure.
module CLM.BioGeoChem.MaintResp
  ( -- * Data types
    MaintRespParams(..)
  , defaultMaintRespParams
  , PftConMaintResp(..)
  , MaintRespInput(..)
  , MaintRespOutput(..)
    -- * Main computation
  , cnMaintResp
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Freezing temperature of water (K).
tfrz :: Double
tfrz = 273.15

-- | Parameters for maintenance respiration.
data MaintRespParams = MaintRespParams
  { mrp_br      :: !Double  -- ^ base rate for MR (gC/gN/s)
  , mrp_br_root :: !Double  -- ^ base rate for root MR (gC/gN/s)
  , mrp_Q10     :: !Double  -- ^ Q10 temperature coefficient
  } deriving (Show, Eq)

defaultMaintRespParams :: MaintRespParams
defaultMaintRespParams = MaintRespParams
  { mrp_br      = 2.525e-6
  , mrp_br_root = 2.525e-6
  , mrp_Q10     = 1.5
  }

-- | PFT constants needed by maintenance respiration.
data PftConMaintResp = PftConMaintResp
  { pmr_woody :: !(VU.Vector Double)  -- ^ binary woody flag (1=woody)
  } deriving (Show)

-- | Input to maintenance respiration calculation.
data MaintRespInput = MaintRespInput
  { mri_np               :: !Int
  , mri_nc               :: !Int
  , mri_nlevgrnd         :: !Int
  , mri_mask_p           :: !(VU.Vector Bool)
  , mri_ivt              :: !(VU.Vector Int)       -- ^ PFT type per patch
  , mri_column           :: !(VU.Vector Int)       -- ^ patch-to-column mapping
  , mri_pftcon           :: !PftConMaintResp
  , mri_params           :: !MaintRespParams
  , mri_npcropmin        :: !Int
  -- Canopy state
  , mri_frac_veg_nosno   :: !(VU.Vector Int)
  , mri_laisun           :: !(VU.Vector Double)
  , mri_laisha           :: !(VU.Vector Double)
  -- Soil root fraction (np * nlevgrnd)
  , mri_crootfr          :: !(VU.Vector Double)
  -- Temperature (patch-level)
  , mri_t_ref2m          :: !(VU.Vector Double)    -- ^ 2m air temp (K)
  , mri_t_a10            :: !(VU.Vector Double)    -- ^ 10-day running mean T (K)
  -- Temperature (column-level, nc * nlevgrnd)
  , mri_t_soisno         :: !(VU.Vector Double)
  -- Photosynthesis
  , mri_lmrsun           :: !(VU.Vector Double)
  , mri_lmrsha           :: !(VU.Vector Double)
  , mri_rootstem_acc     :: !Bool
  -- Nitrogen state
  , mri_frootn           :: !(VU.Vector Double)
  , mri_livestemn        :: !(VU.Vector Double)
  , mri_livecrootn       :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of maintenance respiration calculation.
data MaintRespOutput = MaintRespOutput
  { mro_leaf_mr      :: !(VU.Vector Double)
  , mro_froot_mr     :: !(VU.Vector Double)
  , mro_livestem_mr  :: !(VU.Vector Double)
  , mro_livecroot_mr :: !(VU.Vector Double)
  } deriving (Show)

-- | 2D index helper for column arrays (nc * nlev)
idxC :: Int -> Int -> Int -> Int
idxC nc c j = c + nc * j
{-# INLINE idxC #-}

-- | 2D index helper for patch arrays (np * nlev)
idxP :: Int -> Int -> Int -> Int
idxP np p j = p + np * j
{-# INLINE idxP #-}

-- | Calculate maintenance respiration fluxes.
cnMaintResp :: MaintRespInput -> MaintRespOutput
cnMaintResp inp =
  let !np      = mri_np inp
      !nc      = mri_nc inp
      !nlevgrnd = mri_nlevgrnd inp
      maskP    = mri_mask_p inp
      ivt      = mri_ivt inp
      col      = mri_column inp
      pft      = mri_pftcon inp
      params   = mri_params inp
      npcropmin = mri_npcropmin inp

      br       = mrp_br params
      br_root  = mrp_br_root params
      q10      = mrp_Q10 params
      rootstem_acc = mri_rootstem_acc inp

      -- Column-level temperature correction factors for each soil layer
      -- tcsoi(c,j) = Q10^((t_soisno(c,j) - TFRZ - 20) / 10)
      tcsoi = VU.generate (nc * nlevgrnd) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            t = mri_t_soisno inp VU.! idxC nc c j
        in q10 ** ((t - tfrz - 20.0) / 10.0)

      iv p = ivt VU.! p + 1  -- 0-based to 1-based

      -- Acclimation factor
      acclFactor t10 = if rootstem_acc
                        then 10.0 ** (negate 0.00794 * ((t10 - tfrz) - 25.0))
                        else 1.0

      -- Leaf MR
      leaf_mr = VU.generate np $ \p ->
        if not (maskP VU.! p) then 0.0
        else if mri_frac_veg_nosno inp VU.! p == 1
             then mri_lmrsun inp VU.! p * mri_laisun inp VU.! p * 12.011e-6
                + mri_lmrsha inp VU.! p * mri_laisha inp VU.! p * 12.011e-6
             else 0.0

      -- Live stem MR
      livestem_mr = VU.generate np $ \p ->
        if not (maskP VU.! p) then 0.0
        else let tc = q10 ** ((mri_t_ref2m inp VU.! p - tfrz - 20.0) / 10.0)
                 af = acclFactor (mri_t_a10 inp VU.! p)
             in if pmr_woody pft VU.! iv p == 1.0 || ivt VU.! p >= npcropmin
                then mri_livestemn inp VU.! p * br * af * tc
                else 0.0

      -- Live coarse root MR
      livecroot_mr = VU.generate np $ \p ->
        if not (maskP VU.! p) then 0.0
        else let tc = q10 ** ((mri_t_ref2m inp VU.! p - tfrz - 20.0) / 10.0)
                 af = acclFactor (mri_t_a10 inp VU.! p)
             in if pmr_woody pft VU.! iv p == 1.0
                then mri_livecrootn inp VU.! p * br_root * af * tc
                else 0.0

      -- Fine root MR: sum over soil layers
      froot_mr = VU.generate np $ \p ->
        if not (maskP VU.! p) then 0.0
        else let c  = col VU.! p
                 af = acclFactor (mri_t_a10 inp VU.! p)
                 brr = br_root * af
             in sum [ mri_frootn inp VU.! p * brr
                    * (tcsoi VU.! idxC nc c j)
                    * (mri_crootfr inp VU.! idxP np p j)
                    | j <- [0..nlevgrnd-1] ]

  in MaintRespOutput
    { mro_leaf_mr      = leaf_mr
    , mro_froot_mr     = froot_mr
    , mro_livestem_mr  = livestem_mr
    , mro_livecroot_mr = livecroot_mr
    }
