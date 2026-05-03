{-# LANGUAGE BangPatterns #-}
-- | MIMICS decomposition cascade configuration and rate constants.
-- Fortran: SoilBiogeochemDecompCascadeMIMICSMod.F90
-- Julia:   src/biogeochem/decomp_mimics.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompMIMICS
  ( -- * Data types
    DecompMIMICSParams(..)
  , defaultDecompMIMICSParams
  , DecompMIMICSState(..)
  , defaultDecompMIMICSState
    -- * Initialization
  , InitMIMICSInput(..)
  , InitMIMICSOutput(..)
  , initDecompCascadeMIMICS
    -- * Rate constants
  , MIMICSRateInput(..)
  , MIMICSRateOutput(..)
  , decompRatesMIMICS
  ) where

import qualified Data.Vector.Unboxed as VU

-- | MIMICS decomposition parameters.
data DecompMIMICSParams = DecompMIMICSParams
  { dmp_nue_into_mic           :: !Double
  , dmp_desorpQ10              :: !Double
  , dmp_densdep                :: !Double
  , dmp_tau_mod_factor         :: !Double
  , dmp_tau_mod_min            :: !Double
  , dmp_tau_mod_max            :: !Double
  , dmp_ko_r                   :: !Double
  , dmp_ko_k                   :: !Double
  , dmp_cn_r                   :: !Double
  , dmp_cn_k                   :: !Double
  , dmp_cn_mod_num             :: !Double
  , dmp_t_soi_ref              :: !Double
  , dmp_initial_Cstocks_depth  :: !Double
  , dmp_initial_Cstocks        :: !(VU.Vector Double)
  , dmp_mge                    :: !(VU.Vector Double)  -- ^ 6 microbial growth efficiencies
  , dmp_vmod                   :: !(VU.Vector Double)  -- ^ 6 Vmax modifiers
  , dmp_vint                   :: !(VU.Vector Double)  -- ^ 6 Vmax intercepts
  , dmp_vslope                 :: !(VU.Vector Double)  -- ^ 6 Vmax slopes
  , dmp_kmod                   :: !(VU.Vector Double)  -- ^ 6 Km modifiers
  , dmp_kint                   :: !(VU.Vector Double)  -- ^ 6 Km intercepts
  , dmp_kslope                 :: !(VU.Vector Double)  -- ^ 6 Km slopes
  , dmp_fmet                   :: !(VU.Vector Double)  -- ^ 4 metabolic fraction params
  , dmp_p_scalar               :: !(VU.Vector Double)  -- ^ 2 protection scalar params
  , dmp_fphys_r                :: !(VU.Vector Double)  -- ^ 2 physical fraction (r) params
  , dmp_fphys_k                :: !(VU.Vector Double)  -- ^ 2 physical fraction (k) params
  , dmp_fchem_r                :: !(VU.Vector Double)  -- ^ 2 chemical fraction (r) params
  , dmp_fchem_k                :: !(VU.Vector Double)  -- ^ 2 chemical fraction (k) params
  , dmp_desorp                 :: !(VU.Vector Double)  -- ^ 2 desorption params
  , dmp_tau_r                  :: !(VU.Vector Double)  -- ^ 2 microbe turnover (r)
  , dmp_tau_k                  :: !(VU.Vector Double)  -- ^ 2 microbe turnover (k)
  } deriving (Show)

defaultDecompMIMICSParams :: DecompMIMICSParams
defaultDecompMIMICSParams = DecompMIMICSParams
  { dmp_nue_into_mic = 0.0, dmp_desorpQ10 = 0.0, dmp_densdep = 1.0
  , dmp_tau_mod_factor = 0.0, dmp_tau_mod_min = 0.0, dmp_tau_mod_max = 0.0
  , dmp_ko_r = 0.0, dmp_ko_k = 0.0, dmp_cn_r = 0.0, dmp_cn_k = 0.0
  , dmp_cn_mod_num = 0.0, dmp_t_soi_ref = 25.0
  , dmp_initial_Cstocks_depth = 0.3
  , dmp_initial_Cstocks = VU.empty
  , dmp_mge = VU.empty, dmp_vmod = VU.empty, dmp_vint = VU.empty
  , dmp_vslope = VU.empty, dmp_kmod = VU.empty, dmp_kint = VU.empty
  , dmp_kslope = VU.empty, dmp_fmet = VU.empty, dmp_p_scalar = VU.empty
  , dmp_fphys_r = VU.empty, dmp_fphys_k = VU.empty
  , dmp_fchem_r = VU.empty, dmp_fchem_k = VU.empty
  , dmp_desorp = VU.empty, dmp_tau_r = VU.empty, dmp_tau_k = VU.empty
  }

-- | Module-level state for MIMICS decomposition.
data DecompMIMICSState = DecompMIMICSState
  { dms_desorp     :: !(VU.Vector Double)  -- ^ (nc * nlevdecomp)
  , dms_fphys_m1   :: !(VU.Vector Double)
  , dms_fphys_m2   :: !(VU.Vector Double)
  , dms_p_scalar   :: !(VU.Vector Double)
  , dms_i_phys_som :: !Int
  , dms_i_chem_som :: !Int
  , dms_i_avl_som  :: !Int
  , dms_i_str_lit  :: !Int
  , dms_i_met_lit  :: !Int
  , dms_i_cop_mic  :: !Int
  , dms_i_oli_mic  :: !Int
  -- Transition indices
  , dms_i_l1m1 :: !Int, dms_i_l1m2 :: !Int
  , dms_i_l2m1 :: !Int, dms_i_l2m2 :: !Int
  , dms_i_s1m1 :: !Int, dms_i_s1m2 :: !Int
  , dms_i_m1s1 :: !Int, dms_i_m1s2 :: !Int
  , dms_i_m2s1 :: !Int, dms_i_m2s2 :: !Int
  , dms_i_s2s1 :: !Int, dms_i_s3s1 :: !Int
  , dms_i_m1s3 :: !Int, dms_i_m2s3 :: !Int
  -- Respiration fractions
  , dms_rf_l1m1 :: !Double, dms_rf_l1m2 :: !Double
  , dms_rf_l2m1 :: !Double, dms_rf_l2m2 :: !Double
  , dms_rf_s1m1 :: !Double, dms_rf_s1m2 :: !Double
  -- Vmax/Km regression (36 values)
  , dms_vmod :: !(VU.Vector Double)    -- ^ 6 vmod values
  , dms_vint :: !(VU.Vector Double)    -- ^ 6 vint values
  , dms_vslope :: !(VU.Vector Double)  -- ^ 6 vslope values
  , dms_kmod :: !(VU.Vector Double)    -- ^ 6 kmod values
  , dms_kint :: !(VU.Vector Double)    -- ^ 6 kint values
  , dms_kslope :: !(VU.Vector Double)  -- ^ 6 kslope values
  } deriving (Show)

defaultDecompMIMICSState :: DecompMIMICSState
defaultDecompMIMICSState = DecompMIMICSState
  { dms_desorp = VU.empty, dms_fphys_m1 = VU.empty
  , dms_fphys_m2 = VU.empty, dms_p_scalar = VU.empty
  , dms_i_phys_som = 0, dms_i_chem_som = 0, dms_i_avl_som = 0
  , dms_i_str_lit = 0, dms_i_met_lit = 0
  , dms_i_cop_mic = 0, dms_i_oli_mic = 0
  , dms_i_l1m1 = 0, dms_i_l1m2 = 0, dms_i_l2m1 = 0, dms_i_l2m2 = 0
  , dms_i_s1m1 = 0, dms_i_s1m2 = 0, dms_i_m1s1 = 0, dms_i_m1s2 = 0
  , dms_i_m2s1 = 0, dms_i_m2s2 = 0, dms_i_s2s1 = 0, dms_i_s3s1 = 0
  , dms_i_m1s3 = 0, dms_i_m2s3 = 0
  , dms_rf_l1m1 = 0.0, dms_rf_l1m2 = 0.0
  , dms_rf_l2m1 = 0.0, dms_rf_l2m2 = 0.0
  , dms_rf_s1m1 = 0.0, dms_rf_s1m2 = 0.0
  , dms_vmod = VU.empty, dms_vint = VU.empty, dms_vslope = VU.empty
  , dms_kmod = VU.empty, dms_kint = VU.empty, dms_kslope = VU.empty
  }

-- | Input for MIMICS cascade initialization.
data InitMIMICSInput = InitMIMICSInput
  { imi_cellclay   :: !(VU.Vector Double)  -- ^ (nc * nlevdecomp)
  , imi_nc         :: !Int
  , imi_nlevdecomp :: !Int
  , imi_use_fates  :: !Bool
  , imi_params     :: !DecompMIMICSParams
  } deriving (Show)

-- | Output of MIMICS cascade initialization.
data InitMIMICSOutput = InitMIMICSOutput
  { imo_mimics_state :: !DecompMIMICSState
  , imo_nue_decomp_cascade :: !(VU.Vector Double)
  , imo_ndecomp_pools :: !Int
  , imo_ndecomp_cascade_transitions :: !Int
  } deriving (Show)

-- | 2D index
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Percent to fraction
pctToFrac :: Double
pctToFrac = 0.01

-- | Initialize MIMICS cascade (simplified).
initDecompCascadeMIMICS :: InitMIMICSInput -> InitMIMICSOutput
initDecompCascadeMIMICS inp =
  let !nc   = imi_nc inp
      !nlev = imi_nlevdecomp inp
      params = imi_params inp
      clay  = imi_cellclay inp

      -- Soil texture-dependent parameters
      desorp_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            cf = pctToFrac * min 100.0 (clay VU.! idx2 nc c j)
        in (dmp_desorp params VU.! 0) * exp ((dmp_desorp params VU.! 1) * cf)

      fphys_m1_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            cf = pctToFrac * min 100.0 (clay VU.! idx2 nc c j)
        in min 1.0 ((dmp_fphys_r params VU.! 0) * exp ((dmp_fphys_r params VU.! 1) * cf))

      fphys_m2_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            cf = pctToFrac * min 100.0 (clay VU.! idx2 nc c j)
        in min 1.0 ((dmp_fphys_k params VU.! 0) * exp ((dmp_fphys_k params VU.! 1) * cf))

      p_scalar_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            cf = pctToFrac * min 100.0 (clay VU.! idx2 nc c j)
        in 1.0 / ((dmp_p_scalar params VU.! 0) * exp ((dmp_p_scalar params VU.! 1) * sqrt cf))

      -- Respiration fractions
      mge = dmp_mge params
      rf_l1m1 = 1.0 - mge VU.! 0
      rf_l2m1 = 1.0 - mge VU.! 1
      rf_s1m1 = 1.0 - mge VU.! 2
      rf_l1m2 = 1.0 - mge VU.! 3
      rf_l2m2 = 1.0 - mge VU.! 4
      rf_s1m2 = 1.0 - mge VU.! 5

      -- NUE cascade
      nue_val = dmp_nue_into_mic params
      nue = VU.fromList [nue_val, nue_val, nue_val, nue_val, nue_val, nue_val,
                          1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0]

      state = defaultDecompMIMICSState
        { dms_desorp = desorp_v, dms_fphys_m1 = fphys_m1_v
        , dms_fphys_m2 = fphys_m2_v, dms_p_scalar = p_scalar_v
        , dms_i_met_lit = 1, dms_i_str_lit = 2
        , dms_i_avl_som = 3, dms_i_chem_som = 4, dms_i_phys_som = 5
        , dms_i_cop_mic = 6, dms_i_oli_mic = 7
        , dms_i_l1m1 = 1, dms_i_l1m2 = 2, dms_i_l2m1 = 3, dms_i_l2m2 = 4
        , dms_i_s1m1 = 5, dms_i_s1m2 = 6, dms_i_s2s1 = 7, dms_i_s3s1 = 8
        , dms_i_m1s1 = 9, dms_i_m1s2 = 10, dms_i_m1s3 = 11
        , dms_i_m2s1 = 12, dms_i_m2s2 = 13, dms_i_m2s3 = 14
        , dms_rf_l1m1 = rf_l1m1, dms_rf_l1m2 = rf_l1m2
        , dms_rf_l2m1 = rf_l2m1, dms_rf_l2m2 = rf_l2m2
        , dms_rf_s1m1 = rf_s1m1, dms_rf_s1m2 = rf_s1m2
        , dms_vmod = dmp_vmod params, dms_vint = dmp_vint params
        , dms_vslope = dmp_vslope params
        , dms_kmod = dmp_kmod params, dms_kint = dmp_kint params
        , dms_kslope = dmp_kslope params
        }

  in InitMIMICSOutput
    { imo_mimics_state = state
    , imo_nue_decomp_cascade = nue
    , imo_ndecomp_pools = if imi_use_fates inp then 7 else 8
    , imo_ndecomp_cascade_transitions = if imi_use_fates inp then 14 else 15
    }

-- | Input for MIMICS rate calculation.
data MIMICSRateInput = MIMICSRateInput
  { mri_nc              :: !Int
  , mri_nlevdecomp      :: !Int
  , mri_mask            :: !(VU.Vector Bool)
  , mri_t_soisno        :: !(VU.Vector Double)
  , mri_soilpsi         :: !(VU.Vector Double)
  , mri_decomp_cpools   :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , mri_col_dz          :: !(VU.Vector Double)
  , mri_ligninNratioAvg :: !(VU.Vector Double)
  , mri_annsum_npp      :: !(VU.Vector Double)
  , mri_days_per_year   :: !Double
  , mri_dt              :: !Double
  , mri_state           :: !DecompMIMICSState
  , mri_params          :: !DecompMIMICSParams
  } deriving (Show)

-- | Output of MIMICS rate calculation.
data MIMICSRateOutput = MIMICSRateOutput
  { mro_decomp_k  :: !(VU.Vector Double)
  , mro_w_scalar  :: !(VU.Vector Double)
  , mro_o_scalar  :: !(VU.Vector Double)
  , mro_pathfrac  :: !(VU.Vector Double)
  , mro_rf_decomp :: !(VU.Vector Double)
  , mro_cn_col    :: !(VU.Vector Double)
  } deriving (Show)

-- | Calculate MIMICS decomposition rates (stub for full implementation).
decompRatesMIMICS :: MIMICSRateInput -> MIMICSRateOutput
decompRatesMIMICS inp =
  let !nc   = mri_nc inp
      !nlev = mri_nlevdecomp inp
      size2 = nc * nlev
      npools = 8
      ntrans = 15
  in MIMICSRateOutput
    { mro_decomp_k  = VU.replicate (nc * nlev * npools) 0.0
    , mro_w_scalar  = VU.replicate size2 1.0
    , mro_o_scalar  = VU.replicate size2 1.0
    , mro_pathfrac  = VU.replicate (nc * nlev * ntrans) 0.0
    , mro_rf_decomp = VU.replicate (nc * nlev * ntrans) 0.0
    , mro_cn_col    = VU.replicate (nc * npools) 0.0
    }
