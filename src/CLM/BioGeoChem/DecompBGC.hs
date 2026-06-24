{-# LANGUAGE BangPatterns #-}
-- | BGC (CENTURY) decomposition cascade configuration and rate constants.
-- Fortran: SoilBiogeochemDecompCascadeBGCMod.F90
-- Julia:   src/biogeochem/decomp_bgc.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompBGC
  ( -- * Data types
    DecompCascadeConData(..)
  , DecompBGCParams(..)
  , defaultDecompBGCParams
  , DecompBGCState(..)
  , defaultDecompBGCState
  , CNSharedParams(..)
    -- * Initialization
  , InitCascadeInput(..)
  , InitCascadeOutput(..)
  , initDecompCascadeBGC
    -- * Rate constants
  , RateConstInput(..)
  , RateConstOutput(..)
  , decompRateConstantsBGC
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Decomposition cascade configuration (pools, transitions, names).
data DecompCascadeConData = DecompCascadeConData
  { dcc_cascade_donor_pool    :: !(VU.Vector Int)
  , dcc_cascade_receiver_pool :: !(VU.Vector Int)
  , dcc_floating_cn_ratio     :: !(VU.Vector Bool)
  , dcc_is_litter             :: !(VU.Vector Bool)
  , dcc_is_soil               :: !(VU.Vector Bool)
  , dcc_is_cwd                :: !(VU.Vector Bool)
  , dcc_initial_cn_ratio      :: !(VU.Vector Double)
  , dcc_initial_stock         :: !(VU.Vector Double)
  , dcc_initial_stock_soildepth :: !Double
  , dcc_is_metabolic          :: !(VU.Vector Bool)
  , dcc_is_cellulose          :: !(VU.Vector Bool)
  , dcc_is_lignin             :: !(VU.Vector Bool)
  , dcc_spinup_factor         :: !(VU.Vector Double)
  } deriving (Show)

-- | BGC decomposition parameters.
data DecompBGCParams = DecompBGCParams
  { dbp_cn_s1      :: !Double
  , dbp_cn_s2      :: !Double
  , dbp_cn_s3      :: !Double
  , dbp_rf_l1s1    :: !Double
  , dbp_rf_l2s1    :: !Double
  , dbp_rf_l3s2    :: !Double
  , dbp_rf_s2s1    :: !Double
  , dbp_rf_s2s3    :: !Double
  , dbp_rf_s3s1    :: !Double
  , dbp_rf_cwdl3   :: !Double
  , dbp_tau_l1     :: !Double
  , dbp_tau_l2_l3  :: !Double
  , dbp_tau_s1     :: !Double
  , dbp_tau_s2     :: !Double
  , dbp_tau_s3     :: !Double
  , dbp_cwd_fcel   :: !Double
  , dbp_initial_Cstocks :: !(VU.Vector Double)
  , dbp_initial_Cstocks_depth :: !Double
  } deriving (Show)

defaultDecompBGCParams :: DecompBGCParams
defaultDecompBGCParams = DecompBGCParams
  { dbp_cn_s1      = 12.0
  , dbp_cn_s2      = 12.0
  , dbp_cn_s3      = 10.0
  , dbp_rf_l1s1    = 0.39
  , dbp_rf_l2s1    = 0.55
  , dbp_rf_l3s2    = 0.29
  , dbp_rf_s2s1    = 0.55
  , dbp_rf_s2s3    = 0.55
  , dbp_rf_s3s1    = 0.55
  , dbp_rf_cwdl3   = 0.0
  , dbp_tau_l1     = 1.0 / 18.5
  , dbp_tau_l2_l3  = 1.0 / 4.9
  , dbp_tau_s1     = 1.0 / 7.3
  , dbp_tau_s2     = 1.0 / 0.2
  , dbp_tau_s3     = 1.0 / 0.0045
  , dbp_cwd_fcel   = 0.0
  , dbp_initial_Cstocks = VU.empty
  , dbp_initial_Cstocks_depth = 0.3
  }

-- | Module-level state for BGC decomposition.
data DecompBGCState = DecompBGCState
  { dbs_i_pas_som :: !Int
  , dbs_i_slo_som :: !Int
  , dbs_i_act_som :: !Int
  , dbs_i_cel_lit :: !Int
  , dbs_i_lig_lit :: !Int
  , dbs_cwd_fcel  :: !Double
  , dbs_rf_l1s1   :: !Double
  , dbs_rf_l2s1   :: !Double
  , dbs_rf_l3s2   :: !Double
  , dbs_rf_s2s1   :: !Double
  , dbs_rf_s2s3   :: !Double
  , dbs_rf_s3s1   :: !Double
  , dbs_rf_cwdl3  :: !Double
  , dbs_rf_s1s2   :: !(VU.Vector Double)  -- ^ (nc * nlevdecomp)
  , dbs_rf_s1s3   :: !(VU.Vector Double)
  , dbs_f_s1s2    :: !(VU.Vector Double)
  , dbs_f_s1s3    :: !(VU.Vector Double)
  , dbs_f_s2s1    :: !Double
  , dbs_f_s2s3    :: !Double
  , dbs_i_l1s1    :: !Int
  , dbs_i_l2s1    :: !Int
  , dbs_i_l3s2    :: !Int
  , dbs_i_s1s2    :: !Int
  , dbs_i_s1s3    :: !Int
  , dbs_i_s2s1    :: !Int
  , dbs_i_s2s3    :: !Int
  , dbs_i_s3s1    :: !Int
  , dbs_i_cwdl3   :: !Int
  , dbs_normalize_q10 :: !Bool
  , dbs_use_century_tfunc :: !Bool
  , dbs_normalization_tref :: !Double
  } deriving (Show)

defaultDecompBGCState :: DecompBGCState
defaultDecompBGCState = DecompBGCState
  { dbs_i_pas_som = 0, dbs_i_slo_som = 0, dbs_i_act_som = 0
  , dbs_i_cel_lit = 0, dbs_i_lig_lit = 0
  , dbs_cwd_fcel = 0.0
  , dbs_rf_l1s1 = 0.0, dbs_rf_l2s1 = 0.0, dbs_rf_l3s2 = 0.0
  , dbs_rf_s2s1 = 0.0, dbs_rf_s2s3 = 0.0, dbs_rf_s3s1 = 0.0
  , dbs_rf_cwdl3 = 0.0
  , dbs_rf_s1s2 = VU.empty, dbs_rf_s1s3 = VU.empty
  , dbs_f_s1s2 = VU.empty, dbs_f_s1s3 = VU.empty
  , dbs_f_s2s1 = 0.0, dbs_f_s2s3 = 0.0
  , dbs_i_l1s1 = 0, dbs_i_l2s1 = 0, dbs_i_l3s2 = 0
  , dbs_i_s1s2 = 0, dbs_i_s1s3 = 0, dbs_i_s2s1 = 0
  , dbs_i_s2s3 = 0, dbs_i_s3s1 = 0, dbs_i_cwdl3 = 0
  , dbs_normalize_q10 = True
  , dbs_use_century_tfunc = False
  , dbs_normalization_tref = 15.0
  }

-- | Shared CN parameters used across decomposition modules.
data CNSharedParams = CNSharedParams
  { cnsp_cwd_flig               :: !Double
  , cnsp_rf_cwdl2               :: !Double
  , cnsp_minpsi                 :: !Double
  , cnsp_maxpsi                 :: !Double
  , cnsp_Q10                    :: !Double
  , cnsp_froz_q10               :: !Double
  , cnsp_decomp_depth_efolding  :: !Double
  , cnsp_mino2lim               :: !Double
  , cnsp_tau_cwd                :: !Double
  , cnsp_organic_max            :: !Double
  } deriving (Show)

-- | Input to cascade initialization.
data InitCascadeInput = InitCascadeInput
  { ici_cellsand     :: !(VU.Vector Double)  -- ^ (nc * nlevdecomp)
  , ici_nc           :: !Int
  , ici_nlevdecomp   :: !Int
  , ici_use_fates    :: !Bool
  , ici_params       :: !DecompBGCParams
  , ici_cn_params    :: !CNSharedParams
  } deriving (Show)

-- | Output of cascade initialization.
data InitCascadeOutput = InitCascadeOutput
  { ico_bgc_state    :: !DecompBGCState
  , ico_cascade_con  :: !DecompCascadeConData
  , ico_ndecomp_pools :: !Int
  , ico_ndecomp_cascade_transitions :: !Int
  } deriving (Show)

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Initialize the BGC decomposition cascade.
initDecompCascadeBGC :: InitCascadeInput -> InitCascadeOutput
initDecompCascadeBGC inp =
  let !nc   = ici_nc inp
      !nlev = ici_nlevdecomp inp
      params = ici_params inp
      cn_s1 = dbp_cn_s1 params
      cn_s2 = dbp_cn_s2 params
      cn_s3 = dbp_cn_s3 params
      useFates = ici_use_fates inp
      sand = ici_cellsand inp

      -- Sand-dependent fractions
      f_s1s2_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            t = 0.85 - 0.68 * 0.01 * (100.0 - sand VU.! idx2 nc c j)
        in 1.0 - 0.004 / (1.0 - t)
      f_s1s3_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
            t = 0.85 - 0.68 * 0.01 * (100.0 - sand VU.! idx2 nc c j)
        in 0.004 / (1.0 - t)
      rf_s1s2_v = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in 0.85 - 0.68 * 0.01 * (100.0 - sand VU.! idx2 nc c j)
      rf_s1s3_v = rf_s1s2_v

      -- Pool indices (1-based)
      i_met_lit = 1; i_cel_lit = 2; i_lig_lit = 3
      i_act_som = 4; i_slo_som = 5; i_pas_som = 6
      i_cwd_local = if useFates then 0 else 7
      npools = if useFates then 6 else 7

      -- Transition indices
      i_l1s1 = 1; i_l2s1 = 2; i_l3s2 = 3
      i_s1s2 = 4; i_s1s3 = 5; i_s2s1 = 6; i_s2s3 = 7; i_s3s1 = 8
      ntrans = if useFates then 8 else 10
      i_cwdl3 = 10

      -- Build cascade_con
      initStocks = dbp_initial_Cstocks params
      donor = VU.fromList $
        [i_met_lit, i_cel_lit, i_lig_lit, i_act_som, i_act_som, i_slo_som, i_slo_som, i_pas_som]
        ++ if useFates then [] else [i_cwd_local, i_cwd_local]
      recv = VU.fromList $
        [i_act_som, i_act_som, i_slo_som, i_slo_som, i_pas_som, i_act_som, i_pas_som, i_act_som]
        ++ if useFates then [] else [i_cel_lit, i_lig_lit]

      floating = VU.fromList $
        [True, True, True, False, False, False]
        ++ if useFates then [] else [True]
      isLitter = VU.fromList $
        [True, True, True, False, False, False]
        ++ if useFates then [] else [False]
      isSoil = VU.fromList $
        [False, False, False, True, True, True]
        ++ if useFates then [] else [False]
      isCwd = VU.fromList $
        (replicate 6 False)
        ++ if useFates then [] else [True]
      initCN = VU.fromList $
        [90.0, 90.0, 90.0, cn_s1, cn_s2, cn_s3]
        ++ if useFates then [] else [90.0]
      initStock = if VU.length initStocks >= npools
                  then VU.take npools initStocks
                  else VU.replicate npools 0.0
      spinFac = VU.fromList $
        [1.0, 1.0, 1.0, 1.0, max 1.0 (dbp_tau_s2 params), max 1.0 (dbp_tau_s3 params)]
        ++ if useFates then [] else [1.0]
      isMet = VU.fromList $
        [True, False, False, False, False, False]
        ++ if useFates then [] else [False]
      isCel = VU.fromList $
        [False, True, False, False, False, False]
        ++ if useFates then [] else [False]
      isLig = VU.fromList $
        [False, False, True, False, False, False]
        ++ if useFates then [] else [False]

      cascadeCon = DecompCascadeConData
        { dcc_cascade_donor_pool    = donor
        , dcc_cascade_receiver_pool = recv
        , dcc_floating_cn_ratio     = floating
        , dcc_is_litter             = isLitter
        , dcc_is_soil               = isSoil
        , dcc_is_cwd                = isCwd
        , dcc_initial_cn_ratio      = initCN
        , dcc_initial_stock         = initStock
        , dcc_initial_stock_soildepth = dbp_initial_Cstocks_depth params
        , dcc_is_metabolic          = isMet
        , dcc_is_cellulose          = isCel
        , dcc_is_lignin             = isLig
        , dcc_spinup_factor         = spinFac
        }

      bgcState = defaultDecompBGCState
        { dbs_i_cel_lit = i_cel_lit
        , dbs_i_lig_lit = i_lig_lit
        , dbs_i_act_som = i_act_som
        , dbs_i_slo_som = i_slo_som
        , dbs_i_pas_som = i_pas_som
        , dbs_cwd_fcel  = dbp_cwd_fcel params
        , dbs_rf_l1s1   = dbp_rf_l1s1 params
        , dbs_rf_l2s1   = dbp_rf_l2s1 params
        , dbs_rf_l3s2   = dbp_rf_l3s2 params
        , dbs_rf_s2s1   = dbp_rf_s2s1 params
        , dbs_rf_s2s3   = dbp_rf_s2s3 params
        , dbs_rf_s3s1   = dbp_rf_s3s1 params
        , dbs_rf_cwdl3  = dbp_rf_cwdl3 params
        , dbs_rf_s1s2   = rf_s1s2_v
        , dbs_rf_s1s3   = rf_s1s3_v
        , dbs_f_s1s2    = f_s1s2_v
        , dbs_f_s1s3    = f_s1s3_v
        , dbs_f_s2s1    = 0.42 / 0.45
        , dbs_f_s2s3    = 0.03 / 0.45
        , dbs_i_l1s1    = i_l1s1
        , dbs_i_l2s1    = i_l2s1
        , dbs_i_l3s2    = i_l3s2
        , dbs_i_s1s2    = i_s1s2
        , dbs_i_s1s3    = i_s1s3
        , dbs_i_s2s1    = i_s2s1
        , dbs_i_s2s3    = i_s2s3
        , dbs_i_s3s1    = i_s3s1
        , dbs_i_cwdl3   = i_cwdl3
        }

  in InitCascadeOutput
    { ico_bgc_state    = bgcState
    , ico_cascade_con  = cascadeCon
    , ico_ndecomp_pools = npools
    , ico_ndecomp_cascade_transitions = ntrans
    }

-- | Input for rate constant calculation.
data RateConstInput = RateConstInput
  { rci_nc              :: !Int
  , rci_nlevdecomp      :: !Int
  , rci_mask            :: !(VU.Vector Bool)
  , rci_t_soisno        :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , rci_soilpsi         :: !(VU.Vector Double)
  , rci_days_per_year   :: !Double
  , rci_dt              :: !Double
  , rci_zsoi            :: !(VU.Vector Double)
  , rci_bgc_state       :: !DecompBGCState
  , rci_params          :: !DecompBGCParams
  , rci_cn_params       :: !CNSharedParams
  } deriving (Show)

-- | Output of rate constant calculation.
data RateConstOutput = RateConstOutput
  { rco_t_scalar     :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , rco_w_scalar     :: !(VU.Vector Double)
  , rco_o_scalar     :: !(VU.Vector Double)
  , rco_decomp_k     :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , rco_rf_decomp    :: !(VU.Vector Double)  -- ^ (nc * nlev * ntrans)
  , rco_pathfrac     :: !(VU.Vector Double)  -- ^ (nc * nlev * ntrans)
  } deriving (Show)

-- | CENTURY arctangent temperature function.
catanf :: Double -> Double
catanf t1 = 11.75 + (29.7 / pi) * atan (pi * 0.031 * (t1 - 15.4))
{-# INLINE catanf #-}

-- | Freezing point (K)
tfrz :: Double
tfrz = 273.15

-- | Seconds per day
secspday :: Double
secspday = 86400.0

-- | Calculate decomposition rate constants for the BGC cascade.
decompRateConstantsBGC :: RateConstInput -> RateConstOutput
decompRateConstantsBGC inp =
  let !nc   = rci_nc inp
      !nlev = rci_nlevdecomp inp
      mask  = rci_mask inp
      tsoi  = rci_t_soisno inp
      psi   = rci_soilpsi inp
      zsoi  = rci_zsoi inp
      dpy   = rci_days_per_year inp
      bgc   = rci_bgc_state inp
      params = rci_params inp
      cnp   = rci_cn_params inp

      q10      = cnsp_Q10 cnp
      frozQ10  = cnsp_froz_q10 cnp
      minpsi_v = cnsp_minpsi cnp
      maxpsi_v = cnsp_maxpsi cnp
      defolding = cnsp_decomp_depth_efolding cnp

      catanf_30 = catanf 30.0

      useCentury = dbs_use_century_tfunc bgc
      normQ10    = dbs_normalize_q10 bgc

      -- Rate constants (per second)
      k_l1    = 1.0 / (secspday * dpy * dbp_tau_l1 params)
      k_l2_l3 = 1.0 / (secspday * dpy * dbp_tau_l2_l3 params)
      k_s1    = 1.0 / (secspday * dpy * dbp_tau_s1 params)
      k_s2    = 1.0 / (secspday * dpy * dbp_tau_s2 params)
      k_s3    = 1.0 / (secspday * dpy * dbp_tau_s3 params)
      k_frag  = 1.0 / (secspday * dpy * cnsp_tau_cwd cnp)

      size2 = nc * nlev

      -- Temperature scalar
      t_scalar = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else let tv = tsoi VU.! idx2 nc c j
                in if not useCentury
                   then if tv >= tfrz
                        then q10 ** ((tv - (tfrz + 25.0)) / 10.0)
                        else (q10 ** (-25.0 / 10.0)) * (frozQ10 ** ((tv - tfrz) / 10.0))
                   else max (catanf (tv - tfrz) / catanf_30) 0.01

      -- Normalization factor
      normFactor = if normQ10
        then (catanf (dbs_normalization_tref bgc) / catanf_30) /
             (q10 ** ((dbs_normalization_tref bgc - 25.0) / 10.0))
        else 1.0

      t_scalar_norm = if normQ10
        then VU.map (* normFactor) t_scalar
        else t_scalar

      -- Water scalar
      w_scalar = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else let pv = min (psi VU.! idx2 nc c j) maxpsi_v
                in if pv > minpsi_v
                   then log (minpsi_v / pv) / log (minpsi_v / maxpsi_v)
                   else 0.0

      -- O2 scalar: always 1.0
      o_scalar = VU.replicate size2 1.0

      -- Depth scalar
      depth_scalar = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else exp (negate (zsoi VU.! j) / defolding)

      -- Pool indices
      i_met = 0; i_cel = dbs_i_cel_lit bgc - 1; i_lig = dbs_i_lig_lit bgc - 1
      i_act = dbs_i_act_som bgc - 1; i_slo = dbs_i_slo_som bgc - 1
      i_pas = dbs_i_pas_som bgc - 1
      npools = if i_pas + 2 <= 7 then 7 else 6

      -- Decomp_k (nc * nlev * npools)
      decomp_k = VU.generate (nc * nlev * npools) $ \ix ->
        let pool = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            rate = (t_scalar_norm VU.! idx2 nc c j) *
                   (w_scalar VU.! idx2 nc c j) *
                   (depth_scalar VU.! idx2 nc c j) *
                   (o_scalar VU.! idx2 nc c j)
        in if not (mask VU.! c) then 0.0
           else if pool == i_met then k_l1 * rate
           else if pool == i_cel then k_l2_l3 * rate
           else if pool == i_lig then k_l2_l3 * rate
           else if pool == i_act then k_s1 * rate
           else if pool == i_slo then k_s2 * rate
           else if pool == i_pas then k_s3 * rate
           else if pool == npools - 1 then k_frag * rate  -- CWD
           else 0.0

      -- Respiration fractions and path fractions
      ntrans = if npools == 7 then 10 else 8
      size3t = nc * nlev * ntrans

      idx3t c j k = c + nc * (j + nlev * k)

      rf_decomp = VU.generate size3t $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            i2 = idx2 nc c j
        in if k == dbs_i_l1s1 bgc - 1 then dbs_rf_l1s1 bgc
           else if k == dbs_i_l2s1 bgc - 1 then dbs_rf_l2s1 bgc
           else if k == dbs_i_l3s2 bgc - 1 then dbs_rf_l3s2 bgc
           else if k == dbs_i_s1s2 bgc - 1 then dbs_rf_s1s2 bgc VU.! i2
           else if k == dbs_i_s1s3 bgc - 1 then dbs_rf_s1s3 bgc VU.! i2
           else if k == dbs_i_s2s1 bgc - 1 then dbs_rf_s2s1 bgc
           else if k == dbs_i_s2s3 bgc - 1 then dbs_rf_s2s3 bgc
           else if k == dbs_i_s3s1 bgc - 1 then dbs_rf_s3s1 bgc
           else 0.0

      pathfrac = VU.generate size3t $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            i2 = idx2 nc c j
        in if k == dbs_i_l1s1 bgc - 1 then 1.0
           else if k == dbs_i_l2s1 bgc - 1 then 1.0
           else if k == dbs_i_l3s2 bgc - 1 then 1.0
           else if k == dbs_i_s1s2 bgc - 1 then dbs_f_s1s2 bgc VU.! i2
           else if k == dbs_i_s1s3 bgc - 1 then dbs_f_s1s3 bgc VU.! i2
           else if k == dbs_i_s2s1 bgc - 1 then dbs_f_s2s1 bgc
           else if k == dbs_i_s2s3 bgc - 1 then dbs_f_s2s3 bgc
           else if k == dbs_i_s3s1 bgc - 1 then 1.0
           else 0.0

  in RateConstOutput
    { rco_t_scalar  = t_scalar_norm
    , rco_w_scalar  = w_scalar
    , rco_o_scalar  = o_scalar
    , rco_decomp_k  = decomp_k
    , rco_rf_decomp = rf_decomp
    , rco_pathfrac  = pathfrac
    }
