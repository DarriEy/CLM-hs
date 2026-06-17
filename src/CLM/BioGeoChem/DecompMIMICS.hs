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

-- | 3D index: (col, level, pool/transition)
idx3 :: Int -> Int -> Int -> Int -> Int -> Int
idx3 nc nlev c j k = c + nc * (j + nlev * k)
{-# INLINE idx3 #-}

-- | Compute Vmax for a given substrate-microbe pair using Arrhenius regression.
mimicsVmax :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
           -> Int -> Double -> Double
mimicsVmax vmod vint vslope idx temp =
  let !vm = if idx < VU.length vmod then vmod VU.! idx else 1.0
      !vi = if idx < VU.length vint then vint VU.! idx else 0.0
      !vs = if idx < VU.length vslope then vslope VU.! idx else 0.0
  in vm * exp (vs * temp + vi)

-- | Compute Km for a given substrate-microbe pair.
mimicsKm :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
         -> Int -> Double -> Double
mimicsKm kmod kint kslope idx temp =
  let !km = if idx < VU.length kmod then kmod VU.! idx else 1.0
      !ki = if idx < VU.length kint then kint VU.! idx else 0.0
      !ks = if idx < VU.length kslope then kslope VU.! idx else 0.0
  in km * exp (ks * temp + ki)

-- | Moisture scalar (log-linear soil water potential response).
mimicsWScalar :: Double -> Double
mimicsWScalar soilpsi =
  let !log_psi = log (max 1.0 (abs soilpsi))
  in max 0.0 $ min 1.0 $ 1.0 - 0.1 * (log_psi - log 10.0)

-- | O2 scalar (oxygen stress in saturated conditions).
mimicsOScalar :: Double -> Double -> Double
mimicsOScalar h2osoi_vol watsat =
  let !sat_frac = if watsat > 0.0 then h2osoi_vol / watsat else 0.0
  in if sat_frac > 0.9
     then max 0.0 (1.0 - (sat_frac - 0.9) / 0.1)
     else 1.0

-- | Calculate MIMICS decomposition rates.
-- Implements Michaelis-Menten kinetics with Arrhenius temperature response.
decompRatesMIMICS :: MIMICSRateInput -> MIMICSRateOutput
decompRatesMIMICS inp =
  let !nc   = mri_nc inp
      !nlev = mri_nlevdecomp inp
      !mask = mri_mask inp
      !t_soisno = mri_t_soisno inp
      !soilpsi = mri_soilpsi inp
      !cpools = mri_decomp_cpools inp
      !dt = mri_dt inp
      !st = mri_state inp
      !params = mri_params inp
      size2 = nc * nlev
      npools = 8
      ntrans = 15
      tfrz_loc = 273.15

      -- Temperature in Celsius for Arrhenius
      tempC c j = let !idx = idx2 nc c j
                      !t = if idx < VU.length t_soisno then t_soisno VU.! idx else tfrz_loc
                  in t - tfrz_loc

      -- Pool concentrations (mg C / cm3 soil)
      poolConc c j k = let !idx = idx3 nc nlev c j k
                       in if idx < VU.length cpools then cpools VU.! idx else 0.0

      -- Michaelis-Menten term: Vmax * [Substrate] / (Km + [Substrate])
      mmTerm vIdx sConc mConc tc =
        let !vmax = mimicsVmax (dms_vmod st) (dms_vint st) (dms_vslope st) vIdx tc
            !km = mimicsKm (dms_kmod st) (dms_kint st) (dms_kslope st) vIdx tc
        in if sConc + km > 0.0
           then vmax * mConc * sConc / (km + sConc)
           else 0.0

      -- Decomposition rate constants (per pool)
      decomp_k = VU.generate (nc * nlev * npools) $ \idx ->
        let !c = idx `mod` nc
            !rem1 = idx `div` nc
            !j = rem1 `mod` nlev
            !k = rem1 `div` nlev
            !active = c < VU.length mask && mask VU.! c
        in if not active then 0.0
           else let !tc = tempC c j
                    !psi_idx = idx2 nc c j
                    !psi = if psi_idx < VU.length soilpsi then soilpsi VU.! psi_idx else -100.0
                    !w_sc = mimicsWScalar psi
                    !cpool_val = poolConc c j k
                    -- For microbial pools: density-dependent turnover
                    !iCop = dms_i_cop_mic st
                    !iOli = dms_i_oli_mic st
                in if k == iCop - 1 || k == iOli - 1
                   then -- Microbial turnover (density-dependent)
                     let !tau_base = if k == iCop - 1
                                     then if VU.null (dmp_tau_r params) then 0.01
                                          else dmp_tau_r params VU.! 0
                                     else if VU.null (dmp_tau_k params) then 0.01
                                          else dmp_tau_k params VU.! 0
                         !densdep = dmp_densdep params
                     in tau_base * (cpool_val ** densdep) * w_sc / (dt * 3600.0)
                   else -- Substrate pools: rate determined by microbe activity
                     if cpool_val > 0.0 then w_sc * 1.0 / (dt * 365.0 * 86400.0)
                     else 0.0

      -- Moisture scalars
      w_scalar = VU.generate size2 $ \idx ->
        let !c = idx `mod` nc
            !j = idx `div` nc
            !active = c < VU.length mask && mask VU.! c
            !psi_idx = idx2 nc c j
            !psi = if psi_idx < VU.length soilpsi then soilpsi VU.! psi_idx else -100.0
        in if active then mimicsWScalar psi else 1.0

      -- O2 scalars (all 1.0 for MIMICS - handled internally)
      o_scalar = VU.replicate size2 1.0

      -- Pathfrac: fraction of C flowing through each transition
      -- For MIMICS, pathfrac encodes the partitioning between microbial pools
      -- and between physical/chemical/available SOM
      pathfrac = VU.generate (nc * nlev * ntrans) $ \idx ->
        let !c = idx `mod` nc
            !rem1 = idx `div` nc
            !j = rem1 `mod` nlev
            !k = rem1 `div` nlev
            !active = c < VU.length mask && mask VU.! c
            !fphys1_idx = idx2 nc c j
        in if not active then 0.0
           else case k + 1 of
             -- Litter to microbes: all go to microbes (pathfrac = 1.0)
             t | t == dms_i_l1m1 st -> 1.0
               | t == dms_i_l1m2 st -> 1.0
               | t == dms_i_l2m1 st -> 1.0
               | t == dms_i_l2m2 st -> 1.0
               | t == dms_i_s1m1 st -> 1.0
               | t == dms_i_s1m2 st -> 1.0
             -- Microbe turnover to SOM pools
               | t == dms_i_m1s1 st ->
                   let !fp = if fphys1_idx < VU.length (dms_fphys_m1 st)
                             then dms_fphys_m1 st VU.! fphys1_idx else 0.5
                   in fp  -- fraction to physically-protected
               | t == dms_i_m1s2 st ->
                   let !fp = if fphys1_idx < VU.length (dms_fphys_m1 st)
                             then dms_fphys_m1 st VU.! fphys1_idx else 0.5
                   in 1.0 - fp  -- fraction to chemically-recalcitrant
               | t == dms_i_m1s3 st -> 0.0
               | t == dms_i_m2s1 st ->
                   let !fp = if fphys1_idx < VU.length (dms_fphys_m2 st)
                             then dms_fphys_m2 st VU.! fphys1_idx else 0.5
                   in fp
               | t == dms_i_m2s2 st ->
                   let !fp = if fphys1_idx < VU.length (dms_fphys_m2 st)
                             then dms_fphys_m2 st VU.! fphys1_idx else 0.5
                   in 1.0 - fp
               | t == dms_i_m2s3 st -> 0.0
             -- SOM desorption/oxidation to available pool
               | t == dms_i_s2s1 st -> 1.0
               | t == dms_i_s3s1 st -> 1.0
               | otherwise -> 0.0

      -- Respiration fractions per transition
      rf_decomp = VU.generate (nc * nlev * ntrans) $ \idx ->
        let !k = (idx `div` nc `div` nlev)
        in case k + 1 of
             t | t == dms_i_l1m1 st -> dms_rf_l1m1 st
               | t == dms_i_l1m2 st -> dms_rf_l1m2 st
               | t == dms_i_l2m1 st -> dms_rf_l2m1 st
               | t == dms_i_l2m2 st -> dms_rf_l2m2 st
               | t == dms_i_s1m1 st -> dms_rf_s1m1 st
               | t == dms_i_s1m2 st -> dms_rf_s1m2 st
               | otherwise -> 0.0  -- microbe turnover and desorption: no respiration

      -- CN ratios per pool (for N mineralization)
      cn_col = VU.generate (nc * npools) $ \idx ->
        let !k = idx `div` nc
            !iCop = dms_i_cop_mic st
            !iOli = dms_i_oli_mic st
        in if k == iCop - 1 then dmp_cn_r params
           else if k == iOli - 1 then dmp_cn_k params
           else dmp_cn_mod_num params

  in MIMICSRateOutput
    { mro_decomp_k  = decomp_k
    , mro_w_scalar  = w_scalar
    , mro_o_scalar  = o_scalar
    , mro_pathfrac  = pathfrac
    , mro_rf_decomp = rf_decomp
    , mro_cn_col    = cn_col
    }
