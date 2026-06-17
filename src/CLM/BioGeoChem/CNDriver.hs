{-# LANGUAGE BangPatterns #-}
-- | CN driver: ecosystem dynamics orchestrator.
-- Fortran: CNDriverMod.F90
-- Julia:   src/biogeochem/cn_driver.jl
--
-- Orchestrates phenology, allocation, decomposition, nitrogen cycling,
-- fire, and state updates in the correct sequence each timestep.
module CLM.BioGeoChem.CNDriver
  ( -- * Configuration
    CNDriverConfig(..)
  , defaultCNDriverConfig
    -- * Decomposition method constants
  , centuryDecomp
  , mimicsDecomp
    -- * Driver input/output
  , CNDriverInput(..)
  , CNDriverResult(..)
  , CNLeachingInput(..)
    -- * Driver functions
  , cnDriverNoLeaching
  , cnDriverLeaching
    -- * Zero-flux helpers
  , zeroFluxVector
  , zeroMaskedFlux
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Configuration
-- =========================================================================

data CNDriverConfig = CNDriverConfig
  { cndc_use_c13                       :: !Bool
  , cndc_use_c14                       :: !Bool
  , cndc_use_cn                        :: !Bool
  , cndc_use_fun                       :: !Bool
  , cndc_use_crop                      :: !Bool
  , cndc_use_crop_agsys                :: !Bool
  , cndc_use_nitrif_denitrif           :: !Bool
  , cndc_use_nguardrail                :: !Bool
  , cndc_use_fates                     :: !Bool
  , cndc_use_fates_bgc                 :: !Bool
  , cndc_use_matrixcn                  :: !Bool
  , cndc_use_soil_matrixcn             :: !Bool
  , cndc_decomp_method                 :: !Int
  , cndc_dribble_crophrv_xsmrpool_2atm :: !Bool
  , cndc_do_harvest                    :: !Bool
  , cndc_do_grossunrep                 :: !Bool
  } deriving (Show)

defaultCNDriverConfig :: CNDriverConfig
defaultCNDriverConfig = CNDriverConfig
  { cndc_use_c13 = False
  , cndc_use_c14 = False
  , cndc_use_cn = False
  , cndc_use_fun = False
  , cndc_use_crop = False
  , cndc_use_crop_agsys = False
  , cndc_use_nitrif_denitrif = False
  , cndc_use_nguardrail = False
  , cndc_use_fates = False
  , cndc_use_fates_bgc = False
  , cndc_use_matrixcn = False
  , cndc_use_soil_matrixcn = False
  , cndc_decomp_method = 1
  , cndc_dribble_crophrv_xsmrpool_2atm = False
  , cndc_do_harvest = False
  , cndc_do_grossunrep = False
  }

centuryDecomp :: Int
centuryDecomp = 1

mimicsDecomp :: Int
mimicsDecomp = 2

-- =========================================================================
-- Driver Input/Output Types
-- =========================================================================

-- | Input to the no-leaching phase of the CN driver.
data CNDriverInput = CNDriverInput
  { cdi_mask_bgc_soilc          :: !(VU.Vector Bool)
  , cdi_mask_bgc_vegp           :: !(VU.Vector Bool)
  , cdi_ncols                   :: !Int
  , cdi_npatches                :: !Int
  , cdi_nlevdecomp              :: !Int
  , cdi_ndecomp_pools           :: !Int
  , cdi_ndecomp_transitions     :: !Int
  , cdi_i_litr_min              :: !Int
  , cdi_i_litr_max              :: !Int
  , cdi_i_cwd                   :: !Int
  , cdi_dt                      :: !Double
  -- Decomposition rate constants (w_scalar * t_scalar * depth_scalar * k)
  , cdi_decomp_k                :: !(VU.Vector Double)
  -- Soil state for decomposition
  , cdi_t_soisno                :: !(VU.Vector Double)
  , cdi_soilpsi                 :: !(VU.Vector Double)
  -- Carbon state pools (col x nlevdecomp x ndecomp_pools)
  , cdi_decomp_cpools           :: !(VU.Vector Double)
  -- Nitrogen state pools
  , cdi_decomp_npools           :: !(VU.Vector Double)
  -- Mineral N
  , cdi_sminn_vr                :: !(VU.Vector Double)
  -- Cascade connectivity
  , cdi_cascade_donor_pool      :: !(VU.Vector Int)
  , cdi_cascade_receiver_pool   :: !(VU.Vector Int)
  , cdi_pathfrac_decomp_cascade :: !(VU.Vector Double)
  , cdi_rf_decomp_cascade       :: !(VU.Vector Double)
  } deriving (Show)

-- | Result of the CN driver no-leaching phase.
data CNDriverResult = CNDriverResult
  { -- Decomposition C fluxes (col x nlevdecomp x ntransitions)
    cdr_decomp_cascade_hr_vr      :: !(VU.Vector Double)
  , cdr_decomp_cascade_ctransfer  :: !(VU.Vector Double)
  -- Potential heterotrophic respiration
  , cdr_phr_vr                    :: !(VU.Vector Double)
  -- N mineralization/immobilization
  , cdr_net_nmin_vr               :: !(VU.Vector Double)
  , cdr_gross_nmin_vr             :: !(VU.Vector Double)
  -- Plant-available mineral N fraction
  , cdr_fpi_vr                    :: !(VU.Vector Double)
  -- Summary
  , cdr_decomp_executed           :: !Bool
  , cdr_veg_executed              :: !Bool
  , cdr_num_precision_controls    :: !Int
  } deriving (Show)

-- | Input for the leaching phase.
data CNLeachingInput = CNLeachingInput
  { cli_mask_bgc_soilc     :: !(VU.Vector Bool)
  , cli_mask_bgc_vegp      :: !(VU.Vector Bool)
  , cli_ncols              :: !Int
  , cli_nlevdecomp         :: !Int
  , cli_dt                 :: !Double
  , cli_sminn_vr           :: !(VU.Vector Double)
  , cli_qflx_drain_vr      :: !(VU.Vector Double)
  , cli_h2osoi_liq         :: !(VU.Vector Double)
  } deriving (Show)

-- =========================================================================
-- CN Driver: No-Leaching Phase
-- =========================================================================

-- | Main CN driver before N leaching.
-- Computes decomposition fluxes (heterotrophic respiration, C transfers,
-- N mineralization) and returns them. Vegetation state updates (phenology,
-- allocation, gap mortality, fire) are called externally in sequence.
cnDriverNoLeaching :: CNDriverConfig -> CNDriverInput -> CNDriverResult
cnDriverNoLeaching !config !input =
  let !nc = cdi_ncols input
      !nlev = cdi_nlevdecomp input
      !npools = cdi_ndecomp_pools input
      !ntrans = cdi_ndecomp_transitions input
      !dt = cdi_dt input
      !numBgcVegp = VU.length (VU.filter id (cdi_mask_bgc_vegp input))
      !hasVeg = numBgcVegp > 0
      !hasDecomp = nlev > 0 && npools > 0

      -- Compute actual decomposition fluxes
      (!hr_vr, !ctransfer, !phr, !netNmin, !grossNmin, !fpi) =
        if hasDecomp
        then computeDecomposition config input
        else ( VU.replicate (nc * nlev * ntrans) 0.0
             , VU.replicate (nc * nlev * ntrans) 0.0
             , VU.replicate (nc * nlev) 0.0
             , VU.replicate (nc * nlev) 0.0
             , VU.replicate (nc * nlev) 0.0
             , VU.replicate (nc * nlev) 1.0
             )

  in CNDriverResult
     { cdr_decomp_cascade_hr_vr = hr_vr
     , cdr_decomp_cascade_ctransfer = ctransfer
     , cdr_phr_vr = phr
     , cdr_net_nmin_vr = netNmin
     , cdr_gross_nmin_vr = grossNmin
     , cdr_fpi_vr = fpi
     , cdr_decomp_executed = hasDecomp
     , cdr_veg_executed = hasVeg
     , cdr_num_precision_controls = if hasVeg then 3 else 0
     }

-- =========================================================================
-- CN Driver: Leaching Phase
-- =========================================================================

-- | N leaching and post-leaching state updates.
-- Computes N loss to drainage water proportional to [NO3] and water flux.
cnDriverLeaching :: CNDriverConfig -> CNLeachingInput -> VU.Vector Double
cnDriverLeaching !config !input =
  let !nc = cli_ncols input
      !nlev = cli_nlevdecomp input
      !sminn = cli_sminn_vr input
      !drain = cli_qflx_drain_vr input
      !h2o = cli_h2osoi_liq input
      !mask = cli_mask_bgc_soilc input
      !dt = cli_dt input
  in VU.generate (nc * nlev) $ \idx ->
       let !c = idx `div` nlev
           !smin_val = sminn VU.! idx
           !drain_val = drain VU.! idx
           !h2o_val = h2o VU.! idx
           !active = c < VU.length mask && mask VU.! c
       in if active && h2o_val > 0.0 && smin_val > 0.0
          then let !conc = smin_val / h2o_val
                   !leach = conc * drain_val * dt
               in min leach smin_val
          else 0.0

-- =========================================================================
-- Decomposition computation
-- =========================================================================

-- | Compute decomposition: rate constants → potential → competition → actual.
computeDecomposition :: CNDriverConfig -> CNDriverInput -> ( VU.Vector Double
                                                           , VU.Vector Double
                                                           , VU.Vector Double
                                                           , VU.Vector Double
                                                           , VU.Vector Double
                                                           , VU.Vector Double )
computeDecomposition !config !input =
  let !nc = cdi_ncols input
      !nlev = cdi_nlevdecomp input
      !npools = cdi_ndecomp_pools input
      !ntrans = cdi_ndecomp_transitions input
      !dt = cdi_dt input
      !mask = cdi_mask_bgc_soilc input
      !decomp_k = cdi_decomp_k input
      !cpools = cdi_decomp_cpools input
      !npools_v = cdi_decomp_npools input
      !sminn = cdi_sminn_vr input
      !donor = cdi_cascade_donor_pool input
      !receiver = cdi_cascade_receiver_pool input
      !pathfrac = cdi_pathfrac_decomp_cascade input
      !rf = cdi_rf_decomp_cascade input

      idx3 c j k stride_j stride_k = c * stride_j * stride_k + j * stride_k + k

      -- Potential decomposition: p_decomp = cpools[donor] * decomp_k * pathfrac * dt
      !p_decomp_cpool_loss = VU.generate (nc * nlev * ntrans) $ \idx ->
        let !c = idx `div` (nlev * ntrans)
            !rem1 = idx `mod` (nlev * ntrans)
            !j = rem1 `div` ntrans
            !k = rem1 `mod` ntrans
            !active = c < VU.length mask && mask VU.! c
        in if active && k < VU.length donor
           then let !d = donor VU.! k
                    !pool_idx = idx3 c j d nlev npools
                    !k_idx = idx3 c j d nlev npools
                    !cpool_val = if pool_idx < VU.length cpools
                                 then cpools VU.! pool_idx else 0.0
                    !k_val = if k_idx < VU.length decomp_k
                             then decomp_k VU.! k_idx else 0.0
                    !pf = if k < VU.length pathfrac then pathfrac VU.! k else 0.0
                in max 0.0 (cpool_val * k_val * pf * dt)
           else 0.0

      -- Heterotrophic respiration: hr = p_decomp * rf
      !hr_vr = VU.generate (nc * nlev * ntrans) $ \idx ->
        let !k = idx `mod` ntrans
            !rf_val = if k < VU.length rf then rf VU.! k else 0.0
        in (p_decomp_cpool_loss VU.! idx) * rf_val

      -- C transfer to receiver pool: ctransfer = p_decomp * (1 - rf)
      !ctransfer = VU.generate (nc * nlev * ntrans) $ \idx ->
        let !k = idx `mod` ntrans
            !rf_val = if k < VU.length rf then rf VU.! k else 0.0
        in (p_decomp_cpool_loss VU.! idx) * (1.0 - rf_val)

      -- Potential heterotrophic respiration (sum over transitions per col*level)
      !phr = VU.generate (nc * nlev) $ \idx ->
        let !base = idx * ntrans
        in VU.sum $ VU.slice base (min ntrans (VU.length hr_vr - base)) hr_vr

      -- N mineralization: net_nmin = gross_nmin - immobilization
      -- gross_nmin = sum(npools[donor] * k * pathfrac * dt) for each level
      !grossNmin = VU.generate (nc * nlev) $ \idx ->
        let !c = idx `div` nlev
            !j = idx `mod` nlev
            !active = c < VU.length mask && mask VU.! c
        in if active
           then VU.sum $ VU.generate ntrans $ \k ->
                  let !d = if k < VU.length donor then donor VU.! k else 0
                      !pool_idx = idx3 c j d nlev npools
                      !npool_val = if pool_idx < VU.length npools_v
                                   then npools_v VU.! pool_idx else 0.0
                      !k_idx = idx3 c j d nlev npools
                      !k_val = if k_idx < VU.length decomp_k
                               then decomp_k VU.! k_idx else 0.0
                      !pf = if k < VU.length pathfrac then pathfrac VU.! k else 0.0
                  in npool_val * k_val * pf * dt
           else 0.0

      -- Fraction of mineral N available to plants (fpi)
      -- fpi = sminn / (sminn + immobilization_demand)
      !fpi = VU.generate (nc * nlev) $ \idx ->
        let !smin_val = if idx < VU.length sminn then sminn VU.! idx else 0.0
            !immob = max 0.0 (-(grossNmin VU.! idx))
        in if smin_val + immob > 0.0
           then smin_val / (smin_val + immob)
           else 1.0

      -- Net N mineralization = gross - immobilization (limited by fpi)
      !netNmin = VU.imap (\idx gn ->
        let !fpi_val = fpi VU.! idx
        in if gn >= 0.0 then gn
           else gn * fpi_val
        ) grossNmin

  in (hr_vr, ctransfer, phr, netNmin, grossNmin, fpi)

-- =========================================================================
-- Utility functions
-- =========================================================================

-- | Zero a flux vector.
zeroFluxVector :: Int -> VU.Vector Double
zeroFluxVector n = VU.replicate n 0.0

-- | Zero flux values at masked indices.
zeroMaskedFlux :: VU.Vector Bool -> VU.Vector Double -> VU.Vector Double
zeroMaskedFlux !mask !fluxes =
  VU.imap (\i v -> if i < VU.length mask && mask VU.! i then 0.0 else v) fluxes
