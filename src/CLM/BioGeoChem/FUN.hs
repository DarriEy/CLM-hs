{-# LANGUAGE BangPatterns #-}
-- | FUN (Fixation and Uptake of Nitrogen) model.
-- Fortran: CNFUNMod.F90
-- Julia:   src/biogeochem/fun.jl
--
-- The FUN model developed by Fisher et al. 2010 and Brzostek et al. 2014.
-- Critical outputs: sminn_to_plant_fun and npp_Nuptake.
module CLM.BioGeoChem.FUN
  ( -- * Constants
    bigCost
  , icostFix
  , icostRetrans
  , icostActiveNO3
  , icostActiveNH4
  , icostNonmycNO3
  , icostNonmycNH4
    -- * Data types
  , FUNParams(..)
  , defaultFUNParams
  , PftConFUN(..)
    -- * Cost functions
  , funCostFix
  , funCostActive
  , funCostNonmyc
    -- * Retranslocation
  , RetranslocationInput(..)
  , RetranslocationOutput(..)
  , funRetranslocation
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Constants
-- =========================================================================

-- | Arbitrarily large cost (gC/gN).
bigCost :: Double
bigCost = 1.0e9

icostFix, icostRetrans, icostActiveNO3, icostActiveNH4, icostNonmycNO3, icostNonmycNH4 :: Int
icostFix        = 1
icostRetrans    = 2
icostActiveNO3  = 3
icostActiveNH4  = 4
icostNonmycNO3  = 5
icostNonmycNH4  = 6

-- =========================================================================
-- Data types
-- =========================================================================

data FUNParams = FUNParams
  { fp_ndays_off :: !Double  -- ^ number of days to complete leaf offset
  } deriving (Show)

defaultFUNParams :: FUNParams
defaultFUNParams = FUNParams { fp_ndays_off = 21.0 }

-- | PFT constants needed by FUN.
data PftConFUN = PftConFUN
  { pcf_leafcn          :: !(VU.Vector Double)
  , pcf_season_decid    :: !(VU.Vector Double)
  , pcf_stress_decid    :: !(VU.Vector Double)
  , pcf_a_fix           :: !(VU.Vector Double)
  , pcf_b_fix           :: !(VU.Vector Double)
  , pcf_c_fix           :: !(VU.Vector Double)
  , pcf_s_fix           :: !(VU.Vector Double)
  , pcf_akc_active      :: !(VU.Vector Double)
  , pcf_akn_active      :: !(VU.Vector Double)
  , pcf_ekc_active      :: !(VU.Vector Double)
  , pcf_ekn_active      :: !(VU.Vector Double)
  , pcf_kc_nonmyc       :: !(VU.Vector Double)
  , pcf_kn_nonmyc       :: !(VU.Vector Double)
  , pcf_perecm          :: !(VU.Vector Double)
  , pcf_grperc          :: !(VU.Vector Double)
  , pcf_fun_cn_flex_a   :: !(VU.Vector Double)
  , pcf_fun_cn_flex_b   :: !(VU.Vector Double)
  , pcf_fun_cn_flex_c   :: !(VU.Vector Double)
  , pcf_FUN_fracfixers  :: !(VU.Vector Double)
  , pcf_c3psn           :: !(VU.Vector Double)
  } deriving (Show)

-- =========================================================================
-- Cost functions (pure, GPU-friendly)
-- =========================================================================

-- | Cost of fixing N by nodules (gC/gN).
-- Returns @bigCost@ for non-fixers or layers with negligible root fraction.
funCostFix :: Int     -- ^ fixer flag (1 = fixing)
           -> Double  -- ^ a_fix
           -> Double  -- ^ b_fix
           -> Double  -- ^ c_fix
           -> Double  -- ^ crootfr (root fraction in layer)
           -> Double  -- ^ s_fix
           -> Double  -- ^ tc_soisno (soil temperature, C)
           -> Double
funCostFix fixer a_fix b_fix c_fix crootfr s_fix tc
  | fixer == 1 && crootfr > 1.0e-6 =
      (-s_fix) / (1.25 * exp (a_fix + b_fix * tc * (1.0 - 0.5 * tc / c_fix)))
  | otherwise = bigCost

-- | Cost of mycorrhizal active uptake of N (gC/gN).
funCostActive :: Double  -- ^ sminn_layer [gN/m3]
              -> Double  -- ^ kc_active
              -> Double  -- ^ kn_active
              -> Double  -- ^ rootc_dens [gC/m3]
              -> Double  -- ^ crootfr
              -> Double  -- ^ smallValue
              -> Double
funCostActive sminn kc kn rootc crootfr sv
  | rootc > 1.0e-6 && sminn > sv = kn / sminn + kc / rootc
  | otherwise = bigCost

-- | Cost of non-mycorrhizal uptake of N (gC/gN).
funCostNonmyc :: Double  -- ^ sminn_layer
              -> Double  -- ^ kc_nonmyc
              -> Double  -- ^ kn_nonmyc
              -> Double  -- ^ rootc_dens
              -> Double  -- ^ crootfr
              -> Double  -- ^ smallValue
              -> Double
funCostNonmyc sminn kc kn rootc crootfr sv
  | rootc > 1.0e-6 && sminn > sv = kn / sminn + kc / rootc
  | otherwise = bigCost

-- =========================================================================
-- Retranslocation
-- =========================================================================

data RetranslocationInput = RetranslocationInput
  { ri_npp_to_spend           :: !Double  -- ^ NPP available [gC/m2/s * dt]
  , ri_total_falling_leaf_c   :: !Double  -- ^ falling leaf C [gC/m2]
  , ri_total_falling_leaf_n   :: !Double  -- ^ falling leaf N [gN/m2]
  , ri_total_n_resistance     :: !Double  -- ^ N cost threshold [gC/gN]
  , ri_target_leafcn          :: !Double  -- ^ target leaf C:N
  , ri_grperc                 :: !Double  -- ^ growth respiration fraction
  , ri_plantCN                :: !Double  -- ^ whole-plant C:N
  } deriving (Show)

data RetranslocationOutput = RetranslocationOutput
  { ro_total_c_spent      :: !Double  -- ^ total C spent on retranslocation
  , ro_total_c_accounted  :: !Double  -- ^ C accounted for by N uptake
  , ro_free_n             :: !Double  -- ^ free N from retranslocation
  , ro_paid_for_n         :: !Double  -- ^ N obtained by spending C
  } deriving (Show)

-- | Calculate retranslocation N and C costs.
funRetranslocation :: RetranslocationInput -> RetranslocationOutput
funRetranslocation inp =
  let !min_cn = ri_target_leafcn inp * 1.5
      !max_cn = ri_target_leafcn inp * 3.0
      !free_n = max 0.0 (ri_total_falling_leaf_n inp
                        - (ri_total_falling_leaf_c inp / min_cn))
      !leaf_n0 = ri_total_falling_leaf_n inp - free_n
      !leaf_c  = ri_total_falling_leaf_c inp
      !kresorb = 1.0 / ri_target_leafcn inp
      go !tc_spent !tc_acc !paid_n !npp_left !leaf_n !iter
        | iter >= 150 = (tc_spent, tc_acc, paid_n)
        | leaf_n <= 0.0 = (tc_spent, tc_acc, paid_n)
        | npp_left <= 0.0 = (tc_spent + npp_left, tc_acc, paid_n)
        | otherwise =
            let !cn = leaf_c / leaf_n
                !cost = kresorb / ((1.0 / cn) ** 1.3)
            in if cost >= ri_total_n_resistance inp || cn >= max_cn
               then (tc_spent, tc_acc, paid_n)
               else
                 let !c_spent = min npp_left (cost * (leaf_n - leaf_c / (cn + 1.0)))
                     !n_ext = min leaf_n (c_spent / cost)
                     !c_acc = n_ext * ri_plantCN inp * (1.0 + ri_grperc inp)
                     !leaf_n' = leaf_n - n_ext
                     !npp' = npp_left - c_spent - c_acc
                 in go (tc_spent + c_spent) (tc_acc + c_acc) (paid_n + n_ext) npp' leaf_n' (iter + 1)
      (!cs, !ca, !pn) = go 0.0 0.0 0.0 (ri_npp_to_spend inp) leaf_n0 (0 :: Int)
  in RetranslocationOutput
    { ro_total_c_spent = cs
    , ro_total_c_accounted = ca
    , ro_free_n = free_n
    , ro_paid_for_n = pn
    }
