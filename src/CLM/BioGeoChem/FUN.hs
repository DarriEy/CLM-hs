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
    -- * Main FUN driver
  , FUNDriverInput(..)
  , FUNDriverOutput(..)
  , funDriver
    -- * Layer-level uptake
  , FUNLayerInput(..)
  , funLayerUptake
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

-- =========================================================================
-- Layer-level N uptake
-- =========================================================================

data FUNLayerInput = FUNLayerInput
  { fli_sminn_nh4      :: !Double  -- ^ ammonium N in layer (gN/m3)
  , fli_sminn_no3      :: !Double  -- ^ nitrate N in layer (gN/m3)
  , fli_rootfr         :: !Double  -- ^ root fraction in layer
  , fli_rootc_dens     :: !Double  -- ^ root C density (gC/m3)
  , fli_tc_soisno      :: !Double  -- ^ soil temperature (C)
  , fli_dz             :: !Double  -- ^ layer thickness (m)
  , fli_is_fixer       :: !Bool
  -- PFT parameters
  , fli_a_fix          :: !Double
  , fli_b_fix          :: !Double
  , fli_c_fix          :: !Double
  , fli_s_fix          :: !Double
  , fli_akc_active     :: !Double
  , fli_akn_active     :: !Double
  , fli_kc_nonmyc      :: !Double
  , fli_kn_nonmyc      :: !Double
  , fli_fracfixers     :: !Double
  } deriving (Show)

-- | Compute per-layer N uptake costs for all pathways.
-- Returns (cost_fix, cost_active_no3, cost_active_nh4, cost_nonmyc_no3, cost_nonmyc_nh4)
funLayerUptake :: FUNLayerInput -> (Double, Double, Double, Double, Double)
funLayerUptake !inp =
  let !sv = 1.0e-20
      !rootfr = fli_rootfr inp
      !rootc = fli_rootc_dens inp
      !tc = fli_tc_soisno inp
      !nh4 = fli_sminn_nh4 inp
      !no3 = fli_sminn_no3 inp

      -- Fixation cost
      !fixerFlag = if fli_is_fixer inp && fli_fracfixers inp > 0.0 then 1 else 0
      !costFix = funCostFix fixerFlag (fli_a_fix inp) (fli_b_fix inp)
                   (fli_c_fix inp) rootfr (fli_s_fix inp) tc

      -- Active uptake costs (mycorrhizal)
      !costActiveNO3 = funCostActive no3 (fli_akc_active inp) (fli_akn_active inp)
                         rootc rootfr sv
      !costActiveNH4 = funCostActive nh4 (fli_akc_active inp) (fli_akn_active inp)
                         rootc rootfr sv

      -- Non-mycorrhizal uptake costs
      !costNonmycNO3 = funCostNonmyc no3 (fli_kc_nonmyc inp) (fli_kn_nonmyc inp)
                         rootc rootfr sv
      !costNonmycNH4 = funCostNonmyc nh4 (fli_kc_nonmyc inp) (fli_kn_nonmyc inp)
                         rootc rootfr sv

  in (costFix, costActiveNO3, costActiveNH4, costNonmycNO3, costNonmycNH4)

-- =========================================================================
-- Main FUN Driver
-- =========================================================================

data FUNDriverInput = FUNDriverInput
  { fdi_ndemand          :: !Double  -- ^ total plant N demand (gN/m2/s)
  , fdi_npp_available    :: !Double  -- ^ NPP available for N acquisition (gC/m2/s * dt)
  , fdi_grperc           :: !Double  -- ^ growth respiration fraction
  , fdi_plantCN          :: !Double  -- ^ whole-plant C:N ratio
  , fdi_target_leafcn    :: !Double  -- ^ target leaf C:N
  , fdi_dt               :: !Double  -- ^ timestep (s)
  -- Layer data
  , fdi_nlayers          :: !Int
  , fdi_layer_inputs     :: ![FUNLayerInput]
  -- Retranslocation input
  , fdi_falling_leaf_c   :: !Double
  , fdi_falling_leaf_n   :: !Double
  } deriving (Show)

data FUNDriverOutput = FUNDriverOutput
  { fdo_sminn_to_plant   :: !Double  -- ^ total mineral N uptake (gN/m2/s)
  , fdo_npp_nuptake      :: !Double  -- ^ NPP spent on N uptake (gC/m2/s)
  , fdo_n_fix            :: !Double  -- ^ N from fixation (gN/m2/s)
  , fdo_n_retrans        :: !Double  -- ^ N from retranslocation (gN/m2/s)
  , fdo_n_active         :: !Double  -- ^ N from active uptake (gN/m2/s)
  , fdo_n_nonmyc         :: !Double  -- ^ N from non-mycorrhizal (gN/m2/s)
  , fdo_cost_fix         :: !Double  -- ^ average cost of fixation (gC/gN)
  , fdo_cost_active      :: !Double  -- ^ average cost of active uptake
  , fdo_cost_retrans     :: !Double  -- ^ average cost of retranslocation
  } deriving (Show)

-- | Main FUN driver: optimize N acquisition across pathways.
-- Strategy: acquire N from cheapest source first until demand met or NPP exhausted.
funDriver :: FUNDriverInput -> FUNDriverOutput
funDriver !inp =
  let !dt = fdi_dt inp
      !ndemand = fdi_ndemand inp * dt
      !npp_avail = fdi_npp_available inp

      -- Step 1: Retranslocation (cheapest, essentially free N)
      !retransInp = RetranslocationInput
        { ri_npp_to_spend = npp_avail
        , ri_total_falling_leaf_c = fdi_falling_leaf_c inp
        , ri_total_falling_leaf_n = fdi_falling_leaf_n inp
        , ri_total_n_resistance = bigCost
        , ri_target_leafcn = fdi_target_leafcn inp
        , ri_grperc = fdi_grperc inp
        , ri_plantCN = fdi_plantCN inp
        }
      !retransOut = funRetranslocation retransInp
      !n_retrans = ro_free_n retransOut + ro_paid_for_n retransOut
      !npp_after_retrans = npp_avail - ro_total_c_spent retransOut - ro_total_c_accounted retransOut
      !demand_remaining1 = max 0.0 (ndemand - n_retrans)

      -- Step 2: Compute costs for each layer and pathway
      !layerCosts = map funLayerUptake (fdi_layer_inputs inp)

      -- Find minimum cost across layers for each pathway
      !minCostFix = minimum $ bigCost : map (\(c,_,_,_,_) -> c) layerCosts
      !minCostActiveNO3 = minimum $ bigCost : map (\(_,c,_,_,_) -> c) layerCosts
      !minCostActiveNH4 = minimum $ bigCost : map (\(_,_,c,_,_) -> c) layerCosts
      !minCostNonmycNO3 = minimum $ bigCost : map (\(_,_,_,c,_) -> c) layerCosts
      !minCostNonmycNH4 = minimum $ bigCost : map (\(_,_,_,_,c) -> c) layerCosts

      -- Step 3: Acquire from cheapest pathway first
      -- Sort pathways by cost and fill demand
      !pathways = filter (\(_,c) -> c < bigCost * 0.9)
                    [ (1 :: Int, minCostFix)
                    , (2, minCostActiveNH4)
                    , (3, minCostActiveNO3)
                    , (4, minCostNonmycNH4)
                    , (5, minCostNonmycNO3)
                    ]

      -- Greedy acquisition: cheapest first
      acquire [] _ _ nfix nact nnonm cspent = (nfix, nact, nnonm, cspent)
      acquire _ 0.0 _ nfix nact nnonm cspent = (nfix, nact, nnonm, cspent)
      acquire _ _ 0.0 nfix nact nnonm cspent = (nfix, nact, nnonm, cspent)
      acquire ((pid, cost):rest) demRem nppRem nfix nact nnonm cspent =
        let !max_n_from_npp = if cost > 0.0 then nppRem / cost else 0.0
            !n_acquired = min demRem max_n_from_npp
            !c_cost = n_acquired * cost
        in case pid of
             1 -> acquire rest (demRem - n_acquired) (nppRem - c_cost)
                    (nfix + n_acquired) nact nnonm (cspent + c_cost)
             p | p == 2 || p == 3 ->
                  acquire rest (demRem - n_acquired) (nppRem - c_cost)
                    nfix (nact + n_acquired) nnonm (cspent + c_cost)
             _ -> acquire rest (demRem - n_acquired) (nppRem - c_cost)
                    nfix nact (nnonm + n_acquired) (cspent + c_cost)

      -- Sort by cost (simple insertion sort for small list)
      !sortedPaths = sortByCost pathways
      (!nFix, !nActive, !nNonmyc, !cSpent) =
        acquire sortedPaths demand_remaining1 (max 0.0 npp_after_retrans) 0.0 0.0 0.0 0.0

      !totalN = n_retrans + nFix + nActive + nNonmyc
      !totalC = ro_total_c_spent retransOut + cSpent

  in FUNDriverOutput
     { fdo_sminn_to_plant = totalN / dt
     , fdo_npp_nuptake = totalC / dt
     , fdo_n_fix = nFix / dt
     , fdo_n_retrans = n_retrans / dt
     , fdo_n_active = nActive / dt
     , fdo_n_nonmyc = nNonmyc / dt
     , fdo_cost_fix = minCostFix
     , fdo_cost_active = min minCostActiveNO3 minCostActiveNH4
     , fdo_cost_retrans = if n_retrans > 0.0
                          then ro_total_c_spent retransOut / n_retrans
                          else 0.0
     }

-- | Sort pathway list by cost (ascending).
sortByCost :: [(Int, Double)] -> [(Int, Double)]
sortByCost [] = []
sortByCost [x] = [x]
sortByCost (x:xs) =
  let smaller = filter (\(_,c) -> c <= snd x) xs
      larger  = filter (\(_,c) -> c > snd x) xs
  in sortByCost smaller ++ [x] ++ sortByCost larger
