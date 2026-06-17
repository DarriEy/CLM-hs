{-# LANGUAGE BangPatterns #-}
-- | Plant nutrient demand and competition (CLM4.5 default method).
-- Fortran: NutrientCompetitionCLM45defaultMod.F90
-- Julia:   src/biogeochem/nutrient_competition.jl
--
-- Contains PFT-level parameters for nutrient competition and
-- pure functions for computing plant nitrogen demand and allocation.
module CLM.BioGeoChem.NutrientCompetition
  ( -- * Data types
    PftConNutrientCompetition(..)
    -- * Allocation helpers
  , DynamicStemLeaf(..)
  , calcDynamicStemLeaf
  , NAllocationInput(..)
  , NAllocationOutput(..)
  , calcNAllocation
    -- * N Competition
  , NCompetitionInput(..)
  , NCompetitionOutput(..)
  , calcNCompetition
    -- * N Downregulation
  , calcDownregulation
    -- * Flexible CN
  , FlexCNInput(..)
  , calcFlexibleCN
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- PFT constants needed by nutrient competition
-- =========================================================================

-- | PFT-level parameters used by the nutrient competition routines.
-- Ported from @pftconMod@ fields referenced in
-- @NutrientCompetitionCLM45defaultMod.F90@.
data PftConNutrientCompetition = PftConNutrientCompetition
  { pcnc_woody       :: !(VU.Vector Double)  -- ^ binary woody flag (1=woody)
  , pcnc_froot_leaf  :: !(VU.Vector Double)  -- ^ new froot C per new leaf C
  , pcnc_croot_stem  :: !(VU.Vector Double)  -- ^ new croot C per new stem C
  , pcnc_stem_leaf   :: !(VU.Vector Double)  -- ^ new stem C per new leaf C (-1 = dynamic)
  , pcnc_flivewd     :: !(VU.Vector Double)  -- ^ fraction of new wood that is live
  , pcnc_leafcn      :: !(VU.Vector Double)  -- ^ leaf C:N
  , pcnc_frootcn     :: !(VU.Vector Double)  -- ^ fine root C:N
  , pcnc_livewdcn    :: !(VU.Vector Double)  -- ^ live wood C:N
  , pcnc_deadwdcn    :: !(VU.Vector Double)  -- ^ dead wood C:N
  , pcnc_fcur        :: !(VU.Vector Double)  -- ^ fraction of allocation to current growth
  , pcnc_graincn     :: !(VU.Vector Double)  -- ^ grain C:N
  , pcnc_grperc      :: !(VU.Vector Double)  -- ^ growth respiration fraction
  , pcnc_grpnow      :: !(VU.Vector Double)  -- ^ growth respiration fraction released immediately
  } deriving (Show)

-- =========================================================================
-- Dynamic stem:leaf ratio
-- =========================================================================

data DynamicStemLeaf = DynamicStemLeaf
  { dsl_stem_leaf_flag :: !Double  -- ^ PFT stem_leaf value (-1 = dynamic)
  , dsl_annsum_npp     :: !Double  -- ^ annual NPP [gC/m2/yr]
  } deriving (Show)

-- | Compute stem:leaf allocation ratio. When stem_leaf == -1 (dynamic),
-- use the NPP-dependent logistic formula from CLM4.5/5.
calcDynamicStemLeaf :: DynamicStemLeaf -> Double
calcDynamicStemLeaf inp
  | dsl_stem_leaf_flag inp == (-1.0) =
      (2.7 / (1.0 + exp (-0.004 * (dsl_annsum_npp inp - 300.0)))) - 0.4
  | otherwise = dsl_stem_leaf_flag inp

-- =========================================================================
-- Nitrogen allocation (simplified, single patch)
-- =========================================================================

data NAllocationInput = NAllocationInput
  { nai_avail_c     :: !Double  -- ^ available C for growth [gC/m2/s]
  , nai_fpg         :: !Double  -- ^ fraction of potential growth
  , nai_leafcn      :: !Double  -- ^ leaf C:N
  , nai_frootcn     :: !Double  -- ^ fine root C:N
  , nai_livewdcn    :: !Double  -- ^ live wood C:N
  , nai_deadwdcn    :: !Double  -- ^ dead wood C:N
  , nai_froot_leaf  :: !Double  -- ^ f1: new froot / new leaf
  , nai_stem_leaf   :: !Double  -- ^ f3: new stem / new leaf
  , nai_croot_stem  :: !Double  -- ^ f2: new croot / new stem
  , nai_flivewd     :: !Double  -- ^ fraction of wood that is live
  , nai_fcur        :: !Double  -- ^ fraction to current growth
  , nai_woody       :: !Double  -- ^ woody flag (1.0 or 0.0)
  , nai_grperc      :: !Double  -- ^ growth respiration percentage
  } deriving (Show)

data NAllocationOutput = NAllocationOutput
  { nao_plant_ndemand :: !Double  -- ^ total plant N demand [gN/m2/s]
  , nao_actual_nuptake :: !Double  -- ^ actual N uptake [gN/m2/s]
  } deriving (Show)

-- | Calculate plant nitrogen demand from available C and allometric ratios.
calcNAllocation :: NAllocationInput -> NAllocationOutput
calcNAllocation inp =
  let !f1 = nai_froot_leaf inp
      !f3 = nai_stem_leaf inp
      !f2 = nai_croot_stem inp
      !f4 = nai_flivewd inp
      !g  = nai_grperc inp
      !w  = nai_woody inp

      -- C allocation per unit leaf C
      !c_allom = 1.0 + f1 + w * (f3 * (1.0 + f2))
      !gresp_allom = c_allom * g

      -- N demand per unit leaf C
      !n_allom = 1.0 / nai_leafcn inp
              + f1 / nai_frootcn inp
              + (if w > 0.5
                 then f3 * f4 / nai_livewdcn inp
                    + f3 * (1.0 - f4) / nai_deadwdcn inp
                    + f3 * f2 * f4 / nai_livewdcn inp
                    + f3 * f2 * (1.0 - f4) / nai_deadwdcn inp
                 else 0.0)

      !plant_ndemand = nai_avail_c inp / (c_allom + gresp_allom) * n_allom
      !actual = plant_ndemand * nai_fpg inp
  in NAllocationOutput
    { nao_plant_ndemand = plant_ndemand
    , nao_actual_nuptake = actual
    }

-- =========================================================================
-- Nitrogen competition (plant vs decomposer)
-- =========================================================================

data NCompetitionInput = NCompetitionInput
  { nci_plant_ndemand     :: !Double  -- ^ plant N demand (gN/m2/s)
  , nci_decomp_ndemand    :: !Double  -- ^ decomposer immobilization demand (gN/m2/s)
  , nci_sminn             :: !Double  -- ^ soil mineral N pool (gN/m2)
  , nci_dt                :: !Double  -- ^ timestep (s)
  , nci_use_nitrif_denitrif :: !Bool
  } deriving (Show)

data NCompetitionOutput = NCompetitionOutput
  { nco_fpi           :: !Double  -- ^ fraction of potential immobilization [0,1]
  , nco_fpg           :: !Double  -- ^ fraction of potential growth (N downreg) [0,1]
  , nco_actual_plant_nuptake :: !Double  -- ^ actual plant N uptake (gN/m2/s)
  , nco_actual_immob  :: !Double  -- ^ actual immobilization (gN/m2/s)
  , nco_sminn_to_plant :: !Double  -- ^ SMIN N flux to plant (gN/m2/s)
  } deriving (Show)

-- | Calculate N competition between plant uptake and decomposer immobilization.
-- Uses the relative demand approach from CLM4.5:
--   fpi = sminn_supply / (plant_demand + immob_demand)
--   fpg = sminn_supply / (plant_demand + immob_demand) [symmetric]
-- When supply exceeds total demand, both fpi and fpg = 1.
calcNCompetition :: NCompetitionInput -> NCompetitionOutput
calcNCompetition !inp =
  let !pdem = nci_plant_ndemand inp
      !idem = nci_decomp_ndemand inp
      !sminn = nci_sminn inp
      !dt = nci_dt inp
      -- Available mineral N supply (limited to pool size / dt)
      !supply = sminn / dt
      !total_demand = pdem + idem
  in if total_demand <= 0.0 || supply <= 0.0
     then NCompetitionOutput
          { nco_fpi = 1.0, nco_fpg = 1.0
          , nco_actual_plant_nuptake = 0.0
          , nco_actual_immob = 0.0
          , nco_sminn_to_plant = 0.0 }
     else if supply >= total_demand
          then NCompetitionOutput
               { nco_fpi = 1.0, nco_fpg = 1.0
               , nco_actual_plant_nuptake = pdem
               , nco_actual_immob = idem
               , nco_sminn_to_plant = pdem }
          else -- Supply-limited: partition proportionally
            let !frac = supply / total_demand
                !fpi = frac
                !fpg = frac
                !act_plant = pdem * frac
                !act_immob = idem * frac
            in NCompetitionOutput
               { nco_fpi = fpi, nco_fpg = fpg
               , nco_actual_plant_nuptake = act_plant
               , nco_actual_immob = act_immob
               , nco_sminn_to_plant = act_plant }

-- =========================================================================
-- N downregulation of GPP
-- =========================================================================

-- | Calculate N downregulation factor for GPP.
-- When N is limiting, GPP is reduced to match available N for growth.
-- downreg = actual_N_uptake / potential_N_demand
calcDownregulation :: Double  -- ^ plant_ndemand (gN/m2/s)
                   -> Double  -- ^ actual_nuptake (gN/m2/s)
                   -> Double  -- ^ fpg [0,1]
                   -> Double  -- ^ downregulation factor [0,1]
calcDownregulation !demand !_actual !fpg
  | demand <= 0.0 = 1.0
  | otherwise = min 1.0 fpg

-- =========================================================================
-- Flexible C:N ratios
-- =========================================================================

data FlexCNInput = FlexCNInput
  { fci_leafcn_target  :: !Double  -- ^ target leaf C:N
  , fci_leafcn_min     :: !Double  -- ^ minimum leaf C:N (N-saturated)
  , fci_leafcn_max     :: !Double  -- ^ maximum leaf C:N (N-starved)
  , fci_fpg            :: !Double  -- ^ fraction of potential growth
  , fci_flex_a         :: !Double  -- ^ flexibility parameter a
  , fci_flex_b         :: !Double  -- ^ flexibility parameter b
  , fci_flex_c         :: !Double  -- ^ flexibility parameter c
  } deriving (Show)

-- | Calculate flexible C:N ratio based on N availability.
-- When N is abundant (fpg→1), CN approaches minimum (N-rich tissue).
-- When N is scarce (fpg→0), CN approaches maximum (N-poor tissue).
calcFlexibleCN :: FlexCNInput -> Double
calcFlexibleCN !inp =
  let !fpg = fci_fpg inp
      !cn_min = fci_leafcn_min inp
      !cn_max = fci_leafcn_max inp
      -- Logistic interpolation between min and max CN
      !x = fci_flex_a inp + fci_flex_b inp * fpg
      !sigmoid = 1.0 / (1.0 + exp (-x))
      !cn = cn_max - (cn_max - cn_min) * sigmoid
  in max cn_min (min cn_max cn)
