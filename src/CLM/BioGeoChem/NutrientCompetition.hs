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
