{-# LANGUAGE BangPatterns #-}
-- | Compute seed amounts for new patch areas.
--
-- When patches increase in area, seed amounts are needed to initialize
-- the carbon/nitrogen pools in the new area. This module computes those
-- seed amounts for leaf (split into leaf itself, leaf storage, and leaf
-- transfer) and deadstem, adjusting for C/N species (C12, C13, C14, N).
--
-- Ported from: CNVegComputeSeedMod.F90
-- Julia:       src/biogeochem/veg_compute_seed.jl
--
-- Public functions:
--   speciesTypeMultiplier -- Convert gC to appropriate species amount
--   leafProportions      -- Compute leaf/storage/xfer proportions
--   computeSeedAmounts   -- Compute seed amounts for a single patch
--
module CLM.BioGeoChem.VegComputeSeed
  ( -- * Constants
    componentLeaf
  , componentDeadwood
  , cnSpeciesC12
  , cnSpeciesC13
  , cnSpeciesC14
  , cnSpeciesN
    -- * Functions
  , speciesTypeMultiplier
  , leafProportions
  , LeafProportionResult(..)
  , SeedAmountsResult(..)
  , computeSeedAmounts
  ) where

-- ========================================================================
-- Constants
-- ========================================================================

-- | Plant component identifiers
componentLeaf :: Int
componentLeaf = 1

componentDeadwood :: Int
componentDeadwood = 2

-- | CN species identifiers (from CNSpeciesMod.F90)
cnSpeciesC12 :: Int
cnSpeciesC12 = 1

cnSpeciesC13 :: Int
cnSpeciesC13 = 2

cnSpeciesC14 :: Int
cnSpeciesC14 = 3

cnSpeciesN :: Int
cnSpeciesN = 4

-- | Isotope ratio constants (matching CLM Fortran)
c3R2 :: Double
c3R2 = 0.010815   -- C3 C13/C12 ratio

c4R2 :: Double
c4R2 = 0.011035   -- C4 C13/C12 ratio

c14ratio :: Double
c14ratio = 1.0e-12 -- Pre-bomb C14/C ratio

-- ========================================================================
-- Functions
-- ========================================================================

-- | Convert a seed amount expressed in gC/m2 into the appropriate value
-- for the given C/N species.
--
-- Arguments:
--   species  - CN_SPECIES_C12, C13, C14, or N
--   c3psn    - PFT c3psn flag (1.0 for C3 plants)
--   leafcn   - PFT leaf C:N ratio
--   deadwdcn - PFT dead wood C:N ratio
--   component - COMPONENT_LEAF or COMPONENT_DEADWOOD
--   woody    - PFT woody flag (1.0 for woody)
speciesTypeMultiplier
  :: Int     -- species
  -> Double  -- c3psn
  -> Double  -- leafcn
  -> Double  -- deadwdcn
  -> Int     -- component
  -> Double
speciesTypeMultiplier !species !c3psn !leafcn !deadwdcn !component
  | species == cnSpeciesC12 = 1.0
  | species == cnSpeciesC13 = if c3psn == 1.0 then c3R2 else c4R2
  | species == cnSpeciesC14 = c14ratio
  | species == cnSpeciesN   =
      case component of
        1 -> 1.0 / leafcn     -- COMPONENT_LEAF
        2 -> 1.0 / deadwdcn   -- COMPONENT_DEADWOOD
        _ -> error $ "speciesTypeMultiplier: unknown component: " ++ show component
  | otherwise = error $ "speciesTypeMultiplier: unknown species: " ++ show species

-- | Result of leaf proportion computation
data LeafProportionResult = LeafProportionResult
  { lpr_pleaf    :: !Double
  , lpr_pstorage :: !Double
  , lpr_pxfer    :: !Double
  } deriving (Show, Eq)

-- | Compute proportions of total leaf pool allocated to leaf itself,
-- storage, and transfer. Pure function.
--
-- If ignore_current_state is True, or total leaf mass is zero, use
-- default proportions:
--   evergreen PFTs: all in leaf (pleaf = 1)
--   deciduous PFTs: all in storage (pstorage = 1)
leafProportions
  :: Bool    -- ignore_current_state
  -> Double  -- evergreen (PFT parameter, 1.0 for evergreen)
  -> Double  -- leaf
  -> Double  -- leaf_storage
  -> Double  -- leaf_xfer
  -> LeafProportionResult
leafProportions !ignoreState !evergreen !leaf !leafStorage !leafXfer =
  let tot = leaf + leafStorage + leafXfer
  in if tot == 0.0 || ignoreState
     then if evergreen == 1.0
          then LeafProportionResult 1.0 0.0 0.0
          else LeafProportionResult 0.0 1.0 0.0
     else LeafProportionResult (leaf / tot) (leafStorage / tot) (leafXfer / tot)

-- | Result of seed amount computation for a single patch
data SeedAmountsResult = SeedAmountsResult
  { sar_seed_leaf         :: !Double
  , sar_seed_leaf_storage :: !Double
  , sar_seed_leaf_xfer    :: !Double
  , sar_seed_deadstem     :: !Double
  } deriving (Show, Eq)

-- | Compute seed amounts for a single patch that increases in area.
-- Pure function.
--
-- Arguments:
--   species             - CN species identifier
--   pft_type            - Fortran 0-based PFT index
--   leafc_seed          - seed amount for leaf C [gC/m2]
--   deadstemc_seed      - seed amount for deadstem C [gC/m2]
--   leaf                - current leaf C or N [g/m2]
--   leaf_storage        - current leaf storage [g/m2]
--   leaf_xfer           - current leaf transfer [g/m2]
--   ignore_current_state - use default proportions if True
--   noveg_val           - bare ground PFT index (0-based)
--   c3psn, leafcn, deadwdcn, woody, evergreen - PFT parameters (1-based)
computeSeedAmounts
  :: Int     -- species
  -> Int     -- pft_type
  -> Double  -- leafc_seed
  -> Double  -- deadstemc_seed
  -> Double  -- leaf
  -> Double  -- leaf_storage
  -> Double  -- leaf_xfer
  -> Bool    -- ignore_current_state
  -> Int     -- noveg_val
  -> Double  -- c3psn
  -> Double  -- leafcn
  -> Double  -- deadwdcn
  -> Double  -- woody
  -> Double  -- evergreen
  -> SeedAmountsResult
computeSeedAmounts !species !pftType !leafcSeed !deadStemcSeed
  !leaf !leafStorage !leafXfer !ignoreState !novegVal
  !c3psn !leafcn !deadwdcn !woody !evergreen =
  let
    LeafProportionResult pleaf pstor pxfer =
      leafProportions ignoreState evergreen leaf leafStorage leafXfer

    myLeafSeed = if pftType /= novegVal
                 then leafcSeed * speciesTypeMultiplier species c3psn leafcn deadwdcn componentLeaf
                 else 0.0

    myDeadStemSeed = if pftType /= novegVal && woody == 1.0
                     then deadStemcSeed * speciesTypeMultiplier species c3psn leafcn deadwdcn componentDeadwood
                     else 0.0
  in SeedAmountsResult
       { sar_seed_leaf         = myLeafSeed * pleaf
       , sar_seed_leaf_storage = myLeafSeed * pstor
       , sar_seed_leaf_xfer    = myLeafSeed * pxfer
       , sar_seed_deadstem     = myDeadStemSeed
       }
