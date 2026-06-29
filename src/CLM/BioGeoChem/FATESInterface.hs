{-# LANGUAGE BangPatterns #-}
-- | FATES Interface -- CLM-FATES boundary condition types and integration bypasses.
--
-- FATES (Functionally Assembled Terrestrial Ecosystem Simulator) is a
-- cohort-based vegetation demography model that replaces CLM's big-leaf
-- approach with explicit tracking of plant cohorts within patches of
-- different disturbance ages.
--
-- This module defines all CLM<->FATES data exchange types and provides
-- documented bypass functions at every integration point.
--
-- Ported from:
--   clmfates_interfaceMod.F90      (~3,896 lines)
--   FatesInterfaceTypesMod.F90     (~821 lines)
--
-- Julia: src/biogeochem/fates_interface.jl
--
module CLM.BioGeoChem.FATESInterface
  ( -- * Configuration
    FATESConfig(..)
  , defaultFATESConfig
    -- * Boundary condition types
  , FATESBoundaryCondIn(..)
  , FATESBoundaryCondOut(..)
  , FATESParameterConstants(..)
    -- * State types
  , FATESSiteMap(..)
  , FATESState(..)
  , HLMFATESInterface(..)
    -- * Initialization
  , defaultFATESBoundaryCondIn
  , defaultFATESBoundaryCondOut
  , defaultFATESParameterConstants
  , defaultFATESSiteMap
  , defaultHLMFATESInterface
    -- * Bypass function signatures (documented bypass functions)
  , fatesInit
  , fatesToClmSunfrac
  , fatesToClmBtran
  , fatesToClmPhotosynthesis
  , fatesToClmRadiation
  , fatesToClmCanopyStructure
  , fatesToClmZ0mDispla
  , fatesToClmLitterC
  , fatesToClmLitterN
  , fatesToClmHydraulics
  , fatesToClmWoodProducts
  , fatesDynamics
  , fatesUpdateRunningMeans
  , fatesUpdateHifreqHist
  , fatesWrapSunfrac
  , fatesWrapBtran
  , fatesPrepCanopyfluxes
  , fatesWrapPhotosynthesis
  , fatesWrapAccumulatefluxes
  , fatesWrapCanopyRadiation
  , fatesWrapHydraulicsDrive
  , fatesWrapSeedDispersal
  , fatesUpdateAccVars
  , fatesColdstart
  , fatesRestart
  , fatesTransferZ0mDispla
  , fatesUpdateLitterFluxes
  , fatesWrapWoodProducts
  , fatesComputeRootSoilFlux
  , fatesSpPhenology
  ) where

import CLM.BioGeoChem.FATES.Types

-- ========================================================================
-- Initialization
-- ========================================================================

-- | Initialize the FATES interface for a given number of sites.
-- Returns an initialized HLMFATESInterface.
fatesInit
  :: FATESConfig
  -> Int              -- nsites
  -> Int              -- nlevsoil
  -> Int              -- npatches_max
  -> Int              -- nbands
  -> HLMFATESInterface
fatesInit cfg nsites _nlevsoil _npatchesMax _nbands =
  let state = FATESState
        { fs_nsites   = nsites
        , fs_bc_in    = replicate nsites defaultFATESBoundaryCondIn
        , fs_bc_out   = replicate nsites defaultFATESBoundaryCondOut
        , fs_bc_pconst = replicate nsites defaultFATESParameterConstants
        , fs_sitemap  = defaultFATESSiteMap
        }
  in HLMFATESInterface
       { hlmfi_config      = cfg
       , hlmfi_state       = state
       , hlmfi_initialized = True
       }

-- ========================================================================
-- FATES -> CLM data transfer bypasses (all return their input unchanged)
-- ========================================================================

-- | Transfer sun/shade fractions from FATES back to CLM. Bypass.
fatesToClmSunfrac :: HLMFATESInterface -> HLMFATESInterface
fatesToClmSunfrac = id

-- | Transfer BTRAN and root distribution from FATES back to CLM. Bypass.
fatesToClmBtran :: HLMFATESInterface -> HLMFATESInterface
fatesToClmBtran = id

-- | Transfer stomatal resistance from FATES back to CLM. Bypass.
fatesToClmPhotosynthesis :: HLMFATESInterface -> HLMFATESInterface
fatesToClmPhotosynthesis = id

-- | Transfer canopy radiation from FATES back to CLM. Bypass.
fatesToClmRadiation :: HLMFATESInterface -> HLMFATESInterface
fatesToClmRadiation = id

-- | Transfer canopy structure from FATES to CLM patch arrays. Bypass.
fatesToClmCanopyStructure :: HLMFATESInterface -> HLMFATESInterface
fatesToClmCanopyStructure = id

-- | Transfer roughness length and displacement height. Bypass.
fatesToClmZ0mDispla :: HLMFATESInterface -> HLMFATESInterface
fatesToClmZ0mDispla = id

-- | Transfer carbon litter fluxes from FATES to CLM soil BGC. Bypass.
fatesToClmLitterC :: HLMFATESInterface -> HLMFATESInterface
fatesToClmLitterC = id

-- | Transfer nitrogen litter fluxes from FATES to CLM soil BGC. Bypass.
fatesToClmLitterN :: HLMFATESInterface -> HLMFATESInterface
fatesToClmLitterN = id

-- | Transfer plant hydraulics outputs from FATES to CLM. Bypass.
fatesToClmHydraulics :: HLMFATESInterface -> HLMFATESInterface
fatesToClmHydraulics = id

-- | Transfer harvested wood product fluxes from FATES to CLM. Bypass.
fatesToClmWoodProducts :: HLMFATESInterface -> HLMFATESInterface
fatesToClmWoodProducts = id

-- ========================================================================
-- Core FATES dynamics bypasses
-- ========================================================================

-- | Main FATES daily dynamics driver. Bypass.
fatesDynamics :: HLMFATESInterface -> Double -> Bool -> HLMFATESInterface
fatesDynamics iface _dtime isBegDay
  | not isBegDay = iface
  | otherwise    = iface  -- When implemented: run ed_ecosystem_dynamics

-- | Update FATES running mean temperature. Bypass.
fatesUpdateRunningMeans :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesUpdateRunningMeans iface _dtime = iface

-- | Update FATES high-frequency history diagnostics. Bypass.
fatesUpdateHifreqHist :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesUpdateHifreqHist iface _dtime = iface

-- | Compute sun/shade fractions using FATES two-stream model. Bypass.
fatesWrapSunfrac :: HLMFATESInterface -> HLMFATESInterface
fatesWrapSunfrac = id

-- | Compute BTRAN using FATES root distribution. Bypass.
fatesWrapBtran :: HLMFATESInterface -> HLMFATESInterface
fatesWrapBtran = id

-- | Prepare FATES patches for photosynthesis iteration. Bypass.
fatesPrepCanopyfluxes :: HLMFATESInterface -> HLMFATESInterface
fatesPrepCanopyfluxes = id

-- | Compute photosynthesis for FATES cohorts. Bypass.
fatesWrapPhotosynthesis :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapPhotosynthesis iface _dtime = iface

-- | Accumulate FATES carbon/water fluxes. Bypass.
fatesWrapAccumulatefluxes :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapAccumulatefluxes iface _dtime = iface

-- | Compute canopy radiation using FATES two-stream model. Bypass.
fatesWrapCanopyRadiation :: HLMFATESInterface -> HLMFATESInterface
fatesWrapCanopyRadiation = id

-- | Drive FATES plant hydraulics model. Bypass.
fatesWrapHydraulicsDrive :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapHydraulicsDrive iface _dtime = iface

-- | Global seed dispersal across grid cells. Bypass.
fatesWrapSeedDispersal :: HLMFATESInterface -> HLMFATESInterface
fatesWrapSeedDispersal = id

-- | Update FATES accumulation variables. Bypass.
fatesUpdateAccVars :: HLMFATESInterface -> HLMFATESInterface
fatesUpdateAccVars = id

-- | Cold-start FATES initialization. Bypass.
fatesColdstart :: HLMFATESInterface -> HLMFATESInterface
fatesColdstart = id

-- | Read or write FATES restart data. Bypass.
fatesRestart :: HLMFATESInterface -> HLMFATESInterface
fatesRestart = id

-- | Transfer roughness length and displacement height each timestep. Bypass.
fatesTransferZ0mDispla :: HLMFATESInterface -> HLMFATESInterface
fatesTransferZ0mDispla = id

-- | Transfer FATES litter fluxes to CLM soil BGC model. Bypass.
fatesUpdateLitterFluxes :: HLMFATESInterface -> HLMFATESInterface
fatesUpdateLitterFluxes = id

-- | Transfer FATES harvest wood product fluxes. Bypass.
fatesWrapWoodProducts :: HLMFATESInterface -> HLMFATESInterface
fatesWrapWoodProducts = id

-- | Transfer FATES root-soil water flux. Bypass.
fatesComputeRootSoilFlux :: HLMFATESInterface -> HLMFATESInterface
fatesComputeRootSoilFlux = id

-- | FATES Satellite Phenology mode phenology update. Bypass.
fatesSpPhenology :: HLMFATESInterface -> HLMFATESInterface
fatesSpPhenology = id
