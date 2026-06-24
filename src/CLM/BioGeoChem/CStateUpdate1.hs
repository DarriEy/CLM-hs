{-# LANGUAGE BangPatterns #-}
-- | Carbon state variable update, non-mortality fluxes.
-- Fortran: CNCStateUpdate1Mod.F90
-- Julia:   src/biogeochem/c_state_update1.jl
--
-- Pure functions for prognostic carbon state updates from photosynthesis,
-- phenology, allocation, growth respiration, and storage-to-transfer fluxes.
module CLM.BioGeoChem.CStateUpdate1
  ( -- * Input / Output types
    CStateUpdateDynPatchInput(..)
  , CStateUpdateDynPatchOutput(..)
  , CStateUpdate0Input(..)
  , CStateUpdate0Output(..)
  , CStateUpdate1Config(..)
  , CStateUpdate1ColInput(..)
  , CStateUpdate1ColOutput(..)
  , CStateUpdate1PatchInput(..)
  , CStateUpdate1PatchOutput(..)
    -- * Functions
  , cStateUpdateDynPatch
  , cStateUpdate0
  , cStateUpdate1Col
  , cStateUpdate1Patch
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Dynamic patch carbon state update
-- =========================================================================

-- | Input for dynamic patch C state update (single column, single level).
data CStateUpdateDynPatchInput = CStateUpdateDynPatchInput
  { csudp_decomp_cpools_vr   :: !Double  -- ^ current decomp C pool value
  , csudp_dwt_frootc_to_litr :: !Double  -- ^ flux: froot C to litter C
  , csudp_dwt_livecrootc     :: !Double  -- ^ flux: livecroot C to CWD C
  , csudp_dwt_deadcrootc     :: !Double  -- ^ flux: deadcroot C to CWD C
  , csudp_dt                 :: !Double  -- ^ timestep [s]
  } deriving (Show)

data CStateUpdateDynPatchOutput = CStateUpdateDynPatchOutput
  { csudpo_decomp_cpools_vr :: !Double  -- ^ updated decomp C pool value
  } deriving (Show)

-- | Update decomp C pools from dynamic land cover fluxes (single pool, single layer).
cStateUpdateDynPatch :: CStateUpdateDynPatchInput -> CStateUpdateDynPatchOutput
cStateUpdateDynPatch inp =
  let !dt = csudp_dt inp
      !val = csudp_decomp_cpools_vr inp
             + csudp_dwt_frootc_to_litr inp * dt
             + (csudp_dwt_livecrootc inp + csudp_dwt_deadcrootc inp) * dt
  in CStateUpdateDynPatchOutput { csudpo_decomp_cpools_vr = val }

-- =========================================================================
-- Photosynthesis cpool update (CStateUpdate0)
-- =========================================================================

data CStateUpdate0Input = CStateUpdate0Input
  { csu0_cpool              :: !Double  -- ^ current cpool [gC/m2]
  , csu0_psnsun_to_cpool    :: !Double  -- ^ sunlit photosynthesis flux
  , csu0_psnshade_to_cpool  :: !Double  -- ^ shaded photosynthesis flux
  , csu0_dt                 :: !Double  -- ^ timestep [s]
  } deriving (Show)

data CStateUpdate0Output = CStateUpdate0Output
  { csu0o_cpool :: !Double  -- ^ updated cpool
  } deriving (Show)

-- | Update cpool from photosynthesis fluxes.
cStateUpdate0 :: CStateUpdate0Input -> CStateUpdate0Output
cStateUpdate0 inp =
  let !dt = csu0_dt inp
      !cp = csu0_cpool inp
             + csu0_psnsun_to_cpool inp * dt
             + csu0_psnshade_to_cpool inp * dt
  in CStateUpdate0Output { csu0o_cpool = cp }

-- =========================================================================
-- CStateUpdate1 configuration
-- =========================================================================

data CStateUpdate1Config = CStateUpdate1Config
  { csu1_use_matrixcn      :: !Bool
  , csu1_use_soil_matrixcn :: !Bool
  , csu1_carbon_resp_opt   :: !Int
  , csu1_dribble_crophrv   :: !Bool
  , csu1_npcropmin         :: !Int
  , csu1_nrepr             :: !Int
  , csu1_dt                :: !Double
  } deriving (Show)

-- =========================================================================
-- Column-level soil decomposition (CStateUpdate1 column loop)
-- =========================================================================

-- | Input for a single column/level decomposition source-sink update.
data CStateUpdate1ColInput = CStateUpdate1ColInput
  { csu1c_phenology_c_to_litr   :: !Double  -- ^ phenology C to litter flux
  , csu1c_decomp_hr_vr          :: !Double  -- ^ heterotrophic respiration flux
  , csu1c_decomp_ctransfer_vr   :: !Double  -- ^ C transfer flux in cascade
  , csu1c_is_donor              :: !Bool    -- ^ this pool is cascade donor
  , csu1c_is_receiver           :: !Bool    -- ^ this pool is cascade receiver
  , csu1c_receiver_val          :: !Double  -- ^ current receiver pool value
  , csu1c_current_val           :: !Double  -- ^ current donor pool source-sink
  , csu1c_dt                    :: !Double  -- ^ timestep [s]
  } deriving (Show)

data CStateUpdate1ColOutput = CStateUpdate1ColOutput
  { csu1co_sourcesink :: !Double  -- ^ updated source-sink value
  } deriving (Show)

-- | Compute decomposition source-sink for a single pool/level/transition.
cStateUpdate1Col :: CStateUpdate1ColInput -> CStateUpdate1ColOutput
cStateUpdate1Col inp =
  let !dt = csu1c_dt inp
      !ss = csu1c_current_val inp
            - (csu1c_decomp_hr_vr inp + csu1c_decomp_ctransfer_vr inp) * dt
  in CStateUpdate1ColOutput { csu1co_sourcesink = ss }

-- =========================================================================
-- Patch-level vegetation C state updates (CStateUpdate1 patch loop)
-- =========================================================================

-- | Input for single-patch vegetation C state update.
data CStateUpdate1PatchInput = CStateUpdate1PatchInput
  { -- Current state pools
    csu1p_leafc          :: !Double
  , csu1p_leafc_xfer     :: !Double
  , csu1p_frootc         :: !Double
  , csu1p_frootc_xfer    :: !Double
  , csu1p_cpool          :: !Double
  , csu1p_xsmrpool       :: !Double
  , csu1p_gresp_storage  :: !Double
  , csu1p_gresp_xfer     :: !Double
    -- Transfer fluxes
  , csu1p_leafc_xfer_to_leafc   :: !Double
  , csu1p_frootc_xfer_to_frootc :: !Double
    -- Litterfall fluxes
  , csu1p_leafc_to_litter  :: !Double
  , csu1p_frootc_to_litter :: !Double
    -- Maintenance respiration
  , csu1p_leaf_curmr     :: !Double
  , csu1p_froot_curmr    :: !Double
  , csu1p_cpool_to_xsmrpool :: !Double
  , csu1p_cpool_to_resp  :: !Double
  , csu1p_soilc_change   :: !Double
    -- Excess MR
  , csu1p_leaf_xsmr      :: !Double
  , csu1p_froot_xsmr     :: !Double
    -- Allocation
  , csu1p_cpool_to_leafc          :: !Double
  , csu1p_cpool_to_leafc_storage  :: !Double
  , csu1p_cpool_to_frootc         :: !Double
  , csu1p_cpool_to_frootc_storage :: !Double
    -- Growth respiration
  , csu1p_cpool_leaf_gr          :: !Double
  , csu1p_cpool_froot_gr         :: !Double
  , csu1p_transfer_leaf_gr       :: !Double
  , csu1p_transfer_froot_gr      :: !Double
  , csu1p_cpool_leaf_storage_gr  :: !Double
  , csu1p_cpool_froot_storage_gr :: !Double
  , csu1p_cpool_to_gresp_storage :: !Double
    -- Storage to transfer
  , csu1p_leafc_storage_to_xfer  :: !Double
  , csu1p_frootc_storage_to_xfer :: !Double
    -- Config
  , csu1p_is_woody       :: !Bool
  , csu1p_use_matrixcn   :: !Bool
  , csu1p_dt             :: !Double
  } deriving (Show)

data CStateUpdate1PatchOutput = CStateUpdate1PatchOutput
  { csu1po_leafc         :: !Double
  , csu1po_leafc_xfer    :: !Double
  , csu1po_frootc        :: !Double
  , csu1po_frootc_xfer   :: !Double
  , csu1po_cpool         :: !Double
  , csu1po_xsmrpool      :: !Double
  , csu1po_gresp_storage :: !Double
  , csu1po_gresp_xfer    :: !Double
  } deriving (Show)

-- | Apply non-mortality, non-fire C state updates for a single patch.
-- Handles non-woody and non-crop paths. Woody/crop pools follow same pattern.
cStateUpdate1Patch :: CStateUpdate1PatchInput -> CStateUpdate1PatchOutput
cStateUpdate1Patch inp =
  let !dt = csu1p_dt inp
      !useMat = csu1p_use_matrixcn inp

      -- Phenology transfer growth
      !leafc0 | not useMat = csu1p_leafc inp + csu1p_leafc_xfer_to_leafc inp * dt
              | otherwise  = csu1p_leafc inp
      !leafcX0 | not useMat = csu1p_leafc_xfer inp - csu1p_leafc_xfer_to_leafc inp * dt
               | otherwise  = csu1p_leafc_xfer inp
      !frootc0 | not useMat = csu1p_frootc inp + csu1p_frootc_xfer_to_frootc inp * dt
               | otherwise  = csu1p_frootc inp
      !frootcX0 | not useMat = csu1p_frootc_xfer inp - csu1p_frootc_xfer_to_frootc inp * dt
                | otherwise  = csu1p_frootc_xfer inp

      -- Litterfall
      !leafc1 | not useMat = leafc0 - csu1p_leafc_to_litter inp * dt
              | otherwise  = leafc0
      !frootc1 | not useMat = frootc0 - csu1p_frootc_to_litter inp * dt
               | otherwise  = frootc0

      -- Maintenance respiration from cpool
      !cp0 = csu1p_cpool inp
             - csu1p_cpool_to_xsmrpool inp * dt
             - csu1p_leaf_curmr inp * dt
             - csu1p_froot_curmr inp * dt
             - csu1p_cpool_to_resp inp * dt
             - csu1p_soilc_change inp * dt

      -- Excess MR from xsmrpool
      !xsm0 = csu1p_xsmrpool inp
              + csu1p_cpool_to_xsmrpool inp * dt
              - csu1p_leaf_xsmr inp * dt
              - csu1p_froot_xsmr inp * dt

      -- Allocation from cpool
      !cp1 = cp0
             - csu1p_cpool_to_leafc inp * dt
             - csu1p_cpool_to_leafc_storage inp * dt
             - csu1p_cpool_to_frootc inp * dt
             - csu1p_cpool_to_frootc_storage inp * dt

      !leafc2 | not useMat = leafc1 + csu1p_cpool_to_leafc inp * dt
              | otherwise  = leafc1
      !frootc2 | not useMat = frootc1 + csu1p_cpool_to_frootc inp * dt
               | otherwise  = frootc1

      -- Growth respiration
      !cp2 = cp1
             - csu1p_cpool_leaf_gr inp * dt
             - csu1p_cpool_froot_gr inp * dt
             - csu1p_cpool_leaf_storage_gr inp * dt
             - csu1p_cpool_froot_storage_gr inp * dt
             - csu1p_cpool_to_gresp_storage inp * dt

      !grespS = csu1p_gresp_storage inp + csu1p_cpool_to_gresp_storage inp * dt
      !grespX = csu1p_gresp_xfer inp
                - csu1p_transfer_leaf_gr inp * dt
                - csu1p_transfer_froot_gr inp * dt

      -- Storage to transfer
      !leafcX1 | not useMat = leafcX0 + csu1p_leafc_storage_to_xfer inp * dt
               | otherwise  = leafcX0
      !frootcX1 | not useMat = frootcX0 + csu1p_frootc_storage_to_xfer inp * dt
                | otherwise  = frootcX0

  in CStateUpdate1PatchOutput
       { csu1po_leafc         = leafc2
       , csu1po_leafc_xfer    = leafcX1
       , csu1po_frootc        = frootc2
       , csu1po_frootc_xfer   = frootcX1
       , csu1po_cpool         = cp2
       , csu1po_xsmrpool      = xsm0
       , csu1po_gresp_storage = grespS
       , csu1po_gresp_xfer    = grespX
       }
