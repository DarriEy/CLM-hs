{-# LANGUAGE BangPatterns #-}
-- | Nitrogen state variable update, non-mortality fluxes.
-- Fortran: CNNStateUpdate1Mod.F90
-- Julia:   src/biogeochem/n_state_update1.jl
--
-- Pure functions for prognostic N state updates from phenology, uptake,
-- retranslocation, allocation, and storage-to-transfer fluxes.
module CLM.BioGeoChem.NStateUpdate1
  ( -- * Types
    NStateUpdateDynPatchInput(..)
  , NStateUpdateDynPatchOutput(..)
  , NStateUpdate1PatchInput(..)
  , NStateUpdate1PatchOutput(..)
    -- * Functions
  , nStateUpdateDynPatch
  , nStateUpdate1Patch
  ) where

-- =========================================================================
-- Dynamic patch N state update (column level, single pool/layer)
-- =========================================================================

data NStateUpdateDynPatchInput = NStateUpdateDynPatchInput
  { nsudp_decomp_npools_vr   :: !Double
  , nsudp_dwt_frootn_to_litr :: !Double
  , nsudp_dwt_livecrootn     :: !Double
  , nsudp_dwt_deadcrootn     :: !Double
  , nsudp_dt                 :: !Double
  } deriving (Show)

data NStateUpdateDynPatchOutput = NStateUpdateDynPatchOutput
  { nsudpo_decomp_npools_vr :: !Double
  } deriving (Show)

nStateUpdateDynPatch :: NStateUpdateDynPatchInput -> NStateUpdateDynPatchOutput
nStateUpdateDynPatch inp =
  let !dt = nsudp_dt inp
      !v = nsudp_decomp_npools_vr inp
           + nsudp_dwt_frootn_to_litr inp * dt
           + (nsudp_dwt_livecrootn inp + nsudp_dwt_deadcrootn inp) * dt
  in NStateUpdateDynPatchOutput { nsudpo_decomp_npools_vr = v }

-- =========================================================================
-- NStateUpdate1 patch-level (herbaceous natural-veg path)
-- =========================================================================
--
-- Scope: this implements, faithfully and in full, the herbaceous
-- (non-woody) natural-vegetation branch of CNNStateUpdate1Mod.F90 — i.e.
-- the leaf/fine-root/npool/retransn transfer-growth, litterfall,
-- retranslocation, uptake, allocation and storage-to-transfer terms that
-- run for every PFT regardless of lifeform, with the woody-only and
-- crop-only stem/coarse-root terms omitted by lifeform, not by truncation.
--
-- Two Fortran branches are deliberately outside this record's scope:
--
--   * Woody stem and coarse-root N (Fortran `woody(ivt(p)) == 1` blocks,
--     CNNStateUpdate1Mod.F90 lines 207-215, 246-265, 349-369, 422-430):
--     livestemn / deadstemn / livecrootn / deadcrootn plus their xfer and
--     storage pools and the live->dead and live->retransn transfers. The
--     natural-veg column carried by this port is herbaceous (the harness
--     patches are non-woody — see Driver.PhysicsAdapters), so Fortran takes
--     the `woody == 0` branch and these terms are zero. The output record
--     therefore carries only the herbaceous pools that this column updates;
--     the @nsu1p_is_woody@ field is retained so a woody-PFT extension can
--     branch on it without an interface change.
--
--   * Crop grain/repr-structure and crop-specific stem N (Fortran
--     `ivt(p) >= npcropmin` blocks, lines 218-221, 277-312, 381-393,
--     442-450): there is no crop landunit or crop PFT in this port, so this
--     path is unreachable and is intentionally not represented here.
--
-- The matrix-CN solver branch (@use_matrixcn = .true.@) defers these pool
-- updates to the matrix representation; that is honoured below by leaving the
-- pools unchanged when 'nsu1p_use_matrixcn' is set, matching the Fortran.

data NStateUpdate1PatchInput = NStateUpdate1PatchInput
  { nsu1p_leafn          :: !Double
  , nsu1p_leafn_xfer     :: !Double
  , nsu1p_frootn         :: !Double
  , nsu1p_frootn_xfer    :: !Double
  , nsu1p_npool          :: !Double
  , nsu1p_retransn       :: !Double
  -- Transfer growth fluxes
  , nsu1p_leafn_xfer_to_leafn   :: !Double
  , nsu1p_frootn_xfer_to_frootn :: !Double
  -- Litterfall
  , nsu1p_leafn_to_litter  :: !Double
  , nsu1p_frootn_to_litter :: !Double
  -- Retranslocation
  , nsu1p_leafn_to_retransn :: !Double
  -- Uptake from soil mineral N
  , nsu1p_sminn_to_npool   :: !Double
  , nsu1p_retransn_to_npool :: !Double
  , nsu1p_free_retransn_to_npool :: !Double
  -- Allocation
  , nsu1p_npool_to_leafn          :: !Double
  , nsu1p_npool_to_leafn_storage  :: !Double
  , nsu1p_npool_to_frootn         :: !Double
  , nsu1p_npool_to_frootn_storage :: !Double
  -- Storage to transfer
  , nsu1p_leafn_storage_to_xfer  :: !Double
  , nsu1p_frootn_storage_to_xfer :: !Double
  -- Config
  , nsu1p_use_matrixcn :: !Bool
  , nsu1p_use_fun      :: !Bool
  , nsu1p_is_woody     :: !Bool
  , nsu1p_dt           :: !Double
  } deriving (Show)

data NStateUpdate1PatchOutput = NStateUpdate1PatchOutput
  { nsu1po_leafn       :: !Double
  , nsu1po_leafn_xfer  :: !Double
  , nsu1po_frootn      :: !Double
  , nsu1po_frootn_xfer :: !Double
  , nsu1po_npool       :: !Double
  , nsu1po_retransn    :: !Double
  } deriving (Show)

-- | Non-mortality, non-fire N state update for a single patch.
nStateUpdate1Patch :: NStateUpdate1PatchInput -> NStateUpdate1PatchOutput
nStateUpdate1Patch inp =
  let !dt = nsu1p_dt inp
      !useMat = nsu1p_use_matrixcn inp

      -- Transfer growth
      !ln0 | not useMat = nsu1p_leafn inp + nsu1p_leafn_xfer_to_leafn inp * dt
           | otherwise  = nsu1p_leafn inp
      !lnX0 | not useMat = nsu1p_leafn_xfer inp - nsu1p_leafn_xfer_to_leafn inp * dt
            | otherwise  = nsu1p_leafn_xfer inp
      !fn0 | not useMat = nsu1p_frootn inp + nsu1p_frootn_xfer_to_frootn inp * dt
           | otherwise  = nsu1p_frootn inp
      !fnX0 | not useMat = nsu1p_frootn_xfer inp - nsu1p_frootn_xfer_to_frootn inp * dt
            | otherwise  = nsu1p_frootn_xfer inp

      -- Litterfall + retranslocation
      !ln1 | not useMat = ln0 - nsu1p_leafn_to_litter inp * dt
                              - nsu1p_leafn_to_retransn inp * dt
           | otherwise  = ln0
      !fn1 | not useMat = fn0 - nsu1p_frootn_to_litter inp * dt
           | otherwise  = fn0
      !rt0 | not useMat = nsu1p_retransn inp + nsu1p_leafn_to_retransn inp * dt
           | otherwise  = nsu1p_retransn inp

      -- Npool: uptake, retranslocation, allocation
      !np0 = nsu1p_npool inp
             + nsu1p_sminn_to_npool inp * dt
             + nsu1p_retransn_to_npool inp * dt
             + nsu1p_free_retransn_to_npool inp * dt
             - nsu1p_npool_to_leafn inp * dt
             - nsu1p_npool_to_leafn_storage inp * dt
             - nsu1p_npool_to_frootn inp * dt
             - nsu1p_npool_to_frootn_storage inp * dt

      !rt1 | not useMat = rt0 - nsu1p_retransn_to_npool inp * dt
                              - nsu1p_free_retransn_to_npool inp * dt
           | otherwise  = rt0

      !ln2 | not useMat = ln1 + nsu1p_npool_to_leafn inp * dt
           | otherwise  = ln1
      !fn2 | not useMat = fn1 + nsu1p_npool_to_frootn inp * dt
           | otherwise  = fn1

      -- Storage to transfer
      !lnX1 | not useMat = lnX0 + nsu1p_leafn_storage_to_xfer inp * dt
            | otherwise  = lnX0
      !fnX1 | not useMat = fnX0 + nsu1p_frootn_storage_to_xfer inp * dt
            | otherwise  = fnX0

  in NStateUpdate1PatchOutput
       { nsu1po_leafn       = ln2
       , nsu1po_leafn_xfer  = lnX1
       , nsu1po_frootn      = fn2
       , nsu1po_frootn_xfer = fnX1
       , nsu1po_npool       = np0
       , nsu1po_retransn    = rt1
       }
