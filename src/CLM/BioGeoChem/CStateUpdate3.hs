{-# LANGUAGE BangPatterns #-}
-- | Carbon state variable update, fire fluxes.
-- Fortran: CNCStateUpdate3Mod.F90
-- Julia:   src/biogeochem/c_state_update3.jl
--
-- Pure functions for fire-induced C state updates including combustion
-- emissions, mortality to litter, and decomposition pool fire losses.
module CLM.BioGeoChem.CStateUpdate3
  ( -- * Input / Output types
    CStateUpdate3PatchInput(..)
  , CStateUpdate3PatchOutput(..)
  , DecompFireInput(..)
  , DecompFireOutput(..)
    -- * Functions
  , cStateUpdate3Patch
  , decompFireLoss
  ) where

-- =========================================================================
-- Decomposition pool fire loss (column level)
-- =========================================================================

data DecompFireInput = DecompFireInput
  { dfi_pool_vr              :: !Double  -- ^ current decomp C pool [gC/m3]
  , dfi_fire_loss_vr         :: !Double  -- ^ fire loss flux
  , dfi_fire_mort_to_cwdc    :: !Double  -- ^ fire mortality C to CWD
  , dfi_fire_c_to_litr       :: !Double  -- ^ fire C to litter
  , dfi_dt                   :: !Double
  } deriving (Show)

data DecompFireOutput = DecompFireOutput
  { dfo_pool_vr :: !Double  -- ^ updated decomp C pool
  } deriving (Show)

-- | Update a decomposition pool for fire losses and fire mortality inputs.
decompFireLoss :: DecompFireInput -> DecompFireOutput
decompFireLoss inp =
  let !dt = dfi_dt inp
      !v = dfi_pool_vr inp
           + dfi_fire_mort_to_cwdc inp * dt
           + dfi_fire_c_to_litr inp * dt
           - dfi_fire_loss_vr inp * dt
  in DecompFireOutput { dfo_pool_vr = v }

-- =========================================================================
-- Patch-level fire C state updates
-- =========================================================================

data CStateUpdate3PatchInput = CStateUpdate3PatchInput
  { csu3p_leafc         :: !Double
  , csu3p_frootc        :: !Double
  , csu3p_livestemc     :: !Double
  , csu3p_deadstemc     :: !Double
  , csu3p_livecrootc    :: !Double
  , csu3p_deadcrootc    :: !Double
  , csu3p_gresp_storage :: !Double
  , csu3p_gresp_xfer    :: !Double
  -- Fire emission fluxes
  , csu3p_m_leafc_to_fire          :: !Double
  , csu3p_m_leafc_to_litter_fire   :: !Double
  , csu3p_m_frootc_to_fire         :: !Double
  , csu3p_m_frootc_to_litter_fire  :: !Double
  , csu3p_m_livestemc_to_fire              :: !Double
  , csu3p_m_livestemc_to_litter_fire       :: !Double
  , csu3p_m_livestemc_to_deadstemc_fire    :: !Double
  , csu3p_m_deadstemc_to_fire              :: !Double
  , csu3p_m_deadstemc_to_litter_fire       :: !Double
  , csu3p_m_livecrootc_to_fire             :: !Double
  , csu3p_m_livecrootc_to_litter_fire      :: !Double
  , csu3p_m_livecrootc_to_deadcrootc_fire  :: !Double
  , csu3p_m_deadcrootc_to_fire             :: !Double
  , csu3p_m_deadcrootc_to_litter_fire      :: !Double
  -- Gresp fire fluxes
  , csu3p_m_gresp_storage_to_fire          :: !Double
  , csu3p_m_gresp_storage_to_litter_fire   :: !Double
  , csu3p_m_gresp_xfer_to_fire             :: !Double
  , csu3p_m_gresp_xfer_to_litter_fire      :: !Double
  -- Storage/xfer fire fluxes (simplified: sum of fire + litter_fire for each)
  , csu3p_m_leafc_storage_to_fire          :: !Double
  , csu3p_m_leafc_storage_to_litter_fire   :: !Double
  , csu3p_m_leafc_xfer_to_fire             :: !Double
  , csu3p_m_leafc_xfer_to_litter_fire      :: !Double
  , csu3p_m_frootc_storage_to_fire         :: !Double
  , csu3p_m_frootc_storage_to_litter_fire  :: !Double
  , csu3p_m_frootc_xfer_to_fire            :: !Double
  , csu3p_m_frootc_xfer_to_litter_fire     :: !Double
  -- Config
  , csu3p_use_matrixcn :: !Bool
  , csu3p_dt           :: !Double
  } deriving (Show)

data CStateUpdate3PatchOutput = CStateUpdate3PatchOutput
  { csu3po_leafc         :: !Double
  , csu3po_frootc        :: !Double
  , csu3po_livestemc     :: !Double
  , csu3po_deadstemc     :: !Double
  , csu3po_livecrootc    :: !Double
  , csu3po_deadcrootc    :: !Double
  , csu3po_gresp_storage :: !Double
  , csu3po_gresp_xfer    :: !Double
  } deriving (Show)

-- | Fire C state update for a single patch (non-matrix path).
cStateUpdate3Patch :: CStateUpdate3PatchInput -> CStateUpdate3PatchOutput
cStateUpdate3Patch inp =
  let !dt = csu3p_dt inp
      !useMat = csu3p_use_matrixcn inp
      -- gresp pools always updated
      !gs = csu3p_gresp_storage inp
            - csu3p_m_gresp_storage_to_fire inp * dt
            - csu3p_m_gresp_storage_to_litter_fire inp * dt
      !gx = csu3p_gresp_xfer inp
            - csu3p_m_gresp_xfer_to_fire inp * dt
            - csu3p_m_gresp_xfer_to_litter_fire inp * dt
  in if useMat
     then CStateUpdate3PatchOutput
       { csu3po_leafc = csu3p_leafc inp, csu3po_frootc = csu3p_frootc inp
       , csu3po_livestemc = csu3p_livestemc inp, csu3po_deadstemc = csu3p_deadstemc inp
       , csu3po_livecrootc = csu3p_livecrootc inp, csu3po_deadcrootc = csu3p_deadcrootc inp
       , csu3po_gresp_storage = gs, csu3po_gresp_xfer = gx
       }
     else CStateUpdate3PatchOutput
       { csu3po_leafc = csu3p_leafc inp
           - csu3p_m_leafc_to_fire inp * dt
           - csu3p_m_leafc_to_litter_fire inp * dt
       , csu3po_frootc = csu3p_frootc inp
           - csu3p_m_frootc_to_fire inp * dt
           - csu3p_m_frootc_to_litter_fire inp * dt
       , csu3po_livestemc = csu3p_livestemc inp
           - csu3p_m_livestemc_to_fire inp * dt
           - csu3p_m_livestemc_to_litter_fire inp * dt
           - csu3p_m_livestemc_to_deadstemc_fire inp * dt
       , csu3po_deadstemc = csu3p_deadstemc inp
           - csu3p_m_deadstemc_to_fire inp * dt
           - csu3p_m_deadstemc_to_litter_fire inp * dt
           + csu3p_m_livestemc_to_deadstemc_fire inp * dt
       , csu3po_livecrootc = csu3p_livecrootc inp
           - csu3p_m_livecrootc_to_fire inp * dt
           - csu3p_m_livecrootc_to_litter_fire inp * dt
           - csu3p_m_livecrootc_to_deadcrootc_fire inp * dt
       , csu3po_deadcrootc = csu3p_deadcrootc inp
           - csu3p_m_deadcrootc_to_fire inp * dt
           - csu3p_m_deadcrootc_to_litter_fire inp * dt
           + csu3p_m_livecrootc_to_deadcrootc_fire inp * dt
       , csu3po_gresp_storage = gs
       , csu3po_gresp_xfer = gx
       }
