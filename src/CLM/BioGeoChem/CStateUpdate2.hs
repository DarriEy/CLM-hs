{-# LANGUAGE BangPatterns #-}
-- | Carbon state variable update, mortality fluxes (gap, harvest, gross unrepresented).
-- Fortran: CNCStateUpdate2Mod.F90
-- Julia:   src/biogeochem/c_state_update2.jl
--
-- Pure functions for gap-phase mortality, harvest mortality, and gross
-- unrepresented landcover change mortality C state updates.
module CLM.BioGeoChem.CStateUpdate2
  ( -- * Input / Output types
    CStateUpdate2ColInput(..)
  , CStateUpdate2ColOutput(..)
  , CStateUpdate2PatchInput(..)
  , CStateUpdate2PatchOutput(..)
    -- * Functions
  , cStateUpdate2Col
  , cStateUpdate2Patch
  , cStateUpdate2hPatch
  , cStateUpdate2gPatch
  ) where

-- =========================================================================
-- Column-level: mortality C to decomposition pools
-- =========================================================================

data CStateUpdate2ColInput = CStateUpdate2ColInput
  { csu2c_decomp_pool   :: !Double  -- ^ current decomp C pool [gC/m3]
  , csu2c_mortality_flux :: !Double  -- ^ mortality C flux to this pool
  , csu2c_dt             :: !Double  -- ^ timestep [s]
  } deriving (Show)

data CStateUpdate2ColOutput = CStateUpdate2ColOutput
  { csu2co_decomp_pool :: !Double  -- ^ updated decomp C pool
  } deriving (Show)

-- | Update decomp pool from mortality flux (works for gap, harvest, gru, fire).
cStateUpdate2Col :: CStateUpdate2ColInput -> CStateUpdate2ColOutput
cStateUpdate2Col inp =
  CStateUpdate2ColOutput
    { csu2co_decomp_pool = csu2c_decomp_pool inp + csu2c_mortality_flux inp * csu2c_dt inp }

-- =========================================================================
-- Patch-level: gap-phase mortality
-- =========================================================================

data CStateUpdate2PatchInput = CStateUpdate2PatchInput
  { csu2p_leafc          :: !Double
  , csu2p_frootc         :: !Double
  , csu2p_livestemc      :: !Double
  , csu2p_deadstemc      :: !Double
  , csu2p_livecrootc     :: !Double
  , csu2p_deadcrootc     :: !Double
  , csu2p_gresp_storage  :: !Double
  , csu2p_gresp_xfer     :: !Double
  -- Gap mortality fluxes (displayed pools)
  , csu2p_m_leafc_to_litter      :: !Double
  , csu2p_m_frootc_to_litter     :: !Double
  , csu2p_m_livestemc_to_litter  :: !Double
  , csu2p_m_deadstemc_to_litter  :: !Double
  , csu2p_m_livecrootc_to_litter :: !Double
  , csu2p_m_deadcrootc_to_litter :: !Double
  , csu2p_m_gresp_storage_to_litter :: !Double
  , csu2p_m_gresp_xfer_to_litter    :: !Double
  -- Storage pool mortality fluxes
  , csu2p_m_leafc_storage_to_litter      :: !Double
  , csu2p_m_frootc_storage_to_litter     :: !Double
  , csu2p_m_livestemc_storage_to_litter  :: !Double
  , csu2p_m_deadstemc_storage_to_litter  :: !Double
  , csu2p_m_livecrootc_storage_to_litter :: !Double
  , csu2p_m_deadcrootc_storage_to_litter :: !Double
  -- Transfer pool mortality fluxes
  , csu2p_m_leafc_xfer_to_litter      :: !Double
  , csu2p_m_frootc_xfer_to_litter     :: !Double
  , csu2p_m_livestemc_xfer_to_litter  :: !Double
  , csu2p_m_deadstemc_xfer_to_litter  :: !Double
  , csu2p_m_livecrootc_xfer_to_litter :: !Double
  , csu2p_m_deadcrootc_xfer_to_litter :: !Double
  -- Storage/xfer state
  , csu2p_leafc_storage      :: !Double
  , csu2p_frootc_storage     :: !Double
  , csu2p_livestemc_storage  :: !Double
  , csu2p_deadstemc_storage  :: !Double
  , csu2p_livecrootc_storage :: !Double
  , csu2p_deadcrootc_storage :: !Double
  , csu2p_leafc_xfer         :: !Double
  , csu2p_frootc_xfer        :: !Double
  , csu2p_livestemc_xfer     :: !Double
  , csu2p_deadstemc_xfer     :: !Double
  , csu2p_livecrootc_xfer    :: !Double
  , csu2p_deadcrootc_xfer    :: !Double
  , csu2p_use_matrixcn       :: !Bool
  , csu2p_dt                 :: !Double
  } deriving (Show)

data CStateUpdate2PatchOutput = CStateUpdate2PatchOutput
  { csu2po_leafc          :: !Double
  , csu2po_frootc         :: !Double
  , csu2po_livestemc      :: !Double
  , csu2po_deadstemc      :: !Double
  , csu2po_livecrootc     :: !Double
  , csu2po_deadcrootc     :: !Double
  , csu2po_gresp_storage  :: !Double
  , csu2po_gresp_xfer     :: !Double
  , csu2po_leafc_storage      :: !Double
  , csu2po_frootc_storage     :: !Double
  , csu2po_livestemc_storage  :: !Double
  , csu2po_deadstemc_storage  :: !Double
  , csu2po_livecrootc_storage :: !Double
  , csu2po_deadcrootc_storage :: !Double
  , csu2po_leafc_xfer         :: !Double
  , csu2po_frootc_xfer        :: !Double
  , csu2po_livestemc_xfer     :: !Double
  , csu2po_deadstemc_xfer     :: !Double
  , csu2po_livecrootc_xfer    :: !Double
  , csu2po_deadcrootc_xfer    :: !Double
  } deriving (Show)

-- | Gap-phase mortality C state update for a single patch.
cStateUpdate2Patch :: CStateUpdate2PatchInput -> CStateUpdate2PatchOutput
cStateUpdate2Patch inp =
  let !dt = csu2p_dt inp
      !useMat = csu2p_use_matrixcn inp
      sub v f = v - f * dt
  in CStateUpdate2PatchOutput
    { csu2po_gresp_storage = sub (csu2p_gresp_storage inp) (csu2p_m_gresp_storage_to_litter inp)
    , csu2po_gresp_xfer    = sub (csu2p_gresp_xfer inp) (csu2p_m_gresp_xfer_to_litter inp)
    , csu2po_leafc          = if useMat then csu2p_leafc inp else sub (csu2p_leafc inp) (csu2p_m_leafc_to_litter inp)
    , csu2po_frootc         = if useMat then csu2p_frootc inp else sub (csu2p_frootc inp) (csu2p_m_frootc_to_litter inp)
    , csu2po_livestemc      = if useMat then csu2p_livestemc inp else sub (csu2p_livestemc inp) (csu2p_m_livestemc_to_litter inp)
    , csu2po_deadstemc      = if useMat then csu2p_deadstemc inp else sub (csu2p_deadstemc inp) (csu2p_m_deadstemc_to_litter inp)
    , csu2po_livecrootc     = if useMat then csu2p_livecrootc inp else sub (csu2p_livecrootc inp) (csu2p_m_livecrootc_to_litter inp)
    , csu2po_deadcrootc     = if useMat then csu2p_deadcrootc inp else sub (csu2p_deadcrootc inp) (csu2p_m_deadcrootc_to_litter inp)
    , csu2po_leafc_storage      = if useMat then csu2p_leafc_storage inp else sub (csu2p_leafc_storage inp) (csu2p_m_leafc_storage_to_litter inp)
    , csu2po_frootc_storage     = if useMat then csu2p_frootc_storage inp else sub (csu2p_frootc_storage inp) (csu2p_m_frootc_storage_to_litter inp)
    , csu2po_livestemc_storage  = if useMat then csu2p_livestemc_storage inp else sub (csu2p_livestemc_storage inp) (csu2p_m_livestemc_storage_to_litter inp)
    , csu2po_deadstemc_storage  = if useMat then csu2p_deadstemc_storage inp else sub (csu2p_deadstemc_storage inp) (csu2p_m_deadstemc_storage_to_litter inp)
    , csu2po_livecrootc_storage = if useMat then csu2p_livecrootc_storage inp else sub (csu2p_livecrootc_storage inp) (csu2p_m_livecrootc_storage_to_litter inp)
    , csu2po_deadcrootc_storage = if useMat then csu2p_deadcrootc_storage inp else sub (csu2p_deadcrootc_storage inp) (csu2p_m_deadcrootc_storage_to_litter inp)
    , csu2po_leafc_xfer         = if useMat then csu2p_leafc_xfer inp else sub (csu2p_leafc_xfer inp) (csu2p_m_leafc_xfer_to_litter inp)
    , csu2po_frootc_xfer        = if useMat then csu2p_frootc_xfer inp else sub (csu2p_frootc_xfer inp) (csu2p_m_frootc_xfer_to_litter inp)
    , csu2po_livestemc_xfer     = if useMat then csu2p_livestemc_xfer inp else sub (csu2p_livestemc_xfer inp) (csu2p_m_livestemc_xfer_to_litter inp)
    , csu2po_deadstemc_xfer     = if useMat then csu2p_deadstemc_xfer inp else sub (csu2p_deadstemc_xfer inp) (csu2p_m_deadstemc_xfer_to_litter inp)
    , csu2po_livecrootc_xfer    = if useMat then csu2p_livecrootc_xfer inp else sub (csu2p_livecrootc_xfer inp) (csu2p_m_livecrootc_xfer_to_litter inp)
    , csu2po_deadcrootc_xfer    = if useMat then csu2p_deadcrootc_xfer inp else sub (csu2p_deadcrootc_xfer inp) (csu2p_m_deadcrootc_xfer_to_litter inp)
    }

-- | Harvest mortality C state update (same structure, different flux field names in caller).
cStateUpdate2hPatch :: CStateUpdate2PatchInput -> CStateUpdate2PatchOutput
cStateUpdate2hPatch = cStateUpdate2Patch

-- | Gross unrepresented landcover change mortality C state update.
cStateUpdate2gPatch :: CStateUpdate2PatchInput -> CStateUpdate2PatchOutput
cStateUpdate2gPatch = cStateUpdate2Patch
