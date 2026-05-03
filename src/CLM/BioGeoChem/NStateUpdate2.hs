{-# LANGUAGE BangPatterns #-}
-- | Nitrogen state variable update, mortality fluxes (gap, harvest, gross unrepresented).
-- Fortran: CNNStateUpdate2Mod.F90
-- Julia:   src/biogeochem/n_state_update2.jl
module CLM.BioGeoChem.NStateUpdate2
  ( NStateUpdate2PatchInput(..)
  , NStateUpdate2PatchOutput(..)
  , nStateUpdate2Patch
  , nStateUpdate2hPatch
  , nStateUpdate2gPatch
  ) where

data NStateUpdate2PatchInput = NStateUpdate2PatchInput
  { nsu2p_leafn          :: !Double
  , nsu2p_frootn         :: !Double
  , nsu2p_livestemn      :: !Double
  , nsu2p_deadstemn      :: !Double
  , nsu2p_livecrootn     :: !Double
  , nsu2p_deadcrootn     :: !Double
  , nsu2p_retransn       :: !Double
  -- Mortality fluxes (displayed)
  , nsu2p_m_leafn_to_litter      :: !Double
  , nsu2p_m_frootn_to_litter     :: !Double
  , nsu2p_m_livestemn_to_litter  :: !Double
  , nsu2p_m_deadstemn_to_litter  :: !Double
  , nsu2p_m_livecrootn_to_litter :: !Double
  , nsu2p_m_deadcrootn_to_litter :: !Double
  , nsu2p_m_retransn_to_litter   :: !Double
  -- Storage mortality fluxes
  , nsu2p_m_leafn_storage_to_litter      :: !Double
  , nsu2p_m_frootn_storage_to_litter     :: !Double
  , nsu2p_m_livestemn_storage_to_litter  :: !Double
  , nsu2p_m_deadstemn_storage_to_litter  :: !Double
  , nsu2p_m_livecrootn_storage_to_litter :: !Double
  , nsu2p_m_deadcrootn_storage_to_litter :: !Double
  -- Transfer mortality fluxes
  , nsu2p_m_leafn_xfer_to_litter      :: !Double
  , nsu2p_m_frootn_xfer_to_litter     :: !Double
  , nsu2p_m_livestemn_xfer_to_litter  :: !Double
  , nsu2p_m_deadstemn_xfer_to_litter  :: !Double
  , nsu2p_m_livecrootn_xfer_to_litter :: !Double
  , nsu2p_m_deadcrootn_xfer_to_litter :: !Double
  -- State: storage and transfer pools
  , nsu2p_leafn_storage      :: !Double
  , nsu2p_frootn_storage     :: !Double
  , nsu2p_livestemn_storage  :: !Double
  , nsu2p_deadstemn_storage  :: !Double
  , nsu2p_livecrootn_storage :: !Double
  , nsu2p_deadcrootn_storage :: !Double
  , nsu2p_leafn_xfer         :: !Double
  , nsu2p_frootn_xfer        :: !Double
  , nsu2p_livestemn_xfer     :: !Double
  , nsu2p_deadstemn_xfer     :: !Double
  , nsu2p_livecrootn_xfer    :: !Double
  , nsu2p_deadcrootn_xfer    :: !Double
  , nsu2p_use_matrixcn       :: !Bool
  , nsu2p_dt                 :: !Double
  } deriving (Show)

data NStateUpdate2PatchOutput = NStateUpdate2PatchOutput
  { nsu2po_leafn          :: !Double
  , nsu2po_frootn         :: !Double
  , nsu2po_livestemn      :: !Double
  , nsu2po_deadstemn      :: !Double
  , nsu2po_livecrootn     :: !Double
  , nsu2po_deadcrootn     :: !Double
  , nsu2po_retransn       :: !Double
  , nsu2po_leafn_storage      :: !Double
  , nsu2po_frootn_storage     :: !Double
  , nsu2po_livestemn_storage  :: !Double
  , nsu2po_deadstemn_storage  :: !Double
  , nsu2po_livecrootn_storage :: !Double
  , nsu2po_deadcrootn_storage :: !Double
  , nsu2po_leafn_xfer         :: !Double
  , nsu2po_frootn_xfer        :: !Double
  , nsu2po_livestemn_xfer     :: !Double
  , nsu2po_deadstemn_xfer     :: !Double
  , nsu2po_livecrootn_xfer    :: !Double
  , nsu2po_deadcrootn_xfer    :: !Double
  } deriving (Show)

-- | Gap-phase mortality N state update for a single patch.
nStateUpdate2Patch :: NStateUpdate2PatchInput -> NStateUpdate2PatchOutput
nStateUpdate2Patch inp =
  let !dt = nsu2p_dt inp
      !useMat = nsu2p_use_matrixcn inp
      sub v f = v - f * dt
      cond v f = if useMat then v else sub v f
  in NStateUpdate2PatchOutput
    { nsu2po_leafn          = cond (nsu2p_leafn inp) (nsu2p_m_leafn_to_litter inp)
    , nsu2po_frootn         = cond (nsu2p_frootn inp) (nsu2p_m_frootn_to_litter inp)
    , nsu2po_livestemn      = cond (nsu2p_livestemn inp) (nsu2p_m_livestemn_to_litter inp)
    , nsu2po_deadstemn      = cond (nsu2p_deadstemn inp) (nsu2p_m_deadstemn_to_litter inp)
    , nsu2po_livecrootn     = cond (nsu2p_livecrootn inp) (nsu2p_m_livecrootn_to_litter inp)
    , nsu2po_deadcrootn     = cond (nsu2p_deadcrootn inp) (nsu2p_m_deadcrootn_to_litter inp)
    , nsu2po_retransn       = cond (nsu2p_retransn inp) (nsu2p_m_retransn_to_litter inp)
    , nsu2po_leafn_storage      = cond (nsu2p_leafn_storage inp) (nsu2p_m_leafn_storage_to_litter inp)
    , nsu2po_frootn_storage     = cond (nsu2p_frootn_storage inp) (nsu2p_m_frootn_storage_to_litter inp)
    , nsu2po_livestemn_storage  = cond (nsu2p_livestemn_storage inp) (nsu2p_m_livestemn_storage_to_litter inp)
    , nsu2po_deadstemn_storage  = cond (nsu2p_deadstemn_storage inp) (nsu2p_m_deadstemn_storage_to_litter inp)
    , nsu2po_livecrootn_storage = cond (nsu2p_livecrootn_storage inp) (nsu2p_m_livecrootn_storage_to_litter inp)
    , nsu2po_deadcrootn_storage = cond (nsu2p_deadcrootn_storage inp) (nsu2p_m_deadcrootn_storage_to_litter inp)
    , nsu2po_leafn_xfer         = cond (nsu2p_leafn_xfer inp) (nsu2p_m_leafn_xfer_to_litter inp)
    , nsu2po_frootn_xfer        = cond (nsu2p_frootn_xfer inp) (nsu2p_m_frootn_xfer_to_litter inp)
    , nsu2po_livestemn_xfer     = cond (nsu2p_livestemn_xfer inp) (nsu2p_m_livestemn_xfer_to_litter inp)
    , nsu2po_deadstemn_xfer     = cond (nsu2p_deadstemn_xfer inp) (nsu2p_m_deadstemn_xfer_to_litter inp)
    , nsu2po_livecrootn_xfer    = cond (nsu2p_livecrootn_xfer inp) (nsu2p_m_livecrootn_xfer_to_litter inp)
    , nsu2po_deadcrootn_xfer    = cond (nsu2p_deadcrootn_xfer inp) (nsu2p_m_deadcrootn_xfer_to_litter inp)
    }

nStateUpdate2hPatch :: NStateUpdate2PatchInput -> NStateUpdate2PatchOutput
nStateUpdate2hPatch = nStateUpdate2Patch

nStateUpdate2gPatch :: NStateUpdate2PatchInput -> NStateUpdate2PatchOutput
nStateUpdate2gPatch = nStateUpdate2Patch
