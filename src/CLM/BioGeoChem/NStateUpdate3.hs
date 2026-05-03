{-# LANGUAGE BangPatterns #-}
-- | Nitrogen state variable update, fire fluxes and N leaching.
-- Fortran: CNNStateUpdate3Mod.F90
-- Julia:   src/biogeochem/n_state_update3.jl
module CLM.BioGeoChem.NStateUpdate3
  ( NStateLeachingInput(..)
  , NStateLeachingOutput(..)
  , nStateUpdateLeaching
  , NStateUpdate3PatchInput(..)
  , NStateUpdate3PatchOutput(..)
  , nStateUpdate3Patch
  ) where

-- =========================================================================
-- N leaching
-- =========================================================================

data NStateLeachingInput = NStateLeachingInput
  { nsl_sminn_vr         :: !Double  -- ^ mineral N [gN/m3]
  , nsl_smin_no3_vr      :: !Double  -- ^ NO3 [gN/m3]
  , nsl_smin_nh4_vr      :: !Double  -- ^ NH4 [gN/m3]
  , nsl_sminn_leached_vr :: !Double  -- ^ leaching flux
  , nsl_smin_no3_leached_vr :: !Double
  , nsl_smin_no3_runoff_vr  :: !Double
  , nsl_use_nitrif_denitrif :: !Bool
  , nsl_dt               :: !Double
  } deriving (Show)

data NStateLeachingOutput = NStateLeachingOutput
  { nslo_sminn_vr    :: !Double
  , nslo_smin_no3_vr :: !Double
  } deriving (Show)

nStateUpdateLeaching :: NStateLeachingInput -> NStateLeachingOutput
nStateUpdateLeaching inp =
  let !dt = nsl_dt inp
  in if not (nsl_use_nitrif_denitrif inp)
     then NStateLeachingOutput
       { nslo_sminn_vr = nsl_sminn_vr inp - nsl_sminn_leached_vr inp * dt
       , nslo_smin_no3_vr = nsl_smin_no3_vr inp
       }
     else let !no3' = max 0.0 (nsl_smin_no3_vr inp
                     - (nsl_smin_no3_leached_vr inp + nsl_smin_no3_runoff_vr inp) * dt)
          in NStateLeachingOutput
            { nslo_smin_no3_vr = no3'
            , nslo_sminn_vr = no3' + nsl_smin_nh4_vr inp
            }

-- =========================================================================
-- Fire N state update (patch level)
-- =========================================================================

data NStateUpdate3PatchInput = NStateUpdate3PatchInput
  { nsu3p_leafn      :: !Double
  , nsu3p_frootn     :: !Double
  , nsu3p_livestemn  :: !Double
  , nsu3p_deadstemn  :: !Double
  , nsu3p_livecrootn :: !Double
  , nsu3p_deadcrootn :: !Double
  , nsu3p_retransn   :: !Double
  -- Fire emission fluxes
  , nsu3p_m_leafn_to_fire          :: !Double
  , nsu3p_m_leafn_to_litter_fire   :: !Double
  , nsu3p_m_frootn_to_fire         :: !Double
  , nsu3p_m_frootn_to_litter_fire  :: !Double
  , nsu3p_m_livestemn_to_fire              :: !Double
  , nsu3p_m_livestemn_to_litter_fire       :: !Double
  , nsu3p_m_livestemn_to_deadstemn_fire    :: !Double
  , nsu3p_m_deadstemn_to_fire              :: !Double
  , nsu3p_m_deadstemn_to_litter_fire       :: !Double
  , nsu3p_m_livecrootn_to_fire             :: !Double
  , nsu3p_m_livecrootn_to_litter_fire      :: !Double
  , nsu3p_m_livecrootn_to_deadcrootn_fire  :: !Double
  , nsu3p_m_deadcrootn_to_fire             :: !Double
  , nsu3p_m_deadcrootn_to_litter_fire      :: !Double
  , nsu3p_m_retransn_to_fire               :: !Double
  , nsu3p_m_retransn_to_litter_fire        :: !Double
  -- Storage/xfer fire fluxes (summed fire + litter_fire for each)
  , nsu3p_m_leafn_storage_fire_total      :: !Double
  , nsu3p_m_frootn_storage_fire_total     :: !Double
  , nsu3p_m_livestemn_storage_fire_total  :: !Double
  , nsu3p_m_deadstemn_storage_fire_total  :: !Double
  , nsu3p_m_livecrootn_storage_fire_total :: !Double
  , nsu3p_m_deadcrootn_storage_fire_total :: !Double
  , nsu3p_m_leafn_xfer_fire_total         :: !Double
  , nsu3p_m_frootn_xfer_fire_total        :: !Double
  , nsu3p_m_livestemn_xfer_fire_total     :: !Double
  , nsu3p_m_deadstemn_xfer_fire_total     :: !Double
  , nsu3p_m_livecrootn_xfer_fire_total    :: !Double
  , nsu3p_m_deadcrootn_xfer_fire_total    :: !Double
  -- Config
  , nsu3p_use_matrixcn :: !Bool
  , nsu3p_dt           :: !Double
  } deriving (Show)

data NStateUpdate3PatchOutput = NStateUpdate3PatchOutput
  { nsu3po_leafn      :: !Double
  , nsu3po_frootn     :: !Double
  , nsu3po_livestemn  :: !Double
  , nsu3po_deadstemn  :: !Double
  , nsu3po_livecrootn :: !Double
  , nsu3po_deadcrootn :: !Double
  , nsu3po_retransn   :: !Double
  } deriving (Show)

nStateUpdate3Patch :: NStateUpdate3PatchInput -> NStateUpdate3PatchOutput
nStateUpdate3Patch inp =
  let !dt = nsu3p_dt inp
      !useMat = nsu3p_use_matrixcn inp
  in if useMat
     then NStateUpdate3PatchOutput
       { nsu3po_leafn = nsu3p_leafn inp, nsu3po_frootn = nsu3p_frootn inp
       , nsu3po_livestemn = nsu3p_livestemn inp, nsu3po_deadstemn = nsu3p_deadstemn inp
       , nsu3po_livecrootn = nsu3p_livecrootn inp, nsu3po_deadcrootn = nsu3p_deadcrootn inp
       , nsu3po_retransn = nsu3p_retransn inp }
     else NStateUpdate3PatchOutput
       { nsu3po_leafn = nsu3p_leafn inp
           - nsu3p_m_leafn_to_fire inp * dt - nsu3p_m_leafn_to_litter_fire inp * dt
       , nsu3po_frootn = nsu3p_frootn inp
           - nsu3p_m_frootn_to_fire inp * dt - nsu3p_m_frootn_to_litter_fire inp * dt
       , nsu3po_livestemn = nsu3p_livestemn inp
           - nsu3p_m_livestemn_to_fire inp * dt
           - nsu3p_m_livestemn_to_litter_fire inp * dt
           - nsu3p_m_livestemn_to_deadstemn_fire inp * dt
       , nsu3po_deadstemn = nsu3p_deadstemn inp
           - nsu3p_m_deadstemn_to_fire inp * dt
           - nsu3p_m_deadstemn_to_litter_fire inp * dt
           + nsu3p_m_livestemn_to_deadstemn_fire inp * dt
       , nsu3po_livecrootn = nsu3p_livecrootn inp
           - nsu3p_m_livecrootn_to_fire inp * dt
           - nsu3p_m_livecrootn_to_litter_fire inp * dt
           - nsu3p_m_livecrootn_to_deadcrootn_fire inp * dt
       , nsu3po_deadcrootn = nsu3p_deadcrootn inp
           - nsu3p_m_deadcrootn_to_fire inp * dt
           - nsu3p_m_deadcrootn_to_litter_fire inp * dt
           + nsu3p_m_livecrootn_to_deadcrootn_fire inp * dt
       , nsu3po_retransn = nsu3p_retransn inp
           - nsu3p_m_retransn_to_fire inp * dt
           - nsu3p_m_retransn_to_litter_fire inp * dt
       }
