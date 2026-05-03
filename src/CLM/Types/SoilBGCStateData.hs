-- | Soil biogeochemistry state data structures.
-- Fortran: SoilBiogeochemStateType.F90 — vertical profiles, fraction of potential
-- immobilization/GPP, SOM transport coefficients, N fixation/deposition profiles.
module CLM.Types.SoilBGCStateData
  ( SoilBGCStateData(..)
  , defaultSoilBGCStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Soil biogeochemistry state (SoA layout).
-- Preserves Fortran variable names with @sbgcs_@ prefix.
-- 2D/3D arrays are flattened to 1D vectors.
data SoilBGCStateData = SoilBGCStateData
  { -- Patch-level vertical profiles (patch * nlevdecomp_full, flattened)
    sbgcs_leaf_prof_patch           :: !(VU.Vector Double)   -- ^ (1/m) profile of leaves
  , sbgcs_froot_prof_patch          :: !(VU.Vector Double)   -- ^ (1/m) profile of fine roots
  , sbgcs_croot_prof_patch          :: !(VU.Vector Double)   -- ^ (1/m) profile of coarse roots
  , sbgcs_stem_prof_patch           :: !(VU.Vector Double)   -- ^ (1/m) profile of stems
    -- Column-level vertically-resolved (col * nlevdecomp_full, flattened)
  , sbgcs_fpi_vr_col                :: !(VU.Vector Double)   -- ^ fraction of potential immobilization vr
  , sbgcs_nfixation_prof_col        :: !(VU.Vector Double)   -- ^ (1/m) profile for N fixation additions
  , sbgcs_ndep_prof_col             :: !(VU.Vector Double)   -- ^ (1/m) profile for N deposition additions
  , sbgcs_som_adv_coef_col          :: !(VU.Vector Double)   -- ^ (m/s) SOM advective flux
  , sbgcs_som_diffus_coef_col       :: !(VU.Vector Double)   -- ^ (m2/s) SOM diffusivity (bio/cryo-turbation)
    -- Column-level 1D
  , sbgcs_fpi_col                   :: !(VU.Vector Double)   -- ^ fraction of potential immobilization
  , sbgcs_fpg_col                   :: !(VU.Vector Double)   -- ^ fraction of potential gpp
  , sbgcs_plant_ndemand_col         :: !(VU.Vector Double)   -- ^ column-level plant N demand
    -- Transition-level 1D
  , sbgcs_nue_decomp_cascade_col    :: !(VU.Vector Double)   -- ^ (gN/gN) N use efficiency for a given transition
  } deriving (Show)

defaultSoilBGCStateData :: SoilBGCStateData
defaultSoilBGCStateData = SoilBGCStateData
  { sbgcs_leaf_prof_patch = VU.empty, sbgcs_froot_prof_patch = VU.empty
  , sbgcs_croot_prof_patch = VU.empty, sbgcs_stem_prof_patch = VU.empty
  , sbgcs_fpi_vr_col = VU.empty, sbgcs_nfixation_prof_col = VU.empty
  , sbgcs_ndep_prof_col = VU.empty, sbgcs_som_adv_coef_col = VU.empty
  , sbgcs_som_diffus_coef_col = VU.empty
  , sbgcs_fpi_col = VU.empty, sbgcs_fpg_col = VU.empty
  , sbgcs_plant_ndemand_col = VU.empty
  , sbgcs_nue_decomp_cascade_col = VU.empty
  }
