-- | Soil biogeochemistry carbon state data structures.
-- Fortran: SoilBiogeochemCarbonStateType.F90 — decomposition C pools at column and gridcell levels.
module CLM.Types.SoilBGCCarbonStateData
  ( SoilBGCCarbonStateData(..)
  , defaultSoilBGCCarbonStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Soil biogeochemistry carbon state (SoA layout).
-- Preserves Fortran variable names with @sbgccs_@ prefix.
-- 2D/3D arrays are flattened to 1D vectors with dimension comments.
-- Matrix-CN fields are omitted; add if needed.
data SoilBGCCarbonStateData = SoilBGCCarbonStateData
  { -- Vertically-resolved decomposition pools (col * nlev * npools, flattened)
    sbgccs_decomp_cpools_vr_col       :: !(VU.Vector Double)   -- ^ (gC/m3)
    -- Vertically-resolved summary (col * nlevdecomp_full, flattened)
  , sbgccs_decomp_soilc_vr_col        :: !(VU.Vector Double)   -- ^ (gC/m3) total soil C vr
  , sbgccs_ctrunc_vr_col              :: !(VU.Vector Double)   -- ^ (gC/m3) C truncation vr
    -- Summary (diagnostic) state variables (col)
  , sbgccs_ctrunc_col                  :: !(VU.Vector Double)   -- ^ (gC/m2) C truncation
  , sbgccs_totmicc_col                 :: !(VU.Vector Double)   -- ^ (gC/m2) total microbial C
  , sbgccs_totlitc_col                 :: !(VU.Vector Double)   -- ^ (gC/m2) total litter C
  , sbgccs_totlitc_1m_col             :: !(VU.Vector Double)   -- ^ (gC/m2) total litter C to 1m
  , sbgccs_totsomc_col                 :: !(VU.Vector Double)   -- ^ (gC/m2) total SOM C
  , sbgccs_totsomc_1m_col             :: !(VU.Vector Double)   -- ^ (gC/m2) total SOM C to 1m
  , sbgccs_cwdc_col                    :: !(VU.Vector Double)   -- ^ (gC/m2) coarse woody debris C
  , sbgccs_decomp_cpools_1m_col       :: !(VU.Vector Double)   -- ^ (gC/m2) decomp pools to 1m (col * npools, flattened)
  , sbgccs_decomp_cpools_col          :: !(VU.Vector Double)   -- ^ (gC/m2) decomp pools (col * npools, flattened)
  , sbgccs_dyn_cbal_adjustments_col   :: !(VU.Vector Double)   -- ^ (gC/m2) dynamic column area adjustments
    -- Scalars
  , sbgccs_restart_file_spinup_state   :: !Int                  -- ^ spinup state from restart file
  , sbgccs_totvegcthresh               :: !Double               -- ^ threshold for zeroing decomp pools
    -- Carbon totals
  , sbgccs_totc_col                    :: !(VU.Vector Double)   -- ^ (gC/m2) total column C
  , sbgccs_totecosysc_col             :: !(VU.Vector Double)   -- ^ (gC/m2) total ecosystem C
  , sbgccs_totc_grc                    :: !(VU.Vector Double)   -- ^ (gC/m2) total gridcell C
  } deriving (Show)

defaultSoilBGCCarbonStateData :: SoilBGCCarbonStateData
defaultSoilBGCCarbonStateData = SoilBGCCarbonStateData
  { sbgccs_decomp_cpools_vr_col = VU.empty
  , sbgccs_decomp_soilc_vr_col = VU.empty, sbgccs_ctrunc_vr_col = VU.empty
  , sbgccs_ctrunc_col = VU.empty, sbgccs_totmicc_col = VU.empty
  , sbgccs_totlitc_col = VU.empty, sbgccs_totlitc_1m_col = VU.empty
  , sbgccs_totsomc_col = VU.empty, sbgccs_totsomc_1m_col = VU.empty
  , sbgccs_cwdc_col = VU.empty
  , sbgccs_decomp_cpools_1m_col = VU.empty, sbgccs_decomp_cpools_col = VU.empty
  , sbgccs_dyn_cbal_adjustments_col = VU.empty
  , sbgccs_restart_file_spinup_state = maxBound
  , sbgccs_totvegcthresh = 0.0 / 0.0  -- NaN
  , sbgccs_totc_col = VU.empty, sbgccs_totecosysc_col = VU.empty
  , sbgccs_totc_grc = VU.empty
  }
