-- | Soil biogeochemistry carbon flux data structures.
-- Fortran: SoilBiogeochemCarbonFluxType.F90 — decomposition C fluxes,
-- heterotrophic respiration, vertical transport, and related diagnostics.
module CLM.Types.SoilBGCCarbonFluxData
  ( SoilBGCCarbonFluxData(..)
  , defaultSoilBGCCarbonFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Soil biogeochemistry carbon flux (SoA layout).
-- Preserves Fortran variable names with @sbgccf_@ prefix.
-- 2D/3D arrays are flattened to 1D vectors with dimension comments.
-- Matrix-CN sparse index fields are omitted; add if needed.
data SoilBGCCarbonFluxData = SoilBGCCarbonFluxData
  { -- Fire fluxes (col)
    sbgccf_somc_fire_col                        :: !(VU.Vector Double)  -- ^ (gC/m2/s) C emissions due to peat burning
    -- Decomposition fluxes (col * nlev * npools/ntrans, flattened)
  , sbgccf_decomp_cpools_sourcesink_col         :: !(VU.Vector Double)  -- ^ (gC/m3/timestep) change in decomposing C pools
  , sbgccf_decomp_cascade_hr_vr_col             :: !(VU.Vector Double)  -- ^ (gC/m3/s) het. resp. from decomposing C pools (vr)
  , sbgccf_decomp_cascade_hr_col                :: !(VU.Vector Double)  -- ^ (gC/m2/s) het. resp. from decomposing C pools (integrated)
  , sbgccf_decomp_cascade_ctransfer_vr_col      :: !(VU.Vector Double)  -- ^ (gC/m3/s) C transferred along decomp cascade (vr)
  , sbgccf_decomp_cascade_ctransfer_col         :: !(VU.Vector Double)  -- ^ (gC/m2/s) C transferred along decomp cascade (integrated)
  , sbgccf_c_overflow_vr                        :: !(VU.Vector Double)  -- ^ (gC/m3/s) C rejected by microbes
  , sbgccf_rf_decomp_cascade_col                :: !(VU.Vector Double)  -- ^ (frac) respired fraction in decomp step
  , sbgccf_pathfrac_decomp_cascade_col          :: !(VU.Vector Double)  -- ^ (frac) C fraction through given transition
  , sbgccf_decomp_k_col                         :: !(VU.Vector Double)  -- ^ (1/s) rate coefficient for decomposition
  , sbgccf_cn_col                               :: !(VU.Vector Double)  -- ^ (gC/gN) C:N ratio by pool (col * npools, flattened)
  , sbgccf_litr_lig_c_to_n_col                  :: !(VU.Vector Double)  -- ^ (gC/gN) avg lignin C:N ratio (col)
    -- Environmental scalars (col * nlevdecomp_full, flattened)
  , sbgccf_hr_vr_col                            :: !(VU.Vector Double)  -- ^ (gC/m3/s) total vr het. resp.
  , sbgccf_o_scalar_col                         :: !(VU.Vector Double)  -- ^ fraction decomp limited by anoxia
  , sbgccf_w_scalar_col                         :: !(VU.Vector Double)  -- ^ fraction decomp limited by moisture
  , sbgccf_t_scalar_col                         :: !(VU.Vector Double)  -- ^ fraction decomp limited by temperature
    -- Vertical transport (col)
  , sbgccf_som_c_leached_col                    :: !(VU.Vector Double)  -- ^ (gC/m2/s) total SOM C loss from vertical transport
  , sbgccf_decomp_cpools_leached_col            :: !(VU.Vector Double)  -- ^ (gC/m2/s) C loss from vert transport per pool (col * npools)
  , sbgccf_decomp_cpools_transport_tendency_col :: !(VU.Vector Double)  -- ^ (gC/m3/s) C tendency due to vert transport
    -- Nitrif/denitrif (col * nlevdecomp_full, flattened)
  , sbgccf_phr_vr_col                           :: !(VU.Vector Double)  -- ^ (gC/m3/s) potential hr (not N-limited)
  , sbgccf_fphr_col                             :: !(VU.Vector Double)  -- ^ fraction of potential het. resp.
    -- Summary HR fluxes (col)
  , sbgccf_hr_col                               :: !(VU.Vector Double)  -- ^ (gC/m2/s) total heterotrophic respiration
  , sbgccf_michr_col                            :: !(VU.Vector Double)  -- ^ (gC/m2/s) microbial het. resp.
  , sbgccf_cwdhr_col                            :: !(VU.Vector Double)  -- ^ (gC/m2/s) CWD het. resp.
  , sbgccf_lithr_col                            :: !(VU.Vector Double)  -- ^ (gC/m2/s) litter het. resp.
  , sbgccf_somhr_col                            :: !(VU.Vector Double)  -- ^ (gC/m2/s) SOM het. resp.
  , sbgccf_soilc_change_col                     :: !(VU.Vector Double)  -- ^ (gC/m2/s) FUN used soil C
  , sbgccf_fates_litter_flux                    :: !(VU.Vector Double)  -- ^ (gC/m2/s) total litter flux from FATES
  } deriving (Show)

defaultSoilBGCCarbonFluxData :: SoilBGCCarbonFluxData
defaultSoilBGCCarbonFluxData = SoilBGCCarbonFluxData
  { sbgccf_somc_fire_col = VU.empty
  , sbgccf_decomp_cpools_sourcesink_col = VU.empty
  , sbgccf_decomp_cascade_hr_vr_col = VU.empty
  , sbgccf_decomp_cascade_hr_col = VU.empty
  , sbgccf_decomp_cascade_ctransfer_vr_col = VU.empty
  , sbgccf_decomp_cascade_ctransfer_col = VU.empty
  , sbgccf_c_overflow_vr = VU.empty
  , sbgccf_rf_decomp_cascade_col = VU.empty
  , sbgccf_pathfrac_decomp_cascade_col = VU.empty
  , sbgccf_decomp_k_col = VU.empty
  , sbgccf_cn_col = VU.empty
  , sbgccf_litr_lig_c_to_n_col = VU.empty
  , sbgccf_hr_vr_col = VU.empty
  , sbgccf_o_scalar_col = VU.empty
  , sbgccf_w_scalar_col = VU.empty
  , sbgccf_t_scalar_col = VU.empty
  , sbgccf_som_c_leached_col = VU.empty
  , sbgccf_decomp_cpools_leached_col = VU.empty
  , sbgccf_decomp_cpools_transport_tendency_col = VU.empty
  , sbgccf_phr_vr_col = VU.empty, sbgccf_fphr_col = VU.empty
  , sbgccf_hr_col = VU.empty, sbgccf_michr_col = VU.empty
  , sbgccf_cwdhr_col = VU.empty, sbgccf_lithr_col = VU.empty
  , sbgccf_somhr_col = VU.empty, sbgccf_soilc_change_col = VU.empty
  , sbgccf_fates_litter_flux = VU.empty
  }
