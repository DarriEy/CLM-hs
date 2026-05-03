-- | CN vegetation state data structures.
-- Fortran: CNVegStateType.F90 — crop model, fire, phenology, allocation, accumulators.
module CLM.Types.CNVegStateData
  ( CNVegStateData(..)
  , defaultCNVegStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN vegetation state (SoA layout).
-- Preserves Fortran variable names with @cnvs_@ prefix.
data CNVegStateData = CNVegStateData
  { -- Patch-level integer
    cnvs_burndate_patch              :: !(VU.Vector Int)
  , cnvs_peaklai_patch               :: !(VU.Vector Int)
  , cnvs_idop_patch                  :: !(VU.Vector Int)
  , cnvs_iyop_patch                  :: !(VU.Vector Int)
    -- Patch-level real
  , cnvs_dwt_smoothed_patch          :: !(VU.Vector Double)
  , cnvs_hdidx_patch                 :: !(VU.Vector Double)
  , cnvs_cumvd_patch                 :: !(VU.Vector Double)
  , cnvs_gddmaturity_patch           :: !(VU.Vector Double)
  , cnvs_gddmaturity_thisyr          :: !(VU.Vector Double) -- ^ (patch * mxharvests, flattened)
  , cnvs_huileaf_patch               :: !(VU.Vector Double)
  , cnvs_huigrain_patch              :: !(VU.Vector Double)
  , cnvs_aleafi_patch                :: !(VU.Vector Double)
  , cnvs_astemi_patch                :: !(VU.Vector Double)
  , cnvs_aleaf_patch                 :: !(VU.Vector Double)
  , cnvs_astem_patch                 :: !(VU.Vector Double)
  , cnvs_aroot_patch                 :: !(VU.Vector Double)
  , cnvs_arepr_patch                 :: !(VU.Vector Double) -- ^ (patch * nrepr, flattened)
  , cnvs_aleaf_n_patch               :: !(VU.Vector Double)
  , cnvs_astem_n_patch               :: !(VU.Vector Double)
  , cnvs_aroot_n_patch               :: !(VU.Vector Double)
  , cnvs_arepr_n_patch               :: !(VU.Vector Double) -- ^ (patch * nrepr, flattened)
  , cnvs_htmx_patch                  :: !(VU.Vector Double)
    -- Column-level
  , cnvs_lgdp_col                    :: !(VU.Vector Double)
  , cnvs_lgdp1_col                   :: !(VU.Vector Double)
  , cnvs_lpop_col                    :: !(VU.Vector Double)
  , cnvs_tempavg_t2m_patch           :: !(VU.Vector Double)
  , cnvs_annavg_t2m_patch            :: !(VU.Vector Double)
  , cnvs_annavg_t2m_col              :: !(VU.Vector Double)
  , cnvs_annsum_counter_col          :: !(VU.Vector Double)
    -- Fire column-level
  , cnvs_nfire_col                   :: !(VU.Vector Double)
  , cnvs_fsr_col                     :: !(VU.Vector Double)
  , cnvs_fd_col                      :: !(VU.Vector Double)
  , cnvs_lfc_col                     :: !(VU.Vector Double)
  , cnvs_lfc2_col                    :: !(VU.Vector Double)
  , cnvs_dtrotr_col                  :: !(VU.Vector Double)
  , cnvs_trotr1_col                  :: !(VU.Vector Double)
  , cnvs_trotr2_col                  :: !(VU.Vector Double)
  , cnvs_cropf_col                   :: !(VU.Vector Double)
  , cnvs_baf_crop_col                :: !(VU.Vector Double)
  , cnvs_baf_peatf_col               :: !(VU.Vector Double)
  , cnvs_fbac_col                    :: !(VU.Vector Double)
  , cnvs_fbac1_col                   :: !(VU.Vector Double)
  , cnvs_wtlf_col                    :: !(VU.Vector Double)
  , cnvs_lfwt_col                    :: !(VU.Vector Double)
  , cnvs_farea_burned_col            :: !(VU.Vector Double)
    -- Phenology flags
  , cnvs_dormant_flag_patch          :: !(VU.Vector Double)
  , cnvs_days_active_patch           :: !(VU.Vector Double)
  , cnvs_onset_flag_patch            :: !(VU.Vector Double)
  , cnvs_onset_counter_patch         :: !(VU.Vector Double)
  , cnvs_onset_gddflag_patch         :: !(VU.Vector Double)
  , cnvs_onset_fdd_patch             :: !(VU.Vector Double)
  , cnvs_onset_gdd_patch             :: !(VU.Vector Double)
  , cnvs_onset_swi_patch             :: !(VU.Vector Double)
  , cnvs_offset_flag_patch           :: !(VU.Vector Double)
  , cnvs_offset_counter_patch        :: !(VU.Vector Double)
  , cnvs_offset_fdd_patch            :: !(VU.Vector Double)
  , cnvs_offset_swi_patch            :: !(VU.Vector Double)
  , cnvs_grain_flag_patch            :: !(VU.Vector Double)
  , cnvs_lgsf_patch                  :: !(VU.Vector Double)
  , cnvs_bglfr_patch                 :: !(VU.Vector Double)
  , cnvs_bgtr_patch                  :: !(VU.Vector Double)
    -- Allometry and GPP accumulators
  , cnvs_c_allometry_patch           :: !(VU.Vector Double)
  , cnvs_n_allometry_patch           :: !(VU.Vector Double)
  , cnvs_tempsum_potential_gpp_patch :: !(VU.Vector Double)
  , cnvs_annsum_potential_gpp_patch  :: !(VU.Vector Double)
  , cnvs_tempmax_retransn_patch      :: !(VU.Vector Double)
  , cnvs_annmax_retransn_patch       :: !(VU.Vector Double)
  , cnvs_downreg_patch               :: !(VU.Vector Double)
  , cnvs_leafcn_offset_patch         :: !(VU.Vector Double)
  , cnvs_plantCN_patch               :: !(VU.Vector Double)
  } deriving (Show)

defaultCNVegStateData :: CNVegStateData
defaultCNVegStateData = CNVegStateData
  { cnvs_burndate_patch = VU.empty, cnvs_peaklai_patch = VU.empty
  , cnvs_idop_patch = VU.empty, cnvs_iyop_patch = VU.empty
  , cnvs_dwt_smoothed_patch = VU.empty, cnvs_hdidx_patch = VU.empty
  , cnvs_cumvd_patch = VU.empty, cnvs_gddmaturity_patch = VU.empty
  , cnvs_gddmaturity_thisyr = VU.empty, cnvs_huileaf_patch = VU.empty
  , cnvs_huigrain_patch = VU.empty, cnvs_aleafi_patch = VU.empty
  , cnvs_astemi_patch = VU.empty, cnvs_aleaf_patch = VU.empty
  , cnvs_astem_patch = VU.empty, cnvs_aroot_patch = VU.empty
  , cnvs_arepr_patch = VU.empty, cnvs_aleaf_n_patch = VU.empty
  , cnvs_astem_n_patch = VU.empty, cnvs_aroot_n_patch = VU.empty
  , cnvs_arepr_n_patch = VU.empty, cnvs_htmx_patch = VU.empty
  , cnvs_lgdp_col = VU.empty, cnvs_lgdp1_col = VU.empty
  , cnvs_lpop_col = VU.empty, cnvs_tempavg_t2m_patch = VU.empty
  , cnvs_annavg_t2m_patch = VU.empty, cnvs_annavg_t2m_col = VU.empty
  , cnvs_annsum_counter_col = VU.empty, cnvs_nfire_col = VU.empty
  , cnvs_fsr_col = VU.empty, cnvs_fd_col = VU.empty
  , cnvs_lfc_col = VU.empty, cnvs_lfc2_col = VU.empty
  , cnvs_dtrotr_col = VU.empty, cnvs_trotr1_col = VU.empty
  , cnvs_trotr2_col = VU.empty, cnvs_cropf_col = VU.empty
  , cnvs_baf_crop_col = VU.empty, cnvs_baf_peatf_col = VU.empty
  , cnvs_fbac_col = VU.empty, cnvs_fbac1_col = VU.empty
  , cnvs_wtlf_col = VU.empty, cnvs_lfwt_col = VU.empty
  , cnvs_farea_burned_col = VU.empty
  , cnvs_dormant_flag_patch = VU.empty, cnvs_days_active_patch = VU.empty
  , cnvs_onset_flag_patch = VU.empty, cnvs_onset_counter_patch = VU.empty
  , cnvs_onset_gddflag_patch = VU.empty, cnvs_onset_fdd_patch = VU.empty
  , cnvs_onset_gdd_patch = VU.empty, cnvs_onset_swi_patch = VU.empty
  , cnvs_offset_flag_patch = VU.empty, cnvs_offset_counter_patch = VU.empty
  , cnvs_offset_fdd_patch = VU.empty, cnvs_offset_swi_patch = VU.empty
  , cnvs_grain_flag_patch = VU.empty, cnvs_lgsf_patch = VU.empty
  , cnvs_bglfr_patch = VU.empty, cnvs_bgtr_patch = VU.empty
  , cnvs_c_allometry_patch = VU.empty, cnvs_n_allometry_patch = VU.empty
  , cnvs_tempsum_potential_gpp_patch = VU.empty
  , cnvs_annsum_potential_gpp_patch = VU.empty
  , cnvs_tempmax_retransn_patch = VU.empty
  , cnvs_annmax_retransn_patch = VU.empty
  , cnvs_downreg_patch = VU.empty, cnvs_leafcn_offset_patch = VU.empty
  , cnvs_plantCN_patch = VU.empty
  }
