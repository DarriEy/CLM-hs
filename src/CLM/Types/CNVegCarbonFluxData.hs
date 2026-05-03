-- | CN vegetation carbon flux data structures.
-- Fortran: CNVegCarbonFluxType.F90 — carbon flux variables at patch, column, gridcell levels.
module CLM.Types.CNVegCarbonFluxData
  ( CNVegCarbonFluxData(..)
  , defaultCNVegCarbonFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN vegetation carbon flux (SoA layout).
-- Preserves Fortran variable names with @cnvcf_@ prefix.
-- Only the primary physics fields are included; matrix-CN index scalars are omitted.
data CNVegCarbonFluxData = CNVegCarbonFluxData
  { -- Gap mortality (patch)
    cnvcf_m_leafc_to_litter_patch             :: !(VU.Vector Double)
  , cnvcf_m_frootc_to_litter_patch            :: !(VU.Vector Double)
  , cnvcf_m_livestemc_to_litter_patch         :: !(VU.Vector Double)
  , cnvcf_m_deadstemc_to_litter_patch         :: !(VU.Vector Double)
  , cnvcf_m_livecrootc_to_litter_patch        :: !(VU.Vector Double)
  , cnvcf_m_deadcrootc_to_litter_patch        :: !(VU.Vector Double)
    -- Phenology (patch)
  , cnvcf_leafc_xfer_to_leafc_patch           :: !(VU.Vector Double)
  , cnvcf_frootc_xfer_to_frootc_patch         :: !(VU.Vector Double)
  , cnvcf_livestemc_xfer_to_livestemc_patch   :: !(VU.Vector Double)
  , cnvcf_deadstemc_xfer_to_deadstemc_patch   :: !(VU.Vector Double)
  , cnvcf_livecrootc_xfer_to_livecrootc_patch :: !(VU.Vector Double)
  , cnvcf_deadcrootc_xfer_to_deadcrootc_patch :: !(VU.Vector Double)
    -- Litterfall
  , cnvcf_leafc_to_litter_patch               :: !(VU.Vector Double)
  , cnvcf_frootc_to_litter_patch              :: !(VU.Vector Double)
  , cnvcf_livestemc_to_litter_patch           :: !(VU.Vector Double)
    -- Maintenance respiration
  , cnvcf_leaf_mr_patch                       :: !(VU.Vector Double)
  , cnvcf_froot_mr_patch                      :: !(VU.Vector Double)
  , cnvcf_livestem_mr_patch                   :: !(VU.Vector Double)
  , cnvcf_livecroot_mr_patch                  :: !(VU.Vector Double)
    -- Photosynthesis
  , cnvcf_psnsun_to_cpool_patch               :: !(VU.Vector Double)
  , cnvcf_psnshade_to_cpool_patch             :: !(VU.Vector Double)
    -- Allocation from cpool
  , cnvcf_cpool_to_leafc_patch                :: !(VU.Vector Double)
  , cnvcf_cpool_to_frootc_patch               :: !(VU.Vector Double)
  , cnvcf_cpool_to_livestemc_patch            :: !(VU.Vector Double)
  , cnvcf_cpool_to_deadstemc_patch            :: !(VU.Vector Double)
  , cnvcf_cpool_to_livecrootc_patch           :: !(VU.Vector Double)
  , cnvcf_cpool_to_deadcrootc_patch           :: !(VU.Vector Double)
  , cnvcf_cpool_to_gresp_storage_patch        :: !(VU.Vector Double)
    -- Livewood to deadwood turnover
  , cnvcf_livestemc_to_deadstemc_patch        :: !(VU.Vector Double)
  , cnvcf_livecrootc_to_deadcrootc_patch      :: !(VU.Vector Double)
    -- Growth respiration
  , cnvcf_xsmrpool_to_atm_patch              :: !(VU.Vector Double)
  , cnvcf_xsmrpool_to_atm_col                :: !(VU.Vector Double)
    -- Summary fluxes
  , cnvcf_gpp_patch                           :: !(VU.Vector Double)
  , cnvcf_gpp_col                             :: !(VU.Vector Double)
  , cnvcf_npp_patch                           :: !(VU.Vector Double)
  , cnvcf_npp_col                             :: !(VU.Vector Double)
  , cnvcf_ar_patch                            :: !(VU.Vector Double)
  , cnvcf_ar_col                              :: !(VU.Vector Double)
  , cnvcf_mr_patch                            :: !(VU.Vector Double)
  , cnvcf_gr_patch                            :: !(VU.Vector Double)
  , cnvcf_rr_patch                            :: !(VU.Vector Double)
  , cnvcf_rr_col                              :: !(VU.Vector Double)
  , cnvcf_litfall_patch                       :: !(VU.Vector Double)
  , cnvcf_agnpp_patch                         :: !(VU.Vector Double)
  , cnvcf_bgnpp_patch                         :: !(VU.Vector Double)
    -- Fire summary
  , cnvcf_fire_closs_patch                    :: !(VU.Vector Double)
  , cnvcf_fire_closs_col                      :: !(VU.Vector Double)
    -- Annual sums
  , cnvcf_tempsum_npp_patch                   :: !(VU.Vector Double)
  , cnvcf_annsum_npp_patch                    :: !(VU.Vector Double)
  , cnvcf_annsum_npp_col                      :: !(VU.Vector Double)
  , cnvcf_lag_npp_col                         :: !(VU.Vector Double)
  , cnvcf_tempsum_litfall_patch               :: !(VU.Vector Double)
  , cnvcf_annsum_litfall_patch                :: !(VU.Vector Double)
    -- C balance
  , cnvcf_nep_col                             :: !(VU.Vector Double)
  , cnvcf_nbp_grc                             :: !(VU.Vector Double)
  , cnvcf_nee_grc                             :: !(VU.Vector Double)
    -- Column-level decomp (3D flattened)
  , cnvcf_phenology_c_to_litr_c_col          :: !(VU.Vector Double)
  , cnvcf_gap_mortality_c_to_litr_c_col      :: !(VU.Vector Double)
  , cnvcf_gap_mortality_c_to_cwdc_col        :: !(VU.Vector Double)
    -- Harvest
  , cnvcf_crop_harvestc_to_cropprodc_patch    :: !(VU.Vector Double)
  , cnvcf_crop_harvestc_to_cropprodc_col      :: !(VU.Vector Double)
  , cnvcf_wood_harvestc_patch                 :: !(VU.Vector Double)
  , cnvcf_wood_harvestc_col                   :: !(VU.Vector Double)
    -- Dynamic landcover
  , cnvcf_dwt_seedc_to_leaf_patch             :: !(VU.Vector Double)
  , cnvcf_dwt_seedc_to_leaf_grc              :: !(VU.Vector Double)
  , cnvcf_dwt_conv_cflux_patch               :: !(VU.Vector Double)
  , cnvcf_dwt_conv_cflux_grc                 :: !(VU.Vector Double)
  } deriving (Show)

defaultCNVegCarbonFluxData :: CNVegCarbonFluxData
defaultCNVegCarbonFluxData = CNVegCarbonFluxData
  { cnvcf_m_leafc_to_litter_patch = VU.empty, cnvcf_m_frootc_to_litter_patch = VU.empty
  , cnvcf_m_livestemc_to_litter_patch = VU.empty, cnvcf_m_deadstemc_to_litter_patch = VU.empty
  , cnvcf_m_livecrootc_to_litter_patch = VU.empty, cnvcf_m_deadcrootc_to_litter_patch = VU.empty
  , cnvcf_leafc_xfer_to_leafc_patch = VU.empty, cnvcf_frootc_xfer_to_frootc_patch = VU.empty
  , cnvcf_livestemc_xfer_to_livestemc_patch = VU.empty
  , cnvcf_deadstemc_xfer_to_deadstemc_patch = VU.empty
  , cnvcf_livecrootc_xfer_to_livecrootc_patch = VU.empty
  , cnvcf_deadcrootc_xfer_to_deadcrootc_patch = VU.empty
  , cnvcf_leafc_to_litter_patch = VU.empty, cnvcf_frootc_to_litter_patch = VU.empty
  , cnvcf_livestemc_to_litter_patch = VU.empty
  , cnvcf_leaf_mr_patch = VU.empty, cnvcf_froot_mr_patch = VU.empty
  , cnvcf_livestem_mr_patch = VU.empty, cnvcf_livecroot_mr_patch = VU.empty
  , cnvcf_psnsun_to_cpool_patch = VU.empty, cnvcf_psnshade_to_cpool_patch = VU.empty
  , cnvcf_cpool_to_leafc_patch = VU.empty, cnvcf_cpool_to_frootc_patch = VU.empty
  , cnvcf_cpool_to_livestemc_patch = VU.empty, cnvcf_cpool_to_deadstemc_patch = VU.empty
  , cnvcf_cpool_to_livecrootc_patch = VU.empty, cnvcf_cpool_to_deadcrootc_patch = VU.empty
  , cnvcf_cpool_to_gresp_storage_patch = VU.empty
  , cnvcf_livestemc_to_deadstemc_patch = VU.empty
  , cnvcf_livecrootc_to_deadcrootc_patch = VU.empty
  , cnvcf_xsmrpool_to_atm_patch = VU.empty, cnvcf_xsmrpool_to_atm_col = VU.empty
  , cnvcf_gpp_patch = VU.empty, cnvcf_gpp_col = VU.empty
  , cnvcf_npp_patch = VU.empty, cnvcf_npp_col = VU.empty
  , cnvcf_ar_patch = VU.empty, cnvcf_ar_col = VU.empty
  , cnvcf_mr_patch = VU.empty, cnvcf_gr_patch = VU.empty
  , cnvcf_rr_patch = VU.empty, cnvcf_rr_col = VU.empty
  , cnvcf_litfall_patch = VU.empty, cnvcf_agnpp_patch = VU.empty
  , cnvcf_bgnpp_patch = VU.empty
  , cnvcf_fire_closs_patch = VU.empty, cnvcf_fire_closs_col = VU.empty
  , cnvcf_tempsum_npp_patch = VU.empty, cnvcf_annsum_npp_patch = VU.empty
  , cnvcf_annsum_npp_col = VU.empty, cnvcf_lag_npp_col = VU.empty
  , cnvcf_tempsum_litfall_patch = VU.empty, cnvcf_annsum_litfall_patch = VU.empty
  , cnvcf_nep_col = VU.empty, cnvcf_nbp_grc = VU.empty, cnvcf_nee_grc = VU.empty
  , cnvcf_phenology_c_to_litr_c_col = VU.empty
  , cnvcf_gap_mortality_c_to_litr_c_col = VU.empty
  , cnvcf_gap_mortality_c_to_cwdc_col = VU.empty
  , cnvcf_crop_harvestc_to_cropprodc_patch = VU.empty
  , cnvcf_crop_harvestc_to_cropprodc_col = VU.empty
  , cnvcf_wood_harvestc_patch = VU.empty, cnvcf_wood_harvestc_col = VU.empty
  , cnvcf_dwt_seedc_to_leaf_patch = VU.empty, cnvcf_dwt_seedc_to_leaf_grc = VU.empty
  , cnvcf_dwt_conv_cflux_patch = VU.empty, cnvcf_dwt_conv_cflux_grc = VU.empty
  }
