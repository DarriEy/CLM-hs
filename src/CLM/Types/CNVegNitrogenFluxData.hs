-- | CN vegetation nitrogen flux data structures.
-- Fortran: CNVegNitrogenFluxType.F90 — nitrogen flux variables at patch, column, gridcell levels.
module CLM.Types.CNVegNitrogenFluxData
  ( CNVegNitrogenFluxData(..)
  , defaultCNVegNitrogenFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN vegetation nitrogen flux (SoA layout).
-- Preserves Fortran variable names with @cnvnf_@ prefix.
-- Only the primary physics fields are included; matrix-CN index scalars are omitted.
data CNVegNitrogenFluxData = CNVegNitrogenFluxData
  { -- Gap mortality (patch)
    cnvnf_m_leafn_to_litter_patch           :: !(VU.Vector Double)
  , cnvnf_m_frootn_to_litter_patch          :: !(VU.Vector Double)
  , cnvnf_m_livestemn_to_litter_patch       :: !(VU.Vector Double)
  , cnvnf_m_deadstemn_to_litter_patch       :: !(VU.Vector Double)
  , cnvnf_m_livecrootn_to_litter_patch      :: !(VU.Vector Double)
  , cnvnf_m_deadcrootn_to_litter_patch      :: !(VU.Vector Double)
  , cnvnf_m_retransn_to_litter_patch        :: !(VU.Vector Double)
    -- Harvest (patch)
  , cnvnf_crop_harvestn_to_cropprodn_patch  :: !(VU.Vector Double)
  , cnvnf_crop_harvestn_to_cropprodn_col    :: !(VU.Vector Double)
    -- Fire summary
  , cnvnf_fire_nloss_patch                  :: !(VU.Vector Double)
  , cnvnf_fire_nloss_col                    :: !(VU.Vector Double)
    -- Phenology (patch)
  , cnvnf_leafn_xfer_to_leafn_patch         :: !(VU.Vector Double)
  , cnvnf_frootn_xfer_to_frootn_patch       :: !(VU.Vector Double)
  , cnvnf_livestemn_xfer_to_livestemn_patch :: !(VU.Vector Double)
  , cnvnf_deadstemn_xfer_to_deadstemn_patch :: !(VU.Vector Double)
  , cnvnf_livecrootn_xfer_to_livecrootn_patch :: !(VU.Vector Double)
  , cnvnf_deadcrootn_xfer_to_deadcrootn_patch :: !(VU.Vector Double)
    -- Litterfall
  , cnvnf_leafn_to_litter_patch             :: !(VU.Vector Double)
  , cnvnf_leafn_to_retransn_patch           :: !(VU.Vector Double)
  , cnvnf_frootn_to_litter_patch            :: !(VU.Vector Double)
  , cnvnf_frootn_to_retransn_patch          :: !(VU.Vector Double)
  , cnvnf_livestemn_to_litter_patch         :: !(VU.Vector Double)
    -- Allocation
  , cnvnf_retransn_to_npool_patch           :: !(VU.Vector Double)
  , cnvnf_sminn_to_npool_patch              :: !(VU.Vector Double)
  , cnvnf_npool_to_leafn_patch              :: !(VU.Vector Double)
  , cnvnf_npool_to_frootn_patch             :: !(VU.Vector Double)
  , cnvnf_npool_to_livestemn_patch          :: !(VU.Vector Double)
  , cnvnf_npool_to_deadstemn_patch          :: !(VU.Vector Double)
  , cnvnf_npool_to_livecrootn_patch         :: !(VU.Vector Double)
  , cnvnf_npool_to_deadcrootn_patch         :: !(VU.Vector Double)
    -- Livewood to deadwood turnover
  , cnvnf_livestemn_to_deadstemn_patch      :: !(VU.Vector Double)
  , cnvnf_livestemn_to_retransn_patch       :: !(VU.Vector Double)
  , cnvnf_livecrootn_to_deadcrootn_patch    :: !(VU.Vector Double)
  , cnvnf_livecrootn_to_retransn_patch      :: !(VU.Vector Double)
    -- Fertilizer / soyfix
  , cnvnf_fert_patch                        :: !(VU.Vector Double)
  , cnvnf_fert_counter_patch                :: !(VU.Vector Double)
  , cnvnf_soyfixn_patch                     :: !(VU.Vector Double)
    -- Summary
  , cnvnf_ndeploy_patch                     :: !(VU.Vector Double)
  , cnvnf_plant_ndemand_patch               :: !(VU.Vector Double)
  , cnvnf_plant_nalloc_patch                :: !(VU.Vector Double)
  , cnvnf_wood_harvestn_patch               :: !(VU.Vector Double)
  , cnvnf_wood_harvestn_col                 :: !(VU.Vector Double)
    -- Column-level decomp (3D flattened)
  , cnvnf_phenology_n_to_litr_n_col         :: !(VU.Vector Double)
  , cnvnf_gap_mortality_n_to_litr_n_col     :: !(VU.Vector Double)
  , cnvnf_gap_mortality_n_to_cwdn_col       :: !(VU.Vector Double)
    -- Dynamic landcover
  , cnvnf_dwt_seedn_to_leaf_patch           :: !(VU.Vector Double)
  , cnvnf_dwt_seedn_to_leaf_grc            :: !(VU.Vector Double)
  , cnvnf_dwt_conv_nflux_patch             :: !(VU.Vector Double)
  , cnvnf_dwt_conv_nflux_grc               :: !(VU.Vector Double)
    -- FUN fluxes
  , cnvnf_sminn_to_plant_fun_patch          :: !(VU.Vector Double)
  , cnvnf_Nfix_patch                        :: !(VU.Vector Double)
  , cnvnf_Nuptake_patch                     :: !(VU.Vector Double)
  , cnvnf_Nretrans_patch                    :: !(VU.Vector Double)
    -- Crop
  , cnvnf_crop_seedn_to_leaf_patch          :: !(VU.Vector Double)
  } deriving (Show)

defaultCNVegNitrogenFluxData :: CNVegNitrogenFluxData
defaultCNVegNitrogenFluxData = CNVegNitrogenFluxData
  { cnvnf_m_leafn_to_litter_patch = VU.empty, cnvnf_m_frootn_to_litter_patch = VU.empty
  , cnvnf_m_livestemn_to_litter_patch = VU.empty, cnvnf_m_deadstemn_to_litter_patch = VU.empty
  , cnvnf_m_livecrootn_to_litter_patch = VU.empty, cnvnf_m_deadcrootn_to_litter_patch = VU.empty
  , cnvnf_m_retransn_to_litter_patch = VU.empty
  , cnvnf_crop_harvestn_to_cropprodn_patch = VU.empty
  , cnvnf_crop_harvestn_to_cropprodn_col = VU.empty
  , cnvnf_fire_nloss_patch = VU.empty, cnvnf_fire_nloss_col = VU.empty
  , cnvnf_leafn_xfer_to_leafn_patch = VU.empty, cnvnf_frootn_xfer_to_frootn_patch = VU.empty
  , cnvnf_livestemn_xfer_to_livestemn_patch = VU.empty
  , cnvnf_deadstemn_xfer_to_deadstemn_patch = VU.empty
  , cnvnf_livecrootn_xfer_to_livecrootn_patch = VU.empty
  , cnvnf_deadcrootn_xfer_to_deadcrootn_patch = VU.empty
  , cnvnf_leafn_to_litter_patch = VU.empty, cnvnf_leafn_to_retransn_patch = VU.empty
  , cnvnf_frootn_to_litter_patch = VU.empty, cnvnf_frootn_to_retransn_patch = VU.empty
  , cnvnf_livestemn_to_litter_patch = VU.empty
  , cnvnf_retransn_to_npool_patch = VU.empty, cnvnf_sminn_to_npool_patch = VU.empty
  , cnvnf_npool_to_leafn_patch = VU.empty, cnvnf_npool_to_frootn_patch = VU.empty
  , cnvnf_npool_to_livestemn_patch = VU.empty, cnvnf_npool_to_deadstemn_patch = VU.empty
  , cnvnf_npool_to_livecrootn_patch = VU.empty, cnvnf_npool_to_deadcrootn_patch = VU.empty
  , cnvnf_livestemn_to_deadstemn_patch = VU.empty
  , cnvnf_livestemn_to_retransn_patch = VU.empty
  , cnvnf_livecrootn_to_deadcrootn_patch = VU.empty
  , cnvnf_livecrootn_to_retransn_patch = VU.empty
  , cnvnf_fert_patch = VU.empty, cnvnf_fert_counter_patch = VU.empty
  , cnvnf_soyfixn_patch = VU.empty
  , cnvnf_ndeploy_patch = VU.empty, cnvnf_plant_ndemand_patch = VU.empty
  , cnvnf_plant_nalloc_patch = VU.empty
  , cnvnf_wood_harvestn_patch = VU.empty, cnvnf_wood_harvestn_col = VU.empty
  , cnvnf_phenology_n_to_litr_n_col = VU.empty
  , cnvnf_gap_mortality_n_to_litr_n_col = VU.empty
  , cnvnf_gap_mortality_n_to_cwdn_col = VU.empty
  , cnvnf_dwt_seedn_to_leaf_patch = VU.empty, cnvnf_dwt_seedn_to_leaf_grc = VU.empty
  , cnvnf_dwt_conv_nflux_patch = VU.empty, cnvnf_dwt_conv_nflux_grc = VU.empty
  , cnvnf_sminn_to_plant_fun_patch = VU.empty
  , cnvnf_Nfix_patch = VU.empty, cnvnf_Nuptake_patch = VU.empty
  , cnvnf_Nretrans_patch = VU.empty, cnvnf_crop_seedn_to_leaf_patch = VU.empty
  }
