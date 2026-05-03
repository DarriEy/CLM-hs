-- | Soil biogeochemistry nitrogen flux data structures.
-- Fortran: SoilBiogeochemNitrogenFluxType.F90 — decomposition N fluxes,
-- mineralization/immobilization, nitrification/denitrification, leaching.
module CLM.Types.SoilBGCNitrogenFluxData
  ( SoilBGCNitrogenFluxData(..)
  , defaultSoilBGCNitrogenFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Soil biogeochemistry nitrogen flux (SoA layout).
-- Preserves Fortran variable names with @sbgcnf_@ prefix.
-- 2D/3D arrays are flattened to 1D vectors with dimension comments.
-- Matrix-CN sparse index fields are omitted; add if needed.
data SoilBGCNitrogenFluxData = SoilBGCNitrogenFluxData
  { -- Deposition fluxes (col)
    sbgcnf_ndep_to_sminn_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) atmospheric N deposition
  , sbgcnf_nfix_to_sminn_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) symbiotic/asymbiotic N fixation
  , sbgcnf_ffix_to_sminn_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) free living N fixation
  , sbgcnf_fert_to_sminn_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) fertilizer N to soil mineral N
  , sbgcnf_soyfixn_to_sminn_col                 :: !(VU.Vector Double)  -- ^ (gN/m2/s) soybean fixation
    -- Decomposition cascade fluxes (col * nlev * ntrans, flattened)
  , sbgcnf_decomp_cascade_ntransfer_vr_col      :: !(VU.Vector Double)  -- ^ (gN/m3/s) vr N transfer along decomp cascade
  , sbgcnf_decomp_cascade_ntransfer_col         :: !(VU.Vector Double)  -- ^ (gN/m2/s) vert-int N transfer
  , sbgcnf_decomp_cascade_sminn_flux_vr_col     :: !(VU.Vector Double)  -- ^ (gN/m3/s) vr mineral N flux along decomp cascade
  , sbgcnf_decomp_cascade_sminn_flux_col        :: !(VU.Vector Double)  -- ^ (gN/m2/s) vert-int mineral N flux
    -- Immobilization / mineralization (col * nlevdecomp_full, flattened)
  , sbgcnf_potential_immob_vr_col               :: !(VU.Vector Double)  -- ^ (gN/m3/s) potential N immobilization vr
  , sbgcnf_potential_immob_col                   :: !(VU.Vector Double)  -- ^ (gN/m2/s) potential N immobilization
  , sbgcnf_actual_immob_vr_col                  :: !(VU.Vector Double)  -- ^ (gN/m3/s) actual N immobilization vr
  , sbgcnf_actual_immob_col                      :: !(VU.Vector Double)  -- ^ (gN/m2/s) actual N immobilization
  , sbgcnf_sminn_to_plant_vr_col                :: !(VU.Vector Double)  -- ^ (gN/m3/s) plant uptake of soil mineral N vr
  , sbgcnf_sminn_to_plant_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) plant uptake of soil mineral N
  , sbgcnf_supplement_to_sminn_vr_col           :: !(VU.Vector Double)  -- ^ (gN/m3/s) supplemental N supply vr
  , sbgcnf_supplement_to_sminn_col               :: !(VU.Vector Double)  -- ^ (gN/m2/s) supplemental N supply
  , sbgcnf_gross_nmin_vr_col                    :: !(VU.Vector Double)  -- ^ (gN/m3/s) gross N mineralization vr
  , sbgcnf_gross_nmin_col                        :: !(VU.Vector Double)  -- ^ (gN/m2/s) gross N mineralization
  , sbgcnf_net_nmin_vr_col                      :: !(VU.Vector Double)  -- ^ (gN/m3/s) net N mineralization vr
  , sbgcnf_net_nmin_col                          :: !(VU.Vector Double)  -- ^ (gN/m2/s) net N mineralization
  , sbgcnf_sminn_to_plant_fun_col               :: !(VU.Vector Double)  -- ^ (gN/m2/s) total soil N uptake of FUN
  , sbgcnf_sminn_to_plant_fun_vr_col            :: !(VU.Vector Double)  -- ^ (gN/m2/s) total layer soil N uptake of FUN
  , sbgcnf_sminn_to_plant_fun_no3_vr_col        :: !(VU.Vector Double)  -- ^ (gN/m2/s) total layer NO3 uptake of FUN
  , sbgcnf_sminn_to_plant_fun_nh4_vr_col        :: !(VU.Vector Double)  -- ^ (gN/m2/s) total layer NH4 uptake of FUN
    -- Nitrification / denitrification (col * nlevdecomp_full, flattened)
  , sbgcnf_f_nit_vr_col                         :: !(VU.Vector Double)  -- ^ (gN/m3/s) soil nitrification flux vr
  , sbgcnf_f_denit_vr_col                       :: !(VU.Vector Double)  -- ^ (gN/m3/s) soil denitrification flux vr
  , sbgcnf_f_nit_col                             :: !(VU.Vector Double)  -- ^ (gN/m2/s) soil nitrification flux
  , sbgcnf_f_denit_col                           :: !(VU.Vector Double)  -- ^ (gN/m2/s) soil denitrification flux
  , sbgcnf_pot_f_nit_vr_col                     :: !(VU.Vector Double)  -- ^ (gN/m3/s) potential nitrification vr
  , sbgcnf_pot_f_denit_vr_col                   :: !(VU.Vector Double)  -- ^ (gN/m3/s) potential denitrification vr
  , sbgcnf_pot_f_nit_col                         :: !(VU.Vector Double)  -- ^ (gN/m2/s) potential nitrification
  , sbgcnf_pot_f_denit_col                       :: !(VU.Vector Double)  -- ^ (gN/m2/s) potential denitrification
  , sbgcnf_f_n2o_denit_vr_col                   :: !(VU.Vector Double)  -- ^ (gN/m3/s) N2O from denitrification vr
  , sbgcnf_f_n2o_denit_col                       :: !(VU.Vector Double)  -- ^ (gN/m2/s) N2O from denitrification
  , sbgcnf_f_n2o_nit_vr_col                     :: !(VU.Vector Double)  -- ^ (gN/m3/s) N2O from nitrification vr
  , sbgcnf_f_n2o_nit_col                         :: !(VU.Vector Double)  -- ^ (gN/m2/s) N2O from nitrification
  , sbgcnf_n2_n2o_ratio_denit_vr_col            :: !(VU.Vector Double)  -- ^ (gN/gN) N2:N2O ratio from denitrification
    -- NO3/NH4 immobilization / uptake (col * nlevdecomp_full, flattened)
  , sbgcnf_actual_immob_no3_vr_col              :: !(VU.Vector Double)  -- ^ (gN/m3/s) actual immobilization of NO3 vr
  , sbgcnf_actual_immob_nh4_vr_col              :: !(VU.Vector Double)  -- ^ (gN/m3/s) actual immobilization of NH4 vr
  , sbgcnf_smin_no3_to_plant_vr_col             :: !(VU.Vector Double)  -- ^ (gN/m3/s) plant uptake of NO3 vr
  , sbgcnf_smin_nh4_to_plant_vr_col             :: !(VU.Vector Double)  -- ^ (gN/m3/s) plant uptake of NH4 vr
  , sbgcnf_actual_immob_no3_col                  :: !(VU.Vector Double)  -- ^ (gN/m2/s) actual immobilization of NO3
  , sbgcnf_actual_immob_nh4_col                  :: !(VU.Vector Double)  -- ^ (gN/m2/s) actual immobilization of NH4
  , sbgcnf_smin_no3_to_plant_col                 :: !(VU.Vector Double)  -- ^ (gN/m2/s) plant uptake of NO3
  , sbgcnf_smin_nh4_to_plant_col                 :: !(VU.Vector Double)  -- ^ (gN/m2/s) plant uptake of NH4
    -- NO3 leaching / runoff (col * nlevdecomp_full, flattened)
  , sbgcnf_smin_no3_leached_vr_col              :: !(VU.Vector Double)  -- ^ (gN/m3/s) NO3 loss to leaching vr
  , sbgcnf_smin_no3_leached_col                  :: !(VU.Vector Double)  -- ^ (gN/m2/s) NO3 loss to leaching
  , sbgcnf_smin_no3_runoff_vr_col               :: !(VU.Vector Double)  -- ^ (gN/m3/s) NO3 loss to runoff vr
  , sbgcnf_smin_no3_runoff_col                   :: !(VU.Vector Double)  -- ^ (gN/m2/s) NO3 loss to runoff
    -- Non-nitrif_denitrif denitrification (col * nlev * ntrans, flattened)
  , sbgcnf_sminn_to_denit_decomp_cascade_vr_col :: !(VU.Vector Double)  -- ^ (gN/m3/s) denitrif along decomp cascade vr
  , sbgcnf_sminn_to_denit_decomp_cascade_col    :: !(VU.Vector Double)  -- ^ (gN/m2/s) denitrif along decomp cascade
  , sbgcnf_sminn_to_denit_excess_vr_col         :: !(VU.Vector Double)  -- ^ (gN/m3/s) excess denitrification vr
  , sbgcnf_sminn_to_denit_excess_col             :: !(VU.Vector Double)  -- ^ (gN/m2/s) excess denitrification
    -- Non-nitrif_denitrif leaching (col * nlevdecomp_full, flattened)
  , sbgcnf_sminn_leached_vr_col                 :: !(VU.Vector Double)  -- ^ (gN/m3/s) sminn leaching vr
  , sbgcnf_sminn_leached_col                     :: !(VU.Vector Double)  -- ^ (gN/m2/s) sminn leaching
    -- Summary (diagnostic) flux variables (col)
  , sbgcnf_denit_col                             :: !(VU.Vector Double)  -- ^ (gN/m2/s) total denitrification
  , sbgcnf_ninputs_col                           :: !(VU.Vector Double)  -- ^ (gN/m2/s) column N inputs
  , sbgcnf_noutputs_col                          :: !(VU.Vector Double)  -- ^ (gN/m2/s) column N outputs
  , sbgcnf_som_n_leached_col                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) total SOM N loss from vert transport
  , sbgcnf_decomp_npools_leached_col            :: !(VU.Vector Double)  -- ^ (gN/m2/s) N loss from vert transport per pool
  , sbgcnf_decomp_npools_transport_tendency_col :: !(VU.Vector Double)  -- ^ (gN/m3/s) N tendency due to vert transport
  , sbgcnf_decomp_npools_sourcesink_col         :: !(VU.Vector Double)  -- ^ (gN/m3) change in decomposing N pools
  , sbgcnf_fates_litter_flux                    :: !(VU.Vector Double)  -- ^ (gN/m2/s) total litter flux from FATES
  } deriving (Show)

defaultSoilBGCNitrogenFluxData :: SoilBGCNitrogenFluxData
defaultSoilBGCNitrogenFluxData = SoilBGCNitrogenFluxData
  { sbgcnf_ndep_to_sminn_col = VU.empty, sbgcnf_nfix_to_sminn_col = VU.empty
  , sbgcnf_ffix_to_sminn_col = VU.empty, sbgcnf_fert_to_sminn_col = VU.empty
  , sbgcnf_soyfixn_to_sminn_col = VU.empty
  , sbgcnf_decomp_cascade_ntransfer_vr_col = VU.empty
  , sbgcnf_decomp_cascade_ntransfer_col = VU.empty
  , sbgcnf_decomp_cascade_sminn_flux_vr_col = VU.empty
  , sbgcnf_decomp_cascade_sminn_flux_col = VU.empty
  , sbgcnf_potential_immob_vr_col = VU.empty
  , sbgcnf_potential_immob_col = VU.empty
  , sbgcnf_actual_immob_vr_col = VU.empty, sbgcnf_actual_immob_col = VU.empty
  , sbgcnf_sminn_to_plant_vr_col = VU.empty
  , sbgcnf_sminn_to_plant_col = VU.empty
  , sbgcnf_supplement_to_sminn_vr_col = VU.empty
  , sbgcnf_supplement_to_sminn_col = VU.empty
  , sbgcnf_gross_nmin_vr_col = VU.empty, sbgcnf_gross_nmin_col = VU.empty
  , sbgcnf_net_nmin_vr_col = VU.empty, sbgcnf_net_nmin_col = VU.empty
  , sbgcnf_sminn_to_plant_fun_col = VU.empty
  , sbgcnf_sminn_to_plant_fun_vr_col = VU.empty
  , sbgcnf_sminn_to_plant_fun_no3_vr_col = VU.empty
  , sbgcnf_sminn_to_plant_fun_nh4_vr_col = VU.empty
  , sbgcnf_f_nit_vr_col = VU.empty, sbgcnf_f_denit_vr_col = VU.empty
  , sbgcnf_f_nit_col = VU.empty, sbgcnf_f_denit_col = VU.empty
  , sbgcnf_pot_f_nit_vr_col = VU.empty, sbgcnf_pot_f_denit_vr_col = VU.empty
  , sbgcnf_pot_f_nit_col = VU.empty, sbgcnf_pot_f_denit_col = VU.empty
  , sbgcnf_f_n2o_denit_vr_col = VU.empty, sbgcnf_f_n2o_denit_col = VU.empty
  , sbgcnf_f_n2o_nit_vr_col = VU.empty, sbgcnf_f_n2o_nit_col = VU.empty
  , sbgcnf_n2_n2o_ratio_denit_vr_col = VU.empty
  , sbgcnf_actual_immob_no3_vr_col = VU.empty
  , sbgcnf_actual_immob_nh4_vr_col = VU.empty
  , sbgcnf_smin_no3_to_plant_vr_col = VU.empty
  , sbgcnf_smin_nh4_to_plant_vr_col = VU.empty
  , sbgcnf_actual_immob_no3_col = VU.empty
  , sbgcnf_actual_immob_nh4_col = VU.empty
  , sbgcnf_smin_no3_to_plant_col = VU.empty
  , sbgcnf_smin_nh4_to_plant_col = VU.empty
  , sbgcnf_smin_no3_leached_vr_col = VU.empty
  , sbgcnf_smin_no3_leached_col = VU.empty
  , sbgcnf_smin_no3_runoff_vr_col = VU.empty
  , sbgcnf_smin_no3_runoff_col = VU.empty
  , sbgcnf_sminn_to_denit_decomp_cascade_vr_col = VU.empty
  , sbgcnf_sminn_to_denit_decomp_cascade_col = VU.empty
  , sbgcnf_sminn_to_denit_excess_vr_col = VU.empty
  , sbgcnf_sminn_to_denit_excess_col = VU.empty
  , sbgcnf_sminn_leached_vr_col = VU.empty
  , sbgcnf_sminn_leached_col = VU.empty
  , sbgcnf_denit_col = VU.empty, sbgcnf_ninputs_col = VU.empty
  , sbgcnf_noutputs_col = VU.empty, sbgcnf_som_n_leached_col = VU.empty
  , sbgcnf_decomp_npools_leached_col = VU.empty
  , sbgcnf_decomp_npools_transport_tendency_col = VU.empty
  , sbgcnf_decomp_npools_sourcesink_col = VU.empty
  , sbgcnf_fates_litter_flux = VU.empty
  }
