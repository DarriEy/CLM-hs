-- | CN vegetation carbon state data structures.
-- Fortran: CNVegCarbonStateType.F90 — carbon pools at patch, column, gridcell levels.
module CLM.Types.CNVegCarbonStateData
  ( CNVegCarbonStateData(..)
  , defaultCNVegCarbonStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN vegetation carbon state (SoA layout).
-- Preserves Fortran variable names with @cnvcs_@ prefix.
-- Matrix/2D fields are flattened to 1D vectors.
data CNVegCarbonStateData = CNVegCarbonStateData
  { cnvcs_species                     :: !Int
    -- Patch 2D (flattened patch * nrepr)
  , cnvcs_reproductivec_patch         :: !(VU.Vector Double)
  , cnvcs_reproductivec_storage_patch :: !(VU.Vector Double)
  , cnvcs_reproductivec_xfer_patch    :: !(VU.Vector Double)
    -- Patch 1D core pools
  , cnvcs_leafc_patch                 :: !(VU.Vector Double)
  , cnvcs_leafc_storage_patch         :: !(VU.Vector Double)
  , cnvcs_leafc_xfer_patch            :: !(VU.Vector Double)
  , cnvcs_leafc_storage_xfer_acc_patch :: !(VU.Vector Double)
  , cnvcs_storage_cdemand_patch       :: !(VU.Vector Double)
  , cnvcs_frootc_patch                :: !(VU.Vector Double)
  , cnvcs_frootc_storage_patch        :: !(VU.Vector Double)
  , cnvcs_frootc_xfer_patch           :: !(VU.Vector Double)
  , cnvcs_livestemc_patch             :: !(VU.Vector Double)
  , cnvcs_livestemc_storage_patch     :: !(VU.Vector Double)
  , cnvcs_livestemc_xfer_patch        :: !(VU.Vector Double)
  , cnvcs_deadstemc_patch             :: !(VU.Vector Double)
  , cnvcs_deadstemc_storage_patch     :: !(VU.Vector Double)
  , cnvcs_deadstemc_xfer_patch        :: !(VU.Vector Double)
  , cnvcs_livecrootc_patch            :: !(VU.Vector Double)
  , cnvcs_livecrootc_storage_patch    :: !(VU.Vector Double)
  , cnvcs_livecrootc_xfer_patch       :: !(VU.Vector Double)
  , cnvcs_deadcrootc_patch            :: !(VU.Vector Double)
  , cnvcs_deadcrootc_storage_patch    :: !(VU.Vector Double)
  , cnvcs_deadcrootc_xfer_patch       :: !(VU.Vector Double)
  , cnvcs_gresp_storage_patch         :: !(VU.Vector Double)
  , cnvcs_gresp_xfer_patch            :: !(VU.Vector Double)
  , cnvcs_cpool_patch                 :: !(VU.Vector Double)
  , cnvcs_xsmrpool_patch              :: !(VU.Vector Double)
  , cnvcs_xsmrpool_loss_patch         :: !(VU.Vector Double)
  , cnvcs_ctrunc_patch                :: !(VU.Vector Double)
  , cnvcs_woodc_patch                 :: !(VU.Vector Double)
  , cnvcs_leafcmax_patch              :: !(VU.Vector Double)
  , cnvcs_cropseedc_deficit_patch     :: !(VU.Vector Double)
    -- Column-level
  , cnvcs_rootc_col                   :: !(VU.Vector Double)
  , cnvcs_leafc_col                   :: !(VU.Vector Double)
  , cnvcs_deadstemc_col               :: !(VU.Vector Double)
  , cnvcs_fuelc_col                   :: !(VU.Vector Double)
  , cnvcs_fuelc_crop_col              :: !(VU.Vector Double)
    -- Gridcell-level
  , cnvcs_seedc_grc                   :: !(VU.Vector Double)
    -- Summary diagnostics
  , cnvcs_dispvegc_patch              :: !(VU.Vector Double)
  , cnvcs_storvegc_patch              :: !(VU.Vector Double)
  , cnvcs_totc_patch                  :: !(VU.Vector Double)
  , cnvcs_totvegc_patch               :: !(VU.Vector Double)
  , cnvcs_totvegc_col                 :: !(VU.Vector Double)
  , cnvcs_totc_p2c_col                :: !(VU.Vector Double)
  } deriving (Show)

defaultCNVegCarbonStateData :: CNVegCarbonStateData
defaultCNVegCarbonStateData = CNVegCarbonStateData
  { cnvcs_species = 0
  , cnvcs_reproductivec_patch = VU.empty, cnvcs_reproductivec_storage_patch = VU.empty
  , cnvcs_reproductivec_xfer_patch = VU.empty
  , cnvcs_leafc_patch = VU.empty, cnvcs_leafc_storage_patch = VU.empty
  , cnvcs_leafc_xfer_patch = VU.empty, cnvcs_leafc_storage_xfer_acc_patch = VU.empty
  , cnvcs_storage_cdemand_patch = VU.empty
  , cnvcs_frootc_patch = VU.empty, cnvcs_frootc_storage_patch = VU.empty
  , cnvcs_frootc_xfer_patch = VU.empty
  , cnvcs_livestemc_patch = VU.empty, cnvcs_livestemc_storage_patch = VU.empty
  , cnvcs_livestemc_xfer_patch = VU.empty
  , cnvcs_deadstemc_patch = VU.empty, cnvcs_deadstemc_storage_patch = VU.empty
  , cnvcs_deadstemc_xfer_patch = VU.empty
  , cnvcs_livecrootc_patch = VU.empty, cnvcs_livecrootc_storage_patch = VU.empty
  , cnvcs_livecrootc_xfer_patch = VU.empty
  , cnvcs_deadcrootc_patch = VU.empty, cnvcs_deadcrootc_storage_patch = VU.empty
  , cnvcs_deadcrootc_xfer_patch = VU.empty
  , cnvcs_gresp_storage_patch = VU.empty, cnvcs_gresp_xfer_patch = VU.empty
  , cnvcs_cpool_patch = VU.empty, cnvcs_xsmrpool_patch = VU.empty
  , cnvcs_xsmrpool_loss_patch = VU.empty, cnvcs_ctrunc_patch = VU.empty
  , cnvcs_woodc_patch = VU.empty, cnvcs_leafcmax_patch = VU.empty
  , cnvcs_cropseedc_deficit_patch = VU.empty
  , cnvcs_rootc_col = VU.empty, cnvcs_leafc_col = VU.empty
  , cnvcs_deadstemc_col = VU.empty, cnvcs_fuelc_col = VU.empty
  , cnvcs_fuelc_crop_col = VU.empty, cnvcs_seedc_grc = VU.empty
  , cnvcs_dispvegc_patch = VU.empty, cnvcs_storvegc_patch = VU.empty
  , cnvcs_totc_patch = VU.empty, cnvcs_totvegc_patch = VU.empty
  , cnvcs_totvegc_col = VU.empty, cnvcs_totc_p2c_col = VU.empty
  }
