-- | CN vegetation nitrogen state data structures.
-- Fortran: CNVegNitrogenStateType.F90 — nitrogen pools at patch, column, gridcell levels.
module CLM.Types.CNVegNitrogenStateData
  ( CNVegNitrogenStateData(..)
  , defaultCNVegNitrogenStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | CN vegetation nitrogen state (SoA layout).
-- Preserves Fortran variable names with @cnvns_@ prefix.
data CNVegNitrogenStateData = CNVegNitrogenStateData
  { -- Patch 2D (flattened patch * nrepr)
    cnvns_reproductiven_patch         :: !(VU.Vector Double)
  , cnvns_reproductiven_storage_patch :: !(VU.Vector Double)
  , cnvns_reproductiven_xfer_patch    :: !(VU.Vector Double)
    -- Patch 1D core pools
  , cnvns_leafn_patch                 :: !(VU.Vector Double)
  , cnvns_leafn_storage_patch         :: !(VU.Vector Double)
  , cnvns_leafn_xfer_patch            :: !(VU.Vector Double)
  , cnvns_leafn_storage_xfer_acc_patch :: !(VU.Vector Double)
  , cnvns_storage_ndemand_patch       :: !(VU.Vector Double)
  , cnvns_frootn_patch                :: !(VU.Vector Double)
  , cnvns_frootn_storage_patch        :: !(VU.Vector Double)
  , cnvns_frootn_xfer_patch           :: !(VU.Vector Double)
  , cnvns_livestemn_patch             :: !(VU.Vector Double)
  , cnvns_livestemn_storage_patch     :: !(VU.Vector Double)
  , cnvns_livestemn_xfer_patch        :: !(VU.Vector Double)
  , cnvns_deadstemn_patch             :: !(VU.Vector Double)
  , cnvns_deadstemn_storage_patch     :: !(VU.Vector Double)
  , cnvns_deadstemn_xfer_patch        :: !(VU.Vector Double)
  , cnvns_livecrootn_patch            :: !(VU.Vector Double)
  , cnvns_livecrootn_storage_patch    :: !(VU.Vector Double)
  , cnvns_livecrootn_xfer_patch       :: !(VU.Vector Double)
  , cnvns_deadcrootn_patch            :: !(VU.Vector Double)
  , cnvns_deadcrootn_storage_patch    :: !(VU.Vector Double)
  , cnvns_deadcrootn_xfer_patch       :: !(VU.Vector Double)
  , cnvns_retransn_patch              :: !(VU.Vector Double)
  , cnvns_npool_patch                 :: !(VU.Vector Double)
  , cnvns_ntrunc_patch                :: !(VU.Vector Double)
  , cnvns_cropseedn_deficit_patch     :: !(VU.Vector Double)
    -- Summary diagnostics
  , cnvns_dispvegn_patch              :: !(VU.Vector Double)
  , cnvns_storvegn_patch              :: !(VU.Vector Double)
  , cnvns_totvegn_patch               :: !(VU.Vector Double)
  , cnvns_totn_patch                  :: !(VU.Vector Double)
    -- Column-level
  , cnvns_totvegn_col                 :: !(VU.Vector Double)
  , cnvns_totn_p2c_col                :: !(VU.Vector Double)
    -- Gridcell-level
  , cnvns_seedn_grc                   :: !(VU.Vector Double)
  } deriving (Show)

defaultCNVegNitrogenStateData :: CNVegNitrogenStateData
defaultCNVegNitrogenStateData = CNVegNitrogenStateData
  { cnvns_reproductiven_patch = VU.empty, cnvns_reproductiven_storage_patch = VU.empty
  , cnvns_reproductiven_xfer_patch = VU.empty
  , cnvns_leafn_patch = VU.empty, cnvns_leafn_storage_patch = VU.empty
  , cnvns_leafn_xfer_patch = VU.empty, cnvns_leafn_storage_xfer_acc_patch = VU.empty
  , cnvns_storage_ndemand_patch = VU.empty
  , cnvns_frootn_patch = VU.empty, cnvns_frootn_storage_patch = VU.empty
  , cnvns_frootn_xfer_patch = VU.empty
  , cnvns_livestemn_patch = VU.empty, cnvns_livestemn_storage_patch = VU.empty
  , cnvns_livestemn_xfer_patch = VU.empty
  , cnvns_deadstemn_patch = VU.empty, cnvns_deadstemn_storage_patch = VU.empty
  , cnvns_deadstemn_xfer_patch = VU.empty
  , cnvns_livecrootn_patch = VU.empty, cnvns_livecrootn_storage_patch = VU.empty
  , cnvns_livecrootn_xfer_patch = VU.empty
  , cnvns_deadcrootn_patch = VU.empty, cnvns_deadcrootn_storage_patch = VU.empty
  , cnvns_deadcrootn_xfer_patch = VU.empty
  , cnvns_retransn_patch = VU.empty, cnvns_npool_patch = VU.empty
  , cnvns_ntrunc_patch = VU.empty, cnvns_cropseedn_deficit_patch = VU.empty
  , cnvns_dispvegn_patch = VU.empty, cnvns_storvegn_patch = VU.empty
  , cnvns_totvegn_patch = VU.empty, cnvns_totn_patch = VU.empty
  , cnvns_totvegn_col = VU.empty, cnvns_totn_p2c_col = VU.empty
  , cnvns_seedn_grc = VU.empty
  }
