-- | Soil biogeochemistry nitrogen state data structures.
-- Fortran: SoilBiogeochemNitrogenStateType.F90 — decomposition N pools at column and gridcell levels.
module CLM.Types.SoilBGCNitrogenStateData
  ( SoilBGCNitrogenStateData(..)
  , defaultSoilBGCNitrogenStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Soil biogeochemistry nitrogen state (SoA layout).
-- Preserves Fortran variable names with @sbgcns_@ prefix.
-- 2D/3D arrays are flattened to 1D vectors with dimension comments.
-- Matrix-CN fields are omitted; add if needed.
data SoilBGCNitrogenStateData = SoilBGCNitrogenStateData
  { -- Vertically-resolved decomposition pools (col * nlev * npools, flattened)
    sbgcns_decomp_npools_vr_col       :: !(VU.Vector Double)   -- ^ (gN/m3)
    -- Vertically-resolved summary (col * nlevdecomp_full, flattened)
  , sbgcns_decomp_soiln_vr_col        :: !(VU.Vector Double)   -- ^ (gN/m3) total soil N vr
    -- Vertically-resolved mineral N and truncation (col * nlevdecomp_full, flattened)
  , sbgcns_sminn_vr_col               :: !(VU.Vector Double)   -- ^ (gN/m3) soil mineral N vr
  , sbgcns_ntrunc_vr_col              :: !(VU.Vector Double)   -- ^ (gN/m3) N truncation vr
    -- Nitrification/denitrification pools (col * nlevdecomp_full, flattened)
  , sbgcns_smin_no3_vr_col            :: !(VU.Vector Double)   -- ^ (gN/m3) soil mineral NO3 vr
  , sbgcns_smin_nh4_vr_col            :: !(VU.Vector Double)   -- ^ (gN/m3) soil mineral NH4 vr
    -- Nitrif/denitrif 1D (col)
  , sbgcns_smin_no3_col               :: !(VU.Vector Double)   -- ^ (gN/m2) soil mineral NO3
  , sbgcns_smin_nh4_col               :: !(VU.Vector Double)   -- ^ (gN/m2) soil mineral NH4
    -- Summary (diagnostic) state variables (col)
  , sbgcns_decomp_npools_col          :: !(VU.Vector Double)   -- ^ (gN/m2) decomp N pools (col * npools, flattened)
  , sbgcns_decomp_npools_1m_col       :: !(VU.Vector Double)   -- ^ (gN/m2) decomp N pools to 1m (col * npools, flattened)
  , sbgcns_sminn_col                   :: !(VU.Vector Double)   -- ^ (gN/m2) soil mineral N
  , sbgcns_ntrunc_col                  :: !(VU.Vector Double)   -- ^ (gN/m2) N truncation
  , sbgcns_cwdn_col                    :: !(VU.Vector Double)   -- ^ (gN/m2) coarse woody debris N
  , sbgcns_totlitn_col                 :: !(VU.Vector Double)   -- ^ (gN/m2) total litter N
  , sbgcns_totmicn_col                 :: !(VU.Vector Double)   -- ^ (gN/m2) total microbial N
  , sbgcns_totsomn_col                 :: !(VU.Vector Double)   -- ^ (gN/m2) total SOM N
  , sbgcns_totlitn_1m_col             :: !(VU.Vector Double)   -- ^ (gN/m2) total litter N to 1m
  , sbgcns_totsomn_1m_col             :: !(VU.Vector Double)   -- ^ (gN/m2) total SOM N to 1m
  , sbgcns_dyn_nbal_adjustments_col   :: !(VU.Vector Double)   -- ^ (gN/m2) dynamic column N adjustments
  , sbgcns_dyn_no3bal_adjustments_col :: !(VU.Vector Double)   -- ^ (gN/m2) dynamic column NO3 adjustments
  , sbgcns_dyn_nh4bal_adjustments_col :: !(VU.Vector Double)   -- ^ (gN/m2) dynamic column NH4 adjustments
    -- Scalar
  , sbgcns_totvegcthresh               :: !Double               -- ^ threshold for zeroing decomp pools
    -- Nitrogen totals
  , sbgcns_totn_col                    :: !(VU.Vector Double)   -- ^ (gN/m2) total column N
  , sbgcns_totecosysn_col             :: !(VU.Vector Double)   -- ^ (gN/m2) total ecosystem N
  , sbgcns_totn_grc                    :: !(VU.Vector Double)   -- ^ (gN/m2) total gridcell N
  } deriving (Show)

defaultSoilBGCNitrogenStateData :: SoilBGCNitrogenStateData
defaultSoilBGCNitrogenStateData = SoilBGCNitrogenStateData
  { sbgcns_decomp_npools_vr_col = VU.empty
  , sbgcns_decomp_soiln_vr_col = VU.empty
  , sbgcns_sminn_vr_col = VU.empty, sbgcns_ntrunc_vr_col = VU.empty
  , sbgcns_smin_no3_vr_col = VU.empty, sbgcns_smin_nh4_vr_col = VU.empty
  , sbgcns_smin_no3_col = VU.empty, sbgcns_smin_nh4_col = VU.empty
  , sbgcns_decomp_npools_col = VU.empty, sbgcns_decomp_npools_1m_col = VU.empty
  , sbgcns_sminn_col = VU.empty, sbgcns_ntrunc_col = VU.empty
  , sbgcns_cwdn_col = VU.empty
  , sbgcns_totlitn_col = VU.empty, sbgcns_totmicn_col = VU.empty
  , sbgcns_totsomn_col = VU.empty
  , sbgcns_totlitn_1m_col = VU.empty, sbgcns_totsomn_1m_col = VU.empty
  , sbgcns_dyn_nbal_adjustments_col = VU.empty
  , sbgcns_dyn_no3bal_adjustments_col = VU.empty
  , sbgcns_dyn_nh4bal_adjustments_col = VU.empty
  , sbgcns_totvegcthresh = 0.0 / 0.0  -- NaN
  , sbgcns_totn_col = VU.empty, sbgcns_totecosysn_col = VU.empty
  , sbgcns_totn_grc = VU.empty
  }
