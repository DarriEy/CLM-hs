{-# LANGUAGE BangPatterns #-}
-- | Growth respiration fluxes for coupled carbon-nitrogen code (CN).
-- Fortran: CNGRespMod.F90
-- Julia:   src/biogeochem/growth_resp.jl
--
-- All functions are pure.
module CLM.BioGeoChem.GrowthResp
  ( -- * Data types
    PftConGrowthResp(..)
  , GrowthRespInput(..)
  , GrowthRespOutput(..)
    -- * Main computation
  , cnGrowthResp
  ) where

import qualified Data.Vector.Unboxed as VU

-- | PFT constants needed by growth respiration.
data PftConGrowthResp = PftConGrowthResp
  { pgr_woody  :: !(VU.Vector Double)  -- ^ binary woody flag (1=woody)
  , pgr_grperc :: !(VU.Vector Double)  -- ^ growth respiration parameter
  , pgr_grpnow :: !(VU.Vector Double)  -- ^ fraction now vs storage
  } deriving (Show)

-- | Input to growth respiration calculation.
data GrowthRespInput = GrowthRespInput
  { gri_np                :: !Int
  , gri_mask              :: !(VU.Vector Bool)
  , gri_ivt               :: !(VU.Vector Int)      -- ^ PFT type per patch
  , gri_pftcon            :: !PftConGrowthResp
  , gri_npcropmin         :: !Int
  -- Allocation fluxes (cpool_to_*)
  , gri_cpool_to_leafc              :: !(VU.Vector Double)
  , gri_cpool_to_leafc_storage      :: !(VU.Vector Double)
  , gri_cpool_to_frootc             :: !(VU.Vector Double)
  , gri_cpool_to_frootc_storage     :: !(VU.Vector Double)
  , gri_cpool_to_livestemc          :: !(VU.Vector Double)
  , gri_cpool_to_livestemc_storage  :: !(VU.Vector Double)
  , gri_cpool_to_deadstemc          :: !(VU.Vector Double)
  , gri_cpool_to_deadstemc_storage  :: !(VU.Vector Double)
  , gri_cpool_to_livecrootc         :: !(VU.Vector Double)
  , gri_cpool_to_livecrootc_storage :: !(VU.Vector Double)
  , gri_cpool_to_deadcrootc         :: !(VU.Vector Double)
  , gri_cpool_to_deadcrootc_storage :: !(VU.Vector Double)
  -- Transfer growth fluxes
  , gri_leafc_xfer_to_leafc           :: !(VU.Vector Double)
  , gri_frootc_xfer_to_frootc         :: !(VU.Vector Double)
  , gri_livestemc_xfer_to_livestemc   :: !(VU.Vector Double)
  , gri_deadstemc_xfer_to_deadstemc   :: !(VU.Vector Double)
  , gri_livecrootc_xfer_to_livecrootc :: !(VU.Vector Double)
  , gri_deadcrootc_xfer_to_deadcrootc :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of growth respiration calculation.
-- Each vector is per-patch (np).
data GrowthRespOutput = GrowthRespOutput
  { gro_cpool_leaf_gr              :: !(VU.Vector Double)
  , gro_cpool_leaf_storage_gr      :: !(VU.Vector Double)
  , gro_transfer_leaf_gr           :: !(VU.Vector Double)
  , gro_cpool_froot_gr             :: !(VU.Vector Double)
  , gro_cpool_froot_storage_gr     :: !(VU.Vector Double)
  , gro_transfer_froot_gr          :: !(VU.Vector Double)
  , gro_cpool_livestem_gr          :: !(VU.Vector Double)
  , gro_cpool_livestem_storage_gr  :: !(VU.Vector Double)
  , gro_transfer_livestem_gr       :: !(VU.Vector Double)
  , gro_cpool_deadstem_gr          :: !(VU.Vector Double)
  , gro_cpool_deadstem_storage_gr  :: !(VU.Vector Double)
  , gro_transfer_deadstem_gr       :: !(VU.Vector Double)
  , gro_cpool_livecroot_gr         :: !(VU.Vector Double)
  , gro_cpool_livecroot_storage_gr :: !(VU.Vector Double)
  , gro_transfer_livecroot_gr      :: !(VU.Vector Double)
  , gro_cpool_deadcroot_gr         :: !(VU.Vector Double)
  , gro_cpool_deadcroot_storage_gr :: !(VU.Vector Double)
  , gro_transfer_deadcroot_gr      :: !(VU.Vector Double)
  } deriving (Show)

-- | Calculate growth respiration fluxes.
cnGrowthResp :: GrowthRespInput -> GrowthRespOutput
cnGrowthResp inp =
  let !np = gri_np inp
      mask = gri_mask inp
      ivt  = gri_ivt inp
      pft  = gri_pftcon inp
      npcropmin = gri_npcropmin inp

      gen f = VU.generate np $ \p ->
        if not (mask VU.! p) then 0.0 else f p

      iv p = ivt VU.! p + 1  -- 0-based to 1-based
      gp p = pgr_grperc pft VU.! iv p
      gn p = pgr_grpnow pft VU.! iv p
      wd p = pgr_woody pft VU.! iv p

      -- All respfact values are 1.0 in Fortran (preserved for traceability)

  in GrowthRespOutput
    { gro_cpool_leaf_gr = gen $ \p ->
        gri_cpool_to_leafc inp VU.! p * gp p

    , gro_cpool_leaf_storage_gr = gen $ \p ->
        gri_cpool_to_leafc_storage inp VU.! p * gp p * gn p

    , gro_transfer_leaf_gr = gen $ \p ->
        gri_leafc_xfer_to_leafc inp VU.! p * gp p * (1.0 - gn p)

    -- Note: respfact_froot appears twice in Fortran original
    , gro_cpool_froot_gr = gen $ \p ->
        gri_cpool_to_frootc inp VU.! p * gp p * 1.0

    , gro_cpool_froot_storage_gr = gen $ \p ->
        gri_cpool_to_frootc_storage inp VU.! p * gp p * gn p

    , gro_transfer_froot_gr = gen $ \p ->
        gri_frootc_xfer_to_frootc inp VU.! p * gp p * (1.0 - gn p)

    , gro_cpool_livestem_gr = gen $ \p ->
        if wd p == 1.0 || ivt VU.! p >= npcropmin
        then gri_cpool_to_livestemc inp VU.! p * gp p
        else 0.0

    , gro_cpool_livestem_storage_gr = gen $ \p ->
        if wd p == 1.0 || ivt VU.! p >= npcropmin
        then gri_cpool_to_livestemc_storage inp VU.! p * gp p * gn p
        else 0.0

    , gro_transfer_livestem_gr = gen $ \p ->
        if wd p == 1.0 || ivt VU.! p >= npcropmin
        then gri_livestemc_xfer_to_livestemc inp VU.! p * gp p * (1.0 - gn p)
        else 0.0

    , gro_cpool_deadstem_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_deadstemc inp VU.! p * gp p
        else 0.0

    , gro_cpool_deadstem_storage_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_deadstemc_storage inp VU.! p * gp p * gn p
        else 0.0

    , gro_transfer_deadstem_gr = gen $ \p ->
        if wd p == 1.0
        then gri_deadstemc_xfer_to_deadstemc inp VU.! p * gp p * (1.0 - gn p)
        else 0.0

    , gro_cpool_livecroot_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_livecrootc inp VU.! p * gp p
        else 0.0

    , gro_cpool_livecroot_storage_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_livecrootc_storage inp VU.! p * gp p * gn p
        else 0.0

    , gro_transfer_livecroot_gr = gen $ \p ->
        if wd p == 1.0
        then gri_livecrootc_xfer_to_livecrootc inp VU.! p * gp p * (1.0 - gn p)
        else 0.0

    , gro_cpool_deadcroot_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_deadcrootc inp VU.! p * gp p
        else 0.0

    , gro_cpool_deadcroot_storage_gr = gen $ \p ->
        if wd p == 1.0
        then gri_cpool_to_deadcrootc_storage inp VU.! p * gp p * gn p
        else 0.0

    , gro_transfer_deadcroot_gr = gen $ \p ->
        if wd p == 1.0
        then gri_deadcrootc_xfer_to_deadcrootc inp VU.! p * gp p * (1.0 - gn p)
        else 0.0
    }
