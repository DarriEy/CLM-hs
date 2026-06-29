{-# LANGUAGE BangPatterns #-}
-- | Gap-phase mortality for coupled carbon-nitrogen code (CN).
-- Fortran: CNGapMortalityMod.F90
-- Julia:   src/biogeochem/gap_mortality.jl
--
-- All functions are pure.
module CLM.BioGeoChem.GapMortality
  ( -- * Data types
    GapMortalityParams(..)
  , defaultGapMortalityParams
  , PftConGapMort(..)
  , DgvsGapMortData(..)
    -- * Patch-level mortality
  , GapMortInput(..)
  , GapMortOutput(..)
  , cnGapMortality
    -- * Patch-to-column aggregation
  , GapP2CInput(..)
  , GapP2COutput(..)
  , cnGapPatchToColumn
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | Parameters for gap mortality.
data GapMortalityParams = GapMortalityParams
  { gmp_k_mort :: !Double                -- ^ coeff. of growth efficiency in mortality equation
  , gmp_r_mort :: !(VU.Vector Double)    -- ^ mortality rate (1/yr), one per PFT
  } deriving (Show)

defaultGapMortalityParams :: GapMortalityParams
defaultGapMortalityParams = GapMortalityParams
  { gmp_k_mort = 0.3
  , gmp_r_mort = VU.empty
  }

-- | PFT constants for gap mortality.
data PftConGapMort = PftConGapMort
  { pgm_woody  :: !(VU.Vector Double)
  , pgm_leafcn :: !(VU.Vector Double)
  , pgm_lf_f   :: !(VU.Vector Double)   -- ^ leaf litter fractions (npft * nlitr), row-major
  , pgm_fr_f   :: !(VU.Vector Double)   -- ^ fine root litter fractions (npft * nlitr), row-major
  , pgm_nlitr  :: !Int                  -- ^ number of litter pools
  } deriving (Show)

-- | Dynamic vegetation data for gap mortality.
data DgvsGapMortData = DgvsGapMortData
  { dgm_greffic    :: !(VU.Vector Double)  -- ^ growth efficiency
  , dgm_heatstress :: !(VU.Vector Double)  -- ^ heat stress mortality
  , dgm_nind       :: !(VU.Vector Double)  -- ^ number of individuals (#/m2)
  } deriving (Show)

-- | Input to gap mortality calculation.
data GapMortInput = GapMortInput
  { gmi2_np              :: !Int
  , gmi2_mask            :: !(VU.Vector Bool)
  , gmi2_ivt             :: !(VU.Vector Int)
  , gmi2_params          :: !GapMortalityParams
  , gmi2_pftcon          :: !PftConGapMort
  , gmi2_dgvs            :: !DgvsGapMortData
  , gmi2_use_cndv        :: !Bool
  , gmi2_spinup_state    :: !Int
  , gmi2_spinup_factor_deadwood :: !Double
  , gmi2_days_per_year   :: !Double
  , gmi2_npcropmin       :: !Int
  -- Carbon state (per-patch)
  , gmi2_leafc           :: !(VU.Vector Double)
  , gmi2_frootc          :: !(VU.Vector Double)
  , gmi2_livestemc       :: !(VU.Vector Double)
  , gmi2_deadstemc       :: !(VU.Vector Double)
  , gmi2_livecrootc      :: !(VU.Vector Double)
  , gmi2_deadcrootc      :: !(VU.Vector Double)
  , gmi2_leafc_storage   :: !(VU.Vector Double)
  , gmi2_frootc_storage  :: !(VU.Vector Double)
  , gmi2_livestemc_storage  :: !(VU.Vector Double)
  , gmi2_deadstemc_storage  :: !(VU.Vector Double)
  , gmi2_livecrootc_storage :: !(VU.Vector Double)
  , gmi2_deadcrootc_storage :: !(VU.Vector Double)
  , gmi2_gresp_storage   :: !(VU.Vector Double)
  , gmi2_leafc_xfer      :: !(VU.Vector Double)
  , gmi2_frootc_xfer     :: !(VU.Vector Double)
  , gmi2_livestemc_xfer  :: !(VU.Vector Double)
  , gmi2_deadstemc_xfer  :: !(VU.Vector Double)
  , gmi2_livecrootc_xfer :: !(VU.Vector Double)
  , gmi2_deadcrootc_xfer :: !(VU.Vector Double)
  , gmi2_gresp_xfer      :: !(VU.Vector Double)
  -- Nitrogen state (per-patch)
  , gmi2_leafn           :: !(VU.Vector Double)
  , gmi2_frootn          :: !(VU.Vector Double)
  , gmi2_livestemn       :: !(VU.Vector Double)
  , gmi2_deadstemn       :: !(VU.Vector Double)
  , gmi2_livecrootn      :: !(VU.Vector Double)
  , gmi2_deadcrootn      :: !(VU.Vector Double)
  , gmi2_retransn        :: !(VU.Vector Double)
  , gmi2_leafn_storage   :: !(VU.Vector Double)
  , gmi2_frootn_storage  :: !(VU.Vector Double)
  , gmi2_livestemn_storage  :: !(VU.Vector Double)
  , gmi2_deadstemn_storage  :: !(VU.Vector Double)
  , gmi2_livecrootn_storage :: !(VU.Vector Double)
  , gmi2_deadcrootn_storage :: !(VU.Vector Double)
  , gmi2_leafn_xfer      :: !(VU.Vector Double)
  , gmi2_frootn_xfer     :: !(VU.Vector Double)
  , gmi2_livestemn_xfer  :: !(VU.Vector Double)
  , gmi2_deadstemn_xfer  :: !(VU.Vector Double)
  , gmi2_livecrootn_xfer :: !(VU.Vector Double)
  , gmi2_deadcrootn_xfer :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of gap mortality calculation.
-- All vectors are per-patch (np). Carbon and nitrogen fluxes.
data GapMortOutput = GapMortOutput
  { -- Carbon fluxes (displayed pools)
    gmo2_m_leafc_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootc_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemc_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemc_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootc_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootc_to_litter :: !(VU.Vector Double)
  -- Carbon fluxes (storage pools)
  , gmo2_m_leafc_storage_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootc_storage_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemc_storage_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemc_storage_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootc_storage_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootc_storage_to_litter :: !(VU.Vector Double)
  , gmo2_m_gresp_storage_to_litter      :: !(VU.Vector Double)
  -- Carbon fluxes (transfer pools)
  , gmo2_m_leafc_xfer_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootc_xfer_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemc_xfer_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemc_xfer_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootc_xfer_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootc_xfer_to_litter :: !(VU.Vector Double)
  , gmo2_m_gresp_xfer_to_litter      :: !(VU.Vector Double)
  -- Nitrogen fluxes (displayed pools)
  , gmo2_m_leafn_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootn_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemn_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemn_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootn_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootn_to_litter :: !(VU.Vector Double)
  , gmo2_m_retransn_to_litter   :: !(VU.Vector Double)
  -- Nitrogen fluxes (storage pools)
  , gmo2_m_leafn_storage_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootn_storage_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemn_storage_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemn_storage_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootn_storage_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootn_storage_to_litter :: !(VU.Vector Double)
  -- Nitrogen fluxes (transfer pools)
  , gmo2_m_leafn_xfer_to_litter      :: !(VU.Vector Double)
  , gmo2_m_frootn_xfer_to_litter     :: !(VU.Vector Double)
  , gmo2_m_livestemn_xfer_to_litter  :: !(VU.Vector Double)
  , gmo2_m_deadstemn_xfer_to_litter  :: !(VU.Vector Double)
  , gmo2_m_livecrootn_xfer_to_litter :: !(VU.Vector Double)
  , gmo2_m_deadcrootn_xfer_to_litter :: !(VU.Vector Double)
  -- Updated DGVS nind
  , gmo2_nind              :: !(VU.Vector Double)
  } deriving (Show)

-- | Calculate patch-level gap mortality fluxes.
cnGapMortality :: GapMortInput -> GapMortOutput
cnGapMortality inp =
  let !np = gmi2_np inp
      mask = gmi2_mask inp
      ivt  = gmi2_ivt inp
      params = gmi2_params inp
      pft  = gmi2_pftcon inp
      dgvs = gmi2_dgvs inp
      use_cndv = gmi2_use_cndv inp
      spinup_state = gmi2_spinup_state inp
      spf  = gmi2_spinup_factor_deadwood inp
      dpy  = gmi2_days_per_year inp
      npcropmin = gmi2_npcropmin inp

      iv p = ivt VU.! p + 1  -- 0-based to 1-based

      -- Compute mortality rate m for each patch
      mRate p =
        let am = if use_cndv
                 then if pgm_woody pft VU.! iv p == 1.0
                      then let mort_max = if ivt VU.! p == 8 then 0.03 else 0.01
                               a = mort_max / (1.0 + gmp_k_mort params * dgm_greffic dgvs VU.! p)
                           in min 1.0 (a + dgm_heatstress dgvs VU.! p)
                      else gmp_r_mort params VU.! iv p
                 else gmp_r_mort params VU.! iv p
        in am / (dpy * secspday)

      gen f = VU.generate np $ \p ->
        if not (mask VU.! p) then 0.0 else f p (mRate p)

  in GapMortOutput
    { gmo2_m_leafc_to_litter      = gen $ \p m -> gmi2_leafc inp VU.! p * m
    , gmo2_m_frootc_to_litter     = gen $ \p m -> gmi2_frootc inp VU.! p * m
    , gmo2_m_livestemc_to_litter  = gen $ \p m -> gmi2_livestemc inp VU.! p * m
    , gmo2_m_livecrootc_to_litter = gen $ \p m -> gmi2_livecrootc inp VU.! p * m
    , gmo2_m_deadstemc_to_litter  = gen $ \p m -> gmi2_deadstemc inp VU.! p * m * spf
    , gmo2_m_deadcrootc_to_litter = gen $ \p m -> gmi2_deadcrootc inp VU.! p * m * spf

    , gmo2_m_leafc_storage_to_litter      = gen $ \p m -> gmi2_leafc_storage inp VU.! p * m
    , gmo2_m_frootc_storage_to_litter     = gen $ \p m -> gmi2_frootc_storage inp VU.! p * m
    , gmo2_m_livestemc_storage_to_litter  = gen $ \p m -> gmi2_livestemc_storage inp VU.! p * m
    , gmo2_m_deadstemc_storage_to_litter  = gen $ \p m -> gmi2_deadstemc_storage inp VU.! p * m
    , gmo2_m_livecrootc_storage_to_litter = gen $ \p m -> gmi2_livecrootc_storage inp VU.! p * m
    , gmo2_m_deadcrootc_storage_to_litter = gen $ \p m -> gmi2_deadcrootc_storage inp VU.! p * m
    , gmo2_m_gresp_storage_to_litter      = gen $ \p m -> gmi2_gresp_storage inp VU.! p * m

    , gmo2_m_leafc_xfer_to_litter      = gen $ \p m -> gmi2_leafc_xfer inp VU.! p * m
    , gmo2_m_frootc_xfer_to_litter     = gen $ \p m -> gmi2_frootc_xfer inp VU.! p * m
    , gmo2_m_livestemc_xfer_to_litter  = gen $ \p m -> gmi2_livestemc_xfer inp VU.! p * m
    , gmo2_m_deadstemc_xfer_to_litter  = gen $ \p m -> gmi2_deadstemc_xfer inp VU.! p * m
    , gmo2_m_livecrootc_xfer_to_litter = gen $ \p m -> gmi2_livecrootc_xfer inp VU.! p * m
    , gmo2_m_deadcrootc_xfer_to_litter = gen $ \p m -> gmi2_deadcrootc_xfer inp VU.! p * m
    , gmo2_m_gresp_xfer_to_litter      = gen $ \p m -> gmi2_gresp_xfer inp VU.! p * m

    , gmo2_m_leafn_to_litter      = gen $ \p m -> gmi2_leafn inp VU.! p * m
    , gmo2_m_frootn_to_litter     = gen $ \p m -> gmi2_frootn inp VU.! p * m
    , gmo2_m_livestemn_to_litter  = gen $ \p m -> gmi2_livestemn inp VU.! p * m
    , gmo2_m_livecrootn_to_litter = gen $ \p m -> gmi2_livecrootn inp VU.! p * m
    , gmo2_m_deadstemn_to_litter  = gen $ \p m ->
        if spinup_state == 2 && not use_cndv
        then gmi2_deadstemn inp VU.! p * m * spf
        else gmi2_deadstemn inp VU.! p * m
    , gmo2_m_deadcrootn_to_litter = gen $ \p m ->
        if spinup_state == 2 && not use_cndv
        then gmi2_deadcrootn inp VU.! p * m * spf
        else gmi2_deadcrootn inp VU.! p * m
    , gmo2_m_retransn_to_litter   = gen $ \p m ->
        if ivt VU.! p < npcropmin
        then gmi2_retransn inp VU.! p * m
        else 0.0

    , gmo2_m_leafn_storage_to_litter      = gen $ \p m -> gmi2_leafn_storage inp VU.! p * m
    , gmo2_m_frootn_storage_to_litter     = gen $ \p m -> gmi2_frootn_storage inp VU.! p * m
    , gmo2_m_livestemn_storage_to_litter  = gen $ \p m -> gmi2_livestemn_storage inp VU.! p * m
    , gmo2_m_deadstemn_storage_to_litter  = gen $ \p m -> gmi2_deadstemn_storage inp VU.! p * m
    , gmo2_m_livecrootn_storage_to_litter = gen $ \p m -> gmi2_livecrootn_storage inp VU.! p * m
    , gmo2_m_deadcrootn_storage_to_litter = gen $ \p m -> gmi2_deadcrootn_storage inp VU.! p * m

    , gmo2_m_leafn_xfer_to_litter      = gen $ \p m -> gmi2_leafn_xfer inp VU.! p * m
    , gmo2_m_frootn_xfer_to_litter     = gen $ \p m -> gmi2_frootn_xfer inp VU.! p * m
    , gmo2_m_livestemn_xfer_to_litter  = gen $ \p m -> gmi2_livestemn_xfer inp VU.! p * m
    , gmo2_m_deadstemn_xfer_to_litter  = gen $ \p m -> gmi2_deadstemn_xfer inp VU.! p * m
    , gmo2_m_livecrootn_xfer_to_litter = gen $ \p m -> gmi2_livecrootn_xfer inp VU.! p * m
    , gmo2_m_deadcrootn_xfer_to_litter = gen $ \p m -> gmi2_deadcrootn_xfer inp VU.! p * m

    , gmo2_nind = VU.generate np $ \p ->
        if not (mask VU.! p) then dgm_nind dgvs VU.! p
        else if use_cndv && pgm_woody pft VU.! iv p == 1.0
             then if gmi2_livestemc inp VU.! p + gmi2_deadstemc inp VU.! p > 0.0
                  then dgm_nind dgvs VU.! p * (1.0 - mRate p)
                  else 0.0
             else dgm_nind dgvs VU.! p
    }

-- | Input to patch-to-column aggregation.
data GapP2CInput = GapP2CInput
  { gpc_np          :: !Int
  , gpc_nc          :: !Int
  , gpc_nlevdecomp  :: !Int
  , gpc_mask        :: !(VU.Vector Bool)
  , gpc_ivt         :: !(VU.Vector Int)
  , gpc_column      :: !(VU.Vector Int)       -- ^ patch-to-column mapping
  , gpc_wtcol       :: !(VU.Vector Double)    -- ^ patch weight on column
  , gpc_pftcon      :: !PftConGapMort
  , gpc_i_litr_min  :: !Int
  , gpc_i_litr_max  :: !Int
  , gpc_i_met_lit   :: !Int
  -- Mortality fluxes (from GapMortOutput)
  , gpc_mort_out    :: !GapMortOutput
  -- Vertical profiles (np * nlevdecomp)
  , gpc_leaf_prof   :: !(VU.Vector Double)
  , gpc_froot_prof  :: !(VU.Vector Double)
  , gpc_croot_prof  :: !(VU.Vector Double)
  , gpc_stem_prof   :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of patch-to-column aggregation.
data GapP2COutput = GapP2COutput
  { gpo_gap_mortality_c_to_litr_c :: !(VU.Vector Double)  -- ^ (nc * nlev * nlitr)
  , gpo_gap_mortality_c_to_cwdc   :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , gpo_gap_mortality_n_to_litr_n :: !(VU.Vector Double)  -- ^ (nc * nlev * nlitr)
  , gpo_gap_mortality_n_to_cwdn   :: !(VU.Vector Double)  -- ^ (nc * nlev)
  } deriving (Show)

-- | Aggregate patch-level gap mortality fluxes to column level.
cnGapPatchToColumn :: GapP2CInput -> GapP2COutput
cnGapPatchToColumn inp =
  let !np   = gpc_np inp
      !nc   = gpc_nc inp
      !nlev = gpc_nlevdecomp inp
      nlitr = pgm_nlitr (gpc_pftcon inp)
      mask  = gpc_mask inp
      ivt   = gpc_ivt inp
      col   = gpc_column inp
      wt    = gpc_wtcol inp
      pft   = gpc_pftcon inp
      mort  = gpc_mort_out inp
      i_min = gpc_i_litr_min inp
      i_max = gpc_i_litr_max inp
      i_met = gpc_i_met_lit inp

      iv p = ivt VU.! p + 1

      idxP2 p j = p + np * j
      idxC2 c j = c + nc * j
      idx3C nc' c j k = c + nc' * (j + nlev * k)

      -- Litter fraction helpers (stored as row-major 2D array of shape npft * nlitr)
      getLfF !pftIdx !i = pgm_lf_f pft VU.! (pftIdx * nlitr + i)
      getFrF !pftIdx !i = pgm_fr_f pft VU.! (pftIdx * nlitr + i)

      -- C to litter (nc * nlev * nlitr)
      c_to_litr = VU.generate (nc * nlev * nlitr) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c' = rem1 `mod` nc
            i = k  -- litter pool index
        in if i < i_min || i > i_max
           then -- metabolic litter from storage/xfer
             if i == i_met
             then sum [ let w = wt VU.! p; lp = gpc_leaf_prof inp VU.! idxP2 p j
                            fp = gpc_froot_prof inp VU.! idxP2 p j
                            sp = gpc_stem_prof inp VU.! idxP2 p j
                            cp = gpc_croot_prof inp VU.! idxP2 p j
                            mo = mort
                        in w * (
                          (gmo2_m_leafc_storage_to_litter mo VU.! p + gmo2_m_gresp_storage_to_litter mo VU.! p) * lp
                        + gmo2_m_frootc_storage_to_litter mo VU.! p * fp
                        + (gmo2_m_livestemc_storage_to_litter mo VU.! p + gmo2_m_deadstemc_storage_to_litter mo VU.! p) * sp
                        + (gmo2_m_livecrootc_storage_to_litter mo VU.! p + gmo2_m_deadcrootc_storage_to_litter mo VU.! p) * cp
                        + (gmo2_m_leafc_xfer_to_litter mo VU.! p + gmo2_m_gresp_xfer_to_litter mo VU.! p) * lp
                        + gmo2_m_frootc_xfer_to_litter mo VU.! p * fp
                        + (gmo2_m_livestemc_xfer_to_litter mo VU.! p + gmo2_m_deadstemc_xfer_to_litter mo VU.! p) * sp
                        + (gmo2_m_livecrootc_xfer_to_litter mo VU.! p + gmo2_m_deadcrootc_xfer_to_litter mo VU.! p) * cp
                        )
                      | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]
             else 0.0
           else -- leaf/froot litter fractions
             sum [ let w = wt VU.! p; lp = gpc_leaf_prof inp VU.! idxP2 p j
                       fp = gpc_froot_prof inp VU.! idxP2 p j
                   in w * (gmo2_m_leafc_to_litter mort VU.! p * getLfF (iv p) i * lp
                         + gmo2_m_frootc_to_litter mort VU.! p * getFrF (iv p) i * fp)
                 | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]

      -- C to CWD (nc * nlev)
      c_to_cwd = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c' = ix `mod` nc
        in sum [ let w = wt VU.! p
                     sp = gpc_stem_prof inp VU.! idxP2 p j
                     cp = gpc_croot_prof inp VU.! idxP2 p j
                 in w * ((gmo2_m_livestemc_to_litter mort VU.! p + gmo2_m_deadstemc_to_litter mort VU.! p) * sp
                       + (gmo2_m_livecrootc_to_litter mort VU.! p + gmo2_m_deadcrootc_to_litter mort VU.! p) * cp)
               | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]

      -- N to litter (nc * nlev * nlitr)
      n_to_litr = VU.generate (nc * nlev * nlitr) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c' = rem1 `mod` nc
            i = k
        in if i < i_min || i > i_max
           then if i == i_met
                then sum [ let w = wt VU.! p; lp = gpc_leaf_prof inp VU.! idxP2 p j
                               fp = gpc_froot_prof inp VU.! idxP2 p j
                               sp = gpc_stem_prof inp VU.! idxP2 p j
                               cp = gpc_croot_prof inp VU.! idxP2 p j
                               mo = mort
                           in w * (
                             gmo2_m_retransn_to_litter mo VU.! p * lp
                           + gmo2_m_leafn_storage_to_litter mo VU.! p * lp
                           + gmo2_m_frootn_storage_to_litter mo VU.! p * fp
                           + (gmo2_m_livestemn_storage_to_litter mo VU.! p + gmo2_m_deadstemn_storage_to_litter mo VU.! p) * sp
                           + (gmo2_m_livecrootn_storage_to_litter mo VU.! p + gmo2_m_deadcrootn_storage_to_litter mo VU.! p) * cp
                           + gmo2_m_leafn_xfer_to_litter mo VU.! p * lp
                           + gmo2_m_frootn_xfer_to_litter mo VU.! p * fp
                           + (gmo2_m_livestemn_xfer_to_litter mo VU.! p + gmo2_m_deadstemn_xfer_to_litter mo VU.! p) * sp
                           + (gmo2_m_livecrootn_xfer_to_litter mo VU.! p + gmo2_m_deadcrootn_xfer_to_litter mo VU.! p) * cp
                           )
                         | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]
                else 0.0
           else sum [ let w = wt VU.! p; lp = gpc_leaf_prof inp VU.! idxP2 p j
                          fp = gpc_froot_prof inp VU.! idxP2 p j
                      in w * (gmo2_m_leafn_to_litter mort VU.! p * getLfF (iv p) i * lp
                            + gmo2_m_frootn_to_litter mort VU.! p * getFrF (iv p) i * fp)
                    | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]

      -- N to CWD (nc * nlev)
      n_to_cwd = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c' = ix `mod` nc
        in sum [ let w = wt VU.! p
                     sp = gpc_stem_prof inp VU.! idxP2 p j
                     cp = gpc_croot_prof inp VU.! idxP2 p j
                 in w * ((gmo2_m_livestemn_to_litter mort VU.! p + gmo2_m_deadstemn_to_litter mort VU.! p) * sp
                       + (gmo2_m_livecrootn_to_litter mort VU.! p + gmo2_m_deadcrootn_to_litter mort VU.! p) * cp)
               | p <- [0..np-1], mask VU.! p, col VU.! p == c' ]

  in GapP2COutput
    { gpo_gap_mortality_c_to_litr_c = c_to_litr
    , gpo_gap_mortality_c_to_cwdc   = c_to_cwd
    , gpo_gap_mortality_n_to_litr_n = n_to_litr
    , gpo_gap_mortality_n_to_cwdn   = n_to_cwd
    }
