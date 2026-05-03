{-# LANGUAGE BangPatterns #-}
-- | Vegetation structure updates (LAI, SAI, htop, hbot) from C state variables.
--
-- On the radiation time step, uses C state variables and ecological process
-- constants (epc) to diagnose vegetation structure (LAI, SAI, height).
--
-- Ported from: CNVegStructUpdateMod.F90
-- Julia:       src/biogeochem/veg_struct_update.jl
--
-- Public functions:
--   cnVegStructUpdate -- Update vegetation structure from live C pools
--
module CLM.BioGeoChem.VegStructUpdate
  ( -- * Constants
    dtsmonth
  , fracSnoThreshold
    -- * Configuration
  , VegStructConfig(..)
  , defaultVegStructConfig
    -- * Functions
  , cnVegStructUpdate
  ) where

import qualified Data.Vector.Unboxed as U

-- ========================================================================
-- Constants
-- ========================================================================

-- | Seconds in a 30-day month (60*60*24*30)
dtsmonth :: Double
dtsmonth = 2592000.0

-- | frac_sno values > this treated as 1
fracSnoThreshold :: Double
fracSnoThreshold = 0.999

-- ========================================================================
-- Configuration
-- ========================================================================

-- | Configuration for vegetation structure update, carrying PFT indices
-- and control flags.
data VegStructConfig = VegStructConfig
  { vsc_dt                        :: !Double
  , vsc_use_cndv                  :: !Bool
  , vsc_use_biomass_heat_storage  :: !Bool
  , vsc_spinup_factor_deadwood    :: !Double
  , vsc_c_to_b                    :: !Double
  , vsc_noveg                     :: !Int
  , vsc_nc3crop                   :: !Int
  , vsc_nc3irrig                  :: !Int
  , vsc_nbrdlf_evr_shrub         :: !Int
  , vsc_nbrdlf_dcd_brl_shrub     :: !Int
  , vsc_npcropmin                 :: !Int
  , vsc_ntmp_corn                 :: !Int
  , vsc_nirrig_tmp_corn           :: !Int
  , vsc_ntrp_corn                 :: !Int
  , vsc_nirrig_trp_corn           :: !Int
  , vsc_nsugarcane                :: !Int
  , vsc_nirrig_sugarcane          :: !Int
  , vsc_nmiscanthus               :: !Int
  , vsc_nirrig_miscanthus         :: !Int
  , vsc_nswitchgrass              :: !Int
  , vsc_nirrig_switchgrass        :: !Int
  } deriving (Show, Eq)

defaultVegStructConfig :: VegStructConfig
defaultVegStructConfig = VegStructConfig
  { vsc_dt                        = 1800.0
  , vsc_use_cndv                  = False
  , vsc_use_biomass_heat_storage  = False
  , vsc_spinup_factor_deadwood    = 1.0
  , vsc_c_to_b                    = 2.0
  , vsc_noveg                     = 0
  , vsc_nc3crop                   = 15
  , vsc_nc3irrig                  = 16
  , vsc_nbrdlf_evr_shrub         = 9
  , vsc_nbrdlf_dcd_brl_shrub     = 11
  , vsc_npcropmin                 = 17
  , vsc_ntmp_corn                 = 17
  , vsc_nirrig_tmp_corn           = 18
  , vsc_ntrp_corn                 = 19
  , vsc_nirrig_trp_corn           = 20
  , vsc_nsugarcane                = 21
  , vsc_nirrig_sugarcane          = 22
  , vsc_nmiscanthus               = 23
  , vsc_nirrig_miscanthus         = 24
  , vsc_nswitchgrass              = 25
  , vsc_nirrig_switchgrass        = 26
  }

-- ========================================================================
-- Per-patch input/output record
-- ========================================================================

-- | Result of updating a single patch's vegetation structure.
-- Pure function: takes all inputs, returns updated fields.
data VegStructResult = VegStructResult
  { vsr_tlai               :: !Double
  , vsr_tsai               :: !Double
  , vsr_htop               :: !Double
  , vsr_hbot               :: !Double
  , vsr_elai               :: !Double
  , vsr_esai               :: !Double
  , vsr_frac_veg_nosno_alb :: !Int
  , vsr_htmx               :: !Double
  , vsr_peaklai            :: !Int
  , vsr_stem_biomass       :: !Double
  , vsr_leaf_biomass       :: !Double
  } deriving (Show, Eq)

-- ========================================================================
-- Main update function (pure, per-patch)
-- ========================================================================

-- | Update vegetation structure for a single patch. Pure function.
--
-- Arguments:
--   config        - VegStructConfig with PFT indices and flags
--   ivt           - PFT type (0-based Fortran index)
--   col_frac_sno  - column snow fraction
--   col_snow_depth - column snow depth [m]
--   forc_hgt_u    - forcing height for wind [m]
--   leafc         - leaf carbon [gC/m2]
--   deadstemc     - dead stem carbon [gC/m2]
--   livestemc     - live stem carbon [gC/m2]
--   farea_burned  - fraction area burned
--   harvdate      - harvest date (day of year; 999 = not harvested)
--   tlai_old      - previous tlai
--   tsai_old      - previous tsai
--   htmx_old      - previous htmx
--   peaklai_old   - previous peaklai
--   stem_biomass_old - previous stem_biomass
--   leaf_biomass_old - previous leaf_biomass
--   slatop        - specific leaf area at top [m2/gC]  (1-based PFT index)
--   dsladlai      - dSLA/dLAI (1-based PFT index)
--   z0mr          - z0/h ratio (1-based PFT index)
--   displar       - d/h ratio (1-based PFT index)
--   dwood         - wood density [gC/m3] (1-based PFT index)
--   ztopmx        - max canopy top height [m] (1-based PFT index)
--   laimx         - max LAI (1-based PFT index)
--   nstem         - stem density [#/m2] (1-based PFT index)
--   taper         - stem taper (1-based PFT index)
--   fbw           - fraction biomass that is water (1-based PFT index)
--   woody         - woody flag (1-based PFT index)
cnVegStructUpdate
  :: VegStructConfig
  -> Int         -- ivt
  -> Double      -- col_frac_sno
  -> Double      -- col_snow_depth
  -> Double      -- forc_hgt_u
  -> Double      -- leafc
  -> Double      -- deadstemc
  -> Double      -- livestemc
  -> Double      -- farea_burned
  -> Int         -- harvdate
  -> Double      -- tlai_old
  -> Double      -- tsai_old
  -> Double      -- htmx_old
  -> Int         -- peaklai_old
  -> Double      -- stem_biomass_old
  -> Double      -- leaf_biomass_old
  -- PFT parameters (already looked up at 1-based index)
  -> Double      -- slatop
  -> Double      -- dsladlai
  -> Double      -- z0mr
  -> Double      -- displar
  -> Double      -- dwood
  -> Double      -- ztopmx
  -> Double      -- laimx
  -> Double      -- nstem_pft
  -> Double      -- taper_pft
  -> Double      -- fbw_pft
  -> Double      -- woody_pft
  -> VegStructResult
cnVegStructUpdate !cfg !ivt !frac_sno !snow_depth !forc_hgt_u
  !leafc !deadstemc !livestemc !farea_burned !harvdate
  !tlai_old !tsai_old !htmx_old !peaklai_old
  !stem_biomass_old !leaf_biomass_old
  !slatop_v !dsladlai_v !z0mr_v !displar_v !dwood_v
  !ztopmx_v !laimx_v !nstem_v !taper_v !fbw_v !woody_v =
  let
    noveg     = vsc_noveg cfg
    nc3crop   = vsc_nc3crop cfg
    nc3irrig  = vsc_nc3irrig cfg
    nbrdlf_dcd_brl_shrub = vsc_nbrdlf_dcd_brl_shrub cfg
    npcropmin = vsc_npcropmin cfg
    dt        = vsc_dt cfg
    spf       = vsc_spinup_factor_deadwood cfg
    c2b       = vsc_c_to_b cfg

    -- Determine if this is a corn/sugarcane/miscanthus/switchgrass crop
    isCornLike = ivt == vsc_ntmp_corn cfg || ivt == vsc_nirrig_tmp_corn cfg
              || ivt == vsc_ntrp_corn cfg || ivt == vsc_nirrig_trp_corn cfg
              || ivt == vsc_nsugarcane cfg || ivt == vsc_nirrig_sugarcane cfg
              || ivt == vsc_nmiscanthus cfg || ivt == vsc_nirrig_miscanthus cfg
              || ivt == vsc_nswitchgrass cfg || ivt == vsc_nirrig_switchgrass cfg

    -- LAI computation
    (tlai1, tsai1, htop1, hbot1, htmx1, peaklai1, stem_b, leaf_b)
      | ivt == noveg = (0.0, 0.0, 0.0, 0.0, htmx_old, peaklai_old,
                         stem_biomass_old, leaf_biomass_old)
      | otherwise =
          let
            -- Update LAI (Thornton & Zimmerman 2007 Eq 3)
            tlai_raw = if dsladlai_v > 0.0
                       then (slatop_v * (exp (leafc * dsladlai_v) - 1.0)) / dsladlai_v
                       else slatop_v * leafc
            tlai_new = max 0.0 tlai_raw

            -- SAI (Zeng et al. 2002)
            (tsai_alpha, tsai_min0)
              | ivt == nc3crop || ivt == nc3irrig = (1.0 - 1.0 * dt / dtsmonth, 0.1)
              | otherwise                         = (1.0 - 0.5 * dt / dtsmonth, 1.0)
            tsai_min = tsai_min0 * 0.5
            tsai_new = max (tsai_alpha * tsai_old + max (tlai_old - tlai_new) 0.0) tsai_min

            -- Biomass heat storage
            lb = if vsc_use_biomass_heat_storage cfg
                 then max 0.0025 leafc * c2b * 1.0e-3 / (1.0 - fbw_v)
                 else leaf_biomass_old

            -- Height and classification
            (ht, hb, hmx, pk, sb)
              | woody_v == 1.0 =
                  let ht0 = ((3.0 * deadstemc * spf * taper_v * taper_v)
                            / (pi * nstem_v * dwood_v)) ** (1.0 / 3.0)
                      sb0 = if vsc_use_biomass_heat_storage cfg
                            then (spf * deadstemc + livestemc) * c2b * 1.0e-3 / (1.0 - fbw_v)
                            else stem_biomass_old
                      ht1 = min ht0 (forc_hgt_u / (displar_v + z0mr_v) - 3.0)
                      ht2 = max ht1 0.01
                      hb0 = max 0.0 (min 3.0 (ht2 - 1.0))
                  in (ht2, hb0, htmx_old, peaklai_old, sb0)

              | ivt >= npcropmin =
                  let pk0 = if tlai_new >= laimx_v then 1 else peaklai_old
                      ts = if isCornLike then 0.1 * tlai_new else 0.2 * tlai_new
                      -- stubble after harvest
                      (ts2, hmx0, pk1) = if harvdate < 999 && tlai_new == 0.0
                                         then (0.25 * (1.0 - farea_burned * 0.90), 0.0, 0)
                                         else (ts, htmx_old, pk0)
                      ht0 = ztopmx_v * (min (tlai_new / (laimx_v - 1.0)) 1.0) ** 2
                      hmx1 = max hmx0 ht0
                      ht1 = max 0.05 (max hmx1 ht0)
                  in (ht1, 0.02, hmx1, pk1, stem_biomass_old)

              | otherwise =
                  let ht0 = max 0.25 (tlai_new * 0.25)
                      ht1 = min ht0 (forc_hgt_u / (displar_v + z0mr_v) - 3.0)
                      ht2 = max ht1 0.01
                      hb0 = max 0.0 (min 0.05 (ht2 - 0.20))
                  in (ht2, hb0, htmx_old, peaklai_old, stem_biomass_old)

            -- Override tsai for prognostic crops
            tsai_final
              | ivt >= npcropmin =
                  let ts = if isCornLike then 0.1 * tlai_new else 0.2 * tlai_new
                  in if harvdate < 999 && tlai_new == 0.0
                     then 0.25 * (1.0 - farea_burned * 0.90)
                     else ts
              | otherwise = tsai_new

          in (tlai_new, tsai_final, ht, hb, hmx, pk, sb, lb)

    -- Snow burial adjustment
    fb = if ivt > noveg && ivt <= nbrdlf_dcd_brl_shrub
         then let ol = min (max (snow_depth - hbot1) 0.0) (htop1 - hbot1)
              in 1.0 - ol / max 1.0e-06 (htop1 - hbot1)
         else let depth_req = max 0.05 (htop1 * 0.8)
              in 1.0 - max (min snow_depth depth_req) 0.0 / depth_req

    frac_sno_adj = if frac_sno <= fracSnoThreshold then frac_sno else 1.0

    elai_out = max (tlai1 * (1.0 - frac_sno_adj) + tlai1 * fb * frac_sno_adj) 0.0
    esai_out = max (tsai1 * (1.0 - frac_sno_adj) + tsai1 * fb * frac_sno_adj) 0.0

    fvna = if (elai_out + esai_out) > 0.0 then 1 else 0
  in VegStructResult
       { vsr_tlai               = tlai1
       , vsr_tsai               = tsai1
       , vsr_htop               = htop1
       , vsr_hbot               = hbot1
       , vsr_elai               = elai_out
       , vsr_esai               = esai_out
       , vsr_frac_veg_nosno_alb = fvna
       , vsr_htmx               = htmx1
       , vsr_peaklai            = peaklai1
       , vsr_stem_biomass       = stem_b
       , vsr_leaf_biomass       = leaf_b
       }
