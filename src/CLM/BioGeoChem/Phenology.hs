{-# LANGUAGE BangPatterns #-}
-- | Phenology routines for coupled carbon-nitrogen code (CN).
-- Handles evergreen, seasonal deciduous, stress deciduous, and crop phenology.
-- Fortran: CNPhenologyMod.F90
-- Julia:   src/biogeochem/phenology.jl
module CLM.BioGeoChem.Phenology
  ( -- * Data types
    PhenologyParams(..)
  , defaultPhenologyParams
  , PhenologyState(..)
  , defaultPhenologyState
  , PftConPhenology(..)
  , PatchPhenology(..)
  , defaultPatchPhenology
  , PhenologyFluxes(..)
    -- * Constants
  , notPlanted
  , notHarvested
  , inNH, inSH
    -- * Initialization
  , phenologyInit
    -- * Phenology type dispatch
  , PhenologyType(..)
  , classifyPhenology
    -- * Evergreen phenology
  , evergreenPhenology
    -- * Seasonal deciduous
  , seasonalDecidOnset
  , seasonalDecidOffset
  , seasonalCriticalDaylength
    -- * Stress deciduous
  , stressDecidOnset
  , stressDecidOffset
    -- * Background litterfall
  , backgroundLitterfall
    -- * Main driver
  , phenologyDriver
    -- * Pool-conserving driver (storage -> transfer -> display -> litter)
  , VegPools(..)
  , phenologyAdvancePools
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Constants
-- =========================================================================

notPlanted :: Int
notPlanted = 999

notHarvested :: Int
notHarvested = 999

inNH :: Int
inNH = 1

inSH :: Int
inSH = 2

secspday :: Double
secspday = 86400.0

tfrz :: Double
tfrz = 273.15

-- =========================================================================
-- Parameters
-- =========================================================================

data PhenologyParams = PhenologyParams
  { pp_crit_dayl              :: !Double
  , pp_crit_dayl_at_high_lat  :: !Double
  , pp_crit_dayl_lat_slope    :: !Double
  , pp_ndays_off              :: !Double
  , pp_fstor2tran             :: !Double
  , pp_crit_onset_fdd         :: !Double
  , pp_crit_onset_swi         :: !Double
  , pp_soilpsi_on             :: !Double
  , pp_crit_offset_fdd        :: !Double
  , pp_crit_offset_swi        :: !Double
  , pp_soilpsi_off            :: !Double
  , pp_lwtop                  :: !Double
  , pp_phenology_soil_depth   :: !Double
  , pp_snow5d_thresh_for_onset :: !Double
  } deriving (Show, Eq)

defaultPhenologyParams :: PhenologyParams
defaultPhenologyParams = PhenologyParams
  { pp_crit_dayl = 39200.0
  , pp_crit_dayl_at_high_lat = 54000.0
  , pp_crit_dayl_lat_slope = 720.0
  , pp_ndays_off = 30.0
  , pp_fstor2tran = 0.5
  , pp_crit_onset_fdd = 15.0
  , pp_crit_onset_swi = 15.0
  , pp_soilpsi_on = -0.6
  , pp_crit_offset_fdd = 15.0
  , pp_crit_offset_swi = 15.0
  , pp_soilpsi_off = -0.8
  , pp_lwtop = 0.7
  , pp_phenology_soil_depth = 0.08
  , pp_snow5d_thresh_for_onset = 0.2
  }

-- =========================================================================
-- Module State
-- =========================================================================

data PhenologyState = PhenologyState
  { ps_dt          :: !Double
  , ps_fracday     :: !Double
  , ps_crit_dayl   :: !Double
  , ps_ndays_off   :: !Double
  , ps_fstor2tran  :: !Double
  , ps_crit_onset_fdd :: !Double
  , ps_crit_onset_swi :: !Double
  , ps_soilpsi_on  :: !Double
  , ps_crit_offset_fdd :: !Double
  , ps_crit_offset_swi :: !Double
  , ps_soilpsi_off :: !Double
  , ps_lwtop       :: !Double
  , ps_phenology_soil_layer :: !Int
  } deriving (Show, Eq)

defaultPhenologyState :: PhenologyState
defaultPhenologyState = PhenologyState
  { ps_dt = 0.0, ps_fracday = 0.0, ps_crit_dayl = 0.0
  , ps_ndays_off = 0.0, ps_fstor2tran = 0.0
  , ps_crit_onset_fdd = 0.0, ps_crit_onset_swi = 0.0
  , ps_soilpsi_on = 0.0, ps_crit_offset_fdd = 0.0
  , ps_crit_offset_swi = 0.0, ps_soilpsi_off = 0.0
  , ps_lwtop = 0.0, ps_phenology_soil_layer = 1
  }

-- =========================================================================
-- PFT Constants
-- =========================================================================

data PftConPhenology = PftConPhenology
  { pfp_evergreen      :: !(VU.Vector Double)
  , pfp_season_decid   :: !(VU.Vector Double)
  , pfp_stress_decid   :: !(VU.Vector Double)
  , pfp_woody          :: !(VU.Vector Double)
  , pfp_leaf_long      :: !(VU.Vector Double)
  , pfp_leafcn         :: !(VU.Vector Double)
  , pfp_frootcn        :: !(VU.Vector Double)
  , pfp_ndays_on       :: !(VU.Vector Double)
  , pfp_crit_onset_gdd_sf :: !(VU.Vector Double)
  } deriving (Show)

-- =========================================================================
-- Per-Patch Phenology State
-- =========================================================================

data PatchPhenology = PatchPhenology
  { -- Onset/offset state
    pph_onset_flag      :: !Bool
  , pph_onset_counter   :: !Double
  , pph_onset_gddflag   :: !Bool
  , pph_onset_gdd       :: !Double
  , pph_onset_swi_count :: !Double
  , pph_onset_fdd_count :: !Double
  , pph_offset_flag     :: !Bool
  , pph_offset_counter  :: !Double
  , pph_offset_fdd_count :: !Double
  , pph_offset_swi_count :: !Double
  , pph_dayl_prev       :: !Double
  -- GDD tracking
  , pph_gdd020          :: !Double
  , pph_gdd_base10      :: !Double
  , pph_snow5d          :: !Double
  -- Dormancy
  , pph_dormant_flag    :: !Bool
  , pph_days_active     :: !Double
  -- Leaf/froot transfer
  , pph_lgsf            :: !Double
  , pph_bglfr           :: !Double
  , pph_bgtr            :: !Double
  -- Hemisphere
  , pph_hemisphere      :: !Int
  } deriving (Show, Eq)

defaultPatchPhenology :: PatchPhenology
defaultPatchPhenology = PatchPhenology
  { pph_onset_flag = False, pph_onset_counter = 0.0
  , pph_onset_gddflag = False, pph_onset_gdd = 0.0
  , pph_onset_swi_count = 0.0, pph_onset_fdd_count = 0.0
  , pph_offset_flag = False, pph_offset_counter = 0.0
  , pph_offset_fdd_count = 0.0, pph_offset_swi_count = 0.0
  , pph_dayl_prev = 0.0
  , pph_gdd020 = 0.0, pph_gdd_base10 = 0.0, pph_snow5d = 0.0
  , pph_dormant_flag = True, pph_days_active = 0.0
  , pph_lgsf = 0.0, pph_bglfr = 0.0, pph_bgtr = 0.0
  , pph_hemisphere = inNH
  }

-- =========================================================================
-- Phenology Fluxes (output)
-- =========================================================================

data PhenologyFluxes = PhenologyFluxes
  { phf_leafc_xfer_to_leafc     :: !Double
  , phf_frootc_xfer_to_frootc   :: !Double
  , phf_livestemc_xfer_to_livestemc :: !Double
  , phf_deadstemc_xfer_to_deadstemc :: !Double
  , phf_livecrootc_xfer_to_livecrootc :: !Double
  , phf_deadcrootc_xfer_to_deadcrootc :: !Double
  , phf_leafc_to_litter         :: !Double
  , phf_frootc_to_litter        :: !Double
  , phf_onset_flag              :: !Bool
  , phf_offset_flag             :: !Bool
  } deriving (Show, Eq)

-- =========================================================================
-- Phenology Type Classification
-- =========================================================================

data PhenologyType
  = Evergreen
  | SeasonalDeciduous
  | StressDeciduous
  deriving (Show, Eq)

classifyPhenology :: PftConPhenology -> Int -> PhenologyType
classifyPhenology pfc ivt
  | ivt >= 0 && ivt < VU.length (pfp_evergreen pfc) &&
    (pfp_evergreen pfc VU.! ivt) > 0.5 = Evergreen
  | ivt >= 0 && ivt < VU.length (pfp_season_decid pfc) &&
    (pfp_season_decid pfc VU.! ivt) > 0.5 = SeasonalDeciduous
  | otherwise = StressDeciduous

-- =========================================================================
-- Initialization
-- =========================================================================

phenologyInit :: PhenologyParams -> Double -> PhenologyState
phenologyInit params dt = PhenologyState
  { ps_dt = dt
  , ps_fracday = dt / secspday
  , ps_crit_dayl = pp_crit_dayl params
  , ps_ndays_off = pp_ndays_off params
  , ps_fstor2tran = pp_fstor2tran params
  , ps_crit_onset_fdd = pp_crit_onset_fdd params
  , ps_crit_onset_swi = pp_crit_onset_swi params
  , ps_soilpsi_on = pp_soilpsi_on params
  , ps_crit_offset_fdd = pp_crit_offset_fdd params
  , ps_crit_offset_swi = pp_crit_offset_swi params
  , ps_soilpsi_off = pp_soilpsi_off params
  , ps_lwtop = pp_lwtop params / (secspday * 365.0)
  , ps_phenology_soil_layer = 1
  }

-- =========================================================================
-- Evergreen Phenology
-- =========================================================================

evergreenPhenology :: PhenologyState
                   -> Double  -- ^ leaf_long (leaf longevity, years)
                   -> Double  -- ^ leafc (leaf C, gC/m2)
                   -> Double  -- ^ frootc (fine root C, gC/m2)
                   -> (Double, Double)  -- ^ (leafc_to_litter, frootc_to_litter)
evergreenPhenology !st !leaf_long !leafc !frootc =
  let !bglfr = 1.0 / (leaf_long * 365.0 * secspday)
      !leafLit = leafc * bglfr * ps_dt st
      !frootLit = frootc * bglfr * ps_dt st
  in (leafLit, frootLit)

-- =========================================================================
-- Seasonal Deciduous Onset
-- =========================================================================

seasonalDecidOnset :: PhenologyState
                   -> PatchPhenology
                   -> Double  -- ^ dayl (current daylength, seconds)
                   -> Double  -- ^ t_ref2m (2m reference temperature, K)
                   -> Double  -- ^ snow5d_avg
                   -> Double  -- ^ snow5d_thresh
                   -> PatchPhenology
seasonalDecidOnset !st !pp !dayl !t_ref2m !snow5d !snow_thresh
  -- Already in onset, decrement counter
  | pph_onset_flag pp && pph_onset_counter pp > 0.0 =
    pp { pph_onset_counter = pph_onset_counter pp - ps_fracday st }
  -- Onset complete
  | pph_onset_flag pp =
    pp { pph_onset_flag = False, pph_onset_counter = 0.0
       , pph_dormant_flag = False }
  -- Dormant: check for onset trigger
  | pph_dormant_flag pp =
    let !gdd_incr = if t_ref2m > tfrz
                    then (t_ref2m - tfrz) * ps_fracday st
                    else 0.0
        !newGdd = pph_onset_gdd pp + gdd_incr
        !gddflag = pph_onset_gddflag pp || dayl > ps_crit_dayl st
        !critGdd = ps_crit_onset_fdd st
        !triggered = gddflag && newGdd >= critGdd && snow5d < snow_thresh
    in if triggered
       then pp { pph_onset_flag = True
               , pph_onset_counter = 15.0
               , pph_onset_gdd = 0.0
               , pph_onset_gddflag = False
               , pph_days_active = 0.0 }
       else pp { pph_onset_gdd = newGdd, pph_onset_gddflag = gddflag }
  | otherwise = pp

-- =========================================================================
-- Seasonal Deciduous Offset
-- =========================================================================

seasonalDecidOffset :: PhenologyState
                    -> PatchPhenology
                    -> Double  -- ^ dayl (current daylength, seconds)
                    -> Double  -- ^ critDayl (critical daylength for offset)
                    -> PatchPhenology
seasonalDecidOffset !st !pp !dayl !critDayl
  -- Already in offset
  | pph_offset_flag pp && pph_offset_counter pp > 0.0 =
    pp { pph_offset_counter = pph_offset_counter pp - ps_fracday st }
  -- Offset complete → go dormant
  | pph_offset_flag pp =
    pp { pph_offset_flag = False, pph_offset_counter = 0.0
       , pph_dormant_flag = True, pph_onset_gdd = 0.0 }
  -- Active: check for offset trigger (decreasing daylength below critical)
  | not (pph_dormant_flag pp) && dayl < critDayl &&
    dayl < pph_dayl_prev pp =
    pp { pph_offset_flag = True
       , pph_offset_counter = ps_ndays_off st
       , pph_dayl_prev = dayl }
  | otherwise = pp { pph_dayl_prev = dayl }

-- | Critical daylength calculation.
seasonalCriticalDaylength :: PhenologyParams -> Double -> Double
seasonalCriticalDaylength params lat =
  let critHighLat = 65.0
  in if abs lat > critHighLat
     then pp_crit_dayl_at_high_lat params
     else pp_crit_dayl params + pp_crit_dayl_lat_slope params * (abs lat - critHighLat)

-- =========================================================================
-- Stress Deciduous Onset
-- =========================================================================

stressDecidOnset :: PhenologyState
                 -> PatchPhenology
                 -> Double  -- ^ t_ref2m (2m temperature, K)
                 -> Double  -- ^ soilpsi (soil water potential, MPa)
                 -> Double  -- ^ snow5d_avg
                 -> Double  -- ^ snow5d_thresh
                 -> PatchPhenology
stressDecidOnset !st !pp !t_ref2m !soilpsi !snow5d !snow_thresh
  -- In onset phase
  | pph_onset_flag pp && pph_onset_counter pp > 0.0 =
    pp { pph_onset_counter = pph_onset_counter pp - ps_fracday st }
  | pph_onset_flag pp =
    pp { pph_onset_flag = False, pph_onset_counter = 0.0
       , pph_dormant_flag = False }
  -- Dormant: accumulate GDD and soil water stress
  | pph_dormant_flag pp =
    let !fdd_incr = if t_ref2m < tfrz then ps_fracday st else 0.0
        !swi_incr = if soilpsi > ps_soilpsi_on st then ps_fracday st else 0.0
        !newFdd = pph_onset_fdd_count pp + fdd_incr
        !newSwi = pph_onset_swi_count pp + swi_incr
        !gdd_incr = if t_ref2m > tfrz
                    then (t_ref2m - tfrz) * ps_fracday st
                    else 0.0
        !newGdd = pph_gdd020 pp + gdd_incr
        -- Onset triggered when: enough warm days (GDD) AND soil moist enough
        -- OR enough freeze-free days with adequate soil moisture
        !triggered = newSwi >= ps_crit_onset_swi st &&
                     newGdd >= ps_crit_onset_fdd st &&
                     snow5d < snow_thresh
    in if triggered
       then pp { pph_onset_flag = True
               , pph_onset_counter = 15.0
               , pph_onset_fdd_count = 0.0
               , pph_onset_swi_count = 0.0
               , pph_gdd020 = 0.0
               , pph_days_active = 0.0 }
       else pp { pph_onset_fdd_count = newFdd
               , pph_onset_swi_count = newSwi
               , pph_gdd020 = newGdd }
  | otherwise = pp

-- =========================================================================
-- Stress Deciduous Offset
-- =========================================================================

stressDecidOffset :: PhenologyState
                  -> PatchPhenology
                  -> Double  -- ^ t_ref2m (K)
                  -> Double  -- ^ soilpsi (MPa)
                  -> Double  -- ^ days_active
                  -> PatchPhenology
stressDecidOffset !st !pp !t_ref2m !soilpsi !daysActive
  -- In offset phase
  | pph_offset_flag pp && pph_offset_counter pp > 0.0 =
    pp { pph_offset_counter = pph_offset_counter pp - ps_fracday st }
  | pph_offset_flag pp =
    pp { pph_offset_flag = False, pph_offset_counter = 0.0
       , pph_dormant_flag = True
       , pph_onset_fdd_count = 0.0, pph_onset_swi_count = 0.0 }
  -- Active: accumulate stress indicators
  | not (pph_dormant_flag pp) =
    let !fdd_incr = if t_ref2m < tfrz then ps_fracday st else 0.0
        !swi_incr = if soilpsi < ps_soilpsi_off st then ps_fracday st else 0.0
        !newFdd = pph_offset_fdd_count pp + fdd_incr
        !newSwi = pph_offset_swi_count pp + swi_incr
        -- Offset when: accumulated freeze days OR drought days exceed threshold
        -- AND minimum growing season length met
        !triggered = daysActive > ps_ndays_off st &&
                     (newFdd >= ps_crit_offset_fdd st ||
                      newSwi >= ps_crit_offset_swi st)
    in if triggered
       then pp { pph_offset_flag = True
               , pph_offset_counter = ps_ndays_off st
               , pph_offset_fdd_count = 0.0
               , pph_offset_swi_count = 0.0 }
       else pp { pph_offset_fdd_count = newFdd
               , pph_offset_swi_count = newSwi
               , pph_days_active = daysActive + ps_fracday st }
  | otherwise = pp

-- =========================================================================
-- Background Litterfall
-- =========================================================================

backgroundLitterfall :: Double  -- ^ leaf_long (years)
                     -> Double  -- ^ leafc
                     -> Double  -- ^ frootc
                     -> Double  -- ^ dt (seconds)
                     -> (Double, Double)  -- ^ (leaf_litter_flux, froot_litter_flux)
backgroundLitterfall !leaf_long !leafc !frootc !dt =
  let !bglfr = 1.0 / (max 1.0 leaf_long * 365.0 * secspday)
      !leaf_lit = leafc * bglfr * dt
      !froot_lit = frootc * bglfr * dt
  in (leaf_lit, froot_lit)

-- =========================================================================
-- Onset/Offset Transfer Fluxes
-- =========================================================================

-- | Compute C transfer fluxes during onset.
onsetTransferFlux :: Double  -- ^ xfer pool (gC/m2)
                  -> Double  -- ^ onset_counter (days remaining)
                  -> Double  -- ^ dt
                  -> Double  -- ^ flux (gC/m2/timestep)
onsetTransferFlux !xfer !counter !dt
  | counter <= 0.0 = 0.0
  | otherwise = xfer * (2.0 / (counter * secspday)) * dt

-- | Compute C transfer fluxes during offset (to litter).
offsetLitterfallFlux :: Double  -- ^ pool (gC/m2)
                     -> Double  -- ^ offset_counter (days remaining)
                     -> Double  -- ^ dt
                     -> Double
offsetLitterfallFlux !pool !counter !dt
  | counter <= 0.0 = 0.0
  | otherwise = pool * (2.0 / (counter * secspday)) * dt

-- =========================================================================
-- Main Phenology Driver
-- =========================================================================

-- | Main phenology driver for a single patch.
-- Takes patch state and environment, returns updated phenology and fluxes.
phenologyDriver :: PhenologyState
                -> PftConPhenology
                -> PatchPhenology
                -> Int          -- ^ ivt (PFT index)
                -> Double       -- ^ dayl (daylength, s)
                -> Double       -- ^ t_ref2m (K)
                -> Double       -- ^ soilpsi (MPa)
                -> Double       -- ^ snow5d
                -> Double       -- ^ lat (degrees)
                -> Double       -- ^ leafc (gC/m2)
                -> Double       -- ^ frootc (gC/m2)
                -> Double       -- ^ leafc_xfer (gC/m2)
                -> Double       -- ^ frootc_xfer (gC/m2)
                -> (PatchPhenology, PhenologyFluxes)
phenologyDriver !st !pfc !pp !ivt !dayl !t_ref2m !soilpsi !snow5d !lat
                !leafc !frootc !leafc_xfer !frootc_xfer =
  let !phenType = classifyPhenology pfc ivt
      !snow_thresh = 0.2
      !dt = ps_dt st

      -- Background litterfall (applies to all types)
      !leaf_long = if ivt >= 0 && ivt < VU.length (pfp_leaf_long pfc)
                   then pfp_leaf_long pfc VU.! ivt
                   else 1.0
      (!bgLeafLit, !bgFrootLit) = backgroundLitterfall leaf_long leafc frootc dt

      -- Type-specific onset/offset
      (!pp', !onsetActive, !offsetActive) = case phenType of
        Evergreen ->
          (pp { pph_dormant_flag = False }, False, False)

        SeasonalDeciduous ->
          let !critDayl = seasonalCriticalDaylength defaultPhenologyParams lat
              !pp1 = seasonalDecidOnset st pp dayl t_ref2m snow5d snow_thresh
              !pp2 = seasonalDecidOffset st pp1 dayl critDayl
          in (pp2, pph_onset_flag pp2, pph_offset_flag pp2)

        StressDeciduous ->
          let !daysAct = pph_days_active pp
              !pp1 = stressDecidOnset st pp t_ref2m soilpsi snow5d snow_thresh
              !pp2 = stressDecidOffset st pp1 t_ref2m soilpsi daysAct
          in (pp2, pph_onset_flag pp2, pph_offset_flag pp2)

      -- Compute onset transfer fluxes
      !onsetLeaf = if onsetActive
                   then onsetTransferFlux leafc_xfer (pph_onset_counter pp') dt
                   else 0.0
      !onsetFroot = if onsetActive
                    then onsetTransferFlux frootc_xfer (pph_onset_counter pp') dt
                    else 0.0

      -- Compute offset litterfall fluxes
      !offsetLeaf = if offsetActive
                    then offsetLitterfallFlux leafc (pph_offset_counter pp') dt
                    else 0.0
      !offsetFroot = if offsetActive
                     then offsetLitterfallFlux frootc (pph_offset_counter pp') dt
                     else 0.0

      !fluxes = PhenologyFluxes
        { phf_leafc_xfer_to_leafc = onsetLeaf
        , phf_frootc_xfer_to_frootc = onsetFroot
        , phf_livestemc_xfer_to_livestemc = 0.0
        , phf_deadstemc_xfer_to_deadstemc = 0.0
        , phf_livecrootc_xfer_to_livecrootc = 0.0
        , phf_deadcrootc_xfer_to_deadcrootc = 0.0
        , phf_leafc_to_litter = bgLeafLit + offsetLeaf
        , phf_frootc_to_litter = bgFrootLit + offsetFroot
        , phf_onset_flag = onsetActive
        , phf_offset_flag = offsetActive
        }

  in (pp', fluxes)

-- =========================================================================
-- Pool-conserving driver: storage -> transfer -> display -> litter
-- =========================================================================

-- | Leaf and fine-root carbon and nitrogen pools for one patch, carried across
-- the three phenological compartments. Mirrors the cnveg carbon/nitrogen
-- storage/transfer/display triplet for leaf and froot (CNVegCarbonStateType /
-- CNVegNitrogenStateType). Litter is the cumulative leaf/froot-to-litter sink
-- produced by background turnover plus phenological offset.
data VegPools = VegPools
  { vp_leafc           :: !Double  -- ^ displayed leaf C
  , vp_leafc_storage   :: !Double
  , vp_leafc_xfer      :: !Double
  , vp_frootc          :: !Double  -- ^ displayed fine-root C
  , vp_frootc_storage  :: !Double
  , vp_frootc_xfer     :: !Double
  , vp_leafn           :: !Double
  , vp_leafn_storage   :: !Double
  , vp_leafn_xfer      :: !Double
  , vp_frootn          :: !Double
  , vp_frootn_storage  :: !Double
  , vp_frootn_xfer     :: !Double
  , vp_leafc_to_litter :: !Double  -- ^ accumulated leaf C litterfall this step
  , vp_frootc_to_litter :: !Double
  , vp_leafn_to_litter :: !Double
  , vp_frootn_to_litter :: !Double
  } deriving (Show, Eq)

-- | Advance the leaf/froot C and N pools of one patch through one phenology
-- timestep, conserving total mass.
--
-- The Fortran chain (CNPhenologyMod.F90 + CNCStateUpdate1 / CNNStateUpdate1):
--
--   * dormant patch crosses its onset trigger  ->  a @fstor2tran@ fraction of
--     each storage pool is moved into the matching transfer (xfer) pool;
--   * during the onset window the transfer pool feeds the displayed leaf/froot
--     pool at rate @2 / onset_counter@;
--   * during the offset window the displayed leaf/froot pool is shed to litter
--     at rate @2 / offset_counter@;
--   * outside onset/offset, evergreen and (post-onset) deciduous patches lose a
--     small @bglfr@ background litterfall fraction of the displayed pool.
--
-- Every transfer removes exactly what it deposits (clamped so no pool goes
-- negative), so leaf+froot C summed over display+storage+xfer+litter is
-- invariant, and likewise for N.
phenologyAdvancePools :: PhenologyState
                      -> PftConPhenology
                      -> PatchPhenology
                      -> Int          -- ^ ivt (PFT index)
                      -> Double       -- ^ dayl (daylength, s)
                      -> Double       -- ^ prev_dayl (previous-step daylength, s)
                      -> Double       -- ^ t_ref2m (K)
                      -> Double       -- ^ soilpsi (MPa)
                      -> Double       -- ^ snow5d
                      -> Double       -- ^ lat (degrees)
                      -> VegPools
                      -> (PatchPhenology, VegPools)
phenologyAdvancePools !st !pfc !ppIn !ivt !dayl !prevDayl !t_ref2m !soilpsi
                      !snow5d !lat !pools0 =
  let !phenType = classifyPhenology pfc ivt
      !snow_thresh = 0.2
      !dt = ps_dt st

      !leaf_long = if ivt >= 0 && ivt < VU.length (pfp_leaf_long pfc)
                   then pfp_leaf_long pfc VU.! ivt
                   else 1.0

      -- Detect a dormant->onset transition this step so we can fire the
      -- one-time storage->transfer shift (Fortran sets *_storage_to_xfer only
      -- on the step onset_flag flips to 1).
      !wasDormant = pph_dormant_flag ppIn

      -- 1. Advance the phenological state machine for this PFT.
      (!pp1) = case phenType of
        Evergreen ->
          ppIn { pph_dormant_flag = False }
        SeasonalDeciduous ->
          let !critDayl = seasonalCriticalDaylength defaultPhenologyParams lat
              !ppA = seasonalDecidOnset st (ppIn { pph_dayl_prev = prevDayl })
                       dayl t_ref2m snow5d snow_thresh
              !ppB = seasonalDecidOffset st ppA dayl critDayl
          in ppB
        StressDeciduous ->
          let !daysAct = pph_days_active ppIn
              !ppA = stressDecidOnset st ppIn t_ref2m soilpsi snow5d snow_thresh
              !ppB = stressDecidOffset st ppA t_ref2m soilpsi daysAct
          in ppB

      !justEnteredOnset = pph_onset_flag pp1 && wasDormant

      -- 2. storage -> transfer (one-time at onset trigger).
      !fstor = ps_fstor2tran st
      (!pAfterStor) =
        if justEnteredOnset
          then let move s = fstor * s
                   !dLeafCs  = move (vp_leafc_storage pools0)
                   !dFrootCs = move (vp_frootc_storage pools0)
                   !dLeafNs  = move (vp_leafn_storage pools0)
                   !dFrootNs = move (vp_frootn_storage pools0)
               in pools0
                    { vp_leafc_storage  = vp_leafc_storage pools0  - dLeafCs
                    , vp_leafc_xfer     = vp_leafc_xfer pools0     + dLeafCs
                    , vp_frootc_storage = vp_frootc_storage pools0 - dFrootCs
                    , vp_frootc_xfer    = vp_frootc_xfer pools0    + dFrootCs
                    , vp_leafn_storage  = vp_leafn_storage pools0  - dLeafNs
                    , vp_leafn_xfer     = vp_leafn_xfer pools0     + dLeafNs
                    , vp_frootn_storage = vp_frootn_storage pools0 - dFrootNs
                    , vp_frootn_xfer    = vp_frootn_xfer pools0    + dFrootNs
                    }
          else pools0

      -- 3. transfer -> display (during onset window).
      !onsetActive = pph_onset_flag pp1 && pph_onset_counter pp1 > 0.0
      !onsetCounter = pph_onset_counter pp1
      xferToDisp xfer = if onsetActive
                        then min xfer (onsetTransferFlux xfer onsetCounter dt)
                        else 0.0
      !dLeafCx  = xferToDisp (vp_leafc_xfer pAfterStor)
      !dFrootCx = xferToDisp (vp_frootc_xfer pAfterStor)
      !dLeafNx  = xferToDisp (vp_leafn_xfer pAfterStor)
      !dFrootNx = xferToDisp (vp_frootn_xfer pAfterStor)
      !pAfterXfer = pAfterStor
        { vp_leafc_xfer  = vp_leafc_xfer pAfterStor  - dLeafCx
        , vp_leafc       = vp_leafc pAfterStor       + dLeafCx
        , vp_frootc_xfer = vp_frootc_xfer pAfterStor - dFrootCx
        , vp_frootc      = vp_frootc pAfterStor      + dFrootCx
        , vp_leafn_xfer  = vp_leafn_xfer pAfterStor  - dLeafNx
        , vp_leafn       = vp_leafn pAfterStor       + dLeafNx
        , vp_frootn_xfer = vp_frootn_xfer pAfterStor - dFrootNx
        , vp_frootn      = vp_frootn pAfterStor      + dFrootNx
        }

      -- 4. display -> litter.
      !offsetActive = pph_offset_flag pp1 && pph_offset_counter pp1 > 0.0
      !offsetCounter = pph_offset_counter pp1
      -- Offset litterfall (gradual shed of the displayed pool).
      offsetLit pool = if offsetActive
                       then min pool (offsetLitterfallFlux pool offsetCounter dt)
                       else 0.0
      -- Background turnover applies when NOT shedding at offset: evergreen
      -- always, deciduous only while displaying leaves (active, post-onset).
      !backgroundOn = not offsetActive &&
                      (phenType == Evergreen || not (pph_dormant_flag pp1))
      bgLit pool = if backgroundOn
                   then let (l, _) = backgroundLitterfall leaf_long pool 0.0 dt
                        in min pool l
                   else 0.0
      litLeafC  = offsetLit (vp_leafc pAfterXfer)  + bgLit (vp_leafc pAfterXfer)
      litFrootC = offsetLit (vp_frootc pAfterXfer) + bgLit (vp_frootc pAfterXfer)
      -- N litterfall mirrors the C litterfall fraction so leaf/froot C:N of the
      -- litter flux matches the displayed pool (no retranslocation modelled
      -- here; all displayed N follows C to litter, conserving N).
      fracLeaf  = if vp_leafc pAfterXfer  > 0.0 then litLeafC  / vp_leafc pAfterXfer  else 0.0
      fracFroot = if vp_frootc pAfterXfer > 0.0 then litFrootC / vp_frootc pAfterXfer else 0.0
      litLeafN  = min (vp_leafn pAfterXfer)  (fracLeaf  * vp_leafn pAfterXfer)
      litFrootN = min (vp_frootn pAfterXfer) (fracFroot * vp_frootn pAfterXfer)

      !poolsOut = pAfterXfer
        { vp_leafc  = vp_leafc pAfterXfer  - litLeafC
        , vp_frootc = vp_frootc pAfterXfer - litFrootC
        , vp_leafn  = vp_leafn pAfterXfer  - litLeafN
        , vp_frootn = vp_frootn pAfterXfer - litFrootN
        , vp_leafc_to_litter  = litLeafC
        , vp_frootc_to_litter = litFrootC
        , vp_leafn_to_litter  = litLeafN
        , vp_frootn_to_litter = litFrootN
        }

  in (pp1, poolsOut)
