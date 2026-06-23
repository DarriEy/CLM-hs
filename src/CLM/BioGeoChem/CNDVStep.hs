{-# LANGUAGE BangPatterns #-}
-- | CNDV step glue: the annual dynamic-vegetation driver plus the
-- every-timestep climate accumulators that feed it.
--
-- This wires the already-ported leaf-level CNDV physics ('calcEstablishment',
-- 'lightCompetition', 'calcMortality') into a driver that operates on the
-- carried 'DGVSData' state, advancing it one timestep at a time.
--
-- Cadence (faithful to the Fortran reference):
--   * Every timestep: accumulate agdd (base 5C), agddtw (above twmax),
--     a 30-day running-mean t_mo, a 365-day running-mean prec365, the
--     within-year NPP sum, and the annual max leaf C.
--   * Year boundary (csi_is_annual): finalize annsum_npp, advance the
--     20-year running means tmomin20/agdd20 with the (19*old + new)/20
--     weighting, run Light -> Establishment -> Mortality, then reset the
--     annual accumulators (agdd, agddtw, t_mo_min, leafcmax, tempsum_npp).
--
-- Fortran: CNDVDriverMod.F90, CNDVType.F90 (UpdateAccVars),
--          CNDVEstablishmentMod.F90, CNDVLightMod.F90.
--
-- NOTE: the running means (t_a10, t_mo, prec365) use an exponential-moving-
-- average with weight dt/(period*secs_per_day). This is the steady-state
-- equivalent of CLM's boxcar 'runmean' accumulators (which converge to the same
-- 1/period weighting once the period has filled); it avoids carrying a per-step
-- ring buffer. agddtw is driven by the tracked 10-day mean t_a10.
module CLM.BioGeoChem.CNDVStep
  ( -- * Constants
    tkfrz, cday, gddBase, rampAgddtw, twmaxOff
    -- * Step input
  , CNDVStepInput(..)
    -- * Driver
  , cndvStepAdvance
    -- * Cold-start seed
  , seedDGVS
    -- * PFT bioclimatic limits
  , dgvmPftBioclim
  , isWoodyPFT
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Types.DGVSData (DGVSData(..), defaultDGVSData)
import CLM.BioGeoChem.CNDV
  ( EstablishmentInput(..), EstablishmentOutput(..), calcEstablishment
  , MortalityInput(..), MortalityOutput(..), calcMortality
  , lightCompetition )

-- =========================================================================
-- Constants
-- =========================================================================

-- | Freezing point (K).
tkfrz :: Double
tkfrz = 273.15

-- | Seconds per day.
cday :: Double
cday = 86400.0

-- | GDD base temperature offset above freezing (5 C).
gddBase :: Double
gddBase = 5.0

-- | Heat-stress ramp for agddtw -> heatstress fraction (degree-days).
rampAgddtw :: Double
rampAgddtw = 300.0

-- | "No warmth limit" sentinel: twmax >= this means the PFT has no warmest-
-- month upper limit (Fortran uses 999).
twmaxOff :: Double
twmaxOff = 999.0

-- =========================================================================
-- PFT bioclimatic limits
-- =========================================================================

-- | Per-PFT bioclimatic establishment/survival limits, returned as
-- (tcmin, tcmax, gddmin, twmax) with temperatures in deg C and gddmin in
-- degree-days above 5 C. twmax >= 1000 means "no warmest-month limit";
-- tcmin <= -1000 means "no cold limit".
--
-- CLM reads these (pftpar28/29/30/31) from the NetCDF parameter file at runtime;
-- this port does not load that file, so the table below uses the canonical
-- LPJ/CLM-DGVM values (Sitch et al. 2003 Table 1; Levis et al. 2004), keyed by
-- the CLM natural-PFT index. If the pftcon param file is wired in later, those
-- values should override this table. The CLM PFT ordering used here:
--   1-8  trees (1 ndl-evg-temp, 2 ndl-evg-bor, 3 ndl-dcd-bor, 4 bdl-evg-trop,
--                5 bdl-evg-temp, 6 bdl-dcd-trop, 7 bdl-dcd-temp, 8 bdl-dcd-bor)
--   9-11 shrubs   12-14 grasses (12 c3-arctic, 13 c3, 14 c4)   15+ crops
dgvmPftBioclim :: Int -> (Double, Double, Double, Double)
dgvmPftBioclim ivt = case ivt of
  1  -> (  -2.0,   22.0,  900.0, 1000.0)  -- needleleaf evergreen temperate
  2  -> ( -32.5,   -2.0,  600.0,   23.0)  -- needleleaf evergreen boreal
  3  -> (nolim,    -2.0,  350.0,   23.0)  -- needleleaf deciduous boreal
  4  -> (  15.5, 1000.0,    0.0, 1000.0)  -- broadleaf evergreen tropical
  5  -> (   3.0,   18.8, 1200.0, 1000.0)  -- broadleaf evergreen temperate
  6  -> (  15.5, 1000.0,    0.0, 1000.0)  -- broadleaf deciduous tropical
  7  -> ( -17.0,   15.5, 1200.0, 1000.0)  -- broadleaf deciduous temperate
  8  -> (nolim,    -2.0,  350.0,   23.0)  -- broadleaf deciduous boreal
  9  -> (   3.0,   18.8, 1200.0, 1000.0)  -- broadleaf evergreen shrub
  10 -> ( -17.0,   15.5, 1000.0, 1000.0)  -- broadleaf deciduous temperate shrub
  11 -> (nolim,    -2.0,  350.0,   23.0)  -- broadleaf deciduous boreal shrub
  12 -> (nolim,  1000.0,    0.0, 1000.0)  -- c3 arctic grass
  13 -> (nolim,  1000.0,    0.0, 1000.0)  -- c3 non-arctic grass
  14 -> (  15.5, 1000.0,    0.0, 1000.0)  -- c4 grass (needs warmth)
  _  -> (nolim,  1000.0,    0.0, 1000.0)  -- crops / unknown: no climate limit
  where nolim = -1000.0

-- | Woody (tree or shrub) PFT classification: CLM natural-PFT indices 1-11.
isWoodyPFT :: Int -> Bool
isWoodyPFT ivt = ivt >= 1 && ivt <= 11

-- =========================================================================
-- Step input
-- =========================================================================

-- | Per-timestep inputs for the CNDV step. All per-patch vectors share the
-- patch ordering of the carried 'DGVSData'. PFT-dependent parameters are
-- resolved to per-patch vectors by the caller (from 'DGVEcophysCon').
data CNDVStepInput = CNDVStepInput
  { csi_is_annual  :: !Bool             -- ^ year-boundary step (run annual driver)
  , csi_kyr        :: !Int              -- ^ simulation year index (1-based)
  , csi_dt         :: !Double           -- ^ timestep (s)
    -- Forcing / fluxes (per patch)
  , csi_t_ref2m    :: !(VU.Vector Double) -- ^ 2m reference temperature (K)
  , csi_rain_snow  :: !(VU.Vector Double) -- ^ rain + snow rate (mm/s)
  , csi_npp        :: !(VU.Vector Double) -- ^ NPP (gC/m2/s)
  , csi_leafc      :: !(VU.Vector Double) -- ^ current leaf C (gC/m2)
    -- PFT parameters (per patch, resolved from DGVEcophysCon)
  , csi_tcmin      :: !(VU.Vector Double) -- ^ min coldest-month T (C)
  , csi_tcmax      :: !(VU.Vector Double) -- ^ max coldest-month T (C)
  , csi_gddmin     :: !(VU.Vector Double) -- ^ min GDD (degree-days)
  , csi_twmax      :: !(VU.Vector Double) -- ^ warmest-month T limit (C); >=999 = none
  , csi_is_tree    :: !(VU.Vector Bool)   -- ^ woody/tree PFT flag
  } deriving (Show)

-- =========================================================================
-- Driver
-- =========================================================================

-- | Advance the DGVS state by one timestep: run the annual driver first
-- (consuming the accumulators as they stand from the year just finished),
-- then apply this step's accumulation. A no-op when there are no patches.
cndvStepAdvance :: CNDVStepInput -> DGVSData -> DGVSData
cndvStepAdvance !inp !dgvs
  | VU.null (dgvs_nind_patch dgvs) = dgvs
  | otherwise =
      let !dgvsA = if csi_is_annual inp then cndvAnnual inp dgvs else dgvs
      in cndvAccumStep inp dgvsA

-- | Every-timestep accumulation.
cndvAccumStep :: CNDVStepInput -> DGVSData -> DGVSData
cndvAccumStep !inp !d =
  let !dt = csi_dt inp
      !w10  = dt / (10.0  * cday)  -- 10-day running-mean weight (t_a10)
      !w30  = dt / (30.0  * cday)  -- 30-day running-mean weight (t_mo)
      !w365 = dt / (365.0 * cday)  -- 365-day running-mean weight (prec365)
      !t2m  = csi_t_ref2m inp
      !pr   = csi_rain_snow inp
      !npp  = csi_npp inp
      !lc   = csi_leafc inp
      !twm  = csi_twmax inp

      !agdd' = VU.zipWith (\a t -> a + max 0.0 ((t - (tkfrz + gddBase)) * dt / cday))
                 (dgvs_agdd_patch d) t2m
      -- 10-day running mean of 2m temperature; feeds agddtw (Fortran: t_a10).
      !ta10' = VU.zipWith (\o t -> o + (t - o) * w10) (dgvs_t_a10_patch d) t2m
      -- agddtw accumulates degree-days of the 10-day mean above twmax.
      !agddtw' = VU.izipWith (\i a ta ->
                   let !tw = twm VU.! i
                   in a + max 0.0 ((ta - tkfrz - tw) * dt / cday))
                   (dgvs_agddtw_patch d) ta10'
      !tmo' = VU.zipWith (\o t -> o + (t - o) * w30) (dgvs_t_mo_patch d) t2m
      !tmoMin' = VU.zipWith min (dgvs_t_mo_min_patch d) tmo'
      !prec' = VU.zipWith (\o p -> o + (p - o) * w365) (dgvs_prec365_patch d) pr
      !tempsum' = VU.zipWith (+) (dgvs_tempsum_npp_patch d) npp
      !leafcmax' = VU.zipWith max (dgvs_leafcmax_patch d) lc
  in d { dgvs_agdd_patch = agdd'
       , dgvs_agddtw_patch = agddtw'
       , dgvs_t_a10_patch = ta10'
       , dgvs_t_mo_patch = tmo'
       , dgvs_t_mo_min_patch = tmoMin'
       , dgvs_prec365_patch = prec'
       , dgvs_tempsum_npp_patch = tempsum'
       , dgvs_leafcmax_patch = leafcmax'
       }

-- | Annual driver: finalize NPP, advance 20-year means, run
-- Light -> Establishment -> Mortality, reset annual accumulators.
cndvAnnual :: CNDVStepInput -> DGVSData -> DGVSData
cndvAnnual !inp !d =
  let !dt = csi_dt inp
      !kyr = csi_kyr inp
      !n = VU.length (dgvs_nind_patch d)
      !twm = csi_twmax inp
      !isTree = csi_is_tree inp

      -- Finalize annual NPP from the within-year sum (Fortran: annsum = tempsum*dt).
      !annsum = VU.map (* dt) (dgvs_tempsum_npp_patch d)

      -- Heat stress fraction from agddtw (Fortran CNDVEstablishment:406-410).
      !heat = VU.imap (\i agt ->
                let !tw = twm VU.! i
                in if tw < twmaxOff
                   then max 0.0 (min 1.0 (agt / rampAgddtw))
                   else 0.0)
                (dgvs_agddtw_patch d)

      -- 20-year running means: year 2 initializes; thereafter (19*old + new)/20.
      !mean20 = VU.zipWith (\old newv ->
                  if kyr == 2 then newv else (19.0 * old + newv) / 20.0)
      !tmomin20' = mean20 (dgvs_tmomin20_patch d) (dgvs_t_mo_min_patch d)
      !agdd20' = mean20 (dgvs_agdd20_patch d) (dgvs_agdd_patch d)

      -- Step 1: light competition across present tree patches.
      !lcIn = [ (i, dgvs_fpcgrid_patch d VU.! i, dgvs_nind_patch d VU.! i, isTree VU.! i)
              | i <- [0 .. n - 1] ]
      !lcOut = lightCompetition lcIn
      -- index -> (fpcgrid, nind)
      !fpcAfterLight = VU.generate n (\i -> let (_, f, _) = lcOut !! i in f)
      !nindAfterLight = VU.generate n (\i -> let (_, _, nn) = lcOut !! i in nn)

      -- Step 2 & 3: per-patch establishment (survival filter) then mortality.
      results = [ patchAnnual inp d i annsum heat fpcAfterLight nindAfterLight
                | i <- [0 .. n - 1] ]
      !nind' = VU.fromList [ rNind r | r <- results ]
      !fpc'  = VU.fromList [ rFpc r  | r <- results ]
      !greff' = VU.fromList [ rGreffic r | r <- results ]

  in d { dgvs_annsum_npp_patch = annsum
       , dgvs_heatstress_patch = heat
       , dgvs_tmomin20_patch = tmomin20'
       , dgvs_agdd20_patch = agdd20'
       , dgvs_nind_patch = nind'
       , dgvs_fpcgrid_patch = fpc'
       , dgvs_greffic_patch = greff'
       , dgvs_fpcgridold_patch = dgvs_fpcgrid_patch d
       -- present_patch is a Bool vector omitted from DGVSData by convention;
       -- a killed patch is recorded by nind -> 0 and fpcgrid -> 0.
       -- Reset annual accumulators for the new year.
       , dgvs_agdd_patch = VU.replicate n 0.0
       , dgvs_agddtw_patch = VU.replicate n 0.0
       , dgvs_t_mo_min_patch = VU.replicate n 1.0e36
       , dgvs_leafcmax_patch = VU.replicate n 0.0
       , dgvs_tempsum_npp_patch = VU.replicate n 0.0
       }

-- | Per-patch annual establishment + mortality result.
data PatchAnnual = PatchAnnual
  { rNind    :: !Double
  , rFpc     :: !Double
  , rPresent :: !Bool
  , rGreffic :: !Double
  }

patchAnnual :: CNDVStepInput -> DGVSData -> Int
            -> VU.Vector Double  -- annsum_npp
            -> VU.Vector Double  -- heat
            -> VU.Vector Double  -- fpc after light
            -> VU.Vector Double  -- nind after light
            -> PatchAnnual
patchAnnual !inp !d !i !annsum !heat !fpcL !nindL =
  let !present0 = dgvs_nind_patch d VU.! i > 0.0
      !greffic = dgvs_greffic_patch d VU.! i
      !isTree = csi_is_tree inp VU.! i
      !twm = csi_twmax inp VU.! i

      !ei = EstablishmentInput
        { esi_present    = present0
        , esi_prec365    = dgvs_prec365_patch d VU.! i
        , esi_tcold      = dgvs_tmomin20_patch d VU.! i - tkfrz  -- K -> C
        , esi_twarm      = if dgvs_agddtw_patch d VU.! i == 0.0 then twm else twm + 1.0
        , esi_gdd        = dgvs_agdd20_patch d VU.! i
        , esi_tcmax      = csi_tcmax inp VU.! i
        , esi_tcmin      = csi_tcmin inp VU.! i
        , esi_twmax      = twm
        , esi_gddmin     = csi_gddmin inp VU.! i
        , esi_is_woody   = isTree
        , esi_annsum_npp = annsum VU.! i
        }
      !eo = calcEstablishment ei
      -- A patch that fails the survival filter is removed.
      !survived = eso_survive eo
      !nind1 = if survived then nindL VU.! i else 0.0
      !fpc1  = if survived then fpcL VU.! i else 0.0

      !mi = MortalityInput
        { mi_greffic    = greffic
        , mi_heatstress = heat VU.! i
        , mi_agddtw     = dgvs_agddtw_patch d VU.! i
        , mi_nind       = nind1
        , mi_fpcgrid    = fpc1
        , mi_is_tree    = isTree
        , mi_present    = survived && present0
        }
      !mo = calcMortality mi
      !nind2 = mo_nind_new mo
      !killed = mo_killed mo || not survived
      !fpc2 = if killed then 0.0
              else if nind1 > 0.0 then fpc1 * (nind2 / nind1) else fpc1
  in PatchAnnual
     { rNind = nind2
     , rFpc = fpc2
     , rPresent = not killed && present0
     , rGreffic = greffic
     }

-- =========================================================================
-- Cold-start seed
-- =========================================================================

-- | Seed a single-patch DGVS state for a cold start, given an initial
-- temperature (K) and individual density. All accumulators start neutral:
-- t_mo at the initial temperature, t_mo_min at +inf sentinel, the rest zero.
seedDGVS :: Int       -- ^ number of patches
         -> Double    -- ^ initial 2m temperature (K) for t_mo
         -> Double    -- ^ initial nind (#/m2)
         -> Double    -- ^ initial fpcgrid
         -> DGVSData
seedDGVS np t0 nind0 fpc0 = defaultDGVSData
  { dgvs_agdd_patch        = VU.replicate np 0.0
  , dgvs_agddtw_patch      = VU.replicate np 0.0
  , dgvs_agdd20_patch      = VU.replicate np 0.0
  , dgvs_tmomin20_patch    = VU.replicate np t0
  , dgvs_t_mo_patch        = VU.replicate np t0
  , dgvs_t_a10_patch       = VU.replicate np t0
  , dgvs_t_mo_min_patch    = VU.replicate np 1.0e36
  , dgvs_prec365_patch     = VU.replicate np 0.0
  , dgvs_annsum_npp_patch  = VU.replicate np 0.0
  , dgvs_tempsum_npp_patch  = VU.replicate np 0.0
  , dgvs_leafcmax_patch    = VU.replicate np 0.0
  , dgvs_nind_patch        = VU.replicate np nind0
  , dgvs_lm_ind_patch      = VU.replicate np 0.0
  , dgvs_lai_ind_patch     = VU.replicate np 0.0
  , dgvs_fpcinc_patch      = VU.replicate np 0.0
  , dgvs_fpcgrid_patch     = VU.replicate np fpc0
  , dgvs_fpcgridold_patch  = VU.replicate np fpc0
  , dgvs_crownarea_patch   = VU.replicate np 0.0
  , dgvs_greffic_patch     = VU.replicate np 1.0
  , dgvs_heatstress_patch  = VU.replicate np 0.0
  }
