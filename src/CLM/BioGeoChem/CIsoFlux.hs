{-# LANGUAGE BangPatterns #-}
-- | Carbon isotopic flux variable update, non-mortality fluxes.
--
-- Core isotopic flux calculations for C13 and C14 tracers. The isotopic
-- flux is computed as: ciso_flux = ctot_flux * (ciso_state / ctot_state) * frax
-- where frax is a fractionation factor (doubled deviation for C14).
--
-- Ported from: CNCIsoFluxMod.F90
-- Julia:       src/biogeochem/c_iso_flux.jl
--
-- Public functions:
--   cIsoFluxCalc1d       -- Core 1D isotopic flux calculation
--   cIsoFluxCalc2dFlux   -- 2D wrapper (flux 2D, state 1D)
--   cIsoFluxCalc2dBoth   -- 2D wrapper (both flux and state 2D)
--   isoFluxPair          -- Compute a single isotopic flux pair
--   decompCascadeIsoFlux -- Column-level decomposition isotopic flux
--   litterToColumnIso    -- Phenology litterfall patch-to-column
--   gapPftToColumnIso    -- Gap mortality patch-to-column
--   harvestPftToColumnIso -- Harvest mortality patch-to-column
--   grossUnrepPftToColumnIso -- Gross unrep. LC change patch-to-column
--
module CLM.BioGeoChem.CIsoFlux
  ( -- * Types
    Isotope(..)
    -- * Core flux calculation
  , isoFluxPair
  , cIsoFluxCalc1d
  , cIsoFluxCalc2dFlux
  , cIsoFluxCalc2dBoth
    -- * Column-level decomposition
  , decompCascadeIsoFlux
    -- * Patch-to-column aggregation helpers
  , litterToColumnContrib
  , gapMortalityContrib
  , harvestMortalityContrib
  , grossUnrepContrib
    -- * Phase drivers
  , CIsoFlux1Input(..)
  , CIsoFlux1Output(..)
  , cIsoFlux1
  , CIsoFlux2Input(..)
  , CIsoFlux2Output(..)
  , cIsoFlux2
  , cIsoFlux3
    -- * Discrimination
  , photosyntheticDiscrimination
  , c14DecayFactor
  ) where

import qualified Data.Vector.Unboxed as U

-- ========================================================================
-- Types
-- ========================================================================

-- | Isotope species tag
data Isotope = C13 | C14
  deriving (Show, Eq)

-- ========================================================================
-- Core isotopic flux calculation (pure, element-wise)
-- ========================================================================

-- | Compute the fractionation multiplier for the given isotope.
fractionationFactor :: Isotope -> Double -> Double
fractionationFactor C13 fraxC13 = fraxC13
fractionationFactor C14 fraxC13 = 1.0 + (1.0 - fraxC13) * 2.0

-- | Compute a single isotopic flux value. Pure function.
--
-- isoFlux = totFlux * (isoState / totState) * frax
-- Returns 0 if either state is zero.
isoFluxPair
  :: Isotope   -- isotope species
  -> Double    -- frax_c13 (C13 fractionation factor)
  -> Double    -- ctot_flux
  -> Double    -- ciso_state
  -> Double    -- ctot_state
  -> Double
isoFluxPair !iso !fraxC13 !totFlux !isoState !totState =
  let !frax = fractionationFactor iso fraxC13
  in if totState /= 0.0 && isoState /= 0.0
     then totFlux * (isoState / totState) * frax
     else 0.0

-- | Apply isotopic flux calculation to parallel 1D vectors.
-- For each index in the mask, computes:
--   ciso_flux[i] = ctot_flux[i] * (ciso_state[i] / ctot_state[i]) * frax
cIsoFluxCalc1d
  :: Isotope
  -> Double              -- frax_c13
  -> U.Vector Double     -- ctot_flux
  -> U.Vector Double     -- ciso_state
  -> U.Vector Double     -- ctot_state
  -> U.Vector Bool       -- mask
  -> U.Vector Double     -- result: ciso_flux
cIsoFluxCalc1d !iso !fraxC13 !totFlux !isoState !totState !mask =
  let !frax = fractionationFactor iso fraxC13
      compute i msk =
        if msk
        then let tf = totFlux  U.! i
                 is = isoState U.! i
                 ts = totState U.! i
             in if ts /= 0.0 && is /= 0.0
                then tf * (is / ts) * frax
                else 0.0
        else 0.0
  in U.imap compute mask

-- | 2D flux wrapper: flux arrays are 2D (first dim x nk), state arrays are 1D.
-- Returns result for a single slice k.
cIsoFluxCalc2dFlux
  :: Isotope
  -> Double              -- frax_c13
  -> U.Vector Double     -- ctot_flux (slice k)
  -> U.Vector Double     -- ciso_state (1D)
  -> U.Vector Double     -- ctot_state (1D)
  -> U.Vector Bool       -- mask
  -> U.Vector Double
cIsoFluxCalc2dFlux = cIsoFluxCalc1d

-- | 2D wrapper: both flux and state arrays are 2D.
-- Returns result for a single slice k.
cIsoFluxCalc2dBoth
  :: Isotope
  -> Double              -- frax_c13
  -> U.Vector Double     -- ctot_flux (slice k)
  -> U.Vector Double     -- ciso_state (slice k)
  -> U.Vector Double     -- ctot_state (slice k)
  -> U.Vector Bool       -- mask
  -> U.Vector Double
cIsoFluxCalc2dBoth = cIsoFluxCalc1d

-- ========================================================================
-- Column-level decomposition isotopic flux (pure, per-element)
-- ========================================================================

-- | Compute decomposition cascade isotopic flux for a single (c,j,l) element.
--
-- If the donor pool state is nonzero:
--   iso_flux = tot_flux * (iso_donor_state / tot_donor_state)
-- Otherwise returns 0.
decompCascadeIsoFlux
  :: Double    -- tot_flux (hr or ctransfer for this c,j,l)
  -> Double    -- iso_donor_state (iso decomp pool for donor)
  -> Double    -- tot_donor_state (total decomp pool for donor)
  -> Double
decompCascadeIsoFlux !totFlux !isoDonor !totDonor
  | totDonor /= 0.0 = totFlux * (isoDonor / totDonor)
  | otherwise        = 0.0

-- ========================================================================
-- Patch-to-column aggregation (pure, per-element contributions)
-- ========================================================================

-- | Compute litter-to-column contribution for a single patch at level j
-- and litter pool i. Returns the weighted flux to add to column accumulator.
--
-- contrib = (leafc_to_litter * lf_frac + frootc_to_litter * fr_frac) * wtcol * prof
litterToColumnContrib
  :: Double    -- iso leafc_to_litter
  -> Double    -- iso frootc_to_litter
  -> Double    -- lf_f[ivt+1, i] (leaf litter fraction for pool i)
  -> Double    -- fr_f[ivt+1, i] (fine root litter fraction for pool i)
  -> Double    -- wtcol (patch weight on column)
  -> Double    -- leaf_prof[p, j]
  -> Double    -- froot_prof[p, j]
  -> Double
litterToColumnContrib !leafFlux !frootFlux !lfFrac !frFrac !wtcol !leafProf !frootProf =
  (leafFlux * lfFrac * wtcol * leafProf) + (frootFlux * frFrac * wtcol * frootProf)

-- | Compute gap mortality contribution to CWD column pool at level j.
-- Returns weighted flux from stem and coarse root mortality.
gapMortalityContrib
  :: Double    -- iso m_livestemc_to_litter
  -> Double    -- iso m_deadstemc_to_litter
  -> Double    -- iso m_livecrootc_to_litter
  -> Double    -- iso m_deadcrootc_to_litter
  -> Double    -- wtcol
  -> Double    -- stem_prof
  -> Double    -- croot_prof
  -> Double
gapMortalityContrib !liveStm !deadStm !liveCrt !deadCrt !wtcol !stemProf !crootProf =
  (liveStm * wtcol * stemProf) + (deadStm * wtcol * stemProf) +
  (liveCrt * wtcol * crootProf) + (deadCrt * wtcol * crootProf)

-- | Compute harvest mortality contribution to CWD column pool at level j.
harvestMortalityContrib
  :: Double    -- iso hrv_livestemc_to_litter
  -> Double    -- iso hrv_livecrootc_to_litter
  -> Double    -- iso hrv_deadcrootc_to_litter
  -> Double    -- wtcol
  -> Double    -- stem_prof
  -> Double    -- croot_prof
  -> Double
harvestMortalityContrib !liveStm !liveCrt !deadCrt !wtcol !stemProf !crootProf =
  (liveStm * wtcol * stemProf) +
  (liveCrt * wtcol * crootProf) +
  (deadCrt * wtcol * crootProf)

-- | Compute gross unrepresented landcover change contribution to CWD at level j.
grossUnrepContrib
  :: Double    -- iso gru_livecrootc_to_litter
  -> Double    -- iso gru_deadcrootc_to_litter
  -> Double    -- wtcol
  -> Double    -- croot_prof
  -> Double
grossUnrepContrib !liveCrt !deadCrt !wtcol !crootProf =
  (liveCrt * wtcol * crootProf) + (deadCrt * wtcol * crootProf)

-- ========================================================================
-- Photosynthetic discrimination
-- ========================================================================

-- | C13 photosynthetic discrimination factor.
-- del13C of new photosynthate relative to atmosphere.
-- For C3: disc = 4.4 + (28.2 - 4.4) * ci/ca  (Farquhar 1989)
-- For C4: disc ~ -5.5 permil (approximately)
photosyntheticDiscrimination :: Isotope
                             -> Bool    -- ^ c3flag
                             -> Double  -- ^ ci/ca ratio
                             -> Double  -- ^ discrimination factor (fractionation)
photosyntheticDiscrimination !iso !c3flag !ciOverCa =
  let !disc13_c3 = 4.4e-3 + (28.2e-3 - 4.4e-3) * ciOverCa
      !disc13_c4 = -5.5e-3
      !disc13 = if c3flag then disc13_c3 else disc13_c4
  in case iso of
       C13 -> 1.0 + disc13
       C14 -> (1.0 + disc13) ** 2  -- C14 discrimination is doubled

-- | C14 radioactive decay factor per timestep.
-- Half-life of C14 = 5730 years.
-- decay = exp(-lambda * dt) where lambda = ln(2) / half_life_seconds
c14DecayFactor :: Double  -- ^ dt (seconds)
               -> Double  -- ^ decay factor [0,1]
c14DecayFactor !dt =
  let !halfLife = 5730.0 * 365.25 * 86400.0  -- seconds
      !lambda = log 2.0 / halfLife
  in exp (negate lambda * dt)

-- ========================================================================
-- Phase 1: Phenology/allocation isotopic fluxes (CIsoFlux1)
-- ========================================================================

data CIsoFlux1Input = CIsoFlux1Input
  { cif1_isotope            :: !Isotope
  , cif1_dt                 :: !Double
  -- Photosynthesis isotope inputs
  , cif1_psnsun_to_cpool    :: !Double  -- ^ total C sunlit photosynthesis flux
  , cif1_psnsha_to_cpool    :: !Double  -- ^ total C shaded photosynthesis flux
  , cif1_ci_over_ca_sun     :: !Double  -- ^ ci/ca for sunlit leaves
  , cif1_ci_over_ca_sha     :: !Double  -- ^ ci/ca for shaded leaves
  , cif1_c3flag             :: !Bool
  , cif1_atm_iso_ratio      :: !Double  -- ^ atmospheric isotope ratio (C13/C12 or C14/C)
  -- C state for ratio computation
  , cif1_leafc              :: !Double
  , cif1_leafc_iso          :: !Double
  , cif1_frootc             :: !Double
  , cif1_frootc_iso         :: !Double
  , cif1_cpool              :: !Double
  , cif1_cpool_iso          :: !Double
  -- Allocation fluxes (total C)
  , cif1_cpool_to_leafc     :: !Double
  , cif1_cpool_to_frootc    :: !Double
  , cif1_leafc_to_litter    :: !Double
  , cif1_frootc_to_litter   :: !Double
  } deriving (Show)

data CIsoFlux1Output = CIsoFlux1Output
  { cif1o_psnsun_iso        :: !Double  -- ^ isotopic sunlit photosynthesis
  , cif1o_psnsha_iso        :: !Double  -- ^ isotopic shaded photosynthesis
  , cif1o_cpool_to_leafc_iso :: !Double
  , cif1o_cpool_to_frootc_iso :: !Double
  , cif1o_leafc_to_litter_iso :: !Double
  , cif1o_frootc_to_litter_iso :: !Double
  } deriving (Show)

-- | Compute isotopic fluxes for Phase 1 (phenology/allocation).
-- Photosynthesis uses discrimination; other fluxes use pool ratios.
cIsoFlux1 :: CIsoFlux1Input -> CIsoFlux1Output
cIsoFlux1 !inp =
  let !iso = cif1_isotope inp
      -- Photosynthetic discrimination
      !discSun = photosyntheticDiscrimination iso (cif1_c3flag inp) (cif1_ci_over_ca_sun inp)
      !discSha = photosyntheticDiscrimination iso (cif1_c3flag inp) (cif1_ci_over_ca_sha inp)
      !psnSunIso = cif1_psnsun_to_cpool inp * cif1_atm_iso_ratio inp * discSun
      !psnShaIso = cif1_psnsha_to_cpool inp * cif1_atm_iso_ratio inp * discSha

      -- Allocation from cpool: use cpool isotope ratio (frax=1 for non-photosynthesis)
      !allocLeafIso = isoFluxPair iso 1.0 (cif1_cpool_to_leafc inp)
                        (cif1_cpool_iso inp) (cif1_cpool inp)
      !allocFrootIso = isoFluxPair iso 1.0 (cif1_cpool_to_frootc inp)
                         (cif1_cpool_iso inp) (cif1_cpool inp)

      -- Litterfall: use leaf/froot isotope ratio (frax=1)
      !leafLitIso = isoFluxPair iso 1.0 (cif1_leafc_to_litter inp)
                      (cif1_leafc_iso inp) (cif1_leafc inp)
      !frootLitIso = isoFluxPair iso 1.0 (cif1_frootc_to_litter inp)
                       (cif1_frootc_iso inp) (cif1_frootc inp)

  in CIsoFlux1Output
     { cif1o_psnsun_iso = psnSunIso
     , cif1o_psnsha_iso = psnShaIso
     , cif1o_cpool_to_leafc_iso = allocLeafIso
     , cif1o_cpool_to_frootc_iso = allocFrootIso
     , cif1o_leafc_to_litter_iso = leafLitIso
     , cif1o_frootc_to_litter_iso = frootLitIso
     }

-- ========================================================================
-- Phase 2: Gap mortality isotopic fluxes (CIsoFlux2)
-- ========================================================================

data CIsoFlux2Input = CIsoFlux2Input
  { cif2_isotope                :: !Isotope
  -- Total C mortality fluxes
  , cif2_m_leafc_to_litter      :: !Double
  , cif2_m_livestemc_to_litter  :: !Double
  , cif2_m_deadstemc_to_litter  :: !Double
  , cif2_m_frootc_to_litter     :: !Double
  , cif2_m_livecrootc_to_litter :: !Double
  , cif2_m_deadcrootc_to_litter :: !Double
  -- Isotope pool states
  , cif2_leafc_iso              :: !Double
  , cif2_leafc                  :: !Double
  , cif2_livestemc_iso          :: !Double
  , cif2_livestemc              :: !Double
  , cif2_deadstemc_iso          :: !Double
  , cif2_deadstemc              :: !Double
  , cif2_frootc_iso             :: !Double
  , cif2_frootc                 :: !Double
  , cif2_livecrootc_iso         :: !Double
  , cif2_livecrootc             :: !Double
  , cif2_deadcrootc_iso         :: !Double
  , cif2_deadcrootc             :: !Double
  } deriving (Show)

data CIsoFlux2Output = CIsoFlux2Output
  { cif2o_m_leafc_to_litter_iso      :: !Double
  , cif2o_m_livestemc_to_litter_iso  :: !Double
  , cif2o_m_deadstemc_to_litter_iso  :: !Double
  , cif2o_m_frootc_to_litter_iso     :: !Double
  , cif2o_m_livecrootc_to_litter_iso :: !Double
  , cif2o_m_deadcrootc_to_litter_iso :: !Double
  } deriving (Show)

-- | Compute isotopic fluxes for Phase 2 (gap mortality).
-- All mortality fluxes use pool isotope ratios.
cIsoFlux2 :: CIsoFlux2Input -> CIsoFlux2Output
cIsoFlux2 !inp =
  let !iso = cif2_isotope inp
  in CIsoFlux2Output
  { cif2o_m_leafc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_leafc_to_litter inp) (cif2_leafc_iso inp) (cif2_leafc inp)
  , cif2o_m_livestemc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_livestemc_to_litter inp) (cif2_livestemc_iso inp) (cif2_livestemc inp)
  , cif2o_m_deadstemc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_deadstemc_to_litter inp) (cif2_deadstemc_iso inp) (cif2_deadstemc inp)
  , cif2o_m_frootc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_frootc_to_litter inp) (cif2_frootc_iso inp) (cif2_frootc inp)
  , cif2o_m_livecrootc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_livecrootc_to_litter inp) (cif2_livecrootc_iso inp) (cif2_livecrootc inp)
  , cif2o_m_deadcrootc_to_litter_iso =
      isoFluxPair iso 1.0 (cif2_m_deadcrootc_to_litter inp) (cif2_deadcrootc_iso inp) (cif2_deadcrootc inp)
  }

-- ========================================================================
-- Phase 3: Fire isotopic fluxes (CIsoFlux3)
-- ========================================================================

-- | Compute isotopic fluxes for Phase 3 (fire + C14 decay).
-- Fire fluxes use pool ratios; C14 decay applied to all pools.
cIsoFlux3 :: Isotope
           -> Double  -- ^ dt
           -> Double  -- ^ fire_flux_total (gC/m2/s)
           -> Double  -- ^ pool_iso (gC13 or gC14 /m2)
           -> Double  -- ^ pool_total (gC/m2)
           -> (Double, Double)  -- ^ (fire_flux_iso, decay_flux_iso)
cIsoFlux3 !iso !dt !fireFlux !poolIso !poolTotal =
  let !fireIso = isoFluxPair iso 1.0 fireFlux poolIso poolTotal
      !decayIso = case iso of
                    C14 -> poolIso * (1.0 - c14DecayFactor dt) / dt
                    C13 -> 0.0
  in (fireIso, decayIso)
