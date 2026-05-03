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
