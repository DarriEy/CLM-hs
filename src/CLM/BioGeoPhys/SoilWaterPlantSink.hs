{-# LANGUAGE BangPatterns #-}
-- | Soil water plant sink: root water uptake.
-- Fortran: SoilWaterPlantSinkMod.F90
-- Julia:   src/biogeophys/soil_water_plant_sink.jl
--
-- Computes the effective root fraction at column level and the vertical
-- transpiration sink. Two pathways:
--   1) Default: transpiration-weighted column root fractions from patches
--   2) Hydraulic stress: flux from soil-root conductance and water
--      potential gradients
--
-- All functions are pure.
module CLM.BioGeoPhys.SoilWaterPlantSink
  ( -- * Default method
    DefaultSinkInput(..)
  , DefaultSinkPatch(..)
  , DefaultSinkOutput(..)
  , computeDefaultSink
    -- * Hydraulic stress method
  , HydStressSinkInput(..)
  , HydStressSinkPatch(..)
  , HydStressSinkOutput(..)
  , computeHydStressSink
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (nlevsno)

-- =========================================================================
-- Default method
-- =========================================================================

-- | Per-patch data for the default sink computation.
data DefaultSinkPatch = DefaultSinkPatch
  { dsp_rootr_patch      :: !(VU.Vector Double) -- ^ Patch root fraction (nlevsoi)
  , dsp_qflx_tran_veg    :: !Double             -- ^ Patch transpiration [kg/m2/s]
  , dsp_wtcol            :: !Double             -- ^ Patch weight on column
  , dsp_active           :: !Bool               -- ^ Is patch active?
  } deriving (Show)

-- | Column-level input for default transpiration sink.
data DefaultSinkInput = DefaultSinkInput
  { dsi_nlevsoi          :: !Int                 -- ^ Number of soil layers
  , dsi_qflx_tran_veg_col :: !Double             -- ^ Column-level transpiration [kg/m2/s]
  , dsi_patches          :: ![DefaultSinkPatch]  -- ^ Patches on this column
  } deriving (Show)

-- | Output of default transpiration sink computation.
data DefaultSinkOutput = DefaultSinkOutput
  { dso_rootr_col    :: !(VU.Vector Double) -- ^ Column effective root fraction (nlevsoi)
  , dso_qflx_rootsoi :: !(VU.Vector Double) -- ^ Root-soil water exchange (nlevsoi)
  } deriving (Show)

-- | Compute column-level effective root fraction and transpiration sink
-- using the default (transpiration-weighted) method.
computeDefaultSink :: DefaultSinkInput -> DefaultSinkOutput
computeDefaultSink inp =
  let nlevsoi_v = dsi_nlevsoi inp
      patches   = dsi_patches inp

      -- Accumulate transpiration-weighted root fraction
      accum (!rootr_acc, !denom_acc) patch
        | not (dsp_active patch) = (rootr_acc, denom_acc)
        | otherwise =
            let w = dsp_qflx_tran_veg patch * dsp_wtcol patch
                !rootr_acc' = VU.zipWith (+) rootr_acc
                    (VU.map (* w) (dsp_rootr_patch patch))
                !denom_acc' = denom_acc + w
            in (rootr_acc', denom_acc')

      (!rootr_sum, !denom) = foldl accum (VU.replicate nlevsoi_v 0.0, 0.0) patches

      -- Normalize
      !rootr_col = if denom /= 0.0
                   then VU.map (/ denom) rootr_sum
                   else rootr_sum

      -- Compute qflx_rootsoi
      !qflx_tv_col = dsi_qflx_tran_veg_col inp
      !qflx_rootsoi = VU.map (* qflx_tv_col) rootr_col

  in DefaultSinkOutput
       { dso_rootr_col    = rootr_col
       , dso_qflx_rootsoi = qflx_rootsoi
       }

-- =========================================================================
-- Hydraulic stress method
-- =========================================================================

-- | Per-patch data for the hydraulic stress sink computation.
data HydStressSinkPatch = HydStressSinkPatch
  { hsp_k_soil_root   :: !(VU.Vector Double) -- ^ Soil-root conductance (nlevsoi)
  , hsp_vegwp_root     :: !Double             -- ^ Root water potential [mm]
  , hsp_wtcol          :: !Double             -- ^ Patch weight on column
  , hsp_frac_veg_nosno :: !Int                -- ^ Vegetation fraction flag
  , hsp_active         :: !Bool               -- ^ Is patch active?
  } deriving (Show)

-- | Column-level input for hydraulic stress sink.
data HydStressSinkInput = HydStressSinkInput
  { hsi_nlevsoi          :: !Int
  , hsi_smp_l            :: !(VU.Vector Double) -- ^ Soil matric potential [mm] (nlevsoi)
  , hsi_z                :: !(VU.Vector Double) -- ^ Layer depths [m] (nlevsno+nlevgrnd)
  , hsi_patches          :: ![HydStressSinkPatch]
  } deriving (Show)

-- | Output of hydraulic stress transpiration sink computation.
data HydStressSinkOutput = HydStressSinkOutput
  { hso_qflx_rootsoi    :: !(VU.Vector Double) -- ^ Root-soil water exchange (nlevsoi)
  , hso_qflx_phs_neg    :: !Double             -- ^ Total negative flux [kg/m2/s]
  , hso_qflx_hydr_redist :: ![Double]          -- ^ Per-patch redistribution flux
  } deriving (Show)

-- | Compute column-level transpiration sink using the hydraulic
-- stress method. Flux is driven by soil-root conductance and water
-- potential gradient.
computeHydStressSink :: HydStressSinkInput -> HydStressSinkOutput
computeHydStressSink inp =
  let nlevsoi_v = hsi_nlevsoi inp
      smp       = hsi_smp_l inp
      z_vec     = hsi_z inp
      patches   = hsi_patches inp

      -- Process all layers
      processLayer j =
        let grav2 = z_vec VU.! (j + nlevsno) * 1000.0
            smp_j = smp VU.! j

            patchFlux patch
              | not (hsp_active patch) = 0.0
              | hsp_frac_veg_nosno patch <= 0 = 0.0
              | hsp_wtcol patch <= 0.0 = 0.0
              | otherwise =
                  let k_j = hsp_k_soil_root patch VU.! j
                      pf  = k_j * (smp_j - hsp_vegwp_root patch - grav2)
                  in pf * hsp_wtcol patch

        in sum (map patchFlux patches)

      !qflx_rootsoi = VU.generate nlevsoi_v processLayer

      -- Negative flux sum
      !qflx_phs_neg = VU.foldl' (\acc v -> if v < 0 then acc + v else acc) 0.0 qflx_rootsoi

      -- Per-patch hydraulic redistribution flux (qflx_hydr_redist_patch).
      -- Matches Fortran Compute_EffecRootFrac_And_VertTranSink_HydStress:
      -- for each active vegetated patch, accumulate the per-layer root-soil
      -- flux k_soil_root*(smp - vegwp_root - grav) over all soil layers,
      -- summing only the negative (soil-uptake-reversed, i.e. redistribution)
      -- contributions. Unlike the column sink this term is NOT wtcol-weighted.
      redistPatch patch =
        if not (hsp_active patch) || hsp_frac_veg_nosno patch <= 0 || hsp_wtcol patch <= 0
        then 0.0
        else let redistLayer j =
                   let grav2 = z_vec VU.! (j + nlevsno) * 1000.0
                       k_j = hsp_k_soil_root patch VU.! j
                       pf  = k_j * (smp VU.! j - hsp_vegwp_root patch - grav2)
                   in if pf < 0 then pf else 0.0
             in sum [redistLayer j | j <- [0 .. nlevsoi_v - 1]]

  in HydStressSinkOutput
       { hso_qflx_rootsoi    = qflx_rootsoi
       , hso_qflx_phs_neg    = qflx_phs_neg
       , hso_qflx_hydr_redist = map redistPatch patches
       }
