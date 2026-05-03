{-# LANGUAGE BangPatterns #-}
-- | Ozone damage to photosynthesis.
-- Fortran: OzoneMod.F90
-- Julia:   src/biogeophys/ozone.jl
--
-- Calculates ozone-induced stress on photosynthesis, stomatal conductance,
-- and Jmax. Ozone stress from timestep i is applied in timestep (i+1).
--
-- Two stress methods:
--   - Lombardozzi 2015: linear dose-response for photosynthesis and conductance
--   - Falk: linear dose-response for Jmax (requires LUNA)
--
-- Reference: Lombardozzi et al. (2015), J Climate, 28(1), 292-305.
--
-- All functions are pure.
module CLM.BioGeoPhys.Ozone
  ( -- * Stress methods
    OzoneStressMethod(..)
    -- * Ozone uptake
  , OzoneUptakeInput(..)
  , OzoneUptakeOutput(..)
  , calcOzoneUptakeOnePoint
    -- * Ozone stress (Lombardozzi 2015)
  , calcOzoneStressLombardozzi2015OnePoint
    -- * Ozone stress (Falk)
  , calcOzoneStressFalkOnePoint
    -- * Constants
  , o3_ko3
  , o3_lai_thresh
  , o3_flux_threshold
  , o3_needleleaf_photo_int, o3_needleleaf_photo_slope
  , o3_broadleaf_photo_int, o3_broadleaf_photo_slope
  , o3_nonwoody_photo_int, o3_nonwoody_photo_slope
  , o3_needleleaf_cond_int, o3_needleleaf_cond_slope
  , o3_broadleaf_cond_int, o3_broadleaf_cond_slope
  , o3_nonwoody_cond_int, o3_nonwoody_cond_slope
  , o3_needleleaf_jmax_int, o3_needleleaf_jmax_slope
  , o3_broadleaf_jmax_int, o3_broadleaf_jmax_slope
  , o3_nonwoody_jmax_int, o3_nonwoody_jmax_slope
  ) where

-- =========================================================================
-- Constants
-- =========================================================================

-- | Stress method selector
data OzoneStressMethod = StressLombardozzi2015 | StressFalk
  deriving (Show, Eq)

-- | O3:H2O resistance ratio (Sitch et al. 2007)
o3_ko3 :: Double
o3_ko3 = 1.67

-- | LAI threshold below which o3 damage is zero
o3_lai_thresh :: Double
o3_lai_thresh = 0.5

-- | Flux threshold below which o3flux is set to 0 [nmol/m2/s]
o3_flux_threshold :: Double
o3_flux_threshold = 0.8

-- Photosynthesis intercepts and slopes
o3_needleleaf_photo_int, o3_needleleaf_photo_slope :: Double
o3_needleleaf_photo_int   = 0.8390
o3_needleleaf_photo_slope = 0.0

o3_broadleaf_photo_int, o3_broadleaf_photo_slope :: Double
o3_broadleaf_photo_int    = 0.8752
o3_broadleaf_photo_slope  = 0.0

o3_nonwoody_photo_int, o3_nonwoody_photo_slope :: Double
o3_nonwoody_photo_int     = 0.8021
o3_nonwoody_photo_slope   = -0.0009

-- Conductance intercepts and slopes
o3_needleleaf_cond_int, o3_needleleaf_cond_slope :: Double
o3_needleleaf_cond_int    = 0.7823
o3_needleleaf_cond_slope  = 0.0048

o3_broadleaf_cond_int, o3_broadleaf_cond_slope :: Double
o3_broadleaf_cond_int     = 0.9125
o3_broadleaf_cond_slope   = 0.0

o3_nonwoody_cond_int, o3_nonwoody_cond_slope :: Double
o3_nonwoody_cond_int      = 0.7511
o3_nonwoody_cond_slope    = 0.0

-- Jmax intercepts and slopes
o3_needleleaf_jmax_int, o3_needleleaf_jmax_slope :: Double
o3_needleleaf_jmax_int    = 1.0
o3_needleleaf_jmax_slope  = 0.0

o3_broadleaf_jmax_int, o3_broadleaf_jmax_slope :: Double
o3_broadleaf_jmax_int     = 1.0
o3_broadleaf_jmax_slope   = -0.0037

o3_nonwoody_jmax_int, o3_nonwoody_jmax_slope :: Double
o3_nonwoody_jmax_int      = 1.0
o3_nonwoody_jmax_slope    = 0.0

-- | RGAS for unit conversion [J/K/kmol]
rgas_ozone :: Double
rgas_ozone = 8314.46

-- =========================================================================
-- Ozone uptake
-- =========================================================================

-- | Input for ozone uptake computation (single point, sun or shade).
data OzoneUptakeInput = OzoneUptakeInput
  { oui_forc_ozone :: !Double  -- ^ Ozone partial pressure [mol/mol]
  , oui_forc_pbot  :: !Double  -- ^ Surface pressure [Pa]
  , oui_forc_th    :: !Double  -- ^ Potential temperature [K]
  , oui_rs         :: !Double  -- ^ Stomatal resistance [s/m]
  , oui_rb         :: !Double  -- ^ Boundary layer resistance [s/m]
  , oui_ram        :: !Double  -- ^ Aerodynamic resistance [s/m]
  , oui_tlai       :: !Double  -- ^ One-sided LAI
  , oui_tlai_old   :: !Double  -- ^ LAI from previous timestep
  , oui_pft_type   :: !Int     -- ^ PFT type index (0-based Fortran)
  , oui_o3uptake   :: !Double  -- ^ Previous o3 uptake [mmol/m2]
  , oui_is_evergreen :: !Bool  -- ^ Is this PFT evergreen?
  , oui_leaf_long  :: !Double  -- ^ Leaf longevity [years]
  , oui_dtime      :: !Double  -- ^ Timestep [s]
  } deriving (Show)

-- | Output of ozone uptake computation.
data OzoneUptakeOutput = OzoneUptakeOutput
  { ouo_o3uptake :: !Double  -- ^ Updated o3 uptake [mmol/m2]
  } deriving (Show)

-- | Calculate ozone uptake for a single point.
-- Returns updated cumulative o3 uptake.
calcOzoneUptakeOnePoint :: OzoneUptakeInput -> Double
calcOzoneUptakeOnePoint inp =
  let -- Convert o3 from mol/mol to nmol/m3
      !o3conc = oui_forc_ozone inp * 1.0e9
              * (oui_forc_pbot inp / (oui_forc_th inp * rgas_ozone))

      -- Instantaneous flux [nmol/m2/s]
      !o3flux = o3conc / (o3_ko3 * oui_rs inp + oui_rb inp + oui_ram inp)

      -- Apply threshold
      !o3fluxcrit = if o3flux < o3_flux_threshold then 0.0
                    else o3flux - o3_flux_threshold

      !dtimeh = oui_dtime inp / 3600.0

      -- Flux per timestep [mmol/m2]
      !o3fluxperdt = o3fluxcrit * oui_dtime inp * 1.0e-6

      tlai_v    = oui_tlai inp
      tlai_old_v = oui_tlai_old inp
      o3uptake_prev = oui_o3uptake inp

  in if tlai_v > o3_lai_thresh
     then let -- Healing from new leaf area
              !heal = if tlai_v - tlai_old_v > 0.0
                      then max 0.0 (((tlai_v - tlai_old_v) / tlai_v) * o3fluxperdt)
                      else 0.0

              -- Evergreen leaf turnover decay
              !leafturn = if oui_is_evergreen inp
                          then 1.0 / (oui_leaf_long inp * 365.0 * 24.0)
                          else 0.0

              !decay = o3uptake_prev * leafturn * dtimeh

              -- Cumulative uptake
          in max 0.0 (o3uptake_prev + o3fluxperdt - decay - heal)
     else 0.0

-- =========================================================================
-- Lombardozzi 2015 stress
-- =========================================================================

-- | Calculate ozone stress coefficients using Lombardozzi 2015.
-- Returns (o3coefv, o3coefg) for photosynthesis and conductance.
calcOzoneStressLombardozzi2015OnePoint :: Int     -- ^ PFT type (0-based)
                                       -> Double  -- ^ o3uptake [mmol/m2]
                                       -> Bool    -- ^ Is woody PFT?
                                       -> (Double, Double)
calcOzoneStressLombardozzi2015OnePoint pft_type o3uptake is_woody
  | o3uptake == 0.0 = (1.0, 1.0)
  | otherwise =
      let (!photoInt, !photoSlope, !condInt, !condSlope)
            | pft_type > 3 =
                if not is_woody
                then (o3_nonwoody_photo_int, o3_nonwoody_photo_slope,
                      o3_nonwoody_cond_int, o3_nonwoody_cond_slope)
                else (o3_broadleaf_photo_int, o3_broadleaf_photo_slope,
                      o3_broadleaf_cond_int, o3_broadleaf_cond_slope)
            | otherwise =
                (o3_needleleaf_photo_int, o3_needleleaf_photo_slope,
                 o3_needleleaf_cond_int, o3_needleleaf_cond_slope)

          !o3coefv = max 0.0 (min 1.0 (photoInt + photoSlope * o3uptake))
          !o3coefg = max 0.0 (min 1.0 (condInt + condSlope * o3uptake))
      in (o3coefv, o3coefg)

-- =========================================================================
-- Falk stress
-- =========================================================================

-- | Calculate ozone stress coefficient for Jmax using Falk formulation.
-- Returns updated o3coefjmax.
calcOzoneStressFalkOnePoint :: Int     -- ^ PFT type (0-based)
                            -> Double  -- ^ o3uptake [mmol/m2]
                            -> Bool    -- ^ Is woody PFT?
                            -> Double
calcOzoneStressFalkOnePoint pft_type o3uptake is_woody
  | o3uptake == 0.0 = 1.0
  | otherwise =
      let (!jmaxInt, !jmaxSlope)
            | pft_type > 3 =
                if not is_woody
                then (o3_nonwoody_jmax_int, o3_nonwoody_jmax_slope)
                else (o3_broadleaf_jmax_int, o3_broadleaf_jmax_slope)
            | otherwise =
                (o3_needleleaf_jmax_int, o3_needleleaf_jmax_slope)
      in max 0.0 (min 1.0 (jmaxInt + jmaxSlope * o3uptake))
