-- | Surface resistance calculations for soil evaporation.
-- Computes either a beta factor (Lee-Pielke 1992) or a soil resistance
-- (Swenson & Lawrence 2014) depending on the selected method.
-- Fortran: SurfaceResistanceMod.F90
-- Julia:   src/biogeophys/surface_resistance.jl
--
-- All functions are pure.
--
module CLM.BioGeoPhys.SurfaceResistance
  ( -- * Method selection
    SoilResisMethod(..)
    -- * Data types
  , SurfaceResistanceParams(..)
  , defaultSurfResParams
  , BetaInput(..)
  , BetaResult(..)
  , SL14Input(..)
  , SL14Result(..)
    -- * Functions
  , calcBetaLeePielke1992
  , calcSoilResistanceSL14
  ) where

import CLM.Constants.PhysicalConstants (tfrz, denh2o, denice, nlevgrnd, nlevsno)

-- ========================================================================
-- Constants
-- ========================================================================

-- | Landunit types
istsoil, istcrop, istwet, istice :: Int
istsoil = 1
istcrop = 2
istwet  = 5
istice  = 3

-- | Column types
icolRoadPerv, icolSunwall, icolShadewall, icolRoof, icolRoadImperv :: Int
icolRoadPerv    = 71
icolSunwall     = 72
icolShadewall   = 73
icolRoof        = 74
icolRoadImperv  = 75

-- ========================================================================
-- Data types
-- ========================================================================

-- | Method for computing soil evaporative resistance.
data SoilResisMethod
  = LeePielke1992  -- ^ Beta factor (Lee & Pielke 1992)
  | SwensonLawrence2014  -- ^ Dry surface layer resistance (Swenson & Lawrence 2014)
  deriving (Show, Eq)

-- | Parameters for the Swenson-Lawrence 2014 method.
data SurfaceResistanceParams = SurfaceResistanceParams
  { srp_d_max                   :: !Double  -- ^ Max dry surface layer thickness [mm]
  , srp_frac_sat_soil_dsl_init  :: !Double  -- ^ Fraction of saturation at DSL initiation
  } deriving (Show)

defaultSurfResParams :: SurfaceResistanceParams
defaultSurfResParams = SurfaceResistanceParams
  { srp_d_max = 15.0
  , srp_frac_sat_soil_dsl_init = 0.8
  }

-- | Input for Lee-Pielke 1992 beta calculation (single column).
data BetaInput = BetaInput
  { bi_lunType      :: !Int     -- ^ Landunit type
  , bi_colType      :: !Int     -- ^ Column type
  , bi_dz_top       :: !Double  -- ^ Thickness of top soil layer [m]
  , bi_h2osoi_liq_top :: !Double -- ^ Liquid water in top layer [kg/m2]
  , bi_h2osoi_ice_top :: !Double -- ^ Ice in top layer [kg/m2]
  , bi_watsat_top   :: !Double  -- ^ Porosity, top layer
  , bi_watfc_top    :: !Double  -- ^ Field capacity, top layer
  , bi_frac_sno     :: !Double  -- ^ Snow fraction
  , bi_frac_h2osfc  :: !Double  -- ^ Surface water fraction
  } deriving (Show)

-- | Result of Lee-Pielke 1992 beta factor.
data BetaResult = BetaResult
  { br_soilbeta :: !Double  -- ^ Soil evaporation beta factor [0-1]
  } deriving (Show)

-- | Input for Swenson-Lawrence 2014 soil resistance (single column).
data SL14Input = SL14Input
  { sl_lunType      :: !Int     -- ^ Landunit type
  , sl_colType      :: !Int     -- ^ Column type
  , sl_dz_top       :: !Double  -- ^ Thickness of top soil layer [m]
  , sl_h2osoi_liq_top :: !Double -- ^ Liquid water in top layer [kg/m2]
  , sl_h2osoi_ice_top :: !Double -- ^ Ice in top layer [kg/m2]
  , sl_watsat_top   :: !Double  -- ^ Porosity, top layer
  , sl_bsw_top      :: !Double  -- ^ Clapp-Hornberger b, top layer
  , sl_sucsat_top   :: !Double  -- ^ Saturated suction, top layer [mm]
  , sl_t_soisno_top :: !Double  -- ^ Temperature, top soil layer [K]
  } deriving (Show)

-- | Result of Swenson-Lawrence 2014 soil resistance.
data SL14Result = SL14Result
  { slr_dsl       :: !Double  -- ^ Dry surface layer thickness [mm]
  , slr_soilresis :: !Double  -- ^ Soil resistance [s/m]
  } deriving (Show)

-- ========================================================================
-- Functions
-- ========================================================================

-- | Compute the Lee-Pielke (1992) beta factor for soil evaporation.
--
-- Ported from @calc_beta_leepielke1992@ in @SurfaceResistanceMod.F90@.
calcBetaLeePielke1992 :: BetaInput -> BetaResult
calcBetaLeePielke1992 inp =
  let lt = bi_lunType inp
      ct = bi_colType inp
  in if lt /= istwet && lt /= istice
     then if lt == istsoil || lt == istcrop
          then let wx  = (bi_h2osoi_liq_top inp / denh2o
                         + bi_h2osoi_ice_top inp / denice)
                         / bi_dz_top inp
                   fac0 = min 1.0 (wx / bi_watsat_top inp)
                   _fac  = max 0.01 fac0
               in if wx < bi_watfc_top inp
                  then let facFc0 = min 1.0 (wx / bi_watfc_top inp)
                           facFc  = max 0.01 facFc0
                           beta   = (1.0 - bi_frac_sno inp - bi_frac_h2osfc inp)
                                  * 0.25 * (1.0 - cos (pi * facFc)) ** 2.0
                                  + bi_frac_sno inp + bi_frac_h2osfc inp
                       in BetaResult beta
                  else BetaResult 1.0
          else if ct == icolRoadPerv
               then BetaResult 0.0
               else if ct == icolSunwall || ct == icolShadewall
                    then BetaResult 0.0
                    else if ct == icolRoof || ct == icolRoadImperv
                         then BetaResult 0.0
                         else BetaResult 0.0
     else BetaResult 1.0

-- | Compute soil evaporative resistance using the Swenson & Lawrence (2014)
-- dry surface layer method.
--
-- Ported from @calc_soil_resistance_sl14@ in @SurfaceResistanceMod.F90@.
calcSoilResistanceSL14 :: SurfaceResistanceParams -> SL14Input -> SL14Result
calcSoilResistanceSL14 params inp =
  let lt = sl_lunType inp
      ct = sl_colType inp
  in if lt /= istwet && lt /= istice
     then if lt == istsoil || lt == istcrop
          then let vwcLiq = max (sl_h2osoi_liq_top inp) 1.0e-6
                          / (sl_dz_top inp * denh2o)
                   effPorTop = max 0.01
                             (sl_watsat_top inp
                             - min (sl_watsat_top inp)
                                   (sl_h2osoi_ice_top inp / (sl_dz_top inp * denice)))
                   -- Air-free pore space and diffusivity
                   aird = sl_watsat_top inp
                        * (sl_sucsat_top inp / 1.0e7) ** (1.0 / sl_bsw_top inp)
                   d0   = 2.12e-5 * (sl_t_soisno_top inp / 273.15) ** 1.75
                   eps  = sl_watsat_top inp - aird
                   dg   = eps * d0 * (eps / sl_watsat_top inp)
                        ** (3.0 / max 3.0 (sl_bsw_top inp))
                   -- Dry surface layer thickness
                   dMax = srp_d_max params
                   fInit = srp_frac_sat_soil_dsl_init params
                   dsl0 = dMax * max 0.001 (fInit * effPorTop - vwcLiq)
                        / max 0.001 (fInit * sl_watsat_top inp - aird)
                   dsl1 = max 0.0 dsl0
                   dsl  = min 200.0 dsl1
                   -- Soil resistance
                   sr0  = dsl / (dg * eps * 1.0e3) + 20.0
                   sr   = min 1.0e6 sr0
               in SL14Result dsl sr
          else if ct == icolRoadPerv
               then SL14Result 0.0 1.0e6
               else if ct == icolSunwall || ct == icolShadewall
                    then SL14Result 0.0 1.0e6
                    else if ct == icolRoof || ct == icolRoadImperv
                         then SL14Result 0.0 1.0e6
                         else SL14Result 0.0 1.0e6
     else SL14Result 0.0 0.0
