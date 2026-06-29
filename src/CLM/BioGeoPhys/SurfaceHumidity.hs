-- | Surface humidity calculations.
-- Fortran: SurfaceHumidityMod.F90
-- Julia:   src/biogeophys/surface_humidity.jl
--
-- Computes surface specific humidities for vegetated/non-vegetated surfaces
-- including snow, soil, and standing water components.
--
-- All functions are pure.
--
module CLM.BioGeoPhys.SurfaceHumidity
  ( -- * Data types
    SurfaceHumidityInput(..)
  , SurfaceHumidityResult(..)
  , SoilAlphaResult(..)
    -- * Constants
  , roverg
  , spval
    -- * Functions
  , surfaceHumidity
  , soilAlpha
  ) where

import CLM.Constants.PhysicalConstants (tfrz, denh2o, denice, rgas, nlevgrnd, nlevsno)
import CLM.BioGeoPhys.QSat (qsat, QSatResult(..))
import qualified Data.Vector.Unboxed as VU

-- ========================================================================
-- Constants
-- ========================================================================

-- | Fortran ROVERG = R_wv/g*1000 = (Rgas/Mwv)/g*1000  [mm/K]
-- clm_varcon: roverg = SHR_CONST_RWV/SHR_CONST_G*1000, with the matric
-- potential @psit@ expressed in mm, so the 1000 factor (m->mm) is essential.
roverg :: Double
roverg = rgas / (9.80616 * 18.016) * 1000.0

-- | Special value for missing data
spval :: Double
spval = 1.0e36

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

-- | Input for a single-column surface humidity calculation (soil/crop landunit).
data SurfaceHumidityInput = SurfaceHumidityInput
  { shi_lunType      :: !Int      -- ^ Landunit type
  , shi_colType      :: !Int      -- ^ Column type
  , shi_snl          :: !Int      -- ^ Number of snow layers (negative or 0)
  , shi_dz_top       :: !Double   -- ^ Thickness of top soil layer [m]
  , shi_h2osoi_liq_top :: !Double -- ^ Liquid water in top soil layer [kg/m2]
  , shi_h2osoi_ice_top :: !Double -- ^ Ice in top soil layer [kg/m2]
  , shi_watsat_top   :: !Double   -- ^ Porosity, top soil layer
  , shi_smpmin       :: !Double   -- ^ Minimum soil matric potential [mm]
  , shi_sucsat_top   :: !Double   -- ^ Saturated suction, top layer [mm]
  , shi_bsw_top      :: !Double   -- ^ Clapp-Hornberger b, top layer
  , shi_frac_sno_eff :: !Double   -- ^ Effective snow fraction
  , shi_frac_h2osfc  :: !Double   -- ^ Fraction of surface water
  , shi_t_soisno_top :: !Double   -- ^ Temperature of top soil/snow layer [K]
  , shi_t_soisno_snow:: !Double   -- ^ Temperature of top snow layer [K] (only used when snl < 0)
  , shi_t_grnd       :: !Double   -- ^ Ground temperature [K]
  , shi_t_h2osfc     :: !Double   -- ^ Surface water temperature [K]
  , shi_forc_pbot    :: !Double   -- ^ Atmospheric pressure [Pa]
  , shi_forc_q       :: !Double   -- ^ Atmospheric specific humidity [kg/kg]
  } deriving (Show)

-- | Result of surface humidity for a single column.
data SurfaceHumidityResult = SurfaceHumidityResult
  { shr_qg       :: !Double  -- ^ Ground specific humidity [kg/kg]
  , shr_qg_snow  :: !Double  -- ^ Snow specific humidity [kg/kg]
  , shr_qg_soil  :: !Double  -- ^ Soil specific humidity [kg/kg]
  , shr_qg_h2osfc:: !Double  -- ^ Surface water specific humidity [kg/kg]
  , shr_dqgdT    :: !Double  -- ^ d(qg)/d(T) [kg/kg/K]
  } deriving (Show)

-- | Result of soil alpha (humidity reduction factor).
data SoilAlphaResult = SoilAlphaResult
  { sar_soilalpha   :: !Double  -- ^ Soil alpha for vegetated
  , sar_soilalpha_u :: !Double  -- ^ Soil alpha for urban
  } deriving (Show)

-- ========================================================================
-- Functions
-- ========================================================================

-- | Compute soil alpha (humidity reduction factor) for a single column.
soilAlpha :: SurfaceHumidityInput -> SoilAlphaResult
soilAlpha inp =
  let lt = shi_lunType inp
      ct = shi_colType inp
  in if lt /= istwet && lt /= istice
     then if lt == istsoil || lt == istcrop
          then let wx  = (shi_h2osoi_liq_top inp / denh2o
                         + shi_h2osoi_ice_top inp / denice)
                         / shi_dz_top inp
                   fac0 = min 1.0 (wx / shi_watsat_top inp)
                   fac  = max 0.01 fac0
                   psit0 = -(shi_sucsat_top inp) * fac ** (-(shi_bsw_top inp))
                   psit  = max (shi_smpmin inp) psit0
                   hr    = exp (psit / roverg / shi_t_soisno_top inp)
                   qred  = (1.0 - shi_frac_sno_eff inp - shi_frac_h2osfc inp) * hr
                         + shi_frac_sno_eff inp + shi_frac_h2osfc inp
               in SoilAlphaResult qred spval
          else if ct == icolRoadPerv
               -- Urban pervious road (icol_road_perv).  In CLM the faithful
               -- qred is a FULL-COLUMN integral (SurfaceHumidityMod.F90
               -- L143-166): for each of nlevgrnd layers it forms the effective
               -- porosity from watsat and ice, the liquid fraction, then scales
               -- by (vol_liq-watdry)/(watopt-watdry) and the pervious-road root
               -- fraction rootfr_road_perv, summing into hr_road_perv; qred =
               -- (1-frac_sno_eff)*hr_road_perv + frac_sno_eff, stored in
               -- soilalpha_u.  That integral needs per-column vectors
               -- (h2osoi_liq/ice, dz, watsat, watdry, watopt, rootfr_road_perv
               -- over all layers) which the single-column SurfaceHumidityInput
               -- record (top-layer scalars only) does not carry; porting it
               -- requires extending that type — an EXTERNAL change out of scope
               -- for this module.  This urban branch is never reached by the
               -- validated istsoil/istcrop soil column, so the soilalpha_u value
               -- returned here is inert for the exercised numerics.  FLAG: to
               -- support urban pervious roads, add the multi-layer fields and
               -- port the L143-166 loop here.
               then SoilAlphaResult spval 0.0
               else if ct == icolSunwall || ct == icolShadewall
                    then SoilAlphaResult spval spval
                    else if ct == icolRoof || ct == icolRoadImperv
                         then SoilAlphaResult spval spval
                         else SoilAlphaResult spval spval
     else SoilAlphaResult spval spval

-- | Compute surface humidities for a single column.
--
-- For soil/crop landunits, computes separate humidities for snow, soil,
-- and standing-water fractions.  For other landunits, uses ground
-- temperature with a humidity reduction factor.
--
-- Ported from @CalculateSurfaceHumidity@ in @SurfaceHumidityMod.F90@.
surfaceHumidity :: SurfaceHumidityInput -> SurfaceHumidityResult
surfaceHumidity inp =
  let lt  = shi_lunType inp
      snl = shi_snl inp
      frac_sno = shi_frac_sno_eff inp
      frac_h2osfc = shi_frac_h2osfc inp
      pbot = shi_forc_pbot inp
      forc_q_val = shi_forc_q inp
  in if lt == istsoil || lt == istcrop
     then -- Vegetated/crop: compute per-component humidities
       let -- Soil humidity
           wx  = (shi_h2osoi_liq_top inp / denh2o
                 + shi_h2osoi_ice_top inp / denice)
                 / shi_dz_top inp
           fac0 = min 1.0 (wx / shi_watsat_top inp)
           fac  = max 0.01 fac0
           psit0 = -(shi_sucsat_top inp) * fac ** (-(shi_bsw_top inp))
           psit  = max (shi_smpmin inp) psit0
           hr    = exp (psit / roverg / shi_t_soisno_top inp)

           -- Soil qsat
           qr_soil = qsat (shi_t_soisno_top inp) pbot
           qsatg_soil0 = qsr_qs qr_soil
           qsatgdT_soil0 = qsr_dqsdT qr_soil
           (qsatg_soil, qsatgdT_soil)
             | qsatg_soil0 > forc_q_val && forc_q_val > hr * qsatg_soil0
                         = (forc_q_val, 0.0)
             | otherwise = (qsatg_soil0, qsatgdT_soil0)
           qg_soil_val = hr * qsatg_soil

           -- Snow qsat
           (qg_snow_val, dqgdT_base)
             | snl < 0 =
                 let qr_snow = qsat (shi_t_soisno_snow inp) pbot
                     qsatg_snow = qsr_qs qr_snow
                     qsatgdT_snow = qsr_dqsdT qr_snow
                     dq = frac_sno * qsatgdT_snow
                        + (1.0 - frac_sno - frac_h2osfc) * hr * qsatgdT_soil
                 in (qsatg_snow, dq)
             | otherwise =
                 let dq = (1.0 - frac_h2osfc) * hr * qsatgdT_soil
                 in (qg_soil_val, dq)

           -- Surface water qsat
           (qg_h2osfc_val, dqgdT_val)
             | frac_h2osfc > 0.0 =
                 let qr_h2osfc = qsat (shi_t_h2osfc inp) pbot
                     qsatg_h2o = qsr_qs qr_h2osfc
                     qsatgdT_h2o = qsr_dqsdT qr_h2osfc
                 in (qsatg_h2o, dqgdT_base + frac_h2osfc * qsatgdT_h2o)
             | otherwise = (qg_soil_val, dqgdT_base)

           -- Weighted ground humidity
           qg_val = frac_sno * qg_snow_val
                  + (1.0 - frac_sno - frac_h2osfc) * qg_soil_val
                  + frac_h2osfc * qg_h2osfc_val
       in SurfaceHumidityResult
            { shr_qg        = qg_val
            , shr_qg_snow   = qg_snow_val
            , shr_qg_soil   = qg_soil_val
            , shr_qg_h2osfc = qg_h2osfc_val
            , shr_dqgdT     = dqgdT_val
            }
     else -- Non-vegetated: use ground temperature with qred
       let saRes = soilAlpha inp
           qred  = sar_soilalpha saRes
           qr    = qsat (shi_t_grnd inp) pbot
           qsatg_val = qsr_qs qr
           qsatgdT_val = qsr_dqsdT qr
           (qg_val, dqgdT_val)
             | qsatg_val > forc_q_val && forc_q_val > qred * qsatg_val
                         = (forc_q_val, 0.0)
             | otherwise = (qred * qsatg_val, qred * qsatgdT_val)
       in SurfaceHumidityResult
            { shr_qg        = qg_val
            , shr_qg_snow   = qg_val
            , shr_qg_soil   = qg_val
            , shr_qg_h2osfc = qg_val
            , shr_dqgdT     = dqgdT_val
            }
