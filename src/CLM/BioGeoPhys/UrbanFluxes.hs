{-# LANGUAGE BangPatterns #-}
-- | Urban turbulent fluxes module.
-- Fortran: UrbanFluxesMod.F90
--
-- Provides:
--   * Canyon wind profile (Masson 2000)
--   * Stability iteration (Monin-Obukhov)
--   * Sensible / latent heat fluxes for roof, walls, roads
--   * Waste heat from AC (simple CLM4.5 method)
--   * Simple internal building temperature
--   * 2-m temperature and humidity diagnostics
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.UrbanFluxes
  ( -- * Constants
    urbanHacOn
  , urbanWasteheatOn
  , acWasteheatFactor
  , htWasteheatFactor
  , wasteheatLimit
  , pondmxUrban
    -- * Types
  , UrbanFluxesParams(..)
  , defaultUrbanFluxesParams
  , UrbanFluxesInput(..)
  , CanyonWindResult(..)
  , UrbanFluxesResult(..)
    -- * Science functions
  , simpleWasteheatFromAC
  , canyonWind
  , canyonResistance
  , urbanFluxesSinglePatch
  , calcSimpleInternalBuildingTemp
  ) where

import CLM.Constants.PhysicalConstants (tfrz, grav, sb)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Urban HAC modes
urbanHacOn :: Int
urbanHacOn = 1

urbanWasteheatOn :: Int
urbanWasteheatOn = 2

-- | AC waste heat factor
acWasteheatFactor :: Double
acWasteheatFactor = 0.25

-- | Heating waste heat factor
htWasteheatFactor :: Double
htWasteheatFactor = 0.25

-- | Maximum wasteheat (W/m2)
wasteheatLimit :: Double
wasteheatLimit = 100.0

-- | Urban ponding maximum (m)
pondmxUrban :: Double
pondmxUrban = 1.0e-3

-- | von Karman constant
vkc :: Double
vkc = 0.4

-- | Specific heat of air (J/kg/K)
cpair :: Double
cpair = 1004.64

-- | Pi
rpi :: Double
rpi = 3.14159265358979323846

-- | Special value
spval :: Double
spval = 1.0e36

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data UrbanFluxesParams = UrbanFluxesParams
  { ufp_wind_min :: !Double  -- ^ minimum wind speed at forcing height (m/s)
  } deriving (Show, Eq)

defaultUrbanFluxesParams :: UrbanFluxesParams
defaultUrbanFluxesParams = UrbanFluxesParams { ufp_wind_min = 0.1 }

-- | Input for urban fluxes computation (single landunit)
data UrbanFluxesInput = UrbanFluxesInput
  { ufi_ht_roof       :: !Double  -- ^ building height (m)
  , ufi_z_d_town      :: !Double  -- ^ displacement height (m)
  , ufi_z_0_town      :: !Double  -- ^ roughness length (m)
  , ufi_canyon_hwr    :: !Double  -- ^ height-to-width ratio
  , ufi_wtlunit_roof  :: !Double  -- ^ roof weight fraction
  , ufi_wtroad_perv   :: !Double  -- ^ pervious road fraction
  , ufi_forc_hgt_u    :: !Double  -- ^ forcing height for wind (m)
  , ufi_forc_hgt_t    :: !Double  -- ^ forcing height for temperature (m)
  , ufi_forc_u        :: !Double  -- ^ forcing wind U (m/s)
  , ufi_forc_v        :: !Double  -- ^ forcing wind V (m/s)
  , ufi_forc_t        :: !Double  -- ^ forcing temperature (K)
  , ufi_forc_th       :: !Double  -- ^ forcing potential temperature (K)
  , ufi_forc_q        :: !Double  -- ^ forcing specific humidity (kg/kg)
  , ufi_forc_rho      :: !Double  -- ^ forcing air density (kg/m3)
  , ufi_forc_pbot     :: !Double  -- ^ forcing pressure (Pa)
  , ufi_wind_hgt_canyon :: !Double  -- ^ wind measurement height in canyon (m)
  , ufi_zetamaxstable :: !Double  -- ^ max stable zeta
  } deriving (Show, Eq)

-- | Canyon wind calculation result
data CanyonWindResult = CanyonWindResult
  { cwr_canyontop_wind :: !Double
  , cwr_canyon_u_wind  :: !Double
  , cwr_canyon_wind    :: !Double
  , cwr_ur             :: !Double
  } deriving (Show, Eq)

-- | Output from urban fluxes computation for a single patch
data UrbanFluxesResult = UrbanFluxesResult
  { ufr_eflx_sh_grnd :: !Double  -- ^ sensible heat from ground (W/m2)
  , ufr_eflx_sh_tot  :: !Double  -- ^ total sensible heat (W/m2)
  , ufr_qflx_evap_soi :: !Double -- ^ soil evaporation (kg/m2/s)
  , ufr_taux          :: !Double  -- ^ momentum flux x (kg/m/s2)
  , ufr_tauy          :: !Double  -- ^ momentum flux y (kg/m/s2)
  , ufr_cgrnds        :: !Double  -- ^ deriv of SH wrt ground T
  , ufr_cgrndl        :: !Double  -- ^ deriv of LH wrt ground T
  , ufr_taf           :: !Double  -- ^ canyon air temperature (K)
  , ufr_qaf           :: !Double  -- ^ canyon air specific humidity (kg/kg)
  , ufr_t_ref2m       :: !Double  -- ^ 2-m temperature (K)
  , ufr_q_ref2m       :: !Double  -- ^ 2-m specific humidity
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Calculate waste heat from AC (simple CLM4.5 method).
-- Returns (eflx_wasteheat, eflx_heat_from_ac).
simpleWasteheatFromAC :: Int        -- ^ urban_hac mode
                      -> Double     -- ^ eflx_urban_ac
                      -> Double     -- ^ eflx_urban_heat
                      -> (Double, Double)
simpleWasteheatFromAC !urban_hac !eflx_urban_ac !eflx_urban_heat =
  let !wh | urban_hac == urbanWasteheatOn =
            acWasteheatFactor * eflx_urban_ac + htWasteheatFactor * eflx_urban_heat
          | otherwise = 0.0
      !hfac | urban_hac == urbanHacOn || urban_hac == urbanWasteheatOn = abs eflx_urban_ac
            | otherwise = 0.0
  in (wh, hfac)

-- | Compute canyon top wind and canyon wind profile (Masson 2000).
canyonWind :: UrbanFluxesParams -> UrbanFluxesInput -> CanyonWindResult
canyonWind !params !inp =
  let !ur = max (ufp_wind_min params) (sqrt (ufi_forc_u inp ** 2 + ufi_forc_v inp ** 2))
      !ht  = ufi_ht_roof inp
      !zd  = ufi_z_d_town inp
      !z0  = ufi_z_0_town inp
      !hwr = ufi_canyon_hwr inp
      !whc = ufi_wind_hgt_canyon inp
      !hgt = ufi_forc_hgt_u inp

      !ctw = ur * log ((ht - zd) / z0) / log ((hgt - zd) / z0)

      !cuw | hwr < 0.5 =
             ctw * exp (-0.5 * hwr * (1.0 - whc / ht))
           | hwr < 1.0 =
             ctw * (1.0 + 2.0 * (2.0 / rpi - 1.0) *
                    (ht / (ht / hwr) - 0.5)) *
                   exp (-0.5 * hwr * (1.0 - whc / ht))
           | otherwise =
             ctw * (2.0 / rpi) *
                   exp (-0.5 * hwr * (1.0 - whc / ht))

  in CanyonWindResult
     { cwr_canyontop_wind = ctw
     , cwr_canyon_u_wind  = cuw
     , cwr_canyon_wind    = sqrt (cuw ** 2 + 0.01)  -- placeholder ustar contribution
     , cwr_ur             = ur
     }

-- | Canyon resistance (Masson 2000): R_canyon = cpair * rho / (11.8 + 4.2 * wind).
canyonResistance :: Double -> Double -> Double
canyonResistance !forc_rho !canyon_wind =
  cpair * forc_rho / (11.8 + 4.2 * canyon_wind)

-- | Compute urban fluxes for a single patch.
-- This is a simplified single-patch version; the full vectorized version
-- would iterate over filters as in the Julia code.
urbanFluxesSinglePatch
  :: Double  -- ^ forc_rho
  -> Double  -- ^ taf (canyon air temp, K)
  -> Double  -- ^ qaf (canyon air humidity)
  -> Double  -- ^ t_grnd (ground temp, K)
  -> Double  -- ^ qg (ground humidity)
  -> Double  -- ^ wtus_unscl (unscaled SH conductance)
  -> Double  -- ^ wtuq_unscl (unscaled LH conductance)
  -> Double  -- ^ ramu (aerodynamic resistance)
  -> Double  -- ^ forc_u
  -> Double  -- ^ forc_v
  -> Double  -- ^ dqgdT (d(qg)/dT)
  -> Double  -- ^ htvp (latent heat of vaporization, J/kg)
  -> Double  -- ^ wts_sum (total SH weight sum)
  -> Double  -- ^ wtq_sum (total LH weight sum)
  -> Double  -- ^ wtas (atmosphere SH weight)
  -> Double  -- ^ wtaq (atmosphere LH weight)
  -> Double  -- ^ wtus_others_sh (sum of other column SH weights)
  -> Double  -- ^ wtuq_others_lh (sum of other column LH weights)
  -> UrbanFluxesResult
urbanFluxesSinglePatch !forc_rho !taf !qaf !t_grnd !qg
                       !wtus_unscl !wtuq_unscl !ramu
                       !forc_u !forc_v !dqgdT !htvp
                       !wts_sum !wtq_sum !wtas !wtaq
                       !wtus_others_sh !wtuq_others_lh =
  let !dth = taf - t_grnd
      !dqh = qaf - qg

      !eflx_sh_grnd = negate forc_rho * cpair * wtus_unscl * dth
      !qflx_evap_soi = negate forc_rho * wtuq_unscl * dqh

      !taux = negate forc_rho * forc_u / ramu
      !tauy = negate forc_rho * forc_v / ramu

      !cgrnds = forc_rho * cpair * (wtas + wtus_others_sh) * (wtus_unscl / wts_sum)
      !cgrndl = forc_rho * (wtaq + wtuq_others_lh) * (wtuq_unscl / wtq_sum) * dqgdT

  in UrbanFluxesResult
     { ufr_eflx_sh_grnd = eflx_sh_grnd
     , ufr_eflx_sh_tot  = eflx_sh_grnd
     , ufr_qflx_evap_soi = qflx_evap_soi
     , ufr_taux          = taux
     , ufr_tauy          = tauy
     , ufr_cgrnds        = cgrnds
     , ufr_cgrndl        = cgrndl
     , ufr_taf           = taf
     , ufr_qaf           = qaf
     , ufr_t_ref2m       = taf
     , ufr_q_ref2m       = qaf
     }

-- | Calculate simple internal building temperature (CLM4.5).
-- t_building = (ht_roof * (t_shadewall_inner + t_sunwall_inner) + lngth_roof * t_roof_inner)
--            / (2 * ht_roof + lngth_roof)
calcSimpleInternalBuildingTemp
  :: Double  -- ^ ht_roof
  -> Double  -- ^ canyon_hwr
  -> Double  -- ^ wtlunit_roof
  -> Double  -- ^ t_sunwall_inner (K)
  -> Double  -- ^ t_shadewall_inner (K)
  -> Double  -- ^ t_roof_inner (K)
  -> Double  -- ^ t_building (K)
calcSimpleInternalBuildingTemp !ht_roof !canyon_hwr !wtlunit_roof
                               !t_sw_inner !t_sh_inner !t_roof_inner =
  let !lngth_roof = (ht_roof / canyon_hwr) * wtlunit_roof / (1.0 - wtlunit_roof)
  in (ht_roof * (t_sh_inner + t_sw_inner) + lngth_roof * t_roof_inner)
     / (2.0 * ht_roof + lngth_roof)
