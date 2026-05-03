{-# LANGUAGE BangPatterns #-}
-- | Bare soil sensible and latent heat fluxes.
-- Fortran: BareGroundFluxesMod.F90
-- Julia:   src/biogeophys/bareground_fluxes.jl
--
-- Computes sensible and latent fluxes and their derivatives with respect
-- to ground temperature for bare-ground (no exposed vegetation) patches.
-- Includes Monin-Obukhov stability iteration and z0 parameterization.
--
-- All functions are pure.
module CLM.BioGeoPhys.BaregroundFluxes
  ( -- * Parameters
    BareGroundFluxesParams(..)
  , defaultBareGroundFluxesParams
    -- * Input / Output records
  , BareGroundFluxesInput(..)
  , BareGroundFluxesOutput(..)
  , Z0ParamMethod(..)
    -- * Main driver
  , baregroundFluxes
    -- * Stability helpers
  , moninObukIni
  , ZetaStabilityResult(..)
  , computeZetaStability
    -- * Constants
  , niters
  , vkc
  , cpair
  , nuParam
  , meierParam3
  , betaParam
  ) where

import CLM.Constants.PhysicalConstants (tfrz, grav, denh2o, denice)

-- =========================================================================
-- Constants
-- =========================================================================

-- | Maximum number of stability iterations
niters :: Int
niters = 3

-- | von Karman constant
vkc :: Double
vkc = 0.4

-- | Specific heat of dry air [J/kg/K]
cpair :: Double
cpair = 1004.64

-- | Kinematic viscosity of air [m^2/s]
nuParam :: Double
nuParam = 1.5e-5

-- | Meier 2022 parameter 3
meierParam3 :: Double
meierParam3 = 7.4

-- | Beta parameter for Meier 2022
betaParam :: Double
betaParam = 1.5

-- | RGAS for bareground (J/K/kmol -> need mol level for cf conversion)
rgasBG :: Double
rgasBG = 8314.46

-- =========================================================================
-- Parameters (read from parameter file in Fortran)
-- =========================================================================

-- | Module-level parameters for bare ground fluxes
data BareGroundFluxesParams = BareGroundFluxesParams
  { bgf_a_coef   :: !Double  -- ^ Drag coefficient under less dense canopy
  , bgf_a_exp    :: !Double  -- ^ Drag exponent under less dense canopy
  , bgf_wind_min :: !Double  -- ^ Minimum wind speed at forcing height [m/s]
  } deriving (Show)

defaultBareGroundFluxesParams :: BareGroundFluxesParams
defaultBareGroundFluxesParams = BareGroundFluxesParams
  { bgf_a_coef   = 0.13
  , bgf_a_exp    = 0.45
  , bgf_wind_min = 1.0
  }

-- =========================================================================
-- Input / Output records
-- =========================================================================

-- | Input record for a single bare-ground patch flux computation.
data BareGroundFluxesInput = BareGroundFluxesInput
  { bgi_params         :: !BareGroundFluxesParams
  , bgi_z0param_method :: !Z0ParamMethod
    -- Atmospheric forcing
  , bgi_forc_q          :: !Double  -- ^ Specific humidity [kg/kg]
  , bgi_forc_pbot       :: !Double  -- ^ Surface pressure [Pa]
  , bgi_forc_th         :: !Double  -- ^ Potential temperature [K]
  , bgi_forc_rho        :: !Double  -- ^ Air density [kg/m3]
  , bgi_forc_t          :: !Double  -- ^ Air temperature [K]
  , bgi_forc_u          :: !Double  -- ^ Zonal wind [m/s]
  , bgi_forc_v          :: !Double  -- ^ Meridional wind [m/s]
  , bgi_forc_hgt_t      :: !Double  -- ^ Reference height for T [m]
  , bgi_forc_hgt_u      :: !Double  -- ^ Reference height for wind [m]
  , bgi_forc_hgt_q      :: !Double  -- ^ Reference height for humidity [m]
    -- Ground state
  , bgi_t_grnd          :: !Double  -- ^ Ground temperature [K]
  , bgi_thm             :: !Double  -- ^ Intermediate temp variable [K]
  , bgi_qg              :: !Double  -- ^ Ground specific humidity [kg/kg]
  , bgi_qg_snow         :: !Double  -- ^ Ground q over snow [kg/kg]
  , bgi_qg_soil         :: !Double  -- ^ Ground q over soil [kg/kg]
  , bgi_qg_h2osfc       :: !Double  -- ^ Ground q over surface water [kg/kg]
  , bgi_dqgdT           :: !Double  -- ^ d(qg)/d(T_grnd)
  , bgi_thv             :: !Double  -- ^ Virtual potential temperature [K]
  , bgi_beta            :: !Double  -- ^ Convective velocity parameter
  , bgi_zii             :: !Double  -- ^ Convective BL height [m]
  , bgi_t_h2osfc        :: !Double  -- ^ Surface water temperature [K]
  , bgi_t_soisno_top    :: !Double  -- ^ Top snow/soil layer temperature [K]
  , bgi_t_soil1         :: !Double  -- ^ First soil layer temperature [K]
    -- Roughness
  , bgi_z0mg            :: !Double  -- ^ Momentum roughness length [m]
  , bgi_z0hg            :: !Double  -- ^ Heat roughness length [m]
  , bgi_z0qg            :: !Double  -- ^ Moisture roughness length [m]
  , bgi_zetamaxstable   :: !Double  -- ^ Maximum stable zeta
    -- Soil evaporation
  , bgi_soilbeta        :: !Double  -- ^ Soil evaporation beta factor
  , bgi_soilresis       :: !Double  -- ^ Soil evaporation resistance [s/m]
  , bgi_do_soilevap_beta :: !Bool   -- ^ Use beta method
  , bgi_do_soil_resistance :: !Bool -- ^ Use resistance method
    -- Energy flux constants
  , bgi_htvp            :: !Double  -- ^ Latent heat (HVAP or HSUB) [J/kg]
    -- Top layer water for www
  , bgi_h2osoi_liq_top  :: !Double  -- ^ Liquid water in top soil layer [kg/m2]
  , bgi_h2osoi_ice_top  :: !Double  -- ^ Ice in top soil layer [kg/m2]
  , bgi_dz_top          :: !Double  -- ^ Top soil layer thickness [m]
  , bgi_watsat_top      :: !Double  -- ^ Top soil layer porosity
  } deriving (Show)

-- | Z0 parameterization method
data Z0ParamMethod = ZengWang2007 | Meier2022
  deriving (Show, Eq)

-- | Output record for a single bare-ground patch.
data BareGroundFluxesOutput = BareGroundFluxesOutput
  { bgo_taux            :: !Double  -- ^ Zonal momentum flux [kg/m/s2]
  , bgo_tauy            :: !Double  -- ^ Meridional momentum flux [kg/m/s2]
  , bgo_eflx_sh_grnd    :: !Double  -- ^ Sensible heat flux [W/m2]
  , bgo_eflx_sh_tot     :: !Double  -- ^ Total sensible heat flux [W/m2]
  , bgo_eflx_sh_snow    :: !Double  -- ^ Sensible heat over snow [W/m2]
  , bgo_eflx_sh_soil    :: !Double  -- ^ Sensible heat over soil [W/m2]
  , bgo_eflx_sh_h2osfc  :: !Double  -- ^ Sensible heat over surface water [W/m2]
  , bgo_qflx_evap_soi   :: !Double  -- ^ Soil evaporation [kg/m2/s]
  , bgo_qflx_evap_tot   :: !Double  -- ^ Total evaporation [kg/m2/s]
  , bgo_qflx_ev_snow    :: !Double  -- ^ Evaporation from snow [kg/m2/s]
  , bgo_qflx_ev_soil    :: !Double  -- ^ Evaporation from soil [kg/m2/s]
  , bgo_qflx_ev_h2osfc  :: !Double  -- ^ Evaporation from surface water [kg/m2/s]
  , bgo_cgrnds          :: !Double  -- ^ d(SH)/d(T_grnd) [W/m2/K]
  , bgo_cgrndl          :: !Double  -- ^ d(LH)/d(T_grnd) [W/m2/K]
  , bgo_cgrnd           :: !Double  -- ^ Total d(flux)/d(T_grnd)
  , bgo_t_ref2m         :: !Double  -- ^ 2m air temperature [K]
  , bgo_q_ref2m         :: !Double  -- ^ 2m specific humidity [kg/kg]
  , bgo_rh_ref2m        :: !Double  -- ^ 2m relative humidity [%]
  , bgo_ram1            :: !Double  -- ^ Aerodynamic resistance [s/m]
  , bgo_z0hg_out        :: !Double  -- ^ Updated heat roughness [m]
  , bgo_z0qg_out        :: !Double  -- ^ Updated moisture roughness [m]
  , bgo_ustar           :: !Double  -- ^ Friction velocity [m/s]
  , bgo_kbm1            :: !Double  -- ^ ln(z0m/z0h)
  } deriving (Show)

-- =========================================================================
-- Stability helpers
-- =========================================================================

-- | Initialize Monin-Obukhov length and wind speed.
-- Ported from monin_obuk_ini in FrictionVelocityMod.F90 / friction_velocity.jl.
-- Uses bulk Richardson number approach (Zeng et al. 1998).
moninObukIni :: Double -> Double -> Double -> Double -> Double -> Double
             -> (Double, Double)
moninObukIni zetamaxstable ur thv dthv zldis z0m =
  let !wc = 0.5
      !um = if dthv >= 0.0
            then max ur 0.1
            else sqrt (ur * ur + wc * wc)

      !rib = grav * zldis * dthv / (thv * um * um)

      !zeta = if rib >= 0.0
              then -- neutral or stable
                   let z0 = rib * log (zldis / z0m) / (1.0 - 5.0 * min rib 0.19)
                   in min zetamaxstable (max z0 0.01)
              else -- unstable
                   let z0 = rib * log (zldis / z0m)
                   in max (-100.0) (min z0 (-0.01))

      !obu = zldis / zeta
  in (um, obu)

-- | Result of zeta/stability computation
data ZetaStabilityResult = ZetaStabilityResult
  { zsr_zeta  :: !Double
  , zsr_um    :: !Double
  , zsr_obu   :: !Double
  , zsr_z0hg  :: !Double
  , zsr_z0qg  :: !Double
  , zsr_ustar :: !Double
  , zsr_temp1 :: !Double  -- ^ Heat transfer coefficient from friction_velocity
  , zsr_temp2 :: !Double  -- ^ Moisture transfer coefficient from friction_velocity
  , zsr_temp12m :: !Double -- ^ 2m heat transfer coefficient
  , zsr_temp22m :: !Double -- ^ 2m moisture transfer coefficient
  } deriving (Show)

-- =========================================================================
-- Stability functions (Paulson 1970 / Webb 1970)
-- Ported from StabilityFunc1/2 in FrictionVelocityMod.F90
-- =========================================================================

-- | Stability function for momentum (psi_m). Valid for zeta <= 0 (unstable).
stabilityFunc1 :: Double -> Double
stabilityFunc1 zeta =
  let !z = min zeta 0.0
      !chik2 = sqrt (1.0 - 16.0 * z)
      !chik  = sqrt chik2
  in 2.0 * log ((1.0 + chik) * 0.5)
     + log ((1.0 + chik2) * 0.5)
     - 2.0 * atan chik + pi * 0.5

-- | Stability function for heat/moisture (psi_h). Valid for zeta <= 0 (unstable).
stabilityFunc2 :: Double -> Double
stabilityFunc2 zeta =
  let !z = min zeta 0.0
      !chik2 = sqrt (1.0 - 16.0 * z)
  in 2.0 * log ((1.0 + chik2) * 0.5)

-- | Compute friction velocity and transfer coefficients with full
-- Businger-Dyer stability corrections.
-- Ported from friction_velocity! in FrictionVelocityMod.F90.
-- Returns (ustar, temp1, temp2, temp12m, temp22m).
frictionVelBG :: Double  -- ^ z0m
              -> Double  -- ^ z0h
              -> Double  -- ^ z0q
              -> Double  -- ^ obu (Obukhov length)
              -> Double  -- ^ um (effective wind speed)
              -> Double  -- ^ zldis_u (height for wind)
              -> Double  -- ^ zldis_t (height for temperature)
              -> Double  -- ^ zldis_q (height for humidity)
              -> (Double, Double, Double, Double, Double)
frictionVelBG z0m z0h z0q obu um zldis_u zldis_t zldis_q =
  let zetam = 1.574  -- transition point (wind profile)
      zetat = 0.465  -- transition point (temp profile)

      -- === Wind profile → ustar ===
      !zeta_u = zldis_u / obu
      !ustar_v
        | zeta_u < (-zetam) =
            vkc * um / (log ((-zetam) * obu / z0m)
              - stabilityFunc1 (-zetam)
              + stabilityFunc1 (z0m / obu)
              + 1.14 * ((-zeta_u) ** (1.0/3.0) - zetam ** (1.0/3.0)))
        | zeta_u < 0.0 =
            vkc * um / (log (zldis_u / z0m)
              - stabilityFunc1 zeta_u
              + stabilityFunc1 (z0m / obu))
        | zeta_u <= 1.0 =
            vkc * um / (log (zldis_u / z0m)
              + 5.0 * zeta_u - 5.0 * z0m / obu)
        | otherwise =
            vkc * um / (log (obu / z0m)
              + 5.0 - 5.0 * z0m / obu
              + (5.0 * log zeta_u + zeta_u - 1.0))

      -- === Temperature profile → temp1 ===
      !zeta_t = zldis_t / obu
      !temp1_v
        | zeta_t < (-zetat) =
            vkc / (log ((-zetat) * obu / z0h)
              - stabilityFunc2 (-zetat)
              + stabilityFunc2 (z0h / obu)
              + 0.8 * (zetat ** (-(1.0/3.0)) - (-zeta_t) ** (-(1.0/3.0))))
        | zeta_t < 0.0 =
            vkc / (log (zldis_t / z0h)
              - stabilityFunc2 zeta_t
              + stabilityFunc2 (z0h / obu))
        | zeta_t <= 1.0 =
            vkc / (log (zldis_t / z0h)
              + 5.0 * zeta_t - 5.0 * z0h / obu)
        | otherwise =
            vkc / (log (obu / z0h)
              + 5.0 - 5.0 * z0h / obu
              + (5.0 * log zeta_t + zeta_t - 1.0))

      -- === Humidity profile → temp2 ===
      !zeta_q = zldis_q / obu
      !temp2_v
        | zeta_q < (-zetat) =
            vkc / (log ((-zetat) * obu / z0q)
              - stabilityFunc2 (-zetat)
              + stabilityFunc2 (z0q / obu)
              + 0.8 * (zetat ** (-(1.0/3.0)) - (-zeta_q) ** (-(1.0/3.0))))
        | zeta_q < 0.0 =
            vkc / (log (zldis_q / z0q)
              - stabilityFunc2 zeta_q
              + stabilityFunc2 (z0q / obu))
        | zeta_q <= 1.0 =
            vkc / (log (zldis_q / z0q)
              + 5.0 * zeta_q - 5.0 * z0q / obu)
        | otherwise =
            vkc / (log (obu / z0q)
              + 5.0 - 5.0 * z0q / obu
              + (5.0 * log zeta_q + zeta_q - 1.0))

      -- === 2m reference height ===
      !zeta_2m = 2.0 / obu
      !temp12m_v
        | zeta_2m < (-zetat) =
            vkc / (log ((-zetat) * obu / z0h)
              - stabilityFunc2 (-zetat)
              + stabilityFunc2 (z0h / obu)
              + 0.8 * (zetat ** (-(1.0/3.0)) - (-zeta_2m) ** (-(1.0/3.0))))
        | zeta_2m < 0.0 =
            vkc / (log (2.0 / z0h)
              - stabilityFunc2 zeta_2m
              + stabilityFunc2 (z0h / obu))
        | zeta_2m <= 1.0 =
            vkc / (log (2.0 / z0h)
              + 5.0 * zeta_2m - 5.0 * z0h / obu)
        | otherwise =
            vkc / (log (obu / z0h)
              + 5.0 - 5.0 * z0h / obu
              + (5.0 * log zeta_2m + zeta_2m - 1.0))
      !temp22m_v
        | zeta_2m < (-zetat) =
            vkc / (log ((-zetat) * obu / z0q)
              - stabilityFunc2 (-zetat)
              + stabilityFunc2 (z0q / obu)
              + 0.8 * (zetat ** (-(1.0/3.0)) - (-zeta_2m) ** (-(1.0/3.0))))
        | zeta_2m < 0.0 =
            vkc / (log (2.0 / z0q)
              - stabilityFunc2 zeta_2m
              + stabilityFunc2 (z0q / obu))
        | zeta_2m <= 1.0 =
            vkc / (log (2.0 / z0q)
              + 5.0 * zeta_2m - 5.0 * z0q / obu)
        | otherwise =
            vkc / (log (obu / z0q)
              + 5.0 - 5.0 * z0q / obu
              + (5.0 * log zeta_2m + zeta_2m - 1.0))

  in (max ustar_v 0.001, max temp1_v 0.001, max temp2_v 0.001,
      max temp12m_v 0.001, max temp22m_v 0.001)

-- | Compute stability parameters for one iteration.
-- Uses full Businger-Dyer stability corrections matching
-- friction_velocity! in FrictionVelocityMod.F90.
computeZetaStability :: BareGroundFluxesParams -> Z0ParamMethod
                     -> Double -> Double -> Double -> Double -> Double
                     -> Double -> Double -> Double -> Double -> Double
                     -> Double -> Double -> Double -> Double -> Double
                     -> ZetaStabilityResult
computeZetaStability params z0method
    forc_q forc_th thv beta_val zii
    z0mg z0hg0 z0qg0 zldis dth dqh
    ur0 um0 _ustar0 obu0 =
  let
      -- Heights for temperature/humidity profiles
      -- In Julia: forc_hgt_t_patch = forc_hgt_t_grc + z0hg + displa
      -- friction_velocity! uses: zldis_t = forc_hgt_t_patch - displa
      -- For bare ground (displa=0): zldis_t = forc_hgt_t_grc + z0hg
      -- zldis = forc_hgt_u_patch = forc_hgt_u_grc + z0mg + displa
      -- forc_hgt_base = forc_hgt_u_grc = zldis - z0mg (since displa=0)
      !forc_hgt_base = zldis - z0mg
      !zldis_t = forc_hgt_base + z0hg0
      !zldis_q = forc_hgt_base + z0qg0

      -- Full stability-corrected friction velocity and transfer coefficients
      (!ustar_v, !temp1_v, !temp2_v, !_t12m, !_t22m) =
          frictionVelBG z0mg z0hg0 z0qg0 obu0 um0 zldis zldis_t zldis_q

      !tstar   = temp1_v * dth
      !qstar   = temp2_v * dqh

      -- Z0h update
      !z0hg_new = case z0method of
                    ZengWang2007 -> z0mg / exp (bgf_a_coef params *
                        (ustar_v * z0mg / nuParam) ** bgf_a_exp params)
                    Meier2022 -> meierParam3 * nuParam / ustar_v *
                        exp (-betaParam * sqrt ustar_v * abs tstar ** 0.25)
      !z0qg_new = z0hg_new

      !thvstar  = tstar * (1.0 + 0.61 * forc_q) + 0.61 * forc_th * qstar
      !zeta_new = zldis * vkc * grav * thvstar / (ustar_v * ustar_v * thv)

      (!zeta_final, !um_new) =
        if zeta_new >= 0.0
        then (min 2.0 (max zeta_new 0.01), max ur0 0.1)
        else let !wc_arg = max ((-grav) * ustar_v * thvstar * zii / thv) 0.0
                 !wc     = beta_val * wc_arg ** (1.0/3.0)
             in (max (-100.0) (min zeta_new (-0.01)), sqrt (ur0*ur0 + wc*wc))

      !obu_new = zldis / zeta_final
  in ZetaStabilityResult
       { zsr_zeta  = zeta_final
       , zsr_um    = um_new
       , zsr_obu   = obu_new
       , zsr_z0hg  = z0hg_new
       , zsr_z0qg  = z0qg_new
       , zsr_ustar = ustar_v
       , zsr_temp1 = temp1_v
       , zsr_temp2 = temp2_v
       , zsr_temp12m = _t12m
       , zsr_temp22m = _t22m
       }

-- =========================================================================
-- Main driver
-- =========================================================================

-- | Compute bare-ground sensible and latent heat fluxes for a single patch.
-- Pure function: takes all inputs, returns all outputs.
baregroundFluxes :: BareGroundFluxesInput -> BareGroundFluxesOutput
baregroundFluxes inp =
  let
      params  = bgi_params inp
      z0m_mtd = bgi_z0param_method inp

      -- Wind speed
      !ur0 = max (bgf_wind_min params)
                 (sqrt (bgi_forc_u inp ** 2 + bgi_forc_v inp ** 2))
      !dth0 = bgi_thm inp - bgi_t_grnd inp
      !dqh0 = bgi_forc_q inp - bgi_qg inp
      !dthv0 = dth0 * (1.0 + 0.61 * bgi_forc_q inp)
             + 0.61 * bgi_forc_th inp * dqh0
      -- zldis = forc_hgt_u_patch (already includes z0mg + displa)
      !zldis0 = bgi_forc_hgt_u inp

      -- Initialize Monin-Obukhov
      (!um_init, !obu_init) = moninObukIni (bgi_zetamaxstable inp) ur0
                                           (bgi_thv inp) dthv0 zldis0
                                           (bgi_z0mg inp)

      -- Run stability iterations
      -- State: (um, obu, z0h, z0q, ustar, temp1, temp2, temp12m, temp22m)
      !z0h_init = bgi_z0hg inp
      !z0q_init = bgi_z0qg inp
      iterState0 = (um_init, obu_init, z0h_init, z0q_init,
                    0.001, 0.001, 0.001, 0.001, 0.001)

      iterStep (!um_s, !obu_s, !z0h_s, !z0q_s, !_us_s,
                !_t1_s, !_t2_s, !_t12_s, !_t22_s) _iter_n =
        let !res = computeZetaStability params z0m_mtd
                     (bgi_forc_q inp) (bgi_forc_th inp) (bgi_thv inp)
                     (bgi_beta inp) (bgi_zii inp)
                     (bgi_z0mg inp) z0h_s z0q_s zldis0 dth0 dqh0
                     ur0 um_s _us_s obu_s
        in (zsr_um res, zsr_obu res, zsr_z0hg res, zsr_z0qg res,
            zsr_ustar res, zsr_temp1 res, zsr_temp2 res,
            zsr_temp12m res, zsr_temp22m res)

      (!um_f, !_obu_f, !z0hg_f, !z0qg_f, !ustar_f,
       !temp1_f, !temp2_f, !temp12m_f, !temp22m_f) =
        foldl iterStep iterState0 [1..niters]

      -- Aerodynamic resistances
      !ram  = 1.0 / (ustar_f * ustar_f / um_f)
      !rah  = 1.0 / (temp1_f * ustar_f)
      !raw  = 1.0 / (temp2_f * ustar_f)
      !raih = bgi_forc_rho inp * cpair / rah

      -- Soil evaporation resistance
      !www0 = (bgi_h2osoi_liq_top inp / denh2o + bgi_h2osoi_ice_top inp / denice)
              / bgi_dz_top inp / bgi_watsat_top inp
      !www  = min (max www0 0.0) 1.0

      !raiw = if dqh0 > 0.0
              then bgi_forc_rho inp / raw  -- dew
              else if bgi_do_soilevap_beta inp
                   then bgi_soilbeta inp * bgi_forc_rho inp / raw
                   else if bgi_do_soil_resistance inp
                        then bgi_forc_rho inp / (raw + bgi_soilresis inp)
                        else bgi_forc_rho inp / raw

      -- Flux derivatives
      !cgrnds = raih
      !cgrndl = raiw * bgi_dqgdT inp
      !cgrnd  = cgrnds + bgi_htvp inp * cgrndl

      -- Momentum fluxes
      !taux = -(bgi_forc_rho inp) * bgi_forc_u inp / ram
      !tauy = -(bgi_forc_rho inp) * bgi_forc_v inp / ram

      -- Sensible heat
      !eflx_sh_grnd = -raih * dth0
      !eflx_sh_snow = -raih * (bgi_thm inp - bgi_t_soisno_top inp)
      !eflx_sh_soil = -raih * (bgi_thm inp - bgi_t_soil1 inp)
      !eflx_sh_h2osfc = -raih * (bgi_thm inp - bgi_t_h2osfc inp)

      -- Latent heat (evaporation)
      !qflx_evap_soi = -raiw * dqh0
      !qflx_ev_snow = -raiw * (bgi_forc_q inp - bgi_qg_snow inp)
      !qflx_ev_soil = -raiw * (bgi_forc_q inp - bgi_qg_soil inp)
      !qflx_ev_h2osfc = -raiw * (bgi_forc_q inp - bgi_qg_h2osfc inp)

      -- 2m diagnostics
      !t_ref2m = bgi_thm inp + temp1_f * dth0 * (1.0/temp12m_f - 1.0/temp1_f)
      !q_ref2m = bgi_forc_q inp + temp2_f * dqh0 * (1.0/temp22m_f - 1.0/temp2_f)

      -- Saturation humidity at 2m for RH
      !es_ref = 611.2 * exp (17.67 * (t_ref2m - tfrz) / (t_ref2m - tfrz + 243.5))
      !qsat_ref = 0.622 * es_ref / max (bgi_forc_pbot inp - 0.378 * es_ref) 1.0
      !rh_ref2m = min 100.0 (q_ref2m / qsat_ref * 100.0)

      !kbm1 = log (bgi_z0mg inp / z0hg_f)

  in BareGroundFluxesOutput
       { bgo_taux           = taux
       , bgo_tauy           = tauy
       , bgo_eflx_sh_grnd   = eflx_sh_grnd
       , bgo_eflx_sh_tot    = eflx_sh_grnd
       , bgo_eflx_sh_snow   = eflx_sh_snow
       , bgo_eflx_sh_soil   = eflx_sh_soil
       , bgo_eflx_sh_h2osfc = eflx_sh_h2osfc
       , bgo_qflx_evap_soi  = qflx_evap_soi
       , bgo_qflx_evap_tot  = qflx_evap_soi
       , bgo_qflx_ev_snow   = qflx_ev_snow
       , bgo_qflx_ev_soil   = qflx_ev_soil
       , bgo_qflx_ev_h2osfc = qflx_ev_h2osfc
       , bgo_cgrnds         = cgrnds
       , bgo_cgrndl         = cgrndl
       , bgo_cgrnd          = cgrnd
       , bgo_t_ref2m        = t_ref2m
       , bgo_q_ref2m        = q_ref2m
       , bgo_rh_ref2m       = rh_ref2m
       , bgo_ram1           = ram
       , bgo_z0hg_out       = z0hg_f
       , bgo_z0qg_out       = z0qg_f
       , bgo_ustar          = ustar_f
       , bgo_kbm1           = kbm1
       }
