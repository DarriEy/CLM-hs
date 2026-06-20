-- | Canopy turbulence and flux computation.
-- Ported from: CanopyFluxesMod.F90
--
-- Calculates leaf temperature and surface fluxes using Newton-Raphson
-- iteration to solve the surface energy budget:
--   f(t_veg) = Net radiation - Sensible - Latent = 0
--
-- Core physics:
--   1. Monin-Obukhov similarity theory for stability
--   2. Friction velocity and aerodynamic resistances
--   3. Stomatal resistance (via Photosynthesis module)
--   4. Newton-Raphson iteration for leaf temperature
--   5. Sensible / latent heat partitioning
--
-- Fortran: CanopyFluxesMod
module CLM.BioGeoPhys.CanopyFluxes
  ( -- * Configuration
    CanopyFluxesParams(..)
  , defaultCanopyFluxesParams
  , CanopyFluxesControl(..)
  , defaultCanopyFluxesControl
    -- * Input/Output records
  , CanopyFluxesInput(..)
  , CanopyFluxesOutput(..)
  , defaultCanopyFluxesOutput
    -- * Stability functions (Monin-Obukhov)
  , moninObukIni
  , stabilityPsiM
  , stabilityPsiH
  , frictionVelocityProfile
    -- * QSat (saturation specific humidity)
  , qsat
    -- * Main entry point
  , canopyFluxes
  ) where

import CLM.Constants.PhysicalConstants (grav, sb, hvap, tfrz)

-- =========================================================================
-- Constants (from CanopyFluxes subroutine, Fortran module variables)
-- =========================================================================

-- | von Karman constant [-]
vkc :: Double
vkc = 0.4

-- | Specific heat of dry air [J/kg/K]
cpair :: Double
cpair = 1004.64

-- | Kinematic viscosity of air [m^2/s]
nu_param :: Double
nu_param = 1.5e-5

-- | Maximum change in leaf temperature per iteration [K]
delmax_canopy :: Double
delmax_canopy = 1.0

-- | Energy flux convergence tolerance [W/m^2]
dlemin_canopy :: Double
dlemin_canopy = 0.1

-- | Temperature convergence tolerance [K]
dtmin_canopy :: Double
dtmin_canopy = 0.01

-- | Minimum number of iterations [-]
itmin_canopy :: Int
itmin_canopy = 2

-- | Coefficient of convective velocity [-]
beta_canopy :: Double
beta_canopy = 1.0

-- | Convective boundary layer height [m]
zii_canopy :: Double
zii_canopy = 1000.0

-- | Free parameter for stable formulation [-]
ria_canopy :: Double
ria_canopy = 0.5

-- | Initial btran value (no transpiration threshold)
btran0 :: Double
btran0 = 0.0

-- | Critical LAI+SAI for Zeng-Wang roughness [-]
tlsai_crit :: Double
tlsai_crit = 2.0

-- | Alpha parameter for Zeng-Wang roughness [-]
alpha_aero :: Double
alpha_aero = 1.0

-- =========================================================================
-- Parameters type (from params_type in Fortran)
-- =========================================================================

-- | Tunable parameters for canopy fluxes, read from parameter file.
-- Ported from @params_type@ in @CanopyFluxesMod.F90@.
data CanopyFluxesParams = CanopyFluxesParams
  { cfp_lai_dl   :: !Double   -- ^ Plant litter area index [m^2/m^2]
  , cfp_z_dl     :: !Double   -- ^ Litter layer thickness [m]
  , cfp_a_coef   :: !Double   -- ^ Drag coefficient under less dense canopy [-]
  , cfp_a_exp    :: !Double   -- ^ Drag exponent under less dense canopy [-]
  , cfp_csoilc   :: !Double   -- ^ Soil drag coefficient under dense canopy [-]
  , cfp_cv       :: !Double   -- ^ Turbulent transfer coeff canopy-to-air [m/s^(1/2)]
  , cfp_wind_min :: !Double   -- ^ Minimum wind speed at forcing height [m/s]
  } deriving (Show, Eq)

-- | Default canopy fluxes parameters matching CLM5.
defaultCanopyFluxesParams :: CanopyFluxesParams
defaultCanopyFluxesParams = CanopyFluxesParams
  { cfp_lai_dl   = 0.5
  , cfp_z_dl     = 0.05
  , cfp_a_coef   = 0.13
  , cfp_a_exp    = 0.45
  , cfp_csoilc   = 0.004
  , cfp_cv       = 0.01
  , cfp_wind_min = 1.0
  }

-- =========================================================================
-- Control flags
-- =========================================================================

-- | Module-level control flags for canopy fluxes.
-- Ported from module variables in @CanopyFluxesMod.F90@.
data CanopyFluxesControl = CanopyFluxesControl
  { cfc_perchroot                 :: !Bool  -- ^ btran based only on unfrozen soil levels
  , cfc_perchroot_alt             :: !Bool  -- ^ btran based on active layer
  , cfc_use_undercanopy_stability :: !Bool  -- ^ use undercanopy stability term
  , cfc_itmax_canopy_fluxes       :: !Int   -- ^ max iterations in CanopyFluxes
  , cfc_use_biomass_heat_storage  :: !Bool  -- ^ use biomass heat storage
  } deriving (Show, Eq)

-- | Default control flags matching CLM5.
defaultCanopyFluxesControl :: CanopyFluxesControl
defaultCanopyFluxesControl = CanopyFluxesControl
  { cfc_perchroot                 = False
  , cfc_perchroot_alt             = False
  , cfc_use_undercanopy_stability = False
  , cfc_itmax_canopy_fluxes       = 40
  , cfc_use_biomass_heat_storage  = False
  }

-- =========================================================================
-- QSat: Saturation specific humidity
-- =========================================================================

-- | Compute saturation specific humidity, vapor pressure, and derivative.
-- Ported from QSatMod.F90.
--
-- Returns (qsat, esat, dqsat_dT, desat_dT).
qsat :: Double   -- ^ Temperature [K]
     -> Double   -- ^ Pressure [Pa]
     -> (Double, Double, Double, Double)
qsat t p =
  let -- Saturation vapor pressure [Pa] — Flatau polynomial fit
      td = t - tfrz
      (a, b) | t > tfrz  = (overWater_a, overWater_b)
             | otherwise = (overIce_a, overIce_b)
      es = evalPoly a td
      ds = evalPoly b td
      -- Saturation specific humidity
      denom_q = max (p - 0.378 * es) 1.0
      qs    = 0.622 * es / denom_q
      dqsdT = 0.622 * p * ds / (denom_q * denom_q)
  in (qs, es, dqsdT, ds)
  where
    -- Polynomial coefficients for saturation vapor pressure over water
    overWater_a :: [Double]
    overWater_a =
      [ 611.21, 44.481, 1.4289, 0.026506, 3.0312e-4, 2.0340e-6, 6.1368e-9 ]
    overWater_b :: [Double]
    overWater_b =
      [ 44.481, 2.8578, 0.079518, 1.2124e-3, 1.0170e-5, 3.6821e-8 ]
    -- Polynomial coefficients for saturation vapor pressure over ice
    overIce_a :: [Double]
    overIce_a =
      [ 611.15, 50.344, 1.8860, 0.040825, 5.3675e-4, 4.1767e-6, 1.4634e-8 ]
    overIce_b :: [Double]
    overIce_b =
      [ 50.344, 3.7720, 0.12247, 2.1470e-3, 2.5883e-5, 8.7808e-8 ]

    evalPoly :: [Double] -> Double -> Double
    evalPoly cs x = foldr (\c acc -> c + x * acc) 0.0 cs

-- =========================================================================
-- Monin-Obukhov initialization
-- =========================================================================

-- | Initialize Monin-Obukhov length and wind speed.
-- Returns (um, obu) where um = effective wind, obu = Obukhov length.
-- Ported from @MoninObukIni@ in @FrictionVelocityMod.F90@.
moninObukIni :: Double  -- ^ zetamaxstable (max stable zeta)
             -> Double  -- ^ ur (reference wind speed) [m/s]
             -> Double  -- ^ thv (virtual potential temp at surface) [K]
             -> Double  -- ^ dthv (virtual pot temp difference) [K]
             -> Double  -- ^ zldis (reference height - displacement) [m]
             -> Double  -- ^ z0m (roughness length for momentum) [m]
             -> (Double, Double)  -- ^ (um, obu)
moninObukIni zetamaxstable ur thv dthv zldis z0m =
  -- Faithful port of Fortran MoninObukIni (FrictionVelocityMod.F90) and the
  -- Julia reference (canopy_fluxes.jl monin_obuk_ini): the initial convective
  -- velocity is the FIXED constant wc = 0.5 (NOT the Sakaguchi/Zeng convective
  -- velocity scale), and the bulk Richardson number uses the resulting @um@ in
  -- its denominator — not @ur@. Using the full convective formula here inflated
  -- um0 (~5.3 vs 1.12) and pushed the leaf-temperature / Monin-Obukhov solve
  -- into the wrong stability basin (ustar too low, rah_above too high, under-
  -- canopy ground sensible heat sign-flipped at peak sun).
  let wc   = 0.5 :: Double
      um   = if dthv >= 0.0          -- neutral or stable
             then max ur 0.1
             else sqrt (ur * ur + wc * wc)
      rib  = grav * zldis * dthv / (thv * um * um)
      -- Initial Obukhov length from Richardson number
      zeta | rib >= 0.0  = rib * log (zldis / z0m) / (1.0 - 5.0 * min rib 0.19)
           | otherwise   = rib * log (zldis / z0m)
      zeta' | zeta >= 0.0 = min zetamaxstable (max zeta 0.01)
            | otherwise   = max (-100.0) (min zeta (-0.01))
      obu  = zldis / zeta'
  in (um, obu)

-- =========================================================================
-- Stability functions — full 4-branch Businger-Dyer corrections
-- Matches friction_velocity! in FrictionVelocityMod.F90 / friction_velocity.jl
-- =========================================================================

-- | Stability correction for momentum (psi_m), only for unstable (zeta <= 0).
-- Julia: stability_func1 in friction_velocity.jl
stabilityPsiM :: Double -> Double
stabilityPsiM zeta =
  let zeta' = min zeta 0.0
      chik2 = sqrt (1.0 - 16.0 * zeta')
      chik  = sqrt chik2
  in 2.0 * log ((1.0 + chik) * 0.5) + log ((1.0 + chik2) * 0.5) - 2.0 * atan chik + pi / 2.0

-- | Stability correction for heat/moisture (psi_h), only for unstable (zeta <= 0).
-- Julia: stability_func2 in friction_velocity.jl
stabilityPsiH :: Double -> Double
stabilityPsiH zeta =
  let zeta' = min zeta 0.0
      chik2 = sqrt (1.0 - 16.0 * zeta')
  in 2.0 * log ((1.0 + chik2) * 0.5)

-- =========================================================================
-- Friction velocity profiles — full 4-branch matching friction_velocity!
-- =========================================================================

-- | Compute friction velocity and profile functions with full 4-branch
-- stability corrections matching friction_velocity! in FrictionVelocityMod.F90.
-- Transition points: zetam=1.574 (momentum), zetat=0.465 (heat/moisture).
-- Returns (ustar, temp1, temp2, temp12m, temp22m, fm).
frictionVelocityProfile
  :: Double  -- ^ displa (displacement height) [m]
  -> Double  -- ^ z0m (roughness length for momentum) [m]
  -> Double  -- ^ z0h (roughness length for heat) [m]
  -> Double  -- ^ z0q (roughness length for moisture) [m]
  -> Double  -- ^ obu (Obukhov length) [m]
  -> Double  -- ^ um (effective wind speed) [m/s]
  -> Double  -- ^ forc_hgt_u (forcing height for wind) [m]
  -> Double  -- ^ forc_hgt_t (forcing height for temperature) [m]
  -> Double  -- ^ forc_hgt_q (forcing height for humidity) [m]
  -> (Double, Double, Double, Double, Double, Double)
     -- ^ (ustar, temp1, temp2, temp12m, temp22m, fm)
frictionVelocityProfile displa z0m z0h z0q obu um
                        forc_hgt_u forc_hgt_t forc_hgt_q =
  let zetam = 1.574
      zetat = 0.465

      zldis_u = forc_hgt_u - displa
      zldis_t = forc_hgt_t - displa
      zldis_q = forc_hgt_q - displa

      -- 4-branch wind profile (momentum)
      zeta_u = zldis_u / obu
      fm_val
        | zeta_u < (-zetam) =
            log((-zetam) * obu / z0m)
              - stabilityPsiM (-zetam) + stabilityPsiM (z0m / obu)
              + 1.14 * ((-zeta_u) ** (1.0/3.0) - zetam ** (1.0/3.0))
        | zeta_u < 0.0 =
            log(zldis_u / z0m) - stabilityPsiM zeta_u + stabilityPsiM (z0m / obu)
        | zeta_u <= 1.0 =
            log(zldis_u / z0m) + 5.0 * zeta_u - 5.0 * z0m / obu
        | otherwise =
            log(obu / z0m) + 5.0 - 5.0 * z0m / obu
              + (5.0 * log zeta_u + zeta_u - 1.0)
      fm_safe = max fm_val 0.01
      ustar_val = vkc * um / fm_safe

      -- 4-branch heat profile helper
      heatProfile zldis z0 =
        let zeta = zldis / obu
        in if zeta < (-zetat)
           then log((-zetat) * obu / z0)
                  - stabilityPsiH (-zetat) + stabilityPsiH (z0 / obu)
                  + 0.8 * ((zetat ** (-1.0/3.0)) - ((-zeta) ** (-1.0/3.0)))
           else if zeta < 0.0
           then log(zldis / z0) - stabilityPsiH zeta + stabilityPsiH (z0 / obu)
           else if zeta <= 1.0
           then log(zldis / z0) + 5.0 * zeta - 5.0 * z0 / obu
           else log(obu / z0) + 5.0 - 5.0 * z0 / obu
                  + (5.0 * log zeta + zeta - 1.0)

      fh = max (heatProfile zldis_t z0h) 0.01
      fq = max (heatProfile zldis_q z0q) 0.01

      temp1_val  = vkc / fh
      temp2_val  = vkc / fq

      -- 2m reference height
      fh_2m = max (heatProfile 2.0 z0h) 0.01
      fq_2m = max (heatProfile 2.0 z0q) 0.01
      temp12m_val = vkc / fh_2m
      temp22m_val = vkc / fq_2m
  in (ustar_val, temp1_val, temp2_val, temp12m_val, temp22m_val, fm_safe)

-- =========================================================================
-- Input / Output records for canopy fluxes
-- =========================================================================

-- | All inputs needed for canopy flux computation (single patch, single column).
data CanopyFluxesInput = CanopyFluxesInput
  { -- Atmospheric forcing (column-level)
    cfi_forc_lwrad      :: !Double   -- ^ Downward longwave radiation [W/m^2]
  , cfi_forc_q          :: !Double   -- ^ Specific humidity at reference height [kg/kg]
  , cfi_forc_pbot       :: !Double   -- ^ Atmospheric pressure [Pa]
  , cfi_forc_th         :: !Double   -- ^ Potential temperature [K]
  , cfi_forc_rho        :: !Double   -- ^ Air density [kg/m^3]
  , cfi_forc_t          :: !Double   -- ^ Air temperature at reference height [K]
  , cfi_forc_u          :: !Double   -- ^ East wind component [m/s]
  , cfi_forc_v          :: !Double   -- ^ North wind component [m/s]
  , cfi_forc_hgt_u      :: !Double   -- ^ Observation height for wind [m]
  , cfi_forc_hgt_t      :: !Double   -- ^ Observation height for temperature [m]
  , cfi_forc_hgt_q      :: !Double   -- ^ Observation height for humidity [m]
    -- Vegetation state
  , cfi_elai            :: !Double   -- ^ Exposed one-sided LAI [m^2/m^2]
  , cfi_esai            :: !Double   -- ^ Exposed one-sided SAI [m^2/m^2]
  , cfi_htop            :: !Double   -- ^ Canopy top height [m]
  , cfi_displa          :: !Double   -- ^ Displacement height [m]
  , cfi_z0mv            :: !Double   -- ^ Roughness length for momentum (vegetation) [m]
  , cfi_z0mg            :: !Double   -- ^ Roughness length for momentum (ground) [m]
  , cfi_frac_veg_nosno  :: !Int      -- ^ Fraction of vegetation that is not snow-covered (0 or 1)
    -- Emissivities
  , cfi_emv             :: !Double   -- ^ Vegetation emissivity [-]
  , cfi_emg             :: !Double   -- ^ Ground emissivity [-]
    -- Temperature state
  , cfi_t_veg           :: !Double   -- ^ Initial vegetation temperature [K]
  , cfi_t_grnd          :: !Double   -- ^ Ground surface temperature [K]
  , cfi_thm             :: !Double   -- ^ Intermediate potential temperature [K]
  , cfi_thv             :: !Double   -- ^ Virtual potential temperature [K]
  , cfi_t_soisno_top    :: !Double   -- ^ Top snow/soil layer temperature [K]
  , cfi_t_soisno_topsoil :: !Double  -- ^ Top soil layer temperature [K]
  , cfi_t_h2osfc        :: !Double   -- ^ Surface water temperature [K]
  , cfi_t_stem          :: !Double   -- ^ Stem temperature [K]
    -- Absorbed radiation
  , cfi_sabv            :: !Double   -- ^ Solar absorbed by vegetation [W/m^2]
    -- Surface humidity
  , cfi_qg              :: !Double   -- ^ Ground specific humidity [kg/kg]
  , cfi_qg_snow         :: !Double   -- ^ Snow surface specific humidity [kg/kg]
  , cfi_qg_soil         :: !Double   -- ^ Soil surface specific humidity [kg/kg]
  , cfi_qg_h2osfc       :: !Double   -- ^ Surface water specific humidity [kg/kg]
  , cfi_dqgdT           :: !Double   -- ^ d(qg)/dT [kg/kg/K]
    -- Snow/water fractions
  , cfi_frac_sno_eff    :: !Double   -- ^ Effective snow cover fraction [-]
  , cfi_frac_h2osfc     :: !Double   -- ^ Surface water fraction [-]
  , cfi_snow_depth      :: !Double   -- ^ Snow depth [m]
    -- Vegetation water
  , cfi_fwet            :: !Double   -- ^ Fraction of canopy that is wet [-]
  , cfi_fdry            :: !Double   -- ^ Fraction of canopy that is dry [-]
  , cfi_liqcan          :: !Double   -- ^ Liquid water on canopy [kg/m^2]
  , cfi_snocan          :: !Double   -- ^ Snow water on canopy [kg/m^2]
    -- Stomatal conductance / photosynthesis
  , cfi_rssun           :: !Double   -- ^ Sunlit stomatal resistance [s/m]
  , cfi_rssha           :: !Double   -- ^ Shaded stomatal resistance [s/m]
  , cfi_laisun          :: !Double   -- ^ Sunlit LAI [m^2/m^2]
  , cfi_laisha          :: !Double   -- ^ Shaded LAI [m^2/m^2]
  , cfi_btran           :: !Double   -- ^ Transpiration wetness factor [-]
    -- Soil evaporation resistance
  , cfi_soilbeta        :: !Double   -- ^ Soil wetness factor for evaporation [-]
  , cfi_soilresis       :: !Double   -- ^ Soil evaporative resistance [s/m]
    -- Ground heat flux derivative
  , cfi_htvp            :: !Double   -- ^ Latent heat vap/sub at ground [J/kg]
  , cfi_cgrnds          :: !Double   -- ^ Deriv of ground SH wrt ground T [W/m^2/K]
  , cfi_cgrndl          :: !Double   -- ^ Deriv of ground LH wrt ground T [W/m^2/K]
    -- Soil evaporation method
  , cfi_do_soilevap_beta :: !Bool    -- ^ True = beta method for ground evap
    -- Time step
  , cfi_dtime           :: !Double   -- ^ Model time step [s]
    -- Stability parameter
  , cfi_zetamaxstable   :: !Double   -- ^ Maximum stable zeta value [-]
    -- Leaf/stem dimension
  , cfi_dleaf           :: !Double   -- ^ Leaf dimension [m]
    -- Snow layer index (snl, <= 0)
  , cfi_snl             :: !Int      -- ^ Number of snow layers (negative or 0)
  } deriving (Show)

-- | Outputs from canopy flux computation (single patch).
data CanopyFluxesOutput = CanopyFluxesOutput
  { -- Energy fluxes
    cfo_eflx_sh_veg     :: !Double   -- ^ Sensible heat from vegetation [W/m^2]
  , cfo_eflx_sh_grnd    :: !Double   -- ^ Sensible heat from ground [W/m^2]
  , cfo_eflx_sh_snow    :: !Double   -- ^ Sensible heat from snow [W/m^2]
  , cfo_eflx_sh_soil    :: !Double   -- ^ Sensible heat from soil [W/m^2]
  , cfo_eflx_sh_h2osfc  :: !Double   -- ^ Sensible heat from surface water [W/m^2]
  , cfo_eflx_sh_stem    :: !Double   -- ^ Sensible heat from stem [W/m^2]
    -- Momentum fluxes
  , cfo_taux            :: !Double   -- ^ East wind stress [kg/m/s^2]
  , cfo_tauy            :: !Double   -- ^ North wind stress [kg/m/s^2]
    -- Water fluxes
  , cfo_qflx_evap_veg   :: !Double   -- ^ Vegetation evapotranspiration [mm/s]
  , cfo_qflx_tran_veg   :: !Double   -- ^ Transpiration [mm/s]
  , cfo_qflx_evap_soi   :: !Double   -- ^ Ground evaporation [mm/s]
  , cfo_qflx_ev_snow    :: !Double   -- ^ Snow evaporation [mm/s]
  , cfo_qflx_ev_soil    :: !Double   -- ^ Soil evaporation [mm/s]
  , cfo_qflx_ev_h2osfc  :: !Double   -- ^ Surface water evaporation [mm/s]
    -- Updated temperatures
  , cfo_t_veg           :: !Double   -- ^ Updated vegetation temperature [K]
  , cfo_t_ref2m         :: !Double   -- ^ 2-m reference temperature [K]
  , cfo_t_stem          :: !Double   -- ^ Updated stem temperature [K]
    -- Updated humidity
  , cfo_q_ref2m         :: !Double   -- ^ 2-m specific humidity [kg/kg]
  , cfo_rh_ref2m        :: !Double   -- ^ 2-m relative humidity [%]
    -- Canopy air
  , cfo_taf             :: !Double   -- ^ Canopy air temperature [K]
  , cfo_qaf             :: !Double   -- ^ Canopy air humidity [kg/kg]
    -- Friction velocity diagnostics
  , cfo_ustar           :: !Double   -- ^ Friction velocity [m/s]
  , cfo_ram1            :: !Double   -- ^ Aerodynamic resistance (momentum) [s/m]
  , cfo_rb1             :: !Double   -- ^ Boundary layer resistance [s/m]
  , cfo_obu             :: !Double   -- ^ Obukhov length [m]
  , cfo_zeta            :: !Double   -- ^ Stability parameter z/L [-]
  , cfo_um              :: !Double   -- ^ Effective wind speed [m/s]
  , cfo_num_iter        :: !Int      -- ^ Number of iterations taken
    -- Resistance diagnostics
  , cfo_rah_above       :: !Double   -- ^ Above-canopy aerodynamic resistance [s/m]
  , cfo_raw_above       :: !Double   -- ^ Above-canopy moisture resistance [s/m]
  , cfo_rah_below       :: !Double   -- ^ Below-canopy aerodynamic resistance [s/m]
  , cfo_raw_below       :: !Double   -- ^ Below-canopy moisture resistance [s/m]
  , cfo_vpd             :: !Double   -- ^ Vapor pressure deficit [kPa]
    -- Longwave
  , cfo_dlrad           :: !Double   -- ^ Downward longwave below canopy [W/m^2]
  , cfo_ulrad           :: !Double   -- ^ Upward longwave above canopy [W/m^2]
    -- Ground heat coupling
  , cfo_cgrnds          :: !Double   -- ^ Updated deriv of ground SH [W/m^2/K]
  , cfo_cgrndl          :: !Double   -- ^ Updated deriv of ground LH [W/m^2/K]
  , cfo_cgrnd           :: !Double   -- ^ Total ground heat coupling [W/m^2/K]
    -- Canopy water state updates
  , cfo_liqcan          :: !Double   -- ^ Updated liquid water on canopy [kg/m^2]
  , cfo_snocan          :: !Double   -- ^ Updated snow water on canopy [kg/m^2]
    -- Energy balance error
  , cfo_err             :: !Double   -- ^ Energy balance error [W/m^2]
    -- Relative humidity at canopy air
  , cfo_rh_af           :: !Double   -- ^ Relative humidity at canopy air [-]
  } deriving (Show)

-- | Default (zero) output.
defaultCanopyFluxesOutput :: CanopyFluxesOutput
defaultCanopyFluxesOutput = CanopyFluxesOutput
  { cfo_eflx_sh_veg    = 0.0
  , cfo_eflx_sh_grnd   = 0.0
  , cfo_eflx_sh_snow   = 0.0
  , cfo_eflx_sh_soil   = 0.0
  , cfo_eflx_sh_h2osfc = 0.0
  , cfo_eflx_sh_stem   = 0.0
  , cfo_taux           = 0.0
  , cfo_tauy           = 0.0
  , cfo_qflx_evap_veg  = 0.0
  , cfo_qflx_tran_veg  = 0.0
  , cfo_qflx_evap_soi  = 0.0
  , cfo_qflx_ev_snow   = 0.0
  , cfo_qflx_ev_soil   = 0.0
  , cfo_qflx_ev_h2osfc = 0.0
  , cfo_t_veg          = 283.0
  , cfo_t_ref2m        = 283.0
  , cfo_t_stem         = 283.0
  , cfo_q_ref2m        = 0.0
  , cfo_rh_ref2m       = 0.0
  , cfo_taf            = 0.0
  , cfo_qaf            = 0.0
  , cfo_ustar          = 0.0
  , cfo_ram1           = 0.0
  , cfo_rb1            = 0.0
  , cfo_obu            = 0.0
  , cfo_zeta           = 0.0
  , cfo_um             = 0.0
  , cfo_num_iter       = 0
  , cfo_rah_above      = 0.0
  , cfo_raw_above      = 0.0
  , cfo_rah_below      = 0.0
  , cfo_raw_below      = 0.0
  , cfo_vpd            = 0.0
  , cfo_dlrad          = 0.0
  , cfo_ulrad          = 0.0
  , cfo_cgrnds         = 0.0
  , cfo_cgrndl         = 0.0
  , cfo_cgrnd          = 0.0
  , cfo_liqcan         = 0.0
  , cfo_snocan         = 0.0
  , cfo_err            = 0.0
  , cfo_rh_af          = 0.0
  }

-- =========================================================================
-- Iteration state (internal, threaded through the iteration loop)
-- =========================================================================

-- | Internal state carried through each iteration of the Newton-Raphson
-- stability loop. Not exported.
data IterState = IterState
  { is_t_veg    :: !Double   -- ^ Current leaf temperature [K]
  , is_t_stem   :: !Double   -- ^ Current stem temperature [K]
  , is_obu      :: !Double   -- ^ Obukhov length [m]
  , is_um       :: !Double   -- ^ Effective wind speed [m/s]
  , is_ustar    :: !Double   -- ^ Friction velocity [m/s]
  , is_taf      :: !Double   -- ^ Canopy air temperature [K]
  , is_qaf      :: !Double   -- ^ Canopy air humidity [kg/kg]
  , is_del      :: !Double   -- ^ Previous |delta T_veg| [K]
  , is_efeb     :: !Double   -- ^ Previous latent heat flux [W/m^2]
  , is_nmozsgn  :: !Int      -- ^ Sign changes in Monin-Obukhov
  , is_obuold   :: !Double   -- ^ Previous Obukhov length [m]
  , is_qsatl    :: !Double   -- ^ Sat specific humidity at leaf [kg/kg]
  , is_el       :: !Double   -- ^ Sat vapor pressure at leaf [Pa]
  , is_qsatldT  :: !Double   -- ^ d(qsatl)/dT [kg/kg/K]
    -- Fluxes from last iteration
  , is_eflx_sh_veg  :: !Double
  , is_eflx_sh_stem :: !Double
  , is_qflx_evap_veg :: !Double
  , is_qflx_tran_veg :: !Double
  , is_err        :: !Double
    -- Conductance weights (needed for post-iteration diagnostics)
  , is_wtg       :: !Double
  , is_wtgq      :: !Double
  , is_wta0      :: !Double
  , is_wtl0      :: !Double
  , is_wtstem0   :: !Double
  , is_wtga      :: !Double
  , is_wtal      :: !Double
  , is_wtaq0     :: !Double
  , is_wtlq0     :: !Double
  , is_wtalq     :: !Double
  , is_rb        :: !Double
  , is_ram1      :: !Double
  , is_dt_veg    :: !Double
  , is_tlbef     :: !Double
  , is_delq      :: !Double
    -- Profile coefficients
  , is_temp1     :: !Double
  , is_temp2     :: !Double
  , is_temp12m   :: !Double
  , is_temp22m   :: !Double
  , is_fm        :: !Double
    -- Aerodynamic resistances
  , is_rah_above :: !Double
  , is_raw_above :: !Double
  , is_rah_below :: !Double
  , is_raw_below :: !Double
    -- Iteration tracking
  , is_itlef     :: !Int
  , is_converged :: !Bool
    -- Longwave intermediates
  , is_lw_grnd   :: !Double
  , is_dth       :: !Double
  , is_dqh       :: !Double
  , is_ur        :: !Double
  , is_zldis     :: !Double
    -- vpd
  , is_rh_af     :: !Double
  , is_vpd       :: !Double
  } deriving (Show)

-- =========================================================================
-- Single iteration step
-- =========================================================================

-- | Perform one iteration of the canopy flux Newton-Raphson solver.
-- Pure function: takes current state and inputs, returns updated state.
canopyFluxesIteration :: CanopyFluxesParams
                      -> CanopyFluxesControl
                      -> CanopyFluxesInput
                      -> Double  -- ^ tl_ini (initial leaf T for heat storage)
                      -> Double  -- ^ ts_ini (initial stem T for heat storage)
                      -> Double  -- ^ air (longwave coefficient a)
                      -> Double  -- ^ bir (longwave coefficient b)
                      -> Double  -- ^ cir (longwave coefficient c)
                      -> Double  -- ^ sa_leaf (leaf surface area for SH)
                      -> Double  -- ^ sa_stem (stem surface area)
                      -> Double  -- ^ sa_internal (internal LW area)
                      -> Double  -- ^ cp_leaf (leaf heat capacity)
                      -> Double  -- ^ cp_stem (stem heat capacity)
                      -> Double  -- ^ rstem (stem resistance)
                      -> Double  -- ^ frac_rad_abs_by_stem
                      -> IterState
                      -> IterState
canopyFluxesIteration params ctrl inp tl_ini _ts_ini
                      air_coef bir_coef cir_coef
                      sa_leaf sa_stem sa_internal
                      cp_leaf _cp_stem rstem
                      frac_rad_abs_by_stem
                      st =
  let -- Unpack frequently used inputs
      forc_rho   = cfi_forc_rho inp
      forc_q     = cfi_forc_q inp
      forc_pbot  = cfi_forc_pbot inp
      forc_th    = cfi_forc_th inp
      t_grnd     = cfi_t_grnd inp
      thm        = cfi_thm inp
      thv        = cfi_thv inp
      elai       = cfi_elai inp
      esai       = cfi_esai inp
      qg         = cfi_qg inp
      dtime      = cfi_dtime inp
      displa     = cfi_displa inp
      z0mv       = cfi_z0mv inp
      forc_hgt_u = cfi_forc_hgt_u inp
      forc_hgt_t = cfi_forc_hgt_t inp
      forc_hgt_q = cfi_forc_hgt_q inp

      -- ---------------------------------------------------------------
      -- Step 1: Friction velocity and profile functions
      -- ---------------------------------------------------------------
      z0hv  = z0mv   -- z0h = z0m for vegetation (CLM convention)
      z0qv  = z0mv
      -- forc_hgt_{u,t,q} arrive already adjusted by (z0 + displa) from the
      -- canopyFluxes initialiser (forc_hgt_u' = raw + z0mv + displa, matching
      -- Julia's forc_hgt_u_patch). Re-adding here double-counted z0mv+displa,
      -- inflating zldis (43.26 vs Julia 30.94) -> log(zldis/z0m) too large ->
      -- ustar too low -> the whole cold-canopy/weak-exchange basin.
      fhgt_u = forc_hgt_u
      fhgt_t = forc_hgt_t
      fhgt_q = forc_hgt_q

      (ustar_new, temp1_new, temp2_new, temp12m_new, temp22m_new, fm_new)
        = frictionVelocityProfile displa z0mv z0hv z0qv
            (is_obu st) (is_um st)
            fhgt_u fhgt_t fhgt_q

      -- ---------------------------------------------------------------
      -- Step 2: Aerodynamic resistances
      -- ---------------------------------------------------------------
      ram1_new = 1.0 / (ustar_new * ustar_new / is_um st)
      rah_above = 1.0 / (temp1_new * ustar_new)
      raw_above = 1.0 / (temp2_new * ustar_new)

      -- Bulk boundary layer resistance
      uaf = is_um st * sqrt (1.0 / (ram1_new * is_um st))

      -- Leaf boundary layer resistance
      dleaf = cfi_dleaf inp
      cf = cfp_cv params / (sqrt uaf * sqrt dleaf)
      rb_new = 1.0 / (cf * uaf)

      -- Soil drag coefficient (X. Zeng parameterization)
      z0mg = cfi_z0mg inp
      w = exp (-(elai + esai))
      csoilb = vkc / (cfp_a_coef params *
                       (z0mg * uaf / nu_param) ** cfp_a_exp params)

      -- Stability parameter for ricsoilc
      ri = (grav * cfi_htop inp * (is_taf st - t_grnd)) /
           (is_taf st * uaf * uaf)

      csoilcn | cfc_use_undercanopy_stability ctrl && (is_taf st - t_grnd) > 0.0
              = let ricsoilc = cfp_csoilc params /
                               (1.0 + ria_canopy * min ri 10.0)
                in  csoilb * w + ricsoilc * (1.0 - w)
              | otherwise
              = csoilb * w + cfp_csoilc params * (1.0 - w)

      rah_below = 1.0 / (csoilcn * uaf)
      raw_below = rah_below

      -- ---------------------------------------------------------------
      -- Step 3: Stomatal resistance intermediates
      -- ---------------------------------------------------------------
      svpts = is_el st
      eah   = forc_pbot * is_qaf st / 0.622
      rh_af = eah / svpts

      -- ---------------------------------------------------------------
      -- Step 4: Heat transfer conductances
      -- ---------------------------------------------------------------
      t_veg_cur  = is_t_veg st
      t_stem_cur = is_t_stem st

      wta  = 1.0 / rah_above             -- air
      wtl  = sa_leaf / rb_new             -- leaf
      wtg  = 1.0 / rah_below             -- ground
      wtstem = sa_stem / (rstem + rb_new) -- stem

      wtshi  = 1.0 / (wta + wtl + wtstem + wtg)
      wtl0   = wtl * wtshi
      wtg0   = wtg * wtshi
      wta0   = wta * wtshi
      wtstem0 = wtstem * wtshi
      wtga   = wta0 + wtg0 + wtstem0
      wtal   = wta0 + wtl0 + wtstem0

      -- Internal longwave between leaf and stem
      lw_stem = sa_internal * cfi_emv inp * sb * t_stem_cur ** 4
      lw_leaf = sa_internal * cfi_emv inp * sb * t_veg_cur ** 4

      -- ---------------------------------------------------------------
      -- Step 5: Fraction of potential evaporation from leaf
      -- ---------------------------------------------------------------
      fdry    = cfi_fdry inp
      laisun  = cfi_laisun inp
      laisha  = cfi_laisha inp
      rssun   = cfi_rssun inp
      rssha   = cfi_rssha inp
      btran   = cfi_btran inp

      rppdry | fdry > 0.0 =
                fdry * rb_new *
                  (laisun / (rb_new + rssun) + laisha / (rb_new + rssha)) / elai
             | otherwise   = 0.0

      efpot_q = forc_rho * ((elai + esai) / rb_new) *
                  (is_qsatl st - is_qaf st)
      h2ocan  = cfi_liqcan inp + cfi_snocan inp

      -- Transpiration and rpp (fraction of potential evaporation)
      (rpp, qflx_tran_veg_tmp)
        | efpot_q > 0.0 =
            if btran > btran0
            then let tv = efpot_q * rppdry
                     rp = min (rppdry + cfi_fwet inp)
                              ((tv + h2ocan / dtime) / efpot_q)
                 in  (rp, tv)
            else let rp = min (cfi_fwet inp)
                              (h2ocan / dtime / efpot_q)
                 in  (rp, 0.0)
        | otherwise = (1.0, 0.0)

      -- ---------------------------------------------------------------
      -- Step 6: Latent heat conductances
      -- ---------------------------------------------------------------
      fvn    = fromIntegral (cfi_frac_veg_nosno inp)
      wtaq   = fvn / raw_above
      wtlq   = fvn * (elai + esai) / rb_new * rpp

      -- Litter layer resistance (Sakaguchi)
      snow_depth_c = cfp_z_dl params
      fsno_dl = cfi_snow_depth inp / snow_depth_c
      elai_dl = cfp_lai_dl params * (1.0 - min fsno_dl 1.0)
      rdl     = (1.0 - exp (-elai_dl)) / (0.004 * uaf)

      delq_cur = qg - is_qaf st

      wtgq_val
        | delq_cur < 0.0 = fvn / (raw_below + rdl)
        | cfi_do_soilevap_beta inp
          = cfi_soilbeta inp * fvn / (raw_below + rdl)
        | otherwise  -- soil resistance method
          = fvn / (raw_below + cfi_soilresis inp)

      wtsqi  = 1.0 / (wtaq + wtlq + wtgq_val)
      wtgq0  = wtgq_val * wtsqi
      wtlq0  = wtlq * wtsqi
      wtaq0  = wtaq * wtsqi
      wtgaq  = wtaq0 + wtgq0
      wtalq  = wtaq0 + wtlq0

      dc1 = forc_rho * cpair * wtl
      dc2 = hvap * forc_rho * wtlq

      -- ---------------------------------------------------------------
      -- Step 7: Current sensible and latent heat fluxes
      -- ---------------------------------------------------------------
      efsh = dc1 * (wtga * t_veg_cur - wtg0 * t_grnd -
                     wta0 * thm - wtstem0 * t_stem_cur)

      eflx_sh_stem_cur = forc_rho * cpair * wtstem *
        ((wta0 + wtg0 + wtl0) * t_stem_cur -
          wtg0 * t_grnd - wta0 * thm - wtl0 * t_veg_cur)

      efe_raw = dc2 * (wtgaq * is_qsatl st -
                        wtgq0 * qg - wtaq0 * forc_q)

      -- Evaporation flux sign change limiter
      (efe, erre) =
        if efe_raw * is_efeb st < 0.0
        then let e' = 0.1 * efe_raw in (e', e' - efe_raw)
        else (efe_raw, 0.0)

      -- Fractionate ground emitted longwave
      lw_grnd = cfi_frac_sno_eff inp * cfi_t_soisno_top inp ** 4 +
                (1.0 - cfi_frac_sno_eff inp - cfi_frac_h2osfc inp) *
                  cfi_t_soisno_topsoil inp ** 4 +
                cfi_frac_h2osfc inp * cfi_t_h2osfc inp ** 4

      -- ---------------------------------------------------------------
      -- Step 8: Newton-Raphson dt_veg
      -- ---------------------------------------------------------------
      tlbef = t_veg_cur

      numer = (1.0 - frac_rad_abs_by_stem) *
                (cfi_sabv inp + air_coef +
                 bir_coef * t_veg_cur ** 4 + cir_coef * lw_grnd) -
              efsh - efe - lw_leaf + lw_stem -
              (cp_leaf / dtime) * (t_veg_cur - tl_ini)

      denom = (1.0 - frac_rad_abs_by_stem) *
                (-4.0 * bir_coef * t_veg_cur ** 3) +
              4.0 * sa_internal * cfi_emv inp * sb * t_veg_cur ** 3 +
              dc1 * wtga + dc2 * wtgaq * is_qsatldT st +
              cp_leaf / dtime

      dt_veg_raw = numer / denom
      del_raw    = abs dt_veg_raw

      -- Clamp dt_veg to DELMAX_CANOPY
      (dt_veg_clamped, err_clamped)
        | del_raw > delmax_canopy =
            let dtv = delmax_canopy * dt_veg_raw / del_raw
                t_new = tlbef + dtv
                e = (1.0 - frac_rad_abs_by_stem) *
                      (cfi_sabv inp + air_coef +
                       bir_coef * tlbef ** 3 * (tlbef + 4.0 * dtv) +
                       cir_coef * lw_grnd) -
                    sa_internal * cfi_emv inp * sb *
                      tlbef ** 3 * (tlbef + 4.0 * dtv) +
                    lw_stem -
                    (efsh + dc1 * wtga * dtv) -
                    (efe + dc2 * wtgaq * is_qsatldT st * dtv) -
                    (cp_leaf / dtime) * (t_new - tl_ini)
            in  (dtv, e)
        | otherwise = (dt_veg_raw, 0.0)

      t_veg_new = tlbef + dt_veg_clamped

      -- ---------------------------------------------------------------
      -- Step 9: Updated fluxes
      -- ---------------------------------------------------------------
      efpot_new = forc_rho * ((elai + esai) / rb_new) *
                    (wtgaq * (is_qsatl st + is_qsatldT st * dt_veg_clamped) -
                     wtgq0 * qg - wtaq0 * forc_q)
      qflx_evap_veg_raw = rpp * efpot_new

      -- Interception losses / ecidif
      (qflx_evap_veg_new, qflx_tran_veg_new, ecidif)
        | efpot_new > 0.0 && btran > btran0 =
            let tv = efpot_new * rppdry
                eci = max 0.0 (qflx_evap_veg_raw - tv - h2ocan / dtime)
                qev = min qflx_evap_veg_raw (tv + h2ocan / dtime)
            in  (qev, tv, eci)
        | otherwise =
            let eci = max 0.0 (qflx_evap_veg_raw - h2ocan / dtime)
                qev = min qflx_evap_veg_raw (h2ocan / dtime)
            in  (qev, 0.0, eci)

      -- Sensible heat from leaves
      eflx_sh_veg_new = efsh + dc1 * wtga * dt_veg_clamped +
                         err_clamped + erre + hvap * ecidif

      -- Update SH stem for changes in t_veg
      eflx_sh_stem_new = eflx_sh_stem_cur +
                          forc_rho * cpair * wtstem * (-wtl0 * dt_veg_clamped)

      -- Re-calculate QSat at updated leaf temperature
      (qsatl_new, el_new, qsatldT_new, _) = qsat t_veg_new forc_pbot

      -- Update canopy air temperature and humidity
      taf_new = wtg0 * t_grnd + wta0 * thm +
                wtl0 * t_veg_new + wtstem0 * t_stem_cur
      qaf_new = wtlq0 * qsatl_new + wtgq0 * qg + forc_q * wtaq0

      -- ---------------------------------------------------------------
      -- Step 10: Update Monin-Obukhov length
      -- ---------------------------------------------------------------
      dth_new  = thm - taf_new
      dqh_new  = forc_q - qaf_new
      delq_new = wtalq * qg - wtlq0 * qsatl_new - wtaq0 * forc_q

      tstar   = temp1_new * dth_new
      qstar   = temp2_new * dqh_new
      thvstar = tstar * (1.0 + 0.61 * forc_q) +
                0.61 * forc_th * qstar

      zldis = fhgt_u - displa
      zeta_raw = zldis * vkc * grav * thvstar /
                   (ustar_new ** 2 * thv)

      (zeta_new, um_new)
        | zeta_raw >= 0.0 =
            let z = min (cfi_zetamaxstable inp) (max zeta_raw 0.01)
                u = max (is_ur st) 0.1
            in  (z, u)
        | otherwise =
            let z = max (-100.0) (min zeta_raw (-0.01))
                wc | ustar_new * thvstar > 0.0 = 0.0
                   | otherwise =
                       let wc_arg = max (negate grav * ustar_new * thvstar *
                                         zii_canopy / thv) 0.0
                       in  beta_canopy * wc_arg ** (1.0/3.0 :: Double)
                u = sqrt (is_ur st ** 2 + wc ** 2)
            in  (z, u)

      obu_new_raw = zldis / zeta_new

      nmozsgn_new = if is_obuold st * obu_new_raw < 0.0
                    then is_nmozsgn st + 1
                    else is_nmozsgn st

      obu_new | nmozsgn_new >= 4 = zldis / (-0.01)
              | otherwise        = obu_new_raw

  in st { is_t_veg        = t_veg_new
        , is_obu          = obu_new
        , is_um           = um_new
        , is_ustar        = ustar_new
        , is_taf          = taf_new
        , is_qaf          = qaf_new
        , is_del          = abs dt_veg_clamped
        , is_efeb         = efe
        , is_nmozsgn      = nmozsgn_new
        , is_obuold       = obu_new
        , is_qsatl        = qsatl_new
        , is_el           = el_new
        , is_qsatldT      = qsatldT_new
        , is_eflx_sh_veg  = eflx_sh_veg_new
        , is_eflx_sh_stem = eflx_sh_stem_new
        , is_qflx_evap_veg = qflx_evap_veg_new
        , is_qflx_tran_veg = qflx_tran_veg_new
        , is_err          = err_clamped
        , is_wtg          = wtg
        , is_wtgq         = wtgq_val
        , is_wta0         = wta0
        , is_wtl0         = wtl0
        , is_wtstem0      = wtstem0
        , is_wtga         = wtga
        , is_wtal         = wtal
        , is_wtaq0        = wtaq0
        , is_wtlq0        = wtlq0
        , is_wtalq        = wtalq
        , is_rb           = rb_new
        , is_ram1         = ram1_new
        , is_dt_veg       = dt_veg_clamped
        , is_tlbef        = tlbef
        , is_delq         = delq_new
        , is_temp1        = temp1_new
        , is_temp2        = temp2_new
        , is_temp12m      = temp12m_new
        , is_temp22m      = temp22m_new
        , is_fm           = fm_new
        , is_rah_above    = rah_above
        , is_raw_above    = raw_above
        , is_rah_below    = rah_below
        , is_raw_below    = raw_below
        , is_itlef        = is_itlef st + 1
        , is_converged    = False  -- set below in loop driver
        , is_lw_grnd      = lw_grnd
        , is_dth          = dth_new
        , is_dqh          = dqh_new
        , is_ur           = is_ur st
        , is_zldis        = zldis
        , is_rh_af        = rh_af
        , is_vpd          = max (svpts - eah) 50.0 * 0.001
        }

-- =========================================================================
-- Iteration loop driver
-- =========================================================================

-- | Run the stability iteration loop until convergence or max iterations.
-- Pure recursive function.
iterateCanopyFluxes :: CanopyFluxesParams
                    -> CanopyFluxesControl
                    -> CanopyFluxesInput
                    -> Double -> Double  -- ^ tl_ini, ts_ini
                    -> Double -> Double -> Double  -- ^ air, bir, cir
                    -> Double -> Double -> Double  -- ^ sa_leaf, sa_stem, sa_internal
                    -> Double -> Double -> Double  -- ^ cp_leaf, cp_stem, rstem
                    -> Double  -- ^ frac_rad_abs_by_stem
                    -> IterState
                    -> IterState
iterateCanopyFluxes params ctrl inp tl_ini ts_ini
                    air_coef bir_coef cir_coef
                    sa_leaf sa_stem sa_internal
                    cp_leaf cp_stem rstem
                    frac_rad_abs_by_stem
                    st
  | is_itlef st > cfc_itmax_canopy_fluxes ctrl = st
  | is_converged st && is_itlef st > itmin_canopy = st
  | otherwise =
      let st' = canopyFluxesIteration params ctrl inp tl_ini ts_ini
                  air_coef bir_coef cir_coef
                  sa_leaf sa_stem sa_internal
                  cp_leaf cp_stem rstem
                  frac_rad_abs_by_stem st

          -- Test convergence after minimum iterations
          conv | is_itlef st' > itmin_canopy =
                   let dele = abs (is_efeb st' - is_efeb st)
                       det  = max (is_del st') (is_del st)
                   in  det < dtmin_canopy && dele < dlemin_canopy
               | otherwise = False

          st'' = st' { is_converged = conv }

      in iterateCanopyFluxes params ctrl inp tl_ini ts_ini
           air_coef bir_coef cir_coef
           sa_leaf sa_stem sa_internal
           cp_leaf cp_stem rstem
           frac_rad_abs_by_stem st''

-- =========================================================================
-- Main entry point
-- =========================================================================

-- | Compute canopy fluxes for a single patch.
--
-- This is the main entry point, corresponding to @subroutine CanopyFluxes@
-- in Fortran / @canopy_fluxes!@ in Julia. It operates on a single patch
-- (pure function), returning the complete output record.
--
-- The caller is responsible for iterating over all patches.
canopyFluxes :: CanopyFluxesParams
             -> CanopyFluxesControl
             -> CanopyFluxesInput
             -> CanopyFluxesOutput
canopyFluxes params ctrl inp =
  let -- ================================================================
      -- Phase 1: Initialization
      -- ================================================================
      emv = cfi_emv inp
      emg = cfi_emg inp

      -- Net absorbed longwave = air + bir*t_veg^4 + cir*t_grnd^4
      air_coef =  emv * (1.0 + (1.0 - emv) * (1.0 - emg)) * cfi_forc_lwrad inp
      bir_coef = -(2.0 - emv * (1.0 - emg)) * emv * sb
      cir_coef =  emv * emg * sb

      -- Saturated vapor pressure at initial leaf temperature
      (qsatl0, el0, qsatldT0, _) = qsat (cfi_t_veg inp) (cfi_forc_pbot inp)

      -- Wind speed
      ur = max (cfp_wind_min params)
               (sqrt (cfi_forc_u inp ** 2 + cfi_forc_v inp ** 2))

      -- Canopy air initialization
      taf0 = (cfi_t_grnd inp + cfi_thm inp) / 2.0
      qaf0 = (cfi_forc_q inp + cfi_qg inp) / 2.0

      dth0  = cfi_thm inp - taf0
      dqh0  = cfi_forc_q inp - qaf0
      dthv0 = dth0 * (1.0 + 0.61 * cfi_forc_q inp) +
              0.61 * cfi_forc_th inp * dqh0

      -- Zeng-Wang roughness length modification
      lt    = min (cfi_elai inp + cfi_esai inp) tlsai_crit
      egvf  = (1.0 - alpha_aero * exp (-lt)) /
              (1.0 - alpha_aero * exp (-tlsai_crit))
      displa' = egvf * cfi_displa inp
      z0mv'   = exp (egvf * log (cfi_z0mv inp) +
                     (1.0 - egvf) * log (cfi_z0mg inp))

      forc_hgt_u' = cfi_forc_hgt_u inp + z0mv' + displa'
      forc_hgt_t' = cfi_forc_hgt_t inp + z0mv' + displa'
      forc_hgt_q' = cfi_forc_hgt_q inp + z0mv' + displa'

      zldis0 = forc_hgt_u' - displa'

      -- Monin-Obukhov initialization
      (um0, obu0) = moninObukIni (cfi_zetamaxstable inp) ur
                                  (cfi_thv inp) dthv0 zldis0 z0mv'

      -- No biomass heat storage: simplified surface areas
      sa_leaf     = cfi_elai inp + cfi_esai inp
      sa_stem     = 0.0
      sa_internal = 0.0
      cp_leaf     = 0.0
      cp_stem     = 0.0
      rstem_val   = 0.0
      frac_rad_abs_by_stem = 0.0

      tl_ini = cfi_t_veg inp
      ts_ini = cfi_t_stem inp

      -- Build modified input with adjusted roughness
      inp' = inp { cfi_displa    = displa'
                 , cfi_z0mv      = z0mv'
                 , cfi_forc_hgt_u = forc_hgt_u'
                 , cfi_forc_hgt_t = forc_hgt_t'
                 , cfi_forc_hgt_q = forc_hgt_q'
                 }

      -- Initial iteration state
      st0 = IterState
        { is_t_veg        = cfi_t_veg inp
        , is_t_stem       = cfi_t_stem inp
        , is_obu          = obu0
        , is_um           = um0
        , is_ustar        = 0.0
        , is_taf          = taf0
        , is_qaf          = qaf0
        , is_del          = 0.0
        , is_efeb         = 0.0
        , is_nmozsgn      = 0
        , is_obuold       = 0.0
        , is_qsatl        = qsatl0
        , is_el           = el0
        , is_qsatldT      = qsatldT0
        , is_eflx_sh_veg  = 0.0
        , is_eflx_sh_stem = 0.0
        , is_qflx_evap_veg = 0.0
        , is_qflx_tran_veg = 0.0
        , is_err          = 0.0
        , is_wtg          = 0.0
        , is_wtgq         = 0.0
        , is_wta0         = 0.0
        , is_wtl0         = 0.0
        , is_wtstem0      = 0.0
        , is_wtga         = 0.0
        , is_wtal         = 0.0
        , is_wtaq0        = 0.0
        , is_wtlq0        = 0.0
        , is_wtalq        = 0.0
        , is_rb           = 0.0
        , is_ram1         = 0.0
        , is_dt_veg       = 0.0
        , is_tlbef        = cfi_t_veg inp
        , is_delq         = 0.0
        , is_temp1        = 0.0
        , is_temp2        = 0.0
        , is_temp12m      = 0.0
        , is_temp22m      = 0.0
        , is_fm           = 0.0
        , is_rah_above    = 0.0
        , is_raw_above    = 0.0
        , is_rah_below    = 0.0
        , is_raw_below    = 0.0
        , is_itlef        = 0
        , is_converged    = False
        , is_lw_grnd      = 0.0
        , is_dth          = dth0
        , is_dqh          = dqh0
        , is_ur           = ur
        , is_zldis        = zldis0
        , is_rh_af        = 0.0
        , is_vpd          = 0.0
        }

      -- ================================================================
      -- Phase 2: Stability iteration (Newton-Raphson)
      -- ================================================================
      stF = iterateCanopyFluxes params ctrl inp' tl_ini ts_ini
              air_coef bir_coef cir_coef
              sa_leaf sa_stem sa_internal
              cp_leaf cp_stem rstem_val
              frac_rad_abs_by_stem st0

      -- ================================================================
      -- Phase 3: Post-iteration diagnostics
      -- ================================================================
      forc_rho = cfi_forc_rho inp
      t_grnd   = cfi_t_grnd inp
      thm      = cfi_thm inp
      forc_q   = cfi_forc_q inp
      qg       = cfi_qg inp
      dtime    = cfi_dtime inp
      forc_pbot = cfi_forc_pbot inp

      -- Energy balance check
      lw_grnd_final = is_lw_grnd stF

      err_final = (1.0 - frac_rad_abs_by_stem) *
                    (cfi_sabv inp + air_coef +
                     bir_coef * is_tlbef stF ** 3 *
                       (is_tlbef stF + 4.0 * is_dt_veg stF) +
                     cir_coef * lw_grnd_final) -
                  sa_internal * cfi_emv inp * sb *
                    is_tlbef stF ** 3 * (is_tlbef stF + 4.0 * is_dt_veg stF) +
                  0.0 {- lw_stem for sa_stem=0 -} -
                  is_eflx_sh_veg stF -
                  hvap * is_qflx_evap_veg stF -
                  ((is_t_veg stF - tl_ini) * cp_leaf / dtime)

      -- Ground fluxes
      delt_val = is_wtal stF * t_grnd - is_wtl0 stF * is_t_veg stF -
                 is_wta0 stF * thm - is_wtstem0 stF * is_t_stem stF

      eflx_sh_grnd = cpair * forc_rho * is_wtg stF * delt_val

      -- Wind stress
      taux = negate forc_rho * cfi_forc_u inp / is_ram1 stF
      tauy = negate forc_rho * cfi_forc_v inp / is_ram1 stF

      -- Individual sensible heat by surface type
      delt_snow = is_wtal stF * cfi_t_soisno_top inp -
                  is_wtl0 stF * is_t_veg stF -
                  is_wta0 stF * thm - is_wtstem0 stF * is_t_stem stF
      delt_soil = is_wtal stF * cfi_t_soisno_topsoil inp -
                  is_wtl0 stF * is_t_veg stF -
                  is_wta0 stF * thm - is_wtstem0 stF * is_t_stem stF
      delt_h2osfc = is_wtal stF * cfi_t_h2osfc inp -
                    is_wtl0 stF * is_t_veg stF -
                    is_wta0 stF * thm - is_wtstem0 stF * is_t_stem stF

      eflx_sh_snow   = cpair * forc_rho * is_wtg stF * delt_snow
      eflx_sh_soil   = cpair * forc_rho * is_wtg stF * delt_soil
      eflx_sh_h2osfc = cpair * forc_rho * is_wtg stF * delt_h2osfc

      -- Ground evaporation
      qflx_evap_soi = forc_rho * is_wtgq stF * is_delq stF

      -- Individual latent heat by surface type
      delq_snow  = is_wtalq stF * cfi_qg_snow inp -
                   is_wtlq0 stF * is_qsatl stF - is_wtaq0 stF * forc_q
      delq_soil  = is_wtalq stF * cfi_qg_soil inp -
                   is_wtlq0 stF * is_qsatl stF - is_wtaq0 stF * forc_q
      delq_h2osfc = is_wtalq stF * cfi_qg_h2osfc inp -
                    is_wtlq0 stF * is_qsatl stF - is_wtaq0 stF * forc_q

      qflx_ev_snow  = forc_rho * is_wtgq stF * delq_snow
      qflx_ev_soil  = forc_rho * is_wtgq stF * delq_soil
      qflx_ev_h2osfc = forc_rho * is_wtgq stF * delq_h2osfc

      -- 2-m reference temperature
      t_ref2m = thm + is_temp1 stF * is_dth stF *
                  (1.0 / is_temp12m stF - 1.0 / is_temp1 stF)

      -- 2-m specific humidity
      q_ref2m = forc_q + is_temp2 stF * is_dqh stF *
                  (1.0 / is_temp22m stF - 1.0 / is_temp2 stF)

      -- 2-m relative humidity
      (qsat_ref2m, _e_ref2m, _, _) = qsat t_ref2m forc_pbot
      rh_ref2m = min 100.0 (q_ref2m / qsat_ref2m * 100.0)

      -- Downward longwave below canopy
      dlrad = (1.0 - emv) * emg * cfi_forc_lwrad inp +
              emv * emg * sb * is_tlbef stF ** 3 *
                (is_tlbef stF + 4.0 * is_dt_veg stF)

      -- Upward longwave above canopy
      ulrad = (1.0 - emg) * (1.0 - emv) * (1.0 - emv) * cfi_forc_lwrad inp +
              emv * (1.0 + (1.0 - emg) * (1.0 - emv)) * sb *
                is_tlbef stF ** 3 * (is_tlbef stF + 4.0 * is_dt_veg stF) +
              emg * (1.0 - emv) * sb * lw_grnd_final

      -- Ground heat flux coupling derivatives
      cgrnds_new = cfi_cgrnds inp +
                   cpair * forc_rho * is_wtg stF * is_wtal stF
      cgrndl_new = cfi_cgrndl inp +
                   forc_rho * is_wtgq stF * is_wtalq stF * cfi_dqgdT inp
      cgrnd_new  = cgrnds_new + cgrndl_new * cfi_htvp inp

      -- Update canopy water
      t_veg_f       = is_t_veg stF
      qflx_evap_veg = is_qflx_evap_veg stF
      qflx_tran_veg = is_qflx_tran_veg stF
      liqcan_in     = cfi_liqcan inp
      snocan_in     = cfi_snocan inp

      (liqcan_out, snocan_out)
        | t_veg_f > tfrz =
            let excess = (qflx_evap_veg - qflx_tran_veg) * dtime > liqcan_in
                sn = if excess
                     then max 0.0 (snocan_in + liqcan_in +
                                   (qflx_tran_veg - qflx_evap_veg) * dtime)
                     else snocan_in
                lq = max 0.0 (liqcan_in +
                              (qflx_tran_veg - qflx_evap_veg) * dtime)
            in  (lq, sn)
        | otherwise =
            let excess = (qflx_evap_veg - qflx_tran_veg) * dtime > snocan_in
                lq = if excess
                     then liqcan_in + snocan_in +
                          (qflx_tran_veg - qflx_evap_veg) * dtime
                     else liqcan_in
                sn = max 0.0 (snocan_in +
                              (qflx_tran_veg - qflx_evap_veg) * dtime)
            in  (lq, sn)

  in CanopyFluxesOutput
       { cfo_eflx_sh_veg    = is_eflx_sh_veg stF
       , cfo_eflx_sh_grnd   = eflx_sh_grnd
       , cfo_eflx_sh_snow   = eflx_sh_snow
       , cfo_eflx_sh_soil   = eflx_sh_soil
       , cfo_eflx_sh_h2osfc = eflx_sh_h2osfc
       , cfo_eflx_sh_stem   = is_eflx_sh_stem stF
       , cfo_taux           = taux
       , cfo_tauy           = tauy
       , cfo_qflx_evap_veg  = qflx_evap_veg
       , cfo_qflx_tran_veg  = qflx_tran_veg
       , cfo_qflx_evap_soi  = qflx_evap_soi
       , cfo_qflx_ev_snow   = qflx_ev_snow
       , cfo_qflx_ev_soil   = qflx_ev_soil
       , cfo_qflx_ev_h2osfc = qflx_ev_h2osfc
       , cfo_t_veg          = is_t_veg stF
       , cfo_t_ref2m        = t_ref2m
       , cfo_t_stem         = is_t_stem stF
       , cfo_q_ref2m        = q_ref2m
       , cfo_rh_ref2m       = rh_ref2m
       , cfo_taf            = is_taf stF
       , cfo_qaf            = is_qaf stF
       , cfo_ustar          = is_ustar stF
       , cfo_ram1           = is_ram1 stF
       , cfo_rb1            = is_rb stF
       , cfo_obu            = is_obu stF
       , cfo_zeta           = is_zldis stF / is_obu stF
       , cfo_um             = is_um stF
       , cfo_num_iter       = is_itlef stF
       , cfo_rah_above      = is_rah_above stF
       , cfo_raw_above      = is_raw_above stF
       , cfo_rah_below      = is_rah_below stF
       , cfo_raw_below      = is_raw_below stF
       , cfo_vpd            = is_vpd stF
       , cfo_dlrad          = dlrad
       , cfo_ulrad          = ulrad
       , cfo_cgrnds         = cgrnds_new
       , cfo_cgrndl         = cgrndl_new
       , cfo_cgrnd          = cgrnd_new
       , cfo_liqcan         = liqcan_out
       , cfo_snocan         = snocan_out
       , cfo_err            = err_final
       , cfo_rh_af          = is_rh_af stF
       }
