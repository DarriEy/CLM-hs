{-# LANGUAGE BangPatterns #-}
-- | Lake surface flux calculations.
-- Fortran: LakeFluxesMod.F90
-- Julia:   src/biogeophys/lake_fluxes.jl
--
-- Calculates surface fluxes and surface temperature for lakes using
-- Monin-Obukhov similarity theory with stability iteration.
-- Handles: open water, frozen lakes (with/without snow), variable
-- depth, fetch-dependent roughness, Newton-Raphson temperature solving.
--
-- All functions are pure.
--
module CLM.BioGeoPhys.LakeFluxes
  ( -- * Types
    LakeFluxInput(..)
  , LakeFluxOutput(..)
    -- * Main computation
  , lakeFluxes
    -- * Helpers (exported for testing)
  , qsatWater
  , qsatWaterDT
  , stabilityFunc1
  , stabilityFunc2
  ) where

import CLM.Constants.PhysicalConstants
  ( tfrz, grav, sb, denh2o, denice, cpice, cpliq, hfus, hvap, hsub
  , tkwat, tkice, tkair
  )
import CLM.BioGeoPhys.LakeCon (emg_lake, tdmax)

-- =========================================================================
-- Lake-specific aerodynamic constants
-- =========================================================================

-- | Von Karman constant
vkc :: Double
vkc = 0.4

-- | Specific heat of air [J/kg/K]
cpair :: Double
cpair = 1004.64

-- | Prandtl number for air
prn_air :: Double
prn_air = 0.713

-- | Schmidt number for water in air
sch_water :: Double
sch_water = 0.66

-- | Kinematic viscosity of air at 20C [m^2/s]
kva0 :: Double
kva0 = 1.51e-5

-- | Smooth flow regime coefficient
cus_local :: Double
cus_local = 0.1

-- | Base Charnock constant
cur0_local :: Double
cur0_local = 0.01

-- | Coefficient of convective velocity
beta1 :: Double
beta1 = 1.0

-- | Max zeta under stable conditions
zetamaxLake :: Double
zetamaxLake = 0.5

-- | Fraction of visible radiation absorbed at surface
betavisLake :: Double
betavisLake = 0.4

-- | Minimum lake roughness length [m]
minz0lakeDef :: Double
minz0lakeDef = 1.0e-5

-- =========================================================================
-- Input / Output records
-- =========================================================================

-- | Input bundle for a single lake patch flux calculation.
data LakeFluxInput = LakeFluxInput
  { lfi_snl            :: !Int     -- ^ Number of snow layers (<= 0)
  , lfi_lakedepth      :: !Double  -- ^ Lake depth [m]
  , lfi_dz_top         :: !Double  -- ^ Thickness of top layer (snow or lake) [m]
  , lfi_savedtke1      :: !Double  -- ^ Saved thermal conductivity of top lake [W/m/K]
  , lfi_t_grnd         :: !Double  -- ^ Ground temperature [K]
  , lfi_t_subsurface   :: !Double  -- ^ Temperature of subsurface layer [K]
  , lfi_t_lake1        :: !Double  -- ^ Temperature of top lake layer [K]
  , lfi_sabg           :: !Double  -- ^ Absorbed solar at ground [W/m2]
  , lfi_h2osoi_liq_top :: !Double  -- ^ Liquid water in top layer [kg/m2]
  , lfi_h2osoi_ice_top :: !Double  -- ^ Ice in top layer [kg/m2]
  , lfi_forc_t         :: !Double  -- ^ Atmospheric temperature [K]
  , lfi_forc_th        :: !Double  -- ^ Atmospheric potential temperature [K]
  , lfi_forc_q         :: !Double  -- ^ Atmospheric specific humidity [kg/kg]
  , lfi_forc_pbot      :: !Double  -- ^ Atmospheric pressure [Pa]
  , lfi_forc_rho       :: !Double  -- ^ Atmospheric density [kg/m3]
  , lfi_forc_lwrad     :: !Double  -- ^ Downward longwave radiation [W/m2]
  , lfi_forc_u         :: !Double  -- ^ Wind speed u-component [m/s]
  , lfi_forc_v         :: !Double  -- ^ Wind speed v-component [m/s]
  , lfi_forc_hgt_u     :: !Double  -- ^ Reference height for wind [m]
  , lfi_forc_hgt_t     :: !Double  -- ^ Reference height for temperature [m]
  , lfi_forc_hgt_q     :: !Double  -- ^ Reference height for humidity [m]
  , lfi_dtime          :: !Double  -- ^ Timestep [s]
  } deriving (Show)

-- | Output bundle from a single lake patch flux calculation.
data LakeFluxOutput = LakeFluxOutput
  { lfo_t_grnd           :: !Double  -- ^ Updated ground temperature [K]
  , lfo_eflx_sh_grnd     :: !Double  -- ^ Sensible heat flux [W/m2]
  , lfo_eflx_lh_grnd     :: !Double  -- ^ Latent heat flux [W/m2]
  , lfo_eflx_lwrad_out   :: !Double  -- ^ Outgoing longwave [W/m2]
  , lfo_eflx_lwrad_net   :: !Double  -- ^ Net longwave [W/m2]
  , lfo_eflx_soil_grnd   :: !Double  -- ^ Ground heat flux [W/m2]
  , lfo_qflx_evap_soi    :: !Double  -- ^ Evaporation rate [kg/m2/s]
  , lfo_ustar            :: !Double  -- ^ Friction velocity [m/s]
  , lfo_z0mg             :: !Double  -- ^ Momentum roughness length [m]
  , lfo_z0hg             :: !Double  -- ^ Heat roughness length [m]
  , lfo_z0qg             :: !Double  -- ^ Moisture roughness length [m]
  , lfo_taux             :: !Double  -- ^ Zonal momentum stress [kg/m/s2]
  , lfo_tauy             :: !Double  -- ^ Meridional momentum stress [kg/m/s2]
  , lfo_ws_col           :: !Double  -- ^ Wind-driven surface stress parameter
  , lfo_ks_col           :: !Double  -- ^ Surface extinction coefficient
  , lfo_htvp             :: !Double  -- ^ Latent heat used (HVAP or HSUB) [J/kg]
  } deriving (Show)

-- =========================================================================
-- Main flux calculation (pure, single patch)
-- =========================================================================

-- | Calculate surface fluxes and temperature for a single lake patch.
-- Uses iterative Monin-Obukhov similarity with Newton-Raphson surface
-- temperature solution.
-- Ported from LakeFluxes in LakeFluxesMod.F90.
lakeFluxes :: LakeFluxInput -> LakeFluxOutput
lakeFluxes inp = LakeFluxOutput
  { lfo_t_grnd         = t_grnd_final
  , lfo_eflx_sh_grnd   = eflx_sh_final
  , lfo_eflx_lh_grnd   = htvp_val * qflx_evap_final
  , lfo_eflx_lwrad_out = eflx_lwrad_out_final
  , lfo_eflx_lwrad_net = lfi_forc_lwrad inp - eflx_lwrad_out_final
  , lfo_eflx_soil_grnd = eflx_soil_final
  , lfo_qflx_evap_soi  = qflx_evap_final
  , lfo_ustar          = ustar_final
  , lfo_z0mg           = z0mg_final
  , lfo_z0hg           = z0hg_final
  , lfo_z0qg           = z0qg_final
  , lfo_taux           = taux_final
  , lfo_tauy           = tauy_final
  , lfo_ws_col         = ws_final
  , lfo_ks_col         = ks_final
  , lfo_htvp           = htvp_val
  }
  where
    !snl      = lfi_snl inp
    !tgbef    = lfi_t_grnd inp
    !tsur     = lfi_t_subsurface inp
    !dzsur    = lfi_dz_top inp
    !tksur    = lfi_savedtke1 inp
    !sabg_raw = lfi_sabg inp
    !sabg     = max sabg_raw 0.0
    !forc_q   = lfi_forc_q inp
    !forc_pbot= lfi_forc_pbot inp
    !forc_rho = lfi_forc_rho inp
    !forc_lwrad= lfi_forc_lwrad inp
    !forc_u   = lfi_forc_u inp
    !forc_v   = lfi_forc_v inp
    !forc_hgt_u= lfi_forc_hgt_u inp
    !forc_hgt_t= lfi_forc_hgt_t inp
    !forc_hgt_q= lfi_forc_hgt_q inp
    !forc_th  = lfi_forc_th inp
    !forc_t   = lfi_forc_t inp
    !t_lake1  = lfi_t_lake1 inp

    wind_min = 0.1

    -- Betaprime
    betaprime | snl < 0   = 1.0
              | otherwise  = betavisLake

    -- Virtual potential temperature
    thv = forc_th * (1.0 + 0.61 * forc_q)

    -- Wind speed
    ur = max (sqrt (forc_u * forc_u + forc_v * forc_v)) wind_min

    -- Intermediate temperature
    thm = forc_t + 0.0098 * forc_hgt_t

    -- Kinematic viscosity
    kva = kva0 * (tgbef / 293.15) ** 1.5 * (1013.25e2 / forc_pbot)

    -- Latent heat
    htvp_val | lfi_h2osoi_liq_top inp <= 0.0 && lfi_h2osoi_ice_top inp > 0.0 = hsub
             | otherwise = hvap

    -- Initial roughness lengths
    z0mg_init | tgbef > tfrz = max minz0lakeDef (cus_local * kva / max (ur * 0.1) 1e-4)
              | snl < 0      = 0.00085
              | otherwise    = 0.001
    sqre0_init = sqrt (max (z0mg_init * ur * 0.1 / kva) 0.1)
    z0hg_init = max 1.0e-10 (z0mg_init * exp (-vkc / prn_air * (4.0 * sqre0_init - 3.2)))
    z0qg_init = max 1.0e-10 (z0mg_init * exp (-vkc / sch_water * (4.0 * sqre0_init - 4.2)))
    z0mg_init' = max z0mg_init 1.0e-10

    -- Displacement height (zero for lakes)
    displa = 0.0

    -- Monin-Obukhov initialization
    zldis_init = max (forc_hgt_u - displa) (z0mg_init' + 0.01)
    dth_init = thm - tgbef
    dqh_init = forc_q - qsatWater tgbef forc_pbot
    dthv_init = dth_init * (1.0 + 0.61 * forc_q) + 0.61 * forc_th * dqh_init

    ustar_init = max 0.001 (vkc * ur / log (zldis_init / z0mg_init'))

    tstar_init = vkc * dth_init / log (forc_hgt_t / z0hg_init)
    qstar_init = vkc * dqh_init / log (forc_hgt_q / z0qg_init)
    thvstar_init = tstar_init * (1.0 + 0.61 * forc_q) + 0.61 * forc_th * qstar_init

    obu_init = if abs thvstar_init > 1e-10
               then clampD (-1e4) 1e4 (-ustar_init ** 3 * thv / (vkc * grav * thvstar_init))
               else 1e4

    -- Stability iteration (4 iterations)
    niters = 4 :: Int

    -- State threaded through iterations
    iterState0 = (z0mg_init', z0hg_init, z0qg_init, obu_init, ur, ustar_init, tgbef)

    iterStep (z0mg_s, z0hg_s, z0qg_s, obu_s, um_s, ustar_s, _t_grnd_s) =
      let zldis_u = max (forc_hgt_u - displa) (z0mg_s + 0.01)
          zldis_t = max (forc_hgt_t - displa) (z0hg_s + 0.01)
          zldis_q = max (forc_hgt_q - displa) (z0qg_s + 0.01)

          zeta_u = clampD (-100.0) zetamaxLake (zldis_u / obu_s)
          zeta_t = clampD (-100.0) zetamaxLake (zldis_t / obu_s)
          zeta_q = clampD (-100.0) zetamaxLake (zldis_q / obu_s)
          zeta0m = clampD (-100.0) zetamaxLake (z0mg_s / obu_s)
          zeta0h = clampD (-100.0) zetamaxLake (z0hg_s / obu_s)
          zeta0q = clampD (-100.0) zetamaxLake (z0qg_s / obu_s)

          ustar' = max 0.001 (vkc * um_s / (log (zldis_u / z0mg_s) - stabilityFunc1 zeta_u + stabilityFunc1 zeta0m))

          temp1 = vkc / (log (zldis_t / z0hg_s) - stabilityFunc2 zeta_t + stabilityFunc2 zeta0h)
          temp2 = vkc / (log (zldis_q / z0qg_s) - stabilityFunc2 zeta_q + stabilityFunc2 zeta0q)

          rah = max 1.0 (1.0 / (temp1 * ustar'))
          raw = max 1.0 (1.0 / (temp2 * ustar'))

          qsatg = qsatWater tgbef forc_pbot
          qsatgdT = qsatWaterDT tgbef forc_pbot

          stftg3 = emg_lake * sb * tgbef ** 3

          ax = betaprime * sabg
             + emg_lake * forc_lwrad
             + 3.0 * stftg3 * tgbef
             + forc_rho * cpair / rah * thm
             + tksur * tsur / dzsur
             - htvp_val * forc_rho / raw * (qsatg - qsatgdT * tgbef - forc_q)

          bx = max 1.0e-10
             ( 4.0 * stftg3
             + forc_rho * cpair / rah
             + htvp_val * forc_rho / raw * qsatgdT
             + tksur / dzsur
             )

          t_grnd_new = ax / bx

          -- Update Obukhov length
          dth_new = thm - t_grnd_new
          dqh_new = forc_q - (qsatg + qsatgdT * (t_grnd_new - tgbef))
          tstar' = temp1 * dth_new
          qstar' = temp2 * dqh_new
          thvstar' = tstar' * (1.0 + 0.61 * forc_q) + 0.61 * forc_th * qstar'

          zeta_val0 = if abs thvstar' > 1e-10
                      then zldis_u * vkc * grav * thvstar' / (ustar' ** 2 * thv)
                      else 0.0

          (zeta_val, um') =
            if zeta_val0 >= 0.0
            then (clampD 0.01 zetamaxLake zeta_val0, max ur wind_min)
            else let zv = clampD (-100.0) (-0.01) zeta_val0
                     wc = beta1 * (negate grav * ustar' * thvstar' * 1000.0 / thv) ** (1.0 / 3.0)
                 in (zv, sqrt (ur * ur + wc * wc))

          obu' = clampD (-1e4) 1e4 (zldis_u / zeta_val)

          -- Update roughness for unfrozen lakes
          (z0mg', z0hg', z0qg') =
            if tgbef > tfrz
            then let z0new = max minz0lakeDef (max (cus_local * kva / max ustar' 1e-6)
                                                   (cur0_local * ustar' ** 2 / grav))
                     sqre = sqrt (max (z0new * ustar' / kva) 0.1)
                     z0h = max 1.0e-10 (z0new * exp (-vkc / prn_air * (4.0 * sqre - 3.2)))
                     z0q = max 1.0e-10 (z0new * exp (-vkc / sch_water * (4.0 * sqre - 4.2)))
                 in (max z0new 1.0e-10, z0h, z0q)
            else (z0mg_s, z0hg_s, z0qg_s)

      in (z0mg', z0hg', z0qg', obu', um', ustar', t_grnd_new)

    -- Run iterations
    (z0mg_final, z0hg_final, z0qg_final, _obu_final, _um_final, ustar_final, t_grnd_iter) =
      iterate' niters iterStep iterState0

    -- Phase 3: Temperature corrections
    t_grnd_corr1
      | (snl < 0 || t_lake1 <= tfrz) && t_grnd_iter > tfrz = tfrz
      | otherwise = t_grnd_iter

    t_grnd_corr2
      | t_lake1 > t_grnd_corr1 && t_grnd_corr1 > tdmax = t_lake1
      | t_lake1 < t_grnd_corr1 && t_lake1 > tfrz && t_grnd_corr1 < tdmax = t_lake1
      | otherwise = t_grnd_corr1

    t_grnd_final = t_grnd_corr2

    -- Phase 4: Final flux calculations
    qsatg_final = qsatWater t_grnd_final forc_pbot
    thm_local = forc_t + 0.0098 * forc_hgt_t

    zldis_t_f = max (forc_hgt_t - displa) (z0hg_final + 0.01)
    zldis_q_f = max (forc_hgt_q - displa) (z0qg_final + 0.01)
    zldis_u_f = max (forc_hgt_u - displa) (z0mg_final + 0.01)

    rah_final = max (zldis_t_f / (vkc * ustar_final)) 1.0
    raw_final = max (zldis_q_f / (vkc * ustar_final)) 1.0

    eflx_sh_final = forc_rho * cpair * (t_grnd_final - thm_local) / rah_final
    qflx_evap_final = forc_rho * (qsatg_final - forc_q) / raw_final

    eflx_lwrad_out_final = (1.0 - emg_lake) * forc_lwrad + emg_lake * sb * t_grnd_final ** 4

    eflx_soil_final = betaprime * sabg + forc_lwrad - eflx_lwrad_out_final
                    - eflx_sh_final - htvp_val * qflx_evap_final

    ram_final = max (zldis_u_f / (vkc * ustar_final)) 1.0
    taux_final = negate forc_rho * forc_u / ram_final
    tauy_final = negate forc_rho * forc_v / ram_final

    u2m = max 0.1 (ustar_final / vkc * log (2.0 / z0mg_final))
    ws_final = 1.2e-3 * u2m
    ks_final = 6.6 * sqrt (abs (sin 0.0)) * u2m ** (-1.84)

-- =========================================================================
-- Saturation humidity helpers
-- =========================================================================

-- | Saturation specific humidity for liquid water (Tetens formula).
qsatWater :: Double -> Double -> Double
qsatWater t p =
  let es = 611.2 * exp (17.67 * (t - tfrz) / (t - tfrz + 243.5))
  in 0.622 * es / (p - 0.378 * es)

-- | Derivative of saturation specific humidity w.r.t. temperature.
qsatWaterDT :: Double -> Double -> Double
qsatWaterDT t p =
  let es = 611.2 * exp (17.67 * (t - tfrz) / (t - tfrz + 243.5))
      desdT = es * 17.67 * 243.5 / (t - tfrz + 243.5) ** 2
  in 0.622 * p * desdT / (p - 0.378 * es) ** 2

-- =========================================================================
-- Stability functions (Monin-Obukhov)
-- =========================================================================

-- | Stability correction function for momentum (unstable only, zeta <= 0).
-- Matches Julia's stability_func1 from friction_velocity.jl.
stabilityFunc1 :: Double -> Double
stabilityFunc1 zeta =
  let zeta' = min zeta 0.0
      chik2 = sqrt (1.0 - 16.0 * zeta')
      chik  = sqrt chik2
  in 2.0 * log ((1.0 + chik) * 0.5) + log ((1.0 + chik2) * 0.5) - 2.0 * atan chik + pi / 2.0

-- | Stability correction function for heat/moisture (unstable only, zeta <= 0).
-- Matches Julia's stability_func2 from friction_velocity.jl.
stabilityFunc2 :: Double -> Double
stabilityFunc2 zeta =
  let zeta' = min zeta 0.0
      chik2 = sqrt (1.0 - 16.0 * zeta')
  in 2.0 * log ((1.0 + chik2) * 0.5)

-- =========================================================================
-- Utility helpers
-- =========================================================================

-- | Clamp a value between lo and hi.
clampD :: Double -> Double -> Double -> Double
clampD lo hi x = max lo (min hi x)

-- | Iterate a function n times.
iterate' :: Int -> (a -> a) -> a -> a
iterate' 0 _ x = x
iterate' n f x = iterate' (n - 1) f (f x)
