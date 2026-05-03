{-# LANGUAGE BangPatterns #-}
-- | Nitrification and denitrification rates.
-- CENTURY nitrification scheme (Parton et al., 2001, 1996) and
-- Del Grosso et al. (2000) denitrification model.
-- Fortran: SoilBiogeochemNitrifDenitrifMod.F90
-- Julia:   src/biogeochem/nitrif_denitrif.jl
--
-- All functions are pure.
module CLM.BioGeoChem.NitrifDenitrif
  ( -- * Data types
    NitrifDenitrifParams(..)
  , defaultNitrifDenitrifParams
  , NitrifDenitrifInput(..)
  , NitrifDenitrifOutput(..)
    -- * Main computation
  , nitrifDenitrif
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Freezing temperature of water (K).
tfrz :: Double
tfrz = 273.15

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | Pi.
rpi :: Double
rpi = 3.14159265358979323846

-- | Gravitational acceleration (m/s2).
grav :: Double
grav = 9.80616

-- | Nitrification/denitrification parameters.
data NitrifDenitrifParams = NitrifDenitrifParams
  { ndp_k_nitr_max_perday                :: !Double
  , ndp_surface_tension_water            :: !Double
  , ndp_rij_kro_a                        :: !Double
  , ndp_rij_kro_alpha                    :: !Double
  , ndp_rij_kro_beta                     :: !Double
  , ndp_rij_kro_gamma                    :: !Double
  , ndp_rij_kro_delta                    :: !Double
  , ndp_denitrif_respiration_coefficient :: !Double
  , ndp_denitrif_respiration_exponent    :: !Double
  , ndp_denitrif_nitrateconc_coefficient :: !Double
  , ndp_denitrif_nitrateconc_exponent    :: !Double
  , ndp_om_frac_sf                       :: !Double
  } deriving (Show, Eq)

defaultNitrifDenitrifParams :: NitrifDenitrifParams
defaultNitrifDenitrifParams = NitrifDenitrifParams
  { ndp_k_nitr_max_perday = 0.1
  , ndp_surface_tension_water = 0.073
  , ndp_rij_kro_a = 1.5e-10
  , ndp_rij_kro_alpha = 1.26
  , ndp_rij_kro_beta = 0.6
  , ndp_rij_kro_gamma = 0.6
  , ndp_rij_kro_delta = 0.85
  , ndp_denitrif_respiration_coefficient = 0.1
  , ndp_denitrif_respiration_exponent = 1.3
  , ndp_denitrif_nitrateconc_coefficient = 0.1
  , ndp_denitrif_nitrateconc_exponent = 1.3
  , ndp_om_frac_sf = 1.0
  }

-- | Input to nitrification/denitrification calculation.
data NitrifDenitrifInput = NitrifDenitrifInput
  { ndi_nc               :: !Int
  , ndi_nlevdecomp       :: !Int
  , ndi_mask             :: !(VU.Vector Bool)
  , ndi_params           :: !NitrifDenitrifParams
  , ndi_organic_max      :: !Double
  -- Soil state (nc * nlev)
  , ndi_watsat           :: !(VU.Vector Double)
  , ndi_watfc            :: !(VU.Vector Double)
  , ndi_bd               :: !(VU.Vector Double)
  , ndi_bsw              :: !(VU.Vector Double)
  , ndi_cellorg          :: !(VU.Vector Double)
  , ndi_sucsat           :: !(VU.Vector Double)
  , ndi_soilpsi          :: !(VU.Vector Double)
  , ndi_h2osoi_vol       :: !(VU.Vector Double)
  , ndi_h2osoi_liq       :: !(VU.Vector Double)
  , ndi_t_soisno         :: !(VU.Vector Double)
  , ndi_col_dz           :: !(VU.Vector Double)
  -- CH4 arrays (nc * nlev)
  , ndi_o2_decomp_depth_unsat :: !(VU.Vector Double)
  , ndi_conc_o2_unsat    :: !(VU.Vector Double)
  -- Scalar fields (nc * nlev)
  , ndi_t_scalar         :: !(VU.Vector Double)  -- ^ temperature scalar from HR
  , ndi_w_scalar         :: !(VU.Vector Double)  -- ^ water scalar from HR
  , ndi_phr_vr           :: !(VU.Vector Double)  -- ^ potential HR rate
  -- Mineral N state (nc * nlev)
  , ndi_smin_nh4_vr      :: !(VU.Vector Double)
  , ndi_smin_no3_vr      :: !(VU.Vector Double)
  -- Control flags
  , ndi_use_lch4         :: !Bool
  , ndi_no_frozen_nitrif_denitrif :: !Bool
  -- Diffusion constants for O2 (gas and water)
  -- D_CON_G(2,:) = [a, b] where D_g = (a + b*T) * 1e-4
  -- D_CON_W(2,:) = [a, b, c] where D_w = (a + b*T + c*T^2) * 1e-9
  , ndi_d_con_g21        :: !Double  -- ^ gas diffusion const a for O2
  , ndi_d_con_g22        :: !Double  -- ^ gas diffusion const b for O2
  , ndi_d_con_w21        :: !Double  -- ^ water diffusion const a for O2
  , ndi_d_con_w22        :: !Double  -- ^ water diffusion const b for O2
  , ndi_d_con_w23        :: !Double  -- ^ water diffusion const c for O2
  } deriving (Show)

-- | Output of nitrification/denitrification calculation.
data NitrifDenitrifOutput = NitrifDenitrifOutput
  { ndo_pot_f_nit_vr           :: !(VU.Vector Double)  -- ^ potential nitrification (nc * nlev)
  , ndo_pot_f_denit_vr         :: !(VU.Vector Double)  -- ^ potential denitrification
  , ndo_n2_n2o_ratio_denit_vr  :: !(VU.Vector Double)  -- ^ N2:N2O ratio from denitrification
  , ndo_anaerobic_frac         :: !(VU.Vector Double)  -- ^ anoxic fraction
  , ndo_k_nitr_vr              :: !(VU.Vector Double)  -- ^ nitrification rate constant
  , ndo_diffus                 :: !(VU.Vector Double)  -- ^ diffusivity (m^2/s)
  } deriving (Show)

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Calculate nitrification and denitrification rates.
nitrifDenitrif :: NitrifDenitrifInput -> NitrifDenitrifOutput
nitrifDenitrif inp =
  let !nc   = ndi_nc inp
      !nlev = ndi_nlevdecomp inp
      mask  = ndi_mask inp
      params = ndi_params inp
      use_lch4 = ndi_use_lch4 inp
      no_frozen = ndi_no_frozen_nitrif_denitrif inp
      organic_max = ndi_organic_max inp
      rho_w = 1.0e3

      k_nitr_max_perday = ndp_k_nitr_max_perday params
      surface_tension_water = ndp_surface_tension_water params
      rij_kro_a     = ndp_rij_kro_a params
      rij_kro_alpha = ndp_rij_kro_alpha params
      rij_kro_beta  = ndp_rij_kro_beta params
      rij_kro_gamma = ndp_rij_kro_gamma params
      rij_kro_delta = ndp_rij_kro_delta params
      denit_resp_coef    = ndp_denitrif_respiration_coefficient params
      denit_resp_exp     = ndp_denitrif_respiration_exponent params
      denit_nitrate_coef = ndp_denitrif_nitrateconc_coefficient params
      denit_nitrate_exp  = ndp_denitrif_nitrateconc_exponent params

      -- Compute all per-cell results
      results = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c)
           then (0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
           else
             let t_soil = ndi_t_soisno inp VU.! idx2 nc c j
                 watsat_v = ndi_watsat inp VU.! idx2 nc c j
                 watfc_v  = ndi_watfc inp VU.! idx2 nc c j
                 h2osoi_vol_v = ndi_h2osoi_vol inp VU.! idx2 nc c j
                 h2osoi_liq_v = ndi_h2osoi_liq inp VU.! idx2 nc c j
                 bsw_v    = ndi_bsw inp VU.! idx2 nc c j
                 cellorg_v = ndi_cellorg inp VU.! idx2 nc c j
                 soilpsi_v = ndi_soilpsi inp VU.! idx2 nc c j
                 bd_v     = ndi_bd inp VU.! idx2 nc c j
                 col_dz_v = ndi_col_dz inp VU.! idx2 nc c j
                 t_scalar_v = ndi_t_scalar inp VU.! idx2 nc c j
                 w_scalar_v = ndi_w_scalar inp VU.! idx2 nc c j
                 phr_v    = ndi_phr_vr inp VU.! idx2 nc c j
                 smin_nh4 = ndi_smin_nh4_vr inp VU.! idx2 nc c j
                 smin_no3 = ndi_smin_no3_vr inp VU.! idx2 nc c j

                 pH_c = 6.5

                 -- Anoxic fraction
                 fc_air_frac = watsat_v - watfc_v
                 fc_air_frac_as_frac_porosity = 1.0 - watfc_v / watsat_v

                 (anaerobic_frac, diffus_rel) =
                   if use_lch4
                   then let om_frac = if organic_max > 0.0
                                      then min (ndp_om_frac_sf params * cellorg_v / organic_max) 1.0
                                      else 1.0
                            dm = fc_air_frac ** 2 * fc_air_frac_as_frac_porosity ** (3.0 / bsw_v)
                            dmq = fc_air_frac ** (10.0 / 3.0) / watsat_v ** 2
                            dv = om_frac * dmq + (1.0 - om_frac) * dm

                            r_min_val = 2.0 * surface_tension_water / (rho_w * grav * abs soilpsi_v)
                            r_max = 2.0 * surface_tension_water / (rho_w * grav * 0.1)
                            r_psi = sqrt (r_min_val * r_max)

                            ratio_dw_dg = (ndi_d_con_g21 inp + ndi_d_con_g22 inp * t_soil) * 1.0e-4
                                        / ((ndi_d_con_w21 inp + ndi_d_con_w22 inp * t_soil
                                            + ndi_d_con_w23 inp * t_soil ** 2) * 1.0e-9)

                            o2_unsat = ndi_o2_decomp_depth_unsat inp VU.! idx2 nc c j
                            conc_o2  = ndi_conc_o2_unsat inp VU.! idx2 nc c j

                            af = if o2_unsat > 0.0
                                 then exp (negate rij_kro_a
                                          * r_psi ** (negate rij_kro_alpha)
                                          * o2_unsat ** (negate rij_kro_beta)
                                          * conc_o2 ** rij_kro_gamma
                                          * (h2osoi_vol_v + ratio_dw_dg * watsat_v) ** rij_kro_delta)
                                 else 0.0
                        in (af, dv)
                   else (0.0, 0.0)

                 -- Nitrification
                 k_nitr_t = min t_scalar_v 1.0
                 k_nitr_ph = 0.56 + atan (rpi * 0.45 * (-5.0 + pH_c)) / rpi
                 k_nitr_h2o = w_scalar_v
                 k_nitr = k_nitr_max_perday / secspday * k_nitr_t * k_nitr_h2o * k_nitr_ph

                 pot_nit0 = max (smin_nh4 * k_nitr) 0.0
                 pot_nit1 = pot_nit0 * (1.0 - anaerobic_frac)
                 pot_nit = if t_soil <= tfrz && no_frozen then 0.0 else pot_nit1

                 -- Denitrification
                 soil_bulkdensity = bd_v + h2osoi_liq_v / col_dz_v
                 g_to_ug = 1.0e3 / soil_bulkdensity
                 g_sec_to_ug_day = g_to_ug * secspday
                 no3_massdens = max smin_no3 0.0 * g_to_ug
                 soil_co2_prod = phr_v * g_sec_to_ug_day

                 fmax_carbon = (denit_resp_coef * (soil_co2_prod ** denit_resp_exp)) / g_sec_to_ug_day
                 fmax_nitrate = (denit_nitrate_coef * no3_massdens ** denit_nitrate_exp) / g_sec_to_ug_day

                 f_denit_base0 = max (min fmax_carbon fmax_nitrate) 0.0
                 f_denit_base = if t_soil <= tfrz && no_frozen then 0.0 else f_denit_base0

                 pot_denit = f_denit_base * anaerobic_frac

                 -- N2:N2O ratio
                 ratio_k1 = max 1.7 (38.4 - 350.0 * diffus_rel)

                 d0 = (ndi_d_con_g21 inp + ndi_d_con_g22 inp * t_soil) * 1.0e-4
                 diffus_final = diffus_rel * d0

                 ratio_no3_co2 = if soil_co2_prod > 1.0e-9
                                 then no3_massdens / soil_co2_prod
                                 else 100.0

                 wfps = max (min (h2osoi_vol_v / watsat_v) 1.0) 0.0 * 100.0
                 fr_wfps = max 0.1 (0.015 * wfps - 0.32)

                 n2_n2o_ratio = max (0.16 * ratio_k1)
                                    (ratio_k1 * exp (-0.8 * ratio_no3_co2)) * fr_wfps

             in (pot_nit, pot_denit, n2_n2o_ratio, anaerobic_frac, k_nitr, diffus_final)

  in NitrifDenitrifOutput
    { ndo_pot_f_nit_vr          = VU.map (\(a,_,_,_,_,_) -> a) results
    , ndo_pot_f_denit_vr        = VU.map (\(_,a,_,_,_,_) -> a) results
    , ndo_n2_n2o_ratio_denit_vr = VU.map (\(_,_,a,_,_,_) -> a) results
    , ndo_anaerobic_frac        = VU.map (\(_,_,_,a,_,_) -> a) results
    , ndo_k_nitr_vr             = VU.map (\(_,_,_,_,a,_) -> a) results
    , ndo_diffus                = VU.map (\(_,_,_,_,_,a) -> a) results
    }
