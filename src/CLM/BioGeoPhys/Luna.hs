{-# LANGUAGE BangPatterns #-}
-- | LUNA (Leaf Utilization of Nitrogen for Assimilation) model.
-- Fortran: LunaMod.F90
--
-- Calculates photosynthetic capacities based on prescribed leaf nitrogen
-- content using the LUNA model (Xu et al 2012; Ali et al 2015a).
-- Currently only works for C3 plants.
--
-- Provides:
--   * Temperature response functions (Kattge, Leuning, Bernacchi)
--   * Reference and current NUE calculation
--   * Photosynthesis for LUNA nitrogen allocation
--   * Nitrogen investment calculations
--   * Nitrogen allocation optimization
--   * 24hr / 240hr climate accumulation helpers
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.Luna
  ( -- * Constants
    lunaCv
  , lunaFc25
  , lunaFj25
  , lunaNUEr25
  , lunaCb
  , lunaO2ref
  , lunaCO2ref
  , lunaForcPbotRef
  , lunaQ10Enz
  , lunaNMCp25
  , lunaTrange1
  , lunaTrange2
  , lunaSNC
  , lunaMp
  , lunaPARLowLim
    -- * Types
  , LunaParams(..)
  , defaultLunaParams
  , NUEResult(..)
  , NitrogenInvestResult(..)
  , NitrogenAllocResult(..)
  , PhotoLunaResult(..)
    -- * Science functions
  , vcmxTKattge
  , jmxTKattge
  , vcmxTLeuning
  , jmxTLeuning
  , respTBernacchi
  , quadraticLuna
  , nueRef
  , nueCalc
  , photosynthesisLuna
  , nitrogenInvestments
  , nitrogenAllocation
  , isTimeToRunLuna
  ) where

import CLM.Constants.PhysicalConstants (tfrz)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Universal gas constant (J/K/mol) - LUNA uses per-mole, not per-kmol
rgas_luna :: Double
rgas_luna = 8.314

-- | Conversion factor umol CO2 -> g carbon
lunaCv :: Double
lunaCv = 1.2e-5 * 3600.0

-- | Fc25 = 6.22 * 47.3
lunaFc25 :: Double
lunaFc25 = 294.2

-- | Fj25 = 8.06 * 156
lunaFj25 :: Double
lunaFj25 = 1257.0

-- | Nitrogen use efficiency for respiration
lunaNUEr25 :: Double
lunaNUEr25 = 33.69

-- | NUE for chlorophyll light capture
lunaCb :: Double
lunaCb = 1.78

-- | O2 in air (ppm)
lunaO2ref :: Double
lunaO2ref = 209460.0

-- | Reference CO2 (ppm)
lunaCO2ref :: Double
lunaCO2ref = 380.0

-- | Reference air pressure (Pa)
lunaForcPbotRef :: Double
lunaForcPbotRef = 101325.0

-- | Q10 for enzyme decay
lunaQ10Enz :: Double
lunaQ10Enz = 2.0

-- | Maintenance respiration fraction
lunaNMCp25 :: Double
lunaNMCp25 = 0.715

-- | Lower temperature limit (C)
lunaTrange1 :: Double
lunaTrange1 = 5.0

-- | Upper temperature limit (C)
lunaTrange2 :: Double
lunaTrange2 = 42.0

-- | Structural nitrogen concentration (g N / g dry mass C)
lunaSNC :: Double
lunaSNC = 0.004

-- | Slope of stomatal conductance
lunaMp :: Double
lunaMp = 9.0

-- | Minimum PAR for nitrogen optimization (umol/m2/s)
lunaPARLowLim :: Double
lunaPARLowLim = 200.0

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data LunaParams = LunaParams
  { lp_kc25_coef      :: !Double
  , lp_ko25_coef      :: !Double
  , lp_cp25_yr2000    :: !Double
  , lp_luna_theta_cj  :: !Double
  , lp_minrelh        :: !Double
  , lp_relhExp        :: !Double
  , lp_enzyme_turnover_daily :: !Double
  , lp_jmaxb0         :: !Double  -- ^ per-PFT jmaxb0
  , lp_jmaxb1         :: !Double  -- ^ per-PFT jmaxb1
  , lp_wc2wjb0        :: !Double  -- ^ per-PFT wc2wjb0
  } deriving (Show, Eq)

defaultLunaParams :: LunaParams
defaultLunaParams = LunaParams
  { lp_kc25_coef = 40.49
  , lp_ko25_coef = 27840.0
  , lp_cp25_yr2000 = 4.275
  , lp_luna_theta_cj = 0.95
  , lp_minrelh = 0.25
  , lp_relhExp = 6.0
  , lp_enzyme_turnover_daily = 0.1
  , lp_jmaxb0 = 0.0
  , lp_jmaxb1 = 0.0
  , lp_wc2wjb0 = 0.8
  }

data NUEResult = NUEResult
  { nue_j   :: !Double  -- ^ NUE for electron transport
  , nue_c   :: !Double  -- ^ NUE for carboxylation
  , nue_ratio :: !Double  -- ^ Kj / Kc
  } deriving (Show, Eq)

data NitrogenInvestResult = NitrogenInvestResult
  { nir_vcmax   :: !Double
  , nir_jmax    :: !Double
  , nir_jmeanL  :: !Double
  , nir_jmaxL   :: !Double
  , nir_net     :: !Double
  , nir_ncb     :: !Double
  , nir_nresp   :: !Double
  , nir_psn     :: !Double
  , nir_resp    :: !Double
  , nir_kc      :: !Double
  , nir_kj      :: !Double
  , nir_ci      :: !Double
  } deriving (Show, Eq)

data NitrogenAllocResult = NitrogenAllocResult
  { nar_pnstore :: !Double
  , nar_pnlc    :: !Double
  , nar_pnet    :: !Double
  , nar_pnresp  :: !Double
  , nar_pncb    :: !Double
  } deriving (Show, Eq)

data PhotoLunaResult = PhotoLunaResult
  { plr_ci :: !Double
  , plr_kc :: !Double
  , plr_kj :: !Double
  , plr_a  :: !Double
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Temperature response functions
--------------------------------------------------------------------------------

-- | Vcmax temperature response with acclimation (Kattge & Knorr 2007).
vcmxTKattge :: Double -> Double -> Double
vcmxTKattge !tgrow !tleaf =
  let !tlimVcmx = 668.39 - 1.07 * min (max tgrow 11.0) 35.0
      !f1 = 1.0 + exp ((tlimVcmx * (25.0 + tfrz) - 200000.0) / (rgas_luna * (25.0 + tfrz)))
      !f2 = exp ((72000.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !f3 = 1.0 + exp ((tlimVcmx * (tleaf + tfrz) - 200000.0) / (rgas_luna * (tleaf + tfrz)))
  in f1 * f2 / f3

-- | Jmax temperature response with acclimation (Kattge & Knorr 2007).
jmxTKattge :: Double -> Double -> Double
jmxTKattge !tgrow !tleaf =
  let !tlimJmx = 659.7 - 0.75 * min (max tgrow 11.0) 35.0
      !f1 = 1.0 + exp ((tlimJmx * (25.0 + tfrz) - 200000.0) / (rgas_luna * (25.0 + tfrz)))
      !f2 = exp ((50000.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tleaf + tfrz)))
      !f3 = 1.0 + exp ((tlimJmx * (tleaf + tfrz) - 200000.0) / (rgas_luna * (tleaf + tfrz)))
  in f1 * f2 / f3

-- | Vcmax temperature response without acclimation (Leuning 2002).
vcmxTLeuning :: Double -> Double -> Double
vcmxTLeuning !_tgrow !tleaf =
  let !tlim = 486.0
      !f1 = 1.0 + exp ((tlim * (25.0 + tfrz) - 149252.0) / (rgas_luna * (25.0 + tfrz)))
      !f2 = exp ((73637.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !f3 = 1.0 + exp ((tlim * (tleaf + tfrz) - 149252.0) / (rgas_luna * (tleaf + tfrz)))
  in f1 * f2 / f3

-- | Jmax temperature response without acclimation (Leuning 2002).
jmxTLeuning :: Double -> Double -> Double
jmxTLeuning !_tgrow !tleaf =
  let !tlim = 495.0
      !f1 = 1.0 + exp ((tlim * (25.0 + tfrz) - 152044.0) / (rgas_luna * (25.0 + tfrz)))
      !f2 = exp ((50300.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !f3 = 1.0 + exp ((tlim * (tleaf + tfrz) - 152044.0) / (rgas_luna * (tleaf + tfrz)))
  in f1 * f2 / f3

-- | Respiration temperature response (Bernacchi PCE 2001).
respTBernacchi :: Double -> Double
respTBernacchi !tleaf =
  exp (18.72 - 46.39 / (rgas_luna * (tleaf + tfrz)))

--------------------------------------------------------------------------------
-- Quadratic solver (LUNA-specific)
--------------------------------------------------------------------------------

-- | Solve a*x^2 + b*x + c = 0. Returns (r1, r2).
quadraticLuna :: Double -> Double -> Double -> (Double, Double)
quadraticLuna !a !b !c
  | a == 0.0 = (1.0e36, 1.0e36)
  | otherwise =
    let !disc = b * b - 4.0 * a * c
        !q | b >= 0.0  = -0.5 * (b + sqrt disc)
           | otherwise = -0.5 * (b - sqrt disc)
        !r1 = q / a
        !r2 | q /= 0.0 = c / q
            | otherwise = 1.0e36
    in (r1, r2)

--------------------------------------------------------------------------------
-- NUE calculations
--------------------------------------------------------------------------------

-- | Reference nitrogen use efficiency at 25C and 380 ppm CO2.
nueRef :: LunaParams -> NUEResult
nueRef !lp =
  let !fc = vcmxTKattge 25.0 25.0 * lunaFc25
      !fj = jmxTKattge 25.0 25.0 * lunaFj25
      !co2c = lunaCO2ref * lunaForcPbotRef * 1.0e-6
      !o2c  = lunaO2ref  * lunaForcPbotRef * 1.0e-6
      !kc = lp_kc25_coef lp * exp ((79430.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + 25.0)))
      !ko = lp_ko25_coef lp * exp ((36380.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + 25.0)))
      !cp = lp_cp25_yr2000 lp * exp ((37830.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + 25.0)))
      !awc = kc * (1.0 + o2c / ko)
      !ci = 0.7 * co2c
      !kj_val = max (ci - cp) 0.0 / (4.0 * ci + 8.0 * cp)
      !kc_val = max (ci - cp) 0.0 / (ci + awc)
  in NUEResult
     { nue_j = kj_val * fj
     , nue_c = kc_val * fc
     , nue_ratio = kj_val / kc_val
     }

-- | Current NUE under given environmental conditions.
nueCalc :: Double -> Double -> Double -> Double -> LunaParams -> NUEResult
nueCalc !o2a !ci_in !tgrow !tleaf !lp =
  let !fc = vcmxTKattge tgrow tleaf * lunaFc25
      !fj = jmxTKattge tgrow tleaf * lunaFj25
      !kc = lp_kc25_coef lp * exp ((79430.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !ko = lp_ko25_coef lp * exp ((36380.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !cp = lp_cp25_yr2000 lp * exp ((37830.0 / (rgas_luna * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !awc = kc * (1.0 + o2a / ko)
      !kj_val = max (ci_in - cp) 0.0 / (4.0 * ci_in + 8.0 * cp)
      !kc_val = max (ci_in - cp) 0.0 / (ci_in + awc)
  in NUEResult
     { nue_j = kj_val * fj
     , nue_c = kc_val * fc
     , nue_ratio = kj_val / kc_val
     }

--------------------------------------------------------------------------------
-- Photosynthesis for LUNA
--------------------------------------------------------------------------------

-- | Solve photosynthesis equations for LUNA nitrogen allocation.
photosynthesisLuna :: Double -> Double -> Double -> Double -> Double -> Double
                   -> Double -> Double -> LunaParams -> PhotoLunaResult
photosynthesisLuna !forc_pbot !tleafd !relh !co2a !o2a !rb !vcmax !jmeanL !lp =
  let !rsmax0 = 2.0 * 1.0e4
      !bp = 2000.0
      !tleaf = tleafd
      !tleafk = tleaf + tfrz
      !relhc = max (lp_minrelh lp) relh
      !bbb = 1.0 / bp
      !mbb = lunaMp
      !co2c = co2a
      !cf = forc_pbot / (8.314 * tleafk) * 1.0e6
      !gb_mol = cf / rb
      !kc_e = lp_kc25_coef lp * exp ((79430.0 / (8.314 * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !ko_e = lp_ko25_coef lp * exp ((36380.0 / (8.314 * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !cp_e = lp_cp25_yr2000 lp * exp ((37830.0 / (8.314 * (25.0 + tfrz))) * (1.0 - (tfrz + 25.0) / (tfrz + tleaf)))
      !awc = kc_e * (1.0 + o2a / ko_e)

      -- Rubisco-limited iteration
      iterRub !n !ci !ciold !gs_mol_v
        | n >= 100 || abs (ci - ciold) <= 0.01 = (ci, gs_mol_v)
        | otherwise =
          let !kc_v = max (ci - cp_e) 0.0 / (ci + awc)
              !wc = kc_v * vcmax
              !gs = bbb + mbb * wc / co2c * forc_pbot * relhc
              !phi = forc_pbot * (1.37 * gs + 1.6 * gb_mol) / (gb_mol * gs)
              !bquad = awc - co2c + phi * vcmax
              !cquad = negate (cp_e * phi * vcmax + awc * co2c)
              (!r1, !r2) = quadraticLuna 1.0 bquad cquad
              !ci_new' = max r1 r2
              !ci_new = if ci_new' < 0.0 then cp_e + 0.5 * ci else ci_new'
          in iterRub (n + 1) ci_new ci gs

      (!ci0, !gs0) = iterRub 0 (0.7 * co2a) (0.7 * co2a - 0.02) bbb

      !kj0 = max (ci0 - cp_e) 0.0 / (4.0 * ci0 + 8.0 * cp_e)
      !kc0_v = max (ci0 - cp_e) 0.0 / (ci0 + awc)
      !wc0 = kc0_v * vcmax
      !wj0 = kj0 * jmeanL

      -- Light-limited iteration (if Wj < Wc)
      iterLight !n !ci !ciold
        | n >= 100 || abs (ci - ciold) <= 0.01 = ci
        | otherwise =
          let !kj_v = max (ci - cp_e) 0.0 / (4.0 * ci + 8.0 * cp_e)
              !wj = kj_v * jmeanL
              !gs = bbb + mbb * wj / co2c * forc_pbot * relhc
              !phi = forc_pbot * (1.37 * gs + 1.6 * gb_mol) / (gb_mol * gs)
              !bquad = 2.0 * cp_e - co2c + phi * jmeanL / 4.0
              !cquad = negate (cp_e * phi * jmeanL / 4.0 + 2.0 * cp_e * co2c)
              (!r1, !r2) = quadraticLuna 1.0 bquad cquad
              !ci_new' = max r1 r2
              !ci_new = if ci_new' < 0.0 then cp_e + 0.5 * ci else ci_new'
          in iterLight (n + 1) ci_new ci

      !ciFinal | wj0 < wc0 = iterLight 0 ci0 (ci0 - 0.02)
               | otherwise = ci0

      !kjF = max (ciFinal - cp_e) 0.0 / (4.0 * ciFinal + 8.0 * cp_e)
      !kcF = max (ciFinal - cp_e) 0.0 / (ciFinal + awc)
      !wcF = kcF * vcmax
      !wjF = kjF * jmeanL
      !theta_cj = lp_luna_theta_cj lp
      !aF = (1.0 - theta_cj) * max wcF wjF + theta_cj * min wcF wjF

  in PhotoLunaResult
     { plr_ci = ciFinal
     , plr_kc = kcF
     , plr_kj = kjF
     , plr_a  = aF
     }

--------------------------------------------------------------------------------
-- Nitrogen investments
--------------------------------------------------------------------------------

-- | Calculate nitrogen investments for given Nlc.
nitrogenInvestments :: Int -> Double -> Double -> Double -> Double -> Double -> Double
                    -> Double -> Double -> Double -> Double -> Double -> Double -> Double
                    -> Double -> Double -> Double -> Double -> Double -> Double -> Double
                    -> Double -> Double -> Double -> Double -> Double -> Double
                    -> Double -> Double
                    -> LunaParams -> NitrogenInvestResult
nitrogenInvestments !kcKjFlag !fnca !nlc !forc_pbot10 !relh10 !co2a10 !o2a10
                    !pari10 !parimx10 !rb10 !hourpd !tair10 !tleafd10 !tleafn10
                    !kj2kc !jmaxCoef !fc !fj !nuec !nuej !nuecref !nuejref
                    !nuer !o3coefjmax !jmaxb0 !wc2wjb0
                    !kc_in !kj_in !ci_in !lp =
  let !leaf_mr_vcm = 0.015
      !theta = 0.292 / (1.0 + 0.076 / (nlc * lunaCb))
      !eltrnAbsorb = theta * pari10
      !jmaxb0act = jmaxb0 * fnca * fj
      !jmax = jmaxb0act + jmaxCoef * eltrnAbsorb * o3coefjmax
      !jmaxL = theta * parimx10 / sqrt (1.0 + (theta * parimx10 / jmax) ** 2.0)
      !nueChg = (nuec / nuecref) * (nuejref / nuej)
      !wc2wj = wc2wjb0 * (nueChg ** 0.5)
      !vcmax = wc2wj * jmaxL * kj2kc
      !jmeanL = theta * pari10 / sqrt (1.0 + (eltrnAbsorb / jmax) ** 2.0)

      (!kc_out, !kj_out, !ci_out, !a_out)
        | kcKjFlag == 0 =
          let !r = photosynthesisLuna forc_pbot10 tleafd10 relh10 co2a10 o2a10 rb10 vcmax jmeanL lp
          in (plr_kc r, plr_kj r, plr_ci r, plr_a r)
        | otherwise =
          let !wc = kc_in * vcmax
              !wj = kj_in * jmeanL
              !theta_cj = lp_luna_theta_cj lp
              !a = (1.0 - theta_cj) * max wc wj + theta_cj * min wc wj
          in (kc_in, kj_in, ci_in, a)

      !psn = lunaCv * a_out * hourpd
      !vcmaxNight = vcmxTKattge tair10 tleafn10 / vcmxTKattge tair10 tleafd10 * vcmax
      !resp = lunaCv * leaf_mr_vcm * (vcmax * hourpd + vcmaxNight * (24.0 - hourpd))
      !net = jmax / fj
      !ncb = vcmax / fc
      !nresp = resp / nuer

  in NitrogenInvestResult
     { nir_vcmax = vcmax
     , nir_jmax = jmax
     , nir_jmeanL = jmeanL
     , nir_jmaxL = jmaxL
     , nir_net = net
     , nir_ncb = ncb
     , nir_nresp = nresp
     , nir_psn = psn
     , nir_resp = resp
     , nir_kc = kc_out
     , nir_kj = kj_out
     , nir_ci = ci_out
     }

--------------------------------------------------------------------------------
-- Nitrogen allocation optimization
--------------------------------------------------------------------------------

-- | LUNA nitrogen partitioning optimization.
nitrogenAllocation :: Double -> Double -> Double -> Double -> Double -> Double -> Double
                   -> Double -> Double -> Double -> Double
                   -> Double -> Double -> Double -> Double -> Double -> Double -> Double
                   -> Double -> LunaParams -> NitrogenAllocResult
nitrogenAllocation !fnca !forc_pbot10 !relh10 !co2a10 !o2a10
                   !pari10 !parimx10 !rb10 !hourpd !tair10 !tleafd10 !tleafn10
                   !jmaxb0_val !jmaxb1_val !wc2wjb0_val
                   !pnlcOld !dayl_factor !o3coefjmax !relh_in !lp =
  let !ref = nueRef lp
      !nuejref0 = nue_j ref
      !nuecref0 = nue_c ref
      !kj2kcref0 = nue_ratio ref

      !nlc0 = min (pnlcOld * fnca) (0.5 * fnca)
      !chgPerStep = 0.02 * fnca
      !pari10c = max lunaPARLowLim pari10
      !parimx10c = max lunaPARLowLim parimx10
      !tleafd10c = min (max tleafd10 lunaTrange1) lunaTrange2
      !tleafn10c = min (max tleafn10 lunaTrange1) lunaTrange2
      !ci0 = 0.7 * co2a10
      !jmaxCoef = jmaxb1_val * dayl_factor * (1.0 - exp (negate (lp_relhExp lp) *
                  max (relh_in - lp_minrelh lp) 0.0 / (1.0 - lp_minrelh lp)))

      -- Simplified: run a fixed number of optimization steps
      optimLoop !n !nlc !kc !kj !ci !incFlag
        | n >= 100 = (nlc, kc, kj, ci)
        | otherwise =
          let !fc_v = vcmxTKattge tair10 tleafd10c * lunaFc25
              !fj_v = jmxTKattge tair10 tleafd10c * lunaFj25
              !nuer_v = lunaCv * lunaNUEr25 * (respTBernacchi tleafd10c * hourpd +
                        respTBernacchi tleafn10c * (24.0 - hourpd))
              !nue_res = nueCalc o2a10 ci tair10 tleafd10c lp
              !kj2kc = nue_ratio nue_res

              !inv = nitrogenInvestments 0 fnca nlc forc_pbot10 relh10 co2a10 o2a10
                     pari10c parimx10c rb10 hourpd tair10 tleafd10c tleafn10c
                     kj2kc jmaxCoef fc_v fj_v (nue_c nue_res) (nue_j nue_res)
                     nuecref0 nuejref0 nuer_v o3coefjmax jmaxb0_val wc2wjb0_val
                     kc kj ci lp

              !npsn = nlc + nir_ncb inv + nir_net inv
              !nstore = fnca - npsn - nir_nresp inv

              -- Try increase
              (!nlc', !done)
                | nstore > 0.0 && (incFlag || n == 0) =
                  let !nlc2 = min (nlc + chgPerStep) (0.95 * fnca)
                      !inv2 = nitrogenInvestments 1 fnca nlc2 forc_pbot10 relh10 co2a10 o2a10
                              pari10c parimx10c rb10 hourpd tair10 tleafd10c tleafn10c
                              kj2kc jmaxCoef fc_v fj_v (nue_c nue_res) (nue_j nue_res)
                              nuecref0 nuejref0 nuer_v o3coefjmax jmaxb0_val wc2wjb0_val
                              (nir_kc inv) (nir_kj inv) (nir_ci inv) lp
                      !npsn2 = nlc2 + nir_ncb inv2 + nir_net inv2
                      !cc2 = (npsn2 - npsn) * lunaNMCp25 * lunaCv *
                             (respTBernacchi tleafd10c * hourpd + respTBernacchi tleafn10c * (24.0 - hourpd))
                      !cg2 = nir_psn inv2 - nir_psn inv
                  in if cg2 > cc2 && (npsn2 + nir_nresp inv2 < 0.95 * fnca)
                     then (nlc2, False)
                     else (nlc, n > 0)
                | not incFlag =
                  let !nlc1 | nstore < 0.0 = max (nlc * 0.8) 0.05
                            | otherwise    = max (nlc - chgPerStep) 0.05
                      !inv1 = nitrogenInvestments 1 fnca nlc1 forc_pbot10 relh10 co2a10 o2a10
                              pari10c parimx10c rb10 hourpd tair10 tleafd10c tleafn10c
                              kj2kc jmaxCoef fc_v fj_v (nue_c nue_res) (nue_j nue_res)
                              nuecref0 nuejref0 nuer_v o3coefjmax jmaxb0_val wc2wjb0_val
                              (nir_kc inv) (nir_kj inv) (nir_ci inv) lp
                      !npsn1 = nlc1 + nir_ncb inv1 + nir_net inv1
                      !cc1 = (npsn - npsn1) * lunaNMCp25 * lunaCv *
                             (respTBernacchi tleafd10c * hourpd + respTBernacchi tleafn10c * (24.0 - hourpd))
                      !cg1 = nir_psn inv - nir_psn inv1
                  in if (cg1 < cc1 && nlc1 > 0.05) || (npsn + nir_nresp inv) > 0.95 * fnca
                     then (nlc1, False)
                     else (nlc, True)
                | otherwise = (nlc, True)

          in if done
             then (nlc', nir_kc inv, nir_kj inv, nir_ci inv)
             else optimLoop (n + 1) nlc' (nir_kc inv) (nir_kj inv) (nir_ci inv)
                            (incFlag || (n == 0 && nlc' > nlc))

      (!nlcFinal, !_kcF, !_kjF, !_ciF) = optimLoop 0 nlc0 0.0 0.0 ci0 False

      -- Recompute final allocations
      !fc_fin = vcmxTKattge tair10 tleafd10c * lunaFc25
      !fj_fin = jmxTKattge tair10 tleafd10c * lunaFj25
      !nuer_fin = lunaCv * lunaNUEr25 * (respTBernacchi tleafd10c * hourpd +
                  respTBernacchi tleafn10c * (24.0 - hourpd))
      !nue_fin = nueCalc o2a10 _ciF tair10 tleafd10c lp
      !inv_fin = nitrogenInvestments 0 fnca nlcFinal forc_pbot10 relh10 co2a10 o2a10
                 pari10c parimx10c rb10 hourpd tair10 tleafd10c tleafn10c
                 (nue_ratio nue_fin) jmaxCoef fc_fin fj_fin (nue_c nue_fin) (nue_j nue_fin)
                 nuecref0 nuejref0 nuer_fin o3coefjmax jmaxb0_val wc2wjb0_val
                 _kcF _kjF _ciF lp

      !npsn_fin = nlcFinal + nir_ncb inv_fin + nir_net inv_fin
      !nstore_fin = fnca - npsn_fin - nir_nresp inv_fin

  in NitrogenAllocResult
     { nar_pnstore = nstore_fin / fnca
     , nar_pnlc    = nlcFinal / fnca
     , nar_pnet    = nir_net inv_fin / fnca
     , nar_pnresp  = nir_nresp inv_fin / fnca
     , nar_pncb    = nir_ncb inv_fin / fnca
     }

-- | Check if it is time to run LUNA (end of current day).
isTimeToRunLuna :: Bool -> Bool
isTimeToRunLuna = id
