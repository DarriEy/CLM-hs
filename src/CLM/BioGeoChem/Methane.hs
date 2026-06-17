{-# LANGUAGE BangPatterns #-}
-- | Methane biogeochemistry module.
-- Fortran: ch4Mod.F90
-- Julia:   src/biogeochem/methane.jl
--
-- Full CH4 model: production, oxidation, transport (diffusion, ebullition,
-- plant-mediated), and surface flux calculation.
module CLM.BioGeoChem.Methane
  ( -- * Data types
    CH4VarCon(..)
  , defaultCH4VarCon
  , CH4Params(..)
  , defaultCH4Params
  , CH4ColumnInput(..)
  , CH4LayerState(..)
  , CH4ColumnResult(..)
    -- * Constants
  , rgasLatm
  , henry_ch4_kh
  , diff_ch4_air
  , diff_ch4_wat
  , diff_o2_air
  , diff_o2_wat
    -- * Production
  , ch4ProdQ10Factor
  , ch4Production
    -- * Oxidation
  , ch4OxidRate
  , ch4OxidationSaturated
  , ch4OxidationUnsaturated
    -- * Transport
  , ch4Diffusivity
  , ch4EbullitionCheck
  , ch4Ebullition
  , ch4PlantTransport
  , ch4AerenchymaFlux
    -- * Main driver
  , ch4Driver
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Constants
-- =========================================================================

rgasLatm :: Double
rgasLatm = 0.0821

tfrz :: Double
tfrz = 273.15

-- Henry's law constant for CH4 at 298K (mol/L/atm)
henry_ch4_kh :: Double
henry_ch4_kh = 1.3e-3

-- Diffusion coefficients (m2/s)
diff_ch4_air :: Double
diff_ch4_air = 1.952e-5

diff_ch4_wat :: Double
diff_ch4_wat = 1.5e-9

diff_o2_air :: Double
diff_o2_air = 2.01e-5

diff_o2_wat :: Double
diff_o2_wat = 2.4e-9

-- Molecular weights
mw_ch4 :: Double
mw_ch4 = 16.04e-3

-- =========================================================================
-- CH4VarCon — Methane control flags
-- =========================================================================

data CH4VarCon = CH4VarCon
  { ch4vc_allowlakeprod       :: !Bool
  , ch4vc_replenishlakec      :: !Bool
  , ch4vc_anoxicmicrosites    :: !Bool
  , ch4vc_usephfact           :: !Bool
  , ch4vc_ch4rmcnlim          :: !Bool
  , ch4vc_ch4offline          :: !Bool
  , ch4vc_transpirationloss   :: !Bool
  , ch4vc_use_aereoxid_prog   :: !Bool
  , ch4vc_ch4frzout           :: !Bool
  , ch4vc_finundation_mtd_h2osfc :: !Int
  } deriving (Show)

defaultCH4VarCon :: CH4VarCon
defaultCH4VarCon = CH4VarCon
  { ch4vc_allowlakeprod = False
  , ch4vc_replenishlakec = True
  , ch4vc_anoxicmicrosites = True
  , ch4vc_usephfact = False
  , ch4vc_ch4rmcnlim = True
  , ch4vc_ch4offline = True
  , ch4vc_transpirationloss = True
  , ch4vc_use_aereoxid_prog = False
  , ch4vc_ch4frzout = False
  , ch4vc_finundation_mtd_h2osfc = 1
  }

-- =========================================================================
-- CH4Params — Methane model parameters
-- =========================================================================

data CH4Params = CH4Params
  { ch4p_q10ch4              :: !Double
  , ch4p_q10ch4base          :: !Double
  , ch4p_f_ch4               :: !Double
  , ch4p_rootlitfrac         :: !Double
  , ch4p_cnscalefactor       :: !Double
  , ch4p_redoxlag            :: !Double
  , ch4p_lake_decomp_fact    :: !Double
  , ch4p_redoxlag_vertical   :: !Double
  , ch4p_pHmax               :: !Double
  , ch4p_pHmin               :: !Double
  , ch4p_oxinhib             :: !Double
  , ch4p_vmax_ch4_oxid       :: !Double
  , ch4p_k_m                 :: !Double
  , ch4p_q10_ch4oxid         :: !Double
  , ch4p_smp_crit            :: !Double
  , ch4p_k_m_o2              :: !Double
  , ch4p_k_m_unsat           :: !Double
  , ch4p_vmax_oxid_unsat     :: !Double
  , ch4p_aereoxid            :: !Double
  , ch4p_scale_factor_aere   :: !Double
  , ch4p_nongrassporosratio  :: !Double
  , ch4p_unsat_aere_ratio    :: !Double
  , ch4p_porosmin            :: !Double
  , ch4p_vgc_max             :: !Double
  , ch4p_satpow              :: !Double
  , ch4p_scale_factor_gasdiff :: !Double
  , ch4p_scale_factor_liqdiff :: !Double
  , ch4p_capthick            :: !Double
  , ch4p_f_sat               :: !Double
  , ch4p_qflxlagd            :: !Double
  , ch4p_highlatfact         :: !Double
  , ch4p_q10lakebase         :: !Double
  , ch4p_atmch4              :: !Double
  , ch4p_rob                 :: !Double
  , ch4p_om_frac_sf          :: !Double
  } deriving (Show)

defaultCH4Params :: CH4Params
defaultCH4Params = CH4Params
  { ch4p_q10ch4 = 1.5, ch4p_q10ch4base = 295.0, ch4p_f_ch4 = 0.2
  , ch4p_rootlitfrac = 0.5, ch4p_cnscalefactor = 1.0, ch4p_redoxlag = 30.0
  , ch4p_lake_decomp_fact = 2.0e-8, ch4p_redoxlag_vertical = 30.0
  , ch4p_pHmax = 9.0, ch4p_pHmin = 2.2, ch4p_oxinhib = 10.0
  , ch4p_vmax_ch4_oxid = 0.0125, ch4p_k_m = 5.0e-3, ch4p_q10_ch4oxid = 1.9
  , ch4p_smp_crit = -2.4e5, ch4p_k_m_o2 = 2.0e-2, ch4p_k_m_unsat = 5.0e-3
  , ch4p_vmax_oxid_unsat = 1.25e-3
  , ch4p_aereoxid = 0.5, ch4p_scale_factor_aere = 1.0
  , ch4p_nongrassporosratio = 0.33, ch4p_unsat_aere_ratio = 0.167
  , ch4p_porosmin = 0.05, ch4p_vgc_max = 0.15
  , ch4p_satpow = 2.0, ch4p_scale_factor_gasdiff = 1.0
  , ch4p_scale_factor_liqdiff = 1.0, ch4p_capthick = 100.0
  , ch4p_f_sat = 0.95, ch4p_qflxlagd = 30.0, ch4p_highlatfact = 2.0
  , ch4p_q10lakebase = 298.0, ch4p_atmch4 = 1.7e-6, ch4p_rob = 3.0
  , ch4p_om_frac_sf = 1.0
  }

-- =========================================================================
-- Column-level input for CH4 driver
-- =========================================================================

data CH4ColumnInput = CH4ColumnInput
  { ch4i_nlevgrnd         :: !Int
  , ch4i_zwt              :: !Double       -- ^ water table depth (m)
  , ch4i_finundated       :: !Double       -- ^ fractional inundation
  , ch4i_hr_vr            :: !(VU.Vector Double) -- ^ heterotrophic respiration per layer
  , ch4i_t_soisno         :: !(VU.Vector Double) -- ^ soil temperature (K)
  , ch4i_h2osoi_vol       :: !(VU.Vector Double) -- ^ volumetric soil water
  , ch4i_watsat           :: !(VU.Vector Double) -- ^ porosity
  , ch4i_dz              :: !(VU.Vector Double) -- ^ layer thickness (m)
  , ch4i_z               :: !(VU.Vector Double) -- ^ layer midpoint (m)
  , ch4i_rootfr          :: !(VU.Vector Double) -- ^ root fraction per layer
  , ch4i_pH              :: !Double       -- ^ soil pH
  , ch4i_atm_ch4         :: !Double       -- ^ atmospheric CH4 (mol/mol)
  , ch4i_patm            :: !Double       -- ^ atmospheric pressure (Pa)
  , ch4i_dt              :: !Double       -- ^ timestep (s)
  } deriving (Show)

-- | Per-layer CH4 state.
data CH4LayerState = CH4LayerState
  { ch4s_conc_ch4       :: !Double  -- ^ dissolved CH4 (mol/m3)
  , ch4s_conc_o2        :: !Double  -- ^ dissolved O2 (mol/m3)
  } deriving (Show, Eq)

-- | Column-level CH4 result.
data CH4ColumnResult = CH4ColumnResult
  { ch4r_ch4_surf_flux     :: !Double  -- ^ surface CH4 flux (mol/m2/s)
  , ch4r_ch4_prod_tot      :: !Double  -- ^ total production (mol/m2/s)
  , ch4r_ch4_oxid_tot      :: !Double  -- ^ total oxidation (mol/m2/s)
  , ch4r_ch4_ebul_tot      :: !Double  -- ^ total ebullition (mol/m2/s)
  , ch4r_ch4_aere_tot      :: !Double  -- ^ total aerenchyma flux (mol/m2/s)
  , ch4r_conc_ch4          :: !(VU.Vector Double) -- ^ updated CH4 per layer
  , ch4r_conc_o2           :: !(VU.Vector Double) -- ^ updated O2 per layer
  } deriving (Show)

-- =========================================================================
-- Production
-- =========================================================================

ch4ProdQ10Factor :: Double -> Double -> Double -> Double
ch4ProdQ10Factor q10 base t = q10 ** ((t - base) / 10.0)

-- | CH4 production for a single layer (mol CH4 / m3 / s).
ch4Production :: CH4Params
              -> CH4VarCon
              -> Double  -- ^ hr_vr (gC/m3/s heterotrophic respiration)
              -> Double  -- ^ temperature (K)
              -> Double  -- ^ pH
              -> Double  -- ^ finundated
              -> Bool    -- ^ below water table?
              -> Double
ch4Production !params !varcon !hr !temp !pH !finund !belowWT =
  let !f_ch4 = ch4p_f_ch4 params
      !q10 = ch4ProdQ10Factor (ch4p_q10ch4 params) (ch4p_q10ch4base params) temp
      -- pH factor (triangular function)
      !pHfact = if ch4vc_usephfact varcon
                then let !pHopt = 7.0
                         !range = ch4p_pHmax params - ch4p_pHmin params
                     in max 0.0 $ min 1.0 $
                        if pH < pHopt
                        then (pH - ch4p_pHmin params) / (pHopt - ch4p_pHmin params)
                        else (ch4p_pHmax params - pH) / (ch4p_pHmax params - pHopt)
                else 1.0
      -- Base production from HR
      !base_prod = hr * f_ch4 * q10 * pHfact / 12.0  -- gC -> mol C
      -- Scale by inundation/saturation
      !scale = if belowWT then 1.0
               else if ch4vc_anoxicmicrosites varcon then finund * 0.2
               else 0.0
  in max 0.0 (base_prod * scale)

-- =========================================================================
-- Oxidation
-- =========================================================================

ch4OxidRate :: Double -> Double -> Double -> Double -> Double -> Double
ch4OxidRate vmax conc_ch4 k_m conc_o2 k_m_o2 =
  vmax * conc_ch4 / (k_m + conc_ch4) * conc_o2 / (k_m_o2 + conc_o2)

-- | Saturated-zone CH4 oxidation (Michaelis-Menten).
ch4OxidationSaturated :: CH4Params -> Double -> Double -> Double -> Double -> Double
ch4OxidationSaturated !params !conc_ch4 !conc_o2 !temp !dz =
  let !q10 = (ch4p_q10_ch4oxid params) ** ((temp - tfrz - 25.0) / 10.0)
      !vmax = ch4p_vmax_ch4_oxid params * q10
      !rate = ch4OxidRate vmax conc_ch4 (ch4p_k_m params) conc_o2 (ch4p_k_m_o2 params)
  in rate * dz

-- | Unsaturated-zone CH4 oxidation.
ch4OxidationUnsaturated :: CH4Params -> Double -> Double -> Double -> Double -> Double
ch4OxidationUnsaturated !params !conc_ch4 !temp !h2osoi_vol !dz =
  let !q10 = (ch4p_q10_ch4oxid params) ** ((temp - tfrz - 25.0) / 10.0)
      !vmax = ch4p_vmax_oxid_unsat params * q10
      !rate = vmax * conc_ch4 / (ch4p_k_m_unsat params + conc_ch4)
      !moisture_fac = min 1.0 (h2osoi_vol / 0.3)
  in rate * dz * moisture_fac

-- =========================================================================
-- Transport
-- =========================================================================

-- | Effective diffusivity in soil (Millington-Quirk model).
ch4Diffusivity :: CH4Params
               -> Double  -- ^ porosity (watsat)
               -> Double  -- ^ h2osoi_vol
               -> Double  -- ^ temperature (K)
               -> (Double, Double)  -- ^ (D_ch4, D_o2) effective diffusivities
ch4Diffusivity !params !watsat !h2osoi_vol !temp =
  let !air_frac = max 0.0 (watsat - h2osoi_vol)
      !liq_frac = min watsat h2osoi_vol
      -- Temperature correction (T/273.15)^1.75
      !t_corr = (temp / tfrz) ** 1.75
      -- Millington-Quirk: D_eff = D_free * (air^(10/3) / por^2) for gas
      !tort_gas = if watsat > 0.0
                  then air_frac ** (10.0/3.0) / (watsat * watsat)
                  else 0.0
      !tort_liq = if watsat > 0.0
                  then liq_frac ** (10.0/3.0) / (watsat * watsat)
                  else 0.0
      !d_ch4_gas = diff_ch4_air * t_corr * tort_gas * ch4p_scale_factor_gasdiff params
      !d_ch4_liq = diff_ch4_wat * tort_liq * ch4p_scale_factor_liqdiff params
      !d_o2_gas = diff_o2_air * t_corr * tort_gas * ch4p_scale_factor_gasdiff params
      !d_o2_liq = diff_o2_wat * tort_liq * ch4p_scale_factor_liqdiff params
  in (d_ch4_gas + d_ch4_liq, d_o2_gas + d_o2_liq)

-- | Check ebullition threshold.
ch4EbullitionCheck :: Double -> Double -> Double -> Bool
ch4EbullitionCheck conc_ch4 vgc_max t =
  let !pCH4 = conc_ch4 * rgasLatm * t
  in pCH4 > vgc_max

-- | Compute ebullition flux (removes excess dissolved CH4).
ch4Ebullition :: CH4Params -> Double -> Double -> Double -> Double -> (Double, Double)
ch4Ebullition !params !conc_ch4 !temp !dz !dt =
  let !pCH4 = conc_ch4 * rgasLatm * temp
      !vgc = ch4p_vgc_max params
  in if pCH4 > vgc
     then let !conc_eq = vgc / (rgasLatm * temp)
              !excess = (conc_ch4 - conc_eq) * dz
              !flux = excess / dt
          in (conc_eq, flux)
     else (conc_ch4, 0.0)

-- | Plant-mediated transport (aerenchyma).
ch4PlantTransport :: CH4Params -> Double -> Double -> Double -> Double -> Double -> Double
ch4PlantTransport !params !conc_ch4 !rootfr !atm_ch4_conc !temp !dz =
  let !scale = ch4p_scale_factor_aere params
      !poros_ratio = ch4p_nongrassporosratio params
      !gradient = conc_ch4 - atm_ch4_conc
      !transport_rate = scale * poros_ratio * rootfr
  in gradient * transport_rate * dz

-- | Aerenchyma CH4 flux with oxidation fraction.
ch4AerenchymaFlux :: CH4Params -> Double -> Double -> Double
ch4AerenchymaFlux !params !transport !_temp =
  let !oxid_frac = ch4p_aereoxid params
  in transport * (1.0 - oxid_frac)

-- =========================================================================
-- Main CH4 Driver
-- =========================================================================

-- | Main CH4 driver for a single column.
-- Computes production, oxidation, transport, and surface flux.
ch4Driver :: CH4Params -> CH4VarCon -> CH4ColumnInput
          -> [CH4LayerState] -> CH4ColumnResult
ch4Driver !params !varcon !input !states =
  let !nlev = ch4i_nlevgrnd input
      !dt = ch4i_dt input
      !zwt = ch4i_zwt input
      !finund = ch4i_finundated input
      !atm_conc = ch4i_atm_ch4 input * ch4i_patm input / (8.314 * 288.0)

      -- Per-layer computation
      processLayer j =
        let !temp = ch4i_t_soisno input VU.! j
            !h2o = ch4i_h2osoi_vol input VU.! j
            !watsat = ch4i_watsat input VU.! j
            !dz = ch4i_dz input VU.! j
            !z_mid = ch4i_z input VU.! j
            !hr = ch4i_hr_vr input VU.! j
            !rootfr = ch4i_rootfr input VU.! j
            !st = states !! j
            !conc_ch4 = ch4s_conc_ch4 st
            !conc_o2 = ch4s_conc_o2 st
            !belowWT = z_mid >= zwt
            -- Production
            !prod = if temp > tfrz
                    then ch4Production params varcon hr temp (ch4i_pH input) finund belowWT
                    else 0.0
            -- Oxidation
            !oxid = if belowWT
                    then ch4OxidationSaturated params conc_ch4 conc_o2 temp dz
                    else ch4OxidationUnsaturated params conc_ch4 temp h2o dz
            -- Ebullition
            (!conc_post_ebul, !ebul_flux) =
              if belowWT
              then ch4Ebullition params conc_ch4 temp dz dt
              else (conc_ch4, 0.0)
            -- Plant transport
            !plant_trans = ch4PlantTransport params conc_post_ebul rootfr atm_conc temp dz
            !aere_flux = ch4AerenchymaFlux params plant_trans temp
            -- Diffusion (simplified explicit)
            (!d_ch4, _d_o2) = ch4Diffusivity params watsat h2o temp
            !diff_flux = if j == 0
                         then d_ch4 * (conc_post_ebul - atm_conc) / (z_mid + 0.01)
                         else 0.0
            -- Update concentration
            !net_source = prod - oxid
            !new_conc = max 0.0 (conc_post_ebul + (net_source - aere_flux / dz) * dt)
            !new_o2 = max 0.0 (conc_o2 - oxid * 2.0 * dt / dz)
        in (new_conc, new_o2, prod * dz, oxid, ebul_flux, aere_flux, diff_flux)

      !results = map processLayer [0 .. nlev - 1]

      !new_ch4 = VU.fromList $ map (\(c,_,_,_,_,_,_) -> c) results
      !new_o2 = VU.fromList $ map (\(_,o,_,_,_,_,_) -> o) results
      !tot_prod = sum $ map (\(_,_,p,_,_,_,_) -> p) results
      !tot_oxid = sum $ map (\(_,_,_,ox,_,_,_) -> ox) results
      !tot_ebul = sum $ map (\(_,_,_,_,e,_,_) -> e) results
      !tot_aere = sum $ map (\(_,_,_,_,_,a,_) -> a) results
      !surf_diff = if null results then 0.0
                   else let (_,_,_,_,_,_,d) = head results in d
      !surf_flux = surf_diff + tot_ebul + tot_aere

  in CH4ColumnResult
     { ch4r_ch4_surf_flux = surf_flux
     , ch4r_ch4_prod_tot = tot_prod
     , ch4r_ch4_oxid_tot = tot_oxid
     , ch4r_ch4_ebul_tot = tot_ebul
     , ch4r_ch4_aere_tot = tot_aere
     , ch4r_conc_ch4 = new_ch4
     , ch4r_conc_o2 = new_o2
     }

-- Note: CH4LayerState uses boxed Vector (Data.Vector) since it lacks
-- an Unbox instance. The driver uses list-based processing internally.
