{-# LANGUAGE BangPatterns #-}
-- | Urban radiation module: solar and longwave radiation for urban landunit.
-- Fortran: UrbanRadiationMod.F90
--
-- Provides:
--   * Net longwave radiation with multiple reflections in urban canyon
--   * Solar fluxes absorbed and reflected by roof and canyon
--   * Upward longwave radiation for roof, walls, and roads
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.UrbanRadiation
  ( -- * Constants
    mpeUrbanRad
  , snoem
    -- * Input/output records
  , UrbanViewFactors(..)
  , NetLongwaveInput(..)
  , NetLongwaveResult(..)
  , UrbanRadiationInput(..)
  , UrbanRadiationResult(..)
    -- * Science functions
  , netLongwave
  , urbanRadiation
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (tfrz, sb)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Prevents overflow for division by zero
mpeUrbanRad :: Double
mpeUrbanRad = 1.0e-06

-- | Snow emissivity
snoem :: Double
snoem = 0.97

-- | Maximum iterations for longwave convergence
maxIterLW :: Int
maxIterLW = 50

-- | Special value (uninitialized)
spval :: Double
spval = 1.0e36

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Urban canyon view factors (landunit-level)
data UrbanViewFactors = UrbanViewFactors
  { vf_sr :: !Double  -- ^ sky to road
  , vf_wr :: !Double  -- ^ wall to road
  , vf_sw :: !Double  -- ^ sky to wall
  , vf_rw :: !Double  -- ^ road to wall
  , vf_ww :: !Double  -- ^ wall to wall
  } deriving (Show, Eq)

-- | Input for net longwave radiation (single landunit)
data NetLongwaveInput = NetLongwaveInput
  { nli_canyon_hwr   :: !Double  -- ^ building height / street width
  , nli_wtroad_perv  :: !Double  -- ^ weight of pervious road
  , nli_lwdown       :: !Double  -- ^ atmospheric longwave (W/m2)
  , nli_em_roof      :: !Double  -- ^ roof emissivity (with snow)
  , nli_em_improad   :: !Double  -- ^ impervious road emissivity (with snow)
  , nli_em_perroad   :: !Double  -- ^ pervious road emissivity (with snow)
  , nli_em_wall      :: !Double  -- ^ wall emissivity
  , nli_t_roof       :: !Double  -- ^ roof temperature (K)
  , nli_t_improad    :: !Double  -- ^ impervious road temperature (K)
  , nli_t_perroad    :: !Double  -- ^ pervious road temperature (K)
  , nli_t_sunwall    :: !Double  -- ^ sunlit wall temperature (K)
  , nli_t_shadewall  :: !Double  -- ^ shaded wall temperature (K)
  , nli_vf           :: !UrbanViewFactors
  } deriving (Show, Eq)

-- | Output from net longwave radiation (single landunit)
data NetLongwaveResult = NetLongwaveResult
  { nlr_lwnet_roof      :: !Double  -- ^ net LW roof (W/m2)
  , nlr_lwnet_improad   :: !Double  -- ^ net LW impervious road
  , nlr_lwnet_perroad   :: !Double  -- ^ net LW pervious road
  , nlr_lwnet_sunwall   :: !Double  -- ^ net LW sunlit wall
  , nlr_lwnet_shadewall :: !Double  -- ^ net LW shaded wall
  , nlr_lwnet_canyon    :: !Double  -- ^ net LW canyon total
  , nlr_lwup_roof       :: !Double  -- ^ upward LW roof
  , nlr_lwup_improad    :: !Double  -- ^ upward LW impervious road
  , nlr_lwup_perroad    :: !Double  -- ^ upward LW pervious road
  , nlr_lwup_sunwall    :: !Double  -- ^ upward LW sunlit wall
  , nlr_lwup_shadewall  :: !Double  -- ^ upward LW shaded wall
  , nlr_lwup_canyon     :: !Double  -- ^ upward LW canyon total
  } deriving (Show, Eq)

-- | Input for urban radiation (single landunit)
data UrbanRadiationInput = UrbanRadiationInput
  { uri_canyon_hwr    :: !Double  -- ^ building height / street width
  , uri_wtroad_perv   :: !Double  -- ^ pervious road weight
  , uri_lwdown        :: !Double  -- ^ atmospheric longwave (W/m2)
  , uri_em_roof       :: !Double  -- ^ base roof emissivity
  , uri_em_improad    :: !Double  -- ^ base impervious road emissivity
  , uri_em_perroad    :: !Double  -- ^ base pervious road emissivity
  , uri_em_wall       :: !Double  -- ^ wall emissivity
  , uri_t_roof        :: !Double  -- ^ roof ground temperature (K)
  , uri_t_improad     :: !Double  -- ^ impervious road ground temperature (K)
  , uri_t_perroad     :: !Double  -- ^ pervious road ground temperature (K)
  , uri_t_sunwall     :: !Double  -- ^ sunlit wall ground temperature (K)
  , uri_t_shadewall   :: !Double  -- ^ shaded wall ground temperature (K)
  , uri_frac_sno_roof    :: !Double  -- ^ snow fraction on roof
  , uri_frac_sno_improad :: !Double  -- ^ snow fraction on impervious road
  , uri_frac_sno_perroad :: !Double  -- ^ snow fraction on pervious road
  , uri_vf            :: !UrbanViewFactors
  , uri_forc_solad_vis :: !Double  -- ^ direct beam vis (W/m2)
  , uri_forc_solad_nir :: !Double  -- ^ direct beam nir
  , uri_forc_solai_vis :: !Double  -- ^ diffuse vis
  , uri_forc_solai_nir :: !Double  -- ^ diffuse nir
  -- Absorbed solar fractions (per unit incoming, from albedo module)
  , uri_sabs_roof_dir  :: !(Double, Double)  -- ^ (vis, nir) roof dir
  , uri_sabs_roof_dif  :: !(Double, Double)  -- ^ (vis, nir) roof dif
  , uri_sabs_sunwall_dir :: !(Double, Double)
  , uri_sabs_sunwall_dif :: !(Double, Double)
  , uri_sabs_shadewall_dir :: !(Double, Double)
  , uri_sabs_shadewall_dif :: !(Double, Double)
  , uri_sabs_improad_dir :: !(Double, Double)
  , uri_sabs_improad_dif :: !(Double, Double)
  , uri_sabs_perroad_dir :: !(Double, Double)
  , uri_sabs_perroad_dif :: !(Double, Double)
  } deriving (Show, Eq)

-- | Per-patch output from urban radiation
data UrbanRadiationResult = UrbanRadiationResult
  { urr_eflx_lwrad_out  :: !Double  -- ^ upward longwave (W/m2)
  , urr_eflx_lwrad_net  :: !Double  -- ^ net longwave (W/m2)
  , urr_sabg             :: !Double  -- ^ absorbed ground solar (W/m2)
  , urr_sabv             :: !Double  -- ^ absorbed vegetation solar (always 0 for urban)
  , urr_fsa              :: !Double  -- ^ total absorbed solar
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Net longwave radiation with multiple reflections
--------------------------------------------------------------------------------

-- | Net longwave radiation for road, walls, and roof in urban canyon.
-- Iterates to convergence (up to 50 iterations) for multiple reflections.
-- Ported from net_longwave in UrbanRadiationMod.F90.
netLongwave :: NetLongwaveInput -> NetLongwaveResult
netLongwave !inp =
  let !hwr     = nli_canyon_hwr inp
      !wtp     = nli_wtroad_perv inp
      !wti     = 1.0 - wtp
      !lwd     = nli_lwdown inp
      !emr     = nli_em_roof inp
      !emi     = nli_em_improad inp
      !emp     = nli_em_perroad inp
      !emw     = nli_em_wall inp
      !tr      = nli_t_roof inp
      !ti      = nli_t_improad inp
      !tp      = nli_t_perroad inp
      !tsw     = nli_t_sunwall inp
      !tsh     = nli_t_shadewall inp
      !vsr     = vf_sr (nli_vf inp)
      !vwr     = vf_wr (nli_vf inp)
      !vsw     = vf_sw (nli_vf inp)
      !vrw     = vf_rw (nli_vf inp)
      !vww     = vf_ww (nli_vf inp)

      -- Atmospheric LW incident on canyon surfaces
      !lwd_road = lwd * vsr
      !lwd_sw   = lwd * vsw
      !lwd_sh   = lwd * vsw

      -- Initial absorption, reflection, emission for impervious road
      !imp_a0 = emi * lwd_road
      !imp_r0 = (1.0 - emi) * lwd_road
      !imp_e0 = emi * sb * ti ** 4

      -- Initial for pervious road
      !per_a0 = emp * lwd_road
      !per_r0 = (1.0 - emp) * lwd_road
      !per_e0 = emp * sb * tp ** 4

      -- Combined road
      !road_r0 = imp_r0 * wti + per_r0 * wtp
      !road_e0 = imp_e0 * wti + per_e0 * wtp

      -- Initial for sunwall
      !sw_a0 = emw * lwd_sw
      !sw_r0 = (1.0 - emw) * lwd_sw
      !sw_e0 = emw * sb * tsw ** 4

      -- Initial for shadewall
      !sh_a0 = emw * lwd_sh
      !sh_r0 = (1.0 - emw) * lwd_sh
      !sh_e0 = emw * sb * tsh ** 4

      -- Initial net and upward
      !lwnet_imp0 = imp_e0 - imp_a0
      !lwnet_per0 = per_e0 - per_a0
      !lwnet_sw0  = sw_e0  - sw_a0
      !lwnet_sh0  = sh_e0  - sh_a0

      !lwup_imp0 = imp_r0 * vsr + imp_e0 * vsr
      !lwup_per0 = per_r0 * vsr + per_e0 * vsr
      !lwup_sw0  = sw_r0  * vsw + sw_e0  * vsw
      !lwup_sh0  = sh_r0  * vsw + sh_e0  * vsw

      -- Iterate multiple reflections
      iterate !n !lwni !lwnp !lwnsw !lwnsh
              !lupi !lupp !lupsw !lupsh
              !rd_r_sw !rd_e_sw !rd_r_sh !rd_e_sh
              !sw_r_rd !sw_e_rd !sw_r_sh2 !sw_e_sh2
              !sh_r_rd !sh_e_rd !sh_r_sw2 !sh_e_sw2
        | n >= maxIterLW = (lwni, lwnp, lwnsw, lwnsh, lupi, lupp, lupsw, lupsh)
        | otherwise =
          let -- Step 1: absorption and reflection
              !lwtot_rd = (sw_r_rd + sw_e_rd + sh_r_rd + sh_e_rd) * hwr
              !imp_a = emi * lwtot_rd
              !imp_r = (1.0 - emi) * lwtot_rd
              !per_a = emp * lwtot_rd
              !per_r = (1.0 - emp) * lwtot_rd
              !road_a_new = imp_a * wti + per_a * wtp
              !road_r_new = imp_r * wti + per_r * wtp

              !lwtot_sw = (rd_r_sw + rd_e_sw) / hwr + (sh_r_sw2 + sh_e_sw2)
              !sw_a_new  = emw * lwtot_sw
              !sw_r_new  = (1.0 - emw) * lwtot_sw

              !lwtot_sh = (rd_r_sh + rd_e_sh) / hwr + (sw_r_sh2 + sw_e_sh2)
              !sh_a_new  = emw * lwtot_sh
              !sh_r_new  = (1.0 - emw) * lwtot_sh

              -- Step 2: accumulate net
              !lwni' = lwni - imp_a
              !lwnp' = lwnp - per_a
              !lwnsw' = lwnsw - sw_a_new
              !lwnsh' = lwnsh - sh_a_new

              -- Step 3: distribute reflected
              !imp_r_sky = imp_r * vsr
              !per_r_sky = per_r * vsr
              !sw_r_sky  = sw_r_new * vsw
              !sh_r_sky  = sh_r_new * vsw

              -- Step 4: accumulate upward
              !lupi' = lupi + imp_r_sky
              !lupp' = lupp + per_r_sky
              !lupsw' = lupsw + sw_r_sky
              !lupsh' = lupsh + sh_r_sky

              -- Step 5: convergence
              !crit = max road_a_new (max sw_a_new sh_a_new)

              -- New reflected distribution for next iteration
              !rd_r_sw' = road_r_new * vwr
              !rd_e_sw' = 0.0  -- emission zeroed after first iter
              !rd_r_sh' = road_r_new * vwr
              !rd_e_sh' = 0.0
              !sw_r_rd' = sw_r_new * vrw
              !sw_e_rd' = 0.0
              !sw_r_sh2' = sw_r_new * vww
              !sw_e_sh2' = 0.0
              !sh_r_rd' = sh_r_new * vrw
              !sh_e_rd' = 0.0
              !sh_r_sw2' = sh_r_new * vww
              !sh_e_sw2' = 0.0
          in if crit < 0.001
             then (lwni', lwnp', lwnsw', lwnsh', lupi', lupp', lupsw', lupsh')
             else iterate (n + 1) lwni' lwnp' lwnsw' lwnsh'
                          lupi' lupp' lupsw' lupsh'
                          rd_r_sw' rd_e_sw' rd_r_sh' rd_e_sh'
                          sw_r_rd' sw_e_rd' sw_r_sh2' sw_e_sh2'
                          sh_r_rd' sh_e_rd' sh_r_sw2' sh_e_sw2'

      -- Initial reflected distributions
      !rd_r_sw_0 = road_r0 * vwr
      !rd_e_sw_0 = road_e0 * vwr
      !rd_r_sh_0 = road_r0 * vwr
      !rd_e_sh_0 = road_e0 * vwr
      !sw_r_rd_0 = sw_r0 * vrw
      !sw_e_rd_0 = sw_e0 * vrw
      !sw_r_sh_0 = sw_r0 * vww
      !sw_e_sh_0 = sw_e0 * vww
      !sh_r_rd_0 = sh_r0 * vrw
      !sh_e_rd_0 = sh_e0 * vrw
      !sh_r_sw_0 = sh_r0 * vww
      !sh_e_sw_0 = sh_e0 * vww

      (!lwni_f, !lwnp_f, !lwnsw_f, !lwnsh_f,
       !lupi_f, !lupp_f, !lupsw_f, !lupsh_f) =
        iterate 0 lwnet_imp0 lwnet_per0 lwnet_sw0 lwnet_sh0
                  lwup_imp0 lwup_per0 lwup_sw0 lwup_sh0
                  rd_r_sw_0 rd_e_sw_0 rd_r_sh_0 rd_e_sh_0
                  sw_r_rd_0 sw_e_rd_0 sw_r_sh_0 sw_e_sh_0
                  sh_r_rd_0 sh_e_rd_0 sh_r_sw_0 sh_e_sw_0

      -- Total canyon
      !lwnet_canyon = lwni_f * wti + lwnp_f * wtp +
                      (lwnsw_f + lwnsh_f) * hwr
      !lwup_canyon  = lupi_f * wti + lupp_f * wtp +
                      (lupsw_f + lupsh_f) * hwr

      -- Roof longwave
      !lwup_roof  = emr * sb * tr ** 4 + (1.0 - emr) * lwd
      !lwnet_roof = lwup_roof - lwd

  in NetLongwaveResult
     { nlr_lwnet_roof      = lwnet_roof
     , nlr_lwnet_improad   = lwni_f
     , nlr_lwnet_perroad   = lwnp_f
     , nlr_lwnet_sunwall   = lwnsw_f
     , nlr_lwnet_shadewall = lwnsh_f
     , nlr_lwnet_canyon    = lwnet_canyon
     , nlr_lwup_roof       = lwup_roof
     , nlr_lwup_improad    = lupi_f
     , nlr_lwup_perroad    = lupp_f
     , nlr_lwup_sunwall    = lupsw_f
     , nlr_lwup_shadewall  = lupsh_f
     , nlr_lwup_canyon     = lwup_canyon
     }

--------------------------------------------------------------------------------
-- Urban radiation (combined solar + longwave)
--------------------------------------------------------------------------------

-- | Compute urban radiation output for a single patch given its column type.
-- Combines solar absorbed fractions with net longwave from netLongwave.
-- Ported from UrbanRadiation in UrbanRadiationMod.F90.
urbanRadiation :: UrbanRadiationInput -> NetLongwaveResult -> Int -> UrbanRadiationResult
urbanRadiation !inp !nlr !icol_type =
  let !solad_vis = uri_forc_solad_vis inp
      !solad_nir = uri_forc_solad_nir inp
      !solai_vis = uri_forc_solai_vis inp
      !solai_nir = uri_forc_solai_nir inp
      -- Column type constants (Fortran: ICOL_ROOF=71, SUNWALL=72, etc.)
      icolRoof     = 71
      icolSunwall  = 72
      icolShadewall = 73
      icolRoadPerv  = 74
      icolRoadImperv = 75

      (!lwup, !lwnet, !sabg)
        | icol_type == icolRoof =
          let (sv, sn) = uri_sabs_roof_dir inp
              (dv, dn) = uri_sabs_roof_dif inp
              s = sv * solad_vis + dv * solai_vis + sn * solad_nir + dn * solai_nir
          in (nlr_lwup_roof nlr, nlr_lwnet_roof nlr, s)
        | icol_type == icolSunwall =
          let (sv, sn) = uri_sabs_sunwall_dir inp
              (dv, dn) = uri_sabs_sunwall_dif inp
              s = sv * solad_vis + dv * solai_vis + sn * solad_nir + dn * solai_nir
          in (nlr_lwup_sunwall nlr, nlr_lwnet_sunwall nlr, s)
        | icol_type == icolShadewall =
          let (sv, sn) = uri_sabs_shadewall_dir inp
              (dv, dn) = uri_sabs_shadewall_dif inp
              s = sv * solad_vis + dv * solai_vis + sn * solad_nir + dn * solai_nir
          in (nlr_lwup_shadewall nlr, nlr_lwnet_shadewall nlr, s)
        | icol_type == icolRoadPerv =
          let (sv, sn) = uri_sabs_perroad_dir inp
              (dv, dn) = uri_sabs_perroad_dif inp
              s = sv * solad_vis + dv * solai_vis + sn * solad_nir + dn * solai_nir
          in (nlr_lwup_perroad nlr, nlr_lwnet_perroad nlr, s)
        | icol_type == icolRoadImperv =
          let (sv, sn) = uri_sabs_improad_dir inp
              (dv, dn) = uri_sabs_improad_dif inp
              s = sv * solad_vis + dv * solai_vis + sn * solad_nir + dn * solai_nir
          in (nlr_lwup_improad nlr, nlr_lwnet_improad nlr, s)
        | otherwise = (0.0, 0.0, 0.0)

      !sabv = 0.0
      !fsa  = sabv + sabg

  in UrbanRadiationResult
     { urr_eflx_lwrad_out = lwup
     , urr_eflx_lwrad_net = lwnet
     , urr_sabg           = sabg
     , urr_sabv           = sabv
     , urr_fsa            = fsa
     }
