{-# LANGUAGE BangPatterns #-}
-- | Urban albedo module: solar radiation and albedos for urban landunit.
-- Fortran: UrbanAlbedoMod.F90
--
-- Provides:
--   * Urban snow albedos
--   * Direct beam incident radiation on canyon surfaces
--   * Diffuse incident radiation on canyon surfaces
--   * Net solar radiation with multiple reflections
--   * Combined albedo with snow effects
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.UrbanAlbedo
  ( -- * Constants
    snal0
  , snal1
    -- * Types
  , UrbanViewFactors(..)
  , IncidentDirectResult(..)
  , IncidentDiffuseResult(..)
  , NetSolarInput(..)
  , NetSolarResult(..)
  , UrbanAlbedoResult(..)
    -- * Science functions
  , snowAlbedo
  , incidentDirect
  , incidentDiffuse
  , netSolar
  , combineSnowAlbedo
  , urbanAlbedo
  ) where

import CLM.BioGeoPhys.UrbanRadiation (UrbanViewFactors(..))

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Visible albedo of urban snow
snal0 :: Double
snal0 = 0.66

-- | NIR albedo of urban snow
snal1 :: Double
snal1 = 0.56

-- | Pi
rpi :: Double
rpi = 3.14159265358979323846

-- | Number of radiation bands (vis, nir)
numrad :: Int
numrad = 2

-- | Max iterations for multiple reflection convergence
maxIterSolar :: Int
maxIterSolar = 50

-- | Error criterion for convergence
errcrit :: Double
errcrit = 0.00001

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Direct beam incident radiation result (per band)
data IncidentDirectResult = IncidentDirectResult
  { idr_sdir_road      :: !Double
  , idr_sdir_sunwall   :: !Double
  , idr_sdir_shadewall :: !Double
  } deriving (Show, Eq)

-- | Diffuse incident radiation result (per band)
data IncidentDiffuseResult = IncidentDiffuseResult
  { idf_sdif_road      :: !Double
  , idf_sdif_sunwall   :: !Double
  , idf_sdif_shadewall :: !Double
  } deriving (Show, Eq)

-- | Input for net solar radiation calculation (per band)
data NetSolarInput = NetSolarInput
  { nsi_canyon_hwr     :: !Double
  , nsi_wtroad_perv    :: !Double
  , nsi_alb_improad    :: !Double  -- ^ direct or diffuse albedo
  , nsi_alb_perroad    :: !Double
  , nsi_alb_wall       :: !Double
  , nsi_sdir_road      :: !Double
  , nsi_sdir_sunwall   :: !Double
  , nsi_sdir_shadewall :: !Double
  , nsi_vf             :: !UrbanViewFactors
  } deriving (Show, Eq)

-- | Result of net solar radiation with multiple reflections (per band, dir or dif)
data NetSolarResult = NetSolarResult
  { nsr_sabs_improad   :: !Double
  , nsr_sabs_perroad   :: !Double
  , nsr_sabs_sunwall   :: !Double
  , nsr_sabs_shadewall :: !Double
  , nsr_sref_improad   :: !Double
  , nsr_sref_perroad   :: !Double
  , nsr_sref_sunwall   :: !Double
  , nsr_sref_shadewall :: !Double
  } deriving (Show, Eq)

-- | Full urban albedo result (all bands combined)
data UrbanAlbedoResult = UrbanAlbedoResult
  { uar_albgrd_vis :: !Double  -- ^ ground direct albedo vis
  , uar_albgrd_nir :: !Double  -- ^ ground direct albedo nir
  , uar_albgri_vis :: !Double  -- ^ ground diffuse albedo vis
  , uar_albgri_nir :: !Double  -- ^ ground diffuse albedo nir
  , uar_sabs_roof_dir  :: !(Double, Double)  -- ^ absorbed roof dir (vis, nir)
  , uar_sabs_roof_dif  :: !(Double, Double)  -- ^ absorbed roof dif (vis, nir)
  , uar_sabs_sunwall_dir :: !(Double, Double)
  , uar_sabs_sunwall_dif :: !(Double, Double)
  , uar_sabs_shadewall_dir :: !(Double, Double)
  , uar_sabs_shadewall_dif :: !(Double, Double)
  , uar_sabs_improad_dir :: !(Double, Double)
  , uar_sabs_improad_dif :: !(Double, Double)
  , uar_sabs_perroad_dir :: !(Double, Double)
  , uar_sabs_perroad_dif :: !(Double, Double)
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Snow albedo
--------------------------------------------------------------------------------

-- | Determine urban snow albedo for a surface.
-- Returns (albedo_vis, albedo_nir). If coszen > 0 and h2osno > 0, returns
-- snow albedos; otherwise returns (0, 0).
snowAlbedo :: Double  -- ^ coszen
           -> Double  -- ^ h2osno_total (mm)
           -> (Double, Double)
snowAlbedo !coszen !h2osno
  | coszen > 0.0 && h2osno > 0.0 = (snal0, snal1)
  | otherwise                     = (0.0, 0.0)

--------------------------------------------------------------------------------
-- Incident direct beam radiation
--------------------------------------------------------------------------------

-- | Direct beam solar radiation incident on walls and road.
-- Uses the Masson (2000) geometric model.
incidentDirect :: Double  -- ^ canyon_hwr
               -> Double  -- ^ coszen
               -> Double  -- ^ zen (zenith angle, radians)
               -> Double  -- ^ sdir (incoming direct, per unit)
               -> IncidentDirectResult
incidentDirect !hwr !coszen !zen !sdir
  | coszen <= 0.0 = IncidentDirectResult 0.0 0.0 0.0
  | otherwise =
    let !tanzen = tan zen
        !theta0 = asin (min (1.0 / (hwr * tan (max zen 0.000001))) 1.0)
        !sdir_road = sdir * (2.0 * theta0 / rpi -
                     2.0 / rpi * hwr * tanzen * (1.0 - cos theta0))
        !sdir_sw   = 2.0 * sdir * ((1.0 / hwr) * (0.5 - theta0 / rpi) +
                     (1.0 / rpi) * tanzen * (1.0 - cos theta0))
    in IncidentDirectResult
       { idr_sdir_road      = sdir_road
       , idr_sdir_sunwall   = sdir_sw
       , idr_sdir_shadewall = 0.0
       }

--------------------------------------------------------------------------------
-- Incident diffuse radiation
--------------------------------------------------------------------------------

-- | Diffuse solar radiation incident on walls and road using view factors.
incidentDiffuse :: UrbanViewFactors
                -> Double  -- ^ sdif (incoming diffuse, per unit)
                -> IncidentDiffuseResult
incidentDiffuse !vf !sdif =
  IncidentDiffuseResult
  { idf_sdif_road      = sdif * vf_sr vf
  , idf_sdif_sunwall   = sdif * vf_sw vf
  , idf_sdif_shadewall = sdif * vf_sw vf
  }

--------------------------------------------------------------------------------
-- Net solar with multiple reflections
--------------------------------------------------------------------------------

-- | Solar radiation absorbed by road and walls allowing for multiple reflection.
-- Works for either direct or diffuse component.
netSolar :: NetSolarInput -> NetSolarResult
netSolar !inp =
  let !hwr = nsi_canyon_hwr inp
      !wtp = nsi_wtroad_perv inp
      !wti = 1.0 - wtp
      !ai  = nsi_alb_improad inp
      !ap  = nsi_alb_perroad inp
      !aw  = nsi_alb_wall inp
      !s_road = nsi_sdir_road inp
      !s_sw   = nsi_sdir_sunwall inp
      !s_sh   = nsi_sdir_shadewall inp
      !vsr = vf_sr (nsi_vf inp)
      !vwr = vf_wr (nsi_vf inp)
      !vsw = vf_sw (nsi_vf inp)
      !vrw = vf_rw (nsi_vf inp)
      !vww = vf_ww (nsi_vf inp)

      -- Initial absorption and reflection
      !imp_a0 = (1.0 - ai) * s_road
      !imp_r0 = ai * s_road
      !per_a0 = (1.0 - ap) * s_road
      !per_r0 = ap * s_road
      !road_r0 = imp_r0 * wti + per_r0 * wtp

      !sw_a0 = (1.0 - aw) * s_sw
      !sw_r0 = aw * s_sw
      !sh_a0 = (1.0 - aw) * s_sh
      !sh_r0 = aw * s_sh

      -- Initial sums
      !sabs_imp0 = imp_a0
      !sabs_per0 = per_a0
      !sabs_sw0  = sw_a0
      !sabs_sh0  = sh_a0
      !sref_imp0 = imp_r0 * vsr
      !sref_per0 = per_r0 * vsr
      !sref_sw0  = sw_r0 * vsw
      !sref_sh0  = sh_r0 * vsw

      -- Iterate
      iterate' !n !sabsi !sabsp !sabssw !sabssh
               !srefi !srefp !srefsw !srefsh
               !sw_r_rd !sh_r_rd
               !rd_r_sw !rd_r_sh
               !sw_r_sh2 !sh_r_sw2
        | n >= maxIterSolar = (sabsi, sabsp, sabssw, sabssh, srefi, srefp, srefsw, srefsh)
        | otherwise =
          let !stot_rd = (sw_r_rd + sh_r_rd) * hwr
              !imp_a = (1.0 - ai) * stot_rd
              !imp_r = ai * stot_rd
              !per_a = (1.0 - ap) * stot_rd
              !per_r = ap * stot_rd
              !road_r = imp_r * wti + per_r * wtp

              !stot_sw = rd_r_sw / hwr + sh_r_sw2
              !sw_a = (1.0 - aw) * stot_sw
              !sw_r = aw * stot_sw

              !stot_sh = rd_r_sh / hwr + sw_r_sh2
              !sh_a = (1.0 - aw) * stot_sh
              !sh_r = aw * stot_sh

              !sabsi' = sabsi + imp_a
              !sabsp' = sabsp + per_a
              !sabssw' = sabssw + sw_a
              !sabssh' = sabssh + sh_a

              !imp_r_sky = imp_r * vsr
              !per_r_sky = per_r * vsr
              !sw_r_sky  = sw_r * vsw
              !sh_r_sky  = sh_r * vsw

              !srefi' = srefi + imp_r_sky
              !srefp' = srefp + per_r_sky
              !srefsw' = srefsw + sw_r_sky
              !srefsh' = srefsh + sh_r_sky

              !crit = max (imp_a * wti + per_a * wtp) (max sw_a sh_a)

              !sw_r_rd' = sw_r * vrw
              !sh_r_rd' = sh_r * vrw
              !rd_r_sw' = road_r * vwr
              !rd_r_sh' = road_r * vwr
              !sw_r_sh2' = sw_r * vww
              !sh_r_sw2' = sh_r * vww
          in if crit < errcrit
             then (sabsi', sabsp', sabssw', sabssh', srefi', srefp', srefsw', srefsh')
             else iterate' (n + 1) sabsi' sabsp' sabssw' sabssh'
                           srefi' srefp' srefsw' srefsh'
                           sw_r_rd' sh_r_rd' rd_r_sw' rd_r_sh' sw_r_sh2' sh_r_sw2'

      -- Initial reflected distributions
      !sw_r_rd0 = sw_r0 * vrw
      !sh_r_rd0 = sh_r0 * vrw
      !rd_r_sw0 = road_r0 * vwr
      !rd_r_sh0 = road_r0 * vwr
      !sw_r_sh0 = sw_r0 * vww
      !sh_r_sw0 = sh_r0 * vww

      (!si, !sp, !ssw, !ssh, !ri, !rp, !rsw, !rsh) =
        iterate' 0 sabs_imp0 sabs_per0 sabs_sw0 sabs_sh0
                   sref_imp0 sref_per0 sref_sw0 sref_sh0
                   sw_r_rd0 sh_r_rd0 rd_r_sw0 rd_r_sh0 sw_r_sh0 sh_r_sw0

  in NetSolarResult
     { nsr_sabs_improad   = si
     , nsr_sabs_perroad   = sp
     , nsr_sabs_sunwall   = ssw
     , nsr_sabs_shadewall = ssh
     , nsr_sref_improad   = ri
     , nsr_sref_perroad   = rp
     , nsr_sref_sunwall   = rsw
     , nsr_sref_shadewall = rsh
     }

--------------------------------------------------------------------------------
-- Combine snow and snow-free albedos
--------------------------------------------------------------------------------

-- | Linearly blend snow-free and snow albedo by snow fraction.
combineSnowAlbedo :: Double  -- ^ snow-free albedo
                  -> Double  -- ^ snow albedo
                  -> Double  -- ^ frac_sno
                  -> Double
combineSnowAlbedo !alb_bare !alb_snow !frac_sno =
  alb_bare * (1.0 - frac_sno) + alb_snow * frac_sno

--------------------------------------------------------------------------------
-- Full urban albedo (single-landunit)
--------------------------------------------------------------------------------

-- | Compute urban albedo for a single landunit.
-- Returns absorbed solar fractions (per facet) and the canyon ground albedos.
--
-- This computes the full single-landunit urban canyon radiation balance
-- (Fortran @UrbanAlbedoMod.F90@, subroutine @UrbanAlbedo@): incident direct
-- beam and diffuse on the canyon floor and sunlit/shaded walls (Masson 2000
-- geometry, 'incidentDirect' / 'incidentDiffuse'), followed by the
-- multiple-reflection net-solar solve ('netSolar') for the visible and NIR
-- bands and the direct/diffuse streams.
--
-- Architectural scope: the Fortran routine is vectorized over an urban-landunit
-- filter and then scatters each per-facet reflectance onto the per-column
-- ground-albedo arrays (@albgrd@/@albgri@), one urban column per facet
-- (roof, sunwall, shadewall, pervious road, impervious road). This port runs a
-- single landunit at a time; the per-column scatter is performed by the driver
-- from the per-facet results returned here (see note on 'uar_albgrd_vis').
urbanAlbedo :: UrbanViewFactors
            -> Double  -- ^ coszen
            -> Double  -- ^ canyon_hwr
            -> Double  -- ^ wtroad_perv
            -> (Double, Double)  -- ^ (alb_roof_dir_vis, alb_roof_dir_nir)
            -> (Double, Double)  -- ^ (alb_roof_dif_vis, alb_roof_dif_nir)
            -> (Double, Double)  -- ^ (alb_improad_dir_vis, alb_improad_dir_nir)
            -> (Double, Double)  -- ^ (alb_improad_dif_vis, alb_improad_dif_nir)
            -> (Double, Double)  -- ^ (alb_perroad_dir_vis, alb_perroad_dir_nir)
            -> (Double, Double)  -- ^ (alb_perroad_dif_vis, alb_perroad_dif_nir)
            -> (Double, Double)  -- ^ (alb_wall_dir_vis, alb_wall_dir_nir)
            -> (Double, Double)  -- ^ (alb_wall_dif_vis, alb_wall_dif_nir)
            -> UrbanAlbedoResult
urbanAlbedo !vf !coszen !hwr !wtp
            (!ard_rv, !ard_rn) (!adi_rv, !adi_rn)
            (!ard_iv, !ard_in) (!adi_iv, !adi_in)
            (!ard_pv, !ard_pn) (!adi_pv, !adi_pn)
            (!ard_wv, !ard_wn) (!adi_wv, !adi_wn)
  | coszen <= 0.0 = UrbanAlbedoResult 0.0 0.0 0.0 0.0
                     (0.0,0.0) (0.0,0.0) (0.0,0.0) (0.0,0.0) (0.0,0.0) (0.0,0.0)
                     (0.0,0.0) (0.0,0.0) (0.0,0.0) (0.0,0.0)
  | otherwise =
    let !zen = acos coszen

        -- Incident direct beam (per unit)
        !idr_v = incidentDirect hwr coszen zen 1.0
        !idr_n = incidentDirect hwr coszen zen 1.0

        -- Incident diffuse (per unit)
        !idf = incidentDiffuse vf 1.0

        -- Net solar for each band and component (dir)
        mkNsi alb_i alb_p alb_w sr ssw ssh = NetSolarInput hwr wtp alb_i alb_p alb_w sr ssw ssh vf

        !ns_dir_v = netSolar (mkNsi ard_iv ard_pv ard_wv
                     (idr_sdir_road idr_v) (idr_sdir_sunwall idr_v) (idr_sdir_shadewall idr_v))
        !ns_dir_n = netSolar (mkNsi ard_in ard_pn ard_wn
                     (idr_sdir_road idr_n) (idr_sdir_sunwall idr_n) (idr_sdir_shadewall idr_n))

        -- Net solar (dif)
        !ns_dif_v = netSolar (mkNsi adi_iv adi_pv adi_wv
                     (idf_sdif_road idf) (idf_sdif_sunwall idf) (idf_sdif_shadewall idf))
        !ns_dif_n = netSolar (mkNsi adi_in adi_pn adi_wn
                     (idf_sdif_road idf) (idf_sdif_sunwall idf) (idf_sdif_shadewall idf))

        -- Roof absorbed
        !sabs_roof_dir_v = 1.0 - ard_rv
        !sabs_roof_dir_n = 1.0 - ard_rn
        !sabs_roof_dif_v = 1.0 - adi_rv
        !sabs_roof_dif_n = 1.0 - adi_rn

    in UrbanAlbedoResult
       -- The canyon ground albedo is the impervious-road facet reflectance to
       -- sky (reflected per unit ground per unit incident flux). In the Fortran
       -- per-column scatter (UrbanAlbedoMod.F90 lines 406-446) each urban column
       -- takes the reflectance of its facet: roof -> alb_roof, sun/shade wall ->
       -- sref_sunwall/sref_shadewall, pervious road -> sref_perroad, impervious
       -- road -> sref_improad. Here we return the impervious-road column value
       -- (canyon floor) as the representative single-column ground albedo; the
       -- driver maps the remaining facets from uar_sabs_*/the roof albedos.
       { uar_albgrd_vis = nsr_sref_improad ns_dir_v
       , uar_albgrd_nir = nsr_sref_improad ns_dir_n
       , uar_albgri_vis = nsr_sref_improad ns_dif_v
       , uar_albgri_nir = nsr_sref_improad ns_dif_n
       , uar_sabs_roof_dir = (sabs_roof_dir_v, sabs_roof_dir_n)
       , uar_sabs_roof_dif = (sabs_roof_dif_v, sabs_roof_dif_n)
       , uar_sabs_sunwall_dir = (nsr_sabs_sunwall ns_dir_v, nsr_sabs_sunwall ns_dir_n)
       , uar_sabs_sunwall_dif = (nsr_sabs_sunwall ns_dif_v, nsr_sabs_sunwall ns_dif_n)
       , uar_sabs_shadewall_dir = (nsr_sabs_shadewall ns_dir_v, nsr_sabs_shadewall ns_dir_n)
       , uar_sabs_shadewall_dif = (nsr_sabs_shadewall ns_dif_v, nsr_sabs_shadewall ns_dif_n)
       , uar_sabs_improad_dir = (nsr_sabs_improad ns_dir_v, nsr_sabs_improad ns_dir_n)
       , uar_sabs_improad_dif = (nsr_sabs_improad ns_dif_v, nsr_sabs_improad ns_dif_n)
       , uar_sabs_perroad_dir = (nsr_sabs_perroad ns_dir_v, nsr_sabs_perroad ns_dir_n)
       , uar_sabs_perroad_dif = (nsr_sabs_perroad ns_dif_v, nsr_sabs_perroad ns_dif_n)
       }
