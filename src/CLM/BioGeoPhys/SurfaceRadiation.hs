{-# LANGUAGE BangPatterns #-}
-- | Surface radiative flux calculations.
-- Computes solar fluxes absorbed by vegetation and ground surface,
-- sun/shade fractions, and PAR absorption profiles.
-- Fortran: SurfaceRadiationMod.F90
-- Julia:   src/biogeophys/surface_radiation.jl
--
-- All functions are pure.
--
module CLM.BioGeoPhys.SurfaceRadiation
  ( -- * Constants
    mpeSurfrad
  , spval
    -- * Data types
  , SunShadeFracInput(..)
  , SunShadeFracResult(..)
  , SurfRadColumnInput(..)
  , SurfRadPatchInput(..)
  , SurfRadConfig(..)
  , defaultSurfRadConfig
  , SurfRadResult(..)
    -- * Functions
  , isNearLocalNoon
  , canopySunShadeFracs
  , surfaceRadiationPatch
    -- * Longwave radiation
  , LongwaveInput(..)
  , LongwaveResult(..)
  , longwaveRadiation
    -- * Net radiation
  , netRadiation
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (numrad, nlevsno)

-- ========================================================================
-- Constants
-- ========================================================================

-- | Machine precision epsilon for divisions
mpeSurfrad :: Double
mpeSurfrad = 1.0e-06

-- | Special value for missing data
spval :: Double
spval = 1.0e36

-- | Landunit types
istsoil, istcrop, istdlak :: Int
istsoil = 1
istcrop = 2
istdlak = 5

-- ========================================================================
-- Data types
-- ========================================================================

-- | Input for sun/shade fraction computation (per patch).
data SunShadeFracInput = SunShadeFracInput
  { ssf_nrad         :: !Int                -- ^ Number of radiatively active layers
  , ssf_tlai_z       :: !(VU.Vector Double) -- ^ LAI per canopy layer
  , ssf_fsun_z       :: !(VU.Vector Double) -- ^ Sunlit fraction per canopy layer
  , ssf_fabd_sun_z   :: !(VU.Vector Double) -- ^ Sunlit direct absorbed per layer
  , ssf_fabi_sun_z   :: !(VU.Vector Double) -- ^ Sunlit diffuse absorbed per layer
  , ssf_fabd_sha_z   :: !(VU.Vector Double) -- ^ Shaded direct absorbed per layer
  , ssf_fabi_sha_z   :: !(VU.Vector Double) -- ^ Shaded diffuse absorbed per layer
  , ssf_elai         :: !Double             -- ^ Exposed LAI
  , ssf_forc_solad   :: !Double             -- ^ Direct beam PAR (vis band) [W/m2]
  , ssf_forc_solai   :: !Double             -- ^ Diffuse PAR (vis band) [W/m2]
  } deriving (Show)

-- | Result of sun/shade fraction computation.
data SunShadeFracResult = SunShadeFracResult
  { ssr_laisun       :: !Double             -- ^ Total sunlit LAI
  , ssr_laisha       :: !Double             -- ^ Total shaded LAI
  , ssr_fsun         :: !Double             -- ^ Sunlit fraction of canopy
  , ssr_laisun_z     :: !(VU.Vector Double) -- ^ Sunlit LAI per layer
  , ssr_laisha_z     :: !(VU.Vector Double) -- ^ Shaded LAI per layer
  , ssr_parsun_z     :: !(VU.Vector Double) -- ^ PAR absorbed by sunlit per layer [W/m2]
  , ssr_parsha_z     :: !(VU.Vector Double) -- ^ PAR absorbed by shaded per layer [W/m2]
  } deriving (Show)

-- | Column-level inputs for surface radiation.
data SurfRadColumnInput = SurfRadColumnInput
  { src_snl         :: !Int     -- ^ Number of snow layers (negative or 0)
  , src_albsod      :: !(VU.Vector Double)  -- ^ Soil direct albedo by waveband
  , src_albsoi      :: !(VU.Vector Double)  -- ^ Soil diffuse albedo by waveband
  , src_albsnd_hst  :: !(VU.Vector Double)  -- ^ Snow direct albedo (history)
  , src_albsni_hst  :: !(VU.Vector Double)  -- ^ Snow diffuse albedo (history)
  , src_albgrd      :: !(VU.Vector Double)  -- ^ Ground direct albedo by waveband
  , src_albgri      :: !(VU.Vector Double)  -- ^ Ground diffuse albedo by waveband
  , src_flx_absdv   :: !(VU.Vector Double)  -- ^ Absorbed direct VIS flux factor per layer
  , src_flx_absdn   :: !(VU.Vector Double)  -- ^ Absorbed direct NIR flux factor per layer
  , src_flx_absiv   :: !(VU.Vector Double)  -- ^ Absorbed diffuse VIS flux factor per layer
  , src_flx_absin   :: !(VU.Vector Double)  -- ^ Absorbed diffuse NIR flux factor per layer
  , src_snow_depth  :: !Double  -- ^ Snow depth [m]
  , src_frac_sno    :: !Double  -- ^ Snow fraction
  } deriving (Show)

-- | Patch-level inputs for surface radiation.
data SurfRadPatchInput = SurfRadPatchInput
  { srp_lunType      :: !Int    -- ^ Landunit type
  , srp_londeg       :: !Double -- ^ Longitude [degrees]
  , srp_fabd         :: !(VU.Vector Double)  -- ^ Direct beam absorbed fraction by waveband
  , srp_fabi         :: !(VU.Vector Double)  -- ^ Diffuse absorbed fraction by waveband
  , srp_ftdd         :: !(VU.Vector Double)  -- ^ Direct-through fraction
  , srp_ftid         :: !(VU.Vector Double)  -- ^ Diffuse-from-direct through
  , srp_ftii         :: !(VU.Vector Double)  -- ^ Diffuse-through fraction
  , srp_albd         :: !(VU.Vector Double)  -- ^ Direct beam albedo above canopy
  , srp_albi         :: !(VU.Vector Double)  -- ^ Diffuse albedo above canopy
  , srp_forc_solad   :: !(VU.Vector Double)  -- ^ Direct beam by waveband [W/m2] (col-level)
  , srp_forc_solai   :: !(VU.Vector Double)  -- ^ Diffuse by waveband [W/m2] (grc-level)
  } deriving (Show)

-- | Configuration options for surface radiation.
data SurfRadConfig = SurfRadConfig
  { srf_dtime                :: !Double  -- ^ Timestep [s]
  , srf_current_tod          :: !Double  -- ^ Current time-of-day [s since midnight UTC]
  , srf_use_subgrid_fluxes   :: !Bool
  , srf_use_snicar_frc       :: !Bool
  , srf_use_SSRE             :: !Bool
  } deriving (Show)

defaultSurfRadConfig :: SurfRadConfig
defaultSurfRadConfig = SurfRadConfig
  { srf_dtime = 3600.0
  , srf_current_tod = 43200.0
  , srf_use_subgrid_fluxes = True
  , srf_use_snicar_frc = False
  , srf_use_SSRE = False
  }

-- | Result of surface radiation for a single non-urban patch.
data SurfRadResult = SurfRadResult
  { srr_sabg       :: !Double  -- ^ Absorbed solar by ground [W/m2]
  , srr_sabv       :: !Double  -- ^ Absorbed solar by vegetation [W/m2]
  , srr_fsa        :: !Double  -- ^ Total absorbed solar [W/m2]
  , srr_fsr        :: !Double  -- ^ Total reflected solar [W/m2]
  , srr_sabg_soil  :: !Double  -- ^ Absorbed by soil ground [W/m2]
  , srr_sabg_snow  :: !Double  -- ^ Absorbed by snow ground [W/m2]
  , srr_sabg_lyr   :: !(VU.Vector Double)  -- ^ Absorbed per snow+soil layer [W/m2]
  , srr_fsds_vis_d :: !Double  -- ^ Incident direct VIS [W/m2]
  , srr_fsds_vis_i :: !Double  -- ^ Incident diffuse VIS [W/m2]
  , srr_fsr_vis_d  :: !Double  -- ^ Reflected direct VIS [W/m2]
  , srr_fsr_vis_i  :: !Double  -- ^ Reflected diffuse VIS [W/m2]
  , srr_parveg     :: !Double  -- ^ PAR absorbed by vegetation [W/m2]
  } deriving (Show)

-- ========================================================================
-- Functions
-- ========================================================================

-- | Check if current time is near local solar noon.
--
-- Ported from @is_near_local_noon@ in @SurfaceRadiationMod.F90@.
isNearLocalNoon :: Double  -- ^ Longitude [degrees]
                -> Int     -- ^ Half-window around noon [seconds]
                -> Double  -- ^ Current time-of-day [seconds since midnight UTC]
                -> Bool
isNearLocalNoon londeg deltasec currentTod =
  let localNoon0 = 43200.0 - londeg * 240.0
      localNoon  = localNoon0 - fromIntegral (floor (localNoon0 / 86400.0) :: Int) * 86400.0
      diff0      = abs (currentTod - localNoon)
      diff       = if diff0 > 43200.0 then 86400.0 - diff0 else diff0
  in diff <= fromIntegral deltasec

-- | Compute sun/shade fractions and absorbed PAR for a single non-urban patch.
--
-- Ported from @CanopySunShadeFracs@ in @SurfaceRadiationMod.F90@.
canopySunShadeFracs :: SunShadeFracInput -> SunShadeFracResult
canopySunShadeFracs inp =
  let nrad = ssf_nrad inp
      tlai_z = ssf_tlai_z inp
      fsun_z = ssf_fsun_z inp

      -- Build laisun_z and laisha_z
      laisun_z = VU.generate nrad $ \iv ->
        (tlai_z VU.! iv) * (fsun_z VU.! iv)
      laisha_z = VU.generate nrad $ \iv ->
        (tlai_z VU.! iv) * (1.0 - fsun_z VU.! iv)

      laisun = VU.sum laisun_z
      laisha = VU.sum laisha_z

      fsun = if ssf_elai inp > 0.0
             then laisun / ssf_elai inp
             else 0.0

      -- Absorbed PAR by sunlit and shaded
      parsun_z = VU.generate nrad $ \iv ->
        ssf_forc_solad inp * (ssf_fabd_sun_z inp VU.! iv)
        + ssf_forc_solai inp * (ssf_fabi_sun_z inp VU.! iv)
      parsha_z = VU.generate nrad $ \iv ->
        ssf_forc_solad inp * (ssf_fabd_sha_z inp VU.! iv)
        + ssf_forc_solai inp * (ssf_fabi_sha_z inp VU.! iv)

  in SunShadeFracResult
       { ssr_laisun   = laisun
       , ssr_laisha   = laisha
       , ssr_fsun     = fsun
       , ssr_laisun_z = laisun_z
       , ssr_laisha_z = laisha_z
       , ssr_parsun_z = parsun_z
       , ssr_parsha_z = parsha_z
       }

-- | Compute surface radiation for a single non-urban patch.
-- Simplified single-patch version of the full loop in @surface_radiation!@.
--
-- Ported from @SurfaceRadiation@ in @SurfaceRadiationMod.F90@.
surfaceRadiationPatch :: SurfRadConfig
                      -> SurfRadColumnInput
                      -> SurfRadPatchInput
                      -> SurfRadResult
surfaceRadiationPatch cfg colInp pInp =
  let nband = numrad
      nlyr  = nlevsno + 1  -- sabg_lyr has nlevsno+1 entries

      -- Accumulate over wavebands
      go ib (sabv, sabg, sabgSoil, sabgSnow, fsa, parveg0, trdV, triV) =
        let forc_d = srp_forc_solad pInp VU.! ib
            forc_i = srp_forc_solai pInp VU.! ib
            -- Absorbed by canopy
            cadVal = forc_d * (srp_fabd pInp VU.! ib)
            caiVal = forc_i * (srp_fabi pInp VU.! ib)
            sabv' = sabv + cadVal + caiVal
            fsa'  = fsa + cadVal + caiVal
            parveg' = if ib == 0 then cadVal + caiVal else parveg0
            -- Transmitted to ground
            trd = forc_d * (srp_ftdd pInp VU.! ib)
            tri = forc_d * (srp_ftid pInp VU.! ib) + forc_i * (srp_ftii pInp VU.! ib)
            -- Absorbed by ground
            absrad_soil = trd * (1.0 - src_albsod colInp VU.! ib)
                        + tri * (1.0 - src_albsoi colInp VU.! ib)
            absrad_snow = trd * (1.0 - src_albsnd_hst colInp VU.! ib)
                        + tri * (1.0 - src_albsni_hst colInp VU.! ib)
            absrad      = trd * (1.0 - src_albgrd colInp VU.! ib)
                        + tri * (1.0 - src_albgri colInp VU.! ib)
            sabgSoil' = sabgSoil + absrad_soil
            sabgSnow' = sabgSnow + absrad_snow
            sabg' = sabg + absrad
            fsa'' = fsa' + absrad
            trdV' = if ib == 0 then trd else trdV
            triV' = if ib == 0 then tri else triV
        in (sabv', sabg', sabgSoil', sabgSnow', fsa'', parveg', trdV', triV')

      (sabvF, sabgF, sabgSoilF, sabgSnowF, fsaF, parvegF, _trdVF, _triVF) =
        foldr (\ib acc -> go ib acc) (0,0,0,0,0,0,0,0) [0 .. nband - 1]

      -- Adjust for no-snow case
      (sabgSoilFin, sabgSnowFin)
        | src_snl colInp == 0 = (sabgF, sabgF)
        | not (srf_use_subgrid_fluxes cfg) = (sabgF, sabgF)
        | otherwise = (sabgSoilF, sabgSnowF)

      -- Reflected
      rvis = (srp_albd pInp VU.! 0) * (srp_forc_solad pInp VU.! 0)
           + (srp_albi pInp VU.! 0) * (srp_forc_solai pInp VU.! 0)
      rnir = (srp_albd pInp VU.! 1) * (srp_forc_solad pInp VU.! 1)
           + (srp_albi pInp VU.! 1) * (srp_forc_solai pInp VU.! 1)
      fsr  = rvis + rnir

      -- Layer-resolved absorbed flux (simplified: all in top soil)
      sabg_lyr = VU.generate nlyr $ \j ->
        if j == nlevsno then sabgF else 0.0

      -- VIS diagnostics
      fsds_vis_d = srp_forc_solad pInp VU.! 0
      fsds_vis_i = srp_forc_solai pInp VU.! 0
      fsr_vis_d  = (srp_albd pInp VU.! 0) * fsds_vis_d
      fsr_vis_i  = (srp_albi pInp VU.! 0) * fsds_vis_i

  in SurfRadResult
       { srr_sabg      = sabgF
       , srr_sabv      = sabvF
       , srr_fsa       = fsaF
       , srr_fsr       = fsr
       , srr_sabg_soil = sabgSoilFin
       , srr_sabg_snow = sabgSnowFin
       , srr_sabg_lyr  = sabg_lyr
       , srr_fsds_vis_d = fsds_vis_d
       , srr_fsds_vis_i = fsds_vis_i
       , srr_fsr_vis_d  = fsr_vis_d
       , srr_fsr_vis_i  = fsr_vis_i
       , srr_parveg     = parvegF
       }

-- ========================================================================
-- Longwave radiation (computed in CanopyFluxes/BaregroundFluxes, but
-- diagnostics assembled here to match Fortran SurfaceRadiationMod)
-- ========================================================================

data LongwaveInput = LongwaveInput
  { lwi_forc_lwrad    :: !Double  -- ^ downwelling atmospheric LW (W/m2)
  , lwi_t_grnd        :: !Double  -- ^ ground temperature (K)
  , lwi_t_veg         :: !Double  -- ^ vegetation temperature (K)
  , lwi_emv           :: !Double  -- ^ vegetation emissivity
  , lwi_emg           :: !Double  -- ^ ground emissivity
  , lwi_frac_veg      :: !Double  -- ^ fraction of ground covered by vegetation
  } deriving (Show)

data LongwaveResult = LongwaveResult
  { lwr_eflx_lwrad_out  :: !Double  -- ^ outgoing longwave (W/m2)
  , lwr_eflx_lwrad_net  :: !Double  -- ^ net longwave (W/m2, positive = loss)
  , lwr_lwrad_veg       :: !Double  -- ^ LW emitted by vegetation (W/m2)
  , lwr_lwrad_grnd      :: !Double  -- ^ LW emitted by ground (W/m2)
  } deriving (Show)

-- | Stefan-Boltzmann constant (W/m2/K4)
sbConst :: Double
sbConst = 5.67e-8

-- | Compute longwave radiation balance for a patch.
-- Outgoing LW = emitted by vegetation + emitted by ground * (1 - frac_veg)
-- Net LW = outgoing - incoming
longwaveRadiation :: LongwaveInput -> LongwaveResult
longwaveRadiation !inp =
  let !fv = lwi_frac_veg inp
      !lw_veg = lwi_emv inp * sbConst * lwi_t_veg inp ** 4
      !lw_grnd = lwi_emg inp * sbConst * lwi_t_grnd inp ** 4
      !lw_out = fv * lw_veg + (1.0 - fv) * lw_grnd
                + (1.0 - lwi_emv inp) * fv * lwi_forc_lwrad inp
                + (1.0 - lwi_emg inp) * (1.0 - fv) * lwi_forc_lwrad inp
      !lw_net = lw_out - lwi_forc_lwrad inp
  in LongwaveResult
     { lwr_eflx_lwrad_out = lw_out
     , lwr_eflx_lwrad_net = lw_net
     , lwr_lwrad_veg = lw_veg
     , lwr_lwrad_grnd = lw_grnd
     }

-- ========================================================================
-- Net radiation
-- ========================================================================

-- | Compute net radiation from shortwave and longwave components.
-- Rnet = FSA - LWnet = (sabv + sabg) - (LWout - LWdown)
netRadiation :: Double  -- ^ fsa (absorbed shortwave, W/m2)
             -> Double  -- ^ eflx_lwrad_net (net longwave, W/m2)
             -> Double  -- ^ net radiation (W/m2)
netRadiation !fsa !lwnet = fsa - lwnet
