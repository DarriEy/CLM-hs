{-# LANGUAGE BangPatterns #-}
-- | End-to-end CLM simulation driver.
-- Reads binary test data, runs the physics pipeline for N timesteps,
-- and writes daily-average CSV output.
--
-- This is a monolithic driver for the single-column SP-mode case
-- (Bow at Banff: ng=1, nc=2, np=4). It directly calls physics modules
-- in the same order as Julia's clm_drv!, threading state through each step.
module CLM.Driver.Simulation
  ( -- * Simulation state
    SimState(..)
  , initSimState
    -- * Single timestep
  , stepSimulation
    -- * Time loop
  , runSimulation
    -- * Daily diagnostics
  , DailyAvg(..)
  , zeroDailyAvg
  ) where

import Control.Monad (when)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import Debug.Trace (trace)

import CLM.Constants.PhysicalConstants
  ( nlevsno, nlevgrnd, nlevsoi, tfrz, sb, denh2o, denice
  , cpair, cpice, cpliq, hfus, hvap, hsub, grav )
import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readFloat64Scalar, readInt64Vector
  , readManifestDims, ManifestDims(..) )
import CLM.Infrastructure.ReadParams
  ( readParametersBinary, AllParams(..), PFTConstants(..) )
import CLM.Infrastructure.ForcingReader
  ( ForcingReaderState(..), forcingReaderInitBinary, readForcingStepPure
  , ForcingTimestep(..)
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity
  , splitShortwaveBands )
import CLM.BioGeoPhys.QSat (QSatResult(..), qsat)
import CLM.BioGeoPhys.BaregroundFluxes
  ( BareGroundFluxesInput(..), BareGroundFluxesOutput(..)
  , BareGroundFluxesParams(..), defaultBareGroundFluxesParams
  , Z0ParamMethod(..)
  , baregroundFluxes )
import CLM.BioGeoPhys.CanopyFluxes
  ( CanopyFluxesInput(..), CanopyFluxesOutput(..)
  , CanopyFluxesParams(..), defaultCanopyFluxesParams
  , CanopyFluxesControl(..), defaultCanopyFluxesControl
  , canopyFluxes )
import CLM.BioGeoPhys.SoilTemperature
  ( SoilTempInput(..), SoilTempOutput(..)
  , solveSoilTemperature, SnowThermalCond(..) )
import CLM.Infrastructure.Orbital
  ( computeOrbital, OrbitalParams(..), defaultOrbitalParams )

-- ============================================================================
-- Simulation state
-- ============================================================================

-- | Flat state vector for the simulation.
-- All column arrays are for the soil column only (column index 0).
-- Layer arrays are length nlevsno+nlevgrnd = 37 (0-based).
data SimState = SimState
  { -- Column geometry (constant after init)
    ss_dz           :: !(VU.Vector Double)  -- ^ Layer thicknesses [m]
  , ss_z            :: !(VU.Vector Double)  -- ^ Layer midpoints [m]
  , ss_zi           :: !(VU.Vector Double)  -- ^ Layer interfaces [m]
    -- Soil properties (constant after init)
  , ss_watsat       :: !(VU.Vector Double)  -- ^ Saturated water content [nlevgrnd]
  , ss_bsw          :: !(VU.Vector Double)  -- ^ Clapp-Hornberger B [nlevgrnd]
  , ss_sucsat       :: !(VU.Vector Double)  -- ^ Saturated matric potential [nlevgrnd]
  , ss_tkmg         :: !(VU.Vector Double)  -- ^ Geometric mean thermal cond [nlevgrnd]
  , ss_tkdry        :: !(VU.Vector Double)  -- ^ Dry thermal conductivity [nlevgrnd]
  , ss_csol         :: !(VU.Vector Double)  -- ^ Dry soil heat capacity [nlevgrnd]
  , ss_tksatu       :: !(VU.Vector Double)  -- ^ Saturated thermal cond [nlevgrnd]
  , ss_nbedrock     :: !Int                 -- ^ Bedrock layer index
    -- Temperature state
  , ss_t_soisno     :: !(VU.Vector Double)  -- ^ Snow+soil temperatures [nlevtot]
  , ss_t_grnd       :: !Double              -- ^ Ground surface temperature [K]
  , ss_t_h2osfc     :: !Double              -- ^ Surface water temperature [K]
    -- Water state
  , ss_h2osoi_liq   :: !(VU.Vector Double)  -- ^ Liquid water [nlevtot, kg/m2]
  , ss_h2osoi_ice   :: !(VU.Vector Double)  -- ^ Ice water [nlevtot, kg/m2]
  , ss_h2osno_no_layers :: !Double          -- ^ Unresolved snow [kg/m2]
  , ss_h2osfc       :: !Double              -- ^ Surface water [kg/m2]
    -- Snow state
  , ss_snl          :: !Int                 -- ^ Snow layer count (<= 0)
  , ss_snow_depth   :: !Double              -- ^ Snow depth [m]
  , ss_frac_sno     :: !Double              -- ^ Snow cover fraction [-]
  , ss_frac_sno_eff :: !Double              -- ^ Effective snow cover fraction [-]
  , ss_frac_h2osfc  :: !Double              -- ^ Surface water fraction [-]
  , ss_int_snow     :: !Double              -- ^ Integrated snowfall [mm]
    -- Patch-level state (np=4 patches: bare + 3 veg)
  , ss_t_veg        :: !(VU.Vector Double)  -- ^ Vegetation temperature [np]
  , ss_t_ref2m      :: !(VU.Vector Double)  -- ^ 2m air temperature [np]
  , ss_elai         :: !(VU.Vector Double)  -- ^ Exposed LAI [np]
  , ss_esai         :: !(VU.Vector Double)  -- ^ Exposed SAI [np]
  , ss_htop         :: !(VU.Vector Double)  -- ^ Canopy top height [np]
  , ss_frac_veg_nosno :: !(VU.Vector Int)   -- ^ Veg present flag [np]
    -- Flux outputs (for diagnostics)
  , ss_eflx_sh_tot  :: !(VU.Vector Double)  -- ^ Total sensible heat [np]
  , ss_eflx_lh_tot  :: !(VU.Vector Double)  -- ^ Total latent heat [np]
  , ss_fsa          :: !Double              -- ^ Absorbed shortwave, column-weighted [W/m2]
  , ss_fsa_patch0   :: !Double              -- ^ Absorbed shortwave, bare ground patch [W/m2]
  , ss_qflx_runoff  :: !Double              -- ^ Surface runoff [mm/s]
    -- Patch topology (constant)
  , ss_pch_wtcol    :: !(VU.Vector Double)  -- ^ Patch weight on column [np]
  , ss_pch_itype    :: !(VU.Vector Int)     -- ^ Patch vegetation type [np]
  , ss_pch_column   :: !(VU.Vector Int)     -- ^ Patch → column index (1-based) [np]
    -- Grid info (constant)
  , ss_lat          :: !Double              -- ^ Latitude [rad]
  , ss_lon          :: !Double              -- ^ Longitude [rad]
    -- Parameters
  , ss_params       :: !AllParams
    -- Forcing state
  , ss_forcing      :: !ForcingReaderState
  } deriving (Show)

-- ============================================================================
-- Initialization
-- ============================================================================

-- | Initialize simulation state from binary test data directory.
initSimState :: FilePath -> IO SimState
initSimState dir = do
  dims <- readManifestDims (dir </> "manifest.json")
  let nc = mdNc dims
      nlevtot = nlevsno + nlevgrnd  -- 37

  -- Read cold-start state (column-major [nc, nlev] → extract column 0)
  let extractCol1 :: VU.Vector Double -> Int -> VU.Vector Double
      extractCol1 vec nlev = VU.generate nlev (\j -> vec VU.! (j * nc + 0))

  t_soisno_raw <- readFloat64Vector (dir </> "coldstart" </> "t_soisno.bin")
  h2osoi_liq_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osoi_liq.bin")
  h2osoi_ice_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osoi_ice.bin")
  dz_raw <- readFloat64Vector (dir </> "coldstart" </> "col_dz.bin")
  z_raw <- readFloat64Vector (dir </> "coldstart" </> "col_z.bin")
  zi_raw <- readFloat64Vector (dir </> "coldstart" </> "col_zi.bin")
  watsat_raw <- readFloat64Vector (dir </> "coldstart" </> "watsat.bin")
  bsw_raw <- readFloat64Vector (dir </> "coldstart" </> "bsw.bin")
  sucsat_raw <- readFloat64Vector (dir </> "coldstart" </> "sucsat.bin")
  tkmg_raw <- readFloat64Vector (dir </> "coldstart" </> "tkmg.bin")
  tkdry_raw <- readFloat64Vector (dir </> "coldstart" </> "tkdry.bin")
  csol_raw <- readFloat64Vector (dir </> "coldstart" </> "csol.bin")
  tksatu_raw <- readFloat64Vector (dir </> "coldstart" </> "tksatu.bin")
  t_grnd_raw <- readFloat64Vector (dir </> "coldstart" </> "t_grnd.bin")
  t_h2osfc_raw <- readFloat64Vector (dir </> "coldstart" </> "t_h2osfc.bin")
  h2osfc_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osfc.bin")
  h2osno_raw <- readFloat64Vector (dir </> "coldstart" </> "h2osno.bin")

  -- Patch-level state
  t_veg_raw <- readFloat64Vector (dir </> "coldstart" </> "t_veg.bin")
  t_ref2m_raw <- readFloat64Vector (dir </> "coldstart" </> "t_ref2m.bin")
  elai_raw <- readFloat64Vector (dir </> "coldstart" </> "elai.bin")
  esai_raw <- readFloat64Vector (dir </> "coldstart" </> "esai.bin")
  htop_raw <- readFloat64Vector (dir </> "coldstart" </> "htop.bin")

  -- Patch topology
  pch_wtcol_raw <- readFloat64Vector (dir </> "coldstart" </> "pch_wtcol.bin")
  pch_itype_raw <- readInt64Vector (dir </> "coldstart" </> "pch_itype.bin")
  pch_column_raw <- readInt64Vector (dir </> "coldstart" </> "pch_column.bin")

  -- Parameters
  params <- readParametersBinary (dir </> "params")

  -- Forcing
  forcing <- forcingReaderInitBinary (dir </> "forcing")

  let t_soisno = extractCol1 t_soisno_raw nlevtot
      -- Fix cold-start: if T < tfrz, water should be ice, not liquid
      -- CLM cold start: h2osoi_ice = vol_water * dz * denice when T <= tfrz
      h2osoi_liq_raw_col = extractCol1 h2osoi_liq_raw nlevtot
      h2osoi_ice_raw_col = extractCol1 h2osoi_ice_raw nlevtot
      h2osoi_liq = VU.generate nlevtot $ \j ->
        if j >= nlevsno && t_soisno VU.! j <= tfrz
        then 0.0  -- frozen: no liquid
        else h2osoi_liq_raw_col VU.! j
      h2osoi_ice = VU.generate nlevtot $ \j ->
        if j >= nlevsno && t_soisno VU.! j <= tfrz
        then h2osoi_liq_raw_col VU.! j + h2osoi_ice_raw_col VU.! j  -- all as ice
        else h2osoi_ice_raw_col VU.! j
      dz = extractCol1 dz_raw nlevtot
      z = extractCol1 z_raw nlevtot
      zi = extractCol1 zi_raw (nlevtot + 1)
      -- Sanitize NaN values in soil properties (bedrock layers may have NaN)
      sanitize v = VU.map (\x -> if isNaN x then 0.0 else x) v
      watsat = sanitize $ extractCol1 watsat_raw nlevgrnd
      bsw = sanitize $ extractCol1 bsw_raw nlevgrnd
      sucsat = sanitize $ extractCol1 sucsat_raw nlevgrnd
      tkmg = sanitize $ extractCol1 tkmg_raw nlevgrnd
      tkdry = sanitize $ extractCol1 tkdry_raw nlevgrnd
      csol = sanitize $ extractCol1 csol_raw nlevgrnd
      tksatu = sanitize $ extractCol1 tksatu_raw nlevgrnd

      np = mdNp dims

  return SimState
    { ss_dz = dz
    , ss_z = z
    , ss_zi = zi
    , ss_watsat = watsat
    , ss_bsw = bsw
    , ss_sucsat = sucsat
    , ss_tkmg = tkmg
    , ss_tkdry = tkdry
    , ss_csol = csol
    , ss_tksatu = tksatu
    , ss_nbedrock = nlevsoi  -- bedrock below nlevsoi=20
    , ss_t_soisno = t_soisno
    , ss_t_grnd = t_grnd_raw VU.! 0
    , ss_t_h2osfc = t_h2osfc_raw VU.! 0
    , ss_h2osoi_liq = h2osoi_liq
    , ss_h2osoi_ice = h2osoi_ice
    , ss_h2osno_no_layers = h2osno_raw VU.! 0
    , ss_h2osfc = h2osfc_raw VU.! 0
    , ss_snl = 0  -- cold start: no snow layers
    , ss_snow_depth = 0.0
    , ss_frac_sno = 0.0
    , ss_frac_sno_eff = 0.0
    , ss_frac_h2osfc = 0.0
    , ss_int_snow = 0.0
    -- Override t_veg with t_grnd (cold-start: veg should be near ground temp)
    -- Binary data may have stale warm values from a different init path
    , ss_t_veg = VU.replicate np (t_grnd_raw VU.! 0)
    , ss_t_ref2m = t_ref2m_raw
    , ss_elai = elai_raw
    , ss_esai = esai_raw
    , ss_htop = htop_raw
    , ss_frac_veg_nosno = VU.generate np (\p ->
        if elai_raw VU.! p + esai_raw VU.! p > 0.0 then 1 else 0)
    , ss_eflx_sh_tot = VU.replicate np 0.0
    , ss_eflx_lh_tot = VU.replicate np 0.0
    , ss_fsa = 0.0
    , ss_fsa_patch0 = 0.0
    , ss_qflx_runoff = 0.0
    , ss_pch_wtcol = pch_wtcol_raw
    , ss_pch_itype = VU.map fromIntegral pch_itype_raw
    , ss_pch_column = VU.map fromIntegral pch_column_raw
    , ss_lat = mdLat dims
    , ss_lon = mdLon dims
    , ss_params = params
    , ss_forcing = forcing
    }

-- ============================================================================
-- Daily diagnostics
-- ============================================================================

data DailyAvg = DailyAvg
  { da_t_grnd     :: !Double
  , da_tsa        :: !Double
  , da_fsa        :: !Double
  , da_eflx_lh    :: !Double
  , da_eflx_sh    :: !Double
  , da_h2osno     :: !Double
  , da_qrunoff    :: !Double
  , da_snow_depth :: !Double
  , da_frac_sno   :: !Double
  } deriving (Show)

zeroDailyAvg :: DailyAvg
zeroDailyAvg = DailyAvg 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0

addDailyAvg :: DailyAvg -> SimState -> DailyAvg
addDailyAvg da st =
  let -- Julia exports patch[1] (1-based) = bare ground patch (index 0 in Haskell)
      -- Match that convention: report bare ground patch values for FSA, SH, LH, TSA
      tsa = ss_t_ref2m st VU.! 0      -- bare ground t_ref2m
      sh  = ss_eflx_sh_tot st VU.! 0  -- bare ground SH
      lh  = ss_eflx_lh_tot st VU.! 0  -- bare ground LH
      -- Total snow water (0-based: snow layers at [nlevsno+snl .. nlevsno-1])
      snl = ss_snl st
      h2osno = sum [ ss_h2osoi_liq st VU.! j +
                     ss_h2osoi_ice st VU.! j
                   | j <- [nlevsno + snl .. nlevsno - 1] ]
             + ss_h2osno_no_layers st
  in DailyAvg
    { da_t_grnd     = da_t_grnd da + ss_t_grnd st
    , da_tsa        = da_tsa da + tsa
    , da_fsa        = da_fsa da + ss_fsa_patch0 st  -- bare ground patch FSA
    , da_eflx_lh    = da_eflx_lh da + lh
    , da_eflx_sh    = da_eflx_sh da + sh
    , da_h2osno     = da_h2osno da + h2osno
    , da_qrunoff    = da_qrunoff da + ss_qflx_runoff st
    , da_snow_depth = da_snow_depth da + ss_snow_depth st
    , da_frac_sno   = da_frac_sno da + ss_frac_sno st
    }

avgDailyAvg :: DailyAvg -> Int -> DailyAvg
avgDailyAvg da n =
  let d = fromIntegral n
  in DailyAvg
    { da_t_grnd     = da_t_grnd da / d
    , da_tsa        = da_tsa da / d
    , da_fsa        = da_fsa da / d
    , da_eflx_lh    = da_eflx_lh da / d
    , da_eflx_sh    = da_eflx_sh da / d
    , da_h2osno     = da_h2osno da / d
    , da_qrunoff    = da_qrunoff da / d
    , da_snow_depth = da_snow_depth da / d
    , da_frac_sno   = da_frac_sno da / d
    }

-- ============================================================================
-- Single timestep
-- ============================================================================

-- | Run one timestep of the CLM physics pipeline.
-- Takes current state and a forcing timestep, returns updated state.
stepSimulation :: SimState -> Int -> Double -> SimState
stepSimulation !st nstep dtime =
  let -- Unpack forcing
      -- Both Julia and Haskell read one hourly forcing record per half-hourly
      -- timestep (Julia uses fr.times[step] which indexes hourly NetCDF directly).
      -- So each simulated "day" (48 steps) consumes 48 hourly forcing records.
      fr = ss_forcing st
      (fts, _fr') = readForcingStepPure fr (nstep - 1)  -- 0-based index

      -- Atmospheric forcing
      forc_t    = ft_tbot fts
      forc_pbot = ft_psrf fts
      forc_wind = ft_wind fts
      forc_lwrad = ft_flds fts
      forc_fsds = ft_fsds fts
      forc_precip = ft_precip fts
      forc_qbot = ft_qbot fts

      -- Derived forcing
      forc_vp  = computeVaporPressureFromQ forc_qbot forc_pbot
      forc_th  = computePotentialTemperature forc_t forc_pbot
      forc_rho = computeAirDensity forc_pbot forc_t forc_vp
      forc_q   = forc_qbot  -- already specific humidity
      (forc_rain, forc_snow) = partitionPrecip forc_t forc_precip
      (forc_solad_vis, forc_solad_nir, forc_solai_vis, forc_solai_nir) =
        splitShortwaveBands forc_fsds

      -- Reference heights
      forc_hgt = 30.0   -- default observation height [m]

      -- Calendar day for orbital computation
      calday = fromIntegral (nstep - 1) * dtime / 86400.0 + 1.0
      (declin, eccf) = computeOrbital defaultOrbitalParams calday

      -- ====================================================================
      -- Step 1: Pre-flux calculations
      -- ====================================================================
      snl = ss_snl st
      joff = nlevsno - 1  -- 0-based offset
      nlevtot = nlevsno + nlevgrnd

      -- Top layer index (Fortran: snl+1 → 0-based: nlevsno + snl)
      topLayerIdx = nlevsno + snl  -- 0-based
      t_top = ss_t_soisno st VU.! topLayerIdx
      t_soil1 = ss_t_soisno st VU.! nlevsno  -- first soil layer

      -- Ground surface properties
      t_grnd = ss_t_grnd st
      emg = 0.96  -- ground emissivity for soil
      htvp = if t_grnd < tfrz then hsub else hvap

      -- Saturation humidity at ground
      qr_grnd = qsat t_grnd forc_pbot
      -- Soil evaporation beta factor: reduces evaporation for dry/frozen soil
      -- CLM computes this from soil moisture relative to saturation
      -- For frozen soil: very low evaporation (liquid fraction near zero)
      soilbeta = if t_grnd < tfrz
                 then 0.01  -- frozen: minimal evaporation
                 else 1.0   -- saturated unfrozen soil-column fallback
      qg = soilbeta * qsr_qs qr_grnd
      qr_snow = qsat (min t_grnd tfrz) forc_pbot
      qg_snow = qsr_qs qr_snow
      qr_soil = qsat t_soil1 forc_pbot
      qg_soil = soilbeta * qsr_qs qr_soil
      qr_h2osfc = qsat (ss_t_h2osfc st) forc_pbot
      qg_h2osfc = qsr_qs qr_h2osfc
      dqgdT = qsr_dqsdT qr_grnd

      -- Virtual potential temperature
      -- thm = forc_t adjusted to surface using dry adiabatic lapse rate (CLM convention)
      -- Julia: thm[p] = forc_t[c] + 0.0098 * forc_hgt_t_patch[p]
      thm = forc_t + 0.0098 * forc_hgt
      thv = forc_th * (1.0 + 0.61 * forc_q)

      -- Ground roughness
      z0mg = 0.01  -- default bare soil roughness [m]

      -- ====================================================================
      -- Step 2: Simple radiation estimate with snow albedo
      -- ====================================================================
      -- Snow-dependent ground albedo
      -- CLM soil color tables: Bow-at-Banff is soil color 15 (20-class):
      --   dry VIS=0.18, dry NIR=0.29; sat VIS=0.09, sat NIR=0.18
      --   Frozen soil in winter → dry conditions → use dry values
      -- Average (VIS+NIR)/2 ≈ 0.235
      albedo_soil = 0.235  -- dry soil color 15 (average VIS+NIR)
      albedo_snow = 0.85  -- snow albedo (lower than fresh: aged snow)
      frac_sno_cur = ss_frac_sno st
      albedo_ground = (1.0 - frac_sno_cur) * albedo_soil
                    + frac_sno_cur * albedo_snow
      sabg_bare = (1.0 - albedo_ground) * forc_fsds  -- absorbed by bare ground

      -- For vegetated patches, canopy intercepts some SW.
      -- sabg_veg = transmitted * (1-albedo_ground) where transmitted ≈ exp(-0.5 * vai)
      -- sabv_veg = (1-transmitted) * (1-albedo_canopy) * forc_fsds
      -- fsa = sum of sabg + sabv contributions (per-patch weighted)
      -- Computed after patch flux loop using proper per-patch sabg + sabv

      -- ====================================================================
      -- Step 3: Bareground fluxes (patch 0)
      -- ====================================================================
      bgInp = BareGroundFluxesInput
        { bgi_params         = defaultBareGroundFluxesParams
        , bgi_z0param_method = ZengWang2007
        , bgi_forc_q         = forc_q
        , bgi_forc_pbot      = forc_pbot
        , bgi_forc_th        = forc_th
        , bgi_forc_rho       = forc_rho
        , bgi_forc_t         = forc_t
        , bgi_forc_u         = forc_wind
        , bgi_forc_v         = 0.0
        , bgi_forc_hgt_t     = forc_hgt
        , bgi_forc_hgt_u     = forc_hgt
        , bgi_forc_hgt_q     = forc_hgt
        , bgi_t_grnd         = t_grnd
        , bgi_thm            = thm
        , bgi_qg             = qg
        , bgi_qg_snow        = qg_snow
        , bgi_qg_soil        = qg_soil
        , bgi_qg_h2osfc      = qg_h2osfc
        , bgi_dqgdT          = dqgdT
        , bgi_thv            = thv
          -- Fortran beta_col = 1.0 (coefficient of convective velocity),
          -- distinct from soilbeta (soil evaporation factor, bgi_soilbeta).
        , bgi_beta           = 1.0
        , bgi_zii            = 1000.0
        , bgi_t_h2osfc       = ss_t_h2osfc st
        , bgi_t_soisno_top   = t_top
        , bgi_t_soil1        = t_soil1
        , bgi_z0mg           = z0mg
        , bgi_z0hg           = z0mg
        , bgi_z0qg           = z0mg
        , bgi_zetamaxstable  = 0.5
        , bgi_soilbeta       = soilbeta
        , bgi_soilresis      = 0.0
        , bgi_do_soilevap_beta = True
        , bgi_do_soil_resistance = False
        , bgi_htvp           = htvp
        , bgi_h2osoi_liq_top = ss_h2osoi_liq st VU.! topLayerIdx
        , bgi_h2osoi_ice_top = ss_h2osoi_ice st VU.! topLayerIdx
        , bgi_dz_top         = ss_dz st VU.! topLayerIdx
        , bgi_watsat_top     = if topLayerIdx >= nlevsno
                               then ss_watsat st VU.! (topLayerIdx - nlevsno)
                               else 1.0
        }
      bgOut = baregroundFluxes bgInp

      -- ====================================================================
      -- Step 4: Per-patch fluxes (bareground + canopy)
      -- ====================================================================
      np = VU.length (ss_pch_wtcol st)
      pftcon = ap_pftcon (ss_params st)

      -- Per-patch vegetation absorbed shortwave
      -- sabv = (1 - canopy_albedo) * (1 - transmitted) * forc_fsds
      -- where transmitted = exp(-0.5*vai), canopy_albedo ≈ 0.15 for conifers
      -- Computed per-patch in the canopy flux input below (sabv_p)

      -- Per-patch flux computation (boxed vector for tuple storage)
      -- DIAGNOSTIC: force all patches to bare ground (disable canopy)
      _forceAllBare = False  -- set True to test canopy vs bare ground
      patchFluxes = V.generate np (\p ->
        let wtcol_p = ss_pch_wtcol st VU.! p
            fveg = if _forceAllBare then 0 else ss_frac_veg_nosno st VU.! p
            itype_p = ss_pch_itype st VU.! p
            elai_p_check = ss_elai st VU.! p
        in if wtcol_p <= 0.0
           then (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
                 (bgo_t_ref2m bgOut, ss_t_veg st VU.! p))
           else if fveg == 0 || itype_p == 0
           then -- Bare ground patch: use bareground fluxes, no canopy
                ( bgo_eflx_sh_grnd bgOut, bgo_eflx_sh_snow bgOut
                , bgo_eflx_sh_soil bgOut, bgo_eflx_sh_h2osfc bgOut
                , bgo_qflx_evap_soi bgOut, bgo_cgrnd bgOut
                , bgo_cgrnds bgOut, bgo_cgrndl bgOut
                , 0.0, bgo_eflx_sh_tot bgOut
                , (bgo_t_ref2m bgOut, ss_t_veg st VU.! p))
           else if elai_p_check <= 0.05
           then -- Vegetated patch with no leaves (deciduous winter):
                -- Use bareground fluxes but compute canopy LW from stems
                let esai_bare = ss_esai st VU.! p
                    vai_bare = elai_p_check + esai_bare
                    emv_bare = 1.0 - exp (negate vai_bare)
                    -- dlrad = (1-emv)*emg*forc_lwrad + emv*emg*sb*Tstem^4
                    -- Stems cool radiatively below air temp; use blend with ground temp
                    t_stem_est = 0.5 * forc_t + 0.5 * t_grnd
                    dlrad_bare = (1.0 - emv_bare) * emg * forc_lwrad
                               + emv_bare * emg * sb * t_stem_est ** 4
                in ( bgo_eflx_sh_grnd bgOut, bgo_eflx_sh_snow bgOut
                   , bgo_eflx_sh_soil bgOut, bgo_eflx_sh_h2osfc bgOut
                   , bgo_qflx_evap_soi bgOut, bgo_cgrnd bgOut
                   , bgo_cgrnds bgOut, bgo_cgrndl bgOut
                   , dlrad_bare, bgo_eflx_sh_tot bgOut
                   , (bgo_t_ref2m bgOut, ss_t_veg st VU.! p))
           else -- Vegetated patch: call canopy fluxes
                let elai_p = ss_elai st VU.! p
                    esai_p = ss_esai st VU.! p
                    htop_p = max 0.1 (ss_htop st VU.! p)
                    -- PFT properties (itype_p is 1-based PFT index)
                    pftIdx = itype_p  -- PFT index (0=bare, 1=NET temp, ...)
                    z0mr_p = if VU.null (pft_z0mr pftcon) then 0.055
                             else pft_z0mr pftcon VU.! (min pftIdx (VU.length (pft_z0mr pftcon) - 1))
                    displar_p = if VU.null (pft_displar pftcon) then 0.67
                                else pft_displar pftcon VU.! (min pftIdx (VU.length (pft_displar pftcon) - 1))
                    dleaf_p = if VU.null (pft_dleaf pftcon) then 0.04
                              else pft_dleaf pftcon VU.! (min pftIdx (VU.length (pft_dleaf pftcon) - 1))
                    -- Derived canopy geometry
                    z0mv_p = z0mr_p * htop_p
                    displa_p = displar_p * htop_p
                    -- Split LAI into sunlit/shaded (simple: 50/50)
                    laisun_p = elai_p * 0.5
                    laisha_p = elai_p * 0.5
                    -- Vegetation emissivity
                    avmuir = 1.0  -- unit leaf area projected to horizontal
                    emv_p = 1.0 - exp (-(elai_p + esai_p) / avmuir)
                    -- Per-patch sabv: canopy-intercepted shortwave
                    -- CLM two-stream: snow under canopy raises effective system albedo
                    -- Simple approach: total FSA per patch from system albedo,
                    -- then subtract sabg to get sabv
                    vai_p = elai_p + esai_p
                    canopy_transmit_p = exp (-0.5 * vai_p)
                    -- System albedo blend: no-snow ≈ 0.25, full snow ≈ 0.95
                    -- Two-stream at high zenith angles (winter 51°N) gives higher
                    -- canopy albedo than the nadir value (~0.12). Multiple scattering
                    -- between canopy and light mountain soil also increases albedo.
                    sys_alb = (1.0 - frac_sno_cur) * 0.25 + frac_sno_cur * 0.95
                    fsa_p = (1.0 - sys_alb) * forc_fsds
                    sabg_veg_p = canopy_transmit_p * sabg_bare
                    sabv_p = max 0.0 (fsa_p - sabg_veg_p)
                    -- Initial cgrnd estimates
                    cgrnds0 = forc_rho * cpair / 100.0  -- rough estimate
                    cgrndl0 = forc_rho / 100.0 * dqgdT
                    cfInput = CanopyFluxesInput
                      { cfi_forc_lwrad     = forc_lwrad
                      , cfi_forc_q         = forc_q
                      , cfi_forc_pbot      = forc_pbot
                      , cfi_forc_th        = forc_th
                      , cfi_forc_rho       = forc_rho
                      , cfi_forc_t         = forc_t
                      , cfi_forc_u         = forc_wind
                      , cfi_forc_v         = 0.0
                      , cfi_forc_hgt_u     = forc_hgt
                      , cfi_forc_hgt_t     = forc_hgt
                      , cfi_forc_hgt_q     = forc_hgt
                      , cfi_elai           = elai_p
                      , cfi_esai           = esai_p
                      , cfi_htop           = htop_p
                      , cfi_displa         = displa_p
                      , cfi_z0mv           = z0mv_p
                      , cfi_z0mg           = z0mg
                      , cfi_frac_veg_nosno = 1
                      , cfi_emv            = emv_p
                      , cfi_emg            = emg
                      , cfi_t_veg          = ss_t_veg st VU.! p
                      , cfi_t_grnd         = t_grnd
                      , cfi_thm            = thm
                      , cfi_thv            = thv
                      , cfi_t_soisno_top   = t_top
                      , cfi_t_soisno_topsoil = t_soil1
                      , cfi_t_h2osfc       = ss_t_h2osfc st
                      , cfi_t_stem         = ss_t_veg st VU.! p
                      , cfi_sabv           = sabv_p
                      , cfi_qg             = qg
                      , cfi_qg_snow        = qg_snow
                      , cfi_qg_soil        = qg_soil
                      , cfi_qg_h2osfc      = qg_h2osfc
                      , cfi_dqgdT          = dqgdT
                      , cfi_frac_sno_eff   = ss_frac_sno_eff st
                      , cfi_frac_h2osfc    = ss_frac_h2osfc st
                      , cfi_snow_depth     = ss_snow_depth st
                      , cfi_fwet           = 0.0   -- dry canopy
                      , cfi_fdry           = 1.0
                      , cfi_liqcan         = 0.0
                      , cfi_snocan         = 0.0
                      , cfi_rssun          = 200.0  -- prescribed stomatal resistance
                      , cfi_rssha          = 200.0
                      , cfi_laisun         = laisun_p
                      , cfi_laisha         = laisha_p
                      , cfi_btran          = 1.0    -- no water stress
                      , cfi_soilbeta       = soilbeta
                      , cfi_soilresis      = 0.0
                      , cfi_htvp           = htvp
                      , cfi_cgrnds         = cgrnds0
                      , cfi_cgrndl         = cgrndl0
                      , cfi_do_soilevap_beta = True
                      , cfi_dtime          = dtime
                      , cfi_zetamaxstable  = 0.5
                      , cfi_dleaf          = dleaf_p
                      , cfi_snl            = snl
                      }
                    cfOut = canopyFluxes defaultCanopyFluxesParams
                              defaultCanopyFluxesControl cfInput
                    sh_tot_p = cfo_eflx_sh_veg cfOut + cfo_eflx_sh_grnd cfOut
                in ( cfo_eflx_sh_grnd cfOut
                   , cfo_eflx_sh_snow cfOut
                   , cfo_eflx_sh_soil cfOut
                   , cfo_eflx_sh_h2osfc cfOut
                   , cfo_qflx_evap_soi cfOut
                   , cfo_cgrnd cfOut
                   , cfo_cgrnds cfOut
                   , cfo_cgrndl cfOut
                   , cfo_dlrad cfOut
                   , sh_tot_p
                   , (cfo_t_ref2m cfOut, cfo_t_veg cfOut)
                   ))

      -- Extract per-patch vectors for diagnostics
      eflx_sh_tot_new = VU.generate np (\p ->
        let (_, _, _, _, _, _, _, _, _, sh_tot, _) = patchFluxes V.! p in sh_tot)
      eflx_lh_tot_new = VU.generate np (\p ->
        let (_, _, _, _, qevap, _, _, _, _, _, _) = patchFluxes V.! p in qevap * htvp)
      t_ref2m_new = VU.generate np (\p ->
        let (_, _, _, _, _, _, _, _, _, _, (t2m, _)) = patchFluxes V.! p in t2m)
      t_veg_new = VU.generate np (\p ->
        let (_, _, _, _, _, _, _, _, _, _, (_, tveg)) = patchFluxes V.! p in tveg)

      -- FSA diagnostic: total absorbed shortwave per patch, weighted
      -- Uses system-level albedo that accounts for canopy+snow interactions
      fsa = sum [ let fveg_i = ss_frac_veg_nosno st VU.! i
                      sys_alb_i = if fveg_i /= 0
                                  then (1.0 - frac_sno_cur) * 0.25 + frac_sno_cur * 0.95
                                  else albedo_ground
                  in (1.0 - sys_alb_i) * forc_fsds * ss_pch_wtcol st VU.! i
                | i <- [0..np-1]
                , ss_pch_column st VU.! i == soilColIdx ]

      -- Bare ground patch FSA (patch 0): no canopy, just ground absorption
      fsa_patch0 = sabg_bare  -- (1 - albedo_ground) * forc_fsds

      -- ====================================================================
      -- Step 5: Aggregate patch fluxes → column heat source terms
      -- ====================================================================
      lwrad_emit = emg * sb * t_grnd ** 4
      dlwrad_emit = 4.0 * emg * sb * t_grnd ** 3

      -- Weighted aggregation across patches (soil column = column 1 in 1-based)
      -- Julia formula: hs_top[c] += eflx_gnet_top[p] * wtcol[p]
      --   eflx_gnet_top = sabg + dlrad + emg*lwrad - lwrad_emit - (sh_grnd + qevap*htvp)
      soilColIdx = 1 :: Int  -- soil column is index 1 (1-based)
      (hs_top_agg, hs_soil_agg, hs_h2osfc_agg, dhsdT_agg) =
        let go !hs !hss !hsh !dhs i
              | i >= np = (hs, hss, hsh, dhs)
              | ss_pch_column st VU.! i /= soilColIdx =
                  go hs hss hsh dhs (i + 1)  -- skip non-soil patches
              | otherwise =
                  let wtcol_p = ss_pch_wtcol st VU.! i
                      fveg_i = ss_frac_veg_nosno st VU.! i
                      (sh_grnd, _sh_snow, sh_soil, sh_h2osfc,
                       qevap, cgrnd_p, _cgrnds, _cgrndl, dlrad_p, _, _) =
                        patchFluxes V.! i
                      -- Per-patch ground absorbed shortwave:
                      -- Bare ground: full absorption
                      -- Vegetated: canopy intercepts, only exp(-0.5*vai) reaches ground
                      fveg_d = fromIntegral fveg_i :: Double
                      vai_p = ss_elai st VU.! i + ss_esai st VU.! i
                      canopy_transmit = if fveg_i /= 0 then exp (-0.5 * vai_p) else 1.0
                      sabg_p = canopy_transmit * sabg_bare
                      -- Julia formula: sabg + dlrad + (1-frac_veg_nosno)*emg*lwrad - lwup - (SH+LH)
                      atm_lw = (1.0 - fveg_d) * emg * forc_lwrad
                      eflx_gnet_top = sabg_p + dlrad_p + atm_lw
                                    - lwrad_emit - (sh_grnd + qevap * htvp)
                      eflx_gnet_soil = sabg_p + dlrad_p + atm_lw
                                     - lwrad_emit - (sh_soil + qevap * htvp)
                      eflx_gnet_h2osfc = sabg_p + dlrad_p + atm_lw
                                       - lwrad_emit - (sh_h2osfc + qevap * htvp)
                      dgnet_p = -(cgrnd_p) - dlwrad_emit
                  in go (hs + eflx_gnet_top * wtcol_p)
                        (hss + eflx_gnet_soil * wtcol_p)
                        (hsh + eflx_gnet_h2osfc * wtcol_p)
                        (dhs + dgnet_p * wtcol_p)
                        (i + 1)
        in go 0.0 0.0 0.0 0.0 0

      -- Debug trace: per-patch energy budget
      _debugTrace = if False
        then let showPatch i =
                   let (sh_g, _, sh_s, _, qe, cgr, _, _, dl, sh_t, _) = patchFluxes V.! i
                       fveg_i = ss_frac_veg_nosno st VU.! i
                       fvd = fromIntegral fveg_i :: Double
                       vai_p = ss_elai st VU.! i + ss_esai st VU.! i
                       ct = if fveg_i /= 0 then exp (-0.5 * vai_p) else 1.0
                       sabg_pi = ct * sabg_bare
                       atm_lw_i = (1.0 - fvd) * emg * forc_lwrad
                       gnet_i = sabg_pi + dl + atm_lw_i - lwrad_emit - (sh_g + qe * htvp)
                       wt = ss_pch_wtcol st VU.! i
                   in "  p" ++ show i ++ "(fveg=" ++ show fveg_i
                      ++ ",wt=" ++ show wt
                      ++ "): sabg=" ++ show sabg_pi
                      ++ " dlrad=" ++ show dl
                      ++ " atm_lw=" ++ show atm_lw_i
                      ++ " lwemit=" ++ show lwrad_emit
                      ++ " sh_g=" ++ show sh_g
                      ++ " qe*hvp=" ++ show (qe * htvp)
                      ++ " gnet=" ++ show gnet_i
                      ++ " sh_tot=" ++ show sh_t
             in trace ("step " ++ show nstep ++ ": hs_top=" ++ show hs_top_agg
                       ++ " t_grnd=" ++ show t_grnd ++ " forc_t=" ++ show forc_t
                       ++ " forc_th=" ++ show forc_th ++ " forc_rho=" ++ show forc_rho
                       ++ " forc_wind=" ++ show forc_wind ++ " forc_pbot=" ++ show forc_pbot
                       ++ " forc_q=" ++ show forc_q
                       ++ "\n  BG: ustar=" ++ show (bgo_ustar bgOut)
                       ++ " ram1=" ++ show (bgo_ram1 bgOut)
                       ++ " cgrnds=" ++ show (bgo_cgrnds bgOut)
                       ++ " sh_grnd=" ++ show (bgo_eflx_sh_grnd bgOut)
                       ++ " dth0=" ++ show (forc_th - t_grnd)
                       ++ "\n" ++ concatMap (\i -> showPatch i ++ "\n") [0..np-1]) ()
        else ()

      -- sabg_lyr: current single-layer soil absorption path
      -- Use weighted average sabg across soil column patches
      sabg_agg = sum [ let vai_p = ss_elai st VU.! i + ss_esai st VU.! i
                           fveg_i = ss_frac_veg_nosno st VU.! i
                           ct = if fveg_i /= 0 then exp (-0.5 * vai_p) else 1.0
                       in ct * sabg_bare * ss_pch_wtcol st VU.! i
                     | i <- [0..np-1]
                     , ss_pch_column st VU.! i == soilColIdx ]
      nlyr_sabg = nlevsno + 1
      sabg_lyr = VU.generate nlyr_sabg (\j ->
        if j == 0 then sabg_agg else 0.0)

      -- ====================================================================
      -- Step 6: Soil temperature
      -- ====================================================================
      stInput = SoilTempInput
        { sti_snl              = snl
        , sti_t_soisno         = ss_t_soisno st
        , sti_t_grnd           = t_grnd
        , sti_t_h2osfc         = ss_t_h2osfc st
        , sti_h2osoi_liq       = ss_h2osoi_liq st
        , sti_h2osoi_ice       = ss_h2osoi_ice st
        , sti_dz               = ss_dz st
        , sti_z                = ss_z st
        , sti_zi               = ss_zi st
        , sti_watsat           = ss_watsat st
        , sti_bsw              = ss_bsw st
        , sti_sucsat           = ss_sucsat st
        , sti_tkmg             = ss_tkmg st
        , sti_tkdry            = ss_tkdry st
        , sti_csol             = ss_csol st
        , sti_tksatu           = ss_tksatu st
        , sti_nbedrock         = ss_nbedrock st
        , sti_h2osno_no_layers = ss_h2osno_no_layers st
        , sti_h2osfc           = ss_h2osfc st
        , sti_frac_sno_eff     = ss_frac_sno_eff st
        , sti_frac_h2osfc      = ss_frac_h2osfc st
        , sti_snow_depth       = ss_snow_depth st
        , sti_hs_top           = hs_top_agg
        , sti_dhsdT            = dhsdT_agg
        , sti_hs_soil          = hs_soil_agg
        , sti_hs_h2osfc        = hs_h2osfc_agg
        , sti_sabg_lyr         = sabg_lyr
        , sti_eflx_bot         = 0.0
        , sti_dtime            = dtime
        , sti_snowCondMethod   = Jordan1991
        , sti_thk_override     = Nothing
        , sti_cv_override      = Nothing
        }
      stOutput = solveSoilTemperature stInput


      -- ====================================================================
      -- Step 7: Snow accumulation with Swenson-Lawrence snow cover fraction
      -- ====================================================================
      new_snow_mass = forc_snow * dtime  -- kg/m2 of new snow
      -- CLM fresh snow density: bifall = 50 + 1.7*(max(0,T-tfrz+15))^1.5
      bifall = 50.0 + 1.7 * (max 0.0 (forc_t - tfrz + 15.0)) ** 1.5

      -- Sublimation from evaporation
      qflx_evap_soi_agg = sum [ qe * w
                               | i <- [0..np-1]
                               , ss_pch_column st VU.! i == soilColIdx
                               , let (_, _, _, _, qe, _, _, _, _, _, _) = patchFluxes V.! i
                               , let w = ss_pch_wtcol st VU.! i ]

      -- Soil temperature solver outputs (preserving phase change)
      t_soisno_solver = sto_t_soisno stOutput
      h2osoi_liq_solver = sto_h2osoi_liq stOutput
      h2osoi_ice_solver = sto_h2osoi_ice stOutput

      snl_cur = ss_snl st  -- current number of snow layers (<=0)
      snow_dzmin_1 = 0.010 :: Double  -- CLM threshold for first layer [m]

      -- Total snow water equivalent (resolved layers + unresolved)
      h2osno_total_prev = (sum [ h2osoi_ice_solver VU.! j + h2osoi_liq_solver VU.! j
                                | j <- [nlevsno + snl_cur .. nlevsno - 1] ])
                        + ss_h2osno_no_layers st

      -- Swenson-Lawrence accumulation factor
      accum_factor = 0.1 :: Double

      -- Step 7a: Update frac_sno using Swenson-Lawrence parameterization
      frac_sno_sl =
        if h2osno_total_prev == 0.0
        then if new_snow_mass > 0.0
             then tanh (accum_factor * new_snow_mass)
             else 0.0
        else -- existing snow
          let base_fsno = frac_sno_cur
          in if new_snow_mass > 0.0
             then base_fsno + tanh (accum_factor * new_snow_mass) * (1.0 - base_fsno)
             else base_fsno

      frac_sno_eff_sl = frac_sno_sl  -- same for non-urban

      -- Step 7b: Update snow_depth using Swenson-Lawrence
      -- snow_depth represents depth on the snow-covered fraction
      snow_depth_sl =
        if h2osno_total_prev == 0.0
        then if new_snow_mass > 0.0 && frac_sno_eff_sl > 0.0
             then (new_snow_mass / bifall) / frac_sno_eff_sl
             else 0.0
        else -- existing snow
          if new_snow_mass > 0.0 && frac_sno_eff_sl > 0.0
          then ss_snow_depth st + new_snow_mass / (bifall * frac_sno_eff_sl)
          else ss_snow_depth st

      -- Actual layer thickness increase from new snow (physical, not S-L diagnostic)
      new_snow_depth = if new_snow_mass > 0.0 then new_snow_mass / bifall else 0.0

      -- Phase 1: Add new snowfall mass to state
      (h2osno_nl_1, t_soisno_1, liq_1, ice_1, dz_1, z_1, zi_1) =
        if snl_cur < 0 && new_snow_mass > 0.0
        then
          let topIdx = nlevsno + snl_cur
              ice_upd = h2osoi_ice_solver VU.// [(topIdx, h2osoi_ice_solver VU.! topIdx + new_snow_mass)]
              -- Add physical depth change to top layer dz (mass/density, not S-L)
              dz_add = new_snow_depth
              dz_upd = ss_dz st VU.// [(topIdx, ss_dz st VU.! topIdx + dz_add)]
              z_upd = ss_z st VU.// [(topIdx, ss_z st VU.! topIdx - dz_add / 2.0)]
              zi_upd = ss_zi st VU.// [(topIdx, ss_zi st VU.! topIdx - dz_add)]
          in (0.0, t_soisno_solver, h2osoi_liq_solver, ice_upd, dz_upd, z_upd, zi_upd)
        else
          (ss_h2osno_no_layers st + new_snow_mass,
           t_soisno_solver, h2osoi_liq_solver, h2osoi_ice_solver,
           ss_dz st, ss_z st, ss_zi st)

      -- Phase 2: Snow sublimation (only snow fraction of evaporation)
      snow_sublim_mass = max 0.0 (frac_sno_sl * qflx_evap_soi_agg * dtime)

      (h2osno_nl_2, sd_2, t_soisno_2, liq_2, ice_2, dz_2, z_2, zi_2, snl_2, frac_sno_2) =
        if snl_cur < 0
        then
          -- Remove sublimation from top resolved layer
          let topIdx = nlevsno + snl_cur
              oldIce = ice_1 VU.! topIdx
              oldLiq = liq_1 VU.! topIdx
              totalMass = oldIce + oldLiq
              sublim = min snow_sublim_mass totalMass
              newIce = max 0.0 (oldIce - sublim)
              newLiq = if newIce <= 0.0 then max 0.0 (oldLiq - (sublim - oldIce)) else oldLiq
              -- Reduce dz proportionally to mass removed
              dz_old = dz_1 VU.! topIdx
              frac_removed = if totalMass > 0.0 then sublim / totalMass else 0.0
              newDz = max 0.0 (dz_old * (1.0 - frac_removed))
              -- If layer mass is gone, remove the layer
              layerGone = newIce + newLiq < 1.0e-10
              snl_adj = if layerGone then snl_cur + 1 else snl_cur
          in if layerGone
             then (0.0, 0.0,
                   t_soisno_1, liq_1, ice_1, dz_1, z_1, zi_1, snl_adj, 0.0)
             else (0.0, snow_depth_sl,
                   t_soisno_1, liq_1 VU.// [(topIdx, newLiq)],
                   ice_1 VU.// [(topIdx, newIce)],
                   dz_1 VU.// [(topIdx, newDz)],
                   z_1, zi_1, snl_cur, frac_sno_sl)
        else
          -- Remove from unresolved
          let sublim = min snow_sublim_mass h2osno_nl_1
              nl_new = max 0.0 (h2osno_nl_1 - sublim)
              sd_adj = if nl_new <= 0.0 then 0.0 else snow_depth_sl
              fs_adj = if nl_new <= 0.0 then 0.0 else frac_sno_sl
          in (nl_new, sd_adj,
              t_soisno_1, liq_1, ice_1, dz_1, z_1, zi_1, 0, fs_adj)

      -- Phase 3: Create first resolved layer if enough unresolved snow
      shouldCreateLayer = snl_2 == 0 && h2osno_nl_2 > 0.0
                       && frac_sno_2 > 0.0
                       && sd_2 * frac_sno_2 >= snow_dzmin_1

      (snl_3, h2osno_nl_3, t_soisno_3, liq_3, ice_3, dz_3, z_3, zi_3) =
        if shouldCreateLayer
        then
          let layerIdx = nlevsno - 1  -- index 11 for snl=-1 (single snow layer)
              snow_t = min tfrz forc_t
              -- Layer thickness = physical depth from mass and density
              -- NOT the S-L diagnostic snow_depth
              layer_dz = h2osno_nl_2 / bifall
              -- Initialize the single snow layer
              t_new = t_soisno_2 VU.// [(layerIdx, snow_t)]
              liq_new = liq_2 VU.// [(layerIdx, 0.0)]
              ice_new = ice_2 VU.// [(layerIdx, h2osno_nl_2)]
              dz_new = dz_2 VU.// [(layerIdx, layer_dz)]
              z_new = z_2 VU.// [(layerIdx, -0.5 * layer_dz)]
              zi_new = zi_2 VU.// [(layerIdx, -layer_dz)]
          in (-1, 0.0, t_new, liq_new, ice_new, dz_new, z_new, zi_new)
        else
          (snl_2, h2osno_nl_2, t_soisno_2, liq_2, ice_2, dz_2, z_2, zi_2)

      -- Phase 4: Snow compaction (Anderson 1976)
      -- Apply destructive metamorphism + overburden compaction to each snow layer
      (dz_4, z_4, zi_4) =
        if snl_3 < 0
        then
          let -- Constants for Anderson 1976 compaction
              c3 = 2.777e-6 :: Double   -- destructive metamorphism rate [1/s]
              c4 = 0.04 :: Double        -- temperature factor for metamorphism [1/K]
              c5 = 2.0 :: Double         -- liquid water enhancement factor
              c2_ob = 23.0e-3 :: Double  -- overburden pressure coefficient [m3/kg]
              eta0 = 9.0e5 :: Double     -- Anderson 1976 viscosity [kg s/m2]
              overburden_tfactor = 0.08 :: Double  -- temperature factor for overburden [1/K]
              uplim_destruct = 100.0 :: Double     -- density limit for metamorphism [kg/m3]

              -- Apply compaction to each snow layer from top to bottom
              -- Accumulate overburden as we go down
              compactLayers burden jj dz_acc
                | jj > nlevsno - 1 = dz_acc  -- past bottom snow layer
                | otherwise =
                    let dz_j = dz_acc VU.! jj
                        ice_j = ice_3 VU.! jj
                        liq_j = liq_3 VU.! jj
                        t_j = t_soisno_3 VU.! jj
                        wx = ice_j + liq_j  -- total water content [kg/m2]
                        td = max 0.0 (tfrz - t_j)  -- temperature deficit [K]
                        bi = if frac_sno_2 > 0.0 && dz_j > 0.0
                             then ice_j / (frac_sno_2 * dz_j)  -- bulk ice density [kg/m3]
                             else 0.0

                        -- 1. Destructive metamorphism
                        ddz1_base = negate c3 * exp (negate c4 * td)
                        ddz1_dens = if bi > uplim_destruct
                                    then ddz1_base * exp (negate 46.0e-3 * (bi - uplim_destruct))
                                    else ddz1_base
                        ddz1 = if liq_j > 0.01 * dz_j * frac_sno_2
                               then ddz1_dens * c5
                               else ddz1_dens

                        -- 2. Overburden compaction (Anderson 1976)
                        ddz2 = negate (burden + wx / 2.0)
                             * exp (negate overburden_tfactor * td - c2_ob * bi)
                             / eta0

                        -- Total compaction rate
                        pdzdtc = ddz1 + ddz2

                        -- Apply compaction
                        dz_new_raw = dz_j * (1.0 + pdzdtc * dtime)
                        -- Minimum dz from water content
                        dz_min = if frac_sno_2 > 0.0
                                 then (ice_j / denice + liq_j / denh2o) / frac_sno_2
                                 else 0.0
                        dz_new = max dz_min dz_new_raw

                        -- Update burden for next layer
                        burden' = burden + wx

                    in compactLayers burden' (jj + 1) (dz_acc VU.// [(jj, dz_new)])

              topSnowIdx = nlevsno + snl_3
              dz_compacted = compactLayers 0.0 topSnowIdx dz_3

              -- Recompute z and zi from compacted dz
              -- zi[nlevsno] = 0.0 (surface), working upward for snow
              zi_new = VU.generate (nlevsno + nlevgrnd + 1) $ \k ->
                if k > nlevsno then zi_3 VU.! k  -- soil interfaces unchanged
                else if k == nlevsno then 0.0     -- surface
                else -- snow interface: zi[k] = zi[k+1] - dz[k]
                     -- Need to compute from bottom (surface) up
                     -- zi[nlevsno-1] = 0 - dz[nlevsno-1]
                     -- zi[nlevsno-2] = zi[nlevsno-1] - dz[nlevsno-2]
                     -- etc.
                     let depth_below = sum [ dz_compacted VU.! j | j <- [k .. nlevsno - 1] ]
                     in negate depth_below

              z_new = VU.generate (nlevsno + nlevgrnd) $ \k ->
                if k >= nlevsno then z_3 VU.! k  -- soil midpoints unchanged
                else -- snow midpoint = (zi[k] + zi[k+1]) / 2
                     -- = (zi_new[k] + zi_new[k+1]) / 2
                     0.5 * (zi_new VU.! k + zi_new VU.! (k + 1))

          in (dz_compacted, z_new, zi_new)
        else (dz_3, z_3, zi_3)

      snl_4 = snl_3
      t_soisno_4 = t_soisno_3
      liq_4 = liq_3
      ice_4 = ice_3

      -- Recompute snow_depth from compacted layer dz (sum of all snow layer dz)
      snow_depth_final = if snl_4 < 0
                         then sum [ dz_4 VU.! j | j <- [nlevsno + snl_4 .. nlevsno - 1] ]
                         else sd_2  -- no layers: use S-L diagnostic
      frac_sno_new = frac_sno_2

      -- ====================================================================
      -- Update state
      -- ====================================================================
      t_grnd_new = if snl_4 < 0
                   then t_soisno_4 VU.! (nlevsno + snl_4)  -- top snow layer temp
                   else sto_t_grnd stOutput
      t_veg_final = t_veg_new

  in _debugTrace `seq` st
    { ss_t_soisno     = t_soisno_4
    , ss_t_grnd       = t_grnd_new
    , ss_t_h2osfc     = sto_t_h2osfc stOutput
    , ss_h2osoi_liq   = liq_4
    , ss_h2osoi_ice   = ice_4
    , ss_h2osno_no_layers = h2osno_nl_3
    , ss_snow_depth   = snow_depth_final
    , ss_frac_sno     = frac_sno_new
    , ss_frac_sno_eff = frac_sno_new
    , ss_snl          = snl_4
    , ss_dz           = dz_4
    , ss_z            = z_4
    , ss_zi           = zi_4
    , ss_t_veg        = t_veg_final
    , ss_eflx_sh_tot  = eflx_sh_tot_new
    , ss_eflx_lh_tot  = eflx_lh_tot_new
    , ss_t_ref2m      = t_ref2m_new
    , ss_fsa          = fsa
    , ss_fsa_patch0   = fsa_patch0
    , ss_qflx_runoff  = max 0.0 (forc_rain * 0.1)
    }

-- ============================================================================
-- Time loop
-- ============================================================================

-- | Run simulation for ndays and return daily averages.
runSimulation :: SimState -> Int -> Double -> IO [DailyAvg]
runSimulation st0 ndays dtime = do
  let stepsPerDay = round (86400.0 / dtime) :: Int
      totalSteps = ndays * stepsPerDay
  go st0 1 zeroDailyAvg [] totalSteps stepsPerDay
  where
    go !st !step !dayAcc !results !totalSteps !stepsPerDay
      | step > totalSteps = return (reverse results)
      | otherwise = do
          let st' = stepSimulation st step dtime
              dayAcc' = addDailyAvg dayAcc st'
              dayOfSim = (step - 1) `div` stepsPerDay + 1
              isEndDay = step `mod` stepsPerDay == 0

          -- Debug: detect when T_GRND goes bad
          when (ss_t_grnd st' > 400.0 || ss_t_grnd st' < 100.0 || isNaN (ss_t_grnd st')) $
            putStrLn $ "  *** DEBUG step " ++ show step
              ++ ": T_GRND=" ++ show (ss_t_grnd st')
              ++ ", snl_before=" ++ show (ss_snl st)
              ++ ", snl_after=" ++ show (ss_snl st')
              ++ ", snow_depth=" ++ show (ss_snow_depth st')
              ++ ", h2osno_nl=" ++ show (ss_h2osno_no_layers st')
              ++ ", frac_sno=" ++ show (ss_frac_sno st')

          if isEndDay
            then do
              let avg = avgDailyAvg dayAcc' stepsPerDay
              putStrLn $ "  Day " ++ show dayOfSim
                      ++ ": T_GRND=" ++ showF2 (da_t_grnd avg) ++ " K"
                      ++ ", H2OSNO=" ++ showF4 (da_h2osno avg) ++ " kg/m2"
                      ++ ", SNOW_DEPTH=" ++ showF4 (da_snow_depth avg) ++ " m"
              go st' (step + 1) zeroDailyAvg (avg : results) totalSteps stepsPerDay
            else
              go st' (step + 1) dayAcc' results totalSteps stepsPerDay

    showF2 x = let s = show (fromIntegral (round (x * 100.0) :: Int) / 100.0 :: Double)
               in s
    showF4 x = let s = show (fromIntegral (round (x * 10000.0) :: Int) / 10000.0 :: Double)
               in s
