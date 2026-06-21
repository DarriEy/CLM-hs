{-# LANGUAGE BangPatterns #-}
-- | Pipeline-driven CLM simulation runner.
-- Replaces the monolithic Simulation.hs with one that uses
-- clmDrv + wiredPhysicsPipeline for all physics.
module CLM.Driver.PipelineRunner
  ( -- * Initialization
    initCLMStateFromDir
    -- * Timestep context
  , buildTimestepContext
    -- * Run loop
  , runPipeline
  , PipelineConfig(..)
  , defaultPipelineConfig
    -- * Daily diagnostics
  , DailyDiag(..)
  , zeroDailyDiag
    -- * CSV output
  , writeDailyCSV
    -- * CLM forward model for calibration (extracts QRUNOFF)
  , runCLMForQrunoff
    -- * Re-exports for pipeline users
  , SurfaceAlbedoConstants(..)
  ) where

import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Monad (when)

import CLM.Constants.PhysicalConstants
  ( nlevsno, nlevgrnd, nlevsoi, numrad )
import CLM.Constants.ControlFlags
  ( CLMDriverConfig(..), defaultDriverConfig )
import CLM.Driver.CLMDriver
  ( CLMState(..), CLMDriverState(..), TimestepContext(..)
  , PhysicsPipeline(..)
  , defaultCLMState, defaultDriverState, defaultTimestepContext
  , clmDrv )
import CLM.Driver.PhysicsAdapters (wiredPhysicsPipeline)
import CLM.BioGeoPhys.CanopyHydrology
  ( CanopyHydrologyParams(..), defaultCanopyHydroParams )

import CLM.Types.ColumnData (ColumnData(..))
import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData (WaterStateData(..))
import CLM.Types.WaterFluxData (WaterFluxData(..))
import CLM.Types.WaterDiagnosticBulkData (WaterDiagnosticBulkData(..))
import CLM.Types.EnergyFluxData (EnergyFluxData(..))
import CLM.Types.CanopyStateData (CanopyStateData(..))
import CLM.Types.SoilStateData (SoilStateData(..))
import CLM.Types.GridcellData (GridcellData(..))
import CLM.Types.FrictionVelocityData (FrictionVelocityData(..))

import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readInt64Vector, readFloat64Scalar
  , readManifestDims, ManifestDims(..) )
import CLM.Infrastructure.ReadParams
  ( readParametersBinary, AllParams(..), PFTConstants(..) )
import CLM.Infrastructure.ForcingReader
  ( ForcingReaderState(..), forcingReaderInitBinary, readForcingStepPure
  , ForcingTimestep(..)
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity, splitShortwaveBands )
import CLM.Infrastructure.Orbital
  ( computeOrbital, defaultOrbitalParams )
import CLM.BioGeoPhys.SurfaceAlbedo
  ( SurfaceAlbedoConstants(..), initSoilAlbedoTables )
import CLM.BioGeoPhys.RootBioPhys
  ( RootFrInput(..), RootingProfileMethod(..), computeRootFr )

-- ============================================================================
-- Configuration
-- ============================================================================

data PipelineConfig = PipelineConfig
  { pcDtime       :: !Double
  , pcNdays       :: !Int
  , pcDataDir     :: !FilePath
  , pcVerbose     :: !Bool
  , pcOutputCSV   :: !FilePath
  , pcUseCN       :: !Bool       -- ^ Enable CN biogeochemistry
  } deriving (Show)

defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pcDtime   = 1800.0
  , pcNdays   = 30
  , pcDataDir = "test/data"
  , pcVerbose = True
  , pcOutputCSV = ""
  , pcUseCN = False
  }

-- ============================================================================
-- Canopy-hydrology parameters from the CLM parameter file
-- ============================================================================

-- | Read canopy-hydrology scalar parameters from @<dir>/params/chyd_*.bin@,
-- falling back to 'defaultCanopyHydroParams' field-by-field when a file is
-- absent. This is how the param-file's maximum_leaf_wetted_fraction (etc.)
-- reaches the canopy hydrology step instead of a hard-coded default.
readCanopyHydroParamsFromDir :: FilePath -> IO CanopyHydrologyParams
readCanopyHydroParamsFromDir dir = do
  let pdir = dir </> "params"
      rdOr deflt name = do
        let f = pdir </> ("chyd_" ++ name ++ ".bin")
        ex <- doesFileExist f
        if ex then readFloat64Scalar f else return deflt
      d = defaultCanopyHydroParams
  mlwf  <- rdOr (chp_maximum_leaf_wetted_fraction d) "maximum_leaf_wetted_fraction"
  liqs  <- rdOr (chp_liq_canopy_storage_scalar d)    "liq_canopy_storage_scalar"
  snos  <- rdOr (chp_snow_canopy_storage_scalar d)   "snow_canopy_storage_scalar"
  intf  <- rdOr (chp_interception_fraction d)        "interception_fraction"
  uwind <- rdOr (chp_snowcan_unload_wind_fact d)     "snowcan_unload_wind_fact"
  utemp <- rdOr (chp_snowcan_unload_temp_fact d)     "snowcan_unload_temp_fact"
  return d
    { chp_maximum_leaf_wetted_fraction = mlwf
    , chp_liq_canopy_storage_scalar    = liqs
    , chp_snow_canopy_storage_scalar   = snos
    , chp_interception_fraction        = intf
    , chp_snowcan_unload_wind_fact     = uwind
    , chp_snowcan_unload_temp_fact     = utemp
    }

-- ============================================================================
-- CLMState initialization from binary test data
-- ============================================================================

initCLMStateFromDir :: FilePath -> IO (CLMState, ForcingReaderState, SurfaceAlbedoConstants)
initCLMStateFromDir dir = do
  dims <- readManifestDims (dir </> "manifest.json")
  let nc = mdNc dims
      nlevtot = nlevsno + nlevgrnd

  let extractCol1 :: VU.Vector Double -> Int -> VU.Vector Double
      extractCol1 vec nlev = VU.generate nlev (\j -> vec VU.! (j * nc + 0))
      sanitize v = VU.map (\x -> if isNaN x then 0.0 else x) v
      safeAt vec i def =
        if i >= 0 && i < VU.length vec then vec VU.! i else def
      safeAtI vec i def =
        if i >= 0 && i < VU.length vec then vec VU.! i else def
      readOptionalVector path fallback = do
        exists <- doesFileExist path
        if exists then readFloat64Vector path else return fallback
      readOptionalIntVector path fallback = do
        exists <- doesFileExist path
        if exists then readInt64Vector path else return fallback

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
  t_veg_raw <- readOptionalVector (dir </> "coldstart" </> "t_veg.bin") VU.empty
  t_ref2m_raw <- readOptionalVector (dir </> "coldstart" </> "t_ref2m.bin") VU.empty
  -- Optional snow-state injection (e.g. from a Fortran restart). When absent,
  -- these default to a snow-free cold start (snl=0, frac_sno=0), the prior
  -- behaviour. When present, snl.bin holds the (negative) snow-layer count and
  -- the snow geometry/temperature/water already live in the top nlevsno slots
  -- of t_soisno/h2osoi_liq/h2osoi_ice/col_dz/col_z/col_zi.
  snl_raw <- readOptionalVector (dir </> "coldstart" </> "snl.bin") VU.empty
  snow_depth_raw <- readOptionalVector (dir </> "coldstart" </> "snow_depth.bin") VU.empty
  frac_sno_raw <- readOptionalVector (dir </> "coldstart" </> "frac_sno.bin") VU.empty
  frac_sno_eff_raw <- readOptionalVector (dir </> "coldstart" </> "frac_sno_eff.bin") VU.empty
  zbedrock_raw <- readOptionalVector (dir </> "surfdata" </> "zbedrock.bin") VU.empty

  elai_raw <- readFloat64Vector (dir </> "coldstart" </> "elai.bin")
  esai_raw <- readFloat64Vector (dir </> "coldstart" </> "esai.bin")
  tlai_raw <- readOptionalVector (dir </> "coldstart" </> "tlai.bin") elai_raw
  htop_raw <- readFloat64Vector (dir </> "coldstart" </> "htop.bin")
  pch_wtgcell_raw <- readOptionalVector
    (dir </> "coldstart" </> "pch_wtgcell.bin")
    (VU.generate (mdNp dims) (\i -> if i == 0 then 1.0 else 0.0))
  pch_itype_raw <- readOptionalIntVector
    (dir </> "coldstart" </> "pch_itype.bin")
    (VU.replicate (mdNp dims) 0)
  params <- readParametersBinary (dir </> "params")

  forcing <- forcingReaderInitBinary (dir </> "forcing")

  let t_soisno = extractCol1 t_soisno_raw nlevtot
      h2osoi_liq_raw_col = extractCol1 h2osoi_liq_raw nlevtot
      h2osoi_ice_raw_col = extractCol1 h2osoi_ice_raw nlevtot
      h2osoi_liq = h2osoi_liq_raw_col
      h2osoi_ice = h2osoi_ice_raw_col
      dz = extractCol1 dz_raw nlevtot
      z = extractCol1 z_raw nlevtot
      zi = extractCol1 zi_raw (nlevtot + 1)
      watsat_v = sanitize $ extractCol1 watsat_raw nlevgrnd
      bsw_v = sanitize $ extractCol1 bsw_raw nlevgrnd
      sucsat_v = sanitize $ extractCol1 sucsat_raw nlevgrnd
      tkmg_v = sanitize $ extractCol1 tkmg_raw nlevgrnd
      tkdry_v = sanitize $ extractCol1 tkdry_raw nlevgrnd
      csol_v = sanitize $ extractCol1 csol_raw nlevgrnd
      tksatu_v = sanitize $ extractCol1 tksatu_raw nlevgrnd

      t_grnd = t_grnd_raw VU.! 0
      np = mdNp dims

      patchWeights = VU.generate np (\p -> safeAt pch_wtgcell_raw p 0.0)
      patchWeightSum = max 1.0e-12 (VU.sum patchWeights)
      patchWeighted vec fallback =
        sum
          [ (safeAt patchWeights p 0.0 / patchWeightSum) * safeAt vec p fallback
          | p <- [0 .. np - 1]
          ]
      tVegPatch = VU.generate np (\p -> safeAt t_veg_raw p t_grnd)
      tRefPatch = VU.generate np (\p -> safeAt t_ref2m_raw p t_grnd)
      frac_veg = VU.generate np $ \p ->
        if safeAt elai_raw p 0.0 + safeAt esai_raw p 0.0 > 0.05 then 1 else 0
      pft = ap_pftcon params
      numpft = VU.length (pft_xl pft)
      pftAt p = safeAtI pch_itype_raw p 0
      pftBand table p ib =
        let pt = pftAt p
            idx = pt + ib * numpft
        in if pt >= 0 && idx >= 0 && idx < VU.length table
           then table VU.! idx
           else 0.0
      pftScalar table p def =
        let pt = pftAt p
        in if pt >= 0 && pt < VU.length table then table VU.! pt else def
      patchBand table =
        VU.generate (np * numrad) $ \idx ->
          let (p, ib) = idx `divMod` numrad
          in pftBand table p ib
      soilZ = VU.generate nlevgrnd $ \j -> safeAt z (nlevsno + j) 0.0
      soilDz = VU.generate nlevgrnd $ \j -> safeAt dz (nlevsno + j) 0.0
      soilZi = VU.generate nlevgrnd $ \j ->
        safeAt zi (nlevsno + j + 1) (safeAt zi (nlevsno + j) 0.0)
      -- Bedrock layer index (Fortran initVerticalMod: nbedrock = j where
      -- zisoi(j-1) < zbedrock <= zisoi(j); zisoi(0)=0). soilZi is the soil
      -- interface depths zisoi(1..nlevsoi). Default nlevsoi when no bedrock.
      zbedrock_in = safeAt zbedrock_raw 0 (safeAt soilZi (nlevsoi - 1) 0.0)
      zisoiF k  -- Fortran 1-based zisoi(k), with zisoi(0)=0
        | k <= 0    = 0.0
        | otherwise = safeAt soilZi (k - 1) 0.0
      nbedrockComputed
        | VU.null zbedrock_raw = nlevsoi
        | otherwise =
            let go j acc
                  | j > nlevsoi = acc
                  | zisoiF (j - 1) < zbedrock_in && zisoiF j >= zbedrock_in = go (j + 1) j
                  | otherwise = go (j + 1) acc
            in go 1 nlevsoi
      rootFrForPatch p = computeRootFr RootFrInput
        { rfi_method = Zeng2001Root
        , rfi_nlevsoi = nlevsoi
        , rfi_nlevgrnd = nlevgrnd
        , rfi_nbedrock = nbedrockComputed
        , rfi_is_fates = False
        , rfi_roota_par = pftScalar (pft_roota_par pft) p 7.0
        , rfi_rootb_par = pftScalar (pft_rootb_par pft) p 2.0
        , rfi_rootprof_beta = 0.95
        , rfi_col_zi = soilZi
        , rfi_col_z = soilZ
        , rfi_col_dz = soilDz
        }
      rootfrPatch = VU.concat [ rootFrForPatch p | p <- [0 .. np - 1] ]
      smpsoPatch = VU.generate np (\p -> pftScalar (pft_smpso pft) p (-66000.0))
      smpscPatch = VU.generate np (\p -> pftScalar (pft_smpsc pft) p (-255000.0))

      -- Snow-state injection: snl (negative count of active snow layers) and the
      -- associated snow-fraction diagnostics. Default to a snow-free cold start.
      snlInit = if VU.null snl_raw then 0 else round (snl_raw VU.! 0)
      snowDepthInit = safeAt snow_depth_raw 0 0.0
      fracSnoInit = safeAt frac_sno_raw 0 0.0
      fracSnoEffInit =
        if VU.null frac_sno_eff_raw then fracSnoInit else frac_sno_eff_raw VU.! 0

  let st = defaultCLMState
        { clmColumn = ColumnData
            { colZ = z, colDz = dz, colZi = zi
            , watsat = watsat_v, bsw = bsw_v
            , hksat = VU.replicate nlevgrnd 0.01
            , sucsat = sucsat_v
            , zii = 1000.0, lakedepth = 0.0
            }
        , clmTemp = TemperatureData
            { t_soisno_col = t_soisno
            , t_soisno_bef_col = t_soisno
            , t_grnd_col = t_grnd
            , t_h2osfc_col = t_h2osfc_raw VU.! 0
            , t_h2osfc_bef_col = t_h2osfc_raw VU.! 0
            , t_ref2m_patch = patchWeighted tRefPatch t_grnd
            , t_veg_patch = patchWeighted tVegPatch t_grnd
            , t_ref2m_patch_vec = tRefPatch
            , t_veg_patch_vec = tVegPatch
            }
        , clmWaterState = WaterStateData
            { h2osoi_liq_col = h2osoi_liq
            , h2osoi_ice_col = h2osoi_ice
            , h2osoi_vol_col = VU.replicate nlevgrnd 0.3
            , h2osno_col = h2osno_raw VU.! 0
            , h2osfc_col = h2osfc_raw VU.! 0
            , h2ocan_patch = 0.0
            , liqcan_patch = 0.0
            , snocan_patch = 0.0
            , h2ocan_patch_vec = VU.replicate np 0.0
            , liqcan_patch_vec = VU.replicate np 0.0
            , snocan_patch_vec = VU.replicate np 0.0
            }
        , clmSoilState = (clmSoilState defaultCLMState)
            { sstate_tkmg_col = tkmg_v
            , sstate_tkdry_col = tkdry_v
            , sstate_csol_col = csol_v
            , sstate_tksatu_col = tksatu_v
            , sstate_watsat_col = watsat_v
            , sstate_bsw_col = bsw_v
            , sstate_sucsat_col = sucsat_v
            , sstate_soilbeta_col = VU.singleton 1.0
            , sstate_rootfr_patch = rootfrPatch
            , sstate_crootfr_patch = rootfrPatch
            , sstate_rootfr_col = if np > 0 then VU.slice 0 nlevgrnd rootfrPatch else VU.empty
            , sstate_smpso_patch = smpsoPatch
            , sstate_smpsc_patch = smpscPatch
            }
        , clmCanopyState = (clmCanopyState defaultCLMState)
            { cstate_elai_patch = elai_raw
            , cstate_esai_patch = esai_raw
            , cstate_tlai_patch = tlai_raw
            , cstate_tsai_patch = esai_raw
            , cstate_tlai_hist_patch = tlai_raw
            , cstate_tsai_hist_patch = esai_raw
            , cstate_htop_patch = htop_raw
            , cstate_htop_hist_patch = htop_raw
            , cstate_frac_veg_nosno_patch = frac_veg
            , cstate_frac_veg_nosno_alb_patch = frac_veg
            , cstate_patch_wtgcell = patchWeights
            , cstate_xl_patch = VU.generate np (\p -> pftScalar (pft_xl pft) p 0.01)
            , cstate_rhol_patch = patchBand (pft_rhol pft)
            , cstate_rhos_patch = patchBand (pft_rhos pft)
            , cstate_taul_patch = patchBand (pft_taul pft)
            , cstate_taus_patch = patchBand (pft_taus pft)
            , cstate_fsun_patch = VU.replicate np 0.5
            , cstate_laisun_patch = VU.map (* 0.5) elai_raw
            , cstate_laisha_patch = VU.map (* 0.5) elai_raw
            , cstate_dleaf_patch = VU.replicate np 0.04
            }
        , clmGridcell = (clmGridcell defaultCLMState)
            { grc_lat = VU.singleton (mdLat dims)
            , grc_lon = VU.singleton (mdLon dims)
            , grc_nbedrock = VU.singleton nbedrockComputed
            , grc_dayl = VU.singleton 43200.0
            , grc_max_dayl = VU.singleton 86400.0
            }
        , clmWaterDiagBulk = (clmWaterDiagBulk defaultCLMState)
            { wdiag_frac_sno_col = VU.singleton fracSnoInit
            , wdiag_frac_sno_eff_col = VU.singleton fracSnoEffInit
            , wdiag_frac_h2osfc_col = VU.singleton 0.0
            , wdiag_snow_depth_col = VU.singleton snowDepthInit
            , wdiag_snow_persist_col = VU.singleton 0.0
            , wdiag_qg_col = VU.singleton 0.005
            , wdiag_qg_snow_col = VU.singleton 0.005
            , wdiag_qg_soil_col = VU.singleton 0.005
            , wdiag_qg_h2osfc_col = VU.singleton 0.005
            , wdiag_dqgdT_col = VU.singleton 0.0
            }
        , clmSnl = snlInit
        }

  soil_color_raw <- readInt64Vector (dir </> "surfdata" </> "soil_color.bin")
  let ng = mdNg dims
      soil_color_int = VU.generate ng $ \i ->
        max 1 (min 20 (soil_color_raw VU.! i))
      col_gc = VU.generate nc (\_ -> 0)
      albConst = initSoilAlbedoTables 20 soil_color_int col_gc

  return (st, forcing, albConst)

-- ============================================================================
-- Timestep context builder
-- ============================================================================

buildTimestepContext
  :: ForcingReaderState
  -> Int       -- ^ step number (1-based)
  -> Double    -- ^ dtime [s]
  -> TimestepContext
buildTimestepContext fr nstep dtime =
  let -- Binary forcing exports are hourly; CLM may advance at a shorter timestep.
      stepsPerForcing = max 1 (round (3600.0 / dtime) :: Int)
      forcIdx = (nstep - 1) `div` stepsPerForcing
      (fts, _fr') = readForcingStepPure fr forcIdx

      forc_t    = ft_tbot fts
      forc_pbot = ft_psrf fts
      forc_wind = ft_wind fts
      forc_lwrad = ft_flds fts
      forc_fsds = ft_fsds fts
      forc_precip = ft_precip fts
      forc_qbot = ft_qbot fts

      forc_vp  = computeVaporPressureFromQ forc_qbot forc_pbot
      forc_th  = computePotentialTemperature forc_t forc_pbot
      forc_rho = computeAirDensity forc_pbot forc_t forc_vp
      (forc_rain, forc_snow) = partitionPrecip forc_t forc_precip

      (forc_solad_vis, forc_solad_nir, forc_solai_vis, forc_solai_nir) =
        splitShortwaveBands forc_fsds

      calday = fromIntegral (nstep - 1) * dtime / 86400.0 + 1.0
      (declin, _eccf) = computeOrbital defaultOrbitalParams calday

      stepsPerDay = round (86400.0 / dtime) :: Int

  in defaultTimestepContext
    { tcDoAlb         = True
    , tcDtime         = dtime
    , tcDeclin        = declin
    , tcDeclinP1      = declin
    -- Supply a real next-radiation calendar day so the surface-radiation step
    -- computes the proper solar zenith (shr_orb_cosz) instead of the cos(declin)
    -- fallback. Without this the offline pipeline used cos(declin) ~ 0.92 at Bow
    -- in January (should be ~0.26), absorbing far too much solar.
    , tcNextswCday    = calday + dtime / 86400.0
    , tcObliqr        = 0.4091
    , tcIsFirstStep   = nstep == 1
    , tcIsBegCurrDay  = (nstep - 1) `mod` stepsPerDay == 0
    , tcIsEndCurrDay  = nstep `mod` stepsPerDay == 0
    , tcForcT         = VU.singleton forc_t
    , tcForcTh        = VU.singleton forc_th
    , tcForcQ         = VU.singleton forc_qbot
    , tcForcPbot      = VU.singleton forc_pbot
    , tcForcRho       = VU.singleton forc_rho
    , tcForcRain      = VU.singleton forc_rain
    , tcForcSnow      = VU.singleton forc_snow
    , tcForcLwrad     = VU.singleton forc_lwrad
    , tcForcSolad     = VU.fromList [forc_solad_vis, forc_solad_nir]
    , tcForcSolai     = VU.fromList [forc_solai_vis, forc_solai_nir]
    , tcForcWind      = VU.singleton forc_wind
    , tcForcHgt       = 30.0
    }

-- ============================================================================
-- Daily diagnostics
-- ============================================================================

data DailyDiag = DailyDiag
  { dd_t_grnd     :: !Double
  , dd_fsa        :: !Double
  , dd_h2osno     :: !Double
  , dd_snow_depth :: !Double
  , dd_frac_sno   :: !Double
  , dd_eflx_sh    :: !Double
  , dd_eflx_lh    :: !Double
  , dd_count      :: !Int
  -- CN diagnostics
  , dd_gpp        :: !Double  -- ^ GPP (gC/m2/s)
  , dd_npp        :: !Double  -- ^ NPP (gC/m2/s)
  , dd_nee        :: !Double  -- ^ NEE (gC/m2/s)
  , dd_hr         :: !Double  -- ^ Heterotrophic resp (gC/m2/s)
  , dd_leafc      :: !Double  -- ^ Leaf C (gC/m2)
  , dd_soilorgc   :: !Double  -- ^ Soil organic C (gC/m2)
  } deriving (Show)

zeroDailyDiag :: DailyDiag
zeroDailyDiag = DailyDiag
  { dd_t_grnd = 0.0
  , dd_fsa = 0.0
  , dd_h2osno = 0.0
  , dd_snow_depth = 0.0
  , dd_frac_sno = 0.0
  , dd_eflx_sh = 0.0
  , dd_eflx_lh = 0.0
  , dd_count = 0
  , dd_gpp = 0.0
  , dd_npp = 0.0
  , dd_nee = 0.0
  , dd_hr = 0.0
  , dd_leafc = 0.0
  , dd_soilorgc = 0.0
  }

accumDiag :: DailyDiag -> CLMState -> DailyDiag
accumDiag dd st = DailyDiag
  { dd_t_grnd     = dd_t_grnd dd + t_grnd_col (clmTemp st)
  , dd_fsa        = dd_fsa dd + fsa_patch (clmEnergyFlux st)
  , dd_h2osno     = dd_h2osno dd + h2osno_col (clmWaterState st) + explicitSnowMass
  , dd_snow_depth = dd_snow_depth dd
                  + (if VU.null v then 0.0 else v VU.! 0)
  , dd_frac_sno   = dd_frac_sno dd
                  + (if VU.null fs then 0.0 else fs VU.! 0)
  , dd_eflx_sh    = dd_eflx_sh dd + eflx_sh_tot_patch (clmEnergyFlux st)
  , dd_eflx_lh    = dd_eflx_lh dd + eflx_lh_tot_patch (clmEnergyFlux st)
  , dd_count      = dd_count dd + 1
  , dd_gpp        = dd_gpp dd + clmGPP st
  , dd_npp        = dd_npp dd + clmNPP st
  , dd_nee        = dd_nee dd + clmNEE st
  , dd_hr         = dd_hr dd + clmHR st
  , dd_leafc      = dd_leafc dd + clmLeafC st
  , dd_soilorgc   = dd_soilorgc dd + clmSoilOrgC st
  }
  where
    v = wdiag_snow_depth_col (clmWaterDiagBulk st)
    fs = wdiag_frac_sno_col (clmWaterDiagBulk st)
    -- Grid-mean SWE must include any EXPLICIT snow-layer mass (ice+liq) once a
    -- layer forms; h2osno_col only holds the unresolved no-layer SWE, which is
    -- zeroed on layer creation. Snow layers are bottom-packed at
    -- indices [nlevsno+snl .. nlevsno-1].
    snl = clmSnl st
    ice = h2osoi_ice_col (clmWaterState st)
    liq = h2osoi_liq_col (clmWaterState st)
    explicitSnowMass
      | snl >= 0  = 0.0
      | otherwise = sum [ idx ice j + idx liq j | j <- [nlevsno + snl .. nlevsno - 1] ]
    idx vec j = if j >= 0 && j < VU.length vec then vec VU.! j else 0.0

avgDiag :: DailyDiag -> DailyDiag
avgDiag dd =
  let n = fromIntegral (dd_count dd)
  in if n <= 0 then dd
     else DailyDiag
       { dd_t_grnd     = dd_t_grnd dd / n
       , dd_fsa        = dd_fsa dd / n
       , dd_h2osno     = dd_h2osno dd / n
       , dd_snow_depth = dd_snow_depth dd / n
       , dd_frac_sno   = dd_frac_sno dd / n
       , dd_eflx_sh    = dd_eflx_sh dd / n
       , dd_eflx_lh    = dd_eflx_lh dd / n
       , dd_count      = dd_count dd
       , dd_gpp        = dd_gpp dd / n
       , dd_npp        = dd_npp dd / n
       , dd_nee        = dd_nee dd / n
       , dd_hr         = dd_hr dd / n
       , dd_leafc      = dd_leafc dd / n
       , dd_soilorgc   = dd_soilorgc dd / n
       }

-- ============================================================================
-- Main run loop
-- ============================================================================

runPipeline :: PipelineConfig -> IO [DailyDiag]
runPipeline cfg = do
  let dir = pcDataDir cfg
      dtime = pcDtime cfg
      ndays = pcNdays cfg
      stepsPerDay = round (86400.0 / dtime) :: Int
      totalSteps = ndays * stepsPerDay

  (st0_, forcing, albConst) <- initCLMStateFromDir dir
  chParams <- readCanopyHydroParamsFromDir dir

  let st0 = if pcUseCN cfg
            then st0_ { clmCNActive = True
                       , clmLeafC = 200.0
                       , clmFrootC = 150.0
                       , clmLiveStemC = 500.0
                       , clmDeadStemC = 5000.0
                       , clmCPool = 0.0
                       , clmSoilOrgC = 8000.0
                       , clmLitterC = 300.0
                       , clmSMINN = 5.0
                       , clmLeafN = 8.0
                       , clmFPG = 1.0
                       }
            else st0_

  when (pcVerbose cfg) $ do
    putStrLn $ "Pipeline runner: " ++ show ndays ++ " days, "
            ++ show stepsPerDay ++ " steps/day, dtime=" ++ show dtime ++ "s"
    when (pcUseCN cfg) $
      putStrLn $ "  CN biogeochemistry ENABLED (leafC=" ++ show (clmLeafC st0)
              ++ ", soilOrgC=" ++ show (clmSoilOrgC st0) ++ " gC/m2)"

  let drvCfg = defaultDriverConfig
      pipeline = wiredPhysicsPipeline albConst chParams

  go st0 defaultDriverState forcing 1 zeroDailyDiag [] totalSteps stepsPerDay drvCfg dtime pipeline
  where
    go !st !drvSt !fr !step !dayAcc !results !total !spd !drvCfg !dtime !pl
      | step > total = return (reverse results)
      | otherwise = do
          let ctx = buildTimestepContext fr step dtime
              (!drvSt', !st') = clmDrv drvCfg pl ctx drvSt st
              dayAcc' = accumDiag dayAcc st'
              isEndDay = step `mod` spd == 0


          when (t_grnd_col (clmTemp st') > 400.0 ||
                t_grnd_col (clmTemp st') < 100.0) $
            putStrLn $ "  *** WARNING step " ++ show step
                    ++ ": T_GRND=" ++ show (t_grnd_col (clmTemp st'))

          if isEndDay
            then do
              let avg = avgDiag dayAcc'
                  dayNum = step `div` spd
              when (pcVerbose cfg) $ do
                putStrLn $ "  Day " ++ show dayNum
                        ++ ": T_GRND=" ++ show (dd_t_grnd avg) ++ " K"
                        ++ ", H2OSNO=" ++ show (dd_h2osno avg) ++ " kg/m2"
                        ++ ", SNOW_DEPTH=" ++ show (dd_snow_depth avg) ++ " m"
                when (pcUseCN cfg) $
                  putStrLn $ "    CN: GPP=" ++ showF (dd_gpp avg * 86400.0) ++ " gC/m2/d"
                          ++ ", NPP=" ++ showF (dd_npp avg * 86400.0) ++ " gC/m2/d"
                          ++ ", NEE=" ++ showF (dd_nee avg * 86400.0) ++ " gC/m2/d"
                          ++ ", HR=" ++ showF (dd_hr avg * 86400.0) ++ " gC/m2/d"
                          ++ ", LeafC=" ++ showF (dd_leafc avg) ++ " gC/m2"
                          ++ ", SoilC=" ++ showF (dd_soilorgc avg) ++ " gC/m2"
              go st' drvSt' fr (step + 1) zeroDailyDiag (avg : results) total spd drvCfg dtime pl
            else
              go st' drvSt' fr (step + 1) dayAcc' results total spd drvCfg dtime pl

-- ============================================================================
-- CSV output (matching Julia daily_avg format)
-- ============================================================================

writeDailyCSV :: FilePath -> [DailyDiag] -> IO ()
writeDailyCSV path dailies = do
  let header = "day,T_GRND,FSA,EFLX_LH_TOT,EFLX_SH_TOT,H2OSNO,SNOW_DEPTH,FRAC_SNO"
      rows = zipWith mkRow [1::Int ..] dailies
      mkRow d dd = show d
                ++ "," ++ showE (dd_t_grnd dd)
                ++ "," ++ showE (dd_fsa dd)
                ++ "," ++ showE (dd_eflx_lh dd)
                ++ "," ++ showE (dd_eflx_sh dd)
                ++ "," ++ showE (dd_h2osno dd)
                ++ "," ++ showE (dd_snow_depth dd)
                ++ "," ++ showE (dd_frac_sno dd)
      showE x = show x
  writeFile path (unlines (header : rows))
  putStrLn $ "Wrote " ++ show (length dailies) ++ " daily averages to " ++ path

-- | Format a Double to 3 decimal places for display.
showF :: Double -> String
showF x = show (fromIntegral (round (x * 1000.0) :: Int) / 1000.0 :: Double)

-- ============================================================================
-- CLM forward model for calibration: run full physics, extract QRUNOFF
-- ============================================================================

-- | Run the full CLM physics pipeline and return hourly QRUNOFF (mm/s).
-- This is the forward model used for streamflow calibration.
-- Uses the actual clmDrv physics pipeline (soil temp, snow, hydrology, etc.)
runCLMForQrunoff :: PipelineConfig -> IO [Double]
runCLMForQrunoff cfg = do
  let dir = pcDataDir cfg
      dtime = pcDtime cfg
      ndays = pcNdays cfg
      stepsPerDay = round (86400.0 / dtime) :: Int
      totalSteps = ndays * stepsPerDay

  (st0_, forcing, albConst) <- initCLMStateFromDir dir
  chParams <- readCanopyHydroParamsFromDir dir

  let st0 = if pcUseCN cfg
            then st0_ { clmCNActive = True
                       , clmLeafC = 200.0, clmFrootC = 150.0
                       , clmLiveStemC = 500.0, clmDeadStemC = 5000.0
                       , clmSoilOrgC = 8000.0, clmLitterC = 300.0
                       , clmSMINN = 5.0, clmLeafN = 8.0, clmFPG = 1.0
                       }
            else st0_

  let drvCfg = defaultDriverConfig
      pipeline = wiredPhysicsPipeline albConst chParams

  goQ st0 defaultDriverState forcing 1 [] totalSteps drvCfg dtime pipeline
  where
    goQ !st !drvSt !fr !step !qAcc !total !drvCfg !dtime !pl
      | step > total = return (reverse qAcc)
      | otherwise = do
          let ctx = buildTimestepContext fr step dtime
              (!drvSt', !st') = clmDrv drvCfg pl ctx drvSt st
              wf = clmWaterFlux st'
              qrunoff = qflx_surf_col wf + qflx_drain_col wf
          goQ st' drvSt' fr (step + 1) (qrunoff : qAcc) total drvCfg dtime pl
