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
    -- * Restart I/O (full prognostic state save/restore)
  , writeRestartState
  , readRestartState
  , readFortranRestart
    -- * Daily diagnostics
  , DailyDiag(..)
  , zeroDailyDiag
    -- * CSV output
  , writeDailyCSV
    -- * NetCDF history output
  , writeDailyNetCDF
    -- * Multi-landunit gridcell (Phase 4 #12, Option A: column-loop)
  , SurfdataLandunits(..)
  , readSurfdataLandunits
  , runMixedGridcell
    -- * CLM forward model for calibration (extracts QRUNOFF)
  , runCLMForQrunoff
    -- * Re-exports for pipeline users
  , SurfaceAlbedoConstants(..)
  ) where

import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.Directory (doesFileExist, createDirectoryIfMissing)
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
import CLM.BioGeoChem.CNDVStep (seedDGVS, tkfrz)
import CLM.Driver.PhysicsAdapters
  ( wiredPhysicsPipeline, initCNDecompPools
  , lakeFluxesStep, lakeTemperatureStep )
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
import CLM.Types.SoilHydrologyData (SoilHydrologyData(..))
import CLM.Types.WaterStateBulkData (WaterStateBulkData(..))
import CLM.Types.LakeStateData (LakeStateData(..))

import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readInt64Vector, readFloat64Scalar
  , writeFloat64Vector
  , readManifestDims, ManifestDims(..) )
import CLM.Infrastructure.ReadParams
  ( readParametersBinary, AllParams(..), PFTConstants(..), readDGVEcophysCon
  , readMeganIsopreneEF, readPprod10, readPprod100 )
import CLM.Types.DGVSData (DGVEcophysCon(..), defaultDGVEcophysCon)
import CLM.Infrastructure.NetCDF
  ( NcFile, ncOpen, ncClose, ncReadDouble1D, ncReadDouble2D, ncReadDoubleScalar
  , ncDimLen, ncWriteTimeseries )
import CLM.Infrastructure.ForcingReader
  ( ForcingReaderState(..), forcingReaderInitBinary, readForcingStepPure
  , ForcingTimestep(..)
  , partitionPrecip, computeVaporPressureFromQ
  , computePotentialTemperature, computeAirDensity, splitShortwaveBands )
import CLM.Infrastructure.Orbital
  ( computeOrbital, defaultOrbitalParams )
import qualified CLM.Infrastructure.InitSubgrid as IS
import qualified CLM.Infrastructure.SubgridAverage as SA
import CLM.BioGeoPhys.SurfaceAlbedo
  ( SurfaceAlbedoConstants(..), initSoilAlbedoTables )
import CLM.BioGeoPhys.SnowSNICAR
  ( SnicarOptics(..), emptySnicarOptics )
import CLM.BioGeoPhys.RootBioPhys
  ( RootFrInput(..), RootingProfileMethod(..), computeRootFr )

-- | Load the 5-band SNICAR Mie optics + flux weights from @<dir>/snicar@.
-- Returns 'emptySnicarOptics' if absent (SNICAR then falls back to the
-- age-based snow albedo, preserving prior behavior).
readSnicarOptics :: FilePath -> IO SnicarOptics
readSnicarOptics dir = do
  let sd = dir </> "snicar"
      rd nm = do let p = sd </> (nm ++ ".bin")
                 e <- doesFileExist p
                 if e then readFloat64Vector p else return VU.empty
  ssD <- rd "ss_alb_dir"; exD <- rd "ext_cff_dir"; asD <- rd "asm_dir"; fwD <- rd "flx_wgt_dir"
  ssI <- rd "ss_alb_dif"; exI <- rd "ext_cff_dif"; asI <- rd "asm_dif"; fwI <- rd "flx_wgt_dif"
  aTau <- rd "age_tau"; aKap <- rd "age_kappa"; aDr <- rd "age_drdt0"
  if VU.null ssD then return emptySnicarOptics
                 else return (SnicarOptics ssD exD asD fwD ssI exI asI fwI aTau aKap aDr)

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
  , pcUseCndv     :: !Bool       -- ^ Enable dynamic vegetation (seed clmDGVS)
  , pcFortranRestart :: !(Maybe FilePath)
    -- ^ Warm-start: overlay a Fortran @clm2.r.*.nc@ restart (column 0) onto the
    -- cold-start base before running, so the snow/soil IC matches a Fortran run.
  , pcRestartRoundtripDay :: !(Maybe Int)
    -- ^ Test hook: at the end of this day, write the full prognostic state to
    -- a restart directory and immediately read it back onto a /pristine/
    -- cold-start base, then continue from the restored state. If restart I/O is
    -- both lossless and complete, the daily output is bit-identical to a run
    -- with 'Nothing'. The restart dir is @\<pcDataDir\>/restart-roundtrip@.
  } deriving (Show)

defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pcDtime   = 1800.0
  , pcNdays   = 30
  , pcDataDir = "test/data"
  , pcVerbose = True
  , pcOutputCSV = ""
  , pcUseCN = False
  , pcUseCndv = False
  , pcFortranRestart = Nothing
  , pcRestartRoundtripDay = Nothing
  }

-- | Seed the dynamic-vegetation (CNDV) state for a single natural-veg patch
-- when dynamic vegetation is enabled; otherwise leave clmDGVS empty (which makes
-- the wired cndvStep a no-op). Also installs the per-PFT ecophysiological
-- constants ('DGVEcophysCon', from clm5_params.nc) so the step uses the real
-- bioclimatic limits; if those files are absent the vectors are empty and
-- cndvStep falls back to the built-in LPJ table. Cold start begins with a
-- modest established stand; the first year's accumulation populates
-- t_mo_min / tmomin20.
seedCNDV :: PipelineConfig -> DGVEcophysCon -> CLMState -> CLMState
seedCNDV cfg econ st
  | pcUseCndv cfg = st { clmDGVS = seedDGVS 1 (tkfrz + 5.0) 0.1 0.5
                       , clmCNDVYear = 1
                       , clmDGVEcophys = econ }
  | otherwise     = st

-- | Load the per-PFT CN parameter tables (MEGAN isoprene emission factors and
-- the wood-product partitions) onto the state when CN is enabled. Empty vectors
-- (files absent) leave the VOC / products steps on their representative-constant
-- fallbacks.
loadCNParams :: PipelineConfig -> FilePath -> CLMState -> IO CLMState
loadCNParams cfg dir st
  | not (pcUseCN cfg) = return st
  | otherwise = do
      let pdir = dir </> "params"
      megEF <- readMeganIsopreneEF pdir
      pp10  <- readPprod10 pdir
      pp100 <- readPprod100 pdir
      return st { clmMeganEF = megEF, clmPprod10 = pp10, clmPprod100 = pp100 }

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
  -- Natural-PFT weights (index = PFT type): the single runtime patch is
  -- represented by the dominant natural PFT, so per-PFT CN parameters (MEGAN EF,
  -- pprod, CNDV bioclimatic limits) resolve to the column's actual vegetation.
  wtNatPatch_raw <- readOptionalVector (dir </> "surfdata" </> "wt_nat_patch.bin") VU.empty
  -- Topographic std dev of elevation: sets the snow-cover-fraction SCA shape
  -- parameter n_melt (high relief -> lower frac_sno at a given SWE).
  stdElev_raw <- readOptionalVector (dir </> "surfdata" </> "std_elev.bin") VU.empty

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
        , clmPatchIvt = if VU.null wtNatPatch_raw
                        then VU.empty
                        else VU.singleton (VU.maxIndex wtNatPatch_raw)
        , clmTopoStd = if VU.null stdElev_raw
                       then clmTopoStd defaultCLMState
                       else stdElev_raw VU.! 0
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

-- ============================================================================
-- Restart I/O — full prognostic-state save/restore (Phase 4, item #15)
-- ============================================================================
--
-- The restart carries the /prognostic/ state — everything the timestep loop
-- evolves and reads back on the next step. Static fields (geometry params,
-- soil thermal/hydraulic properties, subgrid topology, parameters) are NOT
-- serialized: they are re-derived from cold-start / surfdata on read and
-- overlaid by 'readRestartState'. This mirrors the matched-state harness's
-- inject-and-carry pattern and keeps the file small.
--
-- Layout: one raw little-endian Float64 @.bin@ file per variable under @dir@,
-- the same on-disk format the cold-start loader already uses. Scalars are
-- written as length-1 vectors. @snl@ (an Int) is stored as a Float64.
--
-- Completeness is validated empirically by the in-line round-trip test
-- ('pcRestartRoundtripDay'): write at day K, read back onto a pristine base,
-- and require bit-identical daily output thereafter. Any prognostic field that
-- is missing here reverts to its cold-start value on read and makes that test
-- diverge — so the test, not this list, is the oracle for completeness.
--
-- NOTE: CN /vectorized/ pools (per-layer soil-BGC C/N, per-patch veg C/N) are
-- not yet serialized; the scalar CN pools are. The round-trip test therefore
-- runs in the default (CN-off) pipeline, where the vectorized pools are static.
-- Serializing them is the next increment, alongside reading a Fortran restart.

-- | Snow-/state-carrying WaterDiagnosticBulk fields that persist across steps.
-- Listed once so 'writeRestartState' and 'readRestartState' cannot drift apart.
restartWdiagFields :: [(String, WaterDiagnosticBulkData -> VU.Vector Double)]
restartWdiagFields =
  [ ("wdiag_qg_snow_col",            wdiag_qg_snow_col)
  , ("wdiag_qg_soil_col",            wdiag_qg_soil_col)
  , ("wdiag_qg_h2osfc_col",          wdiag_qg_h2osfc_col)
  , ("wdiag_qg_col",                 wdiag_qg_col)
  , ("wdiag_dqgdT_col",              wdiag_dqgdT_col)
  , ("wdiag_snow_depth_col",         wdiag_snow_depth_col)
  , ("wdiag_snowdp_col",             wdiag_snowdp_col)
  , ("wdiag_snow_5day_col",          wdiag_snow_5day_col)
  , ("wdiag_snomelt_accum_col",      wdiag_snomelt_accum_col)
  , ("wdiag_frac_sno_col",           wdiag_frac_sno_col)
  , ("wdiag_frac_sno_eff_col",       wdiag_frac_sno_eff_col)
  , ("wdiag_frac_h2osfc_col",        wdiag_frac_h2osfc_col)
  , ("wdiag_frac_h2osfc_nosnow_col", wdiag_frac_h2osfc_nosnow_col)
  , ("wdiag_snw_rds_col",            wdiag_snw_rds_col)
  , ("wdiag_snw_rds_top_col",        wdiag_snw_rds_top_col)
  , ("wdiag_snow_persist_col",       wdiag_snow_persist_col)
  , ("wdiag_frac_iceold_col",        wdiag_frac_iceold_col)
  , ("wdiag_swe_old_col",            wdiag_swe_old_col)
  , ("wdiag_snowice_col",            wdiag_snowice_col)
  , ("wdiag_snowliq_col",            wdiag_snowliq_col)
  , ("wdiag_h2osno_top_col",         wdiag_h2osno_top_col)
  , ("wdiag_sno_liq_top_col",        wdiag_sno_liq_top_col)
  , ("wdiag_bw_col",                 wdiag_bw_col)
  , ("wdiag_h2osno_total_col",       wdiag_h2osno_total_col)
  , ("wdiag_fwet_patch",             wdiag_fwet_patch)
  , ("wdiag_fcansno_patch",          wdiag_fcansno_patch)
  , ("wdiag_fdry_patch",             wdiag_fdry_patch)
  , ("wdiag_h2ocan_patch",           wdiag_h2ocan_patch)
  , ("wdiag_qaf_lun",                wdiag_qaf_lun)
  ]

-- | CanopyState sun/shade fields. 'surfaceRadiation' recomputes these only in
-- daylight (coszen > 0); through night steps they retain their last daytime
-- values, so they genuinely carry across a restart and must be serialized.
restartCanopyFields :: [(String, CanopyStateData -> VU.Vector Double)]
restartCanopyFields =
  [ ("cstate_laisun_patch", cstate_laisun_patch)
  , ("cstate_laisha_patch", cstate_laisha_patch)
  , ("cstate_fsun_patch",   cstate_fsun_patch)
  , ("cstate_parsun_patch", cstate_parsun_patch)
  , ("cstate_parsha_patch", cstate_parsha_patch)
  , ("cstate_lmrsun_patch", cstate_lmrsun_patch)
  , ("cstate_lmrsha_patch", cstate_lmrsha_patch)
  , ("cstate_psnsun_patch", cstate_psnsun_patch)
  , ("cstate_psnsha_patch", cstate_psnsha_patch)
  ]

restartCanopySetters :: [(String, VU.Vector Double -> CanopyStateData -> CanopyStateData)]
restartCanopySetters =
  [ ("cstate_laisun_patch", \v c -> c { cstate_laisun_patch = v })
  , ("cstate_laisha_patch", \v c -> c { cstate_laisha_patch = v })
  , ("cstate_fsun_patch",   \v c -> c { cstate_fsun_patch = v })
  , ("cstate_parsun_patch", \v c -> c { cstate_parsun_patch = v })
  , ("cstate_parsha_patch", \v c -> c { cstate_parsha_patch = v })
  , ("cstate_lmrsun_patch", \v c -> c { cstate_lmrsun_patch = v })
  , ("cstate_lmrsha_patch", \v c -> c { cstate_lmrsha_patch = v })
  , ("cstate_psnsun_patch", \v c -> c { cstate_psnsun_patch = v })
  , ("cstate_psnsha_patch", \v c -> c { cstate_psnsha_patch = v })
  ]

-- | FrictionVelocity fields that carry across steps: the canopy-air temperature
-- 'fvel_taf_patch' is a prognostic reservoir (the CanopyFluxes iteration seed),
-- and the friction velocity / aerodynamic resistances seed the Monin-Obukhov
-- iteration so they perturb the (finite-iteration) flux solve if not restored.
restartFrictionFields :: [(String, FrictionVelocityData -> VU.Vector Double)]
restartFrictionFields =
  [ ("fvel_taf_patch",   fvel_taf_patch)
  , ("fvel_uaf_patch",   fvel_uaf_patch)
  , ("fvel_um_patch",    fvel_um_patch)
  , ("fvel_ustar_patch", fvel_ustar_patch)
  , ("fvel_ram1_patch",  fvel_ram1_patch)
  ]

restartFrictionSetters :: [(String, VU.Vector Double -> FrictionVelocityData -> FrictionVelocityData)]
restartFrictionSetters =
  [ ("fvel_taf_patch",   \v r -> r { fvel_taf_patch = v })
  , ("fvel_uaf_patch",   \v r -> r { fvel_uaf_patch = v })
  , ("fvel_um_patch",    \v r -> r { fvel_um_patch = v })
  , ("fvel_ustar_patch", \v r -> r { fvel_ustar_patch = v })
  , ("fvel_ram1_patch",  \v r -> r { fvel_ram1_patch = v })
  ]

-- | SoilState fields persisted by the Fortran restart (SMP_L / HK_L) plus the
-- ground-evaporation factor, which is read by the flux solve.
restartSoilFields :: [(String, SoilStateData -> VU.Vector Double)]
restartSoilFields =
  [ ("sstate_smp_l_col",    sstate_smp_l_col)
  , ("sstate_hk_l_col",     sstate_hk_l_col)
  , ("sstate_soilbeta_col", sstate_soilbeta_col)
  ]

restartSoilSetters :: [(String, VU.Vector Double -> SoilStateData -> SoilStateData)]
restartSoilSetters =
  [ ("sstate_smp_l_col",    \v s -> s { sstate_smp_l_col = v })
  , ("sstate_hk_l_col",     \v s -> s { sstate_hk_l_col = v })
  , ("sstate_soilbeta_col", \v s -> s { sstate_soilbeta_col = v })
  ]

-- | EnergyFlux scalars that may seed the next step's surface-flux / canopy
-- temperature iteration (ground heat flux, conductances, canopy longwave).
restartEnergyScalars :: [(String, EnergyFluxData -> Double, Double -> EnergyFluxData -> EnergyFluxData)]
restartEnergyScalars =
  [ ("eflx_sh_tot_patch",   eflx_sh_tot_patch,   \x e -> e { eflx_sh_tot_patch = x })
  , ("eflx_lh_tot_patch",   eflx_lh_tot_patch,   \x e -> e { eflx_lh_tot_patch = x })
  , ("eflx_sh_grnd_patch",  eflx_sh_grnd_patch,  \x e -> e { eflx_sh_grnd_patch = x })
  , ("eflx_soil_grnd_col",  eflx_soil_grnd_col,  \x e -> e { eflx_soil_grnd_col = x })
  , ("cgrnds_patch",        cgrnds_patch,        \x e -> e { cgrnds_patch = x })
  , ("cgrndl_patch",        cgrndl_patch,        \x e -> e { cgrndl_patch = x })
  , ("cgrnd_patch",         cgrnd_patch,         \x e -> e { cgrnd_patch = x })
  , ("dlrad_patch",         dlrad_patch,         \x e -> e { dlrad_patch = x })
  , ("ulrad_patch",         ulrad_patch,         \x e -> e { ulrad_patch = x })
  , ("eflx_lwrad_out_patch",eflx_lwrad_out_patch,\x e -> e { eflx_lwrad_out_patch = x })
  , ("eflx_lwrad_net_patch",eflx_lwrad_net_patch,\x e -> e { eflx_lwrad_net_patch = x })
  ]

-- | The patch-vector counterparts (the pipeline is patch-vectorized).
restartEnergyVecs :: [(String, EnergyFluxData -> VU.Vector Double, VU.Vector Double -> EnergyFluxData -> EnergyFluxData)]
restartEnergyVecs =
  [ ("eflx_sh_tot_patch_vec",   eflx_sh_tot_patch_vec,   \v e -> e { eflx_sh_tot_patch_vec = v })
  , ("eflx_lh_tot_patch_vec",   eflx_lh_tot_patch_vec,   \v e -> e { eflx_lh_tot_patch_vec = v })
  , ("eflx_sh_grnd_patch_vec",  eflx_sh_grnd_patch_vec,  \v e -> e { eflx_sh_grnd_patch_vec = v })
  , ("cgrnds_patch_vec",        cgrnds_patch_vec,        \v e -> e { cgrnds_patch_vec = v })
  , ("cgrndl_patch_vec",        cgrndl_patch_vec,        \v e -> e { cgrndl_patch_vec = v })
  , ("cgrnd_patch_vec",         cgrnd_patch_vec,         \v e -> e { cgrnd_patch_vec = v })
  , ("dlrad_patch_vec",         dlrad_patch_vec,         \v e -> e { dlrad_patch_vec = v })
  , ("ulrad_patch_vec",         ulrad_patch_vec,         \v e -> e { ulrad_patch_vec = v })
  , ("eflx_lwrad_out_patch_vec",eflx_lwrad_out_patch_vec,\v e -> e { eflx_lwrad_out_patch_vec = v })
  , ("eflx_lwrad_net_patch_vec",eflx_lwrad_net_patch_vec,\v e -> e { eflx_lwrad_net_patch_vec = v })
  ]

-- | WaterFlux fields that differ at the restart boundary.
restartWaterFluxScalars :: [(String, WaterFluxData -> Double, Double -> WaterFluxData -> WaterFluxData)]
restartWaterFluxScalars =
  [ ("qflx_evap_tot_patch", qflx_evap_tot_patch, \x w -> w { qflx_evap_tot_patch = x })
  , ("qflx_evap_grnd_col",  qflx_evap_grnd_col,  \x w -> w { qflx_evap_grnd_col = x })
  , ("qflx_snow_grnd_col",  qflx_snow_grnd_col,  \x w -> w { qflx_snow_grnd_col = x })
  , ("qflx_surf_col",       qflx_surf_col,       \x w -> w { qflx_surf_col = x })
  , ("qflx_drain_col",      qflx_drain_col,      \x w -> w { qflx_drain_col = x })
  ]

restartWaterFluxVecs :: [(String, WaterFluxData -> VU.Vector Double, VU.Vector Double -> WaterFluxData -> WaterFluxData)]
restartWaterFluxVecs =
  [ ("qflx_evap_tot_patch_vec",  qflx_evap_tot_patch_vec,  \v w -> w { qflx_evap_tot_patch_vec = v })
  , ("qflx_evap_grnd_patch_vec", qflx_evap_grnd_patch_vec, \v w -> w { qflx_evap_grnd_patch_vec = v })
  , ("qflx_tran_veg_patch_vec",  qflx_tran_veg_patch_vec,  \v w -> w { qflx_tran_veg_patch_vec = v })
  ]

-- | Scalar CN pools that persist across steps.
restartCNScalars :: [(String, CLMState -> Double)]
restartCNScalars =
  [ ("clmLeafC", clmLeafC),       ("clmFrootC", clmFrootC)
  , ("clmLiveStemC", clmLiveStemC), ("clmDeadStemC", clmDeadStemC)
  , ("clmCPool", clmCPool),       ("clmSoilOrgC", clmSoilOrgC)
  , ("clmLitterC", clmLitterC),   ("clmSMINN", clmSMINN)
  , ("clmLeafN", clmLeafN),       ("clmFPG", clmFPG)
  , ("clmPlantNUptake", clmPlantNUptake)
  ]

-- | Setters for the scalar CN pools, in the same order as 'restartCNScalars'.
restartCNSetters :: [Double -> CLMState -> CLMState]
restartCNSetters =
  [ \x s -> s { clmLeafC = x },       \x s -> s { clmFrootC = x }
  , \x s -> s { clmLiveStemC = x },   \x s -> s { clmDeadStemC = x }
  , \x s -> s { clmCPool = x },       \x s -> s { clmSoilOrgC = x }
  , \x s -> s { clmLitterC = x },     \x s -> s { clmSMINN = x }
  , \x s -> s { clmLeafN = x },       \x s -> s { clmFPG = x }
  , \x s -> s { clmPlantNUptake = x }
  ]

-- | Write the full prognostic CLM state to @dir@ as per-variable @.bin@ files.
writeRestartState :: FilePath -> CLMState -> IO ()
writeRestartState dir st = do
  createDirectoryIfMissing True dir
  let col = clmColumn st
      tmp = clmTemp st
      wat = clmWaterState st
      wsb = clmWaterStateBulk st
      wd  = clmWaterDiagBulk st
      sh  = clmSoilHydro st
      wv name v = writeFloat64Vector (dir </> name ++ ".bin") v
      ws name x = writeFloat64Vector (dir </> name ++ ".bin") (VU.singleton x)
  -- column geometry (evolves with snow accumulation/compaction)
  wv "colZ" (colZ col); wv "colDz" (colDz col); wv "colZi" (colZi col)
  -- temperatures
  wv "t_soisno_col"     (t_soisno_col tmp)
  wv "t_soisno_bef_col" (t_soisno_bef_col tmp)
  ws "t_grnd_col"       (t_grnd_col tmp)
  ws "t_h2osfc_col"     (t_h2osfc_col tmp)
  ws "t_h2osfc_bef_col" (t_h2osfc_bef_col tmp)
  ws "t_veg_patch"      (t_veg_patch tmp)
  ws "t_ref2m_patch"    (t_ref2m_patch tmp)
  wv "t_veg_patch_vec"  (t_veg_patch_vec tmp)
  wv "t_ref2m_patch_vec"(t_ref2m_patch_vec tmp)
  -- water state
  wv "h2osoi_liq_col"   (h2osoi_liq_col wat)
  wv "h2osoi_ice_col"   (h2osoi_ice_col wat)
  wv "h2osoi_vol_col"   (h2osoi_vol_col wat)
  ws "h2osno_col"       (h2osno_col wat)
  ws "h2osfc_col"       (h2osfc_col wat)
  ws "h2ocan_patch"     (h2ocan_patch wat)
  ws "liqcan_patch"     (liqcan_patch wat)
  ws "snocan_patch"     (snocan_patch wat)
  wv "h2ocan_patch_vec" (h2ocan_patch_vec wat)
  wv "liqcan_patch_vec" (liqcan_patch_vec wat)
  wv "snocan_patch_vec" (snocan_patch_vec wat)
  -- bulk water state that carries across steps (integrated snowfall drives the
  -- snow-cover fraction; snow persistence feeds SNICAR albedo)
  wv "wsbulk_int_snow_col"         (wsbulk_int_snow_col wsb)
  wv "wsbulk_snow_persistence_col" (wsbulk_snow_persistence_col wsb)
  -- snow layer count (Int as Float64)
  ws "snl" (fromIntegral (clmSnl st))
  -- water diagnostics that carry across steps
  mapM_ (\(n, f) -> wv n (f wd)) restartWdiagFields
  -- canopy sun/shade state (recomputed only in daylight)
  mapM_ (\(n, f) -> wv n (f (clmCanopyState st))) restartCanopyFields
  -- canopy-air reservoir + Monin-Obukhov iteration seeds
  mapM_ (\(n, f) -> wv n (f (clmFrictionVel st))) restartFrictionFields
  -- soil matric potential / conductivity / evap factor
  mapM_ (\(n, f) -> wv n (f (clmSoilState st))) restartSoilFields
  -- energy-flux seeds for the surface-flux / canopy iteration
  mapM_ (\(n, f, _) -> ws n (f (clmEnergyFlux st))) restartEnergyScalars
  mapM_ (\(n, f, _) -> wv n (f (clmEnergyFlux st))) restartEnergyVecs
  -- water-flux fields at the restart boundary
  mapM_ (\(n, f, _) -> ws n (f (clmWaterFlux st))) restartWaterFluxScalars
  mapM_ (\(n, f, _) -> wv n (f (clmWaterFlux st))) restartWaterFluxVecs
  -- soil hydrology / water table
  wv "sh_zwt_col"         (sh_zwt_col sh)
  wv "sh_zwts_col"        (sh_zwts_col sh)
  wv "sh_zwt_perched_col" (sh_zwt_perched_col sh)
  wv "sh_qcharge_col"     (sh_qcharge_col sh)
  wv "sh_frost_table_col" (sh_frost_table_col sh)
  wv "sh_icefrac_col"     (sh_icefrac_col sh)
  -- scalar CN pools
  mapM_ (\(n, f) -> ws n (f st)) restartCNScalars

-- | Read a restart written by 'writeRestartState' and overlay it onto a
-- /base/ state (typically a pristine cold-start init), returning the restored
-- state. Any variable whose file is absent keeps the base value. The forcing
-- reader is step-indexed and stateless, so resuming only requires running the
-- driver from the matching step number with this restored state.
readRestartState :: CLMState -> FilePath -> IO CLMState
readRestartState base dir = do
  let rv name = do
        let f = dir </> name ++ ".bin"
        ex <- doesFileExist f
        if ex then Just <$> readFloat64Vector f else return Nothing
      rs name = do
        let f = dir </> name ++ ".bin"
        ex <- doesFileExist f
        if ex then Just <$> readFloat64Scalar f else return Nothing
  -- column
  cZ  <- rv "colZ"; cDz <- rv "colDz"; cZi <- rv "colZi"
  let col' = (clmColumn base)
        { colZ  = maybe (colZ  (clmColumn base)) id cZ
        , colDz = maybe (colDz (clmColumn base)) id cDz
        , colZi = maybe (colZi (clmColumn base)) id cZi }
  -- temperatures
  tSoi <- rv "t_soisno_col"; tSoiB <- rv "t_soisno_bef_col"
  tGr <- rs "t_grnd_col"; tHs <- rs "t_h2osfc_col"; tHsB <- rs "t_h2osfc_bef_col"
  tVeg <- rs "t_veg_patch"; tRef <- rs "t_ref2m_patch"
  tVegV <- rv "t_veg_patch_vec"; tRefV <- rv "t_ref2m_patch_vec"
  let bt = clmTemp base
      tmp' = bt
        { t_soisno_col      = maybe (t_soisno_col bt) id tSoi
        , t_soisno_bef_col  = maybe (t_soisno_bef_col bt) id tSoiB
        , t_grnd_col        = maybe (t_grnd_col bt) id tGr
        , t_h2osfc_col      = maybe (t_h2osfc_col bt) id tHs
        , t_h2osfc_bef_col  = maybe (t_h2osfc_bef_col bt) id tHsB
        , t_veg_patch       = maybe (t_veg_patch bt) id tVeg
        , t_ref2m_patch     = maybe (t_ref2m_patch bt) id tRef
        , t_veg_patch_vec   = maybe (t_veg_patch_vec bt) id tVegV
        , t_ref2m_patch_vec = maybe (t_ref2m_patch_vec bt) id tRefV }
  -- water state
  wLiq <- rv "h2osoi_liq_col"; wIce <- rv "h2osoi_ice_col"; wVol <- rv "h2osoi_vol_col"
  wSno <- rs "h2osno_col"; wSfc <- rs "h2osfc_col"
  hcan <- rs "h2ocan_patch"; lcan <- rs "liqcan_patch"; scan <- rs "snocan_patch"
  hcanV <- rv "h2ocan_patch_vec"; lcanV <- rv "liqcan_patch_vec"; scanV <- rv "snocan_patch_vec"
  let bw = clmWaterState base
      wat' = bw
        { h2osoi_liq_col   = maybe (h2osoi_liq_col bw) id wLiq
        , h2osoi_ice_col   = maybe (h2osoi_ice_col bw) id wIce
        , h2osoi_vol_col   = maybe (h2osoi_vol_col bw) id wVol
        , h2osno_col       = maybe (h2osno_col bw) id wSno
        , h2osfc_col       = maybe (h2osfc_col bw) id wSfc
        , h2ocan_patch     = maybe (h2ocan_patch bw) id hcan
        , liqcan_patch     = maybe (liqcan_patch bw) id lcan
        , snocan_patch     = maybe (snocan_patch bw) id scan
        , h2ocan_patch_vec = maybe (h2ocan_patch_vec bw) id hcanV
        , liqcan_patch_vec = maybe (liqcan_patch_vec bw) id lcanV
        , snocan_patch_vec = maybe (snocan_patch_vec bw) id scanV }
  -- bulk water state
  intSnow <- rv "wsbulk_int_snow_col"; snowPers <- rv "wsbulk_snow_persistence_col"
  let bwsb = clmWaterStateBulk base
      wsb' = bwsb
        { wsbulk_int_snow_col         = maybe (wsbulk_int_snow_col bwsb) id intSnow
        , wsbulk_snow_persistence_col = maybe (wsbulk_snow_persistence_col bwsb) id snowPers }
  -- snl
  snlR <- rs "snl"
  let snl' = maybe (clmSnl base) round snlR
  -- water diagnostics: overlay the carry-over fields onto the base record
  let bwd = clmWaterDiagBulk base
  wd' <- overlayWdiag dir bwd
  -- canopy sun/shade state
  cs' <- overlayNamed dir restartCanopySetters (clmCanopyState base)
  -- canopy-air reservoir + iteration seeds
  fv' <- overlayNamed dir restartFrictionSetters (clmFrictionVel base)
  -- soil matric potential / conductivity / evap factor
  ss' <- overlayNamed dir restartSoilSetters (clmSoilState base)
  -- energy-flux seeds (vectors then scalars)
  ef1 <- overlayNamed   dir [(n, set) | (n, _, set) <- restartEnergyVecs]    (clmEnergyFlux base)
  ef' <- overlayScalars dir [(n, set) | (n, _, set) <- restartEnergyScalars] ef1
  -- water-flux fields
  wf1 <- overlayNamed   dir [(n, set) | (n, _, set) <- restartWaterFluxVecs]    (clmWaterFlux base)
  wf' <- overlayScalars dir [(n, set) | (n, _, set) <- restartWaterFluxScalars] wf1
  -- soil hydrology
  zwt <- rv "sh_zwt_col"; zwts <- rv "sh_zwts_col"; zwtp <- rv "sh_zwt_perched_col"
  qch <- rv "sh_qcharge_col"; frost <- rv "sh_frost_table_col"; icef <- rv "sh_icefrac_col"
  let bsh = clmSoilHydro base
      sh' = bsh
        { sh_zwt_col         = maybe (sh_zwt_col bsh) id zwt
        , sh_zwts_col        = maybe (sh_zwts_col bsh) id zwts
        , sh_zwt_perched_col = maybe (sh_zwt_perched_col bsh) id zwtp
        , sh_qcharge_col     = maybe (sh_qcharge_col bsh) id qch
        , sh_frost_table_col = maybe (sh_frost_table_col bsh) id frost
        , sh_icefrac_col     = maybe (sh_icefrac_col bsh) id icef }
  -- scalar CN pools: read each (Nothing → keep base) and apply its setter
  cnVals <- mapM (\(n, _) -> rs n) restartCNScalars
  let cnApply s =
        foldl (\acc (mv, setter) -> maybe acc (`setter` acc) mv)
              s (zip cnVals restartCNSetters)
  return $ cnApply base
    { clmColumn         = col'
    , clmTemp           = tmp'
    , clmWaterState     = wat'
    , clmWaterStateBulk = wsb'
    , clmWaterDiagBulk  = wd'
    , clmCanopyState    = cs'
    , clmFrictionVel    = fv'
    , clmSoilState      = ss'
    , clmEnergyFlux     = ef'
    , clmWaterFlux      = wf'
    , clmSoilHydro      = sh'
    , clmSnl            = snl'
    }

-- | Read a Fortran CLM restart (@clm2.r.*.nc@) and overlay one column's
-- biophysical prognostic state onto a /base/ CLMState, enabling warm-start from
-- Fortran initial conditions. Returns 'Left' on a NetCDF open/read failure.
--
-- The Fortran restart stores @T_SOISNO@ / @H2OSOI_LIQ@ / @H2OSOI_ICE@ on the
-- combined snow+soil grid (@levtot = nlevsno + nlevgrnd@), snow at the top
-- indices (active layers at the bottom of the snow stack), soil below — the same
-- convention as the port's @*_col@ vectors, so the column slice copies directly.
-- @DZSNO@/@ZSNO@/@ZISNO@ carry only the @nlevsno@ snow layers; the static soil
-- geometry stays from the base. Variable names follow the Fortran restart
-- registry. @icol@ selects the column (0 = the natural-veg soil column).
readFortranRestart :: Int -> FilePath -> CLMState -> IO (Either String CLMState)
readFortranRestart icol path base = do
  eNc <- ncOpen path
  case eNc of
    Left e   -> return (Left ("ncOpen failed: " ++ e))
    Right nc -> do
      let lt = nlevsno + nlevgrnd
      tsoi   <- ncSlice2 nc "T_SOISNO"   icol lt
      liq    <- ncSlice2 nc "H2OSOI_LIQ" icol lt
      ice    <- ncSlice2 nc "H2OSOI_ICE" icol lt
      dzsno  <- ncSlice2 nc "DZSNO"  icol nlevsno
      zsno   <- ncSlice2 nc "ZSNO"   icol nlevsno
      zisno  <- ncSlice2 nc "ZISNO"  icol nlevsno
      tgrnd  <- ncScal nc "T_GRND"  icol
      th2o   <- ncScal nc "TH2OSFC" icol
      h2osfc <- ncScal nc "H2OSFC"  icol
      tveg   <- ncScal nc "T_VEG"   icol
      snoNoL <- ncScal nc "H2OSNO_NO_LAYERS" icol
      zwt    <- ncScal nc "ZWT"        icol
      zwtp   <- ncScal nc "ZWT_PERCH"  icol
      fsno   <- ncScal nc "frac_sno"     icol
      fsnoe  <- ncScal nc "frac_sno_eff" icol
      intsno <- ncScal nc "INT_SNOW"   icol
      snlD   <- ncScal nc "SNLSNO"     icol
      -- lake state (lake columns): read T_LAKE / LAKE_ICEFRAC over levlak
      eNlak  <- ncDimLen nc "levlak"
      let nlak = either (const 10) id eNlak
      tlake   <- ncSlice2 nc "T_LAKE"       icol nlak
      lakeice <- ncSlice2 nc "LAKE_ICEFRAC" icol nlak
      ncClose nc
      case tsoi of
        Nothing -> return (Left "T_SOISNO not found / wrong shape — not a CLM restart?")
        Just tsoiV -> do
          let snl' = maybe (clmSnl base) round snlD
              -- SWE = layered snow (liq+ice over the snow indices) + no-layer reservoir
              snowLayered = case (liq, ice) of
                (Just l, Just i) -> VU.sum (VU.take nlevsno l) + VU.sum (VU.take nlevsno i)
                _                -> 0.0
              h2osno' = maybe 0.0 id snoNoL + snowLayered
              -- replace the first |new| elements (the snow portion) of a combined
              -- vector, keeping the static soil tail from the base.
              ovSnow baseVec = maybe baseVec (\nv -> nv VU.++ VU.drop (VU.length nv) baseVec)
              bcol = clmColumn base
              col' = bcol { colDz = ovSnow (colDz bcol) dzsno
                          , colZ  = ovSnow (colZ  bcol) zsno
                          , colZi = ovSnow (colZi bcol) zisno }
              bt = clmTemp base
              tmp' = bt { t_soisno_col  = tsoiV
                        , t_grnd_col    = maybe (t_grnd_col bt) id tgrnd
                        , t_h2osfc_col  = maybe (t_h2osfc_col bt) id th2o
                        , t_veg_patch   = maybe (t_veg_patch bt) id tveg
                        , t_veg_patch_vec =
                            maybe (t_veg_patch_vec bt)
                                  (\tv -> VU.replicate (VU.length (t_veg_patch_vec bt)) tv) tveg }
              bw = clmWaterState base
              wat' = bw { h2osoi_liq_col = maybe (h2osoi_liq_col bw) id liq
                        , h2osoi_ice_col = maybe (h2osoi_ice_col bw) id ice
                        , h2osno_col     = h2osno'
                        , h2osfc_col     = maybe (h2osfc_col bw) id h2osfc }
              bwd = clmWaterDiagBulk base
              setCol1 mx baseVec = maybe baseVec VU.singleton mx
              wd' = bwd { wdiag_frac_sno_col     = setCol1 fsno  (wdiag_frac_sno_col bwd)
                        , wdiag_frac_sno_eff_col = setCol1 fsnoe (wdiag_frac_sno_eff_col bwd) }
              bwsb = clmWaterStateBulk base
              wsb' = bwsb { wsbulk_int_snow_col = setCol1 intsno (wsbulk_int_snow_col bwsb) }
              bsh = clmSoilHydro base
              sh' = bsh { sh_zwt_col         = setCol1 zwt  (sh_zwt_col bsh)
                        , sh_zwt_perched_col = setCol1 zwtp (sh_zwt_perched_col bsh) }
              blake = clmLakeState base
              lake' = blake
                { lake_t_lake_col       = maybe (lake_t_lake_col blake) id tlake
                , lake_lake_icefrac_col = maybe (lake_lake_icefrac_col blake) id lakeice }
          return $ Right base
            { clmColumn         = col'
            , clmTemp           = tmp'
            , clmWaterState     = wat'
            , clmWaterDiagBulk  = wd'
            , clmWaterStateBulk = wsb'
            , clmSoilHydro      = sh'
            , clmLakeState      = lake'
            , clmSnl            = snl'
            }

-- | Read a Fortran restart 2D variable @(column, n)@ and return column @icol@'s
-- @n@-element slice (NetCDF C-order: column is the slow dimension). 'Nothing'
-- if the variable is absent or the read is too short.
ncSlice2 :: NcFile -> String -> Int -> Int -> IO (Maybe (VU.Vector Double))
ncSlice2 nc name icol n = do
  r <- ncReadDouble2D nc name
  return $ case r of
    Right v | VU.length v >= (icol + 1) * n -> Just (VU.slice (icol * n) n v)
    _ -> Nothing

-- | Read a Fortran restart @(column)@ variable, returning element @icol@.
-- Also reads @int@ variables (nc_get_var_double auto-converts).
ncScal :: NcFile -> String -> Int -> IO (Maybe Double)
ncScal nc name icol = do
  r <- ncReadDouble1D nc name
  return $ case r of
    Right v | VU.length v > icol -> Just (v VU.! icol)
    _ -> Nothing

-- | Overlay every named vector field present on disk onto a base record, via
-- its (name, setter) table. A field whose file is absent keeps the base value.
-- Setters are explicit so a missing field is a compile error, not a silent drop.
overlayNamed :: FilePath
             -> [(String, VU.Vector Double -> r -> r)]
             -> r -> IO r
overlayNamed dir setters = go setters
  where
    go [] rec = return rec
    go ((name, setter) : rest) rec = do
      let f = dir </> name ++ ".bin"
      ex <- doesFileExist f
      rec' <- if ex then (\v -> setter v rec) <$> readFloat64Vector f else return rec
      go rest rec'

-- | Like 'overlayNamed' but for scalar (length-1) files.
overlayScalars :: FilePath
               -> [(String, Double -> r -> r)]
               -> r -> IO r
overlayScalars dir setters = go setters
  where
    go [] rec = return rec
    go ((name, setter) : rest) rec = do
      let f = dir </> name ++ ".bin"
      ex <- doesFileExist f
      rec' <- if ex then (\x -> setter x rec) <$> readFloat64Scalar f else return rec
      go rest rec'

-- | Overlay the carry-over WaterDiagnosticBulk fields onto the base record.
overlayWdiag :: FilePath -> WaterDiagnosticBulkData -> IO WaterDiagnosticBulkData
overlayWdiag dir = overlayNamed dir restartWdiagSetters

-- | Setters paired with the same names as 'restartWdiagFields'.
restartWdiagSetters :: [(String, VU.Vector Double -> WaterDiagnosticBulkData -> WaterDiagnosticBulkData)]
restartWdiagSetters =
  [ ("wdiag_qg_snow_col",            \v wd -> wd { wdiag_qg_snow_col = v })
  , ("wdiag_qg_soil_col",            \v wd -> wd { wdiag_qg_soil_col = v })
  , ("wdiag_qg_h2osfc_col",          \v wd -> wd { wdiag_qg_h2osfc_col = v })
  , ("wdiag_qg_col",                 \v wd -> wd { wdiag_qg_col = v })
  , ("wdiag_dqgdT_col",              \v wd -> wd { wdiag_dqgdT_col = v })
  , ("wdiag_snow_depth_col",         \v wd -> wd { wdiag_snow_depth_col = v })
  , ("wdiag_snowdp_col",             \v wd -> wd { wdiag_snowdp_col = v })
  , ("wdiag_snow_5day_col",          \v wd -> wd { wdiag_snow_5day_col = v })
  , ("wdiag_snomelt_accum_col",      \v wd -> wd { wdiag_snomelt_accum_col = v })
  , ("wdiag_frac_sno_col",           \v wd -> wd { wdiag_frac_sno_col = v })
  , ("wdiag_frac_sno_eff_col",       \v wd -> wd { wdiag_frac_sno_eff_col = v })
  , ("wdiag_frac_h2osfc_col",        \v wd -> wd { wdiag_frac_h2osfc_col = v })
  , ("wdiag_frac_h2osfc_nosnow_col", \v wd -> wd { wdiag_frac_h2osfc_nosnow_col = v })
  , ("wdiag_snw_rds_col",            \v wd -> wd { wdiag_snw_rds_col = v })
  , ("wdiag_snw_rds_top_col",        \v wd -> wd { wdiag_snw_rds_top_col = v })
  , ("wdiag_snow_persist_col",       \v wd -> wd { wdiag_snow_persist_col = v })
  , ("wdiag_frac_iceold_col",        \v wd -> wd { wdiag_frac_iceold_col = v })
  , ("wdiag_swe_old_col",            \v wd -> wd { wdiag_swe_old_col = v })
  , ("wdiag_snowice_col",            \v wd -> wd { wdiag_snowice_col = v })
  , ("wdiag_snowliq_col",            \v wd -> wd { wdiag_snowliq_col = v })
  , ("wdiag_h2osno_top_col",         \v wd -> wd { wdiag_h2osno_top_col = v })
  , ("wdiag_sno_liq_top_col",        \v wd -> wd { wdiag_sno_liq_top_col = v })
  , ("wdiag_bw_col",                 \v wd -> wd { wdiag_bw_col = v })
  , ("wdiag_h2osno_total_col",       \v wd -> wd { wdiag_h2osno_total_col = v })
  , ("wdiag_fwet_patch",             \v wd -> wd { wdiag_fwet_patch = v })
  , ("wdiag_fcansno_patch",          \v wd -> wd { wdiag_fcansno_patch = v })
  , ("wdiag_fdry_patch",             \v wd -> wd { wdiag_fdry_patch = v })
  , ("wdiag_h2ocan_patch",           \v wd -> wd { wdiag_h2ocan_patch = v })
  , ("wdiag_qaf_lun",                \v wd -> wd { wdiag_qaf_lun = v })
  ]

-- ============================================================================
-- Multi-landunit gridcell (Phase 4 #12, Option A: loop the single-column kernel)
-- ============================================================================

-- | Landunit area fractions (percent) + lake depth from a NetCDF surfdata file.
data SurfdataLandunits = SurfdataLandunits
  { sl_pct_natveg  :: !Double
  , sl_pct_lake    :: !Double
  , sl_pct_glacier :: !Double
  , sl_pct_crop    :: !Double
  , sl_pct_wetland :: !Double
  , sl_pct_urban   :: !Double
  , sl_lakedepth   :: !Double
  } deriving (Show)

-- | Read landunit fractions + lake depth directly from a NetCDF surfdata file —
-- no @.bin@ export step. (The cold-start/forcing/param @.bin@ pipeline is a
-- historical Julia-export artifact; with the NetCDF reader we can take surfdata
-- straight from @.nc@.) Missing variables default to 0 (lakedepth to 10).
readSurfdataLandunits :: FilePath -> IO (Either String SurfdataLandunits)
readSurfdataLandunits path = do
  e <- ncOpen path
  case e of
    Left err -> return (Left ("ncOpen surfdata failed: " ++ err))
    Right nc -> do
      let rd v dflt = either (const dflt) id <$> ncReadDoubleScalar nc v
      nv <- rd "PCT_NATVEG" 0.0
      lk <- rd "PCT_LAKE"    0.0
      gl <- rd "PCT_GLACIER" 0.0
      cr <- rd "PCT_CROP"    0.0
      wl <- rd "PCT_WETLAND" 0.0
      ur <- rd "PCT_URBAN"   0.0
      ld <- rd "LAKEDEPTH"  10.0
      ncClose nc
      return $ Right SurfdataLandunits
        { sl_pct_natveg = nv, sl_pct_lake = lk, sl_pct_glacier = gl
        , sl_pct_crop = cr, sl_pct_wetland = wl, sl_pct_urban = ur
        , sl_lakedepth = ld }

-- | Run a soil + lake gridcell as the column-loop realization of the
-- multi-landunit driver (PHASE4_SCOPE Option A): the soil column runs the full
-- wired physics pipeline and the lake column runs the lake surface-flux +
-- temperature path, both sharing the same forcing; gridcell diagnostics are the
-- area-weighted average of the two columns. Columns are independent within a
-- timestep (they interact only through the gridcell aggregate to the
-- atmosphere), so looping the kernel and weighting the outputs is exact for one
-- gridcell. Returns per-step @(gridcell, soil, lake)@ of @(T_GRND, H2OSNO)@.
runMixedGridcell
  :: FilePath  -- ^ data dir (soil base + forcing)
  -> Double    -- ^ natural-veg (soil) area weight
  -> Double    -- ^ lake area weight
  -> Double    -- ^ lake depth [m]
  -> Double    -- ^ dtime [s]
  -> Int       -- ^ forcing step offset
  -> Int       -- ^ number of steps
  -> IO [((Double, Double), (Double, Double), (Double, Double))]
runMixedGridcell dir wNatveg wLake lakeDepth dtime off nsteps = do
  (st0, forcing, albConst) <- initCLMStateFromDir dir
  chParams  <- readCanopyHydroParamsFromDir dir
  snicarOpt <- readSnicarOptics dir
  let pipeline = wiredPhysicsPipeline albConst chParams snicarOpt
      cfg      = defaultDriverConfig
      nlevlak  = 10
      ntot     = nlevsno + nlevgrnd
      -- Build the real subgrid hierarchy for this 1-gridcell / 2-landunit /
      -- 2-column mixed cell (soil landunit -> soil column, lake landunit ->
      -- lake column) and route the gridcell aggregation through the ported
      -- SubgridAverage.c2g1d (column -> gridcell by area weight) instead of an
      -- ad-hoc weighted sum. The down-propagated gridcell weight is the
      -- landunit area weight (each column is the whole of its landunit).
      sgBounds = IS.BoundsType 1 1 1 2 1 2 1 2
      (sgLunA, sgL1) = IS.addLandunit (IS.defaultLandunitData 2) 0 1 IS.istsoil wNatveg
      (sgLunB, sgL2) = IS.addLandunit sgLunA 1 1 IS.istdlak wLake
      (sgColA, sgC1) = IS.addColumn (IS.defaultSubgridColumnData 2) sgLunB 0 sgL1 1 1.0 False
      (sgColB, sgC2) = IS.addColumn sgColA sgLunB 1 sgL2 1 1.0 False
      (sgPchA, _)    = IS.addPatch (IS.defaultSubgridPatchData 2) sgColB sgLunB 0 sgC1 1 1.0 0
      (sgPchB, _)    = IS.addPatch sgPchA sgColB sgLunB 1 sgC2 0 1.0 0
      (_, sgLun, sgCol0) = IS.clmPtrsCompdown sgBounds (IS.defaultGridcellData 1) sgLunB sgColB sgPchB
      sgCol = sgCol0 { IS.colWtgcell = VU.fromList [wNatveg, wLake] }
      -- column -> gridcell area-weighted aggregate of a soil/lake column pair
      c2g sVal lVal =
        SA.c2g1d (VU.fromList [sVal, lVal]) sgBounds SA.C2LUnity SA.L2GUnity sgCol sgLun
          VU.! 0
      soil0    = st0
      lake0    = st0
        { clmColumn = (clmColumn st0) { lakedepth = lakeDepth }
        , clmSnl = 0
        , clmTemp = (clmTemp st0)
            { t_grnd_col = 277.0, t_soisno_col = VU.replicate ntot 277.0 }
        , clmWaterState = (clmWaterState st0)
            { h2osno_col = 0.0
            , h2osoi_liq_col = VU.replicate ntot 0.0
            , h2osoi_ice_col = VU.replicate ntot 0.0 }
        , clmLakeState = (clmLakeState st0)
            { lake_t_lake_col = VU.replicate nlevlak 277.0
            , lake_lake_icefrac_col = VU.replicate nlevlak 0.0 }
        }
      go _ _ _ step acc | step > nsteps = return (reverse acc)
      go soilSt lakeSt drvSt step acc = do
        let ctx = buildTimestepContext forcing (off + step) dtime
            (drvSt', soilSt') = clmDrv cfg pipeline ctx drvSt soilSt
            lakeSt' = lakeTemperatureStep cfg ctx (lakeFluxesStep cfg ctx lakeSt)
            sTG = t_grnd_col (clmTemp soilSt')
            sSno = h2osno_col (clmWaterState soilSt')
            lTG = t_grnd_col (clmTemp lakeSt')
            lSno = h2osno_col (clmWaterState lakeSt')
        go soilSt' lakeSt' drvSt' (step + 1)
           ( ( (c2g sTG lTG, c2g sSno lSno)
             , (sTG, sSno), (lTG, lSno) ) : acc )
  go soil0 lake0 defaultDriverState 1 []

runPipeline :: PipelineConfig -> IO [DailyDiag]
runPipeline cfg = do
  let dir = pcDataDir cfg
      dtime = pcDtime cfg
      ndays = pcNdays cfg
      stepsPerDay = round (86400.0 / dtime) :: Int
      totalSteps = ndays * stepsPerDay

  (st0_, forcing, albConst) <- initCLMStateFromDir dir
  chParams <- readCanopyHydroParamsFromDir dir
  snicarOpt <- readSnicarOptics dir

  let st0 = if pcUseCN cfg
            then initCNDecompPools (st0_ { clmCNActive = True
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
                       })
            else st0_

  when (pcVerbose cfg) $ do
    putStrLn $ "Pipeline runner: " ++ show ndays ++ " days, "
            ++ show stepsPerDay ++ " steps/day, dtime=" ++ show dtime ++ "s"
    when (pcUseCN cfg) $
      putStrLn $ "  CN biogeochemistry ENABLED (leafC=" ++ show (clmLeafC st0)
              ++ ", soilOrgC=" ++ show (clmSoilOrgC st0) ++ " gC/m2)"

  let drvCfg = defaultDriverConfig
      pipeline = wiredPhysicsPipeline albConst chParams snicarOpt

  -- Optional warm-start: overlay a Fortran restart so the snow/soil IC matches.
  st0w <- case pcFortranRestart cfg of
            Nothing -> return st0
            Just rp -> either (const st0) id <$> readFortranRestart 0 rp st0
  econ <- if pcUseCndv cfg then readDGVEcophysCon (dir </> "params")
                           else return defaultDGVEcophysCon
  st0c <- loadCNParams cfg dir (seedCNDV cfg econ st0w)
  go st0c st0c defaultDriverState forcing 1 zeroDailyDiag [] totalSteps stepsPerDay drvCfg dtime pipeline
  where
    go !base !st !drvSt !fr !step !dayAcc !results !total !spd !drvCfg !dtime !pl
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
              -- Restart round-trip test hook: write the full prognostic state,
              -- read it back onto the pristine base, and continue from it.
              st'' <- case pcRestartRoundtripDay cfg of
                        Just k | k == dayNum -> do
                          let rdir = pcDataDir cfg </> "restart-roundtrip"
                          writeRestartState rdir st'
                          readRestartState base rdir
                        _ -> return st'
              go base st'' drvSt' fr (step + 1) zeroDailyDiag (avg : results) total spd drvCfg dtime pl
            else
              go base st' drvSt' fr (step + 1) dayAcc' results total spd drvCfg dtime pl

-- ============================================================================
-- NetCDF history output (Phase 4 #16)
-- ============================================================================

-- | Write the daily diagnostics to a NetCDF history tape: one "time" dimension,
-- one double variable per field, each with a @long_name@ attribute. This is the
-- single-column analogue of a CLM history tape — machine-comparable to CTSM
-- output instead of CSV. Returns 'Left' on any NetCDF write error.
--
-- Round-trips with the NetCDF reader (write then 'ncReadDouble1D' returns the
-- same series), which is how it is validated.
writeDailyNetCDF :: FilePath -> [DailyDiag] -> IO (Either String ())
writeDailyNetCDF path dailies =
  ncWriteTimeseries path (length dailies)
    [ ("T_GRND",      "ground temperature [K]",                 col dd_t_grnd)
    , ("FSA",         "absorbed solar radiation [W/m2]",        col dd_fsa)
    , ("EFLX_LH_TOT", "total latent heat flux [W/m2]",          col dd_eflx_lh)
    , ("EFLX_SH_TOT", "total sensible heat flux [W/m2]",        col dd_eflx_sh)
    , ("H2OSNO",      "snow water equivalent [kg/m2]",          col dd_h2osno)
    , ("SNOW_DEPTH",  "snow depth [m]",                         col dd_snow_depth)
    , ("FRAC_SNO",    "fraction of ground covered by snow",     col dd_frac_sno)
    , ("GPP",         "gross primary production [gC/m2/s]",     col dd_gpp)
    , ("NPP",         "net primary production [gC/m2/s]",       col dd_npp)
    , ("NEE",         "net ecosystem exchange [gC/m2/s]",       col dd_nee)
    , ("HR",          "heterotrophic respiration [gC/m2/s]",    col dd_hr)
    , ("LEAFC",       "leaf carbon [gC/m2]",                    col dd_leafc)
    , ("SOILORGC",    "soil organic carbon [gC/m2]",            col dd_soilorgc)
    ]
  where
    col f = VU.fromList (map f dailies)

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
  snicarOpt <- readSnicarOptics dir

  let st0 = if pcUseCN cfg
            then initCNDecompPools (st0_ { clmCNActive = True
                       , clmLeafC = 200.0, clmFrootC = 150.0
                       , clmLiveStemC = 500.0, clmDeadStemC = 5000.0
                       , clmSoilOrgC = 8000.0, clmLitterC = 300.0
                       , clmSMINN = 5.0, clmLeafN = 8.0, clmFPG = 1.0
                       })
            else st0_

  let drvCfg = defaultDriverConfig
      pipeline = wiredPhysicsPipeline albConst chParams snicarOpt

  econ <- if pcUseCndv cfg then readDGVEcophysCon (dir </> "params")
                           else return defaultDGVEcophysCon
  st0c <- loadCNParams cfg dir (seedCNDV cfg econ st0)
  goQ st0c defaultDriverState forcing 1 [] totalSteps drvCfg dtime pipeline
  where
    goQ !st !drvSt !fr !step !qAcc !total !drvCfg !dtime !pl
      | step > total = return (reverse qAcc)
      | otherwise = do
          let ctx = buildTimestepContext fr step dtime
              (!drvSt', !st') = clmDrv drvCfg pl ctx drvSt st
              wf = clmWaterFlux st'
              qrunoff = qflx_surf_col wf + qflx_drain_col wf
          goQ st' drvSt' fr (step + 1) (qrunoff : qAcc) total drvCfg dtime pl
