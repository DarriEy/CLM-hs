-- | Parameter reading structures and pure defaults.
-- Ported from Julia: src/infrastructure/read_params.jl
-- Fortran: src/main/readParamsMod.F90
--
-- Binary I/O reads parameters exported by Julia's export_test_data.jl.
-- Expected directory: params/ with individual .bin files per parameter.
module CLM.Infrastructure.ReadParams
  ( -- * Top-level parameter reader
    readParameters
  , readParametersBinary
    -- * Individual parameter structures
  , InitVerticalParams(..)
  , defaultInitVerticalParams
  , PhotParams(..)
  , defaultPhotParams
  , CanopyHydrologyParams(..)
  , defaultCanopyHydrologyParams
  , SnowHydrologyParams(..)
  , defaultSnowHydrologyParams
  , SoilHydrologyParams(..)
  , defaultSoilHydrologyParams
  , SnicarParams(..)
  , defaultSnicarParams
  , CanopyFluxesParams(..)
  , defaultCanopyFluxesParams
  , ThermalParams(..)
  , defaultThermalParams
  , SCFParams(..)
  , defaultSCFParams
  , CH4Params(..)
  , defaultCH4Params
  , LunaParams(..)
  , defaultLunaParams
    -- * PFT constants
  , PFTConstants(..)
  , defaultPFTConstants
    -- * Dynamic vegetation (CNDV) ecophysiological constants
  , readDGVEcophysCon
    -- * MEGAN VOC emission factors and wood-product partitions
  , readMeganIsopreneEF
  , readPprod10
  , readPprod100
    -- * Snow layer constants
  , SnowLayerConstants(..)
  , defaultSnowLayerConstants
    -- * All params container
  , AllParams(..)
  , defaultAllParams
  ) where

import qualified Data.Vector.Unboxed as VU
import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, doesFileExist)

import CLM.Infrastructure.BinaryIO
  ( readFloat64Vector, readFloat64Scalar )
import CLM.Types.DGVSData (DGVEcophysCon(..), defaultDGVEcophysCon)

-- ---------------------------------------------------------------------------
-- initVertical scalar parameters
-- ---------------------------------------------------------------------------

data InitVerticalParams = InitVerticalParams
  { ivp_slopebeta    :: !Double
  , ivp_slopemax     :: !Double
  , ivp_zbedrock     :: !Double  -- ^ Negative means don't substitute
  , ivp_zbedrock_sf  :: !Double
  } deriving (Show)

defaultInitVerticalParams :: InitVerticalParams
defaultInitVerticalParams = InitVerticalParams 0.0 0.0 (-1.0) 1.0

-- ---------------------------------------------------------------------------
-- Snow cover fraction parameters
-- ---------------------------------------------------------------------------

data SCFParams = SCFParams
  { scf_n_melt_coef  :: !Double
  , scf_accum_factor :: !Double
  } deriving (Show)

defaultSCFParams :: SCFParams
defaultSCFParams = SCFParams 200.0 0.1

-- ---------------------------------------------------------------------------
-- Thermal conductivity / heat capacity parameters
-- ---------------------------------------------------------------------------

data ThermalParams = ThermalParams
  { thp_csol_sand :: !Double  -- ^ J/(m3.K)
  , thp_csol_clay :: !Double
  , thp_csol_om   :: !Double
  , thp_tkd_sand  :: !Double  -- ^ W/(m.K)
  , thp_tkd_clay  :: !Double
  , thp_tkm_om    :: !Double
  , thp_tkd_om    :: !Double
  , thp_pd        :: !Double  -- ^ Particle density [kg/m3]
  } deriving (Show)

defaultThermalParams :: ThermalParams
defaultThermalParams = ThermalParams
  { thp_csol_sand = 2.128e6
  , thp_csol_clay = 2.385e6
  , thp_csol_om   = 2.5e6
  , thp_tkd_sand  = 8.80
  , thp_tkd_clay  = 2.92
  , thp_tkm_om    = 0.25
  , thp_tkd_om    = 0.05
  , thp_pd        = 2700.0
  }

-- ---------------------------------------------------------------------------
-- Photosynthesis parameters
-- ---------------------------------------------------------------------------

data PhotParams = PhotParams
  { pp_theta_ip     :: !Double
  , pp_act25        :: !Double
  , pp_fnr          :: !Double
  , pp_cp25_yr2000  :: !Double
  , pp_kc25_coef    :: !Double
  , pp_ko25_coef    :: !Double
  , pp_fnps         :: !Double
  , pp_theta_psii   :: !Double
  , pp_vcmaxha      :: !Double
  , pp_jmaxha       :: !Double
  , pp_tpuha        :: !Double
  , pp_lmrha        :: !Double
  , pp_kcha         :: !Double
  , pp_koha         :: !Double
  , pp_cpha         :: !Double
  , pp_vcmaxhd      :: !Double
  , pp_jmaxhd       :: !Double
  , pp_tpuhd        :: !Double
  , pp_lmrhd        :: !Double
  , pp_lmrse        :: !Double
  , pp_tpu25ratio   :: !Double
  , pp_kp25ratio    :: !Double
  , pp_vcmaxse_sf   :: !Double
  , pp_jmaxse_sf    :: !Double
  , pp_tpuse_sf     :: !Double
  , pp_jmax25top_sf :: !Double
  -- PFT-dimensioned
  , pp_theta_cj     :: !(VU.Vector Double)
  , pp_krmax        :: !(VU.Vector Double)
  , pp_lmr_intercept_atkin :: !(VU.Vector Double)
  } deriving (Show)

defaultPhotParams :: PhotParams
defaultPhotParams = PhotParams
  { pp_theta_ip     = 0.95
  , pp_act25        = 72.0
  , pp_fnr          = 7.16
  , pp_cp25_yr2000  = 42.75e-6
  , pp_kc25_coef    = 404.9e-6
  , pp_ko25_coef    = 278.4e-3
  , pp_fnps         = 0.15
  , pp_theta_psii   = 0.7
  , pp_vcmaxha      = 65330.0
  , pp_jmaxha       = 43540.0
  , pp_tpuha        = 53100.0
  , pp_lmrha        = 46390.0
  , pp_kcha         = 79430.0
  , pp_koha         = 36380.0
  , pp_cpha         = 37830.0
  , pp_vcmaxhd      = 149250.0
  , pp_jmaxhd       = 152040.0
  , pp_tpuhd        = 150650.0
  , pp_lmrhd        = 150650.0
  , pp_lmrse        = 490.0
  , pp_tpu25ratio   = 0.167
  , pp_kp25ratio    = 20160.0
  , pp_vcmaxse_sf   = 1.0
  , pp_jmaxse_sf    = 1.0
  , pp_tpuse_sf     = 1.0
  , pp_jmax25top_sf = 1.0
  , pp_theta_cj     = VU.empty
  , pp_krmax        = VU.empty
  , pp_lmr_intercept_atkin = VU.empty
  }

-- ---------------------------------------------------------------------------
-- Canopy hydrology parameters
-- ---------------------------------------------------------------------------

data CanopyHydrologyParams = CanopyHydrologyParams
  { chp_interception_fraction        :: !Double
  , chp_maximum_leaf_wetted_fraction :: !Double
  , chp_snowcan_unload_wind_fact     :: !Double
  , chp_snowcan_unload_temp_fact     :: !Double
  , chp_liq_canopy_storage_scalar    :: !Double
  , chp_snow_canopy_storage_scalar   :: !Double
  } deriving (Show)

defaultCanopyHydrologyParams :: CanopyHydrologyParams
defaultCanopyHydrologyParams = CanopyHydrologyParams 1.0 1.0 1.56e5 1.87e5 0.04 6.0

-- ---------------------------------------------------------------------------
-- Snow hydrology parameters
-- ---------------------------------------------------------------------------

data SnowHydrologyParams = SnowHydrologyParams
  { shp_wimp                       :: !Double
  , shp_ssi                        :: !Double
  , shp_drift_gs                   :: !Double
  , shp_eta0_anderson              :: !Double
  , shp_eta0_vionnet               :: !Double
  , shp_wind_snowcompact_fact      :: !Double
  , shp_rho_max                    :: !Double
  , shp_tau_ref                    :: !Double
  , shp_ceta                       :: !Double
  , shp_snw_rds_min                :: !Double
  , shp_upplim_destruct_metamorph  :: !Double
  , shp_scvng_fct_mlt_sf           :: !Double
  , shp_scvng_fct_mlt_bcphi        :: !Double
  , shp_scvng_fct_mlt_bcpho        :: !Double
  , shp_scvng_fct_mlt_dst1         :: !Double
  , shp_scvng_fct_mlt_dst2         :: !Double
  , shp_scvng_fct_mlt_dst3         :: !Double
  , shp_scvng_fct_mlt_dst4         :: !Double
  } deriving (Show)

defaultSnowHydrologyParams :: SnowHydrologyParams
defaultSnowHydrologyParams = SnowHydrologyParams
  { shp_wimp = 0.05, shp_ssi = 0.033, shp_drift_gs = 0.35
  , shp_eta0_anderson = 9.0e5, shp_eta0_vionnet = 7.62237e6
  , shp_wind_snowcompact_fact = 5.0, shp_rho_max = 350.0
  , shp_tau_ref = 172800.0, shp_ceta = 250.0
  , shp_snw_rds_min = 54.526, shp_upplim_destruct_metamorph = 100.0
  , shp_scvng_fct_mlt_sf = 1.0, shp_scvng_fct_mlt_bcphi = 0.20
  , shp_scvng_fct_mlt_bcpho = 0.03, shp_scvng_fct_mlt_dst1 = 0.02
  , shp_scvng_fct_mlt_dst2 = 0.02, shp_scvng_fct_mlt_dst3 = 0.01
  , shp_scvng_fct_mlt_dst4 = 0.01
  }

-- ---------------------------------------------------------------------------
-- Soil hydrology parameters
-- ---------------------------------------------------------------------------

data SoilHydrologyParams = SoilHydrologyParams
  { sohp_n_baseflow              :: !Double
  , sohp_perched_baseflow_scalar :: !Double
  , sohp_e_ice                   :: !Double
  , sohp_aq_sp_yield_min         :: !Double
  } deriving (Show)

defaultSoilHydrologyParams :: SoilHydrologyParams
defaultSoilHydrologyParams = SoilHydrologyParams 1.0 5.0e-4 6.0 0.01

-- ---------------------------------------------------------------------------
-- SNICAR parameters
-- ---------------------------------------------------------------------------

data SnicarParams = SnicarParams
  { snp_fresh_snw_rds_max :: !Double
  , snp_snw_rds_min       :: !Double
  , snp_snw_rds_refrz     :: !Double
  , snp_xdrdt             :: !Double
  , snp_C2_liq_Brun89     :: !Double
  } deriving (Show)

defaultSnicarParams :: SnicarParams
defaultSnicarParams = SnicarParams 204.526 54.526 1000.0 1.0 4.22e-13

-- ---------------------------------------------------------------------------
-- Canopy fluxes parameters
-- ---------------------------------------------------------------------------

data CanopyFluxesParams = CanopyFluxesParams
  { cfp_csoilc   :: !Double
  , cfp_cv       :: !Double
  , cfp_wind_min :: !Double
  , cfp_lai_dl   :: !Double
  , cfp_z_dl     :: !Double
  , cfp_a_coef   :: !Double
  , cfp_a_exp    :: !Double
  } deriving (Show)

defaultCanopyFluxesParams :: CanopyFluxesParams
defaultCanopyFluxesParams = CanopyFluxesParams 0.004 0.01 1.0 0.5 0.05 0.5 1.0

-- ---------------------------------------------------------------------------
-- CH4 methane parameters
-- ---------------------------------------------------------------------------

data CH4Params = CH4Params
  { ch4p_aereoxid           :: !Double
  , ch4p_q10ch4             :: !Double
  , ch4p_q10ch4base         :: !Double
  , ch4p_f_ch4              :: !Double
  , ch4p_rootlitfrac        :: !Double
  , ch4p_cnscalefactor      :: !Double
  , ch4p_redoxlag           :: !Double
  , ch4p_lake_decomp_fact   :: !Double
  , ch4p_redoxlag_vertical  :: !Double
  , ch4p_pHmax              :: !Double
  , ch4p_pHmin              :: !Double
  , ch4p_oxinhib            :: !Double
  , ch4p_q10_ch4oxid        :: !Double
  , ch4p_smp_crit           :: !Double
  , ch4p_scale_factor_aere  :: !Double
  , ch4p_nongrassporosratio :: !Double
  , ch4p_porosmin           :: !Double
  , ch4p_vgc_max            :: !Double
  , ch4p_satpow             :: !Double
  , ch4p_scale_factor_gasdiff :: !Double
  , ch4p_scale_factor_liqdiff :: !Double
  , ch4p_f_sat              :: !Double
  , ch4p_qflxlagd           :: !Double
  , ch4p_highlatfact        :: !Double
  , ch4p_q10lakebase        :: !Double
  , ch4p_atmch4             :: !Double
  , ch4p_rob                :: !Double
  , ch4p_capthick           :: !Double
  , ch4p_om_frac_sf         :: !Double
  -- Hardcoded overrides (Fortran FIX comments)
  , ch4p_vmax_ch4_oxid      :: !Double
  , ch4p_k_m                :: !Double
  , ch4p_k_m_o2             :: !Double
  , ch4p_k_m_unsat          :: !Double
  , ch4p_vmax_oxid_unsat    :: !Double
  , ch4p_unsat_aere_ratio   :: !Double
  } deriving (Show)

defaultCH4Params :: CH4Params
defaultCH4Params = CH4Params
  { ch4p_aereoxid = 0.5, ch4p_q10ch4 = 1.5, ch4p_q10ch4base = 295.0
  , ch4p_f_ch4 = 0.2, ch4p_rootlitfrac = 0.5, ch4p_cnscalefactor = 1.0
  , ch4p_redoxlag = 30.0, ch4p_lake_decomp_fact = 2.0e-8
  , ch4p_redoxlag_vertical = 30.0, ch4p_pHmax = 9.0, ch4p_pHmin = 2.2
  , ch4p_oxinhib = 10.0, ch4p_q10_ch4oxid = 1.9, ch4p_smp_crit = -2.4e5
  , ch4p_scale_factor_aere = 1.0, ch4p_nongrassporosratio = 0.33
  , ch4p_porosmin = 0.05, ch4p_vgc_max = 0.15, ch4p_satpow = 2.0
  , ch4p_scale_factor_gasdiff = 1.0, ch4p_scale_factor_liqdiff = 1.0
  , ch4p_f_sat = 0.95, ch4p_qflxlagd = 30.0, ch4p_highlatfact = 2.0
  , ch4p_q10lakebase = 298.0, ch4p_atmch4 = 1.7e-6, ch4p_rob = 3.0
  , ch4p_capthick = 100.0, ch4p_om_frac_sf = 1.0
  -- Hardcoded overrides
  , ch4p_vmax_ch4_oxid  = 45.0e-6 * 1000.0 / 3600.0
  , ch4p_k_m            = 5.0e-6 * 1000.0
  , ch4p_k_m_o2         = 20.0e-6 * 1000.0
  , ch4p_k_m_unsat      = 5.0e-6 * 1000.0 / 10.0
  , ch4p_vmax_oxid_unsat = 45.0e-6 * 1000.0 / 3600.0 / 10.0
  , ch4p_unsat_aere_ratio = 0.05 / 0.3
  }

-- ---------------------------------------------------------------------------
-- LUNA parameters
-- ---------------------------------------------------------------------------

data LunaParams = LunaParams
  { lp_cp25_yr2000         :: !Double
  , lp_kc25_coef           :: !Double
  , lp_ko25_coef           :: !Double
  , lp_luna_theta_cj       :: !Double
  , lp_enzyme_turnover_daily :: !Double
  , lp_relhExp             :: !Double
  , lp_minrelh             :: !Double
  -- PFT-dimensioned
  , lp_jmaxb0              :: !(VU.Vector Double)
  , lp_jmaxb1              :: !(VU.Vector Double)
  , lp_wc2wjb0             :: !(VU.Vector Double)
  } deriving (Show)

defaultLunaParams :: LunaParams
defaultLunaParams = LunaParams
  { lp_cp25_yr2000 = 42.75e-6 * 1.0e5
  , lp_kc25_coef   = 404.9e-6 * 1.0e5
  , lp_ko25_coef   = 278.4e-3 * 1.0e5
  , lp_luna_theta_cj = 0.9
  , lp_enzyme_turnover_daily = 0.1
  , lp_relhExp = 6.0
  , lp_minrelh = 0.25
  , lp_jmaxb0 = VU.empty
  , lp_jmaxb1 = VU.empty
  , lp_wc2wjb0 = VU.empty
  }

-- ---------------------------------------------------------------------------
-- PFT constants (from clm5_params.nc)
-- ---------------------------------------------------------------------------

-- | PFT-indexed plant functional type constants.
data PFTConstants = PFTConstants
  { pft_z0mr       :: !(VU.Vector Double)  -- ^ Roughness length ratio
  , pft_displar    :: !(VU.Vector Double)  -- ^ Displacement height ratio
  , pft_dleaf      :: !(VU.Vector Double)  -- ^ Leaf dimension [m]
  , pft_c3psn      :: !(VU.Vector Double)  -- ^ C3 pathway flag
  , pft_vcmx25     :: !(VU.Vector Double)  -- ^ Vcmax25 [umol/m2/s]
  , pft_mp          :: !(VU.Vector Double)  -- ^ Ball-Berry slope
  , pft_flnr       :: !(VU.Vector Double)  -- ^ Fraction leaf N in Rubisco
  , pft_slatop     :: !(VU.Vector Double)  -- ^ SLA at top of canopy [m2/gC]
  , pft_woody      :: !(VU.Vector Double)  -- ^ Woody flag
  , pft_leaf_long  :: !(VU.Vector Double)  -- ^ Leaf longevity [years]
  , pft_evergreen  :: !(VU.Vector Double)  -- ^ Evergreen flag
  , pft_roota_par  :: !(VU.Vector Double)  -- ^ Root a parameter
  , pft_rootb_par  :: !(VU.Vector Double)  -- ^ Root b parameter
  , pft_smpso      :: !(VU.Vector Double)  -- ^ Soil matric pot at stomatal opening [mm]
  , pft_smpsc      :: !(VU.Vector Double)  -- ^ Soil matric pot at stomatal closure [mm]
  , pft_xl         :: !(VU.Vector Double)  -- ^ Leaf/stem orientation index
  , pft_rhol       :: !(VU.Vector Double)  -- ^ Leaf reflectance
  , pft_rhos       :: !(VU.Vector Double)  -- ^ Stem reflectance
  , pft_taul       :: !(VU.Vector Double)  -- ^ Leaf transmittance
  , pft_taus       :: !(VU.Vector Double)  -- ^ Stem transmittance
  } deriving (Show)

defaultPFTConstants :: PFTConstants
defaultPFTConstants = PFTConstants
  { pft_z0mr      = VU.empty
  , pft_displar   = VU.empty
  , pft_dleaf     = VU.empty
  , pft_c3psn     = VU.empty
  , pft_vcmx25    = VU.empty
  , pft_mp        = VU.empty
  , pft_flnr      = VU.empty
  , pft_slatop    = VU.empty
  , pft_woody     = VU.empty
  , pft_leaf_long = VU.empty
  , pft_evergreen = VU.empty
  , pft_roota_par = VU.empty
  , pft_rootb_par = VU.empty
  , pft_smpso     = VU.empty
  , pft_smpsc     = VU.empty
  , pft_xl        = VU.empty
  , pft_rhol      = VU.empty
  , pft_rhos      = VU.empty
  , pft_taul      = VU.empty
  , pft_taus      = VU.empty
  }

-- ---------------------------------------------------------------------------
-- Snow layer constants
-- ---------------------------------------------------------------------------

-- | Snow layer dimension constants from CLM initialization.
data SnowLayerConstants = SnowLayerConstants
  { slc_dzmin  :: !(VU.Vector Double)  -- ^ Minimum layer thickness [m]
  , slc_dzmaxU :: !(VU.Vector Double)  -- ^ Upper bound max thickness [m]
  , slc_dzmaxL :: !(VU.Vector Double)  -- ^ Lower bound max thickness [m]
  } deriving (Show)

defaultSnowLayerConstants :: SnowLayerConstants
defaultSnowLayerConstants = SnowLayerConstants VU.empty VU.empty VU.empty

-- ---------------------------------------------------------------------------
-- All params container
-- ---------------------------------------------------------------------------

-- | Container aggregating all parameter structures.
data AllParams = AllParams
  { ap_initVertical     :: !InitVerticalParams
  , ap_phot             :: !PhotParams
  , ap_canopyHydrology  :: !CanopyHydrologyParams
  , ap_snowHydrology    :: !SnowHydrologyParams
  , ap_soilHydrology    :: !SoilHydrologyParams
  , ap_snicar           :: !SnicarParams
  , ap_canopyFluxes     :: !CanopyFluxesParams
  , ap_thermal          :: !ThermalParams
  , ap_scf              :: !SCFParams
  , ap_ch4              :: !CH4Params
  , ap_luna             :: !LunaParams
  , ap_pftcon           :: !PFTConstants
  , ap_snowLayers       :: !SnowLayerConstants
  } deriving (Show)

defaultAllParams :: AllParams
defaultAllParams = AllParams
  { ap_initVertical    = defaultInitVerticalParams
  , ap_phot            = defaultPhotParams
  , ap_canopyHydrology = defaultCanopyHydrologyParams
  , ap_snowHydrology   = defaultSnowHydrologyParams
  , ap_soilHydrology   = defaultSoilHydrologyParams
  , ap_snicar          = defaultSnicarParams
  , ap_canopyFluxes    = defaultCanopyFluxesParams
  , ap_thermal         = defaultThermalParams
  , ap_scf             = defaultSCFParams
  , ap_ch4             = defaultCH4Params
  , ap_luna            = defaultLunaParams
  , ap_pftcon          = defaultPFTConstants
  , ap_snowLayers      = defaultSnowLayerConstants
  }

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- | Read all parameters for the compatibility parameter API.
readParameters :: FilePath -> IO AllParams
readParameters paramfile
  | null paramfile = return defaultAllParams
  | otherwise = do
      isDir <- doesDirectoryExist paramfile
      if isDir
        then readParametersBinary paramfile
        else return defaultAllParams

-- | Read parameters from binary files exported by Julia.
-- @dir@ is the path to the params/ subdirectory.
readParametersBinary :: FilePath -> IO AllParams
readParametersBinary dir = do
  -- Photosynthesis scalars
  phot <- readPhotParamsBinary dir

  -- Canopy fluxes scalars
  canflux <- readCanopyFluxesParamsBinary dir

  -- PFT constants
  pftcon <- readPFTConstantsBinary dir

  -- Snow layer constants
  snowLayers <- readSnowLayerConstantsBinary dir

  return $! defaultAllParams
    { ap_phot        = phot
    , ap_canopyFluxes = canflux
    , ap_pftcon      = pftcon
    , ap_snowLayers  = snowLayers
    }

-- | Read photosynthesis scalar parameters from binary files.
readPhotParamsBinary :: FilePath -> IO PhotParams
readPhotParamsBinary dir = do
  let rd name = readFloat64Scalar (dir </> ("phot_" ++ name ++ ".bin"))
  theta_ip     <- rd "theta_ip"
  act25        <- rd "act25"
  fnr          <- rd "fnr"
  cp25_yr2000  <- rd "cp25_yr2000"
  kc25_coef    <- rd "kc25_coef"
  ko25_coef    <- rd "ko25_coef"
  fnps         <- rd "fnps"
  theta_psii   <- rd "theta_psii"
  vcmaxha      <- rd "vcmaxha"
  jmaxha       <- rd "jmaxha"
  tpuha        <- rd "tpuha"
  lmrha        <- rd "lmrha"
  kcha         <- rd "kcha"
  koha         <- rd "koha"
  cpha         <- rd "cpha"
  vcmaxhd      <- rd "vcmaxhd"
  jmaxhd       <- rd "jmaxhd"
  tpuhd        <- rd "tpuhd"
  lmrhd        <- rd "lmrhd"
  lmrse        <- rd "lmrse"
  tpu25ratio   <- rd "tpu25ratio"
  kp25ratio    <- rd "kp25ratio"
  vcmaxse_sf   <- rd "vcmaxse_sf"
  jmaxse_sf    <- rd "jmaxse_sf"
  tpuse_sf     <- rd "tpuse_sf"
  jmax25top_sf <- rd "jmax25top_sf"
  -- PFT-dimensioned
  theta_cj <- readPftVecOpt dir "pftcon_theta_cj"
  krmax    <- readPftVecOpt dir "pftcon_krmax"
  lmr_intercept <- readPftVecOpt dir "pftcon_lmr_intercept_atkin"
  return $! PhotParams
    { pp_theta_ip     = theta_ip
    , pp_act25        = act25
    , pp_fnr          = fnr
    , pp_cp25_yr2000  = cp25_yr2000
    , pp_kc25_coef    = kc25_coef
    , pp_ko25_coef    = ko25_coef
    , pp_fnps         = fnps
    , pp_theta_psii   = theta_psii
    , pp_vcmaxha      = vcmaxha
    , pp_jmaxha       = jmaxha
    , pp_tpuha        = tpuha
    , pp_lmrha        = lmrha
    , pp_kcha         = kcha
    , pp_koha         = koha
    , pp_cpha         = cpha
    , pp_vcmaxhd      = vcmaxhd
    , pp_jmaxhd       = jmaxhd
    , pp_tpuhd        = tpuhd
    , pp_lmrhd        = lmrhd
    , pp_lmrse        = lmrse
    , pp_tpu25ratio   = tpu25ratio
    , pp_kp25ratio    = kp25ratio
    , pp_vcmaxse_sf   = vcmaxse_sf
    , pp_jmaxse_sf    = jmaxse_sf
    , pp_tpuse_sf     = tpuse_sf
    , pp_jmax25top_sf = jmax25top_sf
    , pp_theta_cj     = theta_cj
    , pp_krmax        = krmax
    , pp_lmr_intercept_atkin = lmr_intercept
    }

-- | Read canopy flux parameters from binary files.
readCanopyFluxesParamsBinary :: FilePath -> IO CanopyFluxesParams
readCanopyFluxesParamsBinary dir = do
  let rd name = readFloat64Scalar (dir </> ("canflux_" ++ name ++ ".bin"))
  csoilc <- rd "csoilc"
  cv     <- rd "cv"
  return $! defaultCanopyFluxesParams
    { cfp_csoilc = csoilc
    , cfp_cv     = cv
    }

-- | Read PFT constants from binary files.
-- | Read the CNDV (dynamic global vegetation) per-PFT ecophysiological
-- constants from the binary param files (pftpar20/28/29/30/31, extracted from
-- clm5_params.nc). Returns 'defaultDGVEcophysCon' (empty vectors) when the
-- files are absent, so callers fall back to the built-in LPJ table.
--
-- pftpar20 -> crownarea_max, pftpar28 -> tcmin, pftpar29 -> tcmax,
-- pftpar30 -> gddmin, pftpar31 -> twmax. Values are indexed by PFT type
-- (index 0 = bare ground). Fill values (9999.9 for tcmin, 1000 for tcmax/twmax)
-- are loaded as-is; the CNDV survival/establishment logic treats them correctly
-- (twmax >= 999 = no warmth limit; an out-of-range tcmin excludes the PFT).
readDGVEcophysCon :: FilePath -> IO DGVEcophysCon
readDGVEcophysCon dir = do
  crownarea_max <- readPftVecOpt dir "pftcon_pftpar20"
  tcmin         <- readPftVecOpt dir "pftcon_pftpar28"
  tcmax         <- readPftVecOpt dir "pftcon_pftpar29"
  gddmin        <- readPftVecOpt dir "pftcon_pftpar30"
  twmax         <- readPftVecOpt dir "pftcon_pftpar31"
  return $! defaultDGVEcophysCon
    { dgveco_crownarea_max = crownarea_max
    , dgveco_tcmin         = tcmin
    , dgveco_tcmax         = tcmax
    , dgveco_gddmin        = gddmin
    , dgveco_twmax         = twmax
    }

-- | Read per-PFT isoprene emission factors (ug/m2/hr) from megan_isoprene_ef.bin
-- (extracted from the MEGAN2.1 emission-factor file, CLM-indexed with bare = 0).
-- Empty when absent ⇒ the VOC step falls back to a representative constant.
readMeganIsopreneEF :: FilePath -> IO (VU.Vector Double)
readMeganIsopreneEF dir = readPftVecOpt dir "megan_isoprene_ef"

-- | Read per-PFT deadstem-to-10yr-product fraction (pprod10, from clm5_params).
readPprod10 :: FilePath -> IO (VU.Vector Double)
readPprod10 dir = readPftVecOpt dir "pftcon_pprod10"

-- | Read per-PFT deadstem-to-100yr-product fraction (pprod100, from clm5_params).
readPprod100 :: FilePath -> IO (VU.Vector Double)
readPprod100 dir = readPftVecOpt dir "pftcon_pprod100"

readPFTConstantsBinary :: FilePath -> IO PFTConstants
readPFTConstantsBinary dir = do
  let rdv name = readPftVecOpt dir ("pftcon_" ++ name)
  z0mr      <- rdv "z0mr"
  displar   <- rdv "displar"
  dleaf     <- rdv "dleaf"
  c3psn     <- rdv "c3psn"
  vcmx25    <- rdv "vcmx25"
  mp        <- rdv "mp"
  flnr      <- rdv "flnr"
  slatop    <- rdv "slatop"
  woody     <- rdv "woody"
  leaf_long <- rdv "leaf_long"
  evergreen <- rdv "evergreen"
  roota_par <- rdv "roota_par"
  rootb_par <- rdv "rootb_par"
  smpso     <- rdv "smpso"
  smpsc     <- rdv "smpsc"
  xl        <- rdv "xl"
  rhol      <- rdv "rhol"
  rhos      <- rdv "rhos"
  taul      <- rdv "taul"
  taus      <- rdv "taus"
  return $! PFTConstants
    { pft_z0mr      = z0mr
    , pft_displar   = displar
    , pft_dleaf     = dleaf
    , pft_c3psn     = c3psn
    , pft_vcmx25    = vcmx25
    , pft_mp        = mp
    , pft_flnr      = flnr
    , pft_slatop    = slatop
    , pft_woody     = woody
    , pft_leaf_long = leaf_long
    , pft_evergreen = evergreen
    , pft_roota_par = roota_par
    , pft_rootb_par = rootb_par
    , pft_smpso     = smpso
    , pft_smpsc     = smpsc
    , pft_xl        = xl
    , pft_rhol      = rhol
    , pft_rhos      = rhos
    , pft_taul      = taul
    , pft_taus      = taus
    }

-- | Read snow layer constants from binary files.
readSnowLayerConstantsBinary :: FilePath -> IO SnowLayerConstants
readSnowLayerConstantsBinary dir = do
  dzmin  <- readPftVecOpt dir "snow_dzmin"
  dzmaxU <- readPftVecOpt dir "snow_dzmax_u"
  dzmaxL <- readPftVecOpt dir "snow_dzmax_l"
  return $! SnowLayerConstants dzmin dzmaxU dzmaxL

-- | Read a PFT vector if the file exists, otherwise return empty.
readPftVecOpt :: FilePath -> String -> IO (VU.Vector Double)
readPftVecOpt dir name = do
  let fp = dir </> (name ++ ".bin")
  exists <- doesFileExist fp
  if exists
    then readFloat64Vector fp
    else return VU.empty
