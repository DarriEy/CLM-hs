{-# LANGUAGE BangPatterns #-}
-- | FATES Interface -- CLM-FATES boundary condition types and integration bypasses.
--
-- FATES (Functionally Assembled Terrestrial Ecosystem Simulator) is a
-- cohort-based vegetation demography model that replaces CLM's big-leaf
-- approach with explicit tracking of plant cohorts within patches of
-- different disturbance ages.
--
-- This module defines all CLM<->FATES data exchange types and provides
-- documented bypass functions at every integration point.
--
-- Ported from:
--   clmfates_interfaceMod.F90      (~3,896 lines)
--   FatesInterfaceTypesMod.F90     (~821 lines)
--
-- Julia: src/biogeochem/fates_interface.jl
--
module CLM.BioGeoChem.FATESInterface
  ( -- * Configuration
    FATESConfig(..)
  , defaultFATESConfig
    -- * Boundary condition types
  , FATESBoundaryCondIn(..)
  , FATESBoundaryCondOut(..)
  , FATESParameterConstants(..)
    -- * State types
  , FATESSiteMap(..)
  , FATESState(..)
  , HLMFATESInterface(..)
    -- * Initialization
  , defaultFATESBoundaryCondIn
  , defaultFATESBoundaryCondOut
  , defaultFATESParameterConstants
  , defaultFATESSiteMap
  , defaultHLMFATESInterface
    -- * Bypass function signatures (documented bypass functions)
  , fatesInit
  , fatesToClmSunfrac
  , fatesToClmBtran
  , fatesToClmPhotosynthesis
  , fatesToClmRadiation
  , fatesToClmCanopyStructure
  , fatesToClmZ0mDispla
  , fatesToClmLitterC
  , fatesToClmLitterN
  , fatesToClmHydraulics
  , fatesToClmWoodProducts
  , fatesDynamics
  , fatesUpdateRunningMeans
  , fatesUpdateHifreqHist
  , fatesWrapSunfrac
  , fatesWrapBtran
  , fatesPrepCanopyfluxes
  , fatesWrapPhotosynthesis
  , fatesWrapAccumulatefluxes
  , fatesWrapCanopyRadiation
  , fatesWrapHydraulicsDrive
  , fatesWrapSeedDispersal
  , fatesUpdateAccVars
  , fatesColdstart
  , fatesRestart
  , fatesTransferZ0mDispla
  , fatesUpdateLitterFluxes
  , fatesWrapWoodProducts
  , fatesComputeRootSoilFlux
  , fatesSpPhenology
  ) where

import qualified Data.Vector.Unboxed as U

-- ========================================================================
-- FATES Global Configuration
-- ========================================================================

-- | Global configuration parameters for FATES, set once during initialization.
-- Corresponds to the hlm_* global variables in FatesInterfaceTypesMod.F90.
data FATESConfig = FATESConfig
  { fc_hlm_numSWb                   :: !Int
  , fc_hlm_ivis                     :: !Int
  , fc_hlm_inir                     :: !Int
  , fc_hlm_maxlevsoil               :: !Int
  , fc_hlm_stepsize                  :: !Double
  , fc_hlm_use_planthydro           :: !Bool
  , fc_hlm_use_ed_st3               :: !Bool
  , fc_hlm_use_ed_prescribed_phys   :: !Bool
  , fc_hlm_use_inventory_init       :: !Bool
  , fc_hlm_use_fixed_biogeog        :: !Bool
  , fc_hlm_use_nocomp               :: !Bool
  , fc_hlm_use_sp                   :: !Bool
  , fc_hlm_use_logging              :: !Bool
  , fc_hlm_use_lu_harvest           :: !Bool
  , fc_hlm_use_luh                  :: !Bool
  , fc_hlm_use_potentialveg         :: !Bool
  , fc_hlm_use_tree_damage          :: !Bool
  , fc_hlm_use_cohort_age_tracking  :: !Bool
  , fc_hlm_use_ch4                  :: !Bool
  , fc_hlm_use_vertsoilc            :: !Bool
  , fc_hlm_spitfire_mode            :: !Int
  , fc_hlm_parteh_mode              :: !Int
  , fc_hlm_seeddisp_cadence         :: !Int
  , fc_hlm_nitrogen_spec            :: !Int
  , fc_hlm_phosphorus_spec          :: !Int
  , fc_hlm_hist_level_dynam         :: !Int
  , fc_hlm_hist_level_hifrq         :: !Int
  , fc_numpft                       :: !Int
  , fc_nlevsclass                   :: !Int
  , fc_nlevage                      :: !Int
  , fc_nlevheight                   :: !Int
  , fc_nlevcoage                    :: !Int
  , fc_nleafage                     :: !Int
  , fc_nlevdamage                   :: !Int
  , fc_fates_maxPatchesPerSite      :: !Int
  , fc_fates_maxElementsPerPatch    :: !Int
  , fc_fates_maxElementsPerSite     :: !Int
  } deriving (Show, Eq)

defaultFATESConfig :: FATESConfig
defaultFATESConfig = FATESConfig
  { fc_hlm_numSWb                   = 2
  , fc_hlm_ivis                     = 1
  , fc_hlm_inir                     = 2
  , fc_hlm_maxlevsoil               = 20
  , fc_hlm_stepsize                  = 1800.0
  , fc_hlm_use_planthydro           = False
  , fc_hlm_use_ed_st3               = False
  , fc_hlm_use_ed_prescribed_phys   = False
  , fc_hlm_use_inventory_init       = False
  , fc_hlm_use_fixed_biogeog        = False
  , fc_hlm_use_nocomp               = False
  , fc_hlm_use_sp                   = False
  , fc_hlm_use_logging              = False
  , fc_hlm_use_lu_harvest           = False
  , fc_hlm_use_luh                  = False
  , fc_hlm_use_potentialveg         = False
  , fc_hlm_use_tree_damage          = False
  , fc_hlm_use_cohort_age_tracking  = False
  , fc_hlm_use_ch4                  = False
  , fc_hlm_use_vertsoilc            = True
  , fc_hlm_spitfire_mode            = 0
  , fc_hlm_parteh_mode              = 1
  , fc_hlm_seeddisp_cadence         = 0
  , fc_hlm_nitrogen_spec            = 0
  , fc_hlm_phosphorus_spec          = 0
  , fc_hlm_hist_level_dynam         = 1
  , fc_hlm_hist_level_hifrq         = 1
  , fc_numpft                       = 0
  , fc_nlevsclass                   = 0
  , fc_nlevage                      = 0
  , fc_nlevheight                   = 0
  , fc_nlevcoage                    = 0
  , fc_nleafage                     = 0
  , fc_nlevdamage                   = 0
  , fc_fates_maxPatchesPerSite      = 0
  , fc_fates_maxElementsPerPatch    = 0
  , fc_fates_maxElementsPerSite     = 0
  }

-- ========================================================================
-- Boundary Condition Types
-- ========================================================================

-- | Input boundary conditions passed from CLM to FATES for a single site.
-- Corresponds to bc_in_type in FatesInterfaceTypesMod.F90.
data FATESBoundaryCondIn = FATESBoundaryCondIn
  { bcin_npatches              :: !Int
  , bcin_nlevsoil              :: !Int
  , bcin_nlevdecomp            :: !Int
  , bcin_zi_sisl               :: !(U.Vector Double)
  , bcin_dz_sisl               :: !(U.Vector Double)
  , bcin_z_sisl                :: !(U.Vector Double)
  , bcin_dz_decomp_sisl        :: !(U.Vector Double)
  , bcin_decomp_id             :: !(U.Vector Int)
  , bcin_w_scalar_sisl         :: !(U.Vector Double)
  , bcin_t_scalar_sisl         :: !(U.Vector Double)
  , bcin_lightning24           :: !(U.Vector Double)
  , bcin_pop_density           :: !(U.Vector Double)
  , bcin_precip24_pa           :: !(U.Vector Double)
  , bcin_relhumid24_pa         :: !(U.Vector Double)
  , bcin_wind24_pa             :: !(U.Vector Double)
  , bcin_coszen_pa             :: !(U.Vector Double)
  , bcin_fcansno_pa            :: !(U.Vector Double)
  , bcin_albgr_dir_rb          :: !(U.Vector Double)
  , bcin_albgr_dif_rb          :: !(U.Vector Double)
  , bcin_forc_pbot             :: !Double
  , bcin_dayl_factor_pa        :: !(U.Vector Double)
  , bcin_esat_tv_pa            :: !(U.Vector Double)
  , bcin_eair_pa               :: !(U.Vector Double)
  , bcin_oair_pa               :: !(U.Vector Double)
  , bcin_cair_pa               :: !(U.Vector Double)
  , bcin_rb_pa                 :: !(U.Vector Double)
  , bcin_t_veg_pa              :: !(U.Vector Double)
  , bcin_tgcm_pa               :: !(U.Vector Double)
  , bcin_t_soisno_sl           :: !(U.Vector Double)
  , bcin_smp_sl                :: !(U.Vector Double)
  , bcin_eff_porosity_sl       :: !(U.Vector Double)
  , bcin_watsat_sl             :: !(U.Vector Double)
  , bcin_tempk_sl              :: !(U.Vector Double)
  , bcin_h2o_liqvol_sl         :: !(U.Vector Double)
  , bcin_filter_btran          :: !Bool
  , bcin_qflx_transp_pa       :: !(U.Vector Double)
  , bcin_snow_depth_si         :: !Double
  , bcin_frac_sno_eff_si       :: !Double
  , bcin_max_rooting_depth_idx :: !Int
  , bcin_tot_het_resp          :: !Double
  , bcin_tot_somc              :: !Double
  , bcin_tot_litc              :: !Double
  , bcin_site_area             :: !Double
  , bcin_baregroundfrac        :: !Double
  , bcin_smpmin_si             :: !Double
  } deriving (Show, Eq)

defaultFATESBoundaryCondIn :: FATESBoundaryCondIn
defaultFATESBoundaryCondIn = FATESBoundaryCondIn
  { bcin_npatches              = 0
  , bcin_nlevsoil              = 0
  , bcin_nlevdecomp            = 0
  , bcin_zi_sisl               = U.empty
  , bcin_dz_sisl               = U.empty
  , bcin_z_sisl                = U.empty
  , bcin_dz_decomp_sisl        = U.empty
  , bcin_decomp_id             = U.empty
  , bcin_w_scalar_sisl         = U.empty
  , bcin_t_scalar_sisl         = U.empty
  , bcin_lightning24           = U.empty
  , bcin_pop_density           = U.empty
  , bcin_precip24_pa           = U.empty
  , bcin_relhumid24_pa         = U.empty
  , bcin_wind24_pa             = U.empty
  , bcin_coszen_pa             = U.empty
  , bcin_fcansno_pa            = U.empty
  , bcin_albgr_dir_rb          = U.empty
  , bcin_albgr_dif_rb          = U.empty
  , bcin_forc_pbot             = 0.0
  , bcin_dayl_factor_pa        = U.empty
  , bcin_esat_tv_pa            = U.empty
  , bcin_eair_pa               = U.empty
  , bcin_oair_pa               = U.empty
  , bcin_cair_pa               = U.empty
  , bcin_rb_pa                 = U.empty
  , bcin_t_veg_pa              = U.empty
  , bcin_tgcm_pa               = U.empty
  , bcin_t_soisno_sl           = U.empty
  , bcin_smp_sl                = U.empty
  , bcin_eff_porosity_sl       = U.empty
  , bcin_watsat_sl             = U.empty
  , bcin_tempk_sl              = U.empty
  , bcin_h2o_liqvol_sl         = U.empty
  , bcin_filter_btran          = False
  , bcin_qflx_transp_pa       = U.empty
  , bcin_snow_depth_si         = 0.0
  , bcin_frac_sno_eff_si       = 0.0
  , bcin_max_rooting_depth_idx = 0
  , bcin_tot_het_resp          = 0.0
  , bcin_tot_somc              = 0.0
  , bcin_tot_litc              = 0.0
  , bcin_site_area             = 0.0
  , bcin_baregroundfrac        = 0.0
  , bcin_smpmin_si             = 0.0
  }

-- | Output boundary conditions returned from FATES to CLM for a single site.
-- Corresponds to bc_out_type in FatesInterfaceTypesMod.F90.
data FATESBoundaryCondOut = FATESBoundaryCondOut
  { bcout_fsun_pa                :: !(U.Vector Double)
  , bcout_laisun_pa              :: !(U.Vector Double)
  , bcout_laisha_pa              :: !(U.Vector Double)
  , bcout_btran_pa               :: !(U.Vector Double)
  , bcout_rssun_pa               :: !(U.Vector Double)
  , bcout_rssha_pa               :: !(U.Vector Double)
  , bcout_elai_pa                :: !(U.Vector Double)
  , bcout_esai_pa                :: !(U.Vector Double)
  , bcout_tlai_pa                :: !(U.Vector Double)
  , bcout_tsai_pa                :: !(U.Vector Double)
  , bcout_htop_pa                :: !(U.Vector Double)
  , bcout_hbot_pa                :: !(U.Vector Double)
  , bcout_z0m_pa                 :: !(U.Vector Double)
  , bcout_displa_pa              :: !(U.Vector Double)
  , bcout_dleaf_pa               :: !(U.Vector Double)
  , bcout_canopy_fraction_pa     :: !(U.Vector Double)
  , bcout_frac_veg_nosno_alb_pa  :: !(U.Vector Double)
  , bcout_litt_flux_cel_c_si     :: !(U.Vector Double)
  , bcout_litt_flux_lig_c_si     :: !(U.Vector Double)
  , bcout_litt_flux_lab_c_si     :: !(U.Vector Double)
  , bcout_litt_flux_cel_n_si     :: !(U.Vector Double)
  , bcout_litt_flux_lig_n_si     :: !(U.Vector Double)
  , bcout_litt_flux_lab_n_si     :: !(U.Vector Double)
  , bcout_litt_flux_cel_p_si     :: !(U.Vector Double)
  , bcout_litt_flux_lig_p_si     :: !(U.Vector Double)
  , bcout_litt_flux_lab_p_si     :: !(U.Vector Double)
  , bcout_litt_flux_ligc_per_n   :: !Double
  , bcout_num_plant_comps        :: !Int
  , bcout_annavg_agnpp_pa        :: !(U.Vector Double)
  , bcout_annavg_bgnpp_pa        :: !(U.Vector Double)
  , bcout_annsum_npp_pa          :: !(U.Vector Double)
  , bcout_frootc_pa              :: !(U.Vector Double)
  , bcout_root_resp              :: !(U.Vector Double)
  , bcout_ema_npp                :: !Double
  , bcout_plant_stored_h2o_si    :: !Double
  , bcout_qflx_soil2root_sisl   :: !(U.Vector Double)
  , bcout_qflx_ro_sisl           :: !(U.Vector Double)
  , bcout_hrv_deadstemc_to_prod10c  :: !Double
  , bcout_hrv_deadstemc_to_prod100c :: !Double
  , bcout_gpp_site               :: !Double
  , bcout_ar_site                :: !Double
  } deriving (Show, Eq)

defaultFATESBoundaryCondOut :: FATESBoundaryCondOut
defaultFATESBoundaryCondOut = FATESBoundaryCondOut
  { bcout_fsun_pa                = U.empty
  , bcout_laisun_pa              = U.empty
  , bcout_laisha_pa              = U.empty
  , bcout_btran_pa               = U.empty
  , bcout_rssun_pa               = U.empty
  , bcout_rssha_pa               = U.empty
  , bcout_elai_pa                = U.empty
  , bcout_esai_pa                = U.empty
  , bcout_tlai_pa                = U.empty
  , bcout_tsai_pa                = U.empty
  , bcout_htop_pa                = U.empty
  , bcout_hbot_pa                = U.empty
  , bcout_z0m_pa                 = U.empty
  , bcout_displa_pa              = U.empty
  , bcout_dleaf_pa               = U.empty
  , bcout_canopy_fraction_pa     = U.empty
  , bcout_frac_veg_nosno_alb_pa  = U.empty
  , bcout_litt_flux_cel_c_si     = U.empty
  , bcout_litt_flux_lig_c_si     = U.empty
  , bcout_litt_flux_lab_c_si     = U.empty
  , bcout_litt_flux_cel_n_si     = U.empty
  , bcout_litt_flux_lig_n_si     = U.empty
  , bcout_litt_flux_lab_n_si     = U.empty
  , bcout_litt_flux_cel_p_si     = U.empty
  , bcout_litt_flux_lig_p_si     = U.empty
  , bcout_litt_flux_lab_p_si     = U.empty
  , bcout_litt_flux_ligc_per_n   = 0.0
  , bcout_num_plant_comps        = 0
  , bcout_annavg_agnpp_pa        = U.empty
  , bcout_annavg_bgnpp_pa        = U.empty
  , bcout_annsum_npp_pa          = U.empty
  , bcout_frootc_pa              = U.empty
  , bcout_root_resp              = U.empty
  , bcout_ema_npp                = 0.0
  , bcout_plant_stored_h2o_si    = 0.0
  , bcout_qflx_soil2root_sisl   = U.empty
  , bcout_qflx_ro_sisl           = U.empty
  , bcout_hrv_deadstemc_to_prod10c  = 0.0
  , bcout_hrv_deadstemc_to_prod100c = 0.0
  , bcout_gpp_site               = 0.0
  , bcout_ar_site                = 0.0
  }

-- | Constant parameters for nutrient competition.
-- Corresponds to bc_pconst_type in FatesInterfaceTypesMod.F90.
data FATESParameterConstants = FATESParameterConstants
  { fpc_max_plant_comps  :: !Int
  , fpc_vmax_nh4         :: !(U.Vector Double)
  , fpc_vmax_no3         :: !(U.Vector Double)
  , fpc_vmax_p           :: !(U.Vector Double)
  , fpc_eca_km_nh4       :: !(U.Vector Double)
  , fpc_eca_km_no3       :: !(U.Vector Double)
  , fpc_eca_km_p         :: !(U.Vector Double)
  , fpc_eca_plant_escalar :: !Double
  , fpc_j_uptake         :: !(U.Vector Int)
  } deriving (Show, Eq)

defaultFATESParameterConstants :: FATESParameterConstants
defaultFATESParameterConstants = FATESParameterConstants
  { fpc_max_plant_comps  = 0
  , fpc_vmax_nh4         = U.empty
  , fpc_vmax_no3         = U.empty
  , fpc_vmax_p           = U.empty
  , fpc_eca_km_nh4       = U.empty
  , fpc_eca_km_no3       = U.empty
  , fpc_eca_km_p         = U.empty
  , fpc_eca_plant_escalar = 0.0
  , fpc_j_uptake         = U.empty
  }

-- ========================================================================
-- State types
-- ========================================================================

-- | Mapping between FATES sites and CLM columns/gridcells.
data FATESSiteMap = FATESSiteMap
  { fsm_fcolumn :: !(U.Vector Int)
  , fsm_hsites  :: !(U.Vector Int)
  } deriving (Show, Eq)

defaultFATESSiteMap :: FATESSiteMap
defaultFATESSiteMap = FATESSiteMap
  { fsm_fcolumn = U.empty
  , fsm_hsites  = U.empty
  }

-- | Represented container for FATES ecosystem state at a single site.
data FATESState = FATESState
  { fs_nsites   :: !Int
  , fs_bc_in    :: ![FATESBoundaryCondIn]
  , fs_bc_out   :: ![FATESBoundaryCondOut]
  , fs_bc_pconst :: ![FATESParameterConstants]
  , fs_sitemap  :: !FATESSiteMap
  } deriving (Show, Eq)

-- | Top-level FATES interface object.
data HLMFATESInterface = HLMFATESInterface
  { hlmfi_config      :: !FATESConfig
  , hlmfi_state       :: !FATESState
  , hlmfi_initialized :: !Bool
  } deriving (Show, Eq)

defaultHLMFATESInterface :: HLMFATESInterface
defaultHLMFATESInterface = HLMFATESInterface
  { hlmfi_config      = defaultFATESConfig
  , hlmfi_state       = FATESState 0 [] [] [] defaultFATESSiteMap
  , hlmfi_initialized = False
  }

-- ========================================================================
-- Initialization
-- ========================================================================

-- | Initialize the FATES interface for a given number of sites.
-- Returns an initialized HLMFATESInterface.
fatesInit
  :: FATESConfig
  -> Int              -- nsites
  -> Int              -- nlevsoil
  -> Int              -- npatches_max
  -> Int              -- nbands
  -> HLMFATESInterface
fatesInit cfg nsites _nlevsoil _npatchesMax _nbands =
  let state = FATESState
        { fs_nsites   = nsites
        , fs_bc_in    = replicate nsites defaultFATESBoundaryCondIn
        , fs_bc_out   = replicate nsites defaultFATESBoundaryCondOut
        , fs_bc_pconst = replicate nsites defaultFATESParameterConstants
        , fs_sitemap  = defaultFATESSiteMap
        }
  in HLMFATESInterface
       { hlmfi_config      = cfg
       , hlmfi_state       = state
       , hlmfi_initialized = True
       }

-- ========================================================================
-- FATES -> CLM data transfer bypasses (all return their input unchanged)
-- ========================================================================

-- | Transfer sun/shade fractions from FATES back to CLM. Bypass.
fatesToClmSunfrac :: HLMFATESInterface -> HLMFATESInterface
fatesToClmSunfrac = id

-- | Transfer BTRAN and root distribution from FATES back to CLM. Bypass.
fatesToClmBtran :: HLMFATESInterface -> HLMFATESInterface
fatesToClmBtran = id

-- | Transfer stomatal resistance from FATES back to CLM. Bypass.
fatesToClmPhotosynthesis :: HLMFATESInterface -> HLMFATESInterface
fatesToClmPhotosynthesis = id

-- | Transfer canopy radiation from FATES back to CLM. Bypass.
fatesToClmRadiation :: HLMFATESInterface -> HLMFATESInterface
fatesToClmRadiation = id

-- | Transfer canopy structure from FATES to CLM patch arrays. Bypass.
fatesToClmCanopyStructure :: HLMFATESInterface -> HLMFATESInterface
fatesToClmCanopyStructure = id

-- | Transfer roughness length and displacement height. Bypass.
fatesToClmZ0mDispla :: HLMFATESInterface -> HLMFATESInterface
fatesToClmZ0mDispla = id

-- | Transfer carbon litter fluxes from FATES to CLM soil BGC. Bypass.
fatesToClmLitterC :: HLMFATESInterface -> HLMFATESInterface
fatesToClmLitterC = id

-- | Transfer nitrogen litter fluxes from FATES to CLM soil BGC. Bypass.
fatesToClmLitterN :: HLMFATESInterface -> HLMFATESInterface
fatesToClmLitterN = id

-- | Transfer plant hydraulics outputs from FATES to CLM. Bypass.
fatesToClmHydraulics :: HLMFATESInterface -> HLMFATESInterface
fatesToClmHydraulics = id

-- | Transfer harvested wood product fluxes from FATES to CLM. Bypass.
fatesToClmWoodProducts :: HLMFATESInterface -> HLMFATESInterface
fatesToClmWoodProducts = id

-- ========================================================================
-- Core FATES dynamics bypasses
-- ========================================================================

-- | Main FATES daily dynamics driver. Bypass.
fatesDynamics :: HLMFATESInterface -> Double -> Bool -> HLMFATESInterface
fatesDynamics iface _dtime isBegDay
  | not isBegDay = iface
  | otherwise    = iface  -- When implemented: run ed_ecosystem_dynamics

-- | Update FATES running mean temperature. Bypass.
fatesUpdateRunningMeans :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesUpdateRunningMeans iface _dtime = iface

-- | Update FATES high-frequency history diagnostics. Bypass.
fatesUpdateHifreqHist :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesUpdateHifreqHist iface _dtime = iface

-- | Compute sun/shade fractions using FATES two-stream model. Bypass.
fatesWrapSunfrac :: HLMFATESInterface -> HLMFATESInterface
fatesWrapSunfrac = id

-- | Compute BTRAN using FATES root distribution. Bypass.
fatesWrapBtran :: HLMFATESInterface -> HLMFATESInterface
fatesWrapBtran = id

-- | Prepare FATES patches for photosynthesis iteration. Bypass.
fatesPrepCanopyfluxes :: HLMFATESInterface -> HLMFATESInterface
fatesPrepCanopyfluxes = id

-- | Compute photosynthesis for FATES cohorts. Bypass.
fatesWrapPhotosynthesis :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapPhotosynthesis iface _dtime = iface

-- | Accumulate FATES carbon/water fluxes. Bypass.
fatesWrapAccumulatefluxes :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapAccumulatefluxes iface _dtime = iface

-- | Compute canopy radiation using FATES two-stream model. Bypass.
fatesWrapCanopyRadiation :: HLMFATESInterface -> HLMFATESInterface
fatesWrapCanopyRadiation = id

-- | Drive FATES plant hydraulics model. Bypass.
fatesWrapHydraulicsDrive :: HLMFATESInterface -> Double -> HLMFATESInterface
fatesWrapHydraulicsDrive iface _dtime = iface

-- | Global seed dispersal across grid cells. Bypass.
fatesWrapSeedDispersal :: HLMFATESInterface -> HLMFATESInterface
fatesWrapSeedDispersal = id

-- | Update FATES accumulation variables. Bypass.
fatesUpdateAccVars :: HLMFATESInterface -> HLMFATESInterface
fatesUpdateAccVars = id

-- | Cold-start FATES initialization. Bypass.
fatesColdstart :: HLMFATESInterface -> HLMFATESInterface
fatesColdstart = id

-- | Read or write FATES restart data. Bypass.
fatesRestart :: HLMFATESInterface -> HLMFATESInterface
fatesRestart = id

-- | Transfer roughness length and displacement height each timestep. Bypass.
fatesTransferZ0mDispla :: HLMFATESInterface -> HLMFATESInterface
fatesTransferZ0mDispla = id

-- | Transfer FATES litter fluxes to CLM soil BGC model. Bypass.
fatesUpdateLitterFluxes :: HLMFATESInterface -> HLMFATESInterface
fatesUpdateLitterFluxes = id

-- | Transfer FATES harvest wood product fluxes. Bypass.
fatesWrapWoodProducts :: HLMFATESInterface -> HLMFATESInterface
fatesWrapWoodProducts = id

-- | Transfer FATES root-soil water flux. Bypass.
fatesComputeRootSoilFlux :: HLMFATESInterface -> HLMFATESInterface
fatesComputeRootSoilFlux = id

-- | FATES Satellite Phenology mode phenology update. Bypass.
fatesSpPhenology :: HLMFATESInterface -> HLMFATESInterface
fatesSpPhenology = id
