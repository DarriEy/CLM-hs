-- | Simulation control settings and consistency checks.
-- Ported from: src/main/controlMod.F90
-- Preserves Fortran variable names for traceability.
module CLM.Infrastructure.Control
  ( -- * Run-type constants
    nsrStartup
  , nsrContinue
  , nsrBranch
  , runTypeLabel
    -- * Control configuration
  , VarCtl(..)
  , defaultVarCtl
    -- * Initialization and validation
  , controlInit
  , ControlError(..)
    -- * Printing
  , controlPrintLines
  ) where

import qualified Data.Vector as V

-- ---------------------------------------------------------------------------
-- Run-type constants
-- ---------------------------------------------------------------------------

nsrStartup, nsrContinue, nsrBranch :: Int
nsrStartup  = 0
nsrContinue = 1
nsrBranch   = 2

-- | Human-readable run-type labels (indexed nsrest + 1).
runTypeLabels :: V.Vector String
runTypeLabels = V.fromList ["initial", "restart", "branch", "missing"]

-- | Look up the label for a given nsrest value.
runTypeLabel :: Int -> String
runTypeLabel n
  | n >= 0 && n <= 2 = runTypeLabels V.! n
  | otherwise        = "unknown"

-- ---------------------------------------------------------------------------
-- VarCtl: simulation control settings
-- ---------------------------------------------------------------------------

-- | Top-level control flags for a CLM simulation.
-- Mirrors Fortran @clm_varctl@ module-level variables.
data VarCtl = VarCtl
  { ctl_nsrest                          :: !Int
  , ctl_source                          :: !String
  , ctl_version                         :: !String
  , ctl_ctitle                          :: !String
  , ctl_username                        :: !String
  , ctl_hostname                        :: !String
  , ctl_nrevsn                          :: !String
  -- Process control
  , ctl_use_cn                          :: !Bool
  , ctl_use_cndv                        :: !Bool
  , ctl_use_crop                        :: !Bool
  , ctl_create_crop_landunit            :: !Bool
  , ctl_use_fates                       :: !Bool
  , ctl_use_fates_sp                    :: !Bool
  , ctl_use_fates_bgc                   :: !Bool
  , ctl_use_lch4                        :: !Bool
  , ctl_use_nitrif_denitrif             :: !Bool
  , ctl_use_extralakelayers             :: !Bool
  , ctl_use_vichydro                    :: !Bool
  , ctl_use_excess_ice                  :: !Bool
  , ctl_use_luna                        :: !Bool
  , ctl_use_hydrstress                  :: !Bool
  , ctl_use_lai_streams                 :: !Bool
  , ctl_use_snicar_frc                  :: !Bool
  , ctl_snicar_use_aerosol              :: !Bool
  , ctl_snicar_snobc_intmix            :: !Bool
  , ctl_snicar_snodst_intmix           :: !Bool
  , ctl_use_vancouver                   :: !Bool
  , ctl_use_mexicocity                  :: !Bool
  , ctl_use_noio                        :: !Bool
  , ctl_use_SSRE                        :: !Bool
  , ctl_use_c13                         :: !Bool
  , ctl_use_c14                         :: !Bool
  , ctl_use_flexibleCN                  :: !Bool
  , ctl_use_hillslope                   :: !Bool
  , ctl_downscale_hillslope_meteorology :: !Bool
  , ctl_use_hillslope_routing           :: !Bool
  , ctl_hillslope_fsat_equals_zero      :: !Bool
  , ctl_use_fertilizer                  :: !Bool
  , ctl_use_grainproduct                :: !Bool
  , ctl_irrigate                        :: !Bool
  , ctl_flush_gdd20                     :: !Bool
  , ctl_anoxia                          :: !Bool
  , ctl_all_active                      :: !Bool
  , ctl_single_column                   :: !Bool
  , ctl_run_zero_weight_urban           :: !Bool
  , ctl_convert_ocean_to_land           :: !Bool
  , ctl_glc_do_dynglacier               :: !Bool
  , ctl_collapse_urban                  :: !Bool
  , ctl_use_bedrock                     :: !Bool
  -- Scalar values
  , ctl_co2_type                        :: !String
  , ctl_co2_ppmv                        :: !Double
  , ctl_crop_residue_removal_frac       :: !Double
  , ctl_nfix_timeconst                  :: !Double
  , ctl_o3_veg_stress_method            :: !String
  , ctl_scmlat                          :: !Double
  , ctl_scmlon                          :: !Double
  , ctl_n_dom_pfts                      :: !Int
  , ctl_n_dom_landunits                 :: !Int
  , ctl_toosmall_soil                   :: !Double
  , ctl_toosmall_crop                   :: !Double
  , ctl_toosmall_glacier                :: !Double
  , ctl_toosmall_lake                   :: !Double
  , ctl_toosmall_wetland                :: !Double
  , ctl_toosmall_urban                  :: !Double
  , ctl_spinup_state                    :: !Int
  , ctl_spinup_matrixcn                 :: !Bool
  , ctl_hist_wrt_matrixcn_diag          :: !Bool
  , ctl_override_bgc_restart_mismatch_dump :: !Bool
  -- Files
  , ctl_paramfile                       :: !String
  , ctl_fsurdat                         :: !String
  , ctl_soil_layerstruct_predefined     :: !String
  , ctl_nsegspc                         :: !Int
  , ctl_snow_thermal_cond_method        :: !String
  , ctl_snow_cover_fraction_method      :: !String
  -- FATES
  , ctl_fates_spitfire_mode             :: !Int
  , ctl_fates_harvest_mode              :: !String
  , ctl_fates_paramfile                 :: !String
  , ctl_fates_parteh_mode               :: !Int
  , ctl_use_fates_planthydro            :: !Bool
  , ctl_use_fates_tree_damage           :: !Bool
  , ctl_use_fates_cohort_age_tracking   :: !Bool
  , ctl_use_fates_ed_st3               :: !Bool
  , ctl_use_fates_ed_prescribed_phys   :: !Bool
  , ctl_use_fates_inventory_init       :: !Bool
  , ctl_use_fates_fixed_biogeog        :: !Bool
  , ctl_use_fates_nocomp               :: !Bool
  , ctl_use_fates_luh                  :: !Bool
  , ctl_use_fates_lupft                :: !Bool
  , ctl_use_fates_potentialveg         :: !Bool
  , ctl_fates_seeddisp_cadence         :: !Int
  } deriving (Show)

iundef :: Int
iundef = -999999999

rundef :: Double
rundef = -1.0e36

defaultVarCtl :: VarCtl
defaultVarCtl = VarCtl
  { ctl_nsrest = iundef
  , ctl_source = "Community Land Model CLM"
  , ctl_version = "CLM5.0-hs"
  , ctl_ctitle = ""
  , ctl_username = ""
  , ctl_hostname = ""
  , ctl_nrevsn = ""
  , ctl_use_cn = False
  , ctl_use_cndv = False
  , ctl_use_crop = False
  , ctl_create_crop_landunit = False
  , ctl_use_fates = False
  , ctl_use_fates_sp = False
  , ctl_use_fates_bgc = False
  , ctl_use_lch4 = False
  , ctl_use_nitrif_denitrif = False
  , ctl_use_extralakelayers = False
  , ctl_use_vichydro = False
  , ctl_use_excess_ice = False
  , ctl_use_luna = False
  , ctl_use_hydrstress = False
  , ctl_use_lai_streams = False
  , ctl_use_snicar_frc = False
  , ctl_snicar_use_aerosol = True
  , ctl_snicar_snobc_intmix = False
  , ctl_snicar_snodst_intmix = False
  , ctl_use_vancouver = False
  , ctl_use_mexicocity = False
  , ctl_use_noio = False
  , ctl_use_SSRE = False
  , ctl_use_c13 = False
  , ctl_use_c14 = False
  , ctl_use_flexibleCN = False
  , ctl_use_hillslope = False
  , ctl_downscale_hillslope_meteorology = False
  , ctl_use_hillslope_routing = False
  , ctl_hillslope_fsat_equals_zero = False
  , ctl_use_fertilizer = False
  , ctl_use_grainproduct = False
  , ctl_irrigate = False
  , ctl_flush_gdd20 = False
  , ctl_anoxia = False
  , ctl_all_active = False
  , ctl_single_column = False
  , ctl_run_zero_weight_urban = False
  , ctl_convert_ocean_to_land = False
  , ctl_glc_do_dynglacier = False
  , ctl_collapse_urban = False
  , ctl_use_bedrock = True
  , ctl_co2_type = "constant"
  , ctl_co2_ppmv = 367.0
  , ctl_crop_residue_removal_frac = 0.0
  , ctl_nfix_timeconst = -1.2345
  , ctl_o3_veg_stress_method = "unset"
  , ctl_scmlat = rundef
  , ctl_scmlon = rundef
  , ctl_n_dom_pfts = 0
  , ctl_n_dom_landunits = 0
  , ctl_toosmall_soil = -1.0
  , ctl_toosmall_crop = -1.0
  , ctl_toosmall_glacier = -1.0
  , ctl_toosmall_lake = -1.0
  , ctl_toosmall_wetland = -1.0
  , ctl_toosmall_urban = -1.0
  , ctl_spinup_state = 0
  , ctl_spinup_matrixcn = False
  , ctl_hist_wrt_matrixcn_diag = False
  , ctl_override_bgc_restart_mismatch_dump = False
  , ctl_paramfile = ""
  , ctl_fsurdat = ""
  , ctl_soil_layerstruct_predefined = "20SL_8.5m"
  , ctl_nsegspc = 20
  , ctl_snow_thermal_cond_method = "Jordan1991"
  , ctl_snow_cover_fraction_method = "SwensonLawrence2012"
  , ctl_fates_spitfire_mode = 0
  , ctl_fates_harvest_mode = "no_harvest"
  , ctl_fates_paramfile = ""
  , ctl_fates_parteh_mode = 1
  , ctl_use_fates_planthydro = False
  , ctl_use_fates_tree_damage = False
  , ctl_use_fates_cohort_age_tracking = False
  , ctl_use_fates_ed_st3 = False
  , ctl_use_fates_ed_prescribed_phys = False
  , ctl_use_fates_inventory_init = False
  , ctl_use_fates_fixed_biogeog = False
  , ctl_use_fates_nocomp = False
  , ctl_use_fates_luh = False
  , ctl_use_fates_lupft = False
  , ctl_use_fates_potentialveg = False
  , ctl_fates_seeddisp_cadence = 0
  }

-- ---------------------------------------------------------------------------
-- Validation errors
-- ---------------------------------------------------------------------------

-- | Possible errors from 'controlInit' validation.
data ControlError
  = InvalidCO2Type !String
  | NsrestNotSet
  | BranchNeedsRestart
  | CO2PPMVOutOfRange !Double
  | SnicarIntmixConflict
  | InvalidNsrest !Int
  | CropNeedsCropLandunit
  | SingleColumnNeedsLatLon
  | FATESConflict !String
  | LAIStreamConflict
  | AnoxiaNeedsCH4
  | DynGlacierConflict !String
  | InvalidSpinupState !Int
  deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- controlInit: apply derived defaults + consistency checks
-- ---------------------------------------------------------------------------

-- | Apply derived defaults and perform consistency checks.
-- Returns @Right ctl'@ with updated control flags, or @Left err@ on failure.
controlInit :: VarCtl -> Either ControlError VarCtl
controlInit ctl0 = do
  let ctl1 = applyFatesOverrides ctl0
      ctl2 = applyAnoxia ctl1
      ctl3 = applyNfixTimeconst ctl2
      ctl4 = applyNrevsn ctl3

  -- Consistency checks
  checkCO2Type ctl4
  checkNsrestSet ctl4
  checkBranchRestart ctl4
  checkCO2PPMV ctl4
  checkSnicarIntmix ctl4
  checkNsrestValid ctl4
  checkCropLandunit ctl4
  checkSingleColumn ctl4
  checkFATES ctl4
  checkLAIStreams ctl4
  checkAnoxiaCH4 ctl4
  checkDynGlacier ctl4
  checkSpinupState ctl4

  return ctl4

applyFatesOverrides :: VarCtl -> VarCtl
applyFatesOverrides ctl
  | ctl_use_fates ctl =
      ctl { ctl_spinup_matrixcn = False
          , ctl_hist_wrt_matrixcn_diag = False
          , ctl_use_fates_bgc = not (ctl_use_fates_sp ctl)
          }
  | otherwise =
      ctl { ctl_use_fates_sp = False
          , ctl_use_fates_bgc = False
          }

applyAnoxia :: VarCtl -> VarCtl
applyAnoxia ctl = ctl { ctl_anoxia = ctl_use_lch4 ctl }

applyNfixTimeconst :: VarCtl -> VarCtl
applyNfixTimeconst ctl
  | ctl_nfix_timeconst ctl == -1.2345 =
      ctl { ctl_nfix_timeconst = if ctl_use_nitrif_denitrif ctl then 10.0 else 0.0 }
  | otherwise = ctl

applyNrevsn :: VarCtl -> VarCtl
applyNrevsn ctl
  | ctl_nsrest ctl == nsrStartup  = ctl { ctl_nrevsn = "" }
  | ctl_nsrest ctl == nsrContinue = ctl { ctl_nrevsn = "set by restart pointer file" }
  | otherwise = ctl

-- Checks
checkCO2Type :: VarCtl -> Either ControlError ()
checkCO2Type ctl
  | ctl_co2_type ctl `elem` ["constant", "prognostic", "diagnostic"] = Right ()
  | otherwise = Left (InvalidCO2Type (ctl_co2_type ctl))

checkNsrestSet :: VarCtl -> Either ControlError ()
checkNsrestSet ctl
  | ctl_nsrest ctl == iundef = Left NsrestNotSet
  | otherwise = Right ()

checkBranchRestart :: VarCtl -> Either ControlError ()
checkBranchRestart ctl
  | ctl_nsrest ctl == nsrBranch && null (ctl_nrevsn ctl) = Left BranchNeedsRestart
  | otherwise = Right ()

checkCO2PPMV :: VarCtl -> Either ControlError ()
checkCO2PPMV ctl
  | ctl_co2_ppmv ctl <= 0.0 || ctl_co2_ppmv ctl > 3000.0 =
      Left (CO2PPMVOutOfRange (ctl_co2_ppmv ctl))
  | otherwise = Right ()

checkSnicarIntmix :: VarCtl -> Either ControlError ()
checkSnicarIntmix ctl
  | ctl_snicar_snobc_intmix ctl && ctl_snicar_snodst_intmix ctl =
      Left SnicarIntmixConflict
  | otherwise = Right ()

checkNsrestValid :: VarCtl -> Either ControlError ()
checkNsrestValid ctl
  | ctl_nsrest ctl `elem` [nsrStartup, nsrContinue, nsrBranch] = Right ()
  | otherwise = Left (InvalidNsrest (ctl_nsrest ctl))

checkCropLandunit :: VarCtl -> Either ControlError ()
checkCropLandunit ctl
  | ctl_use_crop ctl && not (ctl_create_crop_landunit ctl) =
      Left CropNeedsCropLandunit
  | otherwise = Right ()

checkSingleColumn :: VarCtl -> Either ControlError ()
checkSingleColumn ctl
  | ctl_single_column ctl &&
    (ctl_scmlat ctl == rundef || ctl_scmlon ctl == rundef) =
      Left SingleColumnNeedsLatLon
  | otherwise = Right ()

checkFATES :: VarCtl -> Either ControlError ()
checkFATES ctl
  | not (ctl_use_fates ctl) = Right ()
  | ctl_use_cn ctl          = Left (FATESConflict "use_cn")
  | ctl_use_hydrstress ctl  = Left (FATESConflict "use_hydrstress")
  | ctl_use_crop ctl        = Left (FATESConflict "use_crop")
  | ctl_use_luna ctl        = Left (FATESConflict "use_luna")
  | ctl_o3_veg_stress_method ctl /= "unset" = Left (FATESConflict "ozone")
  | ctl_use_c13 ctl || ctl_use_c14 ctl = Left (FATESConflict "C13/C14")
  | otherwise = Right ()

checkLAIStreams :: VarCtl -> Either ControlError ()
checkLAIStreams ctl
  | not (ctl_use_lai_streams ctl) = Right ()
  | (ctl_use_fates ctl && not (ctl_use_fates_sp ctl)) || ctl_use_cn ctl =
      Left LAIStreamConflict
  | otherwise = Right ()

checkAnoxiaCH4 :: VarCtl -> Either ControlError ()
checkAnoxiaCH4 ctl
  | not (ctl_use_lch4 ctl) && ctl_anoxia ctl = Left AnoxiaNeedsCH4
  | otherwise = Right ()

checkDynGlacier :: VarCtl -> Either ControlError ()
checkDynGlacier ctl
  | not (ctl_glc_do_dynglacier ctl) = Right ()
  | ctl_collapse_urban ctl =
      Left (DynGlacierConflict "collapse_urban")
  | ctl_n_dom_pfts ctl > 0 || ctl_n_dom_landunits ctl > 0 ||
    ctl_toosmall_soil ctl > 0.0 || ctl_toosmall_crop ctl > 0.0 ||
    ctl_toosmall_glacier ctl > 0.0 || ctl_toosmall_lake ctl > 0.0 ||
    ctl_toosmall_wetland ctl > 0.0 || ctl_toosmall_urban ctl > 0.0 =
      Left (DynGlacierConflict "toosmall/n_dom constraints")
  | otherwise = Right ()

checkSpinupState :: VarCtl -> Either ControlError ()
checkSpinupState ctl
  | ctl_spinup_state ctl `elem` [0, 1, 2] = Right ()
  | otherwise = Left (InvalidSpinupState (ctl_spinup_state ctl))

-- ---------------------------------------------------------------------------
-- controlPrintLines: produce human-readable control summary
-- ---------------------------------------------------------------------------

-- | Generate lines describing run control settings.
controlPrintLines :: VarCtl -> [String]
controlPrintLines ctl =
  [ "define run:"
  , "   source                = " ++ ctl_source ctl
  , "   model_version         = " ++ ctl_version ctl
  , "   run type              = " ++ runTypeLabel (ctl_nsrest ctl)
  , "   case title            = " ++ ctl_ctitle ctl
  , "process control parameters:"
  , "    use_lch4 = " ++ show (ctl_use_lch4 ctl)
  , "    use_nitrif_denitrif = " ++ show (ctl_use_nitrif_denitrif ctl)
  , "    use_cn = " ++ show (ctl_use_cn ctl)
  , "    use_cndv = " ++ show (ctl_use_cndv ctl)
  , "    use_crop = " ++ show (ctl_use_crop ctl)
  , "    use_luna = " ++ show (ctl_use_luna ctl)
  , "    use_hydrstress = " ++ show (ctl_use_hydrstress ctl)
  , "    co2_type = " ++ ctl_co2_type ctl
  , "    co2_ppmv = " ++ show (ctl_co2_ppmv ctl)
  , "    use_hillslope = " ++ show (ctl_use_hillslope ctl)
  , "    soil_layerstruct_predefined = " ++ ctl_soil_layerstruct_predefined ctl
  , "    nsegspc = " ++ show (ctl_nsegspc ctl)
  , "input data files:"
  , "   paramfile = " ++ ctl_paramfile ctl
  , "   fsurdat   = " ++ ctl_fsurdat ctl
  ]
