-- | Soil state data structures.
-- Fortran: SoilStateType.F90 — soil properties, hydraulics, thermal, roots.
module CLM.Types.SoilStateData
  ( SoilStateData(..)
  , defaultSoilStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column and patch level soil state (SoA layout).
-- Preserves Fortran variable names with @sstate_@ prefix.
data SoilStateData = SoilStateData
  { -- Sand / clay / organic matter
    sstate_sandfrac_patch           :: !(VU.Vector Double) -- ^ patch sand fraction
  , sstate_clayfrac_patch           :: !(VU.Vector Double) -- ^ patch clay fraction
  , sstate_mss_frc_cly_vld_col      :: !(VU.Vector Double) -- ^ col mass fraction clay (limited to 0.20)
  , sstate_cellorg_col              :: !(VU.Vector Double) -- ^ col organic matter (ncols * nlevsoi, flattened)
  , sstate_cellsand_col             :: !(VU.Vector Double) -- ^ col sand value (ncols * nlevsoi, flattened)
  , sstate_cellclay_col             :: !(VU.Vector Double) -- ^ col clay value (ncols * nlevsoi, flattened)
  , sstate_bd_col                   :: !(VU.Vector Double) -- ^ col bulk density of dry soil [kg/m^3] (ncols * nlevgrnd)
    -- Hydraulic properties
  , sstate_hksat_col                :: !(VU.Vector Double) -- ^ col sat hydraulic conductivity [mm/s] (ncols * nlevgrnd)
  , sstate_hksat_min_col            :: !(VU.Vector Double) -- ^ col mineral sat hydraulic conductivity [mm/s]
  , sstate_hk_l_col                 :: !(VU.Vector Double) -- ^ col hydraulic conductivity [mm/s]
  , sstate_smp_l_col                :: !(VU.Vector Double) -- ^ col soil matric potential [mm]
  , sstate_smpmin_col               :: !(VU.Vector Double) -- ^ col restriction for min soil potential [mm]
  , sstate_bsw_col                  :: !(VU.Vector Double) -- ^ col Clapp-Hornberger "b"
  , sstate_watsat_col               :: !(VU.Vector Double) -- ^ col porosity (ncols * nlevmaxurbgrnd)
  , sstate_watdry_col               :: !(VU.Vector Double) -- ^ col btran=0 parameter
  , sstate_watopt_col               :: !(VU.Vector Double) -- ^ col btran=1 parameter
  , sstate_watfc_col                :: !(VU.Vector Double) -- ^ col field capacity
  , sstate_sucsat_col               :: !(VU.Vector Double) -- ^ col minimum soil suction [mm]
  , sstate_dsl_col                  :: !(VU.Vector Double) -- ^ col dry surface layer thickness [mm]
  , sstate_soilresis_col            :: !(VU.Vector Double) -- ^ col soil evaporative resistance [s/m]
  , sstate_soilbeta_col             :: !(VU.Vector Double) -- ^ col ground evaporation reduction factor
  , sstate_soilalpha_col            :: !(VU.Vector Double) -- ^ col factor reducing ground sat specific humidity
  , sstate_soilalpha_u_col          :: !(VU.Vector Double) -- ^ col urban factor reducing ground sat specific humidity
  , sstate_soilpsi_col              :: !(VU.Vector Double) -- ^ col soil water potential [MPa]
  , sstate_wtfact_col               :: !(VU.Vector Double) -- ^ col max saturated fraction for gridcell
  , sstate_porosity_col             :: !(VU.Vector Double) -- ^ col soil porosity (VIC)
  , sstate_eff_porosity_col         :: !(VU.Vector Double) -- ^ col effective porosity = porosity - vol_ice
  , sstate_gwc_thr_col              :: !(VU.Vector Double) -- ^ col threshold soil moisture based on clay
    -- Van Genuchten parameters
  , sstate_msw_col                  :: !(VU.Vector Double) -- ^ col vanGenuchten "m"
  , sstate_nsw_col                  :: !(VU.Vector Double) -- ^ col vanGenuchten "n"
  , sstate_alphasw_col              :: !(VU.Vector Double) -- ^ col vanGenuchten "alpha"
  , sstate_watres_col               :: !(VU.Vector Double) -- ^ col residual soil water content
    -- Thermal conductivity / heat capacity
  , sstate_thk_col                  :: !(VU.Vector Double) -- ^ col thermal conductivity [W/m-K]
  , sstate_tkmg_col                 :: !(VU.Vector Double) -- ^ col thermal conductivity, soil minerals [W/m-K]
  , sstate_tkdry_col                :: !(VU.Vector Double) -- ^ col thermal conductivity, dry soil [W/m/K]
  , sstate_tksatu_col               :: !(VU.Vector Double) -- ^ col thermal conductivity, saturated [W/m-K]
  , sstate_csol_col                 :: !(VU.Vector Double) -- ^ col heat capacity, soil solids [J/m^3/K]
  , sstate_thk_override_col         :: !(VU.Vector Double)
      -- ^ Optional injected per-layer thermal conductivity for the soil-temp
      -- heat solve [W/m/K], laid out per column as nlevtot = nlevsno+nlevgrnd
      -- slots (snow layers first, then soil), matching the Fortran dump's
      -- THK_C(levtot) ordering. Empty ⇒ no override (default conductivity
      -- computation is used). Used by the Fortran-parity harness to inject the
      -- exact bgc-spinup conductivity.
  , sstate_cv_override_col          :: !(VU.Vector Double)
      -- ^ Optional injected per-layer volumetric heat capacity [J/m2/K], same
      -- levtot layout as 'sstate_thk_override_col'. Empty ⇒ default cv. Used by
      -- the parity harness to inject the exact Fortran cv (cvdump CV_C).
    -- Roots
  , sstate_rootr_patch              :: !(VU.Vector Double) -- ^ patch effective root fraction (npatches * nlevgrnd)
  , sstate_rootr_col                :: !(VU.Vector Double) -- ^ col effective root fraction
  , sstate_rootfr_col               :: !(VU.Vector Double) -- ^ col root fraction
  , sstate_rootfr_patch             :: !(VU.Vector Double) -- ^ patch root fraction for water
  , sstate_crootfr_patch            :: !(VU.Vector Double) -- ^ patch root fraction for carbon
  , sstate_smpso_patch              :: !(VU.Vector Double) -- ^ patch soil potential at full stomatal opening [mm]
  , sstate_smpsc_patch              :: !(VU.Vector Double) -- ^ patch soil potential at stomatal closure [mm]
  , sstate_root_depth_patch         :: !(VU.Vector Double) -- ^ patch root depth
  , sstate_rootr_road_perv_col      :: !(VU.Vector Double) -- ^ col effective root fraction in urban pervious road
  , sstate_rootfr_road_perv_col     :: !(VU.Vector Double) -- ^ col root fraction in urban pervious road
  , sstate_k_soil_root_patch        :: !(VU.Vector Double) -- ^ patch soil-root interface conductance [mm/s]
  , sstate_root_conductance_patch   :: !(VU.Vector Double) -- ^ patch root conductance [mm/s]
  , sstate_soil_conductance_patch   :: !(VU.Vector Double) -- ^ patch soil conductance [mm/s]
  } deriving (Show)

defaultSoilStateData :: SoilStateData
defaultSoilStateData = SoilStateData
  { sstate_sandfrac_patch           = VU.empty
  , sstate_clayfrac_patch           = VU.empty
  , sstate_mss_frc_cly_vld_col      = VU.empty
  , sstate_cellorg_col              = VU.empty
  , sstate_cellsand_col             = VU.empty
  , sstate_cellclay_col             = VU.empty
  , sstate_bd_col                   = VU.empty
  , sstate_hksat_col                = VU.empty
  , sstate_hksat_min_col            = VU.empty
  , sstate_hk_l_col                 = VU.empty
  , sstate_smp_l_col                = VU.empty
  , sstate_smpmin_col               = VU.empty
  , sstate_bsw_col                  = VU.empty
  , sstate_watsat_col               = VU.empty
  , sstate_watdry_col               = VU.empty
  , sstate_watopt_col               = VU.empty
  , sstate_watfc_col                = VU.empty
  , sstate_sucsat_col               = VU.empty
  , sstate_dsl_col                  = VU.empty
  , sstate_soilresis_col            = VU.empty
  , sstate_soilbeta_col             = VU.empty
  , sstate_soilalpha_col            = VU.empty
  , sstate_soilalpha_u_col          = VU.empty
  , sstate_soilpsi_col              = VU.empty
  , sstate_wtfact_col               = VU.empty
  , sstate_porosity_col             = VU.empty
  , sstate_eff_porosity_col         = VU.empty
  , sstate_gwc_thr_col              = VU.empty
  , sstate_msw_col                  = VU.empty
  , sstate_nsw_col                  = VU.empty
  , sstate_alphasw_col              = VU.empty
  , sstate_watres_col               = VU.empty
  , sstate_thk_col                  = VU.empty
  , sstate_tkmg_col                 = VU.empty
  , sstate_tkdry_col                = VU.empty
  , sstate_tksatu_col               = VU.empty
  , sstate_csol_col                 = VU.empty
  , sstate_thk_override_col         = VU.empty
  , sstate_cv_override_col          = VU.empty
  , sstate_rootr_patch              = VU.empty
  , sstate_rootr_col                = VU.empty
  , sstate_rootfr_col               = VU.empty
  , sstate_rootfr_patch             = VU.empty
  , sstate_crootfr_patch            = VU.empty
  , sstate_smpso_patch              = VU.empty
  , sstate_smpsc_patch              = VU.empty
  , sstate_root_depth_patch         = VU.empty
  , sstate_rootr_road_perv_col      = VU.empty
  , sstate_rootfr_road_perv_col     = VU.empty
  , sstate_k_soil_root_patch        = VU.empty
  , sstate_root_conductance_patch   = VU.empty
  , sstate_soil_conductance_patch   = VU.empty
  }
