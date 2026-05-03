-- | Solar absorption data structures.
-- Fortran: SolarAbsorbedType.F90 — absorbed/reflected solar radiation.
module CLM.Types.SolarAbsorbedData
  ( SolarAbsorbedData(..)
  , defaultSolarAbsorbedData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Patch and landunit level solar absorption data (SoA layout).
-- Preserves Fortran variable names with @sabs_@ prefix.
data SolarAbsorbedData = SolarAbsorbedData
  { -- Solar reflected (patch 1D)
    sabs_fsr_patch                :: !(VU.Vector Double) -- ^ Solar radiation reflected [W/m^2]
  , sabs_fsrSF_patch              :: !(VU.Vector Double) -- ^ Snow-free solar radiation reflected [W/m^2]
  , sabs_ssre_fsr_patch           :: !(VU.Vector Double) -- ^ Snow radiative effect on reflected [W/m^2]
    -- Solar absorbed (patch 1D)
  , sabs_fsa_patch                :: !(VU.Vector Double) -- ^ Solar radiation absorbed (total) [W/m^2]
  , sabs_fsa_u_patch              :: !(VU.Vector Double) -- ^ Urban solar radiation absorbed [W/m^2]
  , sabs_fsa_r_patch              :: !(VU.Vector Double) -- ^ Rural solar radiation absorbed [W/m^2]
    -- Canopy PAR absorption (patch 2D: npatch * nlevcan, flattened)
  , sabs_parsun_z_patch           :: !(VU.Vector Double) -- ^ Absorbed PAR for sunlit leaves
  , sabs_parsha_z_patch           :: !(VU.Vector Double) -- ^ Absorbed PAR for shaded leaves
  , sabs_par240d_z_patch          :: !(VU.Vector Double) -- ^ 10-day mean daytime absorbed PAR
  , sabs_par240x_z_patch          :: !(VU.Vector Double) -- ^ 10-day mean max absorbed PAR
  , sabs_par24d_z_patch           :: !(VU.Vector Double) -- ^ Daily accumulated absorbed PAR [J/m^2]
  , sabs_par24x_z_patch           :: !(VU.Vector Double) -- ^ Daily max absorbed PAR [W/m^2]
    -- Ground absorption (patch 1D)
  , sabs_sabg_soil_patch          :: !(VU.Vector Double) -- ^ Solar absorbed by soil [W/m^2]
  , sabs_sabg_snow_patch          :: !(VU.Vector Double) -- ^ Solar absorbed by snow [W/m^2]
  , sabs_sabg_patch               :: !(VU.Vector Double) -- ^ Solar absorbed by ground [W/m^2]
  , sabs_sabg_chk_patch           :: !(VU.Vector Double) -- ^ fsno weighted sum [W/m^2]
  , sabs_sabg_pen_patch           :: !(VU.Vector Double) -- ^ Shortwave penetrating top soisno layer [W/m^2]
  , sabs_sub_surf_abs_SW_patch    :: !(VU.Vector Double) -- ^ Fraction absorbed below first snow layer
    -- Ground absorption per layer (patch 2D: npatch * (nlevsno+1), flattened)
  , sabs_sabg_lyr_patch           :: !(VU.Vector Double) -- ^ Absorbed radiation in each snow/soil layer
    -- Vegetation absorption (patch 1D)
  , sabs_sabv_patch               :: !(VU.Vector Double) -- ^ Solar absorbed by vegetation [W/m^2]
    -- Urban surface absorption (landunit 2D: nlun * numrad, flattened)
  , sabs_sabs_roof_dir_lun        :: !(VU.Vector Double) -- ^ Direct solar absorbed by roof
  , sabs_sabs_roof_dif_lun        :: !(VU.Vector Double) -- ^ Diffuse solar absorbed by roof
  , sabs_sabs_sunwall_dir_lun     :: !(VU.Vector Double) -- ^ Direct solar absorbed by sunwall
  , sabs_sabs_sunwall_dif_lun     :: !(VU.Vector Double) -- ^ Diffuse solar absorbed by sunwall
  , sabs_sabs_shadewall_dir_lun   :: !(VU.Vector Double) -- ^ Direct solar absorbed by shadewall
  , sabs_sabs_shadewall_dif_lun   :: !(VU.Vector Double) -- ^ Diffuse solar absorbed by shadewall
  , sabs_sabs_improad_dir_lun     :: !(VU.Vector Double) -- ^ Direct solar absorbed by impervious road
  , sabs_sabs_improad_dif_lun     :: !(VU.Vector Double) -- ^ Diffuse solar absorbed by impervious road
  , sabs_sabs_perroad_dir_lun     :: !(VU.Vector Double) -- ^ Direct solar absorbed by pervious road
  , sabs_sabs_perroad_dif_lun     :: !(VU.Vector Double) -- ^ Diffuse solar absorbed by pervious road
    -- NIR diagnostics (patch 1D)
  , sabs_fsds_nir_d_patch         :: !(VU.Vector Double) -- ^ Incident direct beam NIR [W/m^2]
  , sabs_fsds_nir_i_patch         :: !(VU.Vector Double) -- ^ Incident diffuse NIR [W/m^2]
  , sabs_fsds_nir_d_ln_patch      :: !(VU.Vector Double) -- ^ Incident direct beam NIR at local noon [W/m^2]
  , sabs_fsr_nir_d_patch          :: !(VU.Vector Double) -- ^ Reflected direct beam NIR [W/m^2]
  , sabs_fsr_nir_i_patch          :: !(VU.Vector Double) -- ^ Reflected diffuse NIR [W/m^2]
  , sabs_fsr_nir_d_ln_patch       :: !(VU.Vector Double) -- ^ Reflected direct beam NIR at local noon [W/m^2]
    -- Snow-free NIR diagnostics (patch 1D)
  , sabs_fsrSF_nir_d_patch        :: !(VU.Vector Double) -- ^ Snow-free reflected direct beam NIR
  , sabs_fsrSF_nir_i_patch        :: !(VU.Vector Double) -- ^ Snow-free reflected diffuse NIR
  , sabs_fsrSF_nir_d_ln_patch     :: !(VU.Vector Double) -- ^ Snow-free reflected direct beam NIR at local noon
  , sabs_ssre_fsr_nir_d_patch     :: !(VU.Vector Double) -- ^ Snow radiative effect on direct beam NIR reflected
  , sabs_ssre_fsr_nir_i_patch     :: !(VU.Vector Double) -- ^ Snow radiative effect on diffuse NIR reflected
  , sabs_ssre_fsr_nir_d_ln_patch  :: !(VU.Vector Double) -- ^ Snow radiative effect on direct beam NIR at local noon
  } deriving (Show)

defaultSolarAbsorbedData :: SolarAbsorbedData
defaultSolarAbsorbedData = SolarAbsorbedData
  { sabs_fsr_patch                = VU.empty
  , sabs_fsrSF_patch              = VU.empty
  , sabs_ssre_fsr_patch           = VU.empty
  , sabs_fsa_patch                = VU.empty
  , sabs_fsa_u_patch              = VU.empty
  , sabs_fsa_r_patch              = VU.empty
  , sabs_parsun_z_patch           = VU.empty
  , sabs_parsha_z_patch           = VU.empty
  , sabs_par240d_z_patch          = VU.empty
  , sabs_par240x_z_patch          = VU.empty
  , sabs_par24d_z_patch           = VU.empty
  , sabs_par24x_z_patch           = VU.empty
  , sabs_sabg_soil_patch          = VU.empty
  , sabs_sabg_snow_patch          = VU.empty
  , sabs_sabg_patch               = VU.empty
  , sabs_sabg_chk_patch           = VU.empty
  , sabs_sabg_pen_patch           = VU.empty
  , sabs_sub_surf_abs_SW_patch    = VU.empty
  , sabs_sabg_lyr_patch           = VU.empty
  , sabs_sabv_patch               = VU.empty
  , sabs_sabs_roof_dir_lun        = VU.empty
  , sabs_sabs_roof_dif_lun        = VU.empty
  , sabs_sabs_sunwall_dir_lun     = VU.empty
  , sabs_sabs_sunwall_dif_lun     = VU.empty
  , sabs_sabs_shadewall_dir_lun   = VU.empty
  , sabs_sabs_shadewall_dif_lun   = VU.empty
  , sabs_sabs_improad_dir_lun     = VU.empty
  , sabs_sabs_improad_dif_lun     = VU.empty
  , sabs_sabs_perroad_dir_lun     = VU.empty
  , sabs_sabs_perroad_dif_lun     = VU.empty
  , sabs_fsds_nir_d_patch         = VU.empty
  , sabs_fsds_nir_i_patch         = VU.empty
  , sabs_fsds_nir_d_ln_patch      = VU.empty
  , sabs_fsr_nir_d_patch          = VU.empty
  , sabs_fsr_nir_i_patch          = VU.empty
  , sabs_fsr_nir_d_ln_patch       = VU.empty
  , sabs_fsrSF_nir_d_patch        = VU.empty
  , sabs_fsrSF_nir_i_patch        = VU.empty
  , sabs_fsrSF_nir_d_ln_patch     = VU.empty
  , sabs_ssre_fsr_nir_d_patch     = VU.empty
  , sabs_ssre_fsr_nir_i_patch     = VU.empty
  , sabs_ssre_fsr_nir_d_ln_patch  = VU.empty
  }
