-- | Canopy state data structures.
-- Fortran: CanopyStateType.F90 — LAI, SAI, canopy geometry, sunlit/shaded fractions.
module CLM.Types.CanopyStateData
  ( CanopyStateData(..)
  , defaultCanopyStateData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Patch-level canopy state (SoA layout).
-- Preserves Fortran variable names with @cstate_@ prefix.
data CanopyStateData = CanopyStateData
  { -- Integer patch-level fields (stored as VU.Vector Int but we use Double for uniformity)
    cstate_frac_veg_nosno_patch     :: !(VU.Vector Int)    -- ^ Fraction of veg not covered by snow (0 or 1)
  , cstate_frac_veg_nosno_alb_patch :: !(VU.Vector Int)    -- ^ Fraction of veg not covered by snow for albedo (0 or 1)
  , cstate_patch_wtgcell            :: !(VU.Vector Double) -- ^ Patch weights relative to gridcell
    -- LAI / SAI (patch 1D)
  , cstate_tlai_patch               :: !(VU.Vector Double) -- ^ One-sided leaf area index, no burying by snow
  , cstate_tsai_patch               :: !(VU.Vector Double) -- ^ One-sided stem area index, no burying by snow
  , cstate_elai_patch               :: !(VU.Vector Double) -- ^ One-sided leaf area index with burying by snow
  , cstate_esai_patch               :: !(VU.Vector Double) -- ^ One-sided stem area index with burying by snow
    -- SP mode history fields
  , cstate_tlai_hist_patch          :: !(VU.Vector Double) -- ^ LAI for SP mode
  , cstate_tsai_hist_patch          :: !(VU.Vector Double) -- ^ SAI for SP mode
  , cstate_htop_hist_patch          :: !(VU.Vector Double) -- ^ Canopy height for SP mode [m]
    -- Sunlit/shaded LAI (patch 1D)
  , cstate_elai240_patch            :: !(VU.Vector Double) -- ^ LAI with burying, 10-day avg
  , cstate_laisun_patch             :: !(VU.Vector Double) -- ^ Sunlit projected LAI
  , cstate_laisha_patch             :: !(VU.Vector Double) -- ^ Shaded projected LAI
  , cstate_mlaidiff_patch           :: !(VU.Vector Double) -- ^ Diff between LAI month one and two
    -- Sunlit/shaded LAI by canopy layer (patch 2D: npatch * nlevcan)
  , cstate_laisun_z_patch           :: !(VU.Vector Double) -- ^ Sunlit LAI per canopy layer (flattened)
  , cstate_laisha_z_patch           :: !(VU.Vector Double) -- ^ Shaded LAI per canopy layer (flattened)
    -- Monthly LAI (patch 2D: npatch * 12)
  , cstate_annlai_patch             :: !(VU.Vector Double) -- ^ 12 months of monthly LAI (flattened)
    -- Biomass (patch 1D)
  , cstate_stem_biomass_patch       :: !(VU.Vector Double) -- ^ Aboveground stem biomass [kg/m^2]
  , cstate_leaf_biomass_patch       :: !(VU.Vector Double) -- ^ Aboveground leaf biomass [kg/m^2]
    -- Canopy geometry (patch 1D)
  , cstate_htop_patch               :: !(VU.Vector Double) -- ^ Canopy top [m]
  , cstate_hbot_patch               :: !(VU.Vector Double) -- ^ Canopy bottom [m]
  , cstate_z0m_patch                :: !(VU.Vector Double) -- ^ Momentum roughness length [m]
  , cstate_displa_patch             :: !(VU.Vector Double) -- ^ Displacement height [m]
    -- PFT optical properties (patch 1D and patch*numrad flattened)
  , cstate_xl_patch                 :: !(VU.Vector Double) -- ^ Leaf angle departure from spherical
  , cstate_rhol_patch               :: !(VU.Vector Double) -- ^ Leaf reflectance by patch and band
  , cstate_rhos_patch               :: !(VU.Vector Double) -- ^ Stem reflectance by patch and band
  , cstate_taul_patch               :: !(VU.Vector Double) -- ^ Leaf transmittance by patch and band
  , cstate_taus_patch               :: !(VU.Vector Double) -- ^ Stem transmittance by patch and band
    -- Sunlit fraction (patch 1D)
  , cstate_fsun_patch               :: !(VU.Vector Double) -- ^ Sunlit fraction of canopy
  , cstate_fsun24_patch             :: !(VU.Vector Double) -- ^ 24hr avg sunlit fraction
  , cstate_fsun240_patch            :: !(VU.Vector Double) -- ^ 240hr avg sunlit fraction
    -- Leaf properties (patch 1D)
  , cstate_dleaf_patch              :: !(VU.Vector Double) -- ^ Characteristic leaf width [m]
  , cstate_rscanopy_patch           :: !(VU.Vector Double) -- ^ Canopy stomatal resistance [s/m]
    -- Vegetation water potential (patch 2D: npatch * nvegwcs, flattened)
  , cstate_vegwp_patch              :: !(VU.Vector Double) -- ^ Vegetation water matric potential [mm]
  , cstate_vegwp_ln_patch           :: !(VU.Vector Double) -- ^ Vegetation water matric potential at local noon [mm]
  , cstate_vegwp_pd_patch           :: !(VU.Vector Double) -- ^ Predawn vegetation water matric potential [mm]
    -- Namelist parameter (scalar)
  , cstate_leaf_mr_vcm              :: !Double             -- ^ Leaf respiration constant with Vcmax
  } deriving (Show)

defaultCanopyStateData :: CanopyStateData
defaultCanopyStateData = CanopyStateData
  { cstate_frac_veg_nosno_patch     = VU.empty
  , cstate_frac_veg_nosno_alb_patch = VU.empty
  , cstate_patch_wtgcell            = VU.empty
  , cstate_tlai_patch               = VU.empty
  , cstate_tsai_patch               = VU.empty
  , cstate_elai_patch               = VU.empty
  , cstate_esai_patch               = VU.empty
  , cstate_tlai_hist_patch          = VU.empty
  , cstate_tsai_hist_patch          = VU.empty
  , cstate_htop_hist_patch          = VU.empty
  , cstate_elai240_patch            = VU.empty
  , cstate_laisun_patch             = VU.empty
  , cstate_laisha_patch             = VU.empty
  , cstate_mlaidiff_patch           = VU.empty
  , cstate_laisun_z_patch           = VU.empty
  , cstate_laisha_z_patch           = VU.empty
  , cstate_annlai_patch             = VU.empty
  , cstate_stem_biomass_patch       = VU.empty
  , cstate_leaf_biomass_patch       = VU.empty
  , cstate_htop_patch               = VU.empty
  , cstate_hbot_patch               = VU.empty
  , cstate_z0m_patch                = VU.empty
  , cstate_displa_patch             = VU.empty
  , cstate_xl_patch                 = VU.empty
  , cstate_rhol_patch               = VU.empty
  , cstate_rhos_patch               = VU.empty
  , cstate_taul_patch               = VU.empty
  , cstate_taus_patch               = VU.empty
  , cstate_fsun_patch               = VU.empty
  , cstate_fsun24_patch             = VU.empty
  , cstate_fsun240_patch            = VU.empty
  , cstate_dleaf_patch              = VU.empty
  , cstate_rscanopy_patch           = VU.empty
  , cstate_vegwp_patch              = VU.empty
  , cstate_vegwp_ln_patch           = VU.empty
  , cstate_vegwp_pd_patch           = VU.empty
  , cstate_leaf_mr_vcm              = 0.015
  }
