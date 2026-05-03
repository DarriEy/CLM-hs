-- | Friction velocity data structures.
-- Fortran: FrictionVelocityMod.F90 — u*, z0, displacement height, stability.
module CLM.Types.FrictionVelocityData
  ( FrictionVelocityData(..)
  , defaultFrictionVelocityData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Patch and column level friction velocity data (SoA layout).
-- Preserves Fortran variable names with @fvel_@ prefix.
data FrictionVelocityData = FrictionVelocityData
  { -- Scalar parameters
    fvel_zetamaxstable :: !Double             -- ^ Max zeta under stable conditions
  , fvel_zsno          :: !Double             -- ^ Momentum roughness length for snow [m]
  , fvel_zlnd          :: !Double             -- ^ Momentum roughness length for soil/glacier/wetland [m]
  , fvel_zglc          :: !Double             -- ^ Momentum roughness length for glacier (Meier2022) [m]
    -- Patch-level fields (1D)
  , fvel_forc_hgt_u_patch :: !(VU.Vector Double) -- ^ Wind forcing height [m]
  , fvel_forc_hgt_t_patch :: !(VU.Vector Double) -- ^ Temperature forcing height [m]
  , fvel_forc_hgt_q_patch :: !(VU.Vector Double) -- ^ Humidity forcing height [m]
  , fvel_u10_patch        :: !(VU.Vector Double) -- ^ 10-m wind for dust model [m/s]
  , fvel_u10_clm_patch    :: !(VU.Vector Double) -- ^ 10-m wind for clm_map2gcell [m/s]
  , fvel_va_patch         :: !(VU.Vector Double) -- ^ Wind speed + convective velocity [m/s]
  , fvel_vds_patch        :: !(VU.Vector Double) -- ^ Deposition velocity term [m/s]
  , fvel_fv_patch         :: !(VU.Vector Double) -- ^ Friction velocity for dust model [m/s]
  , fvel_rb1_patch        :: !(VU.Vector Double) -- ^ Aerodynamical resistance [s/m]
  , fvel_rb10_patch       :: !(VU.Vector Double) -- ^ 10-day mean aerodynamical resistance [s/m]
  , fvel_ram1_patch       :: !(VU.Vector Double) -- ^ Aerodynamical resistance [s/m]
  , fvel_z0mv_patch       :: !(VU.Vector Double) -- ^ Roughness length over veg, momentum [m]
  , fvel_z0hv_patch       :: !(VU.Vector Double) -- ^ Roughness length over veg, sensible heat [m]
  , fvel_z0qv_patch       :: !(VU.Vector Double) -- ^ Roughness length over veg, latent heat [m]
  , fvel_z0mg_patch       :: !(VU.Vector Double) -- ^ Roughness length over ground, momentum [m]
  , fvel_z0hg_patch       :: !(VU.Vector Double) -- ^ Roughness length over ground, sensible heat [m]
  , fvel_z0qg_patch       :: !(VU.Vector Double) -- ^ Roughness length over ground, latent heat [m]
  , fvel_kbm1_patch       :: !(VU.Vector Double) -- ^ ln(z0mg/z0hg)
  , fvel_rah1_patch       :: !(VU.Vector Double) -- ^ Sensible heat flux resistance [s/m]
  , fvel_rah2_patch       :: !(VU.Vector Double) -- ^ Below-canopy sensible heat flux resistance [s/m]
  , fvel_raw1_patch       :: !(VU.Vector Double) -- ^ Moisture flux resistance [s/m]
  , fvel_raw2_patch       :: !(VU.Vector Double) -- ^ Below-canopy moisture flux resistance [s/m]
  , fvel_ustar_patch      :: !(VU.Vector Double) -- ^ Friction velocity [m/s]
  , fvel_um_patch         :: !(VU.Vector Double) -- ^ Wind speed including stability effect [m/s]
  , fvel_uaf_patch        :: !(VU.Vector Double) -- ^ Canopy air speed [m/s]
  , fvel_taf_patch        :: !(VU.Vector Double) -- ^ Canopy air temperature [K]
  , fvel_qaf_patch        :: !(VU.Vector Double) -- ^ Canopy humidity [kg/kg]
  , fvel_obu_patch        :: !(VU.Vector Double) -- ^ Monin-Obukhov length [m]
  , fvel_zeta_patch       :: !(VU.Vector Double) -- ^ Dimensionless stability parameter
  , fvel_vpd_patch        :: !(VU.Vector Double) -- ^ Vapor pressure deficit [Pa]
  , fvel_num_iter_patch   :: !(VU.Vector Double) -- ^ Number of iterations
  , fvel_z0m_actual_patch :: !(VU.Vector Double) -- ^ Roughness length actually used, momentum [m]
    -- Column-level fields (1D)
  , fvel_z0mg_col    :: !(VU.Vector Double) -- ^ col roughness length over ground, momentum [m]
  , fvel_z0mg_2D_col :: !(VU.Vector Double) -- ^ 2-D input col roughness length over ground, momentum [m]
  , fvel_z0hg_col    :: !(VU.Vector Double) -- ^ col roughness length over ground, sensible heat [m]
  , fvel_z0qg_col    :: !(VU.Vector Double) -- ^ col roughness length over ground, latent heat [m]
  } deriving (Show)

defaultFrictionVelocityData :: FrictionVelocityData
defaultFrictionVelocityData = FrictionVelocityData
  { fvel_zetamaxstable = 0.5
  , fvel_zsno          = 0.00085
  , fvel_zlnd          = 0.000775
  , fvel_zglc          = 0.0023
  , fvel_forc_hgt_u_patch = VU.empty
  , fvel_forc_hgt_t_patch = VU.empty
  , fvel_forc_hgt_q_patch = VU.empty
  , fvel_u10_patch        = VU.empty
  , fvel_u10_clm_patch    = VU.empty
  , fvel_va_patch         = VU.empty
  , fvel_vds_patch        = VU.empty
  , fvel_fv_patch         = VU.empty
  , fvel_rb1_patch        = VU.empty
  , fvel_rb10_patch       = VU.empty
  , fvel_ram1_patch       = VU.empty
  , fvel_z0mv_patch       = VU.empty
  , fvel_z0hv_patch       = VU.empty
  , fvel_z0qv_patch       = VU.empty
  , fvel_z0mg_patch       = VU.empty
  , fvel_z0hg_patch       = VU.empty
  , fvel_z0qg_patch       = VU.empty
  , fvel_kbm1_patch       = VU.empty
  , fvel_rah1_patch       = VU.empty
  , fvel_rah2_patch       = VU.empty
  , fvel_raw1_patch       = VU.empty
  , fvel_raw2_patch       = VU.empty
  , fvel_ustar_patch      = VU.empty
  , fvel_um_patch         = VU.empty
  , fvel_uaf_patch        = VU.empty
  , fvel_taf_patch        = VU.empty
  , fvel_qaf_patch        = VU.empty
  , fvel_obu_patch        = VU.empty
  , fvel_zeta_patch       = VU.empty
  , fvel_vpd_patch        = VU.empty
  , fvel_num_iter_patch   = VU.empty
  , fvel_z0m_actual_patch = VU.empty
  , fvel_z0mg_col    = VU.empty
  , fvel_z0mg_2D_col = VU.empty
  , fvel_z0hg_col    = VU.empty
  , fvel_z0qg_col    = VU.empty
  }
