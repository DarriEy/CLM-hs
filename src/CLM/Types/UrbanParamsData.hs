-- | Urban parameter data structures.
-- Fortran: UrbanParamsType.F90 — emissivities, albedos, thermal properties, view factors.
module CLM.Types.UrbanParamsData
  ( UrbanParamsData(..)
  , defaultUrbanParamsData
  , UrbanInputData(..)
  , defaultUrbanInputData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Landunit-level urban parameters (SoA layout).
-- Preserves Fortran variable names with @up_@ prefix.
data UrbanParamsData = UrbanParamsData
  { up_wind_hgt_canyon     :: !(VU.Vector Double) -- ^ Height above road for wind [m]
  , up_em_roof             :: !(VU.Vector Double) -- ^ Roof emissivity
  , up_em_improad          :: !(VU.Vector Double) -- ^ Impervious road emissivity
  , up_em_perroad          :: !(VU.Vector Double) -- ^ Pervious road emissivity
  , up_em_wall             :: !(VU.Vector Double) -- ^ Wall emissivity
  , up_alb_roof_dir        :: !(VU.Vector Double) -- ^ Direct roof albedo (nlun * numrad, flattened)
  , up_alb_roof_dif        :: !(VU.Vector Double) -- ^ Diffuse roof albedo (nlun * numrad, flattened)
  , up_alb_improad_dir     :: !(VU.Vector Double) -- ^ Direct impervious road albedo (nlun * numrad, flattened)
  , up_alb_improad_dif     :: !(VU.Vector Double) -- ^ Diffuse impervious road albedo (nlun * numrad, flattened)
  , up_alb_perroad_dir     :: !(VU.Vector Double) -- ^ Direct pervious road albedo (nlun * numrad, flattened)
  , up_alb_perroad_dif     :: !(VU.Vector Double) -- ^ Diffuse pervious road albedo (nlun * numrad, flattened)
  , up_alb_wall_dir        :: !(VU.Vector Double) -- ^ Direct wall albedo (nlun * numrad, flattened)
  , up_alb_wall_dif        :: !(VU.Vector Double) -- ^ Diffuse wall albedo (nlun * numrad, flattened)
  , up_nlev_improad        :: !(VU.Vector Int)    -- ^ Number of impervious road layers
  , up_tk_wall             :: !(VU.Vector Double) -- ^ Thermal conductivity of wall (nlun * nlevurb, flattened)
  , up_tk_roof             :: !(VU.Vector Double) -- ^ Thermal conductivity of roof (nlun * nlevurb, flattened)
  , up_tk_improad          :: !(VU.Vector Double) -- ^ Thermal conductivity of impervious road (nlun * nlevurb, flattened)
  , up_cv_wall             :: !(VU.Vector Double) -- ^ Heat capacity of wall (nlun * nlevurb, flattened)
  , up_cv_roof             :: !(VU.Vector Double) -- ^ Heat capacity of roof (nlun * nlevurb, flattened)
  , up_cv_improad          :: !(VU.Vector Double) -- ^ Heat capacity of impervious road (nlun * nlevurb, flattened)
  , up_thick_wall          :: !(VU.Vector Double) -- ^ Total thickness of wall [m]
  , up_thick_roof          :: !(VU.Vector Double) -- ^ Total thickness of roof [m]
  , up_vf_sr               :: !(VU.Vector Double) -- ^ View factor of sky for road
  , up_vf_wr               :: !(VU.Vector Double) -- ^ View factor of one wall for road
  , up_vf_sw               :: !(VU.Vector Double) -- ^ View factor of sky for one wall
  , up_vf_rw               :: !(VU.Vector Double) -- ^ View factor of road for one wall
  , up_vf_ww               :: !(VU.Vector Double) -- ^ View factor of opposing wall for one wall
  , up_t_building_min      :: !(VU.Vector Double) -- ^ Minimum internal building air temperature [K]
  , up_eflx_traffic_factor :: !(VU.Vector Double) -- ^ Multiplicative traffic factor for sensible HF
  } deriving (Show)

defaultUrbanParamsData :: UrbanParamsData
defaultUrbanParamsData = UrbanParamsData
  { up_wind_hgt_canyon     = VU.empty, up_em_roof         = VU.empty
  , up_em_improad          = VU.empty, up_em_perroad      = VU.empty
  , up_em_wall             = VU.empty, up_alb_roof_dir    = VU.empty
  , up_alb_roof_dif        = VU.empty, up_alb_improad_dir = VU.empty
  , up_alb_improad_dif     = VU.empty, up_alb_perroad_dir = VU.empty
  , up_alb_perroad_dif     = VU.empty, up_alb_wall_dir    = VU.empty
  , up_alb_wall_dif        = VU.empty, up_nlev_improad    = VU.empty
  , up_tk_wall             = VU.empty, up_tk_roof         = VU.empty
  , up_tk_improad          = VU.empty, up_cv_wall         = VU.empty
  , up_cv_roof             = VU.empty, up_cv_improad      = VU.empty
  , up_thick_wall          = VU.empty, up_thick_roof      = VU.empty
  , up_vf_sr               = VU.empty, up_vf_wr           = VU.empty
  , up_vf_sw               = VU.empty, up_vf_rw           = VU.empty
  , up_vf_ww               = VU.empty, up_t_building_min  = VU.empty
  , up_eflx_traffic_factor = VU.empty
  }

-- | Raw urban input data read from the surface dataset (SoA layout).
-- Preserves Fortran variable names with @ui_@ prefix.
data UrbanInputData = UrbanInputData
  { ui_canyon_hwr      :: !(VU.Vector Double) -- ^ Canyon H/W ratio (ng * numurbl, flattened)
  , ui_wtlunit_roof    :: !(VU.Vector Double) -- ^ Roof weight fraction (ng * numurbl, flattened)
  , ui_wtroad_perv     :: !(VU.Vector Double) -- ^ Pervious road weight (ng * numurbl, flattened)
  , ui_em_roof         :: !(VU.Vector Double) -- ^ Roof emissivity (ng * numurbl, flattened)
  , ui_em_improad      :: !(VU.Vector Double) -- ^ Impervious road emissivity (ng * numurbl, flattened)
  , ui_em_perroad      :: !(VU.Vector Double) -- ^ Pervious road emissivity (ng * numurbl, flattened)
  , ui_em_wall         :: !(VU.Vector Double) -- ^ Wall emissivity (ng * numurbl, flattened)
  , ui_alb_roof_dir    :: !(VU.Vector Double) -- ^ Direct roof albedo (3D flattened)
  , ui_alb_roof_dif    :: !(VU.Vector Double) -- ^ Diffuse roof albedo (3D flattened)
  , ui_alb_improad_dir :: !(VU.Vector Double) -- ^ Direct impervious road albedo (3D flattened)
  , ui_alb_improad_dif :: !(VU.Vector Double) -- ^ Diffuse impervious road albedo (3D flattened)
  , ui_alb_perroad_dir :: !(VU.Vector Double) -- ^ Direct pervious road albedo (3D flattened)
  , ui_alb_perroad_dif :: !(VU.Vector Double) -- ^ Diffuse pervious road albedo (3D flattened)
  , ui_alb_wall_dir    :: !(VU.Vector Double) -- ^ Direct wall albedo (3D flattened)
  , ui_alb_wall_dif    :: !(VU.Vector Double) -- ^ Diffuse wall albedo (3D flattened)
  , ui_ht_roof         :: !(VU.Vector Double) -- ^ Roof height (ng * numurbl, flattened)
  , ui_wind_hgt_canyon :: !(VU.Vector Double) -- ^ Wind height in canyon (ng * numurbl, flattened)
  , ui_tk_wall         :: !(VU.Vector Double) -- ^ Wall thermal conductivity (3D flattened)
  , ui_tk_roof         :: !(VU.Vector Double) -- ^ Roof thermal conductivity (3D flattened)
  , ui_tk_improad      :: !(VU.Vector Double) -- ^ Impervious road thermal conductivity (3D flattened)
  , ui_cv_wall         :: !(VU.Vector Double) -- ^ Wall heat capacity (3D flattened)
  , ui_cv_roof         :: !(VU.Vector Double) -- ^ Roof heat capacity (3D flattened)
  , ui_cv_improad      :: !(VU.Vector Double) -- ^ Impervious road heat capacity (3D flattened)
  , ui_thick_wall      :: !(VU.Vector Double) -- ^ Wall thickness (ng * numurbl, flattened)
  , ui_thick_roof      :: !(VU.Vector Double) -- ^ Roof thickness (ng * numurbl, flattened)
  , ui_nlev_improad    :: !(VU.Vector Int)    -- ^ Number of impervious road layers (ng * numurbl, flattened)
  , ui_t_building_min  :: !(VU.Vector Double) -- ^ Min building temperature (ng * numurbl, flattened)
  } deriving (Show)

defaultUrbanInputData :: UrbanInputData
defaultUrbanInputData = UrbanInputData
  { ui_canyon_hwr = VU.empty, ui_wtlunit_roof = VU.empty, ui_wtroad_perv = VU.empty
  , ui_em_roof = VU.empty, ui_em_improad = VU.empty, ui_em_perroad = VU.empty
  , ui_em_wall = VU.empty, ui_alb_roof_dir = VU.empty, ui_alb_roof_dif = VU.empty
  , ui_alb_improad_dir = VU.empty, ui_alb_improad_dif = VU.empty
  , ui_alb_perroad_dir = VU.empty, ui_alb_perroad_dif = VU.empty
  , ui_alb_wall_dir = VU.empty, ui_alb_wall_dif = VU.empty
  , ui_ht_roof = VU.empty, ui_wind_hgt_canyon = VU.empty
  , ui_tk_wall = VU.empty, ui_tk_roof = VU.empty, ui_tk_improad = VU.empty
  , ui_cv_wall = VU.empty, ui_cv_roof = VU.empty, ui_cv_improad = VU.empty
  , ui_thick_wall = VU.empty, ui_thick_roof = VU.empty
  , ui_nlev_improad = VU.empty, ui_t_building_min = VU.empty
  }
