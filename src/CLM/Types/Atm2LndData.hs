-- | Atmosphere-to-land forcing data structures.
-- Fortran: atm2lndType.F90 — atmospheric forcing fields, downscaled and gridcell-level.
module CLM.Types.Atm2LndData
  ( Atm2LndData(..)
  , defaultAtm2LndData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Gridcell, column, and patch level atmospheric forcing (SoA layout).
-- Preserves Fortran variable names with @a2l_@ prefix.
data Atm2LndData = Atm2LndData
  { -- atm->lnd not downscaled (gridcell-level)
    a2l_forc_u_grc                    :: !(VU.Vector Double) -- ^ Wind speed, east direction [m/s]
  , a2l_forc_v_grc                    :: !(VU.Vector Double) -- ^ Wind speed, north direction [m/s]
  , a2l_forc_wind_grc                 :: !(VU.Vector Double) -- ^ Atmospheric wind speed [m/s]
  , a2l_forc_hgt_grc                  :: !(VU.Vector Double) -- ^ Atmospheric reference height [m]
  , a2l_forc_topo_grc                 :: !(VU.Vector Double) -- ^ Atmospheric surface height [m]
  , a2l_forc_hgt_u_grc                :: !(VU.Vector Double) -- ^ Obs height of wind [m]
  , a2l_forc_hgt_t_grc                :: !(VU.Vector Double) -- ^ Obs height of temperature [m]
  , a2l_forc_hgt_q_grc                :: !(VU.Vector Double) -- ^ Obs height of humidity [m]
  , a2l_forc_vp_grc                   :: !(VU.Vector Double) -- ^ Atmospheric vapor pressure [Pa]
  , a2l_forc_pco2_grc                 :: !(VU.Vector Double) -- ^ CO2 partial pressure [Pa]
  , a2l_forc_pco2_240_patch           :: !(VU.Vector Double) -- ^ 10-day mean CO2 partial pressure [Pa]
  , a2l_forc_solad_not_downscaled_grc :: !(VU.Vector Double) -- ^ Direct beam radiation (ng * numrad, flattened)
  , a2l_forc_solai_grc                :: !(VU.Vector Double) -- ^ Diffuse radiation (ng * numrad, flattened)
  , a2l_forc_solar_not_downscaled_grc :: !(VU.Vector Double) -- ^ Incident solar radiation [W/m^2]
  , a2l_forc_solar_downscaled_col     :: !(VU.Vector Double) -- ^ Incident solar radiation, downscaled [W/m^2]
  , a2l_forc_ndep_grc                 :: !(VU.Vector Double) -- ^ Nitrogen deposition rate [gN/m^2/s]
  , a2l_forc_pc13o2_grc               :: !(VU.Vector Double) -- ^ C13O2 partial pressure [Pa]
  , a2l_forc_po2_grc                  :: !(VU.Vector Double) -- ^ O2 partial pressure [Pa]
  , a2l_forc_po2_240_patch            :: !(VU.Vector Double) -- ^ 10-day mean O2 partial pressure [Pa]
  , a2l_forc_aer_grc                  :: !(VU.Vector Double) -- ^ Aerosol deposition (ng * 14, flattened)
  , a2l_forc_pch4_grc                 :: !(VU.Vector Double) -- ^ CH4 partial pressure [Pa]
  , a2l_forc_o3_grc                   :: !(VU.Vector Double) -- ^ Ozone partial pressure [mol/mol]
    -- atm->lnd not downscaled (gridcell-level, thermo)
  , a2l_forc_t_not_downscaled_grc     :: !(VU.Vector Double) -- ^ Temperature [K]
  , a2l_forc_th_not_downscaled_grc    :: !(VU.Vector Double) -- ^ Potential temperature [K]
  , a2l_forc_pbot_not_downscaled_grc  :: !(VU.Vector Double) -- ^ Pressure [Pa]
  , a2l_forc_pbot240_downscaled_patch :: !(VU.Vector Double) -- ^ 10-day mean downscaled pressure [Pa]
  , a2l_forc_rho_not_downscaled_grc   :: !(VU.Vector Double) -- ^ Density [kg/m^3]
  , a2l_forc_lwrad_not_downscaled_grc :: !(VU.Vector Double) -- ^ Downward IR longwave radiation [W/m^2]
    -- atm->lnd downscaled (column-level)
  , a2l_forc_t_downscaled_col         :: !(VU.Vector Double) -- ^ Downscaled temperature [K]
  , a2l_forc_th_downscaled_col        :: !(VU.Vector Double) -- ^ Downscaled potential temperature [K]
  , a2l_forc_pbot_downscaled_col      :: !(VU.Vector Double) -- ^ Downscaled pressure [Pa]
  , a2l_forc_rho_downscaled_col       :: !(VU.Vector Double) -- ^ Downscaled density [kg/m^3]
  , a2l_forc_lwrad_downscaled_col     :: !(VU.Vector Double) -- ^ Downscaled downward IR longwave [W/m^2]
  , a2l_forc_solad_downscaled_col     :: !(VU.Vector Double) -- ^ Direct beam radiation downscaled (nc * numrad)
    -- Precipitation / humidity (gridcell-level, not downscaled)
  , a2l_forc_rain_not_downscaled_grc  :: !(VU.Vector Double) -- ^ Rain rate [mm H2O/s]
  , a2l_forc_snow_not_downscaled_grc  :: !(VU.Vector Double) -- ^ Snow rate [mm H2O/s]
  , a2l_forc_q_not_downscaled_grc     :: !(VU.Vector Double) -- ^ Specific humidity [kg/kg]
    -- Precipitation / humidity (column-level, downscaled)
  , a2l_forc_rain_downscaled_col      :: !(VU.Vector Double) -- ^ Rain rate [mm H2O/s]
  , a2l_forc_snow_downscaled_col      :: !(VU.Vector Double) -- ^ Snow rate [mm H2O/s]
  , a2l_forc_q_downscaled_col         :: !(VU.Vector Double) -- ^ Specific humidity [kg/kg]
    -- Time averaged quantities (patch-level)
  , a2l_fsd24_patch                   :: !(VU.Vector Double) -- ^ 24hr avg direct beam radiation
  , a2l_fsd240_patch                  :: !(VU.Vector Double) -- ^ 240hr avg direct beam radiation
  , a2l_fsi24_patch                   :: !(VU.Vector Double) -- ^ 24hr avg diffuse beam radiation
  , a2l_fsi240_patch                  :: !(VU.Vector Double) -- ^ 240hr avg diffuse beam radiation
  , a2l_wind24_patch                  :: !(VU.Vector Double) -- ^ 24-hour running mean of wind
  , a2l_t_mo_patch                    :: !(VU.Vector Double) -- ^ 30-day average temperature [K]
  , a2l_t_mo_min_patch                :: !(VU.Vector Double) -- ^ Annual min of t_mo [K]
  } deriving (Show)

defaultAtm2LndData :: Atm2LndData
defaultAtm2LndData = Atm2LndData
  { a2l_forc_u_grc                    = VU.empty
  , a2l_forc_v_grc                    = VU.empty
  , a2l_forc_wind_grc                 = VU.empty
  , a2l_forc_hgt_grc                  = VU.empty
  , a2l_forc_topo_grc                 = VU.empty
  , a2l_forc_hgt_u_grc                = VU.empty
  , a2l_forc_hgt_t_grc                = VU.empty
  , a2l_forc_hgt_q_grc                = VU.empty
  , a2l_forc_vp_grc                   = VU.empty
  , a2l_forc_pco2_grc                 = VU.empty
  , a2l_forc_pco2_240_patch           = VU.empty
  , a2l_forc_solad_not_downscaled_grc = VU.empty
  , a2l_forc_solai_grc                = VU.empty
  , a2l_forc_solar_not_downscaled_grc = VU.empty
  , a2l_forc_solar_downscaled_col     = VU.empty
  , a2l_forc_ndep_grc                 = VU.empty
  , a2l_forc_pc13o2_grc               = VU.empty
  , a2l_forc_po2_grc                  = VU.empty
  , a2l_forc_po2_240_patch            = VU.empty
  , a2l_forc_aer_grc                  = VU.empty
  , a2l_forc_pch4_grc                 = VU.empty
  , a2l_forc_o3_grc                   = VU.empty
  , a2l_forc_t_not_downscaled_grc     = VU.empty
  , a2l_forc_th_not_downscaled_grc    = VU.empty
  , a2l_forc_pbot_not_downscaled_grc  = VU.empty
  , a2l_forc_pbot240_downscaled_patch = VU.empty
  , a2l_forc_rho_not_downscaled_grc   = VU.empty
  , a2l_forc_lwrad_not_downscaled_grc = VU.empty
  , a2l_forc_t_downscaled_col         = VU.empty
  , a2l_forc_th_downscaled_col        = VU.empty
  , a2l_forc_pbot_downscaled_col      = VU.empty
  , a2l_forc_rho_downscaled_col       = VU.empty
  , a2l_forc_lwrad_downscaled_col     = VU.empty
  , a2l_forc_solad_downscaled_col     = VU.empty
  , a2l_forc_rain_not_downscaled_grc  = VU.empty
  , a2l_forc_snow_not_downscaled_grc  = VU.empty
  , a2l_forc_q_not_downscaled_grc     = VU.empty
  , a2l_forc_rain_downscaled_col      = VU.empty
  , a2l_forc_snow_downscaled_col      = VU.empty
  , a2l_forc_q_downscaled_col         = VU.empty
  , a2l_fsd24_patch                   = VU.empty
  , a2l_fsd240_patch                  = VU.empty
  , a2l_fsi24_patch                   = VU.empty
  , a2l_fsi240_patch                  = VU.empty
  , a2l_wind24_patch                  = VU.empty
  , a2l_t_mo_patch                    = VU.empty
  , a2l_t_mo_min_patch                = VU.empty
  }
