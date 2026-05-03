-- | Land-to-atmosphere data structures.
-- Fortran: lnd2atmType.F90 — gridcell-level flux fields sent to the atmosphere.
module CLM.Types.Lnd2AtmData
  ( Lnd2AtmData(..)
  , defaultLnd2AtmData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Land-to-atmosphere data (SoA layout).
-- Preserves Fortran variable names with @l2a_@ prefix.
data Lnd2AtmData = Lnd2AtmData
  { l2a_t_rad_grc                     :: !(VU.Vector Double) -- ^ Radiative temperature [K]
  , l2a_t_ref2m_grc                   :: !(VU.Vector Double) -- ^ 2m surface air temperature [K]
  , l2a_u_ref10m_grc                  :: !(VU.Vector Double) -- ^ 10m surface wind speed [m/s]
  , l2a_albd_grc                      :: !(VU.Vector Double) -- ^ Surface albedo direct (ng * numrad, flattened)
  , l2a_albi_grc                      :: !(VU.Vector Double) -- ^ Surface albedo diffuse (ng * numrad, flattened)
  , l2a_taux_grc                      :: !(VU.Vector Double) -- ^ Wind stress e-w [kg/m/s^2]
  , l2a_tauy_grc                      :: !(VU.Vector Double) -- ^ Wind stress n-s [kg/m/s^2]
  , l2a_eflx_lh_tot_grc              :: !(VU.Vector Double) -- ^ Total latent heat flux [W/m^2]
  , l2a_eflx_sh_tot_grc              :: !(VU.Vector Double) -- ^ Total sensible heat flux [W/m^2]
  , l2a_eflx_sh_precip_conversion_grc :: !(VU.Vector Double) -- ^ Sensible HF from precip conversion [W/m^2]
  , l2a_eflx_lwrad_out_grc           :: !(VU.Vector Double) -- ^ IR longwave radiation [W/m^2]
  , l2a_fsa_grc                       :: !(VU.Vector Double) -- ^ Solar radiation absorbed [W/m^2]
  , l2a_z0m_grc                       :: !(VU.Vector Double) -- ^ Roughness length, momentum [m]
  , l2a_net_carbon_exchange_grc       :: !(VU.Vector Double) -- ^ Net CO2 flux [kg CO2/m^2/s]
  , l2a_nem_grc                       :: !(VU.Vector Double) -- ^ Net methane correction to CO2 [g C/m^2/s]
  , l2a_ram1_grc                      :: !(VU.Vector Double) -- ^ Aerodynamical resistance [s/m]
  , l2a_fv_grc                        :: !(VU.Vector Double) -- ^ Friction velocity [m/s]
  , l2a_flxdst_grc                    :: !(VU.Vector Double) -- ^ Dust flux (ng * ndst, flattened)
  , l2a_ddvel_grc                     :: !(VU.Vector Double) -- ^ Dry deposition velocities (ng * n_drydep, flattened)
  , l2a_flxvoc_grc                    :: !(VU.Vector Double) -- ^ VOC flux (ng * n_megan_comps, flattened)
  , l2a_fireflx_grc                   :: !(VU.Vector Double) -- ^ Wild fire emissions (ng * n_fire_emis, flattened)
  , l2a_fireztop_grc                  :: !(VU.Vector Double) -- ^ Wild fire emissions vert dist top
  , l2a_ch4_surf_flux_tot_grc        :: !(VU.Vector Double) -- ^ Net CH4 flux [kg C/m^2/s]
  , l2a_eflx_sh_ice_to_liq_col       :: !(VU.Vector Double) -- ^ Sensible HF from ice-to-liquid [W/m^2]
  } deriving (Show)

defaultLnd2AtmData :: Lnd2AtmData
defaultLnd2AtmData = Lnd2AtmData
  { l2a_t_rad_grc                     = VU.empty
  , l2a_t_ref2m_grc                   = VU.empty
  , l2a_u_ref10m_grc                  = VU.empty
  , l2a_albd_grc                      = VU.empty
  , l2a_albi_grc                      = VU.empty
  , l2a_taux_grc                      = VU.empty
  , l2a_tauy_grc                      = VU.empty
  , l2a_eflx_lh_tot_grc              = VU.empty
  , l2a_eflx_sh_tot_grc              = VU.empty
  , l2a_eflx_sh_precip_conversion_grc = VU.empty
  , l2a_eflx_lwrad_out_grc           = VU.empty
  , l2a_fsa_grc                       = VU.empty
  , l2a_z0m_grc                       = VU.empty
  , l2a_net_carbon_exchange_grc       = VU.empty
  , l2a_nem_grc                       = VU.empty
  , l2a_ram1_grc                      = VU.empty
  , l2a_fv_grc                        = VU.empty
  , l2a_flxdst_grc                    = VU.empty
  , l2a_ddvel_grc                     = VU.empty
  , l2a_flxvoc_grc                    = VU.empty
  , l2a_fireflx_grc                   = VU.empty
  , l2a_fireztop_grc                  = VU.empty
  , l2a_ch4_surf_flux_tot_grc        = VU.empty
  , l2a_eflx_sh_ice_to_liq_col       = VU.empty
  }
