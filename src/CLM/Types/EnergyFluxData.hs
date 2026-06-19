-- | Energy flux variables.
-- Fortran: EnergyFluxType
module CLM.Types.EnergyFluxData
  ( EnergyFluxData(..)
  , defaultEnergyFluxData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column/patch-level energy fluxes.
data EnergyFluxData = EnergyFluxData
  { eflx_sh_tot_patch   :: !Double  -- ^ Total sensible heat flux [W/m²]
  , eflx_lh_tot_patch   :: !Double  -- ^ Total latent heat flux [W/m²]
  , eflx_sh_grnd_patch  :: !Double  -- ^ Ground sensible heat flux [W/m²]
  , eflx_soil_grnd_col  :: !Double  -- ^ Ground heat flux into soil [W/m²]
  , sabv_patch           :: !Double  -- ^ Solar absorbed by vegetation [W/m²]
  , sabg_patch           :: !Double  -- ^ Solar absorbed by ground [W/m²]
  , fsa_patch            :: !Double  -- ^ Total absorbed solar radiation [W/m²]
  , cgrnds_patch         :: !Double  -- ^ Ground sensible conductance [W/m²/K]
  , cgrndl_patch         :: !Double  -- ^ Ground latent conductance [kg/m²/s/K]
  , cgrnd_patch          :: !Double  -- ^ Combined ground heat conductance [W/m²/K]
  , dlrad_patch          :: !Double  -- ^ Downward longwave below canopy [W/m²]
  , ulrad_patch          :: !Double  -- ^ Upward longwave above canopy [W/m²]
  , eflx_lwrad_out_patch :: !Double  -- ^ Outgoing longwave radiation [W/m²]
  , eflx_lwrad_net_patch :: !Double  -- ^ Net longwave radiation [W/m²]
  , eflx_sh_tot_patch_vec  :: !(VU.Vector Double)  -- ^ Patch total sensible heat fluxes [W/m²]
  , eflx_lh_tot_patch_vec  :: !(VU.Vector Double)  -- ^ Patch latent heat fluxes [W/m²]
  , eflx_sh_grnd_patch_vec :: !(VU.Vector Double)  -- ^ Patch ground sensible heat fluxes [W/m²]
  , eflx_gnet_patch_vec    :: !(VU.Vector Double)  -- ^ Patch net ground heat flux (EFLX_GNET) [W/m²]
  , sabv_patch_vec         :: !(VU.Vector Double)  -- ^ Patch solar absorbed by vegetation [W/m²]
  , sabg_patch_vec         :: !(VU.Vector Double)  -- ^ Patch solar absorbed by ground [W/m²]
  , fsa_patch_vec          :: !(VU.Vector Double)  -- ^ Patch total absorbed solar radiation [W/m²]
  , cgrnds_patch_vec       :: !(VU.Vector Double)  -- ^ Patch ground sensible conductances [W/m²/K]
  , cgrndl_patch_vec       :: !(VU.Vector Double)  -- ^ Patch ground latent conductances [kg/m²/s/K]
  , cgrnd_patch_vec        :: !(VU.Vector Double)  -- ^ Patch combined ground conductances [W/m²/K]
  , dlrad_patch_vec        :: !(VU.Vector Double)  -- ^ Patch downward canopy longwave [W/m²]
  , ulrad_patch_vec        :: !(VU.Vector Double)  -- ^ Patch upward canopy longwave [W/m²]
  , eflx_lwrad_out_patch_vec :: !(VU.Vector Double) -- ^ Patch outgoing longwave [W/m²]
  , eflx_lwrad_net_patch_vec :: !(VU.Vector Double) -- ^ Patch net longwave [W/m²]
  } deriving (Show)

defaultEnergyFluxData :: EnergyFluxData
defaultEnergyFluxData = EnergyFluxData
  { eflx_sh_tot_patch  = 0.0
  , eflx_lh_tot_patch  = 0.0
  , eflx_sh_grnd_patch = 0.0
  , eflx_soil_grnd_col = 0.0
  , sabv_patch          = 0.0
  , sabg_patch          = 0.0
  , fsa_patch           = 0.0
  , cgrnds_patch        = 0.0
  , cgrndl_patch        = 0.0
  , cgrnd_patch         = 0.0
  , dlrad_patch         = 0.0
  , ulrad_patch         = 0.0
  , eflx_lwrad_out_patch = 0.0
  , eflx_lwrad_net_patch = 0.0
  , eflx_sh_tot_patch_vec  = VU.empty
  , eflx_lh_tot_patch_vec  = VU.empty
  , eflx_sh_grnd_patch_vec = VU.empty
  , eflx_gnet_patch_vec    = VU.empty
  , sabv_patch_vec         = VU.empty
  , sabg_patch_vec         = VU.empty
  , fsa_patch_vec          = VU.empty
  , cgrnds_patch_vec       = VU.empty
  , cgrndl_patch_vec       = VU.empty
  , cgrnd_patch_vec        = VU.empty
  , dlrad_patch_vec        = VU.empty
  , ulrad_patch_vec        = VU.empty
  , eflx_lwrad_out_patch_vec = VU.empty
  , eflx_lwrad_net_patch_vec = VU.empty
  }
