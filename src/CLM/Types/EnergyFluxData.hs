-- | Energy flux variables.
-- Fortran: EnergyFluxType
module CLM.Types.EnergyFluxData
  ( EnergyFluxData(..)
  , defaultEnergyFluxData
  ) where

-- | Column/patch-level energy fluxes.
data EnergyFluxData = EnergyFluxData
  { eflx_sh_tot_patch   :: !Double  -- ^ Total sensible heat flux [W/m²]
  , eflx_lh_tot_patch   :: !Double  -- ^ Total latent heat flux [W/m²]
  , eflx_sh_grnd_patch  :: !Double  -- ^ Ground sensible heat flux [W/m²]
  , eflx_soil_grnd_col  :: !Double  -- ^ Ground heat flux into soil [W/m²]
  , sabv_patch           :: !Double  -- ^ Solar absorbed by vegetation [W/m²]
  , sabg_patch           :: !Double  -- ^ Solar absorbed by ground [W/m²]
  , fsa_patch            :: !Double  -- ^ Total absorbed solar radiation [W/m²]
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
  }
