-- | LUNA model parameters.
-- Fortran: luna params_type — CO2 compensation, Michaelis-Menten, curvature params.
module CLM.Types.LunaParamsData
  ( LunaParamsData(..)
  , defaultLunaParamsData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | LUNA model parameters (SoA layout).
-- Preserves Fortran variable names with @luna_@ prefix.
data LunaParamsData = LunaParamsData
  { luna_cp25_yr2000          :: !Double            -- ^ CO2 compensation point at 25C [mol/mol]
  , luna_kc25_coef            :: !Double            -- ^ Michaelis-Menten const at 25C for CO2
  , luna_ko25_coef            :: !Double            -- ^ Michaelis-Menten const at 25C for O2
  , luna_theta_cj             :: !Double            -- ^ Empirical curvature parameter for ac,aj co-limitation
  , luna_enzyme_turnover_daily :: !Double           -- ^ Daily turnover rate for photosynthetic enzyme at 25C
  , luna_relhExp              :: !Double            -- ^ Impact of relative humidity on electron transport rate
  , luna_minrelh              :: !Double            -- ^ Minimum relative humidity for N optimization [fraction]
  , luna_jmaxb0               :: !(VU.Vector Double) -- ^ Baseline proportion of N for electron transport
  , luna_jmaxb1               :: !(VU.Vector Double) -- ^ Coefficient for electron transport rate response to light
  , luna_wc2wjb0              :: !(VU.Vector Double) -- ^ Baseline ratio of rubisco vs light limited rate
  } deriving (Show)

defaultLunaParamsData :: LunaParamsData
defaultLunaParamsData = LunaParamsData
  { luna_cp25_yr2000          = 0/0  -- NaN
  , luna_kc25_coef            = 0/0
  , luna_ko25_coef            = 0/0
  , luna_theta_cj             = 0/0
  , luna_enzyme_turnover_daily = 0/0
  , luna_relhExp              = 0/0
  , luna_minrelh              = 0/0
  , luna_jmaxb0               = VU.empty
  , luna_jmaxb1               = VU.empty
  , luna_wc2wjb0              = VU.empty
  }
