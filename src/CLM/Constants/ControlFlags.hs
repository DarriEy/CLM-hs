-- | Control flags and configuration for CLM driver.
module CLM.Constants.ControlFlags
  ( -- * BGC mode
    BGCMode(..)
    -- * Stomatal conductance method
  , StomatalCondMethod(..)
    -- * Driver configuration
  , CLMDriverConfig(..)
  , defaultDriverConfig
  ) where

-- | Biogeochemistry mode
data BGCMode
  = SPMode     -- ^ Satellite phenology (prescribed LAI)
  | CNMode     -- ^ Carbon-nitrogen biogeochemistry
  | FATESMode  -- ^ FATES ecosystem demography
  deriving (Show, Eq)

-- | Stomatal conductance method
data StomatalCondMethod
  = BallBerry1987  -- ^ Ball-Berry 1987 (uses relative humidity)
  | Medlyn2011     -- ^ Medlyn 2011 (uses VPD) — CLM5 default
  deriving (Show, Eq)

-- | Main driver configuration record
data CLMDriverConfig = CLMDriverConfig
  { bgcMode            :: !BGCMode
  , stomatalCondMethod  :: !StomatalCondMethod
  , useAquiferLayer    :: !Bool
  , useHydrstress      :: !Bool
  , useClm5Fpi         :: !Bool
  , useLuna            :: !Bool
  , windDependentSnow  :: !Bool
  } deriving (Show, Eq)

-- | Default configuration matching CLM5 defaults
defaultDriverConfig :: CLMDriverConfig
defaultDriverConfig = CLMDriverConfig
  { bgcMode            = SPMode
  , stomatalCondMethod  = Medlyn2011
  , useAquiferLayer    = False
  , useHydrstress      = False
  , useClm5Fpi         = True
  , useLuna            = False
  , windDependentSnow  = True
  }
