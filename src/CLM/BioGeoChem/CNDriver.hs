{-# LANGUAGE BangPatterns #-}
-- | CN driver: ecosystem dynamics orchestrator.
-- Fortran: CNDriverMod.F90
-- Julia:   src/biogeochem/cn_driver.jl
--
-- Contains the CNDriverConfig type that holds all configuration flags
-- for the CN driver, plus decomposition method constants.
module CLM.BioGeoChem.CNDriver
  ( -- * Configuration
    CNDriverConfig(..)
  , defaultCNDriverConfig
    -- * Decomposition method constants
  , centuryDecomp
  , mimicsDecomp
  ) where

-- | Configuration flags for the CN driver module.
-- Ported from module-level @use@ statements in @CNDriverMod.F90@.
data CNDriverConfig = CNDriverConfig
  { cndc_use_c13                       :: !Bool
  , cndc_use_c14                       :: !Bool
  , cndc_use_cn                        :: !Bool
  , cndc_use_fun                       :: !Bool
  , cndc_use_crop                      :: !Bool
  , cndc_use_crop_agsys                :: !Bool
  , cndc_use_nitrif_denitrif           :: !Bool
  , cndc_use_nguardrail                :: !Bool
  , cndc_use_fates                     :: !Bool
  , cndc_use_fates_bgc                 :: !Bool
  , cndc_use_matrixcn                  :: !Bool
  , cndc_use_soil_matrixcn             :: !Bool
  , cndc_decomp_method                 :: !Int   -- ^ 1=century_decomp, 2=mimics_decomp
  , cndc_dribble_crophrv_xsmrpool_2atm :: !Bool
  , cndc_do_harvest                    :: !Bool
  , cndc_do_grossunrep                 :: !Bool
  } deriving (Show)

defaultCNDriverConfig :: CNDriverConfig
defaultCNDriverConfig = CNDriverConfig
  { cndc_use_c13 = False
  , cndc_use_c14 = False
  , cndc_use_cn = False
  , cndc_use_fun = False
  , cndc_use_crop = False
  , cndc_use_crop_agsys = False
  , cndc_use_nitrif_denitrif = False
  , cndc_use_nguardrail = False
  , cndc_use_fates = False
  , cndc_use_fates_bgc = False
  , cndc_use_matrixcn = False
  , cndc_use_soil_matrixcn = False
  , cndc_decomp_method = 1
  , cndc_dribble_crophrv_xsmrpool_2atm = False
  , cndc_do_harvest = False
  , cndc_do_grossunrep = False
  }

-- | Century decomposition method constant.
centuryDecomp :: Int
centuryDecomp = 1

-- | MIMICS decomposition method constant.
mimicsDecomp :: Int
mimicsDecomp = 2
