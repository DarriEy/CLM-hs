{-# LANGUAGE BangPatterns #-}
-- | Vegetation facade: unified interface to the CN Vegetation subsystem.
--
-- A "facade" provides a higher-level interface that makes the subsystem
-- easier to use. This module coordinates calls to vegetation state,
-- carbon/nitrogen state and flux, products, balance checking, fire,
-- dynamic vegetation, and the CN driver.
--
-- Ported from: CNVegetationFacade.F90
-- Julia:       src/biogeochem/vegetation_facade.jl
--
-- Public types:
--   CNVegetationConfig  -- control flags for the vegetation facade
--   CNVegetationData    -- aggregated state for the CN vegetation subsystem
--
-- Public functions:
--   defaultCNVegetationConfig -- default configuration
--   syncDriverConfig         -- copy facade config to driver config
--   getNetCarbonExchangeGrc  -- gridcell net C exchange
--   getLeafnPatch            -- patch leaf nitrogen
--   getDownregPatch          -- patch N downregulation
--   getRootRespirationPatch  -- patch root respiration
--   getAnnsumNppPatch        -- patch annual sum NPP
--   getAgnppPatch            -- patch aboveground NPP
--   getBgnppPatch            -- patch belowground NPP
--   getFrootCarbonPatch      -- patch fine root carbon (SP fallback)
--   getCrootCarbonPatch      -- patch coarse root carbon (SP fallback)
--   getTotvegcCol            -- column total veg carbon
--
module CLM.BioGeoChem.VegetationFacade
  ( -- * Configuration
    CNVegetationConfig(..)
  , defaultCNVegetationConfig
    -- * Data types
  , CNVegetationData(..)
  , CNDriverConfig(..)
  , defaultCNDriverConfig
    -- * Config sync
  , syncDriverConfig
    -- * Getter functions
  , getNetCarbonExchangeGrc
  , getLeafnPatch
  , getDownregPatch
  , getRootRespirationPatch
  , getAnnsumNppPatch
  , getAgnppPatch
  , getBgnppPatch
  , getFrootCarbonPatch
  , getCrootCarbonPatch
  , getTotvegcCol
  ) where

import qualified Data.Vector.Unboxed as U

-- ========================================================================
-- Configuration
-- ========================================================================

-- | Configuration flags for the CN Vegetation facade.
data CNVegetationConfig = CNVegetationConfig
  { cvc_use_cn                          :: !Bool
  , cvc_use_c13                         :: !Bool
  , cvc_use_c14                         :: !Bool
  , cvc_use_cndv                        :: !Bool
  , cvc_use_fates_bgc                   :: !Bool
  , cvc_use_fun                         :: !Bool
  , cvc_use_crop                        :: !Bool
  , cvc_use_crop_agsys                  :: !Bool
  , cvc_use_nitrif_denitrif             :: !Bool
  , cvc_use_matrixcn                    :: !Bool
  , cvc_use_soil_matrixcn               :: !Bool
  , cvc_reseed_dead_plants              :: !Bool
  , cvc_dribble_crophrv_xsmrpool_2atm  :: !Bool
  , cvc_skip_steps                      :: !Int
  } deriving (Show, Eq)

defaultCNVegetationConfig :: CNVegetationConfig
defaultCNVegetationConfig = CNVegetationConfig
  { cvc_use_cn                          = False
  , cvc_use_c13                         = False
  , cvc_use_c14                         = False
  , cvc_use_cndv                        = False
  , cvc_use_fates_bgc                   = False
  , cvc_use_fun                         = False
  , cvc_use_crop                        = False
  , cvc_use_crop_agsys                  = False
  , cvc_use_nitrif_denitrif             = False
  , cvc_use_matrixcn                    = False
  , cvc_use_soil_matrixcn               = False
  , cvc_reseed_dead_plants              = False
  , cvc_dribble_crophrv_xsmrpool_2atm  = False
  , cvc_skip_steps                      = 0
  }

-- | CN driver configuration (subset of flags passed to cn_driver functions)
data CNDriverConfig = CNDriverConfig
  { cdc_use_cn                          :: !Bool
  , cdc_use_c13                         :: !Bool
  , cdc_use_c14                         :: !Bool
  , cdc_use_fun                         :: !Bool
  , cdc_use_crop                        :: !Bool
  , cdc_use_crop_agsys                  :: !Bool
  , cdc_use_nitrif_denitrif             :: !Bool
  , cdc_use_fates_bgc                   :: !Bool
  , cdc_use_matrixcn                    :: !Bool
  , cdc_use_soil_matrixcn               :: !Bool
  , cdc_dribble_crophrv_xsmrpool_2atm  :: !Bool
  } deriving (Show, Eq)

defaultCNDriverConfig :: CNDriverConfig
defaultCNDriverConfig = CNDriverConfig
  { cdc_use_cn                          = False
  , cdc_use_c13                         = False
  , cdc_use_c14                         = False
  , cdc_use_fun                         = False
  , cdc_use_crop                        = False
  , cdc_use_crop_agsys                  = False
  , cdc_use_nitrif_denitrif             = False
  , cdc_use_fates_bgc                   = False
  , cdc_use_matrixcn                    = False
  , cdc_use_soil_matrixcn               = False
  , cdc_dribble_crophrv_xsmrpool_2atm  = False
  }

-- ========================================================================
-- Aggregated state
-- ========================================================================

-- | Aggregated state for the CN Vegetation subsystem.
-- In Haskell we represent this as a record of typed vectors for each
-- field category. The actual state vectors (carbon state, flux, nitrogen
-- state, flux) are referenced by the caller -- this type carries the
-- config and driver config.
data CNVegetationData = CNVegetationData
  { cvd_config        :: !CNVegetationConfig
  , cvd_driver_config :: !CNDriverConfig
  } deriving (Show, Eq)

-- ========================================================================
-- Config sync
-- ========================================================================

-- | Synchronize driver config from facade config. Pure function.
syncDriverConfig :: CNVegetationConfig -> CNDriverConfig
syncDriverConfig fc = CNDriverConfig
  { cdc_use_cn                         = cvc_use_cn fc
  , cdc_use_c13                        = cvc_use_c13 fc
  , cdc_use_c14                        = cvc_use_c14 fc
  , cdc_use_fun                        = cvc_use_fun fc
  , cdc_use_crop                       = cvc_use_crop fc
  , cdc_use_crop_agsys                 = cvc_use_crop_agsys fc
  , cdc_use_nitrif_denitrif            = cvc_use_nitrif_denitrif fc
  , cdc_use_fates_bgc                  = cvc_use_fates_bgc fc
  , cdc_use_matrixcn                   = cvc_use_matrixcn fc
  , cdc_use_soil_matrixcn              = cvc_use_soil_matrixcn fc
  , cdc_dribble_crophrv_xsmrpool_2atm = cvc_dribble_crophrv_xsmrpool_2atm fc
  }

-- ========================================================================
-- Getter functions (pure)
-- ========================================================================

-- | Get gridcell-level net carbon exchange (positive = source).
-- Returns -nbp_grc when use_cn, otherwise zeros.
getNetCarbonExchangeGrc
  :: Bool                   -- use_cn
  -> U.Vector Double        -- nbp_grc
  -> U.Vector Double
getNetCarbonExchangeGrc useCN nbpGrc
  | useCN     = U.map negate nbpGrc
  | otherwise = U.replicate (U.length nbpGrc) 0.0

-- | Get patch-level leaf nitrogen array [gN/m2].
getLeafnPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getLeafnPatch useCN leafnPatch
  | useCN     = leafnPatch
  | otherwise = U.replicate (U.length leafnPatch) (0.0 / 0.0)  -- NaN

-- | Get patch-level N downregulation factor.
getDownregPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getDownregPatch useCN downregPatch
  | useCN     = downregPatch
  | otherwise = U.replicate (U.length downregPatch) (0.0 / 0.0)

-- | Get patch-level root respiration [gC/m2/s].
getRootRespirationPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getRootRespirationPatch useCN rrPatch
  | useCN     = rrPatch
  | otherwise = U.replicate (U.length rrPatch) (0.0 / 0.0)

-- | Get patch-level annual sum NPP [gC/m2/yr].
getAnnsumNppPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getAnnsumNppPatch useCN annsumNppPatch
  | useCN     = annsumNppPatch
  | otherwise = U.replicate (U.length annsumNppPatch) (0.0 / 0.0)

-- | Get patch-level aboveground NPP [gC/m2/s].
getAgnppPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getAgnppPatch useCN agnppPatch
  | useCN     = agnppPatch
  | otherwise = U.replicate (U.length agnppPatch) (0.0 / 0.0)

-- | Get patch-level belowground NPP [gC/m2/s].
getBgnppPatch
  :: Bool -> U.Vector Double -> U.Vector Double
getBgnppPatch useCN bgnppPatch
  | useCN     = bgnppPatch
  | otherwise = U.replicate (U.length bgnppPatch) (0.0 / 0.0)

-- | Get patch-level fine root carbon [gC/m2].
-- When use_cn, returns frootc_patch directly.
-- Otherwise estimates from LAI and PFT traits:
--   froot_c = (LAI / slatop) * froot_leaf
getFrootCarbonPatch
  :: Bool                   -- use_cn
  -> U.Vector Double        -- frootc_patch (CN mode)
  -> U.Vector Double        -- tlai
  -> U.Vector Double        -- slatop (indexed by pft)
  -> U.Vector Double        -- froot_leaf (indexed by pft)
  -> U.Vector Int            -- ivt (pft index per patch, 1-based into slatop)
  -> U.Vector Double
getFrootCarbonPatch useCN frootc tlai slatop frootLeaf ivt
  | useCN     = frootc
  | otherwise = U.imap estimate tlai
  where
    estimate i lai =
      let pft = ivt U.! i
          sl  = slatop U.! pft
          fl  = frootLeaf U.! pft
      in if sl > 0.0 then lai / sl * fl else 0.0

-- | Get patch-level live coarse root carbon [gC/m2].
-- When use_cn, returns livecrootc_patch directly.
-- Otherwise estimates: croot_c = (LAI / slatop) * stem_leaf * croot_stem
getCrootCarbonPatch
  :: Bool
  -> U.Vector Double        -- livecrootc_patch
  -> U.Vector Double        -- tlai
  -> U.Vector Double        -- slatop
  -> U.Vector Double        -- stem_leaf
  -> U.Vector Double        -- croot_stem
  -> U.Vector Int            -- ivt
  -> U.Vector Double
getCrootCarbonPatch useCN livecrootc tlai slatop stemLeaf crootStem ivt
  | useCN     = livecrootc
  | otherwise = U.imap estimate tlai
  where
    estimate i lai =
      let pft = ivt U.! i
          sl  = slatop U.! pft
          stl = stemLeaf U.! pft
          crs = crootStem U.! pft
      in if sl > 0.0 then lai / sl * stl * crs else 0.0

-- | Get column-level total vegetation carbon [gC/m2].
getTotvegcCol
  :: Bool -> U.Vector Double -> U.Vector Double
getTotvegcCol useCN totvegcCol
  | useCN     = totvegcCol
  | otherwise = U.replicate (U.length totvegcCol) (0.0 / 0.0)
