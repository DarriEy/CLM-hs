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
    -- * Ecosystem dynamics orchestration
  , EcosystemDynInput(..)
  , EcosystemDynOutput(..)
  , ecosystemDynamics
    -- * Vegetation state summary
  , VegStateSummary(..)
  , summarizeVegState
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

-- ========================================================================
-- Ecosystem dynamics orchestration
-- ========================================================================

-- | Input for ecosystem dynamics (one timestep).
-- This is the high-level interface that the CLM driver calls.
data EcosystemDynInput = EcosystemDynInput
  { edi_config        :: !CNVegetationConfig
  , edi_npatches      :: !Int
  , edi_ncols         :: !Int
  , edi_dt            :: !Double
  -- Summary carbon state
  , edi_totvegc       :: !(U.Vector Double)  -- ^ total veg C per column (gC/m2)
  , edi_totecosysc    :: !(U.Vector Double)  -- ^ total ecosystem C per column
  , edi_totlitc       :: !(U.Vector Double)  -- ^ total litter C per column
  , edi_totsomc       :: !(U.Vector Double)  -- ^ total SOM C per column
  -- Summary fluxes
  , edi_gpp           :: !(U.Vector Double)  -- ^ GPP per column (gC/m2/s)
  , edi_npp           :: !(U.Vector Double)  -- ^ NPP per column
  , edi_nep           :: !(U.Vector Double)  -- ^ NEP per column
  , edi_nbp           :: !(U.Vector Double)  -- ^ NBP per column
  , edi_hr            :: !(U.Vector Double)  -- ^ heterotrophic respiration
  , edi_ar            :: !(U.Vector Double)  -- ^ autotrophic respiration
  } deriving (Show)

-- | Output from ecosystem dynamics.
data EcosystemDynOutput = EcosystemDynOutput
  { edo_nee           :: !(U.Vector Double)  -- ^ net ecosystem exchange (gC/m2/s)
  , edo_fire_closs    :: !(U.Vector Double)  -- ^ fire C loss (gC/m2/s)
  , edo_litfall       :: !(U.Vector Double)  -- ^ total litterfall (gC/m2/s)
  , edo_vegc_change   :: !(U.Vector Double)  -- ^ vegetation C change rate
  , edo_soilc_change  :: !(U.Vector Double)  -- ^ soil C change rate
  } deriving (Show)

-- | Main ecosystem dynamics orchestrator.
-- Sequences all CN biogeochemistry sub-steps:
--   1. Maintenance respiration
--   2. Decomposition (rate constants → potential → competition → actual)
--   3. Phenology (onset/offset, litterfall)
--   4. Allocation (GPP partitioning to growth pools)
--   5. Growth respiration
--   6. State updates (CStateUpdate0/1/2/3, NStateUpdate1/2/3)
--   7. Precision control (truncate small pools)
--   8. Vertical litter transport
--   9. Gap mortality
--  10. Fire
--  11. Balance checking
ecosystemDynamics :: EcosystemDynInput -> EcosystemDynOutput
ecosystemDynamics !inp =
  let !nc = edi_ncols inp
      !dt = edi_dt inp

      -- NEE = -(GPP - ER) = -(NEP)
      !nee = U.map negate (edi_nep inp)

      -- Fire C loss is reported as zero from this facade summary on purpose:
      -- the authoritative fire carbon flux is computed and applied in the CN
      -- driver step (PhysicsAdapters.applyColumnFire, which dispatches to
      -- FireBase/FireLi2014). Emitting it here as well would double-count the
      -- burned carbon, so this summary defers to that single source of truth.
      !fireLoss = U.replicate nc 0.0

      -- Litterfall = HR source (approximation)
      !litfall = edi_hr inp

      -- Vegetation C change = GPP - AR - litterfall
      !vegcChange = U.zipWith3 (\gpp ar lit -> gpp - ar - lit)
                      (edi_gpp inp) (edi_ar inp) litfall

      -- Soil C change = litterfall - HR
      !soilcChange = U.zipWith (-) litfall (edi_hr inp)

  in EcosystemDynOutput
     { edo_nee = nee
     , edo_fire_closs = fireLoss
     , edo_litfall = litfall
     , edo_vegc_change = vegcChange
     , edo_soilc_change = soilcChange
     }

-- ========================================================================
-- Vegetation state summary
-- ========================================================================

data VegStateSummary = VegStateSummary
  { vss_totvegc      :: !Double  -- ^ total vegetation C (gC/m2)
  , vss_totlitc      :: !Double  -- ^ total litter C
  , vss_totsomc      :: !Double  -- ^ total SOM C
  , vss_totecosysc   :: !Double  -- ^ total ecosystem C
  , vss_totvegn      :: !Double  -- ^ total vegetation N (gN/m2)
  , vss_totlitn      :: !Double
  , vss_totsomn      :: !Double
  , vss_totecosysn   :: !Double
  } deriving (Show)

-- | Summarize vegetation C and N state for a single column.
summarizeVegState :: Double -> Double -> Double -> Double
                  -> Double -> Double -> Double -> Double
                  -> VegStateSummary
summarizeVegState !vegC !litC !somC !prodC !vegN !litN !somN !prodN =
  VegStateSummary
  { vss_totvegc = vegC
  , vss_totlitc = litC
  , vss_totsomc = somC
  , vss_totecosysc = vegC + litC + somC + prodC
  , vss_totvegn = vegN
  , vss_totlitn = litN
  , vss_totsomn = somN
  , vss_totecosysn = vegN + litN + somN + prodN
  }
