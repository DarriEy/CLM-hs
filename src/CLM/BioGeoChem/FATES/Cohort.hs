{-# LANGUAGE BangPatterns #-}
-- | FATES Cohort Data Structures and Dynamics
-- Ported from FatesCohortMod.F90
module CLM.BioGeoChem.FATES.Cohort
  ( FATESCohort(..)
  , defaultFATESCohort
  , cohortCreate
  , cohortCopy
  , cohortCanUpperUnder
  ) where

import CLM.BioGeoChem.FATES.Constants

-- | Represents a plant cohort in FATES.
-- Corresponds to fates_cohort_type in FatesCohortMod.F90.
data FATESCohort = FATESCohort
  { cohPft                     :: !Int       -- ^ PFT index
  , cohN                       :: !Double    -- ^ Number of individuals per unit area [/m2]
  , cohDbh                     :: !Double    -- ^ Diameter at breast height [cm]
  , cohCoage                  :: !Double    -- ^ Cohort age [years]
  , cohHeight                  :: !Double    -- ^ Height [m]
  , cohIndexNumber             :: !Int       -- ^ Unique cohort index
  , cohCanopyLayer            :: !Int       -- ^ Canopy layer status (1=canopy, 2=understorey)
  , cohCanopyLayerYesterday   :: !Double    -- ^ Canopy status from previous timestep
  , cohCrownDamage            :: !Int       -- ^ Crown damage class
  , cohC_area                  :: !Double    -- ^ Canopy area of the cohort [m2]
  , cohTreeLai                 :: !Double    -- ^ LAI of individual [m2 leaf/m2 crown]
  , cohTreeSai                 :: !Double    -- ^ SAI of individual [m2 stem/m2 crown]
  , cohIsNew                   :: !Bool      -- ^ Flag indicating a newly established cohort
  , cohSizeClass              :: !Int       -- ^ Size class bin index
  , cohCoageClass             :: !Int       -- ^ Cohort age class bin index
  , cohSizeByPftClass         :: !Int       -- ^ Size x PFT class bin index
  , cohCoageByPftClass        :: !Int       -- ^ Cohort age x PFT class bin index
  , cohSizeClassLastTimestep  :: !Int       -- ^ Size class from previous timestep
  -- PRT state variables represented as flat fields for performance
  , cohLeafC                   :: !Double    -- ^ Leaf carbon biomass [kgC/individual]
  , cohSapwC                   :: !Double    -- ^ Sapwood carbon biomass [kgC/individual]
  , cohStructC                 :: !Double    -- ^ Structural carbon biomass [kgC/individual]
  } deriving (Show, Eq)

-- | Construct a default cohort initialized with unset or zero values.
defaultFATESCohort :: FATESCohort
defaultFATESCohort = FATESCohort
  { cohPft                     = 0
  , cohN                       = 0.0
  , cohDbh                     = 0.0
  , cohCoage                  = 0.0
  , cohHeight                  = 0.0
  , cohIndexNumber             = 0
  , cohCanopyLayer            = 2
  , cohCanopyLayerYesterday   = 2.0
  , cohCrownDamage            = 1
  , cohC_area                  = 0.0
  , cohTreeLai                 = 0.0
  , cohTreeSai                 = 0.0
  , cohIsNew                   = True
  , cohSizeClass              = 1
  , cohCoageClass             = 1
  , cohSizeByPftClass         = 1
  , cohCoageByPftClass        = 1
  , cohSizeClassLastTimestep  = 1
  , cohLeafC                   = 0.0
  , cohSapwC                   = 0.0
  , cohStructC                 = 0.0
  }

-- | Initialize a new cohort.
-- Maps to Create in FatesCohortMod.F90.
cohortCreate
  :: Int       -- ^ PFT index
  -> Double    -- ^ Number of individuals per unit area [/m2]
  -> Double    -- ^ Height [m]
  -> Double    -- ^ Cohort age [years]
  -> Double    -- ^ Diameter [cm]
  -> Int       -- ^ Canopy layer status (1=canopy, 2=understorey)
  -> Int       -- ^ Crown damage class
  -> Double    -- ^ Canopy area of the cohort [m2]
  -> FATESCohort
cohortCreate !pft !nn !height !coage !dbh !clayer !crowndamage !carea =
  defaultFATESCohort
    { cohPft                   = pft
    , cohN                     = nn
    , cohHeight                = height
    , cohCoage                 = coage
    , cohDbh                   = dbh
    , cohCanopyLayer           = clayer
    , cohCanopyLayerYesterday  = fromIntegral clayer
    , cohCrownDamage           = crowndamage
    , cohC_area                = carea
    , cohIsNew                 = True
    }

-- | Copy cohort attributes into a new cohort structure.
-- Maps to Copy in FatesCohortMod.F90.
cohortCopy :: FATESCohort -> FATESCohort
cohortCopy !coh = coh

-- | Determine canopy crown position.
-- Maps to CanUpperUnder in FatesCohortMod.F90.
cohortCanUpperUnder :: FATESCohort -> Int
cohortCanUpperUnder !coh =
  if cohCanopyLayer coh == 1
    then icanUpper
    else icanUstory
