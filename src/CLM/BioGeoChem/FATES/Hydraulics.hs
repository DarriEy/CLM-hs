{-# LANGUAGE BangPatterns #-}
-- | FATES Plant Hydraulics and Soil-Plant Water Flow
-- Ported from FatesPlantHydraulicsMod.F90 & FatesHydraulicsMemMod.F90
module CLM.BioGeoChem.FATES.Hydraulics
  ( FATESCohortHydr(..)
  , defaultFATESCohortHydr
  , hydraulicsDriveBypass
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Represents plant hydraulics state for a cohort.
-- Corresponds to ed_cohort_hydr_type in FatesHydraulicsMemMod.F90.
data FATESCohortHydr = FATESCohortHydr
  { hydrBtran            :: !Double         -- ^ leaf water potential limitation on gs [0-1]
  , hydrQtop             :: !Double         -- ^ mean transpiration flux rate [kg/cohort/s]
  , hydrErrH2o           :: !Double         -- ^ total water balance error per unit crown area [kgh2o/m2]
  , hydrIsNewlyRecruited :: !Bool           -- ^ whether the cohort is newly recruited
  , hydrV_troot          :: !Double         -- ^ transporting root volume [m3]
  , hydrTh_troot         :: !Double         -- ^ transporting root water content [kgh2o/indiv]
  , hydrPsi_troot        :: !Double         -- ^ transporting root water potential [MPa]
  } deriving (Show, Eq)

-- | Construct a default cohort hydraulics state.
defaultFATESCohortHydr :: FATESCohortHydr
defaultFATESCohortHydr = FATESCohortHydr
  { hydrBtran            = 1.0
  , hydrQtop             = 0.0
  , hydrErrH2o           = 0.0
  , hydrIsNewlyRecruited = False
  , hydrV_troot          = 0.0
  , hydrTh_troot         = 0.0
  , hydrPsi_troot        = 0.0
  }

-- | Main FATES plant hydraulics driver bypass step.
-- Maps to hydraulics_drive in FatesPlantHydraulicsMod.F90.
hydraulicsDriveBypass :: Double -> Double
hydraulicsDriveBypass !dtime = dtime
