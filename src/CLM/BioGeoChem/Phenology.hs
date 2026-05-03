{-# LANGUAGE BangPatterns #-}
-- | Phenology routines for coupled carbon-nitrogen code (CN).
-- Handles evergreen, seasonal deciduous, stress deciduous, and crop phenology.
-- Fortran: CNPhenologyMod.F90
-- Julia:   src/biogeochem/phenology.jl
--
-- All functions are pure.
module CLM.BioGeoChem.Phenology
  ( -- * Data types
    PhenologyParams(..)
  , defaultPhenologyParams
  , PhenologyState(..)
  , defaultPhenologyState
  , PftConPhenology(..)
    -- * Constants
  , notPlanted
  , notHarvested
  , inNH, inSH
    -- * Initialization
  , phenologyInit
    -- * Seasonal deciduous onset test
  , seasonalDecidOnset
  , seasonalCriticalDaylength
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Phenology parameters.
data PhenologyParams = PhenologyParams
  { pp_crit_dayl              :: !Double
  , pp_crit_dayl_at_high_lat  :: !Double
  , pp_crit_dayl_lat_slope    :: !Double
  , pp_ndays_off              :: !Double
  , pp_fstor2tran             :: !Double
  , pp_crit_onset_fdd         :: !Double
  , pp_crit_onset_swi         :: !Double
  , pp_soilpsi_on             :: !Double
  , pp_crit_offset_fdd        :: !Double
  , pp_crit_offset_swi        :: !Double
  , pp_soilpsi_off            :: !Double
  , pp_lwtop                  :: !Double
  , pp_phenology_soil_depth   :: !Double
  , pp_snow5d_thresh_for_onset :: !Double
  } deriving (Show, Eq)

defaultPhenologyParams :: PhenologyParams
defaultPhenologyParams = PhenologyParams
  { pp_crit_dayl = 39200.0
  , pp_crit_dayl_at_high_lat = 54000.0
  , pp_crit_dayl_lat_slope = 720.0
  , pp_ndays_off = 30.0
  , pp_fstor2tran = 0.5
  , pp_crit_onset_fdd = 15.0
  , pp_crit_onset_swi = 15.0
  , pp_soilpsi_on = -0.6
  , pp_crit_offset_fdd = 15.0
  , pp_crit_offset_swi = 15.0
  , pp_soilpsi_off = -0.8
  , pp_lwtop = 0.7
  , pp_phenology_soil_depth = 0.08
  , pp_snow5d_thresh_for_onset = 0.2
  }

-- | Phenology module state.
data PhenologyState = PhenologyState
  { ps_dt          :: !Double
  , ps_fracday     :: !Double
  , ps_crit_dayl   :: !Double
  , ps_ndays_off   :: !Double
  , ps_fstor2tran  :: !Double
  , ps_crit_onset_fdd :: !Double
  , ps_crit_onset_swi :: !Double
  , ps_soilpsi_on  :: !Double
  , ps_crit_offset_fdd :: !Double
  , ps_crit_offset_swi :: !Double
  , ps_soilpsi_off :: !Double
  , ps_lwtop       :: !Double
  , ps_phenology_soil_layer :: !Int
  } deriving (Show, Eq)

defaultPhenologyState :: PhenologyState
defaultPhenologyState = PhenologyState
  { ps_dt = 0.0, ps_fracday = 0.0, ps_crit_dayl = 0.0
  , ps_ndays_off = 0.0, ps_fstor2tran = 0.0
  , ps_crit_onset_fdd = 0.0, ps_crit_onset_swi = 0.0
  , ps_soilpsi_on = 0.0, ps_crit_offset_fdd = 0.0
  , ps_crit_offset_swi = 0.0, ps_soilpsi_off = 0.0
  , ps_lwtop = 0.0, ps_phenology_soil_layer = 1
  }

-- | PFT constants needed by phenology.
data PftConPhenology = PftConPhenology
  { pfp_evergreen      :: !(VU.Vector Double)
  , pfp_season_decid   :: !(VU.Vector Double)
  , pfp_stress_decid   :: !(VU.Vector Double)
  , pfp_woody          :: !(VU.Vector Double)
  , pfp_leaf_long      :: !(VU.Vector Double)
  , pfp_leafcn         :: !(VU.Vector Double)
  , pfp_frootcn        :: !(VU.Vector Double)
  , pfp_ndays_on       :: !(VU.Vector Double)
  , pfp_crit_onset_gdd_sf :: !(VU.Vector Double)
  } deriving (Show)

-- | Constants
notPlanted :: Int
notPlanted = 999

notHarvested :: Int
notHarvested = 999

inNH :: Int
inNH = 1

inSH :: Int
inSH = 2

-- | Seconds per day
secspday :: Double
secspday = 86400.0

-- | Initialize phenology state from parameters and timestep.
phenologyInit :: PhenologyParams -> Double -> PhenologyState
phenologyInit params dt = PhenologyState
  { ps_dt = dt
  , ps_fracday = dt / secspday
  , ps_crit_dayl = pp_crit_dayl params
  , ps_ndays_off = pp_ndays_off params
  , ps_fstor2tran = pp_fstor2tran params
  , ps_crit_onset_fdd = pp_crit_onset_fdd params
  , ps_crit_onset_swi = pp_crit_onset_swi params
  , ps_soilpsi_on = pp_soilpsi_on params
  , ps_crit_offset_fdd = pp_crit_offset_fdd params
  , ps_crit_offset_swi = pp_crit_offset_swi params
  , ps_soilpsi_off = pp_soilpsi_off params
  , ps_lwtop = pp_lwtop params / (secspday * 365.0)
  , ps_phenology_soil_layer = 1
  }

-- | Test for seasonal deciduous onset based on GDD accumulation.
seasonalDecidOnset :: Double  -- ^ current onset GDD sum
                   -> Double  -- ^ critical onset GDD
                   -> Bool    -- ^ onset flag
                   -> Bool
seasonalDecidOnset !gddSum !critGdd !oldOnset
  | oldOnset  = True
  | gddSum > critGdd = True
  | otherwise = False

-- | Compute critical daylength for seasonal deciduous offset.
seasonalCriticalDaylength :: PhenologyParams -> Double -> Double
seasonalCriticalDaylength params lat =
  let critHighLat = 65.0
  in if abs lat > critHighLat
     then pp_crit_dayl_at_high_lat params
     else pp_crit_dayl params + pp_crit_dayl_lat_slope params * (abs lat - critHighLat)
