{-# LANGUAGE BangPatterns #-}
-- | Infiltration-excess surface runoff.
-- Fortran: InfiltrationExcessRunoffMod.F90
--
-- Provides:
--   * Infiltration excess runoff parameters and configuration
--   * Maximum infiltration rate (qinmax) from hksat
--   * Column-averaged infiltration excess computation
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.InfiltExcessRunoff
  ( -- * Parameters
    InfiltExcessRunoffParams(..)
  , defaultInfiltExcessParams
    -- * Method constants
  , QinmaxMethod(..)
    -- * Input/output records
  , InfiltExcessRunoffInput(..)
  , InfiltExcessRunoffResult(..)
    -- * Science functions
  , computeQinmaxHksat
  , infiltrationExcessRunoff
  ) where

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Effectively infinite max infiltration rate [mm H2O/s]
qinmaxUnlimited :: Double
qinmaxUnlimited = 1.0e200

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Parameters for infiltration excess runoff (from params file)
data InfiltExcessRunoffParams = InfiltExcessRunoffParams
  { ierp_e_ice :: !Double  -- ^ Soil ice impedance factor (unitless)
  } deriving (Show, Eq)

-- | Default parameters
defaultInfiltExcessParams :: InfiltExcessRunoffParams
defaultInfiltExcessParams = InfiltExcessRunoffParams
  { ierp_e_ice = 6.0
  }

-- | Method for computing qinmax
data QinmaxMethod
  = QinmaxNone     -- ^ No infiltration excess (unlimited qinmax)
  | QinmaxHksat    -- ^ CLM default: min over top 3 layers of ice-impeded hksat
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Inputs for infiltration excess runoff computation
data InfiltExcessRunoffInput = InfiltExcessRunoffInput
  { ieri_icefrac      :: !(VU.Vector Double)  -- ^ Ice fraction per layer (nlevgrnd)
  , ieri_hksat        :: !(VU.Vector Double)  -- ^ Saturated hydraulic conductivity (nlevgrnd)
  , ieri_fsat         :: !Double              -- ^ Fractional saturated area
  , ieri_qflx_in_soil :: !Double              -- ^ Water flux into soil [mm/s]
  , ieri_frac_h2osfc  :: !Double              -- ^ Fraction of surface water
  , ieri_method       :: !QinmaxMethod        -- ^ Method selector
  } deriving (Show)

-- | Result of infiltration excess runoff computation
data InfiltExcessRunoffResult = InfiltExcessRunoffResult
  { ierr_qinmax           :: !Double  -- ^ Maximum infiltration rate [mm/s]
  , ierr_qflx_infl_excess :: !Double  -- ^ Infiltration excess runoff [mm/s]
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute qinmax on unsaturated area using hksat parameterization.
-- For each column: min over top 3 layers of 10^(-e_ice * icefrac) * hksat.
-- Ported from ComputeQinmaxHksat in InfiltrationExcessRunoffMod.F90
computeQinmaxHksat
  :: InfiltExcessRunoffParams
  -> VU.Vector Double       -- ^ icefrac (nlevgrnd)
  -> VU.Vector Double       -- ^ hksat (nlevgrnd)
  -> Double                 -- ^ qinmax on unsaturated area
computeQinmaxHksat params icefrac hksat =
  let eIce = ierp_e_ice params
      vals = map (\j ->
        let icf = vix icefrac j
            hks = vix hksat j
        in (10.0 ** (negate eIce * icf)) * hks) [0, 1, 2]
  in minimum vals

-- | Calculate infiltration excess runoff.
-- Sets qinmax (column-averaged) and infiltration excess flux.
-- Ported from InfiltrationExcessRunoff in InfiltrationExcessRunoffMod.F90
infiltrationExcessRunoff
  :: InfiltExcessRunoffParams
  -> InfiltExcessRunoffInput
  -> InfiltExcessRunoffResult
infiltrationExcessRunoff params inp =
  let qinmax_unsaturated = case ieri_method inp of
        QinmaxNone  -> qinmaxUnlimited
        QinmaxHksat -> computeQinmaxHksat params (ieri_icefrac inp) (ieri_hksat inp)

      fsat_val = ieri_fsat inp
      qinmax_col = (1.0 - fsat_val) * qinmax_unsaturated

      qflx_infl_excess = max 0.0
        (ieri_qflx_in_soil inp - (1.0 - ieri_frac_h2osfc inp) * qinmax_col)

  in InfiltExcessRunoffResult
       { ierr_qinmax           = qinmax_col
       , ierr_qflx_infl_excess = qflx_infl_excess
       }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe 0-based vector index
vix :: VU.Vector Double -> Int -> Double
vix v i = v `VU.unsafeIndex` i
