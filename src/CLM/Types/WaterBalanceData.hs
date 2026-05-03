-- | Water balance data structures.
-- Fortran: WaterBalanceType.F90 — water balance variables for bulk water and tracers.
module CLM.Types.WaterBalanceData
  ( WaterBalanceData(..)
  , defaultWaterBalanceData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Column, patch, and gridcell level water balance (SoA layout).
-- Preserves Fortran variable names with @wb_@ prefix.
data WaterBalanceData = WaterBalanceData
  { -- Column-level
    wb_h2osno_old_col                    :: !(VU.Vector Double) -- ^ Snow mass for previous timestep [kg/m^2]
  , wb_snow_sources_col                  :: !(VU.Vector Double) -- ^ Snow sources [mm H2O/s]
  , wb_snow_sinks_col                    :: !(VU.Vector Double) -- ^ Snow sinks [mm H2O/s]
  , wb_wa_reset_nonconservation_gain_col :: !(VU.Vector Double) -- ^ Mass gained from resetting wa [mm]
  , wb_begwb_col                         :: !(VU.Vector Double) -- ^ Water mass beginning of timestep
  , wb_endwb_col                         :: !(VU.Vector Double) -- ^ Water mass end of timestep
  , wb_errh2o_col                        :: !(VU.Vector Double) -- ^ Water conservation error [mm H2O]
  , wb_errh2osno_col                     :: !(VU.Vector Double) -- ^ Snow water conservation error [mm H2O]
    -- Patch-level
  , wb_errh2o_patch                      :: !(VU.Vector Double) -- ^ Patch water conservation error [mm H2O]
    -- Gridcell-level
  , wb_liq1_grc                          :: !(VU.Vector Double) -- ^ Initial gridcell total h2o liq content
  , wb_liq2_grc                          :: !(VU.Vector Double) -- ^ Post land cover change total liq content
  , wb_ice1_grc                          :: !(VU.Vector Double) -- ^ Initial gridcell total h2o ice content
  , wb_ice2_grc                          :: !(VU.Vector Double) -- ^ Post land cover change total ice content
  , wb_begwb_grc                         :: !(VU.Vector Double) -- ^ Gridcell water mass beginning of timestep
  , wb_endwb_grc                         :: !(VU.Vector Double) -- ^ Gridcell water mass end of timestep
  } deriving (Show)

defaultWaterBalanceData :: WaterBalanceData
defaultWaterBalanceData = WaterBalanceData
  { wb_h2osno_old_col                    = VU.empty
  , wb_snow_sources_col                  = VU.empty
  , wb_snow_sinks_col                    = VU.empty
  , wb_wa_reset_nonconservation_gain_col = VU.empty
  , wb_begwb_col                         = VU.empty
  , wb_endwb_col                         = VU.empty
  , wb_errh2o_col                        = VU.empty
  , wb_errh2osno_col                     = VU.empty
  , wb_errh2o_patch                      = VU.empty
  , wb_liq1_grc                          = VU.empty
  , wb_liq2_grc                          = VU.empty
  , wb_ice1_grc                          = VU.empty
  , wb_ice2_grc                          = VU.empty
  , wb_begwb_grc                         = VU.empty
  , wb_endwb_grc                         = VU.empty
  }
