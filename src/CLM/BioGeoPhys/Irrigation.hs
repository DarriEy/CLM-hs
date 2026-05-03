{-# LANGUAGE BangPatterns #-}
-- | Irrigation module: calculate irrigation flux.
-- Fortran: IrrigationMod.F90
--
-- Provides:
--   * Irrigation parameters and state data types
--   * Irrigation need determination (deficit-based)
--   * Irrigation flux application (drip/sprinkler)
--   * River volume limitation
--   * Relative saturation to water content conversion
--   * Irrigation method assignment
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.Irrigation
  ( -- * Constants
    wiltingPointSmp
  , m3OverKm2ToMM
  , irrigMethodUnset
  , irrigMethodDrip
  , irrigMethodSprinkler
  , isecspday
    -- * Types
  , IrrigationParams(..)
  , defaultIrrigationParams
  , IrrigDeficitInput(..)
  , IrrigDeficitResult(..)
  , IrrigApplicationResult(..)
    -- * Science functions
  , calcIrrigNstepsPerDay
  , relsatToH2osoi
  , pointNeedsCheckForIrrig
  , calcDeficit
  , calcDeficitVolrLimited
  , calcApplicationFlux
  , checkNamelistValidity
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Soil matric potential at wilting point (mm)
wiltingPointSmp :: Double
wiltingPointSmp = -150000.0

-- | Conversion: m3/km2 to mm
m3OverKm2ToMM :: Double
m3OverKm2ToMM = 1.0e-3

-- | Irrigation methods
irrigMethodUnset :: Int
irrigMethodUnset = 0

irrigMethodDrip :: Int
irrigMethodDrip = 1

irrigMethodSprinkler :: Int
irrigMethodSprinkler = 2

-- | Seconds per day
isecspday :: Int
isecspday = 86400

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data IrrigationParams = IrrigationParams
  { ip_irrig_min_lai      :: !Double  -- ^ minimum LAI for irrigation
  , ip_irrig_start_time   :: !Int     -- ^ seconds since midnight to check (0 = midnight)
  , ip_irrig_length       :: !Int     -- ^ desired irrigation duration per day (sec)
  , ip_irrig_target_smp   :: !Double  -- ^ target matric potential (mm, negative)
  , ip_irrig_depth        :: !Double  -- ^ soil depth to measure (m)
  , ip_irrig_threshold_fraction :: !Double  -- ^ threshold fraction [0,1]
  , ip_irrig_river_volume_threshold :: !Double  -- ^ river volume threshold fraction [0,1]
  , ip_limit_irrigation_if_rof_enabled :: !Bool
  , ip_use_groundwater_irrigation :: !Bool
  , ip_irrig_method_default :: !Int
  } deriving (Show, Eq)

defaultIrrigationParams :: IrrigationParams
defaultIrrigationParams = IrrigationParams
  { ip_irrig_min_lai = 0.0
  , ip_irrig_start_time = 21600
  , ip_irrig_length = 14400
  , ip_irrig_target_smp = -3400.0
  , ip_irrig_depth = 0.6
  , ip_irrig_threshold_fraction = 1.0
  , ip_irrig_river_volume_threshold = 0.1
  , ip_limit_irrigation_if_rof_enabled = False
  , ip_use_groundwater_irrigation = False
  , ip_irrig_method_default = irrigMethodDrip
  }

-- | Per-layer input for deficit calculation
data IrrigDeficitInput = IrrigDeficitInput
  { idi_z              :: !Double  -- ^ layer midpoint depth (m)
  , idi_dz             :: !Double  -- ^ layer thickness (m)
  , idi_t_soisno       :: !Double  -- ^ soil temperature (K)
  , idi_h2osoi_liq     :: !Double  -- ^ liquid water (kg/m2)
  , idi_eff_porosity   :: !Double  -- ^ effective porosity
  , idi_relsat_wp      :: !Double  -- ^ relative saturation at wilting point
  , idi_relsat_target  :: !Double  -- ^ relative saturation at irrigation target
  , idi_nbedrock       :: !Int     -- ^ bedrock layer index
  } deriving (Show, Eq)

-- | Result of deficit calculation for a single column
data IrrigDeficitResult = IrrigDeficitResult
  { idr_deficit :: !Double  -- ^ water deficit (mm)
  , idr_needs_irrigation :: !Bool
  } deriving (Show, Eq)

-- | Result of irrigation application for a single patch
data IrrigApplicationResult = IrrigApplicationResult
  { iar_qflx_irrig_drip      :: !Double  -- ^ drip irrigation flux (mm/s)
  , iar_qflx_irrig_sprinkler :: !Double  -- ^ sprinkler irrigation flux (mm/s)
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute number of irrigation steps per day (rounded up).
calcIrrigNstepsPerDay :: Int -> Int -> Int
calcIrrigNstepsPerDay !irrig_length !dtime =
  (irrig_length + dtime - 1) `div` dtime

-- | Convert relative saturation to kg/m2 water.
relsatToH2osoi :: Double -> Double -> Double -> Double
relsatToH2osoi !relsat !eff_porosity !dz =
  eff_porosity * relsat * 1000.0 * dz  -- denh2o = 1000.0

-- | Determine whether a patch needs to be checked for irrigation now.
pointNeedsCheckForIrrig :: IrrigationParams
                        -> Double  -- ^ elai
                        -> Bool    -- ^ pftcon_irrigated (is this PFT irrigated?)
                        -> Int     -- ^ local_time_sec (seconds since midnight)
                        -> Int     -- ^ dtime (seconds)
                        -> Bool
pointNeedsCheckForIrrig !params !elai !irrigated !localTime !dtime
  | not irrigated = False
  | elai <= ip_irrig_min_lai params = False
  | otherwise =
    let !startOffset = ip_irrig_start_time params - dtime
        !secsSince = (localTime - startOffset) `mod` isecspday
        !secsSince' = if secsSince < 0 then secsSince + isecspday else secsSince
    in secsSince' < dtime

-- | Calculate soil water deficit for a single column.
-- Iterates over layers until max depth, bedrock, or frozen layer.
calcDeficit :: IrrigationParams
            -> V.Vector IrrigDeficitInput  -- ^ per-layer inputs
            -> IrrigDeficitResult
calcDeficit !params !layers =
  let tfrz = 273.15
      go !j !h2o_tot !h2o_target_tot !h2o_wp_tot !reached
        | j >= V.length layers = (h2o_tot, h2o_target_tot, h2o_wp_tot)
        | reached = (h2o_tot, h2o_target_tot, h2o_wp_tot)
        | otherwise =
          let !l = layers V.! j
          in if idi_z l > ip_irrig_depth params
             then (h2o_tot, h2o_target_tot, h2o_wp_tot)
             else if j > idi_nbedrock l
             then (h2o_tot, h2o_target_tot, h2o_wp_tot)
             else if idi_t_soisno l <= tfrz
             then (h2o_tot, h2o_target_tot, h2o_wp_tot)
             else
               let !h2o_liq_target = relsatToH2osoi (idi_relsat_target l) (idi_eff_porosity l) (idi_dz l)
                   !h2o_liq_wp     = relsatToH2osoi (idi_relsat_wp l) (idi_eff_porosity l) (idi_dz l)
               in go (j + 1)
                     (h2o_tot + idi_h2osoi_liq l)
                     (h2o_target_tot + h2o_liq_target)
                     (h2o_wp_tot + h2o_liq_wp)
                     False

      (!h2oTot, !h2oTargetTot, !h2oWpTot) = go 0 0.0 0.0 0.0 False

      !h2o_at_threshold = h2oWpTot + ip_irrig_threshold_fraction params *
                          (h2oTargetTot - h2oWpTot)
      !deficit | h2oTot < h2o_at_threshold = max 0.0 (h2oTargetTot - h2oTot)
               | otherwise = 0.0
  in IrrigDeficitResult
     { idr_deficit = deficit
     , idr_needs_irrigation = deficit > 0.0
     }

-- | Limit deficit by available river volume.
-- Returns the limited deficit fraction (0-1 multiplier).
calcDeficitVolrLimited :: IrrigationParams
                       -> Double  -- ^ deficit_grc (gridcell-average deficit, mm)
                       -> Double  -- ^ volr (river volume, m3)
                       -> Double  -- ^ grc_area (km2)
                       -> Double  -- ^ ratio (0 to 1)
calcDeficitVolrLimited !params !deficit_grc !volr !grc_area
  | deficit_grc <= 0.0 = 1.0
  | otherwise =
    let !available = if volr > 0.0
                     then volr * (1.0 - ip_irrig_river_volume_threshold params)
                     else 0.0
        !max_deficit = available / grc_area * m3OverKm2ToMM
    in if deficit_grc > max_deficit
       then max_deficit / deficit_grc
       else 1.0

-- | Set drip/sprinkler application flux for a single patch.
calcApplicationFlux :: Int     -- ^ irrig_method (IRRIG_METHOD_DRIP or SPRINKLER)
                    -> Double  -- ^ total irrigation flux (mm/s)
                    -> IrrigApplicationResult
calcApplicationFlux !method !qflx_tot
  | method == irrigMethodDrip =
    IrrigApplicationResult { iar_qflx_irrig_drip = qflx_tot
                           , iar_qflx_irrig_sprinkler = 0.0 }
  | method == irrigMethodSprinkler =
    IrrigApplicationResult { iar_qflx_irrig_drip = 0.0
                           , iar_qflx_irrig_sprinkler = qflx_tot }
  | otherwise =
    IrrigApplicationResult { iar_qflx_irrig_drip = 0.0
                           , iar_qflx_irrig_sprinkler = 0.0 }

-- | Check validity of irrigation parameters.
-- Returns Nothing if valid, Just errMsg otherwise.
checkNamelistValidity :: IrrigationParams -> Bool -> Maybe String
checkNamelistValidity !p !use_aquifer_layer
  | ip_irrig_min_lai p < 0.0 =
      Just "irrig_min_lai must be >= 0"
  | ip_irrig_start_time p < 0 || ip_irrig_start_time p >= isecspday =
      Just "irrig_start_time out of range"
  | ip_irrig_length p <= 0 || ip_irrig_length p > isecspday =
      Just "irrig_length out of range"
  | ip_irrig_target_smp p >= 0.0 =
      Just "irrig_target_smp must be negative"
  | ip_irrig_target_smp p < wiltingPointSmp =
      Just "irrig_target_smp must be >= WILTING_POINT_SMP"
  | ip_irrig_depth p < 0.0 =
      Just "irrig_depth must be >= 0"
  | ip_irrig_threshold_fraction p < 0.0 || ip_irrig_threshold_fraction p > 1.0 =
      Just "irrig_threshold_fraction out of [0,1]"
  | ip_limit_irrigation_if_rof_enabled p &&
    (ip_irrig_river_volume_threshold p < 0.0 || ip_irrig_river_volume_threshold p > 1.0) =
      Just "irrig_river_volume_threshold out of [0,1]"
  | ip_use_groundwater_irrigation p && not (ip_limit_irrigation_if_rof_enabled p) =
      Just "use_groundwater_irrigation requires limit_irrigation_if_rof_enabled"
  | use_aquifer_layer && ip_use_groundwater_irrigation p =
      Just "use_groundwater_irrigation and use_aquifer_layer cannot both be true"
  | otherwise = Nothing
