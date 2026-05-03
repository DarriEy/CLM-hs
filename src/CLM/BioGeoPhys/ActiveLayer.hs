{-# LANGUAGE BangPatterns #-}
-- | Active layer depth calculation (permafrost diagnostics).
-- Fortran: ActiveLayerMod.F90
-- Julia:   src/biogeophys/active_layer.jl
--
-- Calculates the depth of the active layer (ALT = active layer thickness)
-- using soil temperature profiles. The active layer is defined as the
-- deepest thawed layer. Linear interpolation is used where temperature
-- crosses the freezing point between layers.
--
-- All functions are pure.
module CLM.BioGeoPhys.ActiveLayer
  ( -- * Input / Output
    AltCalcInput(..)
  , AltCalcOutput(..)
    -- * Main computation
  , altCalc
    -- * Annual maximum update
  , AltAnnualResetInput(..)
  , AltAnnualState(..)
  , altAnnualReset
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (tfrz, nlevsno, nlevgrnd)

-- =========================================================================
-- Active layer thickness calculation
-- =========================================================================

-- | Input for active layer thickness calculation (single column).
data AltCalcInput = AltCalcInput
  { alti_t_soisno :: !(VU.Vector Double)  -- ^ Layer temperatures [K] (nlevsno+nlevgrnd)
  , alti_zsoi     :: !(VU.Vector Double)  -- ^ Soil node depths [m] (nlevgrnd, 0-indexed)
  } deriving (Show)

-- | Output of active layer thickness calculation.
data AltCalcOutput = AltCalcOutput
  { alto_alt      :: !Double  -- ^ Current depth of thaw [m]
  , alto_alt_indx :: !Int     -- ^ Index of deepest thawed layer
  } deriving (Show)

-- | Calculate active layer thickness for a single soil column.
-- Searches upward from the base of the soil profile for the first
-- layer above freezing. Uses linear interpolation between layers.
altCalc :: AltCalcInput -> AltCalcOutput
altCalc inp =
  let tsoi = alti_t_soisno inp
      zsoi = alti_zsoi inp
      joff = nlevsno

      -- Check deepest soil layer
      t_bot = tsoi VU.! (nlevgrnd + joff - 1)
  in if t_bot > tfrz
     then -- Entire column thawed
          AltCalcOutput
            { alto_alt      = zsoi VU.! (nlevgrnd - 1)
            , alto_alt_indx = nlevgrnd
            }
     else -- Search upward for first thawed layer
          let findThaw j
                | j < 1     = (0, False)
                | tsoi VU.! (j + joff - 1) > tfrz = (j, True)
                | otherwise = findThaw (j - 1)
              (!k_frz, !found) = findThaw (nlevgrnd - 1)
          in if found
             then let z1 = zsoi VU.! (k_frz - 1)
                      z2 = zsoi VU.! k_frz
                      t1 = tsoi VU.! (k_frz + joff - 1)
                      t2 = tsoi VU.! (k_frz + joff)
                      !alt_v = z1 + (t1 - tfrz) * (z2 - z1) / (t1 - t2)
                  in AltCalcOutput { alto_alt = alt_v, alto_alt_indx = k_frz }
             else AltCalcOutput { alto_alt = 0.0, alto_alt_indx = 0 }

-- =========================================================================
-- Annual maximum reset
-- =========================================================================

-- | Input for annual maximum reset check.
data AltAnnualResetInput = AltAnnualResetInput
  { aari_mon   :: !Int     -- ^ Current month (1-12)
  , aari_day   :: !Int     -- ^ Current day of month (1-31)
  , aari_sec   :: !Int     -- ^ Seconds into current date
  , aari_dtime :: !Int     -- ^ Timestep [s]
  , aari_lat   :: !Double  -- ^ Latitude [degrees]
  } deriving (Show)

-- | Mutable annual state for ALT tracking.
data AltAnnualState = AltAnnualState
  { aas_altmax      :: !Double  -- ^ Maximum annual depth of thaw [m]
  , aas_altmax_indx :: !Int     -- ^ Index of annual max
  , aas_altmax_lastyear      :: !Double  -- ^ Prior year max [m]
  , aas_altmax_lastyear_indx :: !Int     -- ^ Prior year max index
  } deriving (Show)

-- | Update annual maximum ALT. Returns updated state.
-- Resets occur on Jan 1 (NH) and Jul 1 (SH).
altAnnualReset :: AltAnnualResetInput -> AltAnnualState
               -> Double -> Int  -- ^ current alt and alt_indx
               -> AltAnnualState
altAnnualReset ri state alt_cur alt_indx_cur =
  let isJan1 = aari_mon ri == 1 && aari_day ri == 1 && div (aari_sec ri) (aari_dtime ri) == 1
      isJul1 = aari_mon ri == 7 && aari_day ri == 1 && div (aari_sec ri) (aari_dtime ri) == 1
      isNH   = aari_lat ri > 0.0

      -- Reset logic
      state1
        | isJan1 && isNH = state
            { aas_altmax_lastyear      = aas_altmax state
            , aas_altmax_lastyear_indx = aas_altmax_indx state
            , aas_altmax               = 0.0
            , aas_altmax_indx          = 0
            }
        | isJul1 && not isNH = state
            { aas_altmax_lastyear      = aas_altmax state
            , aas_altmax_lastyear_indx = aas_altmax_indx state
            , aas_altmax               = 0.0
            , aas_altmax_indx          = 0
            }
        | otherwise = state

      -- Update running maximum
      state2
        | alt_cur > aas_altmax state1 = state1
            { aas_altmax      = alt_cur
            , aas_altmax_indx = alt_indx_cur
            }
        | otherwise = state1

  in state2
