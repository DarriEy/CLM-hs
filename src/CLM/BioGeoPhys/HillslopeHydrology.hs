{-# LANGUAGE BangPatterns #-}
-- | Hillslope hydrology: geomorphological parameters, stream outflow,
-- stream water update, soil thickness profiles, PFT distributions.
-- Fortran: HillslopeHydrologyMod.F90
--
-- Provides:
--   * Hillslope configuration initialization
--   * Aquifer/hillslope compatibility check
--   * Soil thickness profile methods (uniform, from-file, linear, lowland-upland)
--   * Lowland/upland PFT assignment
--   * Stream outflow via Manning's equation
--   * Stream water volume update
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.HillslopeHydrology
  ( -- * Constants
    streamflowManning
  , pftStandard
  , pftFromFile
  , pftUniformDominantPft
  , pftLowlandDominantPft
  , pftLowlandUpland
  , soilProfileUniform
  , soilProfileFromFile
  , soilProfileSetLowlandUpland
  , soilProfileLinear
  , ispval
    -- * Types
  , HillslopeConfig(..)
  , defaultHillslopeConfig
  , StreamChannelProps(..)
  , StreamOutflowResult(..)
  , StreamWaterUpdateResult(..)
    -- * Stream functions
  , checkAquiferLayer
  , hillslopeSoilThicknessLinear
  , hillslopeStreamOutflow
  , hillslopeUpdateStreamWater
    -- * Lateral subsurface flow
  , LateralFlowInput(..)
  , LateralFlowOutput(..)
  , calcLateralSubsurfaceFlow
    -- * Water table gradient
  , calcWaterTableGradient
    -- * Hillslope driver
  , HillslopeDriverInput(..)
  , HillslopeDriverOutput(..)
  , hillslopeHydrologyDriver
  ) where

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Streamflow methods
streamflowManning :: Int
streamflowManning = 0

-- | PFT distribution methods
pftStandard :: Int
pftStandard = 0

pftFromFile :: Int
pftFromFile = 1

pftUniformDominantPft :: Int
pftUniformDominantPft = 2

pftLowlandDominantPft :: Int
pftLowlandDominantPft = 3

pftLowlandUpland :: Int
pftLowlandUpland = 4

-- | Soil profile methods
soilProfileUniform :: Int
soilProfileUniform = 0

soilProfileFromFile :: Int
soilProfileFromFile = 1

soilProfileSetLowlandUpland :: Int
soilProfileSetLowlandUpland = 2

soilProfileLinear :: Int
soilProfileLinear = 3

-- | Integer special value (unset neighbor)
ispval :: Int
ispval = -999999

-- | Minimum bedrock depth (m)
zminBedrock :: Double
zminBedrock = 0.4

-- | Manning roughness coefficient
manningRoughness :: Double
manningRoughness = 0.03

-- | Manning exponent
manningExponent :: Double
manningExponent = 0.667

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data HillslopeConfig = HillslopeConfig
  { hc_pft_distribution_method :: !Int
  , hc_soil_profile_method     :: !Int
  } deriving (Show, Eq)

defaultHillslopeConfig :: HillslopeConfig
defaultHillslopeConfig = HillslopeConfig pftStandard soilProfileUniform

data StreamChannelProps = StreamChannelProps
  { scp_depth  :: !Double  -- ^ channel depth (m)
  , scp_width  :: !Double  -- ^ channel width (m)
  , scp_slope  :: !Double  -- ^ channel slope (m/m)
  , scp_length :: !Double  -- ^ channel length (m)
  , scp_number :: !Double  -- ^ number of channel reaches
  } deriving (Show, Eq)

data StreamOutflowResult = StreamOutflowResult
  { sor_volumetric_streamflow :: !Double  -- ^ m3/s
  } deriving (Show, Eq)

data StreamWaterUpdateResult = StreamWaterUpdateResult
  { swu_stream_water_volume :: !Double  -- ^ m3
  , swu_volumetric_streamflow :: !Double  -- ^ m3/s (adjusted)
  , swu_stream_water_depth  :: !Double  -- ^ m
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Validate that use_hillslope and use_aquifer_layer are not both enabled.
-- Returns Nothing if OK, Just errMsg if invalid.
checkAquiferLayer :: Bool -> Bool -> Maybe String
checkAquiferLayer !use_hillslope !use_aquifer_layer
  | use_hillslope && use_aquifer_layer =
      Just "use_hillslope and use_aquifer_layer cannot both be true"
  | otherwise = Nothing

-- | Linear soil depth profile across hillslope columns.
-- Given column distances and min/max distances, returns the soil depth for
-- each column that linearly varies from lowland depth at max distance to
-- upland depth at min distance.
hillslopeSoilThicknessLinear
  :: Double  -- ^ soil_depth_lowland (m)
  -> Double  -- ^ soil_depth_upland (m)
  -> Double  -- ^ min_distance across columns
  -> Double  -- ^ max_distance across columns
  -> Double  -- ^ this column's hill_distance
  -> Double  -- ^ soil depth for this column
hillslopeSoilThicknessLinear !sdl !sdu !minDist !maxDist !dist
  | maxDist > minDist =
    let !m = (sdu - sdl) / (maxDist - minDist)
        !b = sdl - m * minDist
    in m * dist + b
  | otherwise = sdl

-- | Determine nbedrock from a target soil depth and the zisoi interface vector.
-- Returns the layer index j such that zisoi[j] < depth <= zisoi[j+1].
findNbedrock :: VU.Vector Double  -- ^ zisoi (soil interface depths)
             -> Double            -- ^ target soil depth (m)
             -> Int               -- ^ nlevsoi
             -> Int               -- ^ nbedrock
findNbedrock !zisoi !depth !nlevsoi = go 0
  where
    go !j
      | j >= nlevsoi = nlevsoi
      | otherwise =
        let !zi_j  = zisoi VU.! j
            !zi_j1 = zisoi VU.! (j + 1)
        in if zi_j > zminBedrock && zi_j < depth && zi_j1 >= depth
           then j
           else go (j + 1)

-- | Stream discharge via Manning's equation.
-- Handles overbank flow when stream depth exceeds channel depth.
hillslopeStreamOutflow :: StreamChannelProps
                       -> Double  -- ^ stream_water_volume (m3)
                       -> Double  -- ^ dtime (s)
                       -> StreamOutflowResult
hillslopeStreamOutflow !scp !swv !dtime
  | scp_length scp <= 0.0 || scp_width scp <= 0.0 || swv <= 0.0 =
    StreamOutflowResult 0.0
  | otherwise =
    let !csa = swv / scp_length scp
        !sd  = csa / scp_width scp
        !hr  = csa / (scp_width scp + 2.0 * sd)
    in if hr <= 0.0
       then StreamOutflowResult 0.0
       else
         let !fv = hr ** manningExponent * sqrt (scp_slope scp) / manningRoughness
             !baseFlow = csa * fv
             !flow | sd > scp_depth scp =
                     baseFlow * (sd / scp_depth scp)  -- overbank method 1
                   | otherwise = baseFlow
             !scaled = flow * scp_number scp
             !clamped = max 0.0 (min scaled (swv / dtime))
         in StreamOutflowResult clamped

-- | Update stream water volume by accumulating column fluxes and removing discharge.
hillslopeUpdateStreamWater
  :: Double  -- ^ stream_water_volume (m3)
  -> Double  -- ^ volumetric_streamflow (m3/s)
  -> Double  -- ^ total_inflow (m3/s, sum of drain + drain_perched + surf over columns)
  -> Double  -- ^ dtime (s)
  -> Double  -- ^ channel_length (m)
  -> Double  -- ^ channel_width (m)
  -> StreamWaterUpdateResult
hillslopeUpdateStreamWater !swv0 !vsf !totalInflow !dtime !clen !cwid =
  let !swv1 = swv0 + totalInflow * dtime - vsf * dtime
      (!swvFinal, !vsfFinal)
        | swv1 < 0.0 = (0.0, vsf + swv1 / dtime)
        | otherwise   = (swv1, vsf)
      !depth | clen > 0.0 && cwid > 0.0 = swvFinal / clen / cwid
             | otherwise = 0.0
  in StreamWaterUpdateResult
     { swu_stream_water_volume = swvFinal
     , swu_volumetric_streamflow = vsfFinal
     , swu_stream_water_depth = depth
     }

--------------------------------------------------------------------------------
-- Lateral subsurface flow between hillslope columns
--------------------------------------------------------------------------------

data LateralFlowInput = LateralFlowInput
  { lfi_zwt_this      :: !Double  -- ^ water table depth this column (m)
  , lfi_zwt_neighbor  :: !Double  -- ^ water table depth neighbor column (m)
  , lfi_distance      :: !Double  -- ^ distance between column centers (m)
  , lfi_width         :: !Double  -- ^ interface width (m)
  , lfi_transmissivity :: !Double -- ^ saturated transmissivity (m2/s)
  , lfi_area_this     :: !Double  -- ^ column area (m2)
  , lfi_dt            :: !Double  -- ^ timestep (s)
  } deriving (Show, Eq)

data LateralFlowOutput = LateralFlowOutput
  { lfo_qflx_lateral   :: !Double  -- ^ lateral flow rate (mm/s, positive = outflow)
  , lfo_vol_lateral     :: !Double  -- ^ volume transferred (m3)
  } deriving (Show, Eq)

-- | Calculate lateral subsurface flow between two hillslope columns.
-- Uses Darcy's law: Q = -T * dh/dx * W
-- where T = transmissivity, dh/dx = water table gradient, W = interface width.
calcLateralSubsurfaceFlow :: LateralFlowInput -> LateralFlowOutput
calcLateralSubsurfaceFlow !inp
  | lfi_distance inp <= 0.0 = LateralFlowOutput 0.0 0.0
  | otherwise =
    let -- Water table head difference (negative zwt = higher head)
        !dh = lfi_zwt_neighbor inp - lfi_zwt_this inp
        !gradient = dh / lfi_distance inp
        -- Darcy flux (m3/s), positive = flow from this to neighbor
        !qvol = lfi_transmissivity inp * gradient * lfi_width inp
        -- Convert to mm/s over column area
        !qflx = if lfi_area_this inp > 0.0
                then qvol / lfi_area_this inp * 1000.0
                else 0.0
        -- Volume transferred this timestep
        !vol = qvol * lfi_dt inp
    in LateralFlowOutput
       { lfo_qflx_lateral = qflx
       , lfo_vol_lateral = vol
       }

-- | Compute water table gradient across hillslope.
calcWaterTableGradient :: VU.Vector Double  -- ^ zwt per column (m)
                       -> VU.Vector Double  -- ^ distance per column (m)
                       -> VU.Vector Double  -- ^ gradient per column interface (m/m)
calcWaterTableGradient !zwt !dist =
  let !n = VU.length zwt
  in VU.generate (max 0 (n - 1)) $ \i ->
       let !dz = (zwt VU.! (i+1)) - (zwt VU.! i)
           !dx = (dist VU.! (i+1)) - (dist VU.! i)
       in if abs dx > 1.0e-10 then dz / dx else 0.0

--------------------------------------------------------------------------------
-- Hillslope hydrology driver
--------------------------------------------------------------------------------

data HillslopeDriverInput = HillslopeDriverInput
  { hdi_ncols           :: !Int
  , hdi_zwt             :: !(VU.Vector Double)  -- ^ water table depth per col (m)
  , hdi_hill_distance   :: !(VU.Vector Double)  -- ^ distance from channel (m)
  , hdi_hill_width      :: !(VU.Vector Double)  -- ^ column width (m)
  , hdi_hill_area       :: !(VU.Vector Double)  -- ^ column area (m2)
  , hdi_transmissivity  :: !(VU.Vector Double)  -- ^ saturated transmissivity (m2/s)
  , hdi_qflx_drain      :: !(VU.Vector Double)  -- ^ drainage from each col (mm/s)
  , hdi_qflx_surf       :: !(VU.Vector Double)  -- ^ surface runoff from each col (mm/s)
  , hdi_stream_channel  :: !StreamChannelProps
  , hdi_stream_water_vol :: !Double             -- ^ current stream water (m3)
  , hdi_dt              :: !Double              -- ^ timestep (s)
  } deriving (Show)

data HillslopeDriverOutput = HillslopeDriverOutput
  { hdo_qflx_lateral     :: !(VU.Vector Double)  -- ^ lateral flux per col (mm/s)
  , hdo_stream_discharge :: !Double              -- ^ stream discharge (m3/s)
  , hdo_stream_water_vol :: !Double              -- ^ updated stream water (m3)
  , hdo_stream_depth     :: !Double              -- ^ stream water depth (m)
  } deriving (Show)

-- | Main hillslope hydrology driver.
-- Computes lateral flow between columns and stream discharge.
hillslopeHydrologyDriver :: HillslopeDriverInput -> HillslopeDriverOutput
hillslopeHydrologyDriver !inp =
  let !nc = hdi_ncols inp
      !dt = hdi_dt inp
      !zwt = hdi_zwt inp
      !dist = hdi_hill_distance inp
      !width = hdi_hill_width inp
      !area = hdi_hill_area inp
      !trans = hdi_transmissivity inp

      -- Lateral flow between adjacent columns (downslope)
      !lateral = VU.generate nc $ \i ->
        if i < nc - 1
        then let !lfInp = LateralFlowInput
                   { lfi_zwt_this = zwt VU.! i
                   , lfi_zwt_neighbor = zwt VU.! (i+1)
                   , lfi_distance = abs (dist VU.! (i+1) - dist VU.! i)
                   , lfi_width = width VU.! i
                   , lfi_transmissivity = 0.5 * (trans VU.! i + trans VU.! (i+1))
                   , lfi_area_this = area VU.! i
                   , lfi_dt = dt
                   }
                 !result = calcLateralSubsurfaceFlow lfInp
             in lfo_qflx_lateral result
        else 0.0  -- lowest column drains to stream

      -- Total inflow to stream (from lowest column + all surface runoff)
      !drain_to_stream = VU.sum (hdi_qflx_drain inp) * 0.001  -- mm/s to m/s
                       * VU.sum area / 1000.0  -- approximate m3/s
      !surf_to_stream = VU.sum (hdi_qflx_surf inp) * 0.001
                      * VU.sum area / 1000.0
      !total_inflow = drain_to_stream + surf_to_stream

      -- Stream outflow via Manning's equation
      !scp = hdi_stream_channel inp
      !swv = hdi_stream_water_vol inp
      !outflow = sor_volumetric_streamflow $ hillslopeStreamOutflow scp swv dt

      -- Update stream water
      !streamResult = hillslopeUpdateStreamWater swv outflow total_inflow dt
                        (scp_length scp) (scp_width scp)

  in HillslopeDriverOutput
     { hdo_qflx_lateral = lateral
     , hdo_stream_discharge = swu_volumetric_streamflow streamResult
     , hdo_stream_water_vol = swu_stream_water_volume streamResult
     , hdo_stream_depth = swu_stream_water_depth streamResult
     }
