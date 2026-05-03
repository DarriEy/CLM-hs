-- | Atmospheric forcing downscaling from gridcell to column.
-- Ported from Julia: src/infrastructure/downscale_forcings.jl
-- Fortran: src/main/atm2lndMod.F90
--
-- Pure functions for temperature/pressure/LW downscaling based on
-- topographic elevation differences, and precipitation partitioning.
module CLM.Infrastructure.DownscaleForcings
  ( -- * Downscaling configuration
    DownscaleParams(..)
  , defaultDownscaleParams
    -- * Atmospheric forcing data
  , AtmForcingGrc(..)
  , ColumnForcing(..)
    -- * Main downscaling
  , downscaleForcings
    -- * Sub-routines
  , downscaleTemperature
  , partitionPrecip
  , downscaleLongwave
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!))

import CLM.Constants.PhysicalConstants (tfrz, numrad)
import CLM.Infrastructure.InitSubgrid
  ( BoundsType(..), LandunitData(..), SubgridColumnData(..)
  , istice
  )

-- ============================================================================
-- Constants (from varcon.jl)
-- ============================================================================

rair :: Double
rair = 287.04

cpair :: Double
cpair = 1004.64

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Parameters controlling downscaling behavior.
data DownscaleParams = DownscaleParams
  { dpLapseRate                        :: !Double  -- ^ Temperature lapse rate [K/m] (default 0.006)
  , dpRepartitionRainSnow              :: !Bool    -- ^ Repartition rain/snow based on column T?
  , dpPrecipRepartGlcAllSnowT          :: !Double  -- ^ All-snow temperature for glaciers [K]
  , dpPrecipRepartGlcFracRainSlope     :: !Double  -- ^ Rain fraction slope for glaciers [1/K]
  , dpPrecipRepartNonglcAllSnowT       :: !Double  -- ^ All-snow temperature for non-glaciers [K]
  , dpPrecipRepartNonglcFracRainSlope  :: !Double  -- ^ Rain fraction slope for non-glaciers [1/K]
  , dpGlcmecDownscaleLongwave          :: !Bool    -- ^ Enable longwave downscaling?
  , dpLapseRateLongwave                :: !Double  -- ^ LW lapse rate [W/m^2/m]
  , dpLongwaveDownscalingLimit         :: !Double  -- ^ LW fractional change limit
  } deriving (Show, Eq)

defaultDownscaleParams :: DownscaleParams
defaultDownscaleParams = DownscaleParams
  { dpLapseRate                       = 0.006
  , dpRepartitionRainSnow             = False
  , dpPrecipRepartGlcAllSnowT         = 273.15
  , dpPrecipRepartGlcFracRainSlope    = 0.5
  , dpPrecipRepartNonglcAllSnowT      = 273.15
  , dpPrecipRepartNonglcFracRainSlope = 0.5
  , dpGlcmecDownscaleLongwave         = False
  , dpLapseRateLongwave               = 0.032
  , dpLongwaveDownscalingLimit         = 0.5
  }

-- ============================================================================
-- Atmospheric forcing data (gridcell level)
-- ============================================================================

-- | Gridcell-level atmospheric forcing (not downscaled).
data AtmForcingGrc = AtmForcingGrc
  { afForcT       :: !(Vector Double)  -- ^ Temperature [K] per gridcell
  , afForcTh      :: !(Vector Double)  -- ^ Potential temperature [K]
  , afForcPbot    :: !(Vector Double)  -- ^ Surface pressure [Pa]
  , afForcRho     :: !(Vector Double)  -- ^ Air density [kg/m^3]
  , afForcLwrad   :: !(Vector Double)  -- ^ Longwave radiation [W/m^2]
  , afForcSolad   :: !(Vector Double)  -- ^ Direct solar (ngrc * numrad), row-major
  , afForcSolar   :: !(Vector Double)  -- ^ Total solar [W/m^2]
  , afForcRain    :: !(Vector Double)  -- ^ Rain rate [mm/s]
  , afForcSnow    :: !(Vector Double)  -- ^ Snow rate [mm/s]
  , afForcVp      :: !(Vector Double)  -- ^ Vapor pressure [Pa]
  , afForcTopo    :: !(Vector Double)  -- ^ Gridcell topographic height [m]
  , afForcHgt     :: !(Vector Double)  -- ^ Forcing reference height [m]
  } deriving (Show)

-- ============================================================================
-- Column-level forcing (downscaled)
-- ============================================================================

-- | Column-level downscaled forcing.
data ColumnForcing = ColumnForcing
  { cfForcT     :: !(Vector Double)  -- ^ Temperature [K] per column
  , cfForcTh    :: !(Vector Double)  -- ^ Potential temperature [K]
  , cfForcPbot  :: !(Vector Double)  -- ^ Surface pressure [Pa]
  , cfForcRho   :: !(Vector Double)  -- ^ Air density [kg/m^3]
  , cfForcLwrad :: !(Vector Double)  -- ^ Longwave radiation [W/m^2]
  , cfForcSolad :: !(Vector Double)  -- ^ Direct solar (ncol * numrad)
  , cfForcSolar :: !(Vector Double)  -- ^ Total solar [W/m^2]
  , cfForcRain  :: !(Vector Double)  -- ^ Rain rate [mm/s]
  , cfForcSnow  :: !(Vector Double)  -- ^ Snow rate [mm/s]
  , cfForcQ     :: !(Vector Double)  -- ^ Specific humidity [kg/kg]
  } deriving (Show)

-- ============================================================================
-- Main downscaling (pure)
-- ============================================================================

-- | Downscale atmospheric forcings from gridcell to column level.
-- Pure function: takes gridcell forcing, topo, and subgrid data,
-- returns column-level forcing.
downscaleForcings
  :: BoundsType
  -> DownscaleParams
  -> AtmForcingGrc
  -> Vector Double       -- ^ topo_col: column topographic height [m]
  -> SubgridColumnData
  -> LandunitData
  -> ColumnForcing
downscaleForcings bounds params atm topocol col lun =
  let nc = endc bounds - begc bounds + 1

      -- Step 1: Copy gridcell values as baseline
      baseline = initBaseline bounds atm col

      -- Step 2: Temperature downscaling
      tempDown = downscaleTemperature bounds params atm topocol col baseline

      -- Step 3: Partition precipitation
      precDown = partitionPrecip bounds params atm tempDown col lun

      -- Step 4: Longwave downscaling (if enabled)
      lwDown = if dpGlcmecDownscaleLongwave params
               then downscaleLongwave bounds params atm topocol col tempDown
               else tempDown

  in  lwDown { cfForcRain = cfForcRain precDown
             , cfForcSnow = cfForcSnow precDown
             , cfForcQ    = cfForcQ precDown
             }

-- | Initialize column forcing by copying gridcell values.
initBaseline :: BoundsType -> AtmForcingGrc -> SubgridColumnData -> ColumnForcing
initBaseline bounds atm col =
  let nc = endc bounds
      genCol f = VU.generate nc $ \ix ->
        let g = colGridcell col ! ix - 1
        in  f ! g
  in  ColumnForcing
        { cfForcT     = genCol (afForcT atm)
        , cfForcTh    = genCol (afForcTh atm)
        , cfForcPbot  = genCol (afForcPbot atm)
        , cfForcRho   = genCol (afForcRho atm)
        , cfForcLwrad = genCol (afForcLwrad atm)
        , cfForcSolad = VU.generate (nc * numrad) $ \idx ->
            let (cix, b) = idx `divMod` numrad
                g = colGridcell col ! cix - 1
            in  afForcSolad atm ! (g * numrad + b)
        , cfForcSolar = genCol (afForcSolar atm)
        , cfForcRain  = VU.replicate nc 0.0
        , cfForcSnow  = VU.replicate nc 0.0
        , cfForcQ     = VU.replicate nc 0.0
        }

-- ============================================================================
-- Temperature downscaling
-- ============================================================================

-- | Elevation-based temperature, pressure, and density downscaling.
downscaleTemperature
  :: BoundsType
  -> DownscaleParams
  -> AtmForcingGrc
  -> Vector Double       -- ^ topo_col
  -> SubgridColumnData
  -> ColumnForcing       -- ^ baseline
  -> ColumnForcing
downscaleTemperature bounds params atm topocol col cf0 =
  let nc = endc bounds
      lr = dpLapseRate params
      grav_ = 9.80616

      process ix =
        let g = colGridcell col ! ix - 1
            hsurfG = afForcTopo atm ! g
            hsurfC = topocol ! ix
        in  if isNaN hsurfC || isNaN hsurfG
            then (cfForcT cf0 ! ix, cfForcTh cf0 ! ix,
                  cfForcPbot cf0 ! ix, cfForcRho cf0 ! ix)
            else let tbotG = afForcT atm ! g
                     tbotC = tbotG - lr * (hsurfC - hsurfG)
                     zbot = afForcHgt atm ! g
                     hbot = rair * 0.5 * (tbotG + tbotC) / grav_

                     thC = if hbot > 0.0
                           then afForcTh atm ! g +
                                (tbotC - tbotG) * exp ((zbot / hbot) * (rair / cpair))
                           else cfForcTh cf0 ! ix

                     pbotC = if hbot > 0.0
                             then afForcPbot atm ! g *
                                  exp (negate (hsurfC - hsurfG) / hbot)
                             else cfForcPbot cf0 ! ix

                     pbotG = afForcPbot atm ! g
                     rhoEstG = pbotG / (rair * tbotG)
                     rhoEstC = pbotC / (rair * tbotC)
                     rhoC = if rhoEstG > 0.0
                            then afForcRho atm ! g * (rhoEstC / rhoEstG)
                            else cfForcRho cf0 ! ix
                 in  (tbotC, thC, pbotC, rhoC)

      results = map process [0 .. nc - 1]
      (ts, ths, pbots, rhos) = unzip4 results
  in  cf0 { cfForcT    = VU.fromList ts
           , cfForcTh   = VU.fromList ths
           , cfForcPbot = VU.fromList pbots
           , cfForcRho  = VU.fromList rhos
           }

unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4 [] = ([],[],[],[])
unzip4 ((a,b,c,d):xs) =
  let (as,bs,cs,ds) = unzip4 xs in (a:as, b:bs, c:cs, d:ds)

-- ============================================================================
-- Precipitation partitioning
-- ============================================================================

-- | Partition precipitation into rain and snow based on column temperature.
-- Also computes specific humidity at column level.
partitionPrecip
  :: BoundsType
  -> DownscaleParams
  -> AtmForcingGrc
  -> ColumnForcing       -- ^ After temperature downscaling
  -> SubgridColumnData
  -> LandunitData
  -> ColumnForcing
partitionPrecip bounds params atm cf col lun =
  let nc = endc bounds

      processP ix =
        let g = colGridcell col ! ix - 1
            totalRain = if g < VU.length (afForcRain atm)
                        then afForcRain atm ! g else 0.0
            totalSnow = if g < VU.length (afForcSnow atm)
                        then afForcSnow atm ! g else 0.0
            totalPrecip = totalRain + totalSnow
            tCol = cfForcT cf ! ix
        in  if dpRepartitionRainSnow params && totalPrecip > 0.0
            then let l = colLandunit col ! ix
                     isGlc = lunItype lun ! (l - 1) == istice
                     (allSnowT, fracSlope) =
                       if isGlc
                       then (dpPrecipRepartGlcAllSnowT params,
                             dpPrecipRepartGlcFracRainSlope params)
                       else (dpPrecipRepartNonglcAllSnowT params,
                             dpPrecipRepartNonglcFracRainSlope params)
                     fracRain = clamp 0.0 1.0 ((tCol - allSnowT) * fracSlope)
                 in  (totalPrecip * fracRain, totalPrecip * (1.0 - fracRain))
            else if tCol > tfrz
                 then (totalPrecip, 0.0)
                 else (0.0, totalPrecip)

      processQ ix =
        let g = colGridcell col ! ix - 1
            vp = afForcVp atm ! g
            pbot = cfForcPbot cf ! ix
        in  0.622 * vp / max (pbot - 0.378 * vp) 1.0

      precipResults = map processP [0 .. nc - 1]
      (rains, snows) = unzip precipResults
      qs = map processQ [0 .. nc - 1]

  in  cf { cfForcRain = VU.fromList rains
          , cfForcSnow = VU.fromList snows
          , cfForcQ    = VU.fromList qs
          }

-- ============================================================================
-- Longwave downscaling
-- ============================================================================

-- | Downscale longwave radiation based on elevation.
downscaleLongwave
  :: BoundsType
  -> DownscaleParams
  -> AtmForcingGrc
  -> Vector Double       -- ^ topo_col
  -> SubgridColumnData
  -> ColumnForcing       -- ^ After temperature downscaling
  -> ColumnForcing
downscaleLongwave bounds params atm topocol col cf =
  let nc = endc bounds
      lrLw = dpLapseRateLongwave params
      limit = dpLongwaveDownscalingLimit params

      processLW ix =
        let g = colGridcell col ! ix - 1
            hsurfG = afForcTopo atm ! g
            hsurfC = if ix < VU.length topocol then topocol ! ix else hsurfG
            lwradG = afForcLwrad atm ! g
            lwradC = lwradG - lrLw * (hsurfC - hsurfG)
            lwMin = lwradG * (1.0 - limit)
            lwMax = lwradG * (1.0 + limit)
        in  max (clamp lwMin lwMax lwradC) 0.0

      lws = map processLW [0 .. nc - 1]
  in  cf { cfForcLwrad = VU.fromList lws }

-- ============================================================================
-- Utility
-- ============================================================================

-- | Clamp a value to [lo, hi].
clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)
