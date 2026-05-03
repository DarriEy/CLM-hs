-- | Cold-start state initialization for CLM.
-- Ported from Julia: src/infrastructure/cold_start.jl
-- Fortran: scattered *InitTimeConst and *InitCold subroutines.
--
-- All functions are pure: they take grid dimensions and surface input data,
-- and produce initial state records. No IO or mutation.
module CLM.Infrastructure.ColdStart
  ( -- * Top-level cold start
    coldStartInitialize
    -- * Individual initializers
  , initTemperatures
  , initSoilMoisture
  , initSnowState
  , initWaterDiagnostics
  , initSoilProperties
  , initSoilStateDefaults
  , initEffPorosity
  , initMiscState
    -- * Configuration
  , ColdStartConfig(..)
  , defaultColdStartConfig
    -- * Surface input data
  , SurfaceInputData(..)
  , defaultSurfaceInputData
    -- * Soil property results
  , SoilProperties(..)
    -- * Water diagnostics
  , WaterDiagnostics(..)
  , MiscColdState(..)
  , SoilStateDefaults(..)
    -- * Constants
  , tSoil, tLake, tH2osfc, tVeg
  , volInit, aquiferWaterBaseline, snwRdsMin
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants
  ( nlevsoi, nlevgrnd, nlevsno, denh2o, tkwat )
import CLM.Infrastructure.InitVertical (soilCoordinates)
import CLM.Types.TemperatureData (TemperatureData(..))
import CLM.Types.WaterStateData  (WaterStateData(..))
import CLM.Types.EnergyFluxData  (EnergyFluxData(..))

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for cold-start initialization.
data ColdStartConfig = ColdStartConfig
  { cscUseAquiferLayer :: !Bool
  , cscNcols           :: !Int    -- ^ Number of columns
  , cscNpatches        :: !Int    -- ^ Number of patches
  } deriving (Show, Eq)

defaultColdStartConfig :: ColdStartConfig
defaultColdStartConfig = ColdStartConfig
  { cscUseAquiferLayer = False
  , cscNcols           = 1
  , cscNpatches        = 1
  }

-- ============================================================================
-- Surface input data (simplified from NetCDF reader)
-- ============================================================================

-- | Surface data read from surfdata_clm.nc.
-- Vectors are sized (ncols * nlevsoi) in row-major order.
data SurfaceInputData = SurfaceInputData
  { surfPctSand   :: !(VU.Vector Double)  -- ^ Sand percentage per layer
  , surfPctClay   :: !(VU.Vector Double)  -- ^ Clay percentage per layer
  , surfOrganic   :: !(VU.Vector Double)  -- ^ Organic matter density [kg/m^3]
  , surfFmax      :: !(VU.Vector Double)  -- ^ Maximum saturated fraction (TOPMODEL)
  , surfColZ      :: !(VU.Vector Double)  -- ^ Soil layer midpoint depths [m]
  } deriving (Show)

defaultSurfaceInputData :: SurfaceInputData
defaultSurfaceInputData = SurfaceInputData
  { surfPctSand = VU.empty
  , surfPctClay = VU.empty
  , surfOrganic = VU.empty
  , surfFmax    = VU.empty
  , surfColZ    = VU.empty
  }

-- ============================================================================
-- Soil properties result
-- ============================================================================

-- | Computed soil physical properties per layer (ncols * nlevsoi).
data SoilProperties = SoilProperties
  { spWatsat  :: !(VU.Vector Double)  -- ^ Saturated volumetric water content
  , spBsw     :: !(VU.Vector Double)  -- ^ Clapp-Hornberger exponent
  , spSucsat  :: !(VU.Vector Double)  -- ^ Saturated matric potential [mm]
  , spHksat   :: !(VU.Vector Double)  -- ^ Saturated hydraulic conductivity [mm/s]
  , spTkmg    :: !(VU.Vector Double)  -- ^ Thermal cond. at saturation (mineral+organic)
  , spTksatu  :: !(VU.Vector Double)  -- ^ Saturated thermal conductivity
  , spTkdry   :: !(VU.Vector Double)  -- ^ Dry thermal conductivity
  , spCsol    :: !(VU.Vector Double)  -- ^ Heat capacity of soil solids [J/m^3/K]
  , spWatdry  :: !(VU.Vector Double)  -- ^ Water content at air-dry [m^3/m^3]
  , spWatopt  :: !(VU.Vector Double)  -- ^ Optimal water content [m^3/m^3]
  , spWatfc   :: !(VU.Vector Double)  -- ^ Water content at field capacity [m^3/m^3]
  } deriving (Show)

-- ============================================================================
-- Water diagnostic state
-- ============================================================================

-- | Water diagnostic fields initialized at cold start.
data WaterDiagnostics = WaterDiagnostics
  { wdFracSno         :: !Double  -- ^ Snow cover fraction
  , wdFracSnoEff      :: !Double  -- ^ Effective snow cover fraction
  , wdFracH2osfc      :: !Double  -- ^ Surface water fraction
  , wdSnowDepth       :: !Double  -- ^ Snow depth [m]
  , wdH2osnoTotal     :: !Double  -- ^ Total snow water equivalent [kg/m^2]
  , wdSnwRdsTop       :: !Double  -- ^ Top-layer snow grain radius [microns]
  , wdFwet            :: !Double  -- ^ Wetted fraction of canopy
  , wdFdry            :: !Double  -- ^ Dry fraction of canopy
  , wdQg              :: !Double  -- ^ Ground saturation specific humidity
  , wdFracIceold      :: !(VU.Vector Double)  -- ^ Ice fraction at previous timestep
  , wdSnwRds          :: !(VU.Vector Double)  -- ^ Snow grain radius per layer [microns]
  , wdTotalPlantH2o   :: !Double  -- ^ Total plant stored water [kg/m^2]
  } deriving (Show)

-- ============================================================================
-- Miscellaneous cold-start state
-- ============================================================================

-- | Miscellaneous state fields initialized at cold start.
data MiscColdState = MiscColdState
  { mcsZii           :: !Double  -- ^ Convective BL height [m]
  , mcsEmissivity    :: !Double  -- ^ Ground emissivity
  , mcsVegwp         :: !(VU.Vector Double)  -- ^ Vegetation water potential [mm]
  , mcsForcHgtU      :: !Double  -- ^ Forcing height for wind [m]
  , mcsForcHgtT      :: !Double  -- ^ Forcing height for temperature [m]
  , mcsForcHgtQ      :: !Double  -- ^ Forcing height for humidity [m]
  } deriving (Show)

-- ============================================================================
-- Cold-start temperature defaults (Fortran TemperatureType%InitCold)
-- ============================================================================

-- | Default temperatures for cold start [K].
tSoil, tLake, tH2osfc, tVeg :: Double
tSoil   = 272.0   -- soil/crop columns
tLake   = 277.0   -- lake columns
tH2osfc = 274.0   -- surface water
tVeg    = 283.0   -- vegetation and 2-m reference

-- | Initial volumetric water content [m^3/m^3].
volInit :: Double
volInit = 0.15

-- | Aquifer water baseline [mm] (Fortran default = 5000).
aquiferWaterBaseline :: Double
aquiferWaterBaseline = 5000.0

-- | Fresh snow grain radius minimum [microns].
snwRdsMin :: Double
snwRdsMin = 54.526

-- ============================================================================
-- Thermal property constants (calibrated defaults from read_params.jl)
-- ============================================================================

thermalCsolSand, thermalCsolCite, thermalCsolOm :: Double
thermalCsolSand = 2.128e6
thermalCsolCite = 2.385e6    -- clay
thermalCsolOm   = 2.5e6

thermalTkdSand, thermalTkdClay :: Double
thermalTkdSand = 8.80
thermalTkdClay = 2.92

thermalTkmOm, thermalTkdOm :: Double
thermalTkmOm  = 0.25
thermalTkdOm  = 0.05

thermalPd :: Double
thermalPd = 2700.0

-- ============================================================================
-- Top-level cold start
-- ============================================================================

-- | Perform full cold-start initialization, returning all state records.
-- Pure function: takes config and surface data, produces initial state.
coldStartInitialize
  :: ColdStartConfig
  -> SurfaceInputData
  -> ( TemperatureData    -- initial temperatures
     , WaterStateData     -- initial soil moisture / snow water
     , SoilProperties     -- computed soil properties
     , WaterDiagnostics   -- water diagnostic fields
     , MiscColdState      -- miscellaneous state
     , EnergyFluxData     -- zero-initialized energy fluxes
     )
coldStartInitialize cfg surf =
  ( initTemperatures nlevtot
  , initSoilMoisture cfg spProps colDz
  , spProps
  , initWaterDiagnostics
  , initMiscState
  , initEnergyFluxes
  )
  where
    nlevtot = nlevsno + nlevgrnd
    colDz   = colDz_ cfg surf
    spProps = initSoilProperties surf

-- | Compute column dz using real soil coordinates from InitVertical.
-- Snow layers get dz=0, soil layers get real exponential spacing.
colDz_ :: ColdStartConfig -> SurfaceInputData -> VU.Vector Double
colDz_ _cfg _surf =
  let (_, dzsoi, _) = soilCoordinates
  in  VU.generate (nlevsno + nlevgrnd) $ \i ->
        if i < nlevsno then 0.0
        else dzsoi VU.! (i - nlevsno)

-- ============================================================================
-- Temperature initialization
-- ============================================================================

-- | Initialize temperature state for cold start.
-- All soil/snow layers set to tSoil (272 K), ground = tSoil,
-- surface water = tH2osfc (274 K), vegetation/ref2m = tVeg (283 K).
initTemperatures :: Int -> TemperatureData
initTemperatures nlevtot = TemperatureData
  { t_soisno_col  = VU.replicate nlevtot tSoil
  , t_grnd_col    = tSoil
  , t_h2osfc_col  = tH2osfc
  , t_ref2m_patch = tVeg
  , t_veg_patch   = tVeg
  }

-- ============================================================================
-- Soil moisture initialization
-- ============================================================================

-- | Initialize soil moisture for cold start.
-- Sets volumetric water content to min(volInit, watsat), partitions all
-- as liquid (ice = 0). Snow water = 0, surface water = 0.
initSoilMoisture
  :: ColdStartConfig
  -> SoilProperties   -- ^ Computed soil properties (for watsat)
  -> VU.Vector Double -- ^ Column layer thicknesses (snow+soil)
  -> WaterStateData
initSoilMoisture cfg sp colDz = WaterStateData
  { h2osoi_liq_col = liqVec
  , h2osoi_ice_col = VU.replicate nlevtot 0.0
  , h2osoi_vol_col = volVec
  , h2osno_col     = 0.0
  , h2osfc_col     = 0.0
  , h2ocan_patch   = 0.0
  }
  where
    _ncols  = cscNcols cfg
    nlevtot = nlevsno + nlevgrnd

    -- Volumetric water content for soil layers (soil-only indexing)
    volVec = VU.generate nlevsoi $ \j ->
      let ws = if j < VU.length (spWatsat sp) then spWatsat sp VU.! j else 0.489
      in  min volInit ws

    -- Liquid water [kg/m^2] for all layers (snow layers = 0, soil layers = dz*rho*vol)
    liqVec = VU.generate nlevtot $ \jj ->
      if jj < nlevsno
        then 0.0  -- snow layers
        else
          let j   = jj - nlevsno  -- soil layer index (0-based)
              vol = if j < VU.length volVec then volVec VU.! j else 0.0
              dz  = if jj < VU.length colDz then colDz VU.! jj else 0.1
          in  if j < nlevsoi then dz * denh2o * vol else 0.0

-- ============================================================================
-- Snow state initialization
-- ============================================================================

-- | Initialize snow state for cold start (no snow layers).
-- Returns the number of snow layers (snl = 0).
initSnowState :: Int  -- ^ snl = 0
initSnowState = 0

-- ============================================================================
-- Water diagnostic state initialization
-- ============================================================================

-- | Initialize water diagnostic fields for cold start.
-- Snow fractions = 0, surface water fractions = 0, grain radius = snwRdsMin.
initWaterDiagnostics :: WaterDiagnostics
initWaterDiagnostics = WaterDiagnostics
  { wdFracSno         = 0.0
  , wdFracSnoEff      = 0.0
  , wdFracH2osfc      = 0.0
  , wdSnowDepth       = 0.0
  , wdH2osnoTotal     = 0.0
  , wdSnwRdsTop       = snwRdsMin
  , wdFwet            = 0.0
  , wdFdry            = 1.0
  , wdQg              = 0.0
  , wdFracIceold      = VU.replicate nlevsno 0.0
  , wdSnwRds          = VU.replicate nlevsno snwRdsMin
  , wdTotalPlantH2o   = 0.0
  }

-- ============================================================================
-- Soil properties computation
-- ============================================================================

-- | Compute soil physical properties from sand/clay/organic fractions.
-- Implements Clapp-Hornberger retention, organic blending (Lawrence & Slater 2008),
-- percolation-based hksat, and thermal conductivity formulas.
--
-- If surface data vectors are empty, returns default mineral-soil properties
-- for 50% sand, 20% clay, 0% organic.
initSoilProperties :: SurfaceInputData -> SoilProperties
initSoilProperties surf
  | VU.null (surfPctSand surf) = defaultSoilProperties
  | otherwise = SoilProperties
      { spWatsat  = VU.generate nlev (computeWatsat surf)
      , spBsw     = VU.generate nlev (computeBsw surf)
      , spSucsat  = VU.generate nlev (computeSucsat surf)
      , spHksat   = VU.generate nlev (computeHksat surf)
      , spTkmg    = VU.generate nlev (computeTkmg surf)
      , spTksatu  = VU.generate nlev (computeTksatu surf)
      , spTkdry   = VU.generate nlev (computeTkdry surf)
      , spCsol    = VU.generate nlev (computeCsol surf)
      , spWatdry  = VU.generate nlev (computeWatdry surf)
      , spWatopt  = VU.generate nlev (computeWatopt surf)
      , spWatfc   = VU.generate nlev (computeWatfc surf)
      }
  where
    nlev = min (VU.length (surfPctSand surf)) nlevsoi

-- | Default soil properties for 50% sand, 20% clay, 0% organic.
defaultSoilProperties :: SoilProperties
defaultSoilProperties =
  let wsVal  = 0.489 - 0.00126 * 50.0
      bswVal = 2.91 + 0.159 * 20.0
      sucVal = 10.0 * 10.0 ** (1.88 - 0.0131 * 50.0)
      hkVal  = 0.0070556 * 10.0 ** (-0.884 + 0.0153 * 50.0)
      spc    = max (50.0 + 20.0) 1.0
      tkm    = (thermalTkdSand * 50.0 + thermalTkdClay * 20.0) / spc
      tkmg   = tkm ** (1.0 - wsVal)
      bd     = (1.0 - wsVal) * thermalPd
  in  SoilProperties
    { spWatsat  = VU.replicate nlevsoi wsVal
    , spBsw     = VU.replicate nlevsoi bswVal
    , spSucsat  = VU.replicate nlevsoi sucVal
    , spHksat   = VU.replicate nlevsoi hkVal
    , spTkmg    = VU.replicate nlevsoi tkmg
    , spTksatu  = VU.replicate nlevsoi (tkmg * tkwat ** wsVal)
    , spTkdry   = VU.replicate nlevsoi
        (((0.135 * bd + 64.7) / (thermalPd - 0.947 * bd)))
    , spCsol    = VU.replicate nlevsoi
        ((thermalCsolSand * 50.0 + thermalCsolCite * 20.0) / spc)
    , spWatdry  = VU.replicate nlevsoi (wsVal * (316230.0 / sucVal) ** (-1.0 / bswVal))
    , spWatopt  = VU.replicate nlevsoi (wsVal * (158490.0 / sucVal) ** (-1.0 / bswVal))
    , spWatfc   = VU.replicate nlevsoi (wsVal * (0.01 / hkVal) ** (1.0 / (2.0 * bswVal + 3.0)))
    }

-- ============================================================================
-- Per-layer soil property computations
-- ============================================================================

-- Helpers for mineral/organic blending

mineralWatsat :: Double -> Double
mineralWatsat sand = 0.489 - 0.00126 * sand

mineralBsw :: Double -> Double
mineralBsw clay = 2.91 + 0.159 * clay

mineralSucsat :: Double -> Double
mineralSucsat sand = 10.0 * 10.0 ** (1.88 - 0.0131 * sand)

mineralHksat :: Double -> Double
mineralHksat sand = 0.0070556 * 10.0 ** (-0.884 + 0.0153 * sand)

organicWatsat :: Double -> Double
organicWatsat zRatio = max (0.93 - 0.1 * zRatio) 0.83

organicBsw :: Double -> Double
organicBsw zRatio = min (2.7 + 9.3 * zRatio) 12.0

organicSucsat :: Double -> Double
organicSucsat zRatio = min (10.3 - 0.2 * zRatio) 10.1

organicHksat :: Double -> Double -> Double
organicHksat zRatio hkMin = max (0.28 - 0.2799 * zRatio) hkMin

-- | Depth ratio (z / zsapric) for organic property blending.
depthRatio :: SurfaceInputData -> Int -> Double
depthRatio surf j =
  let zsoi = if j < VU.length (surfColZ surf)
             then surfColZ surf VU.! j
             else fromIntegral (j + 1) * 0.1
  in  zsoi / 0.5  -- zsapric = 0.5

-- | Blended watsat (mineral + organic), capped at 0.93.
computeWatsat :: SurfaceInputData -> Int -> Double
computeWatsat surf j =
  let (sand, _clay, omFrac) = getSandClayOm' surf j
      zr = depthRatio surf j
      wMin = mineralWatsat sand
      wOrg = organicWatsat zr
  in  min ((1.0 - omFrac) * wMin + omFrac * wOrg) 0.93

-- | Blended bsw.
computeBsw :: SurfaceInputData -> Int -> Double
computeBsw surf j =
  let (_sand, clay, omFrac) = getSandClayOm' surf j
      zr = depthRatio surf j
  in  (1.0 - omFrac) * mineralBsw clay + omFrac * organicBsw zr

-- | Blended sucsat.
computeSucsat :: SurfaceInputData -> Int -> Double
computeSucsat surf j =
  let (sand, _clay, omFrac) = getSandClayOm' surf j
      zr = depthRatio surf j
  in  (1.0 - omFrac) * mineralSucsat sand + omFrac * organicSucsat zr

-- | Blended hksat using percolation-based formula.
computeHksat :: SurfaceInputData -> Int -> Double
computeHksat surf j =
  let (sand, _clay, omFrac) = getSandClayOm' surf j
      zr       = depthRatio surf j
      hkMin    = mineralHksat sand
      hkOrg    = organicHksat zr hkMin
      pcalpha  = 0.5
      pcbeta   = 0.139
      percFrac = if omFrac > pcalpha
                 then let norm = (1.0 - pcalpha) ** (negate pcbeta)
                      in  norm * (omFrac - pcalpha) ** pcbeta
                 else 0.0
      unconFrac = (1.0 - omFrac) + (1.0 - percFrac) * omFrac
      unconHksat = if omFrac < 1.0
                   then unconFrac / ((1.0 - omFrac) / hkMin
                                     + ((1.0 - percFrac) * omFrac) / hkOrg)
                   else 0.0
  in  unconFrac * unconHksat + (percFrac * omFrac) * hkOrg

-- | Thermal cond. at saturation (mineral geometry).
computeTkmg :: SurfaceInputData -> Int -> Double
computeTkmg surf j =
  let (sand, clay, omFrac) = getSandClayOm' surf j
      spc = max (sand + clay) 1.0
      tkm = (1.0 - omFrac) * (thermalTkdSand * sand + thermalTkdClay * clay) / spc
            + thermalTkmOm * omFrac
      ws  = computeWatsat surf j
  in  tkm ** (1.0 - ws)

-- | Saturated thermal conductivity.
computeTksatu :: SurfaceInputData -> Int -> Double
computeTksatu surf j =
  let tkmg = computeTkmg surf j
      ws   = computeWatsat surf j
  in  tkmg * tkwat ** ws

-- | Dry thermal conductivity.
computeTkdry :: SurfaceInputData -> Int -> Double
computeTkdry surf j =
  let (_sand, _clay, omFrac) = getSandClayOm' surf j
      ws = computeWatsat surf j
      bd = (1.0 - ws) * thermalPd
  in  ((0.135 * bd + 64.7) / (thermalPd - 0.947 * bd)) * (1.0 - omFrac)
      + thermalTkdOm * omFrac

-- | Heat capacity of soil solids.
computeCsol :: SurfaceInputData -> Int -> Double
computeCsol surf j =
  let (sand, clay, omFrac) = getSandClayOm' surf j
      spc = max (sand + clay) 1.0
  in  (1.0 - omFrac) * (thermalCsolSand * sand + thermalCsolCite * clay) / spc
      + thermalCsolOm * omFrac

-- | Water content at air-dry.
computeWatdry :: SurfaceInputData -> Int -> Double
computeWatdry surf j =
  let ws  = computeWatsat surf j
      suc = computeSucsat surf j
      b   = computeBsw surf j
  in  ws * (316230.0 / suc) ** (-1.0 / b)

-- | Optimal water content for ET.
computeWatopt :: SurfaceInputData -> Int -> Double
computeWatopt surf j =
  let ws  = computeWatsat surf j
      suc = computeSucsat surf j
      b   = computeBsw surf j
  in  ws * (158490.0 / suc) ** (-1.0 / b)

-- | Water content at field capacity.
computeWatfc :: SurfaceInputData -> Int -> Double
computeWatfc surf j =
  let ws = computeWatsat surf j
      hk = computeHksat surf j
      b  = computeBsw surf j
  in  ws * (0.01 / hk) ** (1.0 / (2.0 * b + 3.0))

-- | Get sand/clay/omFrac (simplified accessor avoiding seq issues).
getSandClayOm' :: SurfaceInputData -> Int -> (Double, Double, Double)
getSandClayOm' surf j =
  let jSurf = min j (VU.length (surfPctSand surf) - 1)
      sand  = surfPctSand surf VU.! jSurf
      clay  = surfPctClay surf VU.! min jSurf (VU.length (surfPctClay surf) - 1)
      omRaw = if jSurf < VU.length (surfOrganic surf)
              then surfOrganic surf VU.! jSurf
              else 0.0
  in  (sand, clay, min (omRaw / 130.0) 1.0)

-- ============================================================================
-- Soil state defaults
-- ============================================================================

-- | Default soil state fields for cold start.
data SoilStateDefaults = SoilStateDefaults
  { ssdSmpmin    :: !Double  -- ^ Minimum soil matric potential [mm]
  , ssdSoilbeta  :: !Double  -- ^ Soil evaporation beta factor
  , ssdSoilalpha :: !Double  -- ^ Soil evaporation alpha
  , ssdWtfact    :: !Double  -- ^ Maximum saturated fraction
  } deriving (Show)

-- | Initialize soil state defaults.
initSoilStateDefaults :: Maybe Double -> SoilStateDefaults
initSoilStateDefaults mFmax = SoilStateDefaults
  { ssdSmpmin    = -1.0e8
  , ssdSoilbeta  = 1.0
  , ssdSoilalpha = 1.0
  , ssdWtfact    = maybe 0.38 id mFmax
  }

-- ============================================================================
-- Effective porosity
-- ============================================================================

-- | Initialize effective porosity: eff_porosity = max(0.01, watsat - vol_ice).
-- At cold start with zero ice, this is simply watsat (capped at min 0.01).
initEffPorosity :: SoilProperties -> VU.Vector Double
initEffPorosity sp =
  VU.map (\ws -> max 0.01 ws) (spWatsat sp)

-- ============================================================================
-- Miscellaneous cold-start state
-- ============================================================================

-- | Initialize miscellaneous state: BL height, emissivity, vegetation water
-- potential, forcing heights.
initMiscState :: MiscColdState
initMiscState = MiscColdState
  { mcsZii        = 1000.0
  , mcsEmissivity = 0.96
  , mcsVegwp      = VU.replicate 4 (-2.5e4)  -- 4 segments (leaf, stem, root, soil-root)
  , mcsForcHgtU   = 30.0
  , mcsForcHgtT   = 30.0
  , mcsForcHgtQ   = 30.0
  }

-- ============================================================================
-- Energy flux cold-start
-- ============================================================================

-- | Initialize energy fluxes to zero (cold start).
initEnergyFluxes :: EnergyFluxData
initEnergyFluxes = EnergyFluxData
  { eflx_sh_tot_patch   = 0.0
  , eflx_lh_tot_patch   = 0.0
  , eflx_sh_grnd_patch  = 0.0
  , eflx_soil_grnd_col  = 0.0
  , sabv_patch           = 0.0
  , sabg_patch           = 0.0
  , fsa_patch            = 0.0
  }
