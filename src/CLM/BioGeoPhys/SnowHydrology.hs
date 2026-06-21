{-# LANGUAGE BangPatterns #-}
-- | Snow hydrology: compaction, layering, water balance, density, snow cover fraction.
-- Fortran: SnowHydrologyMod (~4070 lines)
-- Julia:   src/biogeophys/snow_hydrology.jl
--
-- All functions are pure. Column-level operations return updated state tuples.
-- Uses Data.Vector.Unboxed for layer arrays.
module CLM.BioGeoPhys.SnowHydrology
  ( -- * Configuration
    SnowHydrologyParams(..)
  , defaultSnowHydroParams
  , OverburdenMethod(..)
  , NewSnowDensityMethod(..)
  , SCFMethod(..)

    -- * Snow layer thickness constants
  , SnowLayerBounds(..)
  , initSnowLayerBounds

    -- * New snow density
  , newSnowBulkDensity

    -- * Snow cover fraction (Swenson-Lawrence 2012)
  , fracSnowDuringMelt
  , updateSnowDepthAndFracSL2012
  , addNewsnowToIntsnowSL2012
  , calcFracSnoEff

    -- * Snow compaction
  , snowCompactionLayer
  , overburdenCompactionAnderson1976
  , overburdenCompactionVionnet2012
  , windDriftCompaction

    -- * Snow layer combine
  , combo
  , comboScalar
  , massWeightedSnowRadius
  , combineSnowLayers

    -- * Snow layer divide
  , divideSnowLayers

    -- * Snow percolation
  , snowPercolation

    -- * Layer state helpers
  , zeroEmptySnowLayers
  , initSnowLayers
  , buildSnowFilter
  , snowCappingExcess

    -- * Snow layer data
  , SnowLayerState(..)
  , emptySnowLayerState

    -- * New snow addition
  , AddNewSnowInput(..)
  , addNewSnowToColumn

    -- * Snow water main driver
  , SnowWaterInput(..)
  , SnowWaterOutput(..)
  , snowWaterDriver

    -- * Remove snow from thawed wetlands
  , removeSnowFromThawedWetlands
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.List (unzip3)
import CLM.Constants.PhysicalConstants
  ( tfrz, denh2o, denice, cpice, cpliq, hfus, nlevsno )

-- =========================================================================
-- Configuration types
-- =========================================================================

-- | Overburden compaction method
data OverburdenMethod
  = Anderson1976
  | Vionnet2012
  deriving (Show, Eq)

-- | Low-temperature new snow density method
data NewSnowDensityMethod
  = TruncatedAnderson1976
  | Slater2017
  deriving (Show, Eq)

-- | Snow cover fraction method
data SCFMethod
  = SwensonLawrence2012
  | NiuYang2007
  deriving (Show, Eq)

-- | Snow hydrology parameters (from Fortran params_type)
data SnowHydrologyParams = SnowHydrologyParams
  { wimp                      :: !Double  -- ^ Water impermeable if porosity < wimp
  , ssi                       :: !Double  -- ^ Irreducible water saturation of snow
  , drift_gs                  :: !Double  -- ^ Wind drift compaction / grain size
  , eta0_anderson             :: !Double  -- ^ Viscosity coefficient Anderson1976 [kg*s/m2]
  , eta0_vionnet              :: !Double  -- ^ Viscosity coefficient Vionnet2012 [kg*s/m2]
  , wind_snowcompact_fact     :: !Double  -- ^ Reference wind for snow density increase [m/s]
  , rho_max                   :: !Double  -- ^ Wind drift compaction / max density [kg/m3]
  , tau_ref                   :: !Double  -- ^ Wind drift compaction / reference time [s]
  , ceta                      :: !Double  -- ^ Overburden compaction constant [kg/m3]
  , snw_rds_min               :: !Double  -- ^ Min allowed snow effective radius [microns]
  , upplim_destruct_metamorph :: !Double  -- ^ Upper limit on destructive metamorphism [kg/m3]
  , overburden_compress_tfactor :: !Double -- ^ Temperature factor for overburden compaction
  , overburdenMethod          :: !OverburdenMethod
  , newSnowDensityMethod      :: !NewSnowDensityMethod
  , windDependentSnowDensity  :: !Bool
  } deriving (Show)

-- | Default parameters matching CLM5
defaultSnowHydroParams :: SnowHydrologyParams
defaultSnowHydroParams = SnowHydrologyParams
  { wimp                      = 0.05
  , ssi                       = 0.033
  , drift_gs                  = 0.35
  , eta0_anderson             = 9.0e5
  , eta0_vionnet              = 7.62237e6
  , wind_snowcompact_fact     = 5.0
  , rho_max                   = 350.0
  , tau_ref                   = 172800.0
  , ceta                      = 250.0
  , snw_rds_min               = 54.526
  , upplim_destruct_metamorph = 100.0
  , overburden_compress_tfactor = 0.08
  , overburdenMethod          = Vionnet2012
  , newSnowDensityMethod      = Slater2017
  , windDependentSnowDensity  = True
  }

-- =========================================================================
-- Snow layer thickness bounds
-- =========================================================================

-- | Min/max layer thickness arrays (initialized from namelist parameters)
data SnowLayerBounds = SnowLayerBounds
  { snowDzmin  :: !(VU.Vector Double)  -- ^ Minimum thickness per layer [m]
  , snowDzmaxL :: !(VU.Vector Double)  -- ^ Max thickness (lower, for divide) [m]
  , snowDzmaxU :: !(VU.Vector Double)  -- ^ Max thickness (upper, for divide) [m]
  } deriving (Show)

-- | Initialize snow layer thickness bounds.
-- Matches Fortran init_snow_layers logic for computing dzmin/dzmax arrays.
initSnowLayerBounds :: SnowLayerBounds
initSnowLayerBounds =
  let n = nlevsno
      -- Namelist defaults
      dzmin1   = 0.010
      dzmin2   = 0.015
      dzmaxL1  = 0.03
      dzmaxL2  = 0.07
      dzmaxU1  = 0.02
      dzmaxU2  = 0.05

      go :: Int -> Double -> Double -> Double
         -> [(Double, Double, Double)] -> [(Double, Double, Double)]
      go j prevDzmaxU prevDzmaxL _prevDzmin acc
        | j > n = reverse acc
        | j == 1 = go (j+1) dzmaxU1 dzmaxL1 dzmin1 ((dzmin1, dzmaxL1, dzmaxU1) : acc)
        | j == 2 = go (j+1) dzmaxU2 dzmaxL2 dzmin2 ((dzmin2, dzmaxL2, dzmaxU2) : acc)
        | j == n =
            let dzm  = prevDzmaxU * 0.5
                dzu  = 1.0e308  -- floatmax
                dzl  = 1.0e308
            in  go (j+1) dzu dzl dzm ((dzm, dzl, dzu) : acc)
        | otherwise =
            let dzm  = prevDzmaxU * 0.5
                dzu  = 2.0 * prevDzmaxU + 0.01
                dzl  = dzu + prevDzmaxL
            in  go (j+1) dzu dzl dzm ((dzm, dzl, dzu) : acc)

      triples = go 1 0.0 0.0 0.0 []
      (mins, maxLs, maxUs) = unzip3 triples
  in SnowLayerBounds
       { snowDzmin  = VU.fromList mins
       , snowDzmaxL = VU.fromList maxLs
       , snowDzmaxU = VU.fromList maxUs
       }

-- =========================================================================
-- Snow layer state (per-column, layers indexed 0..nlevsno-1, top=0)
-- =========================================================================

-- | Per-column snow layer state. Layer index 0 = topmost active layer.
-- Vectors have length nlevsno. Active layers are indices 0..(nLayers-1)
-- where nLayers = abs(snl).
data SnowLayerState = SnowLayerState
  { slDz         :: !(VU.Vector Double)  -- ^ Layer thickness [m]
  , slZ          :: !(VU.Vector Double)  -- ^ Layer midpoint depth [m] (negative above surface)
  , slZi         :: !(VU.Vector Double)  -- ^ Interface depth [m] (length nlevsno+1, index 0 = bottom)
  , slTSoisno    :: !(VU.Vector Double)  -- ^ Layer temperature [K]
  , slH2osoiIce  :: !(VU.Vector Double)  -- ^ Ice content [kg/m2]
  , slH2osoiLiq  :: !(VU.Vector Double)  -- ^ Liquid water content [kg/m2]
  , slSnwRds     :: !(VU.Vector Double)  -- ^ Snow grain radius [microns]
  , slSnl        :: !Int                 -- ^ Number of snow layers (negative, e.g. -3 = 3 layers)
  } deriving (Show)

-- | Empty snow layer state (no active layers)
emptySnowLayerState :: SnowLayerState
emptySnowLayerState = SnowLayerState
  { slDz        = VU.replicate nlevsno 0.0
  , slZ         = VU.replicate nlevsno 0.0
  , slZi        = VU.replicate (nlevsno + 1) 0.0
  , slTSoisno   = VU.replicate nlevsno 0.0
  , slH2osoiIce = VU.replicate nlevsno 0.0
  , slH2osoiLiq = VU.replicate nlevsno 0.0
  , slSnwRds    = VU.replicate nlevsno 54.526
  , slSnl       = 0
  }

-- =========================================================================
-- Landunit type constants (from Fortran landunit_varcon)
-- =========================================================================

istsoil, istcrop, istice, istdlak, istwet :: Int
istsoil = 1
istcrop = 2
istice  = 4
istdlak = 5
istwet  = 6

-- | Additional minimum thickness for lake snow layers [m]
lsadz :: Double
lsadz = 0.03

-- | Maximum allowed snow effective radius [microns]
snwRdsMax :: Double
snwRdsMax = 1500.0

-- | pi
rpi :: Double
rpi = 3.14159265358979323846

-- =========================================================================
-- NewSnowBulkDensity
-- =========================================================================

-- | Compute bulk density of newly-fallen snow [kg/m3].
-- Uses Alta relationship from Anderson(1976), optionally with wind enhancement.
newSnowBulkDensity
  :: SnowHydrologyParams
  -> Double  -- ^ forc_t: atmospheric temperature [K]
  -> Double  -- ^ forc_wind: atmospheric wind speed [m/s]
  -> Double  -- ^ bifall: bulk density [kg/m3]
newSnowBulkDensity params forc_t forc_wind =
  let -- Base density from temperature
      base
        | forc_t > tfrz + 2.0 =
            50.0 + 1.7 * (17.0 ** 1.5)
        | forc_t > tfrz - 15.0 =
            50.0 + 1.7 * ((forc_t - tfrz + 15.0) ** 1.5)
        | newSnowDensityMethod params == TruncatedAnderson1976 =
            50.0
        | otherwise =  -- Slater2017
            let t_degC = if forc_t > tfrz - 57.55
                         then forc_t - tfrz
                         else -57.55
            in  negate (50.0 / 15.0 + 0.0333 * 15.0) * t_degC
                - 0.0333 * t_degC * t_degC

      -- Wind enhancement (Lehning et al.)
      windAdj
        | windDependentSnowDensity params && forc_wind > 0.1 =
            266.861 * ((1.0 + tanh (forc_wind / wind_snowcompact_fact params)) / 2.0) ** 8.8
        | otherwise = 0.0

  in base + windAdj

-- =========================================================================
-- Snow cover fraction: Swenson-Lawrence 2012
-- =========================================================================

-- | Fractional snow cover during melt (Swenson & Lawrence 2012).
-- Pure point function.
fracSnowDuringMelt
  :: Double  -- ^ n_melt: SCA shape parameter
  -> Double  -- ^ int_snow_max: limit on integrated snowfall [mm H2O]
  -> Double  -- ^ h2osno_total: total snow water [mm]
  -> Double  -- ^ int_snow: integrated snowfall [mm]
  -> Double  -- ^ frac_sno
fracSnowDuringMelt n_melt int_snow_max h2osno_total int_snow =
  let int_snow_limited = min int_snow int_snow_max
      smr = min 1.0 (h2osno_total / int_snow_limited)
      cosArg = min 1.0 (2.0 * smr - 1.0)
  in  1.0 - (acos cosArg / rpi) ** n_melt

-- | Update snow depth and fractional snow cover (Swenson-Lawrence 2012).
-- Returns (frac_sno, frac_sno_eff, snow_depth).
updateSnowDepthAndFracSL2012
  :: Double  -- ^ n_melt
  -> Double  -- ^ accum_factor
  -> Double  -- ^ int_snow_max
  -> Double  -- ^ h2osno_total [mm]
  -> Double  -- ^ snowmelt [mm] (from previous timestep)
  -> Double  -- ^ int_snow [mm]
  -> Double  -- ^ newsnow [mm]
  -> Double  -- ^ bifall [kg/m3]
  -> Double  -- ^ old snow_depth [m]
  -> Double  -- ^ old frac_sno
  -> Int     -- ^ lun_itype_col
  -> Bool    -- ^ urbpoi
  -> Bool    -- ^ use_subgrid_fluxes
  -> (Double, Double, Double)
     -- ^ (frac_sno, frac_sno_eff, snow_depth)
updateSnowDepthAndFracSL2012
  n_melt accum_factor int_snow_max
  h2osno_total snowmelt int_snow
  newsnow bifall old_snow_depth old_frac_sno
  lun_itype urbpoi use_subgrid_fluxes =

  let -- Step 1: Update frac_sno
      frac_sno_1
        | h2osno_total == 0.0 =
            if newsnow > 0.0
            then tanh (accum_factor * newsnow)
            else 0.0
        | otherwise =
            let melt_adj = if snowmelt > 0.0
                           then fracSnowDuringMelt n_melt int_snow_max h2osno_total int_snow
                           else old_frac_sno
                accum_adj = if newsnow > 0.0
                            then melt_adj + tanh (accum_factor * newsnow) * (1.0 - melt_adj)
                            else melt_adj
            in  accum_adj

      -- Step 2: frac_sno_eff
      frac_sno_eff_1 = calcFracSnoEff frac_sno_1 lun_itype urbpoi use_subgrid_fluxes

      -- Step 3: Update snow_depth
      snow_depth_1
        | h2osno_total > 0.0 =
            if frac_sno_eff_1 > 0.0
            then old_snow_depth + newsnow / (bifall * frac_sno_eff_1)
            else 0.0
        | newsnow > 0.0 =
            let z_avg = newsnow / bifall
            in  z_avg / frac_sno_eff_1
        | otherwise = 0.0

  in (frac_sno_1, frac_sno_eff_1, snow_depth_1)

-- | Calculate effective snow-covered fraction.
-- For urban or lake columns, or when use_subgrid_fluxes is false,
-- frac_sno_eff is 0 or 1. Otherwise it equals frac_sno.
calcFracSnoEff
  :: Double  -- ^ frac_sno
  -> Int     -- ^ lun_itype_col
  -> Bool    -- ^ urbpoi
  -> Bool    -- ^ use_subgrid_fluxes
  -> Double  -- ^ frac_sno_eff
calcFracSnoEff frac_sno lun_itype urbpoi use_subgrid_fluxes
  | not allow_fractional = if frac_sno > 0.0 then 1.0 else 0.0
  | otherwise            = frac_sno
  where
    allow_fractional = not (urbpoi || lun_itype == istdlak || not use_subgrid_fluxes)

-- | Add new snow to integrated snowfall (Swenson-Lawrence 2012).
-- Returns updated int_snow.
addNewsnowToIntsnowSL2012
  :: Double  -- ^ n_melt
  -> Double  -- ^ newsnow [mm]
  -> Double  -- ^ h2osno_total [mm]
  -> Double  -- ^ frac_sno
  -> Double  -- ^ old int_snow [mm]
  -> Double  -- ^ updated int_snow
addNewsnowToIntsnowSL2012 n_melt newsnow h2osno_total frac_sno old_int_snow =
  let base = if newsnow > 0.0
             then let temp_intsnow = (h2osno_total + newsnow) /
                        (0.5 * (cos (rpi * (1.0 - max frac_sno 1.0e-6)
                                       ** (1.0 / n_melt)) + 1.0))
                  in  min 1.0e8 temp_intsnow
             else old_int_snow
  in  base + newsnow

-- =========================================================================
-- Snow compaction
-- =========================================================================

-- | Overburden compaction rate using Anderson 1976 formula.
-- Returns compaction rate ddz2 [1/s].
overburdenCompactionAnderson1976
  :: Double  -- ^ eta0_anderson [kg*s/m2]
  -> Double  -- ^ overburden_compress_tfactor
  -> Double  -- ^ burden: overburden pressure [kg/m2]
  -> Double  -- ^ wx: total layer water mass [kg/m2]
  -> Double  -- ^ td: temperature deficit (tfrz - T) [K]
  -> Double  -- ^ bi: ice density [kg/m3]
  -> Double  -- ^ ddz2 [1/s]
overburdenCompactionAnderson1976 eta0 compress_tf burden wx td bi =
  let c2 = 23.0e-3  -- [m3/kg]
  in  negate (burden + wx / 2.0) * exp (negate compress_tf * td - c2 * bi) / eta0

-- | Overburden compaction rate using Vionnet et al. 2012 formula.
-- Returns compaction rate ddz2 [1/s].
overburdenCompactionVionnet2012
  :: SnowHydrologyParams
  -> Double  -- ^ h2osoi_liq [kg/m2]
  -> Double  -- ^ dz [m]
  -> Double  -- ^ burden [kg/m2]
  -> Double  -- ^ wx: total layer water [kg/m2]
  -> Double  -- ^ td: temperature deficit (tfrz - T) [K]
  -> Double  -- ^ bi: ice density [kg/m3]
  -> Double  -- ^ ddz2 [1/s]
overburdenCompactionVionnet2012 params h2osoi_liq dz burden wx td bi =
  let aeta = 0.1
      beta = 0.023
      f1 = 1.0 / (1.0 + 60.0 * h2osoi_liq / (denh2o * dz))
      f2 = 4.0  -- fixed to maximum value
      eta = f1 * f2 * (bi / ceta params)
            * exp (aeta * td + beta * bi) * eta0_vionnet params
  in  negate (burden + wx / 2.0) / eta

-- | Wind drift compaction for a single layer.
-- Returns (compaction_rate, updated zpseudo, updated mobile).
windDriftCompaction
  :: SnowHydrologyParams
  -> Double  -- ^ bi: ice density [kg/m3]
  -> Double  -- ^ forc_wind [m/s]
  -> Double  -- ^ dz [m]
  -> Double  -- ^ zpseudo (accumulated pseudo-depth from above)
  -> Bool    -- ^ mobile: whether this layer is still mobile
  -> (Double, Double, Bool)
     -- ^ (compaction_rate, zpseudo', mobile')
windDriftCompaction params bi forc_wind dz zpseudo mobile
  | not mobile = (0.0, zpseudo, False)
  | otherwise  =
      let rho_min  = 50.0
          drift_sph = 1.0
          frho = 1.25 - 0.0042 * (max rho_min bi - rho_min)
          mo   = 0.34 * (negate 0.583 * drift_gs params - 0.833 * drift_sph + 0.833)
                 + 0.66 * frho
          si_raw = negate 2.868 * exp (negate 0.085 * forc_wind) + 1.0 + mo
      in  if si_raw > 0.0
          then let si = min si_raw 3.25
                   zp1 = zpseudo + 0.5 * dz * (3.25 - si)
                   gamma_drift = si * exp (negate zp1 / 0.1)
                   tau_inverse = gamma_drift / tau_ref params
                   cr = negate (max 0.0 (rho_max params - bi)) * tau_inverse
                   zp2 = zp1 + 0.5 * dz * (3.25 - si)
               in  (cr, zp2, True)
          else (0.0, zpseudo, False)

-- | Compute compaction for a single snow layer.
-- Returns the compaction rate pdzdtc [1/s] and updated (burden, zpseudo, mobile).
snowCompactionLayer
  :: SnowHydrologyParams
  -> Double  -- ^ dtime [s]
  -> Double  -- ^ t_soisno [K]
  -> Double  -- ^ h2osoi_ice [kg/m2]
  -> Double  -- ^ h2osoi_liq [kg/m2]
  -> Int     -- ^ imelt: melt flag (1 = melting)
  -> Double  -- ^ frac_sno
  -> Double  -- ^ swe_old [mm]
  -> Double  -- ^ int_snow [mm]
  -> Double  -- ^ frac_iceold
  -> Double  -- ^ frac_h2osfc
  -> Double  -- ^ forc_wind [m/s]
  -> Double  -- ^ dz [m]
  -> Double  -- ^ burden (overburden from layers above) [kg/m2]
  -> Double  -- ^ zpseudo (wind drift pseudo-depth from above)
  -> Bool    -- ^ mobile (wind drift mobility flag)
  -> Bool    -- ^ is_lake_or_urban
  -> Double  -- ^ wsum: total column SWE [kg/m2] (for melt frac_sno calc)
  -> (Double, Double, Double, Bool)
     -- ^ (pdzdtc, burden', zpseudo', mobile')
snowCompactionLayer params dtime t_soisno h2osoi_ice h2osoi_liq
  imelt frac_sno swe_old int_snow frac_iceold frac_h2osfc
  forc_wind dz burden zpseudo mobile is_lake_or_urban wsum =

  let wx   = h2osoi_ice + h2osoi_liq
      void = 1.0 - (h2osoi_ice / denice + h2osoi_liq / denh2o)
                    / (frac_sno * dz)
      burden' = burden + wx
  in
    if void > 0.001 && h2osoi_ice > 0.1
    then
      let bi = h2osoi_ice / (frac_sno * dz)
          fi = h2osoi_ice / wx
          td = tfrz - t_soisno
          dexpf = exp (negate 0.04 * td)

          -- Destructive metamorphism
          c3 = 2.777e-6
          ddz1_base = negate c3 * dexpf
          ddz1_dens = if bi > upplim_destruct_metamorph params
                      then ddz1_base * exp (negate 46.0e-3 * (bi - upplim_destruct_metamorph params))
                      else ddz1_base
          ddz1 = if h2osoi_liq > 0.01 * dz * frac_sno
                 then ddz1_dens * 2.0  -- c5 = 2.0
                 else ddz1_dens

          -- Overburden compaction
          ddz2 = case overburdenMethod params of
            Anderson1976 ->
              overburdenCompactionAnderson1976
                (eta0_anderson params) (overburden_compress_tfactor params)
                burden wx td bi
            Vionnet2012 ->
              overburdenCompactionVionnet2012 params h2osoi_liq dz burden wx td bi

          -- Melt compaction
          ddz3
            | imelt /= 1 = 0.0
            | is_lake_or_urban =
                negate (1.0 / dtime) * max 0.0 ((frac_iceold - fi) / frac_iceold)
            | otherwise =
                let raw = max 0.0 (min 1.0 ((swe_old - wx) / wx))
                    adj = if swe_old - wx > 0.0
                          then let fsno_melt0 = if int_snow > 0.0
                                                then min 1.0 (wsum / int_snow)
                                                else 1.0
                                   fsno_melt = if fsno_melt0 + frac_h2osfc > 1.0
                                               then 1.0 - frac_h2osfc
                                               else fsno_melt0
                               in  raw - max 0.0 ((fsno_melt - frac_sno) / frac_sno)
                          else raw
                in  negate (1.0 / dtime) * adj

          -- Wind drift compaction
          (ddz4, zpseudo', mobile') =
            if windDependentSnowDensity params
            then windDriftCompaction params bi forc_wind dz zpseudo mobile
            else (0.0, zpseudo, mobile)

          pdzdtc = ddz1 + ddz2 + ddz3 + ddz4

      in (pdzdtc, burden', zpseudo', mobile')
    else
      -- No compaction for this layer (insufficient void/ice)
      (0.0, burden', zpseudo, False)

-- =========================================================================
-- Combo: combining two snow elements
-- =========================================================================

-- | Combine two snow elements. Returns (dz, wliq, wice, t).
combo
  :: Double -> Double -> Double -> Double  -- ^ Element 1: dz, wliq, wice, t
  -> Double -> Double -> Double -> Double  -- ^ Element 2: dz2, wliq2, wice2, t2
  -> (Double, Double, Double, Double)      -- ^ Combined: (dzc, wliqc, wicec, tc)
combo dz1 wliq1 wice1 t1 dz2 wliq2 wice2 t2 =
  let dzc   = dz1 + dz2
      wicec = wice1 + wice2
      wliqc = wliq1 + wliq2
      h1    = (cpice * wice1 + cpliq * wliq1) * (t1 - tfrz) + hfus * wliq1
      h2    = (cpice * wice2 + cpliq * wliq2) * (t2 - tfrz) + hfus * wliq2
      hc    = h1 + h2
      denom = cpice * wicec + cpliq * wliqc
      tc    = if denom > 0.0
              then tfrz + (hc - hfus * wliqc) / denom
              else tfrz
  in (dzc, wliqc, wicec, tc)

-- | Scalar version of combo (same as combo, kept for API parity).
comboScalar
  :: Double -> Double -> Double -> Double
  -> Double -> Double -> Double -> Double
  -> (Double, Double, Double, Double)
comboScalar = combo

-- | Mass-weighted snow grain radius when combining two layers.
massWeightedSnowRadius
  :: Double  -- ^ snw_rds_min
  -> Double  -- ^ rds1: radius of element 1 [microns]
  -> Double  -- ^ rds2: radius of element 2 [microns]
  -> Double  -- ^ swtot: total mass of element 1 [kg/m2]
  -> Double  -- ^ zwtot: total mass of element 2 [kg/m2]
  -> Double  -- ^ Combined radius [microns]
massWeightedSnowRadius rds_min rds1 rds2 swtot zwtot =
  let r = (rds2 * swtot + rds1 * zwtot) / (swtot + zwtot)
  in  max rds_min (min snwRdsMax r)

-- =========================================================================
-- Combine snow layers
-- =========================================================================

-- | Combine snow layers that are too thin or too light.
-- Returns updated SnowLayerState plus (snow_depth, frac_sno, frac_sno_eff, int_snow, h2osno_no_layers).
combineSnowLayers
  :: SnowLayerBounds
  -> SnowLayerState
  -> Double  -- ^ frac_sno
  -> Double  -- ^ frac_sno_eff
  -> Double  -- ^ int_snow
  -> Double  -- ^ h2osno_no_layers
  -> Double  -- ^ snow_depth
  -> Int     -- ^ lun_itype
  -> Bool    -- ^ urbpoi
  -> (SnowLayerState, Double, Double, Double, Double, Double)
     -- ^ (state', snow_depth', frac_sno', frac_sno_eff', int_snow', h2osno_no_layers')
combineSnowLayers bounds st frac_sno frac_sno_eff int_snow h2osno_no_layers snow_depth lun_itype urbpoi =
  let msno = abs (slSnl st)

      -- Phase 1: Remove layers with very little ice (< 0.01 kg/m2)
      -- This is done iteratively, working from bottom (msno-1) to top (0)
      (st1, msno1) = removeThinLayers st msno lun_itype urbpoi

      -- Recompute snow_depth and h2osno_total
      (snow_depth1, h2osno_total1) = computeSnowTotals st1 msno1

      -- Check if all snow should be collapsed to no-layer state
      (st2, msno2, snow_depth2, frac_sno2, frac_sno_eff2, int_snow2, h2osno_no_layers2) =
        checkCollapseToNoLayer bounds st1 msno1 snow_depth1 h2osno_total1
          frac_sno frac_sno_eff int_snow h2osno_no_layers lun_itype urbpoi

      -- Phase 2: Combine thin neighboring layers (when >= 2 layers)
      (st3, msno3) = combineThinNeighbors bounds st2 msno2 frac_sno_eff2

      -- Update snl and recompute z/zi
      st4 = recomputeZiZ (st3 { slSnl = negate msno3 })

  in (st4, snow_depth2, frac_sno2, frac_sno_eff2, int_snow2, h2osno_no_layers2)

-- Helper: remove layers with ice < 0.01, shifting layers down
removeThinLayers :: SnowLayerState -> Int -> Int -> Bool -> (SnowLayerState, Int)
removeThinLayers st msno _lun_itype _urbpoi =
  -- Simple implementation: iterate through layers, remove any with ice < 0.01
  go st msno 0
  where
    go s m j
      | j >= m = (s, m)
      | VU.unsafeIndex (slH2osoiIce s) j <= 0.01 =
          -- Transfer water to layer below if possible
          let liq_j = VU.unsafeIndex (slH2osoiLiq s) j
              ice_j = VU.unsafeIndex (slH2osoiIce s) j
              dz_j  = VU.unsafeIndex (slDz s) j
              -- Shift all layers from j+1..m-1 up by one
              s' = if j < m - 1
                   then let liq' = VU.imap (\i v -> if i == j + 1
                                                    then v + liq_j
                                                    else v) (slH2osoiLiq s)
                            ice' = VU.imap (\i v -> if i == j + 1
                                                    then v + ice_j
                                                    else v) (slH2osoiIce s)
                            dz'  = VU.imap (\i v -> if i == j + 1
                                                    then v + dz_j
                                                    else v) (slDz s)
                            -- Shift layers above j down
                            shift vec = VU.generate nlevsno $ \i ->
                              if i < j then VU.unsafeIndex vec i
                              else if i < m - 1 then VU.unsafeIndex vec (i + 1)
                              else 0.0
                        in  s { slH2osoiLiq = shift liq'
                              , slH2osoiIce = shift ice'
                              , slDz        = shift dz'
                              , slTSoisno   = shift (slTSoisno s)
                              , slSnwRds    = shift (slSnwRds s)
                              }
                   else s  -- last layer, just drop it
              m' = m - 1
          in go s' m' j  -- re-check same index
      | otherwise = go s m (j + 1)

-- Helper: compute snow_depth and h2osno_total from active layers
computeSnowTotals :: SnowLayerState -> Int -> (Double, Double)
computeSnowTotals st msno =
  let go_sum j (depth_acc, swe_acc)
        | j >= msno = (depth_acc, swe_acc)
        | otherwise =
            let dz_j  = VU.unsafeIndex (slDz st) j
                ice_j = VU.unsafeIndex (slH2osoiIce st) j
                liq_j = VU.unsafeIndex (slH2osoiLiq st) j
            in  go_sum (j+1) (depth_acc + dz_j, swe_acc + ice_j + liq_j)
  in go_sum 0 (0.0, 0.0)

-- Helper: check whether to collapse snowpack to no-layer state
checkCollapseToNoLayer
  :: SnowLayerBounds -> SnowLayerState -> Int
  -> Double -> Double -> Double -> Double -> Double -> Double
  -> Int -> Bool
  -> (SnowLayerState, Int, Double, Double, Double, Double, Double)
checkCollapseToNoLayer bounds st msno snow_depth h2osno_total
  frac_sno frac_sno_eff int_snow h2osno_no_layers lun_itype _urbpoi =
  let dzmin1 = VU.unsafeIndex (snowDzmin bounds) 0
      -- Bulk density of the whole snow pack (over the snow-covered fraction).
      -- A physically real snowpack has density >= bifall_min (~50 kg/m3). When
      -- frac_sno_eff is tiny, the SL2012 in-patch snow_depth (= grid-mean/frac_sno_eff)
      -- inflates to tens of metres, so an explicit layer can be created with an
      -- absurdly low bulk density (<< 50). Fortran sheds such snow via the mass<0.01
      -- removeThinLayers path before it ever reaches this state; our layer survives
      -- (mass>0.01) and a single such layer is never combined (needs >=2 layers).
      -- Collapse it back to the no-layer state, resetting snow_depth to the physical
      -- density floor so the next step re-forms a real (density-floor) layer instead
      -- of persisting a 70 m massless slab that crashes T_GRND. Threshold 50 kg/m3
      -- matches Fortran CombineSnowLayers' low-density combine criterion.
      packDz = snow_depth  -- = sum(dz) over snow layers when snl<0
      bulkDensity
        | frac_sno_eff > 0.0 && packDz > 0.0 = h2osno_total / (frac_sno_eff * packDz)
        | otherwise = 1.0e30
      shouldCollapse = h2osno_total > 0.0 && bulkDensity < 50.0
  in
    if shouldCollapse
    then
      -- Collapse: sum ice+liq -> h2osno_no_layers; reset snow_depth to the density
      -- floor (h2osno/denice)/frac_sno_eff (minimum physical in-patch depth).
      let sumWat j acc
            | j >= msno = acc
            | otherwise = sumWat (j+1)
                (acc + VU.unsafeIndex (slH2osoiIce st) j
                     + VU.unsafeIndex (slH2osoiLiq st) j)
          h2osno_no_layers' = sumWat 0 0.0
          snow_depth'
            | h2osno_no_layers' <= 0.0 = 0.0
            | frac_sno_eff > 0.0       = (h2osno_no_layers' / denice) / frac_sno_eff
            | otherwise                = 0.0
          st' = emptySnowLayerState
      in (st', 0, snow_depth', frac_sno, frac_sno_eff, int_snow, h2osno_no_layers')
    else if h2osno_total <= 0.0
    then (st, msno, 0.0, 0.0, 0.0, 0.0, h2osno_no_layers)
    else (st, msno, snow_depth, frac_sno, frac_sno_eff, int_snow, h2osno_no_layers)

-- Helper: combine thin neighboring layers
combineThinNeighbors :: SnowLayerBounds -> SnowLayerState -> Int -> Double -> (SnowLayerState, Int)
combineThinNeighbors bounds st msno frac_sno_eff
  | msno < 2 = (st, msno)
  | otherwise = go st msno 0 1
  where
    dzminV = snowDzmin bounds

    go s m i mssi
      | i >= m || m < 2 = (s, m)
      | otherwise =
          let dz_i = VU.unsafeIndex (slDz s) i
              ice_i = VU.unsafeIndex (slH2osoiIce s) i
              liq_i = VU.unsafeIndex (slH2osoiLiq s) i
              dzminI = if mssi <= VU.length dzminV
                       then VU.unsafeIndex dzminV (mssi - 1)
                       else VU.unsafeIndex dzminV (VU.length dzminV - 1)
              wt_i = ice_i + liq_i
              needsCombine = (frac_sno_eff * dz_i < dzminI) ||
                             (wt_i / (frac_sno_eff * dz_i) < 50.0)
          in
            if needsCombine
            then
              let -- Choose neighbor to combine with
                  neibor
                    | i == 0     = i + 1
                    | i == m - 1 = i - 1
                    | otherwise  =
                        let dz_im1 = VU.unsafeIndex (slDz s) (i - 1)
                            dz_ip1 = VU.unsafeIndex (slDz s) (i + 1)
                        in  if dz_im1 + dz_i < dz_ip1 + dz_i then i - 1 else i + 1
                  (j_keep, l_remove) = if neibor > i then (neibor, i) else (i, neibor)

                  -- Combine elements
                  dz_k  = VU.unsafeIndex (slDz s) j_keep
                  liq_k = VU.unsafeIndex (slH2osoiLiq s) j_keep
                  ice_k = VU.unsafeIndex (slH2osoiIce s) j_keep
                  t_k   = VU.unsafeIndex (slTSoisno s) j_keep
                  dz_r  = VU.unsafeIndex (slDz s) l_remove
                  liq_r = VU.unsafeIndex (slH2osoiLiq s) l_remove
                  ice_r = VU.unsafeIndex (slH2osoiIce s) l_remove
                  t_r   = VU.unsafeIndex (slTSoisno s) l_remove

                  -- Mass-weighted radius
                  rds_k = VU.unsafeIndex (slSnwRds s) j_keep
                  rds_r = VU.unsafeIndex (slSnwRds s) l_remove
                  total_mass = liq_k + ice_k + liq_r + ice_r
                  rds_combined = if total_mass > 0.0
                                 then (rds_k * (liq_k + ice_k) + rds_r * (liq_r + ice_r))
                                      / total_mass
                                 else rds_k

                  (dzc, wliqc, wicec, tc) = combo dz_k liq_k ice_k t_k dz_r liq_r ice_r t_r

                  -- Write combined into j_keep, then shift layers above l_remove
                  s' = let update vec val idx = VU.imap (\ii v -> if ii == idx then val else v) vec
                           s1 = s { slDz        = update (slDz s) dzc j_keep
                                  , slH2osoiLiq = update (slH2osoiLiq s) wliqc j_keep
                                  , slH2osoiIce = update (slH2osoiIce s) wicec j_keep
                                  , slTSoisno   = update (slTSoisno s) tc j_keep
                                  , slSnwRds    = update (slSnwRds s) rds_combined j_keep
                                  }
                           -- Shift: remove l_remove, shift higher layers down
                           shiftRem vec = VU.generate nlevsno $ \ii ->
                             if ii < l_remove then VU.unsafeIndex vec ii
                             else if ii < m - 1 then VU.unsafeIndex vec (ii + 1)
                             else 0.0
                       in  s1 { slDz        = shiftRem (slDz s1)
                              , slH2osoiLiq = shiftRem (slH2osoiLiq s1)
                              , slH2osoiIce = shiftRem (slH2osoiIce s1)
                              , slTSoisno   = shiftRem (slTSoisno s1)
                              , slSnwRds    = shiftRem (slSnwRds s1)
                              }
                  m' = m - 1
              in  if m' < 2
                  then (s', m')
                  else go s' m' i mssi  -- re-check same position
            else go s m (i + 1) (mssi + 1)

-- Helper: recompute z and zi from dz (top-down)
recomputeZiZ :: SnowLayerState -> SnowLayerState
recomputeZiZ st =
  let msno = abs (slSnl st)
      -- zi[msno] = 0.0 (surface), zi[j] = zi[j+1] - dz[j]
      -- Working from bottom (index msno-1) to top (index 0)
      ziSurface = 0.0

      ziPairs = (msno, ziSurface) : buildZi msno (slDz st) (msno - 1) ziSurface []

      ziVec = VU.generate (nlevsno + 1) $ \idx ->
        case lookup idx ziPairs of
          Just v  -> v
          Nothing -> 0.0

      zVec = VU.generate nlevsno $ \j ->
        if j < msno
        then let zi_below = VU.unsafeIndex ziVec (j + 1)
                 dz_j = VU.unsafeIndex (slDz st) j
             in  zi_below - 0.5 * dz_j
        else 0.0

  in st { slZ = zVec, slZi = ziVec }

-- Helper: build zi pairs list from bottom to top
buildZi :: Int -> VU.Vector Double -> Int -> Double -> [(Int, Double)] -> [(Int, Double)]
buildZi msno dzVec j ziBelow acc
  | j < 0     = acc
  | j >= msno = buildZi msno dzVec (j-1) ziBelow acc
  | otherwise =
      let dz_j = VU.unsafeIndex dzVec j
          zi_j = ziBelow - dz_j
      in  buildZi msno dzVec (j-1) zi_j ((j, zi_j) : acc)

-- =========================================================================
-- Divide snow layers
-- =========================================================================

-- | Subdivide snow layers that exceed their prescribed maximum thickness.
-- Returns updated SnowLayerState.
divideSnowLayers
  :: SnowLayerBounds
  -> SnowLayerState
  -> Double  -- ^ frac_sno
  -> Bool    -- ^ is_lake
  -> SnowLayerState
divideSnowLayers bounds st frac_sno is_lake =
  let msno0 = abs (slSnl st)
      dzmaxLV = snowDzmaxL bounds
      dzmaxUV = snowDzmaxU bounds

      -- Copy to local arrays (area-weighted for non-lake)
      toDzsno j = let dz_j = VU.unsafeIndex (slDz st) j
                  in  if is_lake then dz_j else frac_sno * dz_j

      dzsno0 = VU.generate nlevsno $ \j -> if j < msno0 then toDzsno j else 0.0
      swice0 = slH2osoiIce st
      swliq0 = slH2osoiLiq st
      tsno0  = slTSoisno st
      rds0   = slSnwRds st

      -- Process layers: traverse top to bottom
      (msno_f, dzsno_f, swice_f, swliq_f, tsno_f, rds_f) =
        divideLayers dzmaxLV dzmaxUV is_lake msno0 dzsno0 swice0 swliq0 tsno0 rds0

      -- Copy back (divide by frac_sno for non-lake)
      toDz j dz_j = if j < msno_f
                     then if is_lake then dz_j else dz_j / frac_sno
                     else 0.0

      st' = st { slSnl       = negate msno_f
               , slDz        = VU.imap toDz dzsno_f
               , slH2osoiIce = swice_f
               , slH2osoiLiq = swliq_f
               , slTSoisno   = tsno_f
               , slSnwRds    = rds_f
               }

  in recomputeZiZ st'

-- Helper for divide_snow_layers: iterate through layers
divideLayers
  :: VU.Vector Double -> VU.Vector Double -> Bool
  -> Int
  -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
  -> VU.Vector Double -> VU.Vector Double
  -> (Int, VU.Vector Double, VU.Vector Double, VU.Vector Double,
      VU.Vector Double, VU.Vector Double)
divideLayers dzmaxLV dzmaxUV is_lake msno0 dzsno0 swice0 swliq0 tsno0 rds0 =
  go 0 msno0 dzsno0 swice0 swliq0 tsno0 rds0
  where
    upd :: VU.Vector Double -> Int -> Double -> VU.Vector Double
    upd vec i val = VU.imap (\j v -> if j == i then val else v) vec

    go k msno dzsno swice swliq tsno rds
      | k >= msno || k >= nlevsno = (msno, dzsno, swice, swliq, tsno, rds)
      | k == msno - 1 =
          -- Bottom layer case
          let offset = if is_lake then 2.0 * lsadz else 0.0
              dzmaxL_k = VU.unsafeIndex dzmaxLV k
          in  if VU.unsafeIndex dzsno k > dzmaxL_k + offset && msno < nlevsno
              then
                let dz_k = VU.unsafeIndex dzsno k
                    ice_k = VU.unsafeIndex swice k
                    liq_k = VU.unsafeIndex swliq k
                    t_k   = VU.unsafeIndex tsno k
                    r_k   = VU.unsafeIndex rds k

                    dz_half = dz_k / 2.0
                    ice_half = ice_k / 2.0
                    liq_half = liq_k / 2.0
                    -- Temperature for new layer
                    t_new = t_k  -- simple split for bottom-most

                    dzsno' = upd (upd dzsno k dz_half) (k+1) dz_half
                    swice' = upd (upd swice k ice_half) (k+1) ice_half
                    swliq' = upd (upd swliq k liq_half) (k+1) liq_half
                    tsno'  = upd tsno (k+1) t_new
                    rds'   = upd rds (k+1) r_k

                in  go (k+1) (msno+1) dzsno' swice' swliq' tsno' rds'
              else (msno, dzsno, swice, swliq, tsno, rds)
      | otherwise =
          -- Non-bottom layer: check upper max thickness
          let offset = if is_lake then lsadz else 0.0
              dzmaxU_k = VU.unsafeIndex dzmaxUV k
          in  if VU.unsafeIndex dzsno k > dzmaxU_k + offset
              then
                let dz_k  = VU.unsafeIndex dzsno k
                    ice_k = VU.unsafeIndex swice k
                    liq_k = VU.unsafeIndex swliq k
                    t_k   = VU.unsafeIndex tsno k
                    r_k   = VU.unsafeIndex rds k

                    drr = dz_k - dzmaxU_k - offset
                    propor = drr / dz_k
                    propor_keep = (dzmaxU_k + offset) / dz_k

                    zwice = propor * ice_k
                    zwliq = propor * liq_k

                    ice_k' = ice_k * propor_keep
                    liq_k' = liq_k * propor_keep
                    dz_k'  = dzmaxU_k + offset

                    -- Combine excess with layer below
                    ice_kp1 = VU.unsafeIndex swice (k+1)
                    liq_kp1 = VU.unsafeIndex swliq (k+1)
                    dz_kp1  = VU.unsafeIndex dzsno (k+1)
                    t_kp1   = VU.unsafeIndex tsno (k+1)
                    r_kp1   = VU.unsafeIndex rds (k+1)

                    r_combined = massWeightedSnowRadius 54.526
                                   r_k r_kp1 (liq_kp1 + ice_kp1) (zwliq + zwice)

                    (dz_c, liq_c, ice_c, t_c) =
                      combo dz_kp1 liq_kp1 ice_kp1 t_kp1 drr zwliq zwice t_k

                    dzsno' = upd (upd dzsno k dz_k') (k+1) dz_c
                    swice' = upd (upd swice k ice_k') (k+1) ice_c
                    swliq' = upd (upd swliq k liq_k') (k+1) liq_c
                    tsno'  = upd tsno (k+1) t_c
                    rds'   = upd (upd rds k r_k) (k+1) r_combined

                in  go (k+1) msno dzsno' swice' swliq' tsno' rds'
              else go (k+1) msno dzsno swice swliq tsno rds

-- =========================================================================
-- Snow percolation
-- =========================================================================

-- | Calculate liquid percolation through snow pack.
-- Returns updated h2osoi_liq vector and drainage rate from bottom [mm/s].
snowPercolation
  :: SnowHydrologyParams
  -> Double  -- ^ dtime [s]
  -> Double  -- ^ frac_sno_eff
  -> SnowLayerState
  -> (VU.Vector Double, Double)
     -- ^ (h2osoi_liq', qflx_snow_drain [mm/s])
snowPercolation params dtime frac_sno_eff st =
  let msno = abs (slSnl st)
      dz_v  = slDz st
      ice_v = slH2osoiIce st
      liq_v = slH2osoiLiq st

      -- Compute porosity and volume fractions per layer
      vol_ice j = min 1.0 (VU.unsafeIndex ice_v j
                           / (VU.unsafeIndex dz_v j * frac_sno_eff * denice))
      eff_por j = 1.0 - vol_ice j
      vol_liq j = min (eff_por j)
                      (VU.unsafeIndex liq_v j
                       / (VU.unsafeIndex dz_v j * frac_sno_eff * denh2o))

      -- Compute percolation for each layer (top to bottom, index 0 = top)
      -- qperc[j] = drainage out of bottom of layer j [mm]
      computePerc :: Int -> [Double] -> [Double]
      computePerc j acc
        | j >= msno = reverse acc
        | otherwise =
            let ep_j = eff_por j
                vl_j = vol_liq j
                vi_j = vol_ice j
                dz_j = VU.unsafeIndex dz_v j
                qperc
                  | j < msno - 1 =
                      let ep_jp1 = eff_por (j + 1)
                          vi_jp1 = vol_ice (j + 1)
                          vl_jp1 = vol_liq (j + 1)
                      in  if ep_j < wimp params || ep_jp1 < wimp params
                          then 0.0
                          else let q1 = max 0.0 ((vl_j - ssi params * ep_j) *
                                          dz_j * frac_sno_eff)
                                   q2 = (1.0 - vi_jp1 - vl_jp1) *
                                        VU.unsafeIndex dz_v (j + 1) * frac_sno_eff
                               in  min q1 q2
                  | otherwise =
                      -- Bottom layer
                      max 0.0 ((vl_j - ssi params * ep_j) * dz_j * frac_sno_eff)
                -- Convert from m to mm H2O/s
                qperc_mms = qperc * 1000.0 / dtime
            in  computePerc (j + 1) (qperc_mms : acc)

      percRates = VU.fromList (computePerc 0 [])

      -- Update h2osoi_liq: subtract outflow, add inflow from above
      liq' = VU.generate nlevsno $ \j ->
        if j >= msno then VU.unsafeIndex liq_v j
        else
          let base = VU.unsafeIndex liq_v j
              outflow = VU.unsafeIndex percRates j * dtime
              inflow = if j > 0 then VU.unsafeIndex percRates (j - 1) * dtime else 0.0
          in  base + inflow - outflow

      -- Drainage from bottom layer
      qflx_drain = if msno > 0 then VU.unsafeIndex percRates (msno - 1) else 0.0

  in (liq', qflx_drain)

-- =========================================================================
-- Zero empty snow layers
-- =========================================================================

-- | Set empty (inactive) snow layers to zero.
zeroEmptySnowLayers :: SnowLayerState -> SnowLayerState
zeroEmptySnowLayers st =
  let msno = abs (slSnl st)
      zeroInactive vec = VU.imap (\j v -> if j >= msno then 0.0 else v) vec
  in  st { slDz        = zeroInactive (slDz st)
         , slZ         = zeroInactive (slZ st)
         , slTSoisno   = zeroInactive (slTSoisno st)
         , slH2osoiIce = zeroInactive (slH2osoiIce st)
         , slH2osoiLiq = zeroInactive (slH2osoiLiq st)
         }

-- =========================================================================
-- Init snow layers (cold start)
-- =========================================================================

-- | Initialize snow layer thickness from specified total depth.
-- Returns (SnowLayerState, snl).
initSnowLayers
  :: SnowLayerBounds
  -> Double  -- ^ snow_depth [m]
  -> Double  -- ^ forc_t [K] (for initial temperature)
  -> Bool    -- ^ is_lake
  -> SnowLayerState
initSnowLayers bounds snow_depth forc_t is_lake =
  let dzminV  = snowDzmin bounds
      dzmaxUV = snowDzmaxU bounds
      dzmaxLV = snowDzmaxL bounds
      dzmin1  = VU.unsafeIndex dzminV 0
  in
    if is_lake || snow_depth < dzmin1
    then emptySnowLayerState
    else
      let -- Determine number of layers
          findLayers :: Int -> Int
          findLayers nlyr
            | nlyr >= nlevsno = nlevsno
            | otherwise =
                let maxBound = VU.sum (VU.slice 0 nlyr dzmaxUV)
                in  if snow_depth <= maxBound then nlyr
                    else findLayers (nlyr + 1)

          msno = if snow_depth <= VU.unsafeIndex dzmaxLV 0
                 then 1
                 else findLayers 2

          -- Set layer thicknesses
          dzVec = VU.generate nlevsno $ \k ->
            if k >= msno then 0.0
            else if msno == 1 then snow_depth
            else if k < msno - 2 then VU.unsafeIndex dzmaxUV k
            else if k == msno - 2 then
              -- Second-to-last layer
              let used = VU.sum (VU.slice 0 (msno - 2) dzmaxUV)
                  remaining = snow_depth - used
                  dzu_m1 = VU.unsafeIndex dzmaxUV (msno - 2)
              in  if remaining <= 2.0 * dzu_m1
                  then remaining / 2.0
                  else dzu_m1
            else
              -- Last layer: whatever is left
              let used = VU.sum (VU.slice 0 (msno - 2) dzmaxUV)
                  remaining = snow_depth - used
                  dzu_m1 = VU.unsafeIndex dzmaxUV (msno - 2)
              in  if remaining <= 2.0 * dzu_m1
                  then remaining / 2.0
                  else remaining - dzu_m1

          tInit = min tfrz forc_t

          st0 = SnowLayerState
            { slDz        = dzVec
            , slZ         = VU.replicate nlevsno 0.0
            , slZi        = VU.replicate (nlevsno + 1) 0.0
            , slTSoisno   = VU.generate nlevsno $ \k ->
                              if k < msno then tInit else 0.0
            , slH2osoiIce = VU.replicate nlevsno 0.0
            , slH2osoiLiq = VU.replicate nlevsno 0.0
            , slSnwRds    = VU.generate nlevsno $ \k ->
                              if k < msno then 54.526 else 0.0
            , slSnl       = negate msno
            }

      in recomputeZiZ st0

-- =========================================================================
-- Build snow filter
-- =========================================================================

-- | Build snow/no-snow masks from snl values.
-- Returns (mask_snow, mask_nosnow) as Bool vectors.
buildSnowFilter
  :: VU.Vector Int   -- ^ snl per column (negative = has snow)
  -> VU.Vector Bool  -- ^ mask_nolake per column
  -> (VU.Vector Bool, VU.Vector Bool)
     -- ^ (mask_snow, mask_nosnow)
buildSnowFilter snlVec maskNolake =
  let n = VU.length snlVec
      mkSnow = VU.generate n $ \c ->
        VU.unsafeIndex maskNolake c && VU.unsafeIndex snlVec c < 0
      mkNosnow = VU.generate n $ \c ->
        VU.unsafeIndex maskNolake c && VU.unsafeIndex snlVec c >= 0
  in (mkSnow, mkNosnow)

-- =========================================================================
-- Snow capping excess
-- =========================================================================

-- | Determine excess snow that needs to be capped.
-- Returns (h2osno_excess, apply_runoff).
snowCappingExcess
  :: Double  -- ^ h2osno [mm]
  -> Double  -- ^ h2osno_excess
snowCappingExcess h2osno =
  let h2osno_max_val = 10000.0
  in  if h2osno > h2osno_max_val
      then h2osno - h2osno_max_val
      else 0.0

-- =========================================================================
-- Add new snow to column (Fortran: UpdateQuantitiesForNewSnow)
-- =========================================================================

data AddNewSnowInput = AddNewSnowInput
  { ansi_qflx_snow_grnd :: !Double  -- ^ snowfall flux reaching ground (mm/s = kg/m2/s)
  , ansi_forc_t         :: !Double  -- ^ atmospheric temperature (K)
  , ansi_frac_sno_eff   :: !Double  -- ^ effective snow-covered fraction
  , ansi_bifall         :: !Double  -- ^ new snow bulk density (kg/m3)
  , ansi_dt             :: !Double  -- ^ timestep (s)
  , ansi_state          :: !SnowLayerState
  } deriving (Show)

-- | Add new snowfall to the top snow layer.
-- Updates ice mass, temperature (mass-weighted), and snow depth.
addNewSnowToColumn :: AddNewSnowInput -> SnowLayerState
addNewSnowToColumn !inp =
  let !st = ansi_state inp
      !dt = ansi_dt inp
      !newsnow = ansi_qflx_snow_grnd inp * dt  -- kg/m2
      !snl_val = slSnl st
  in if newsnow <= 0.0 || snl_val >= 0
     then st
     else let !topIdx = nlevsno + snl_val  -- 0-based top active layer
              !old_ice = slH2osoiIce st VU.! topIdx
              !old_t = slTSoisno st VU.! topIdx
              !old_mass = old_ice + (slH2osoiLiq st VU.! topIdx)
              !t_new = min tfrz (ansi_forc_t inp)
              -- Mass-weighted temperature
              !new_mass = old_mass + newsnow
              !new_t = if new_mass > 0.0
                       then (old_mass * old_t + newsnow * t_new) / new_mass
                       else t_new
              -- Update ice and temperature
              !ice' = (slH2osoiIce st) VU.// [(topIdx, old_ice + newsnow)]
              !t' = (slTSoisno st) VU.// [(topIdx, new_t)]
              -- Update snow depth
              !dz_add = newsnow / ansi_bifall inp
              !dz' = (slDz st) VU.// [(topIdx, slDz st VU.! topIdx + dz_add)]
          in st { slH2osoiIce = ice', slTSoisno = t', slDz = dz' }

-- =========================================================================
-- Remove snow from thawed wetlands (Fortran: RemoveSnowFromThawedWetlands)
-- =========================================================================

-- | Remove thin snowpack from thawed wetlands.
-- If the column has thin snow (< threshold) and ground temperature > tfrz,
-- convert snow to liquid runoff.
removeSnowFromThawedWetlands :: Double  -- ^ t_grnd (K)
                             -> Double  -- ^ h2osno (mm)
                             -> Double  -- ^ threshold (mm, typically 1.0)
                             -> SnowLayerState
                             -> (SnowLayerState, Double)  -- ^ (updated state, qflx_snomelt)
removeSnowFromThawedWetlands !t_grnd !h2osno !threshold !st
  | t_grnd > tfrz && h2osno > 0.0 && h2osno < threshold =
    (emptySnowLayerState, h2osno)  -- all snow becomes melt
  | otherwise = (st, 0.0)

-- =========================================================================
-- Snow water main driver (Fortran: SnowWater)
-- =========================================================================

data SnowWaterInput = SnowWaterInput
  { swi_state            :: !SnowLayerState
  , swi_qflx_snow_grnd   :: !Double  -- ^ snowfall to ground (mm/s)
  , swi_qflx_snomelt     :: !Double  -- ^ snow melt rate (mm/s)
  , swi_qflx_rain_grnd   :: !Double  -- ^ rain reaching ground (mm/s)
  , swi_qflx_dew_snow    :: !Double  -- ^ dew deposition to snow (mm/s)
  , swi_qflx_sub_snow    :: !Double  -- ^ sublimation from snow (mm/s)
  , swi_forc_t            :: !Double  -- ^ atmospheric temperature (K)
  , swi_frac_sno_eff      :: !Double
  , swi_bifall            :: !Double  -- ^ new snow density (kg/m3)
  , swi_dt                :: !Double
  , swi_params            :: !SnowHydrologyParams
  } deriving (Show)

data SnowWaterOutput = SnowWaterOutput
  { swo_state             :: !SnowLayerState
  , swo_qflx_snow_drain   :: !Double  -- ^ meltwater draining from snowpack (mm/s)
  , swo_qflx_rain_plus_melt :: !Double  -- ^ rain + snowmelt reaching soil (mm/s)
  , swo_h2osno_updated    :: !Double  -- ^ updated total SWE (mm)
  } deriving (Show)

-- | Main snow water driver. Orchestrates:
--   1. Add new snow and dew/sublimation to top layer
--   2. Snow water percolation through layers
--   3. Sum bottom percolation as snow drain
--   4. Adjust layer thicknesses
snowWaterDriver :: SnowWaterInput -> SnowWaterOutput
snowWaterDriver !inp =
  let !st0 = swi_state inp
      !dt = swi_dt inp
      !snl_val = slSnl st0

  in if snl_val >= 0
     then -- No snow layers: all precip goes to soil
       SnowWaterOutput
         { swo_state = st0
         , swo_qflx_snow_drain = 0.0
         , swo_qflx_rain_plus_melt = swi_qflx_rain_grnd inp + swi_qflx_snomelt inp
         , swo_h2osno_updated = 0.0
         }
     else let
       -- Step 1: Add new snow to top layer
       !st1 = addNewSnowToColumn AddNewSnowInput
         { ansi_qflx_snow_grnd = swi_qflx_snow_grnd inp
         , ansi_forc_t = swi_forc_t inp
         , ansi_frac_sno_eff = swi_frac_sno_eff inp
         , ansi_bifall = swi_bifall inp
         , ansi_dt = dt
         , ansi_state = st0
         }

       -- Step 2: Add dew, remove sublimation from top layer
       !topIdx = nlevsno + snl_val
       !dew_add = swi_qflx_dew_snow inp * dt * swi_frac_sno_eff inp
       !sub_remove = swi_qflx_sub_snow inp * dt * swi_frac_sno_eff inp
       !ice_top = (slH2osoiIce st1 VU.! topIdx) + dew_add - sub_remove
       !ice_top_clamped = max 0.0 ice_top
       !st2 = st1 { slH2osoiIce = (slH2osoiIce st1) VU.// [(topIdx, ice_top_clamped)] }

       -- Step 3: Snow percolation (liquid water flow through layers)
       !nactive = negate snl_val
       !(_liq_perc, !percolation_bottom) = snowPercolation (swi_params inp) dt (swi_frac_sno_eff inp) st2
       !st3 = st2 { slH2osoiLiq = _liq_perc }

       -- Step 4: Compute output fluxes
       !qflx_snow_drain = percolation_bottom / dt
       !qflx_rain_plus_melt = swi_qflx_rain_grnd inp + qflx_snow_drain

       -- Step 5: Update total SWE
       !h2osno = VU.sum (VU.slice topIdx nactive (slH2osoiIce st3))
              + VU.sum (VU.slice topIdx nactive (slH2osoiLiq st3))

       in SnowWaterOutput
          { swo_state = st3
          , swo_qflx_snow_drain = qflx_snow_drain
          , swo_qflx_rain_plus_melt = qflx_rain_plus_melt
          , swo_h2osno_updated = h2osno
          }
