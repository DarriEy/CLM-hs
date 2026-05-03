{-# LANGUAGE BangPatterns #-}
-- | Soil hydrology (Richards equation solver).
-- Fortran: SoilHydrologyMod, SoilWaterMovementMod
--
-- Provides:
--   * Configuration types for soil water movement
--   * Clapp-Hornberger hydraulic properties (hk, smp, derivatives)
--   * Ice impedance factor
--   * Infiltration, surface runoff, drainage
--   * Water table computation
--   * Richards equation solver (Zeng-Decker 2009, moisture-form)
--   * Tridiagonal system assembly and solution
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SoilHydrology
  ( -- * Configuration
    SoilWaterMovementConfig(..)
  , defaultSoilWaterConfig
  , SoilHydrologyParams(..)
  , defaultSoilHydroParams
    -- * Solution method constants
  , SolnMethod(..)
  , BoundaryCondition(..)
    -- * Hydraulic property functions
  , iceImpedance
  , clappHornbergerHk
  , clappHornbergerSmp
  , computeHydraulicProperties
  , HydraulicProps(..)
    -- * Flux computation
  , computeMoistureFluxes
  , FluxResult(..)
    -- * Tridiagonal system
  , computeRHSMoistureForm
  , computeLHSMoistureForm
  , TridiagSystem(..)
    -- * Richards equation solvers
  , soilwaterMoistureForm
  , soilwaterZengDecker2009
  , soilWater
    -- * Soil hydrology subroutines
  , setSoilWaterFractions
  , SoilWaterFractions(..)
  , infiltration
  , computeQcharge
  , waterTable
  , WaterTableResult(..)
  , drainage
  , DrainageResult(..)
  , totalSurfaceRunoff
    -- * Water movement result
  , SoilWaterResult(..)
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (tfrz, denh2o, denice, nlevsoi, nlevsno)
import CLM.Infrastructure.Tridiagonal (tridiagonalSolve)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Minimum water content [kg/m^2]
watmin :: Double
watmin = 0.001

-- | Meters to millimeters
mToMM :: Double
mToMM = 1.0e3

-- | Pi
rpi :: Double
rpi = 3.14159265358979323846

-- | Minimum matric potential [mm]
smpmin :: Double
smpmin = -1.0e8

-- | Maximum ponding depth [mm]
pondmx :: Double
pondmx = 0.0

--------------------------------------------------------------------------------
-- Solution method and boundary condition types
--------------------------------------------------------------------------------

-- | Solution method for Richards equation
data SolnMethod
  = ZengDecker2009   -- ^ Zeng-Decker 2009 equilibrium method
  | MoistureForm     -- ^ Moisture-based Richards equation
  deriving (Show, Eq)

-- | Boundary condition type
data BoundaryCondition
  = BCFlux           -- ^ Specified flux (e.g. infiltration at top, free drainage at bottom)
  | BCZeroFlux       -- ^ Zero flux
  | BCWaterTable     -- ^ Water table boundary
  | BCAquifer        -- ^ Coupled aquifer layer
  | BCHead           -- ^ Specified head (not commonly used)
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration types
--------------------------------------------------------------------------------

-- | Parameters for soil hydrology (from params file)
data SoilHydrologyParams = SoilHydrologyParams
  { aq_sp_yield_min         :: !Double  -- ^ Minimum aquifer specific yield
  , n_baseflow              :: !Double  -- ^ Drainage power law exponent
  , perched_baseflow_scalar :: !Double  -- ^ Perched baseflow rate scalar [kg/m2/s]
  , e_ice_hydro             :: !Double  -- ^ Soil ice impedance factor for hydrology
  } deriving (Show, Eq)

-- | Default soil hydrology parameters
defaultSoilHydroParams :: SoilHydrologyParams
defaultSoilHydroParams = SoilHydrologyParams
  { aq_sp_yield_min         = 0.01
  , n_baseflow              = 1.0
  , perched_baseflow_scalar = 5.0e-4
  , e_ice_hydro             = 6.0
  }

-- | Configuration for soil water movement solver
data SoilWaterMovementConfig = SoilWaterMovementConfig
  { soilwater_movement_method :: !SolnMethod
  , upper_boundary_condition  :: !BoundaryCondition
  , lower_boundary_condition  :: !BoundaryCondition
  , dtmin           :: !Double  -- ^ Minimum substep length [s]
  , verySmall       :: !Double  -- ^ Tolerance for substep completion
  , xTolerUpper     :: !Double  -- ^ Upper error tolerance (halve substep)
  , xTolerLower     :: !Double  -- ^ Lower error tolerance (double substep)
  , e_ice           :: !Double  -- ^ Soil ice impedance factor
  , baseflow_scalar :: !Double  -- ^ Baseflow scalar
  } deriving (Show, Eq)

-- | Default configuration (Zeng-Decker 2009 with aquifer)
defaultSoilWaterConfig :: SoilWaterMovementConfig
defaultSoilWaterConfig = SoilWaterMovementConfig
  { soilwater_movement_method = ZengDecker2009
  , upper_boundary_condition  = BCFlux
  , lower_boundary_condition  = BCAquifer
  , dtmin       = 60.0
  , verySmall   = 1.0e-8
  , xTolerUpper = 1.0e-1
  , xTolerLower = 1.0e-2
  , e_ice       = 6.0
  , baseflow_scalar = 1.0e-2
  }

--------------------------------------------------------------------------------
-- Hydraulic property functions
--------------------------------------------------------------------------------

-- | Ice impedance factor: 10^(-e_ice * icefrac)
-- Ported from IceImpedance in SoilWaterMovementMod.F90
iceImpedance :: Double -> Double -> Double
iceImpedance icefrac eIce = 10.0 ** (negate eIce * icefrac)

-- | Clapp-Hornberger saturated hydraulic conductivity and derivative.
-- Returns (hk, dhkds) given relative saturation s_interface, impedance,
-- hksat, and bsw.
-- Ported from soil_hk for Clapp-Hornberger SWRC.
clappHornbergerHk :: Double  -- ^ Relative saturation at interface
                  -> Double  -- ^ Ice impedance factor
                  -> Double  -- ^ hksat [mm/s]
                  -> Double  -- ^ bsw (Clapp-Hornberger exponent)
                  -> (Double, Double)  -- ^ (hk, dhk/ds)
clappHornbergerHk s_raw imped hksat_val bsw_val =
  let s = clamp 0.01 1.0 s_raw
      exp1 = 2.0 * bsw_val + 3.0
      hk_unimpeded = hksat_val * s ** exp1
      hk_val = imped * hk_unimpeded
      dhkds = imped * exp1 * hksat_val * s ** (exp1 - 1.0)
  in (hk_val, dhkds)

-- | Clapp-Hornberger matric potential and derivative.
-- Returns (smp, dsmp/ds) given relative saturation s_node, sucsat, bsw.
-- Ported from soil_suction for Clapp-Hornberger SWRC.
clappHornbergerSmp :: Double  -- ^ Relative saturation at node
                   -> Double  -- ^ sucsat [mm]
                   -> Double  -- ^ bsw
                   -> (Double, Double)  -- ^ (smp, dsmp/ds)
clappHornbergerSmp s_raw sucsat_val bsw_val =
  let s = clamp 0.01 1.0 s_raw
      smp_val = negate sucsat_val * s ** (negate bsw_val)
      smp_clamped = max smpmin smp_val
      dsmpds = negate bsw_val * smp_clamped / s
  in (smp_clamped, dsmpds)

-- | Hydraulic properties for all layers
data HydraulicProps = HydraulicProps
  { hp_hk     :: !(VU.Vector Double)  -- ^ Hydraulic conductivity [mm/s]
  , hp_smp    :: !(VU.Vector Double)  -- ^ Matric potential [mm]
  , hp_dhkdw  :: !(VU.Vector Double)  -- ^ d(hk)/d(saturation) at interface
  , hp_dsmpdw :: !(VU.Vector Double)  -- ^ d(smp)/d(vwc) at node
  , hp_imped  :: !(VU.Vector Double)  -- ^ Ice impedance per layer
  } deriving (Show)

-- | Compute hydraulic properties for all soil layers in a column.
-- Ported from compute_hydraulic_properties in SoilWaterMovementMod.F90
computeHydraulicProperties
  :: Int                    -- ^ Number of active layers (nlayers)
  -> Double                 -- ^ e_ice parameter
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> VU.Vector Double       -- ^ bsw (nlevsoi)
  -> VU.Vector Double       -- ^ hksat (nlevsoi)
  -> VU.Vector Double       -- ^ sucsat (nlevsoi)
  -> VU.Vector Double       -- ^ icefrac (nlevsoi)
  -> VU.Vector Double       -- ^ vwc_liq (nlevsoi) -- volumetric liquid water content
  -> HydraulicProps
computeHydraulicProperties nlayers eIce watsat_v bsw_v hksat_v sucsat_v icefrac_v vwc_liq_v =
  let
    -- Relative saturation at nodes
    s2 j = clamp 0.01 1.0 (vix vwc_liq_v j / vix watsat_v j)

    computeLayer j =
      let
        -- Interface saturation and impedance
        (s1_raw, imp) =
          if j >= nlayers - 1
          then (s2 j, iceImpedance (vix icefrac_v j) eIce)
          else ( 0.5 * (s2 j + s2 (j+1))
               , iceImpedance (0.5 * (vix icefrac_v j + vix icefrac_v (j+1))) eIce
               )

        s1 = clamp 0.01 1.0 s1_raw

        (hk_val, dhkds) = clappHornbergerHk s1 imp (vix hksat_v j) (vix bsw_v j)
        (smp_val, dsmpdsi) = clappHornbergerSmp (s2 j) (vix sucsat_v j) (vix bsw_v j)

        -- Derivative w.r.t. volumetric water content
        dsmpdw_val = dsmpdsi / vix watsat_v j
      in (hk_val, smp_val, dhkds, dsmpdw_val, imp)

    results = map computeLayer [0 .. nlayers - 1]
    (hks, smps, dhkdws, dsmpdws, impeds) = unzip5 results
  in HydraulicProps
       { hp_hk     = VU.fromList hks
       , hp_smp    = VU.fromList smps
       , hp_dhkdw  = VU.fromList dhkdws
       , hp_dsmpdw = VU.fromList dsmpdws
       , hp_imped  = VU.fromList impeds
       }

--------------------------------------------------------------------------------
-- Moisture flux computation
--------------------------------------------------------------------------------

-- | Result of flux computation at layer boundaries
data FluxResult = FluxResult
  { fr_qin    :: !(VU.Vector Double)  -- ^ Flux into layer [mm/s]
  , fr_qout   :: !(VU.Vector Double)  -- ^ Flux out of layer [mm/s]
  , fr_dqidw0 :: !(VU.Vector Double)  -- ^ d(qin)/d(w_above)
  , fr_dqidw1 :: !(VU.Vector Double)  -- ^ d(qin)/d(w_this)
  , fr_dqodw1 :: !(VU.Vector Double)  -- ^ d(qout)/d(w_this)
  , fr_dqodw2 :: !(VU.Vector Double)  -- ^ d(qout)/d(w_below)
  } deriving (Show)

-- | Compute fluxes at layer boundaries and their derivatives.
-- Uses a two-pass approach: first compute outgoing fluxes, then derive incoming.
-- Ported from compute_moisture_fluxes_and_derivs in SoilWaterMovementMod.F90
computeMoistureFluxes
  :: Int                    -- ^ Number of active layers
  -> BoundaryCondition      -- ^ Upper BC
  -> BoundaryCondition      -- ^ Lower BC
  -> Double                 -- ^ qflx_infl [mm/s]
  -> Double                 -- ^ zwt [m]
  -> VU.Vector Double       -- ^ z_col (soil midpoints, nlevsoi) [m]
  -> VU.Vector Double       -- ^ zi_col (soil interfaces, nlevsoi+1) [m]
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> HydraulicProps         -- ^ Hydraulic properties
  -> FluxResult
computeMoistureFluxes nlayers upperBC lowerBC qflx_infl_val zwt_val
                      z_col zi_col watsat_v hp =
  let
    hk_v    = hp_hk hp
    smp_v   = hp_smp hp
    dhkdw_v = hp_dhkdw hp
    dsmpdw_v = hp_dsmpdw hp

    -- Interior flux at boundary between j and j+1 (returns qout, dqodw1, dqodw2)
    interiorFlux j =
      let num = vix smp_v (j+1) - vix smp_v j
          den = mToMM * (vix z_col (j+1) - vix z_col j)
          dhkds1 = 0.5 * vix dhkdw_v j / vix watsat_v j
          dhkds2 = 0.5 * vix dhkdw_v j / vix watsat_v (j+1)
          qout_val = negate (vix hk_v j) * num / den + vix hk_v j
          dqodw1_val = (vix hk_v j * vix dsmpdw_v j - dhkds1 * num) / den + dhkds1
          dqodw2_val = (negate (vix hk_v j) * vix dsmpdw_v (j+1) - dhkds2 * num) / den + dhkds2
      in (qout_val, dqodw1_val, dqodw2_val)

    -- Bottom flux for the last active layer
    bottomFlux j = case lowerBC of
      BCFlux ->
        let qout_val = vix hk_v j
            dqodw1_val = vix dhkdw_v j / vix watsat_v j
        in (qout_val, dqodw1_val, 0.0)
      BCZeroFlux ->
        (0.0, 0.0, 0.0)
      BCWaterTable ->
        let jwt = findJwt zwt_val zi_col nlayers
        in if j > jwt
           then (0.0, 0.0, 0.0)
           else let dhkds1_local = vix dhkdw_v j / vix watsat_v j
                    num = negate (vix smp_v j)
                    den = mToMM * (zwt_val - vix z_col j)
                    qout_val = negate (vix hk_v j) * num / den + vix hk_v j
                    dqodw1_val = (vix hk_v j * vix dsmpdw_v j - dhkds1_local * num) / den + dhkds1_local
                in (qout_val, dqodw1_val, 0.0)
      _ -> (0.0, 0.0, 0.0)

    -- Compute outgoing flux for layer j
    outFlux j
      | nlayers == 1       = bottomFlux j
      | j == nlayers - 1   = bottomFlux j
      | otherwise          = interiorFlux j

    -- Step 1: compute outgoing flux components for each layer
    outQout   = VU.generate nlayers $ \j ->
      let (q, _, _) = outFlux j in q
    outDqodw1 = VU.generate nlayers $ \j ->
      let (_, d, _) = outFlux j in d
    outDqodw2 = VU.generate nlayers $ \j ->
      let (_, _, d) = outFlux j in d

    -- Step 2: derive incoming fluxes from previous layer's outgoing
    qinV = VU.generate nlayers $ \j ->
      if j == 0
      then case upperBC of { BCFlux -> qflx_infl_val; _ -> 0.0 }
      else vix outQout (j - 1)

    dqidw0V = VU.generate nlayers $ \j ->
      if j == 0 then 0.0 else vix outDqodw1 (j - 1)

    dqidw1V = VU.generate nlayers $ \j ->
      if j == 0 then 0.0 else vix outDqodw2 (j - 1)

  in FluxResult
       { fr_qin    = qinV
       , fr_qout   = outQout
       , fr_dqidw0 = dqidw0V
       , fr_dqidw1 = dqidw1V
       , fr_dqodw1 = outDqodw1
       , fr_dqodw2 = outDqodw2
       }

--------------------------------------------------------------------------------
-- Tridiagonal system assembly
--------------------------------------------------------------------------------

-- | Tridiagonal system coefficients
data TridiagSystem = TridiagSystem
  { ts_amx :: !(VU.Vector Double)  -- ^ Sub-diagonal
  , ts_bmx :: !(VU.Vector Double)  -- ^ Main diagonal
  , ts_cmx :: !(VU.Vector Double)  -- ^ Super-diagonal
  , ts_rmx :: !(VU.Vector Double)  -- ^ Right-hand side
  } deriving (Show)

-- | Compute RHS of moisture-based Richards equation.
-- Ported from compute_RHS_moisture_form in SoilWaterMovementMod.F90
computeRHSMoistureForm
  :: Int                  -- ^ nlayers
  -> VU.Vector Double     -- ^ vert_trans_sink (root water uptake) [mm/s]
  -> VU.Vector Double     -- ^ qin [mm/s]
  -> VU.Vector Double     -- ^ qout [mm/s]
  -> VU.Vector Double     -- ^ dt_dz = dtsub / (mToMM * dz)
  -> VU.Vector Double     -- ^ rmx (RHS)
computeRHSMoistureForm nlayers sink qin_v qout_v dt_dz =
  VU.generate nlayers $ \j ->
    let fluxNet = vix qin_v j - vix qout_v j - vix sink j
    in negate fluxNet * vix dt_dz j

-- | Compute LHS tridiagonal matrix for moisture-based Richards equation.
-- Ported from compute_LHS_moisture_form in SoilWaterMovementMod.F90
computeLHSMoistureForm
  :: Int                  -- ^ nlayers
  -> VU.Vector Double     -- ^ dt_dz
  -> FluxResult           -- ^ Flux derivatives
  -> TridiagSystem
computeLHSMoistureForm nlayers dt_dz fr =
  let
    amx = VU.generate nlayers $ \j ->
            if j == 0 then 0.0
            else vix (fr_dqidw0 fr) j * vix dt_dz j

    bmx = VU.generate nlayers $ \j ->
            (-1.0) - (negate (vix (fr_dqidw1 fr) j) + vix (fr_dqodw1 fr) j) * vix dt_dz j

    cmx = VU.generate nlayers $ \j ->
            if j == nlayers - 1 then 0.0
            else negate (vix (fr_dqodw2 fr) j) * vix dt_dz j

    rmx = VU.replicate nlayers 0.0  -- placeholder, caller fills via computeRHSMoistureForm
  in TridiagSystem amx bmx cmx rmx

--------------------------------------------------------------------------------
-- Soil water fractions
--------------------------------------------------------------------------------

-- | Result of setSoilWaterFractions
data SoilWaterFractions = SoilWaterFractions
  { swf_eff_porosity :: !(VU.Vector Double)  -- ^ Effective porosity [m3/m3]
  , swf_icefrac      :: !(VU.Vector Double)  -- ^ Ice fraction per layer
  } deriving (Show)

-- | Set diagnostic water/ice fractions per soil layer.
-- Ported from SetSoilWaterFractions in SoilHydrologyMod.F90
setSoilWaterFractions
  :: VU.Vector Double   -- ^ watsat (nlevsoi)
  -> VU.Vector Double   -- ^ h2osoi_ice (snow+soil), with nlevsno offset
  -> VU.Vector Double   -- ^ col_dz (snow+soil), with nlevsno offset
  -> SoilWaterFractions
setSoilWaterFractions watsat_v h2osoi_ice_v col_dz_v =
  let nl = nlevsoi
      joff = nlevsno
      compute j =
        let dz_val = vix col_dz_v (joff + j)
            ice_val = vix h2osoi_ice_v (joff + j)
            ws = vix watsat_v j
            vol_ice_val = min ws (ice_val / (dz_val * denice))
            eff_por = max 0.01 (ws - vol_ice_val)
            icf = min 1.0 (vol_ice_val / ws)
        in (eff_por, icf)
      results = map compute [0 .. nl - 1]
      (effs, icfs) = unzip results
  in SoilWaterFractions
       { swf_eff_porosity = VU.fromList effs
       , swf_icefrac      = VU.fromList icfs
       }

--------------------------------------------------------------------------------
-- Infiltration
--------------------------------------------------------------------------------

-- | Calculate total infiltration [mm/s].
-- Ported from Infiltration in SoilHydrologyMod.F90
infiltration :: Double  -- ^ qflx_in_soil_limited [mm/s]
             -> Double  -- ^ qflx_h2osfc_drain [mm/s]
             -> Double  -- ^ qflx_infl [mm/s]
infiltration qflx_in_soil_limited qflx_h2osfc_drain =
  qflx_in_soil_limited + qflx_h2osfc_drain

-- | Calculate total surface runoff [mm/s].
-- Ported from TotalSurfaceRunoff in SoilHydrologyMod.F90
totalSurfaceRunoff :: Double  -- ^ qflx_sat_excess_surf
                   -> Double  -- ^ qflx_infl_excess_surf
                   -> Double  -- ^ qflx_h2osfc_surf
                   -> Double  -- ^ qflx_surf
totalSurfaceRunoff qflx_sat_excess_surf qflx_infl_excess_surf qflx_h2osfc_surf =
  qflx_sat_excess_surf + qflx_infl_excess_surf + qflx_h2osfc_surf

--------------------------------------------------------------------------------
-- Water table computation
--------------------------------------------------------------------------------

-- | Result of water table computation
data WaterTableResult = WaterTableResult
  { wtr_zwt          :: !Double             -- ^ Water table depth [m]
  , wtr_wa           :: !Double             -- ^ Aquifer water [mm]
  , wtr_zwt_perched  :: !Double             -- ^ Perched water table depth [m]
  , wtr_frost_table  :: !Double             -- ^ Frost table depth [m]
  , wtr_h2osoi_liq   :: !(VU.Vector Double) -- ^ Updated liquid water (snow+soil)
  } deriving (Show)

-- | Find index of soil layer just above water table.
-- jwt = nlevsoi means water table below soil column.
findJwt :: Double -> VU.Vector Double -> Int -> Int
findJwt zwt_val zi_v nl = go 0
  where
    go j
      | j >= nl   = nl - 1  -- below soil column (0-indexed: nl-1 is max)
      | zwt_val <= vix zi_v j = max 0 (j - 1)
      | otherwise = go (j + 1)

-- | Compute water table, considering aquifer recharge but no drainage.
-- Single-column, pure version.
-- Ported from WaterTable in SoilHydrologyMod.F90
waterTable
  :: SoilHydrologyParams
  -> Double                 -- ^ dtime [s]
  -> Double                 -- ^ qcharge [mm/s]
  -> Double                 -- ^ zwt_in [m]
  -> Double                 -- ^ wa_in [mm]
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> VU.Vector Double       -- ^ bsw (nlevsoi)
  -> VU.Vector Double       -- ^ sucsat (nlevsoi)
  -> VU.Vector Double       -- ^ eff_porosity (nlevsoi)
  -> VU.Vector Double       -- ^ col_z (soil midpoints, offset applied, nlevsoi)
  -> VU.Vector Double       -- ^ col_zi (soil interfaces, offset applied, nlevsoi+1)
  -> VU.Vector Double       -- ^ col_dz (soil layers, offset applied, nlevsoi)
  -> VU.Vector Double       -- ^ h2osoi_liq (snow+soil, full array)
  -> VU.Vector Double       -- ^ h2osoi_ice (snow+soil, full array)
  -> VU.Vector Double       -- ^ t_soisno (snow+soil, full array)
  -> WaterTableResult
waterTable params dtime qcharge_val zwt_in wa_in
           watsat_v bsw_v sucsat_v _eff_porosity_v
           z_col zi_col _dz_col
           h2osoi_liq_v _h2osoi_ice_v t_soisno_v =
  let
    nl = nlevsoi
    joff = nlevsno

    -- Find jwt for initial zwt
    jwt0 = findJwtSoil zwt_in zi_col nl

    -- Specific yield at bottom soil layer
    rousBase = calcSpecificYield (vix watsat_v (nl-1)) (vix bsw_v (nl-1))
                                 (vix sucsat_v (nl-1)) zwt_in (aq_sp_yield_min params)

    -- Apply qcharge to water table
    (zwt1, wa1) =
      if jwt0 >= nl  -- water table below soil column
      then ( zwt_in - (qcharge_val * dtime) / 1000.0 / rousBase
           , wa_in + qcharge_val * dtime
           )
      else applyQchargeInColumn qcharge_val dtime zwt_in wa_in
             watsat_v bsw_v sucsat_v zi_col nl jwt0 (aq_sp_yield_min params) rousBase

    -- Compute frost table
    k_frz = findFrostTable t_soisno_v joff nl
    frost_table_val = vix z_col k_frz

    -- Perched water table (simplified: set to frost table)
    zwt_perched_val = frost_table_val

  in WaterTableResult
       { wtr_zwt         = clamp 0.0 80.0 zwt1
       , wtr_wa          = wa1
       , wtr_zwt_perched = zwt_perched_val
       , wtr_frost_table = frost_table_val
       , wtr_h2osoi_liq  = h2osoi_liq_v  -- unchanged in water_table (changed in drainage)
       }

-- | Find jwt (0-indexed) using zi interfaces (0-indexed)
findJwtSoil :: Double -> VU.Vector Double -> Int -> Int
findJwtSoil zwt_val zi_v nl = go 0
  where
    go j
      | j >= nl   = nl
      | zwt_val <= vix zi_v j = j
      | otherwise = go (j + 1)

-- | Compute specific yield at a given depth
calcSpecificYield :: Double -> Double -> Double -> Double -> Double -> Double
calcSpecificYield ws bswVal sucsatVal zwt_val spYieldMin =
  let base = 1.0 + 1.0e3 * zwt_val / sucsatVal
      expn = -1.0 / bswVal
      rous = ws * (1.0 - base ** expn)
  in max rous spYieldMin

-- | Apply qcharge when water table is within soil column
applyQchargeInColumn :: Double -> Double -> Double -> Double
                     -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
                     -> VU.Vector Double -> Int -> Int -> Double -> Double
                     -> (Double, Double)
applyQchargeInColumn qcharge_val dtime zwt_in wa_in
                     watsat_v bsw_v sucsat_v zi_col nl jwt0 spYieldMin rousBase =
  let qcharge_tot0 = qcharge_val * dtime
  in if qcharge_tot0 > 0.0
     then -- Rising water table
       risingWT qcharge_tot0 zwt_in jwt0 watsat_v bsw_v sucsat_v zi_col spYieldMin
     else -- Deepening water table
       let (zwt', qtot') = deepeningWT qcharge_tot0 zwt_in (jwt0+1) watsat_v bsw_v sucsat_v zi_col nl spYieldMin
           zwt'' = if qtot' > 0.0
                   then zwt' - qtot' / 1000.0 / rousBase
                   else zwt'
       in (zwt'', wa_in)

-- | Rising water table loop
risingWT :: Double -> Double -> Int
         -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
         -> VU.Vector Double -> Double
         -> (Double, Double)
risingWT qtot zwt_val jwt watsat_v bsw_v sucsat_v zi_col spYieldMin =
  go qtot zwt_val jwt
  where
    go qt zw j
      | qt <= 0.0 || j < 0 = (zw, 0.0)
      | otherwise =
          let s_y = calcSpecificYield (vix watsat_v j) (vix bsw_v j)
                      (vix sucsat_v j) zw spYieldMin
              zi_jm1 = if j == 0 then 0.0 else vix zi_col (j - 1)
              ql = min qt (max 0.0 (s_y * (zw - zi_jm1) * 1.0e3))
              zw' = if s_y > 0.0 then zw - ql / s_y / 1000.0 else zw
              qt' = qt - ql
          in go qt' zw' (j - 1)

-- | Deepening water table loop
deepeningWT :: Double -> Double -> Int
            -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
            -> VU.Vector Double -> Int -> Double
            -> (Double, Double)
deepeningWT qtot zwt_val jstart watsat_v bsw_v sucsat_v zi_col nl spYieldMin =
  go qtot zwt_val jstart
  where
    go qt zw j
      | qt >= 0.0 || j >= nl = (zw, qt)
      | otherwise =
          let s_y = calcSpecificYield (vix watsat_v j) (vix bsw_v j)
                      (vix sucsat_v j) zw spYieldMin
              ql = max qt (negate (s_y * (vix zi_col j - zw) * 1.0e3))
              ql' = min ql 0.0
              qt' = qt - ql'
          in if qt' >= 0.0
             then let zw' = zw - ql' / s_y / 1000.0
                  in (zw', qt')
             else go qt' (vix zi_col j) (j + 1)

-- | Find frost table layer index (0-based index into soil-only)
findFrostTable :: VU.Vector Double -> Int -> Int -> Int
findFrostTable t_soisno_v joff nl =
  let t j = vix t_soisno_v (joff + j)
  in if t 0 > tfrz
     then -- search for first frozen layer below an unfrozen one
       let go k
             | k >= nl   = nl - 1
             | t (k-1) > tfrz && t k <= tfrz = k
             | otherwise = go (k + 1)
       in go 1
     else 0

--------------------------------------------------------------------------------
-- Drainage
--------------------------------------------------------------------------------

-- | Result of drainage computation
data DrainageResult = DrainageResult
  { dr_qflx_drain    :: !Double             -- ^ Subsurface drainage [mm/s]
  , dr_qflx_rsub_sat :: !Double             -- ^ Saturation excess drainage [mm/s]
  , dr_qflx_drain_perched :: !Double        -- ^ Perched drainage [mm/s]
  , dr_zwt           :: !Double             -- ^ Updated water table [m]
  , dr_wa            :: !Double             -- ^ Updated aquifer water [mm]
  , dr_h2osoi_liq    :: !(VU.Vector Double) -- ^ Updated liquid water (snow+soil)
  } deriving (Show)

-- | Subsurface drainage.
-- Single-column, pure version.
-- Ported from Drainage in SoilHydrologyMod.F90
drainage
  :: SoilHydrologyParams
  -> Double                 -- ^ dtime [s]
  -> Double                 -- ^ topo_slope [degrees]
  -> Double                 -- ^ hkdepth [m]
  -> Double                 -- ^ zwt_in [m]
  -> Double                 -- ^ wa_in [mm]
  -> Double                 -- ^ aquifer_water_baseline [mm]
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> VU.Vector Double       -- ^ bsw (nlevsoi)
  -> VU.Vector Double       -- ^ hksat (nlevsoi)
  -> VU.Vector Double       -- ^ sucsat (nlevsoi)
  -> VU.Vector Double       -- ^ eff_porosity (nlevsoi)
  -> VU.Vector Double       -- ^ icefrac (nlevsoi)
  -> VU.Vector Double       -- ^ col_z_soil (soil midpoints, nlevsoi)
  -> VU.Vector Double       -- ^ col_zi_soil (soil interfaces, nlevsoi+1, 0=surface)
  -> VU.Vector Double       -- ^ col_dz_soil (soil thicknesses, nlevsoi)
  -> VU.Vector Double       -- ^ h2osoi_liq (snow+soil, full array)
  -> VU.Vector Double       -- ^ t_soisno (snow+soil, full array)
  -> DrainageResult
drainage params dtime topo_slope hkdepth_val zwt_in wa_in aq_baseline
         watsat_v bsw_v _hksat_v sucsat_v eff_porosity_v icefrac_v
         z_soil zi_soil dz_soil
         h2osoi_liq_in t_soisno_v =
  let
    nl = nlevsoi
    joff = nlevsno
    eIceH = e_ice_hydro params
    spYieldMin = aq_sp_yield_min params

    -- Convert dz to mm
    dzmm j = vix dz_soil j * 1.0e3

    -- Find jwt
    jwt = findJwtSoil zwt_in zi_soil nl

    -- Frost table (computed for completeness, used in full perched WT code)
    _k_frz = findFrostTable t_soisno_v joff nl
    _frost_table_val = vix z_soil _k_frz

    -- Topographic runoff
    fff = 1.0 / hkdepth_val

    -- Ice-weighted impedance for drainage
    (dzsum, icefracsum) = foldl
      (\(dz_acc, ice_acc) j ->
        (dz_acc + dzmm j, ice_acc + vix icefrac_v j * dzmm j))
      (0.0, 0.0)
      [max jwt 0 .. nl - 1]

    imped_drain = if dzsum > 0.0
                  then 10.0 ** (negate eIceH * (icefracsum / dzsum))
                  else 1.0

    rsub_top_max = 10.0 * sin (rpi / 180.0 * topo_slope)
    rsub_top_val = imped_drain * rsub_top_max * exp (negate fff * zwt_in)

    -- Specific yield at bottom
    rousLocal = calcSpecificYield (vix watsat_v (nl-1)) (vix bsw_v (nl-1))
                                  (vix sucsat_v (nl-1)) zwt_in spYieldMin

    -- Apply topographic drainage
    (zwt1, wa1, h2osoi_liq1) =
      if jwt >= nl
      then -- Water table below soil column
        let wa' = wa_in - rsub_top_val * dtime
            zwt' = zwt_in + rsub_top_val * dtime / 1000.0 / rousLocal
            -- Move excess water from aquifer to bottom soil
            extra = max 0.0 (wa' - aq_baseline)
            h2osoi_liq' = updateLiq h2osoi_liq_in (joff + nl - 1) (+ extra)
            wa'' = min wa' aq_baseline
        in (zwt', wa'', h2osoi_liq')
      else -- Water table within soil column
        let rsub_top_tot0 = negate rsub_top_val * dtime
            (zwt', h2osoi_liq', rsub_rem) =
              drainFromLayers rsub_top_tot0 zwt_in jwt nl joff
                              watsat_v bsw_v sucsat_v zi_soil eff_porosity_v
                              h2osoi_liq_in spYieldMin
            -- Residual goes to aquifer
            zwt'' = zwt' - rsub_rem / 1000.0 / rousLocal
            wa' = wa_in + rsub_rem
        in (zwt'', wa', h2osoi_liq')

    -- Redistribute excessive water upward
    h2osoi_liq2 = redistributeExcessUp nl joff eff_porosity_v dzmm h2osoi_liq1

    -- Handle top layer saturation excess
    h2osoi_liq_top = vix h2osoi_liq2 (joff + 0)
    h2osoi_ice_top = vix (VU.slice (joff) nl (VU.replicate (joff + nl) 0.0)) 0  -- would need ice
    xs1 = max 0.0 (max 0.0 (h2osoi_liq_top - watmin) -
           max 0.0 (pondmx + vix watsat_v 0 * dzmm 0 - watmin))
    h2osoi_liq3 = updateLiq h2osoi_liq2 (joff + 0) (\x -> x - xs1)

    qflx_rsub_sat_val = xs1 / dtime

    -- Ensure minimum water in each layer (pull from below)
    h2osoi_liq4 = ensureMinWater nl joff h2osoi_liq3

    -- Final drainage = rsub_sat + rsub_top
    qflx_drain_val = qflx_rsub_sat_val + rsub_top_val

    zwt_final = clamp 0.0 80.0 zwt1

  in DrainageResult
       { dr_qflx_drain         = qflx_drain_val
       , dr_qflx_rsub_sat      = qflx_rsub_sat_val
       , dr_qflx_drain_perched = 0.0  -- simplified: perched drainage handled separately
       , dr_zwt                = zwt_final
       , dr_wa                 = wa1
       , dr_h2osoi_liq         = h2osoi_liq4
       }

-- | Drain from saturated layers within soil column
drainFromLayers :: Double -> Double -> Int -> Int -> Int
                -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
                -> VU.Vector Double -> VU.Vector Double
                -> VU.Vector Double -> Double
                -> (Double, VU.Vector Double, Double)
drainFromLayers rsub_tot0 zwt_in jwt nl joff
                watsat_v bsw_v sucsat_v zi_col eff_porosity_v
                h2osoi_liq_v spYieldMin =
  go rsub_tot0 zwt_in (jwt + 1) h2osoi_liq_v
  where
    go rsub_tot zwt_val j liq
      | rsub_tot >= 0.0 || j >= nl = (zwt_val, liq, rsub_tot)
      | otherwise =
          let s_y = calcSpecificYield (vix watsat_v j) (vix bsw_v j)
                      (vix sucsat_v j) zwt_val spYieldMin
              rsub_layer = max rsub_tot (negate (s_y * (vix zi_col j - zwt_val) * 1.0e3))
              rsub_layer' = min rsub_layer 0.0
              liq' = updateLiq liq (joff + j) (+ rsub_layer')
              rsub_tot' = rsub_tot - rsub_layer'
          in if rsub_tot' >= 0.0
             then let zwt' = zwt_val - rsub_layer' / s_y / 1000.0
                  in (zwt', liq', rsub_tot')
             else go rsub_tot' (vix zi_col j) (j + 1) liq'

-- | Redistribute excessive water upward through soil column
redistributeExcessUp :: Int -> Int -> VU.Vector Double -> (Int -> Double)
                     -> VU.Vector Double -> VU.Vector Double
redistributeExcessUp nl joff eff_porosity_v dzmm liq_in =
  foldl redistribute liq_in (reverse [1 .. nl - 1])
  where
    redistribute liq j =
      let idx = joff + j
          maxLiq = vix eff_porosity_v j * dzmm j
          current = vix liq idx
          xs = max 0.0 (current - maxLiq)
          liq' = updateLiq liq idx (const (min maxLiq current))
      in updateLiq liq' (idx - 1) (+ xs)

-- | Ensure minimum water content in each layer (pull from below)
ensureMinWater :: Int -> Int -> VU.Vector Double -> VU.Vector Double
ensureMinWater nl joff liq_in =
  foldl ensureLayer liq_in [0 .. nl - 2]
  where
    ensureLayer liq j =
      let idx = joff + j
          current = vix liq idx
      in if current < watmin
         then let xs = watmin - current
              in updateLiq (updateLiq liq idx (+ xs)) (idx + 1) (\x -> x - xs)
         else liq

--------------------------------------------------------------------------------
-- Aquifer recharge (qcharge)
--------------------------------------------------------------------------------

-- | Compute aquifer recharge rate [mm/s].
-- Ported from compute_qcharge in SoilWaterMovementMod.F90
computeQcharge
  :: Double              -- ^ zwt [m]
  -> Double              -- ^ dtime [s]
  -> VU.Vector Double    -- ^ watsat (nlevsoi)
  -> VU.Vector Double    -- ^ bsw (nlevsoi)
  -> VU.Vector Double    -- ^ hksat (nlevsoi)
  -> VU.Vector Double    -- ^ sucsat (nlevsoi)
  -> VU.Vector Double    -- ^ icefrac (nlevsoi)
  -> VU.Vector Double    -- ^ vwc_liq (nlevsoi)
  -> VU.Vector Double    -- ^ smp_arr (nlevsoi)
  -> VU.Vector Double    -- ^ z_soil (nlevsoi, soil midpoints)
  -> VU.Vector Double    -- ^ zi_soil (nlevsoi+1, soil interfaces with 0=surface)
  -> Double              -- ^ e_ice
  -> Double              -- ^ qcharge [mm/s]
computeQcharge zwt_val dtime watsat_v bsw_v hksat_v sucsat_v
               icefrac_v vwc_liq_v smp_v z_soil zi_soil eIce =
  let nl = nlevsoi
      jwt = findJwtSoil zwt_val zi_soil nl
  in if jwt < nl
     then
       let jw = jwt  -- 0-based index of layer at water table
           wh_zwt = negate (vix sucsat_v jw) - zwt_val * mToMM

           s1 = clamp 0.01 1.0 (vix vwc_liq_v jw / vix watsat_v jw)
           imp = iceImpedance (vix icefrac_v jw) eIce
           (ka, _) = clappHornbergerHk s1 imp (vix hksat_v jw) (vix bsw_v jw)

           smp1 = max smpmin (if jwt > 0 then vix smp_v (jwt - 1) else vix smp_v 0)
           z_jwt = if jwt > 0 then vix z_soil (jwt - 1) else 0.0
           wh = smp1 - z_jwt * mToMM

           qc = if jwt == 0
                then negate ka * (wh_zwt - wh) / ((zwt_val + 1.0e-3) * mToMM)
                else negate ka * (wh_zwt - wh) / ((zwt_val - z_jwt) * mToMM * 2.0)

       in clamp (negate 10.0 / dtime) (10.0 / dtime) qc
     else 0.0  -- water table below soil column; qcharge from solver

--------------------------------------------------------------------------------
-- Richards equation solvers
--------------------------------------------------------------------------------

-- | Result from soil water movement solver
data SoilWaterResult = SoilWaterResult
  { swr_h2osoi_liq :: !(VU.Vector Double)  -- ^ Updated liquid water (snow+soil)
  , swr_qcharge    :: !Double              -- ^ Aquifer recharge [mm/s]
  , swr_nsubsteps  :: !Int                 -- ^ Number of adaptive substeps used
  } deriving (Show)

-- | Moisture-based Richards equation solver with adaptive time stepping.
-- Single-column, pure.
-- Ported from soilwater_moisture_form in SoilWaterMovementMod.F90
soilwaterMoistureForm
  :: SoilWaterMovementConfig
  -> Int                     -- ^ nlayers (active layers, typically nbedrock)
  -> Double                  -- ^ dtime [s]
  -> Double                  -- ^ qflx_infl [mm/s]
  -> Double                  -- ^ zwt [m]
  -> VU.Vector Double        -- ^ watsat (nlevsoi)
  -> VU.Vector Double        -- ^ bsw (nlevsoi)
  -> VU.Vector Double        -- ^ hksat (nlevsoi)
  -> VU.Vector Double        -- ^ sucsat (nlevsoi)
  -> VU.Vector Double        -- ^ icefrac (nlevsoi)
  -> VU.Vector Double        -- ^ qflx_rootsoi (nlevsoi) -- root water uptake per layer
  -> VU.Vector Double        -- ^ z_soil (nlevsoi, soil midpoints)
  -> VU.Vector Double        -- ^ zi_soil (nlevsoi+1, soil interfaces)
  -> VU.Vector Double        -- ^ dz_soil (nlevsoi, soil thicknesses) [m]
  -> VU.Vector Double        -- ^ h2osoi_liq (snow+soil, full array)
  -> SoilWaterResult
soilwaterMoistureForm cfg nlayers dtime qflx_infl_val zwt_val
                      watsat_v bsw_v hksat_v sucsat_v icefrac_v
                      rootsoi z_soil zi_soil dz_soil h2osoi_liq_in =
  let joff = nlevsno

      -- Adaptive sub-stepping loop
      result = substepLoop 0 dtime 0.0 0.0 0 h2osoi_liq_in

      substepLoop :: Int -> Double -> Double -> Double -> Int
                   -> VU.Vector Double -> SoilWaterResult
      substepLoop !nstep !dtsub !dtdone !qcharge_acc !_prevSteps !liq
        | nstep > 1000 =  -- safety limit
            SoilWaterResult liq qcharge_acc nstep
        | otherwise =
            let
              -- Volumetric liquid water content and dt/dz
              vwc_liq = VU.generate nlayers $ \j ->
                max 1.0e-6 (vix liq (joff + j)) / (vix dz_soil j * denh2o)

              dt_dz = VU.generate nlayers $ \j ->
                dtsub / (mToMM * vix dz_soil j)

              -- Hydraulic properties
              hp = computeHydraulicProperties nlayers (e_ice cfg)
                     watsat_v bsw_v hksat_v sucsat_v icefrac_v vwc_liq

              -- Fluxes and derivatives
              fr = computeMoistureFluxes nlayers
                     (upper_boundary_condition cfg)
                     (lower_boundary_condition cfg)
                     qflx_infl_val zwt_val
                     z_soil zi_soil watsat_v hp

              -- RHS
              rmx = computeRHSMoistureForm nlayers rootsoi
                      (fr_qin fr) (fr_qout fr) dt_dz

              -- LHS
              ts = computeLHSMoistureForm nlayers dt_dz fr

              -- Solve tridiagonal system
              dwat = tridiagonalSolve (ts_amx ts) (ts_bmx ts) (ts_cmx ts) rmx

              -- Error estimation (inexpensive method)
              errorMax = VU.foldl' max 0.0 $ VU.generate nlayers $ \j ->
                let fluxNet0 = vix dwat j / vix dt_dz j
                    fluxNet1 = vix (fr_qin fr) j - vix (fr_qout fr) j - vix rootsoi j
                in abs (fluxNet1 - fluxNet0) * dtsub * 0.5

            in if errorMax > xTolerUpper cfg && dtsub > dtmin cfg
               then -- Reject substep, halve
                 substepLoop (nstep + 1) (max (dtsub / 2.0) (dtmin cfg))
                             dtdone qcharge_acc 0 liq
               else
                 let
                   -- Accept: update h2osoi_liq
                   liq' = VU.imap (\i x ->
                     if i >= joff && i < joff + nlayers
                     then x + vix dwat (i - joff) * (mToMM * vix dz_soil (i - joff))
                     else x) liq

                   -- Compute qcharge increment
                   qcTemp = case lower_boundary_condition cfg of
                     BCFlux ->
                       vix (hp_hk hp) (nlayers - 1) +
                       vix (hp_dhkdw hp) (nlayers - 1) * vix dwat (nlayers - 1)
                     BCZeroFlux -> 0.0
                     BCWaterTable ->
                       vix (fr_qout fr) (nlayers - 1) +
                       vix (fr_dqodw1 fr) (nlayers - 1) * vix dwat (nlayers - 1)
                     _ -> 0.0

                   qcharge_acc' = qcharge_acc + qcTemp * (dtsub / dtime)
                   dtdone' = dtdone + dtsub

                 in if abs (dtime - dtdone') < verySmall cfg
                    then SoilWaterResult liq' qcharge_acc' (nstep + 1)
                    else let dtsub' = if errorMax < xTolerLower cfg
                                      then dtsub * 2.0
                                      else dtsub
                             dtsub'' = min dtsub' (dtime - dtdone')
                         in substepLoop (nstep + 1) dtsub'' dtdone' qcharge_acc' 0 liq'

  in result

-- | Zeng-Decker 2009 method for soil water movement.
-- Single-column, pure.
-- Ported from soilwater_zengdecker2009 in SoilWaterMovementMod.F90
soilwaterZengDecker2009
  :: SoilWaterMovementConfig
  -> Double                  -- ^ dtime [s]
  -> Double                  -- ^ qflx_infl [mm/s]
  -> Double                  -- ^ zwt [m]
  -> VU.Vector Double        -- ^ watsat (nlevsoi)
  -> VU.Vector Double        -- ^ bsw (nlevsoi)
  -> VU.Vector Double        -- ^ hksat (nlevsoi)
  -> VU.Vector Double        -- ^ sucsat (nlevsoi)
  -> VU.Vector Double        -- ^ icefrac (nlevsoi)
  -> VU.Vector Double        -- ^ h2osoi_vol (nlevsoi)
  -> VU.Vector Double        -- ^ qflx_rootsoi (nlevsoi)
  -> VU.Vector Double        -- ^ z_soil (nlevsoi, soil midpoints) [m]
  -> VU.Vector Double        -- ^ zi_soil (nlevsoi+1, soil interfaces, 0=surface) [m]
  -> VU.Vector Double        -- ^ dz_soil (nlevsoi) [m]
  -> VU.Vector Double        -- ^ h2osoi_liq (snow+soil, full array)
  -> VU.Vector Double        -- ^ h2osoi_ice (snow+soil, full array)
  -> VU.Vector Double        -- ^ t_soisno (snow+soil, full array)
  -> SoilWaterResult
soilwaterZengDecker2009 cfg dtime qflx_infl_val zwt_val
                        watsat_v bsw_v hksat_v sucsat_v icefrac_v
                        h2osoi_vol_v rootsoi
                        z_soil zi_soil dz_soil
                        h2osoi_liq_in _h2osoi_ice_in _t_soisno_v =
  let
    nl = nlevsoi
    joff = nlevsno
    eIceVal = e_ice cfg

    -- Convert to mm
    zmm j = vix z_soil j * 1.0e3
    dzmm_val j = vix dz_soil j * 1.0e3
    zimm j = vix zi_soil j * 1.0e3  -- zi_soil has nlevsoi+1 elements (0=surface)
    zwtmm = zwt_val * 1.0e3

    -- Volumetric liquid water content
    vwc_liq j = max 1.0e-6 (vix h2osoi_liq_in (joff + j)) / (vix dz_soil j * denh2o)

    -- Compute jwt
    jwt = findJwtSoil zwt_val zi_soil nl

    -- VWC at water table
    vwc_zwt = vix watsat_v (nl - 1)  -- simplified: assume saturated at WT

    -- Equilibrium water content
    vol_eq = VU.generate nl $ \j ->
      let zimm_jm1 = zimm j       -- interface above layer j (zi_soil index j = Fortran j-1)
          zimm_j   = zimm (j + 1) -- interface below layer j
          ws = vix watsat_v j
          bswj = vix bsw_v j
          sucj = vix sucsat_v j
      in if zwtmm <= zimm_jm1
         then ws
         else if zwtmm < zimm_j && zwtmm > zimm_jm1
         then let tempi = 1.0
                  temp0 = ((sucj + zwtmm - zimm_jm1) / sucj) ** (1.0 - 1.0 / bswj)
                  voleq1 = negate sucj * ws / (1.0 - 1.0 / bswj) / (zwtmm - zimm_jm1) * (tempi - temp0)
                  veq = (voleq1 * (zwtmm - zimm_jm1) + ws * (zimm_j - zwtmm)) / (zimm_j - zimm_jm1)
              in clamp 0.0 ws veq
         else let tempi = ((sucj + zwtmm - zimm_j) / sucj) ** (1.0 - 1.0 / bswj)
                  temp0 = ((sucj + zwtmm - zimm_jm1) / sucj) ** (1.0 - 1.0 / bswj)
                  veq = negate sucj * ws / (1.0 - 1.0 / bswj) / (zimm_j - zimm_jm1) * (tempi - temp0)
              in clamp 0.0 ws veq

    -- Equilibrium matric potential
    zq j = max smpmin (negate (vix sucsat_v j) *
             (max (vix vol_eq j / vix watsat_v j) 0.01) ** (negate (vix bsw_v j)))

    -- Extra zq for aquifer layer (j+1)
    zq_aq = if jwt >= nl
            then let j = nl - 1
                     zimm_j = zimm (j + 1)
                     sucj = vix sucsat_v j
                     bswj = vix bsw_v j
                     ws = vix watsat_v j
                     tempi = 1.0
                     temp0 = ((sucj + zwtmm - zimm_j) / sucj) ** (1.0 - 1.0 / bswj)
                     veq = clamp 0.0 ws (negate sucj * ws / (1.0 - 1.0 / bswj) / (zwtmm - zimm_j) * (tempi - temp0))
                 in max smpmin (negate (vix sucsat_v j) * (max (veq / ws) 0.01) ** (negate bswj))
            else 0.0

    -- Hydraulic conductivity and matric potential
    hk_v = VU.generate nl $ \j ->
      let s1_raw = 0.5 * (vwc_liq j + vwc_liq (min (nl-1) (j+1))) /
                   (0.5 * (vix watsat_v j + vix watsat_v (min (nl-1) (j+1))))
          s1 = clamp 0.01 1.0 s1_raw
          s2_val = vix hksat_v j * s1 ** (2.0 * vix bsw_v j + 2.0)
          imp = iceImpedance (0.5 * (vix icefrac_v j + vix icefrac_v (min (nl-1) (j+1)))) eIceVal
      in imp * s1 * s2_val

    dhkdw_v = VU.generate nl $ \j ->
      let s1_raw = 0.5 * (vwc_liq j + vwc_liq (min (nl-1) (j+1))) /
                   (0.5 * (vix watsat_v j + vix watsat_v (min (nl-1) (j+1))))
          s1 = clamp 0.01 1.0 s1_raw
          s2_val = vix hksat_v j * s1 ** (2.0 * vix bsw_v j + 2.0)
          imp = iceImpedance (0.5 * (vix icefrac_v j + vix icefrac_v (min (nl-1) (j+1)))) eIceVal
      in imp * (2.0 * vix bsw_v j + 3.0) * s2_val *
         (1.0 / (vix watsat_v j + vix watsat_v (min (nl-1) (j+1))))

    smp_arr = VU.generate nl $ \j ->
      let s_node = clamp 0.01 1.0 (vwc_liq j / vix watsat_v j)
      in max smpmin (negate (vix sucsat_v j) * s_node ** (negate (vix bsw_v j)))

    dsmpdw_arr = VU.generate nl $ \j ->
      negate (vix bsw_v j) * vix smp_arr j / vwc_liq j

    -- Aquifer layer midpoint and thickness [mm]
    zmm_aq = 0.5 * (1.0e3 * zwt_val + zmm (nl - 1))
    dzmm_aq = if jwt < nl
              then dzmm_val (nl - 1)
              else 1.0e3 * zwt_val - zmm (nl - 1)

    -- Total system size (soil + aquifer)
    nSys = nl + 1

    -- Build tridiagonal system
    -- Node j=0 (top)
    topQin = qflx_infl_val
    topDen = zmm 1 - zmm 0
    topDzq = zq 1 - zq 0
    topNum = (vix smp_arr 1 - vix smp_arr 0) - topDzq
    topQout = negate (vix hk_v 0) * topNum / topDen
    topDqodw1 = negate (negate (vix hk_v 0) * vix dsmpdw_arr 0 + topNum * vix dhkdw_v 0) / topDen
    topDqodw2 = negate (vix hk_v 0 * vix dsmpdw_arr 1 + topNum * vix dhkdw_v 0) / topDen
    topRmx = topQin - topQout - vix rootsoi 0
    topAmx = 0.0
    topBmx = dzmm_val 0 / dtime + topDqodw1
    topCmx = topDqodw2

    -- Interior nodes j=1..nl-2
    interiorNode j =
      let den1 = zmm j - zmm (j-1)
          dzq1 = zq j - zq (j-1)
          num1 = (vix smp_arr j - vix smp_arr (j-1)) - dzq1
          qin_j = negate (vix hk_v (j-1)) * num1 / den1
          dqidw0_j = negate (negate (vix hk_v (j-1)) * vix dsmpdw_arr (j-1) + num1 * vix dhkdw_v (j-1)) / den1
          dqidw1_j = negate (vix hk_v (j-1) * vix dsmpdw_arr j + num1 * vix dhkdw_v (j-1)) / den1
          den2 = zmm (j+1) - zmm j
          dzq2 = zq (j+1) - zq j
          num2 = (vix smp_arr (j+1) - vix smp_arr j) - dzq2
          qout_j = negate (vix hk_v j) * num2 / den2
          dqodw1_j = negate (negate (vix hk_v j) * vix dsmpdw_arr j + num2 * vix dhkdw_v j) / den2
          dqodw2_j = negate (vix hk_v j * vix dsmpdw_arr (j+1) + num2 * vix dhkdw_v j) / den2
          rmx_j = qin_j - qout_j - vix rootsoi j
          amx_j = negate dqidw0_j
          bmx_j = dzmm_val j / dtime - dqidw1_j + dqodw1_j
          cmx_j = dqodw2_j
      in (amx_j, bmx_j, cmx_j, rmx_j)

    -- Bottom soil node (j=nl-1) and aquifer node (j=nl)
    botAndAquifer =
      let j = nl - 1
          den1 = zmm j - zmm (j-1)
          dzq1 = zq j - zq (j-1)
          num1 = (vix smp_arr j - vix smp_arr (j-1)) - dzq1
          qin_j = negate (vix hk_v (j-1)) * num1 / den1
          dqidw0_j = negate (negate (vix hk_v (j-1)) * vix dsmpdw_arr (j-1) + num1 * vix dhkdw_v (j-1)) / den1
          dqidw1_j = negate (vix hk_v (j-1) * vix dsmpdw_arr j + num1 * vix dhkdw_v (j-1)) / den1
      in if jwt >= nl
         then -- Water table below soil: couple to aquifer
           let -- Aquifer layer SMP
               s_node_aq = clamp 0.01 1.0 (0.5 * ((vwc_zwt + vwc_liq j) / vix watsat_v j))
               smp1_aq = max smpmin (negate (vix sucsat_v j) * s_node_aq ** (negate (vix bsw_v j)))
               dsmpdw1_aq = negate (vix bsw_v j) * smp1_aq / (s_node_aq * vix watsat_v j)

               den2 = zmm_aq - zmm j
               dzq2 = zq_aq - zq j
               num2 = (smp1_aq - vix smp_arr j) - dzq2
               qout_j = negate (vix hk_v j) * num2 / den2
               dqodw1_j = negate (negate (vix hk_v j) * vix dsmpdw_arr j + num2 * vix dhkdw_v j) / den2
               dqodw2_j = negate (vix hk_v j * dsmpdw1_aq + num2 * vix dhkdw_v j) / den2

               rmx_j = qin_j - qout_j - vix rootsoi j
               amx_j = negate dqidw0_j
               bmx_j = dzmm_val j / dtime - dqidw1_j + dqodw1_j
               cmx_j = dqodw2_j

               -- Aquifer layer
               qin_aq = qout_j
               dqidw0_aq = negate (negate (vix hk_v j) * vix dsmpdw_arr j + num2 * vix dhkdw_v j) / den2
               dqidw1_aq = negate (vix hk_v j * dsmpdw1_aq + num2 * vix dhkdw_v j) / den2
               rmx_aq = qin_aq
               amx_aq = negate dqidw0_aq
               bmx_aq = dzmm_aq / dtime - dqidw1_aq
               cmx_aq = 0.0
           in ((amx_j, bmx_j, cmx_j, rmx_j), (amx_aq, bmx_aq, cmx_aq, rmx_aq))
         else -- Water table in soil: no flow at bottom
           let qout_j = 0.0
               dqodw1_j = 0.0
               rmx_j = qin_j - qout_j - vix rootsoi j
               amx_j = negate dqidw0_j
               bmx_j = dzmm_val j / dtime - dqidw1_j + dqodw1_j
               cmx_j = 0.0
               -- Inactive aquifer
               rmx_aq = 0.0
               amx_aq = 0.0
               bmx_aq = dzmm_aq / dtime
               cmx_aq = 0.0
           in ((amx_j, bmx_j, cmx_j, rmx_j), (amx_aq, bmx_aq, cmx_aq, rmx_aq))

    -- Assemble full tridiagonal system (nSys = nl + 1)
    ((botA, botB, botC, botR), (aqA, aqB, aqC, aqR)) = botAndAquifer

    amxFull = VU.generate nSys $ \i ->
      if i == 0 then topAmx
      else if i < nl - 1 then let (a, _, _, _) = interiorNode i in a
      else if i == nl - 1 then botA
      else aqA

    bmxFull = VU.generate nSys $ \i ->
      if i == 0 then topBmx
      else if i < nl - 1 then let (_, b, _, _) = interiorNode i in b
      else if i == nl - 1 then botB
      else aqB

    cmxFull = VU.generate nSys $ \i ->
      if i == 0 then topCmx
      else if i < nl - 1 then let (_, _, c, _) = interiorNode i in c
      else if i == nl - 1 then botC
      else aqC

    rmxFull = VU.generate nSys $ \i ->
      if i == 0 then topRmx
      else if i < nl - 1 then let (_, _, _, r) = interiorNode i in r
      else if i == nl - 1 then botR
      else aqR

    -- Solve tridiagonal system
    dwat = tridiagonalSolve amxFull bmxFull cmxFull rmxFull

    -- Update h2osoi_liq
    h2osoi_liq_out = VU.imap (\i x ->
      if i >= joff && i < joff + nl
      then x + vix dwat (i - joff) * dzmm_val (i - joff)
      else x) h2osoi_liq_in

    -- Compute qcharge
    qcharge_val =
      if jwt < nl
      then -- Water table in soil column
        let jw = jwt
            s_node = clamp 0.01 1.0 (vix h2osoi_vol_v jw / vix watsat_v jw)
            imp = iceImpedance (vix icefrac_v jw) eIceVal
            ka = imp * vix hksat_v jw * s_node ** (2.0 * vix bsw_v jw + 3.0)
            smp1 = max smpmin (if jw > 0 then vix smp_arr (jw - 1) else vix smp_arr 0)
            wh = smp1 - (if jw > 0 then zq (jw - 1) else zq 0)
            z_jwt_m = if jw > 0 then vix z_soil (jw - 1) else 0.0
            qc = if jw == 0
                 then negate ka * (negate wh) / ((zwt_val + 1.0e-3) * 1000.0)
                 else negate ka * (negate wh) / ((zwt_val - z_jwt_m) * 1000.0 * 2.0)
        in clamp (negate 10.0 / dtime) (10.0 / dtime) qc
      else -- Water table below soil: from aquifer layer solution
        vix dwat nl * dzmm_aq / dtime

  in SoilWaterResult
       { swr_h2osoi_liq = h2osoi_liq_out
       , swr_qcharge    = qcharge_val
       , swr_nsubsteps  = 1
       }

-- | Main entry point for soil water movement.
-- Dispatches to appropriate solver.
-- Ported from SoilWater in SoilWaterMovementMod.F90
soilWater
  :: SoilWaterMovementConfig
  -> Int                     -- ^ nlayers
  -> Double                  -- ^ dtime
  -> Double                  -- ^ qflx_infl
  -> Double                  -- ^ zwt
  -> VU.Vector Double        -- ^ watsat
  -> VU.Vector Double        -- ^ bsw
  -> VU.Vector Double        -- ^ hksat
  -> VU.Vector Double        -- ^ sucsat
  -> VU.Vector Double        -- ^ icefrac
  -> VU.Vector Double        -- ^ h2osoi_vol
  -> VU.Vector Double        -- ^ qflx_rootsoi
  -> VU.Vector Double        -- ^ z_soil
  -> VU.Vector Double        -- ^ zi_soil
  -> VU.Vector Double        -- ^ dz_soil
  -> VU.Vector Double        -- ^ h2osoi_liq (snow+soil)
  -> VU.Vector Double        -- ^ h2osoi_ice (snow+soil)
  -> VU.Vector Double        -- ^ t_soisno (snow+soil)
  -> SoilWaterResult
soilWater cfg nlayers dtime qflx_infl_val zwt_val
          watsat_v bsw_v hksat_v sucsat_v icefrac_v h2osoi_vol_v
          rootsoi z_soil zi_soil dz_soil
          h2osoi_liq_in h2osoi_ice_in t_soisno_v =
  case soilwater_movement_method cfg of
    ZengDecker2009 ->
      soilwaterZengDecker2009 cfg dtime qflx_infl_val zwt_val
        watsat_v bsw_v hksat_v sucsat_v icefrac_v h2osoi_vol_v
        rootsoi z_soil zi_soil dz_soil
        h2osoi_liq_in h2osoi_ice_in t_soisno_v
    MoistureForm ->
      soilwaterMoistureForm cfg nlayers dtime qflx_infl_val zwt_val
        watsat_v bsw_v hksat_v sucsat_v icefrac_v
        rootsoi z_soil zi_soil dz_soil
        h2osoi_liq_in

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- | Safe vector index (0-based)
vix :: VU.Vector Double -> Int -> Double
vix v i
  | i >= 0 && i < VU.length v = v `VU.unsafeIndex` i
  | otherwise = 0.0
{-# INLINE vix #-}

-- | Clamp a value to [lo, hi]
clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)
{-# INLINE clamp #-}

-- | Update a single element in an immutable vector
updateLiq :: VU.Vector Double -> Int -> (Double -> Double) -> VU.Vector Double
updateLiq v i f
  | i >= 0 && i < VU.length v =
      let old = v `VU.unsafeIndex` i
      in v VU.// [(i, f old)]
  | otherwise = v
{-# INLINE updateLiq #-}

-- | Unzip a list of 5-tuples
unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 [] = ([],[],[],[],[])
unzip5 ((a,b,c,d,e):rest) =
  let (as, bs, cs, ds, es) = unzip5 rest
  in (a:as, b:bs, c:cs, d:ds, e:es)
