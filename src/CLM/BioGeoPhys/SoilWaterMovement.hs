{-# LANGUAGE BangPatterns #-}
-- | Richards equation solver for soil water movement.
-- Fortran: SoilWaterMovementMod.F90
--
-- Provides:
--   * Configuration types for soil water movement
--   * Ice impedance factor
--   * Hydraulic property computation (hk, smp, derivatives)
--   * Moisture flux computation at layer boundaries
--   * Tridiagonal system assembly (RHS and LHS)
--   * Adaptive time-stepping moisture-form Richards solver
--   * Zeng-Decker 2009 equilibrium method
--   * Aquifer recharge (qcharge) computation
--   * Baseflow sink placeholder
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SoilWaterMovement
  ( -- * Configuration
    SoilWaterMovementConfig(..)
  , defaultSoilWaterMovementConfig
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
  , SoilWaterResult(..)
    -- * Aquifer recharge
  , computeQcharge
    -- * Baseflow sink
  , baseflowSink
    -- * Zeng-Decker 2009 equilibrium method
  , ZengDeckerInput(..)
  , ZengDeckerResult(..)
  , equilibriumWaterContent
  , equilibriumMatricPotential
  , soilwaterZengDecker2009
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (denh2o, nlevsoi, nlevsno)
import CLM.Infrastructure.Tridiagonal (tridiagonalSolve)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Meters to millimeters
mToMM :: Double
mToMM = 1.0e3

-- | Minimum matric potential [mm]
smpmin :: Double
smpmin = -1.0e8

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
  = BCFlux           -- ^ Specified flux (infiltration at top, free drainage at bottom)
  | BCZeroFlux       -- ^ Zero flux
  | BCWaterTable     -- ^ Water table boundary
  | BCAquifer        -- ^ Coupled aquifer layer
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for soil water movement solver
data SoilWaterMovementConfig = SoilWaterMovementConfig
  { swmc_method      :: !SolnMethod
  , swmc_upper_bc    :: !BoundaryCondition
  , swmc_lower_bc    :: !BoundaryCondition
  , swmc_dtmin       :: !Double  -- ^ Minimum substep length [s]
  , swmc_verySmall   :: !Double  -- ^ Tolerance for substep completion
  , swmc_xTolerUpper :: !Double  -- ^ Upper error tolerance (halve substep)
  , swmc_xTolerLower :: !Double  -- ^ Lower error tolerance (double substep)
  , swmc_e_ice       :: !Double  -- ^ Soil ice impedance factor
  } deriving (Show, Eq)

-- | Default configuration
defaultSoilWaterMovementConfig :: SoilWaterMovementConfig
defaultSoilWaterMovementConfig = SoilWaterMovementConfig
  { swmc_method      = ZengDecker2009
  , swmc_upper_bc    = BCFlux
  , swmc_lower_bc    = BCAquifer
  , swmc_dtmin       = 60.0
  , swmc_verySmall   = 1.0e-8
  , swmc_xTolerUpper = 1.0e-1
  , swmc_xTolerLower = 1.0e-2
  , swmc_e_ice       = 6.0
  }

--------------------------------------------------------------------------------
-- Hydraulic property functions
--------------------------------------------------------------------------------

-- | Ice impedance factor: 10^(-e_ice * icefrac)
-- Ported from IceImpedance in SoilWaterMovementMod.F90
iceImpedance :: Double -> Double -> Double
iceImpedance icefrac eIce = 10.0 ** (negate eIce * icefrac)

-- | Clapp-Hornberger hydraulic conductivity and derivative.
-- Returns (hk, dhk/ds).
-- Ported from soil_hk for Clapp-Hornberger SWRC.
clappHornbergerHk :: Double -> Double -> Double -> Double -> (Double, Double)
clappHornbergerHk s_raw imped hksat_val bsw_val =
  let s = clamp 0.01 1.0 s_raw
      exp1 = 2.0 * bsw_val + 3.0
      hk_unimpeded = hksat_val * s ** exp1
      hk_val = imped * hk_unimpeded
      dhkds = imped * exp1 * hksat_val * s ** (exp1 - 1.0)
  in (hk_val, dhkds)

-- | Clapp-Hornberger matric potential and derivative.
-- Returns (smp, dsmp/ds).
-- Ported from soil_suction for Clapp-Hornberger SWRC.
clappHornbergerSmp :: Double -> Double -> Double -> (Double, Double)
clappHornbergerSmp s_raw sucsat_val bsw_val =
  let s = clamp 0.01 1.0 s_raw
      smp_val = negate sucsat_val * s ** (negate bsw_val)
      smp_clamped = max smpmin smp_val
      dsmpds = negate bsw_val * smp_clamped / s
  in (smp_clamped, dsmpds)

-- | Hydraulic properties for all soil layers
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
  :: Int                    -- ^ nlayers
  -> Double                 -- ^ e_ice
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> VU.Vector Double       -- ^ bsw (nlevsoi)
  -> VU.Vector Double       -- ^ hksat (nlevsoi)
  -> VU.Vector Double       -- ^ sucsat (nlevsoi)
  -> VU.Vector Double       -- ^ icefrac (nlevsoi)
  -> VU.Vector Double       -- ^ vwc_liq (nlevsoi)
  -> HydraulicProps
computeHydraulicProperties nlayers eIce watsat_v bsw_v hksat_v sucsat_v icefrac_v vwc_liq_v =
  let s2 j = clamp 0.01 1.0 (vix vwc_liq_v j / vix watsat_v j)

      computeLayer j =
        let (s1_raw, imp) =
              if j >= nlayers - 1
              then (s2 j, iceImpedance (vix icefrac_v j) eIce)
              else ( 0.5 * (s2 j + s2 (j+1))
                   , iceImpedance (0.5 * (vix icefrac_v j + vix icefrac_v (j+1))) eIce
                   )
            s1 = clamp 0.01 1.0 s1_raw
            (hk_val, dhkds) = clappHornbergerHk s1 imp (vix hksat_v j) (vix bsw_v j)
            (smp_val, dsmpdsi) = clappHornbergerSmp (s2 j) (vix sucsat_v j) (vix bsw_v j)
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
-- Flux computation
--------------------------------------------------------------------------------

-- | Result of flux computation at layer boundaries
data FluxResult = FluxResult
  { fr_qin    :: !(VU.Vector Double)
  , fr_qout   :: !(VU.Vector Double)
  , fr_dqidw0 :: !(VU.Vector Double)
  , fr_dqidw1 :: !(VU.Vector Double)
  , fr_dqodw1 :: !(VU.Vector Double)
  , fr_dqodw2 :: !(VU.Vector Double)
  } deriving (Show)

-- | Compute fluxes at layer boundaries and their derivatives.
-- Ported from compute_moisture_fluxes_and_derivs in SoilWaterMovementMod.F90
computeMoistureFluxes
  :: Int                    -- ^ nlayers
  -> BoundaryCondition      -- ^ Upper BC
  -> BoundaryCondition      -- ^ Lower BC
  -> Double                 -- ^ qflx_infl [mm/s]
  -> Double                 -- ^ zwt [m]
  -> VU.Vector Double       -- ^ z_col (soil midpoints, nlevsoi)
  -> VU.Vector Double       -- ^ zi_col (soil interfaces, nlevsoi+1)
  -> VU.Vector Double       -- ^ watsat (nlevsoi)
  -> HydraulicProps
  -> FluxResult
computeMoistureFluxes nlayers upperBC lowerBC qflx_infl_val zwt_val
                      z_col zi_col watsat_v hp =
  let hk_v    = hp_hk hp
      smp_v   = hp_smp hp
      dhkdw_v = hp_dhkdw hp
      dsmpdw_v = hp_dsmpdw hp

      interiorFlux j =
        let num = vix smp_v (j+1) - vix smp_v j
            den = mToMM * (vix z_col (j+1) - vix z_col j)
            dhkds1 = 0.5 * vix dhkdw_v j / vix watsat_v j
            dhkds2 = 0.5 * vix dhkdw_v j / vix watsat_v (j+1)
            qout_val = negate (vix hk_v j) * num / den + vix hk_v j
            dqodw1_val = (vix hk_v j * vix dsmpdw_v j - dhkds1 * num) / den + dhkds1
            dqodw2_val = (negate (vix hk_v j) * vix dsmpdw_v (j+1) - dhkds2 * num) / den + dhkds2
        in (qout_val, dqodw1_val, dqodw2_val)

      bottomFlux j = case lowerBC of
        BCFlux ->
          let qout_val = vix hk_v j
              dqodw1_val = vix dhkdw_v j / vix watsat_v j
          in (qout_val, dqodw1_val, 0.0)
        BCZeroFlux -> (0.0, 0.0, 0.0)
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

      outFlux j
        | nlayers == 1     = bottomFlux j
        | j == nlayers - 1 = bottomFlux j
        | otherwise        = interiorFlux j

      outQout   = VU.generate nlayers $ \j -> let (q, _, _) = outFlux j in q
      outDqodw1 = VU.generate nlayers $ \j -> let (_, d, _) = outFlux j in d
      outDqodw2 = VU.generate nlayers $ \j -> let (_, _, d) = outFlux j in d

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
  { ts_amx :: !(VU.Vector Double)
  , ts_bmx :: !(VU.Vector Double)
  , ts_cmx :: !(VU.Vector Double)
  , ts_rmx :: !(VU.Vector Double)
  } deriving (Show)

-- | Compute RHS of moisture-based Richards equation.
-- Ported from compute_RHS_moisture_form in SoilWaterMovementMod.F90
computeRHSMoistureForm
  :: Int -> VU.Vector Double -> VU.Vector Double
  -> VU.Vector Double -> VU.Vector Double -> VU.Vector Double
computeRHSMoistureForm nlayers sink qin_v qout_v dt_dz =
  VU.generate nlayers $ \j ->
    let fluxNet = vix qin_v j - vix qout_v j - vix sink j
    in negate fluxNet * vix dt_dz j

-- | Compute LHS tridiagonal matrix for moisture-based Richards equation.
-- Ported from compute_LHS_moisture_form in SoilWaterMovementMod.F90
computeLHSMoistureForm :: Int -> VU.Vector Double -> FluxResult -> TridiagSystem
computeLHSMoistureForm nlayers dt_dz fr =
  let amx = VU.generate nlayers $ \j ->
        if j == 0 then 0.0
        else vix (fr_dqidw0 fr) j * vix dt_dz j

      bmx = VU.generate nlayers $ \j ->
        (-1.0) - (negate (vix (fr_dqidw1 fr) j) + vix (fr_dqodw1 fr) j) * vix dt_dz j

      cmx = VU.generate nlayers $ \j ->
        if j == nlayers - 1 then 0.0
        else negate (vix (fr_dqodw2 fr) j) * vix dt_dz j

      rmx = VU.replicate nlayers 0.0
  in TridiagSystem amx bmx cmx rmx

--------------------------------------------------------------------------------
-- Richards equation solver
--------------------------------------------------------------------------------

-- | Result from soil water movement solver
data SoilWaterResult = SoilWaterResult
  { swr_h2osoi_liq :: !(VU.Vector Double)  -- ^ Updated liquid water (snow+soil)
  , swr_qcharge    :: !Double              -- ^ Aquifer recharge [mm/s]
  , swr_nsubsteps  :: !Int                 -- ^ Number of adaptive substeps
  } deriving (Show)

-- | Moisture-based Richards equation solver with adaptive time stepping.
-- Single-column, pure.
-- Ported from soilwater_moisture_form in SoilWaterMovementMod.F90
soilwaterMoistureForm
  :: SoilWaterMovementConfig
  -> Int                     -- ^ nlayers
  -> Double                  -- ^ dtime [s]
  -> Double                  -- ^ qflx_infl [mm/s]
  -> Double                  -- ^ zwt [m]
  -> VU.Vector Double        -- ^ watsat (nlevsoi)
  -> VU.Vector Double        -- ^ bsw (nlevsoi)
  -> VU.Vector Double        -- ^ hksat (nlevsoi)
  -> VU.Vector Double        -- ^ sucsat (nlevsoi)
  -> VU.Vector Double        -- ^ icefrac (nlevsoi)
  -> VU.Vector Double        -- ^ qflx_rootsoi (nlevsoi)
  -> VU.Vector Double        -- ^ z_soil (nlevsoi)
  -> VU.Vector Double        -- ^ zi_soil (nlevsoi+1)
  -> VU.Vector Double        -- ^ dz_soil (nlevsoi) [m]
  -> VU.Vector Double        -- ^ h2osoi_liq (snow+soil, full array)
  -> SoilWaterResult
soilwaterMoistureForm cfg nlayers dtime qflx_infl_val zwt_val
                      watsat_v bsw_v hksat_v sucsat_v icefrac_v
                      rootsoi z_soil zi_soil dz_soil h2osoi_liq_in =
  substepLoop 0 dtime 0.0 0.0 h2osoi_liq_in
  where
    joff = nlevsno

    substepLoop :: Int -> Double -> Double -> Double -> VU.Vector Double -> SoilWaterResult
    substepLoop !nstep !dtsub !dtdone !qcharge_acc !liq
      | nstep > 1000 = SoilWaterResult liq qcharge_acc nstep
      | otherwise =
          let vwc_liq = VU.generate nlayers $ \j ->
                max 1.0e-6 (vix liq (joff + j)) / (vix dz_soil j * denh2o)

              dt_dz = VU.generate nlayers $ \j ->
                dtsub / (mToMM * vix dz_soil j)

              hp = computeHydraulicProperties nlayers (swmc_e_ice cfg)
                     watsat_v bsw_v hksat_v sucsat_v icefrac_v vwc_liq

              fr = computeMoistureFluxes nlayers
                     (swmc_upper_bc cfg) (swmc_lower_bc cfg)
                     qflx_infl_val zwt_val
                     z_soil zi_soil watsat_v hp

              rmx = computeRHSMoistureForm nlayers rootsoi
                      (fr_qin fr) (fr_qout fr) dt_dz

              ts = computeLHSMoistureForm nlayers dt_dz fr

              dwat = tridiagonalSolve (ts_amx ts) (ts_bmx ts) (ts_cmx ts) rmx

              errorMax = VU.foldl' max 0.0 $ VU.generate nlayers $ \j ->
                let fluxNet0 = vix dwat j / vix dt_dz j
                    fluxNet1 = vix (fr_qin fr) j - vix (fr_qout fr) j - vix rootsoi j
                in abs (fluxNet1 - fluxNet0) * dtsub * 0.5

          in if errorMax > swmc_xTolerUpper cfg && dtsub > swmc_dtmin cfg
             then substepLoop (nstep + 1) (max (dtsub / 2.0) (swmc_dtmin cfg))
                              dtdone qcharge_acc liq
             else
               let liq' = VU.imap (\i x ->
                     if i >= joff && i < joff + nlayers
                     then x + vix dwat (i - joff) * (mToMM * vix dz_soil (i - joff))
                     else x) liq

                   qcTemp = case swmc_lower_bc cfg of
                     BCFlux ->
                       vix (hp_hk hp) (nlayers - 1)
                       + vix (hp_dhkdw hp) (nlayers - 1) * vix dwat (nlayers - 1)
                     BCZeroFlux -> 0.0
                     BCWaterTable ->
                       vix (fr_qout fr) (nlayers - 1)
                       + vix (fr_dqodw1 fr) (nlayers - 1) * vix dwat (nlayers - 1)
                     _ -> 0.0

                   qcharge_acc' = qcharge_acc + qcTemp * (dtsub / dtime)
                   dtdone' = dtdone + dtsub

               in if abs (dtime - dtdone') < swmc_verySmall cfg
                  then SoilWaterResult liq' qcharge_acc' (nstep + 1)
                  else let dtsub' = if errorMax < swmc_xTolerLower cfg
                                    then dtsub * 2.0
                                    else dtsub
                           dtsub'' = min dtsub' (dtime - dtdone')
                       in substepLoop (nstep + 1) dtsub'' dtdone' qcharge_acc' liq'

--------------------------------------------------------------------------------
-- Aquifer recharge
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
  -> VU.Vector Double    -- ^ z_soil (nlevsoi)
  -> VU.Vector Double    -- ^ zi_soil (nlevsoi+1)
  -> Double              -- ^ e_ice
  -> Double              -- ^ qcharge [mm/s]
computeQcharge zwt_val dtime watsat_v bsw_v hksat_v sucsat_v
               icefrac_v vwc_liq_v smp_v z_soil zi_soil eIce =
  let nl = nlevsoi
      jwt = findJwtSoil zwt_val zi_soil nl
  in if jwt < nl
     then let jw = jwt
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
     else 0.0

--------------------------------------------------------------------------------
-- Baseflow sink
--------------------------------------------------------------------------------

-- | Apply baseflow as a vertically distributed sink.
-- Currently a placeholder that returns zero vector.
-- Ported from BaseflowSink in SoilWaterMovementMod.F90
baseflowSink :: Int -> VU.Vector Double
baseflowSink nl = VU.replicate nl 0.0

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe 0-based vector index
vix :: VU.Vector Double -> Int -> Double
vix v i = v `VU.unsafeIndex` i

-- | Clamp a value between lo and hi
clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)

-- | Unzip a list of 5-tuples
unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5 [] = ([],[],[],[],[])
unzip5 ((a,b,c,d,e):rest) =
  let (as,bs,cs,ds,es) = unzip5 rest
  in (a:as, b:bs, c:cs, d:ds, e:es)

-- | Find jwt (0-indexed): index of soil layer just above water table
findJwt :: Double -> VU.Vector Double -> Int -> Int
findJwt zwt_val zi_v nl = go 0
  where
    go j
      | j >= nl   = nl - 1
      | zwt_val <= vix zi_v j = max 0 (j - 1)
      | otherwise = go (j + 1)

-- | Find jwt for soil column (0-indexed)
findJwtSoil :: Double -> VU.Vector Double -> Int -> Int
findJwtSoil zwt_val zi_v nl = go 0
  where
    go j
      | j >= nl   = nl
      | zwt_val <= vix zi_v j = j
      | otherwise = go (j + 1)

-- =========================================================================
-- Zeng-Decker 2009 Equilibrium Method
-- =========================================================================

-- | Input for the Zeng-Decker 2009 equilibrium solver.
data ZengDeckerInput = ZengDeckerInput
  { zdi_nlayers   :: !Int
  , zdi_dtime     :: !Double          -- ^ timestep [s]
  , zdi_qflx_infl :: !Double          -- ^ infiltration [mm/s]
  , zdi_zwt       :: !Double          -- ^ water table depth [m]
  , zdi_watsat    :: !(VU.Vector Double) -- ^ porosity per layer
  , zdi_bsw       :: !(VU.Vector Double) -- ^ Clapp-Hornberger b
  , zdi_hksat     :: !(VU.Vector Double) -- ^ saturated hydraulic conductivity [mm/s]
  , zdi_sucsat    :: !(VU.Vector Double) -- ^ saturated matric suction [mm]
  , zdi_icefrac   :: !(VU.Vector Double) -- ^ ice fraction per layer
  , zdi_rootsoi   :: !(VU.Vector Double) -- ^ root water sink [mm/s]
  , zdi_z_soil    :: !(VU.Vector Double) -- ^ layer midpoint depth [m]
  , zdi_zi_soil   :: !(VU.Vector Double) -- ^ layer interface depth [m] (nlayers+1)
  , zdi_dz_soil   :: !(VU.Vector Double) -- ^ layer thickness [m]
  , zdi_h2osoi_liq :: !(VU.Vector Double) -- ^ liquid water (full snow+soil array)
  , zdi_h2osoi_vol :: !(VU.Vector Double) -- ^ volumetric water content per soil layer
  , zdi_t_soisno  :: !(VU.Vector Double)  -- ^ soil temperature (full array)
  , zdi_e_ice     :: !Double            -- ^ ice impedance parameter
  , zdi_smpmin_val :: !Double           -- ^ minimum matric potential [mm]
  } deriving (Show)

-- | Result from the Zeng-Decker solver.
data ZengDeckerResult = ZengDeckerResult
  { zdr_h2osoi_liq :: !(VU.Vector Double) -- ^ updated liquid water (full array)
  , zdr_qcharge    :: !Double             -- ^ aquifer recharge [mm/s]
  , zdr_smp        :: !(VU.Vector Double) -- ^ matric potential per layer [mm]
  , zdr_hk         :: !(VU.Vector Double) -- ^ hydraulic conductivity per layer [mm/s]
  , zdr_vol_eq     :: !(VU.Vector Double) -- ^ equilibrium water content per layer
  } deriving (Show)

-- | Compute equilibrium volumetric water content for a soil layer
-- based on its position relative to the water table.
-- Uses Clapp-Hornberger integral from the water table to the layer boundaries.
equilibriumWaterContent :: Double -- ^ zwt_mm (water table depth in mm)
                        -> Double -- ^ zi_top (layer top interface depth in mm)
                        -> Double -- ^ zi_bot (layer bottom interface depth in mm)
                        -> Double -- ^ watsat (porosity)
                        -> Double -- ^ bsw (Clapp-Hornberger b)
                        -> Double -- ^ sucsat (saturated suction in mm)
                        -> Double -- ^ equilibrium vol water content
equilibriumWaterContent !zwtmm !ziTop !ziBot !watsat_j !bsw_j !sucsat_j
  -- Fully below water table: saturated
  | zwtmm <= ziTop = watsat_j
  -- Partially saturated: integrate Clapp-Hornberger from WT to layer boundaries
  | zwtmm < ziBot && zwtmm > ziTop =
    let !expo = 1.0 - 1.0 / bsw_j
        !tempi = 1.0
        !temp0 = ((sucsat_j + zwtmm - ziTop) / sucsat_j) ** expo
        !voleq1 = negate sucsat_j * watsat_j / expo / (zwtmm - ziTop) * (tempi - temp0)
        !dz_layer = ziBot - ziTop
        !vol_eq = (voleq1 * (zwtmm - ziTop) + watsat_j * (ziBot - zwtmm)) / dz_layer
    in clamp 0.0 watsat_j vol_eq
  -- Fully above water table: equilibrium from Clapp-Hornberger
  | otherwise =
    let !expo = 1.0 - 1.0 / bsw_j
        !tempi = ((sucsat_j + zwtmm - ziBot) / sucsat_j) ** expo
        !temp0 = ((sucsat_j + zwtmm - ziTop) / sucsat_j) ** expo
        !dz_layer = ziBot - ziTop
        !vol_eq = negate sucsat_j * watsat_j / expo / dz_layer * (tempi - temp0)
    in clamp 0.0 watsat_j vol_eq

-- | Compute equilibrium matric potential from equilibrium water content.
equilibriumMatricPotential :: Double -> Double -> Double -> Double -> Double
equilibriumMatricPotential !vol_eq !watsat_j !sucsat_j !bsw_j =
  let !s = max 0.01 (vol_eq / watsat_j)
  in max smpmin (negate sucsat_j * s ** (negate bsw_j))

-- | Zeng-Decker 2009 equilibrium solver.
-- Computes fluxes relative to an equilibrium water content profile
-- determined by the water table depth. The deviation from equilibrium
-- drives the flow, replacing the raw matric potential gradient.
soilwaterZengDecker2009
  :: SoilWaterMovementConfig
  -> ZengDeckerInput
  -> ZengDeckerResult
soilwaterZengDecker2009 !cfg !inp =
  let !nl = zdi_nlayers inp
      !dt = zdi_dtime inp
      !zwtmm = zdi_zwt inp * mToMM
      !joff = nlevsno
      !eIce = zdi_e_ice inp

      -- Layer interface depths in mm
      !zimm = VU.generate (nl + 1) $ \j ->
        if j == 0 then 0.0
        else zdi_zi_soil inp VU.! (j - 1) * mToMM

      -- Compute equilibrium water content and matric potential per layer
      !vol_eq = VU.generate nl $ \j ->
        let !ziTop = zimm VU.! j
            !ziBot = zimm VU.! (j + 1)
        in equilibriumWaterContent zwtmm ziTop ziBot
             (vix (zdi_watsat inp) j)
             (vix (zdi_bsw inp) j)
             (vix (zdi_sucsat inp) j)

      !zq = VU.generate nl $ \j ->
        equilibriumMatricPotential (vol_eq VU.! j)
          (vix (zdi_watsat inp) j)
          (vix (zdi_sucsat inp) j)
          (vix (zdi_bsw inp) j)

      -- Volumetric liquid water content per layer
      !vwc_liq = VU.generate nl $ \j ->
        let !liq_j = vix (zdi_h2osoi_liq inp) (j + joff)
            !dz_j = vix (zdi_dz_soil inp) j
        in max 1.0e-6 (liq_j / (dz_j * denh2o))

      -- Matric potential per layer
      !smp_v = VU.generate nl $ \j ->
        let !s = clamp 0.01 1.0 (vwc_liq VU.! j / vix (zdi_watsat inp) j)
        in max (zdi_smpmin_val inp) (negate (vix (zdi_sucsat inp) j) * s ** (negate (vix (zdi_bsw inp) j)))

      -- Hydraulic conductivity per layer
      !hk_v = VU.generate nl $ \j ->
        let !s = clamp 0.01 1.0 (vwc_liq VU.! j / vix (zdi_watsat inp) j)
            !imp = iceImpedance (vix (zdi_icefrac inp) j) eIce
            !(hk_val, _) = clappHornbergerHk s imp (vix (zdi_hksat inp) j) (vix (zdi_bsw inp) j)
        in hk_val

      -- Equilibrium-adjusted flux at each interface
      -- The Zeng-Decker method replaces (smp_j - smp_{j+1}) with
      -- (smp_j - zq_j) - (smp_{j+1} - zq_{j+1}) = (smp_j - smp_{j+1}) - (zq_j - zq_{j+1})
      -- effectively subtracting the equilibrium gradient
      !qflux = VU.generate (nl + 1) $ \j ->
        if j == 0
        then zdi_qflx_infl inp  -- top boundary: infiltration
        else if j == nl
        then 0.0  -- bottom boundary: zero flux (aquifer handled separately)
        else let !hk_avg = 0.5 * (hk_v VU.! (j-1) + hk_v VU.! j)
                 !smp_grad = (smp_v VU.! (j-1) - smp_v VU.! j)
                 !zq_grad = (zq VU.! (j-1) - zq VU.! j)
                 !zmm_j  = vix (zdi_z_soil inp) (j-1) * mToMM
                 !zmm_j1 = vix (zdi_z_soil inp) j * mToMM
                 !den = zmm_j1 - zmm_j
                 -- Gravity-adjusted flux using equilibrium deviation
                 !num = (smp_grad - zq_grad)
             in if den > 0.0
                then negate hk_avg * num / den - hk_avg
                else 0.0

      -- Update liquid water: dW/dt = (q_in - q_out - sink) * dt
      !liq_updated = VU.generate (VU.length (zdi_h2osoi_liq inp)) $ \jj ->
        if jj < joff || jj >= joff + nl
        then vix (zdi_h2osoi_liq inp) jj
        else let !j = jj - joff
                 !q_in = qflux VU.! j
                 !q_out = qflux VU.! (j + 1)
                 !sink = vix (zdi_rootsoi inp) j
                 !liq_old = vix (zdi_h2osoi_liq inp) jj
                 !dz_mm = vix (zdi_dz_soil inp) j * mToMM
                 !liq_new = liq_old + (q_in - q_out - sink) * dt
                 -- Enforce bounds: 0 <= liq <= watsat * dz * denh2o
                 !liq_max = vix (zdi_watsat inp) j * vix (zdi_dz_soil inp) j * denh2o
             in clamp 0.0 liq_max liq_new

      -- Aquifer recharge
      !qcharge = computeQcharge
                   (zdi_zwt inp) dt
                   (zdi_watsat inp) (zdi_bsw inp) (zdi_hksat inp)
                   (zdi_sucsat inp) (zdi_icefrac inp)
                   vwc_liq smp_v
                   (zdi_z_soil inp) (zdi_zi_soil inp)
                   eIce

  in ZengDeckerResult
     { zdr_h2osoi_liq = liq_updated
     , zdr_qcharge = qcharge
     , zdr_smp = smp_v
     , zdr_hk = hk_v
     , zdr_vol_eq = vol_eq
     }
