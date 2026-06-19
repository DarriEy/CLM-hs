-- | Soil and snow temperature solver.
-- Fortran: SoilTemperatureMod
-- Julia:   src/biogeophys/soil_temperature.jl
--
-- Implements the implicit Crank-Nicolson diffusion scheme with phase change
-- for a single non-urban, non-lake column.  Urban columns and excess-ice
-- coordinate stretching are not yet handled; they can be added later when the
-- urban and excess-ice modules are ported.
--
-- Key functions (all pure or in ST):
--
--   soilThermProp        -- thermal conductivity and heat capacity per layer
--   computeHeatDiffFlux  -- heat diffusion fluxes fn and factor fact
--   buildTridiagSystem   -- assemble tridiagonal a/b/c/r for Crank-Nicolson
--   solveSoilTemperature -- top-level driver: therm props -> solve -> phase change
--   phaseChangeBeta      -- melting/freezing within snow and soil layers
--
module CLM.BioGeoPhys.SoilTemperature
  ( -- * Top-level driver
    solveSoilTemperature
  , SoilTempInput(..)
  , SoilTempOutput(..)
    -- * Thermal property computation
  , soilThermProp
  , ThermPropInput(..)
  , ThermPropOutput(..)
    -- * Heat diffusion
  , computeHeatDiffFlux
  , HeatDiffInput(..)
  , HeatDiffOutput(..)
    -- * Tridiagonal assembly and solve
  , buildTridiagSystem
  , TridiagInput(..)
    -- * Phase change
  , phaseChangeBeta
  , PhaseChangeInput(..)
  , PhaseChangeOutput(..)
    -- * Constants
  , cnfac
  , capr
  , thinSfcLayer
  , thkBedrock
  , csolBedrock
    -- * Snow thermal conductivity method
  , SnowThermalCond(..)
  ) where

import qualified Data.Vector.Unboxed as VU
import Debug.Trace (trace)
import CLM.Constants.PhysicalConstants
  ( tfrz, grav, denh2o, denice, cpice, cpliq, hfus
  , tkwat, tkice, tkair
  , nlevgrnd, nlevsno
  )
import CLM.Infrastructure.Tridiagonal (tridiagonalSolve)

-- =========================================================================
-- Constants (Fortran: clm_varpar / SoilTemperatureMod)
-- =========================================================================

-- | Crank-Nicolson weighting factor (0=fully implicit, 0.5=standard C-N)
cnfac :: Double
cnfac = 0.5

-- | Tunable constant for surface temperature computation (CAPR in Fortran)
capr :: Double
capr = 0.34

-- | Threshold for thin surface layer [m or kg/m2]
thinSfcLayer :: Double
thinSfcLayer = 1.0e-6

-- | Thermal conductivity of bedrock [W/m/K]
thkBedrock :: Double
thkBedrock = 3.0

-- | Volumetric heat capacity of bedrock [J/m3/K]
csolBedrock :: Double
csolBedrock = 2.0e6

-- =========================================================================
-- Thermal properties: conductivity and heat capacity
-- =========================================================================

-- | Input record for thermal property computation.
-- All vectors are indexed 0..nlevsno+nlevgrnd-1 where indices 0..nlevsno-1
-- are snow layers (bottom-up, only snl..nlevsno-1 active) and
-- nlevsno..nlevsno+nlevgrnd-1 are soil layers.
data ThermPropInput = ThermPropInput
  { tpi_snl           :: !Int              -- ^ Number of snow layers (<= 0)
  , tpi_t_soisno      :: !(VU.Vector Double) -- ^ Layer temperatures [K]
  , tpi_h2osoi_liq    :: !(VU.Vector Double) -- ^ Liquid water per layer [kg/m2]
  , tpi_h2osoi_ice    :: !(VU.Vector Double) -- ^ Ice per layer [kg/m2]
  , tpi_dz            :: !(VU.Vector Double) -- ^ Layer thickness [m]
  , tpi_z             :: !(VU.Vector Double) -- ^ Layer midpoint depth [m]
  , tpi_zi            :: !(VU.Vector Double) -- ^ Interface depths [m] (len nlevsno+nlevgrnd+1)
  , tpi_watsat        :: !(VU.Vector Double) -- ^ Porosity (soil-only, len nlevgrnd)
  , tpi_tkmg          :: !(VU.Vector Double) -- ^ Mineral thermal cond [W/m/K]
  , tpi_tkdry         :: !(VU.Vector Double) -- ^ Dry thermal cond [W/m/K]
  , tpi_csol          :: !(VU.Vector Double) -- ^ Dry vol heat cap [J/m3/K]
  , tpi_tksatu        :: !(VU.Vector Double) -- ^ Saturated thermal cond [W/m/K]
  , tpi_nbedrock      :: !Int              -- ^ Bedrock top layer index (1-based soil)
  , tpi_h2osno_no_layers :: !Double        -- ^ Unresolved snow SWE [kg/m2]
  , tpi_frac_sno      :: !Double           -- ^ Effective snow fraction
  , tpi_h2osfc        :: !Double           -- ^ Surface water [kg/m2]
  , tpi_snowCondMethod :: !SnowThermalCond -- ^ Snow thermal conductivity method
  , tpi_thk_override  :: !(Maybe (VU.Vector Double))
      -- ^ Optional injected per-layer thermal conductivity [W/m/K], indexed
      -- 0..nlevsno+nlevgrnd-1 (snow layers first, then soil) matching the
      -- Fortran dump's levtot ordering. When Just, overrides the computed
      -- per-layer @thk@ value (bedrock handling still applied). When Nothing,
      -- behavior is byte-identical to the default conductivity computation.
  , tpi_cv_override   :: !(Maybe (VU.Vector Double))
      -- ^ Optional injected per-layer volumetric heat capacity [J/m2/K], same
      -- levtot ordering as 'tpi_thk_override'. Fill values (>=1e30) and inactive
      -- snow slots are ignored. When Nothing, the default 'cv' is computed.
  } deriving (Show)

-- | Snow thermal conductivity method
data SnowThermalCond = Jordan1991 | Sturm1997
  deriving (Show, Eq)

-- | Output of thermal property computation.
data ThermPropOutput = ThermPropOutput
  { tpo_tk       :: !(VU.Vector Double) -- ^ Interface thermal conductivity [W/m/K]
  , tpo_cv       :: !(VU.Vector Double) -- ^ Layer volumetric heat capacity [J/m2/K]
  , tpo_thk      :: !(VU.Vector Double) -- ^ Layer-centre thermal conductivity [W/m/K]
  , tpo_tk_h2osfc :: !Double            -- ^ Thermal cond for surface water [W/m/K]
  } deriving (Show)

-- | Compute thermal conductivities and heat capacities for all snow+soil layers.
-- Ported from soil_therm_prop! in the Julia source.
soilThermProp :: ThermPropInput -> ThermPropOutput
soilThermProp inp = ThermPropOutput
  { tpo_tk       = tkInterf
  , tpo_cv       = cvOut
  , tpo_thk      = thkOut
  , tpo_tk_h2osfc = tk_h2osfc_val
  }
  where
    joff  = nlevsno - 1  -- 0-based: Fortran layer j → index j + joff
    nlev  = nlevsno + nlevgrnd  -- total number of layer slots

    snl   = tpi_snl inp
    t     = tpi_t_soisno inp
    liq   = tpi_h2osoi_liq inp
    ice   = tpi_h2osoi_ice inp
    dz    = tpi_dz inp
    z     = tpi_z inp
    ws    = tpi_watsat inp       -- soil-only, 0-indexed → soil layer j
    tkmg_ = tpi_tkmg inp
    tkdry_= tpi_tkdry inp
    csol_ = tpi_csol inp
    nbed  = tpi_nbedrock inp
    h2osno_nl = tpi_h2osno_no_layers inp
    frac_sno  = tpi_frac_sno inp
    thkOvr    = tpi_thk_override inp  -- optional injected per-layer thk

    -- Override lookup for slot @jj@ (same 0-based slot ordering: snow first,
    -- then soil). Returns the injected value if an override vector is present
    -- and the slot is in range.
    thkOverrideAt jj = case thkOvr of
      Just v | jj >= 0 && jj < VU.length v -> Just (v VU.! jj)
      _                                    -> Nothing

    cvOvr = tpi_cv_override inp  -- optional injected per-layer heat capacity
    -- Use the injected value only when present, finite, and not a dump fill
    -- (>=1e30) — inactive snow slots are written as fill.
    cvOverrideAt jj = case cvOvr of
      Just v | jj >= 0 && jj < VU.length v
             , let o = v VU.! jj
             , not (isNaN o) && o < 1.0e30 && o > 0.0 -> Just o
      _                                                -> Nothing

    -- ---- Layer-centre thermal conductivity (thk) ----
    thkOut = VU.generate nlev $ \jj ->
      let j = jj - joff  -- Fortran-style layer index (-nlevsno+1..nlevgrnd)
      in if j >= 1
         then soilThk j jj
         else if snl + 1 < 1 && j >= snl + 1 && j <= 0
              then snowThk jj
              else 0.0

    soilThk j jj =
      let ws_j    = ws  VU.! (j - 1)   -- soil-only 0-based
          tkmg_j  = tkmg_ VU.! (j - 1)
          tkdry_j = tkdry_ VU.! (j - 1)
          t_j     = t   VU.! jj
          liq_j   = liq VU.! jj
          ice_j   = ice VU.! jj
          dz_j    = dz  VU.! jj
          satw0   = (liq_j / denh2o + ice_j / denice) / (dz_j * ws_j)
          satw    = min 1.0 satw0
          dke     | satw <= 1.0e-7 = 0.0
                  | t_j >= tfrz    = max 0.0 (logBase 10 satw + 1.0)
                  | otherwise      = satw
          fl_den  = liq_j / (denh2o * dz_j) + ice_j / (denice * dz_j)
          fl      | fl_den > 0.0 = (liq_j / (denh2o * dz_j)) / fl_den
                  | otherwise    = 0.0
          dksat   = tkmg_j * tkwat ** (fl * ws_j) * tkice ** ((1.0 - fl) * ws_j)
          thk_computed | satw > 1.0e-7 = dke * dksat + (1.0 - dke) * tkdry_j
                       | otherwise     = tkdry_j
          -- Prefer the injected Fortran conductivity for this layer when present.
          thk_val = case thkOverrideAt jj of
                      Just o  -> o
                      Nothing -> thk_computed
      in if j > nbed then thkBedrock else thk_val

    snowThk jj =
      case thkOverrideAt jj of
        Just o  -> o
        Nothing ->
          let denom = max frac_sno 1.0e-6 * max (dz VU.! jj) 1.0e-6
              bw_val = (ice VU.! jj + liq VU.! jj) / denom
          in case tpi_snowCondMethod inp of
               Jordan1991 ->
                 tkair + (7.75e-5 * bw_val + 1.105e-6 * bw_val * bw_val) * (tkice - tkair)
               Sturm1997
                 | bw_val <= 156.0 -> 0.023 + 0.234 * (bw_val / 1000.0)
                 | otherwise       -> 0.138 - 1.01 * (bw_val / 1000.0)
                                      + 3.233 * (bw_val / 1000.0) ** 2

    -- ---- Interface thermal conductivity (tk) ----
    tkInterf = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= snl + 1 && j <= nlevgrnd - 1
         then let thk_j  = thkOut VU.! jj
                  thk_j1 = thkOut VU.! (jj + 1)
                  zj     = z VU.! jj
                  zj1    = z VU.! (jj + 1)
                  zij1   = colZi_at inp (jj + 1)
              in thk_j * thk_j1 * (zj1 - zj)
                 / (thk_j * (zj1 - zij1) + thk_j1 * (zij1 - zj))
         else if j == nlevgrnd
              then 0.0
              else 0.0

    -- ---- Heat capacity (cv) ----
    cvOut = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= 1
         then soilCv j jj
         else if snl + 1 < 1 && j >= snl + 1
              then snowCv jj
              else 0.0

    soilCv j jj =
      let ws_j   = ws   VU.! (j - 1)
          csol_j = csol_ VU.! (j - 1)
          dz_j   = dz   VU.! jj
          liq_j  = liq  VU.! jj
          ice_j  = ice  VU.! jj
          cv_base = csol_j * (1.0 - ws_j) * dz_j + ice_j * cpice + liq_j * cpliq
          cv_val  | j > nbed  = csolBedrock * dz_j
                  | otherwise = cv_base
          -- Add unresolved snow to top soil layer
          cv_adj  | j == 1 && h2osno_nl > 0.0 = cv_val + cpice * h2osno_nl
                  | otherwise                  = cv_val
      -- Prefer the injected Fortran heat capacity for this layer when present.
      in case cvOverrideAt jj of
           Just o  -> o
           Nothing -> cv_adj

    snowCv jj =
      if frac_sno > 0.0
      then max thinSfcLayer
               ((cpliq * (liq VU.! jj) + cpice * (ice VU.! jj)) / frac_sno)
      else thinSfcLayer

    -- ---- Surface water thermal conductivity ----
    tk_h2osfc_val =
      let zh2osfc = 1.0e-3 * (0.5 * tpi_h2osfc inp)
          thk1    = thkOut VU.! (joff + 1)  -- soil layer 1 thk
          z1      = z      VU.! (joff + 1)  -- soil layer 1 midpoint
      in tkwat * thk1 * (z1 + zh2osfc) / (tkwat * z1 + thk1 * zh2osfc)

-- | Access interface depth zi at a given 0-based index.
-- zi has length nlevsno+nlevgrnd+1.
colZi_at :: ThermPropInput -> Int -> Double
colZi_at inp jj = tpi_zi inp VU.! jj

-- =========================================================================
-- Heat diffusion fluxes and factor
-- =========================================================================

data HeatDiffInput = HeatDiffInput
  { hdi_snl      :: !Int
  , hdi_t_soisno :: !(VU.Vector Double)
  , hdi_z        :: !(VU.Vector Double)
  , hdi_zi       :: !(VU.Vector Double)  -- ^ Interface depths (len nlevsno+nlevgrnd+1)
  , hdi_dz       :: !(VU.Vector Double)
  , hdi_tk       :: !(VU.Vector Double)  -- ^ Interface thermal conductivity
  , hdi_cv       :: !(VU.Vector Double)  -- ^ Heat capacity
  , hdi_eflx_bot :: !Double              -- ^ Bottom heat flux [W/m2]
  , hdi_dtime    :: !Double              -- ^ Timestep [s]
  } deriving (Show)

data HeatDiffOutput = HeatDiffOutput
  { hdo_fn   :: !(VU.Vector Double)  -- ^ Heat flux at time n [W/m2]
  , hdo_fact :: !(VU.Vector Double)  -- ^ Factor dt/cv (with CAPR adjustment at top)
  } deriving (Show)

-- | Compute heat diffusion flux fn and time-step factor fact.
-- Ported from compute_heat_diff_flux_and_factor! (non-urban branch).
computeHeatDiffFlux :: HeatDiffInput -> HeatDiffOutput
computeHeatDiffFlux inp = HeatDiffOutput
  { hdo_fn   = fnVec
  , hdo_fact = factVec
  }
  where
    joff  = nlevsno - 1  -- 0-based: Fortran layer j → index j + joff
    nlev  = nlevsno + nlevgrnd  -- total number of layer slots
    snl_  = hdi_snl inp
    t     = hdi_t_soisno inp
    z     = hdi_z inp
    zi    = hdi_zi inp
    dz_v  = hdi_dz inp
    tk    = hdi_tk inp
    cv    = hdi_cv inp
    ebot  = hdi_eflx_bot inp
    dt    = hdi_dtime inp

    -- Fortran layer j -> 0-based index
    ix j = j + joff

    factVec = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= snl_ + 1 && j <= nlevgrnd
         then let cv_j = max 1.0e-6 (cv VU.! jj)
              in if j == snl_ + 1
                 then let z_j  = z  VU.! jj
                          zi_j = zi VU.! jj
                          z_j1 = z  VU.! (jj + 1)
                          dz_j = dz_v VU.! jj
                          denom = max 1.0e-6 (0.5 * (z_j - zi_j + capr * (z_j1 - zi_j)))
                      in (dt / cv_j) * dz_j / denom
                 else dt / cv_j
         else 0.0

    fnVec = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= snl_ + 1 && j <= nlevgrnd
         then if j <= nlevgrnd - 1
              then let z_j  = z VU.! jj
                       z_j1 = z VU.! (jj + 1)
                       t_j  = t VU.! jj
                       t_j1 = t VU.! (jj + 1)
                   in (tk VU.! jj) * (t_j1 - t_j) / (z_j1 - z_j)
              else ebot  -- bottom boundary
         else 0.0

-- =========================================================================
-- Tridiagonal system assembly
-- =========================================================================

-- | Input for building the tridiagonal system.
data TridiagInput = TridiagInput
  { tri_snl          :: !Int
  , tri_t_soisno     :: !(VU.Vector Double)
  , tri_z            :: !(VU.Vector Double)
  , tri_tk           :: !(VU.Vector Double)
  , tri_fn           :: !(VU.Vector Double)
  , tri_fact         :: !(VU.Vector Double)
  , tri_cv           :: !(VU.Vector Double)
  , tri_hs_top       :: !Double  -- ^ Top layer heat source [W/m2]
  , tri_dhsdT        :: !Double  -- ^ d(hs)/dT [W/m2/K]
  , tri_hs_soil      :: !Double  -- ^ Soil heat source [W/m2]
  , tri_sabg_lyr     :: !(VU.Vector Double) -- ^ Absorbed solar per layer [W/m2]
  , tri_frac_sno_eff :: !Double
  , tri_frac_h2osfc  :: !Double
  , tri_hs_h2osfc    :: !Double
  , tri_tk_h2osfc    :: !Double
  , tri_c_h2osfc     :: !Double
  , tri_dz_h2osfc    :: !Double
  , tri_t_h2osfc     :: !Double
  , tri_dtime        :: !Double
  } deriving (Show)

-- | Build and solve the tridiagonal temperature system.
-- Returns updated temperatures for snow+soil layers and surface water.
-- For simplicity this handles the non-urban case where the system runs
-- from snl+1 to nlevgrnd, plus a coupled surface-water equation handled
-- by correcting the soil-layer-1 row.
--
-- The approach:
--   1. Build a/b/c/r for the snow layers (snl+1..0) if any,
--      plus soil layers (1..nlevgrnd).
--   2. Apply snow-soil coupling between layer 0 and layer 1 by
--      encoding it in the tridiagonal bands.
--   3. Solve with tridiagonalSolve (Thomas algorithm).
--   4. Solve surface water temperature separately (single equation).
buildTridiagSystem
  :: TridiagInput
  -> (VU.Vector Double, Double)  -- ^ (updated t_soisno, updated t_h2osfc)
buildTridiagSystem inp = (tNew, t_h2osfc_new)
  where
    joff = nlevsno - 1  -- 0-based: Fortran layer j → index j + joff
    snl_ = tri_snl inp
    t    = tri_t_soisno inp
    z    = tri_z inp
    tk   = tri_tk inp
    fn   = tri_fn inp
    fact = tri_fact inp
    dt   = tri_dtime inp
    dhsdT_  = tri_dhsdT inp
    hs_top_ = tri_hs_top inp
    hs_soil_= tri_hs_soil inp
    sabg    = tri_sabg_lyr inp
    fse     = tri_frac_sno_eff inp
    fh2o    = tri_frac_h2osfc inp
    hs_h2o  = tri_hs_h2osfc inp
    tk_h2o  = tri_tk_h2osfc inp
    c_h2o   = tri_c_h2osfc inp
    dz_h2o  = tri_dz_h2osfc inp
    t_h2o   = tri_t_h2osfc inp

    -- System runs from Fortran layer snl_+1 to nlevgrnd
    jtop = snl_ + 1   -- first active Fortran layer
    jbot = nlevgrnd    -- last Fortran layer
    n    = jbot - jtop + 1

    -- Map Fortran layer j to local system index k = j - jtop
    k j = j - jtop
    -- Map local system index back to vector index
    vix j = j + joff

    -- Build tridiagonal coefficients
    aVec = VU.generate n $ \ki ->
      let j = ki + jtop
          jj = vix j
      in if ki == 0
         then 0.0   -- top row has no sub-diagonal
         else let dzm = (z VU.! jj) - (z VU.! (jj - 1))
                  tk_below = tk VU.! (jj - 1)
              in if j == 1 && snl_ < 0
                 -- Soil layer 1 receives snow coupling via frac_sno_eff
                 then -(1.0 - cnfac) * (fact VU.! jj) * fse * tk_below / dzm
                 else -(1.0 - cnfac) * (fact VU.! jj) * tk_below / dzm

    cVec = VU.generate n $ \ki ->
      let j = ki + jtop
          jj = vix j
      in if j >= nlevgrnd
         then 0.0   -- bottom row has no super-diagonal
         else let dzp = (z VU.! (jj + 1)) - (z VU.! jj)
              in -(1.0 - cnfac) * (fact VU.! jj) * (tk VU.! jj) / dzp

    bVec = VU.generate n $ \ki ->
      let j = ki + jtop
          jj = vix j
      in if j == jtop
         then -- Top active layer
              let dzp = (z VU.! (jj + 1)) - (z VU.! jj)
              in 1.0 + (1.0 - cnfac) * (fact VU.! jj) * (tk VU.! jj) / dzp
                     - (fact VU.! jj) * dhsdT_
         else if j == 1 && snl_ < 0
         then -- Soil layer 1 with snow coupling
              let dzp = (z VU.! (jj + 1)) - (z VU.! jj)
                  dzm = (z VU.! jj) - (z VU.! (jj - 1))
                  tk_below = tk VU.! (jj - 1)
              in 1.0 + (1.0 - cnfac) * (fact VU.! jj)
                       * ((tk VU.! jj) / dzp + fse * tk_below / dzm)
                     - (1.0 - fse) * (fact VU.! jj) * dhsdT_
         else if j == nlevgrnd
         then -- Bottom layer
              let dzm = (z VU.! jj) - (z VU.! (jj - 1))
                  tk_below = tk VU.! (jj - 1)
              in 1.0 + (1.0 - cnfac) * (fact VU.! jj) * tk_below / dzm
         else -- Interior layers
              let dzp = (z VU.! (jj + 1)) - (z VU.! jj)
                  dzm = (z VU.! jj) - (z VU.! (jj - 1))
                  tk_below = tk VU.! (jj - 1)
              in 1.0 + (1.0 - cnfac) * (fact VU.! jj)
                       * ((tk VU.! jj) / dzp + tk_below / dzm)

    -- h2osfc correction to soil layer 1 diagonal
    bVec' = if fh2o /= 0.0
            then let j1k = k 1
                     jj1 = vix 1
                     dzm_h2o = 0.5 * dz_h2o + (z VU.! jj1)
                     corr = fh2o * ((1.0 - cnfac) * (fact VU.! jj1) * tk_h2o / dzm_h2o
                                   + (fact VU.! jj1) * dhsdT_)
                 in bVec VU.// [(j1k, (bVec VU.! j1k) + corr)]
            else bVec

    -- Build RHS vector
    rVec = VU.generate n $ \ki ->
      let j = ki + jtop
          jj = vix j
          t_j = t VU.! jj
      in if j == jtop
         then -- Top active layer
              t_j + (fact VU.! jj) * (hs_top_ - dhsdT_ * t_j + cnfac * (fn VU.! jj))
         else if j == 1 && snl_ < 0
         then -- Soil layer 1 with snow coupling
              let fn_j  = fn VU.! jj
                  fn_jm = fn VU.! (jj - 1)
                  sabg_j = if ki < VU.length sabg then sabg VU.! ki else 0.0
              in t_j + (fact VU.! jj) *
                   ((1.0 - fse) * (hs_soil_ - dhsdT_ * t_j)
                    + cnfac * (fn_j - fse * fn_jm))
                 + fse * (fact VU.! jj) * sabg_j
         else if j > 0 && j < nlevgrnd
         then -- Interior soil layers
              let fn_j  = fn VU.! jj
                  fn_jm = fn VU.! (jj - 1)
              in t_j + cnfac * (fact VU.! jj) * (fn_j - fn_jm)
         else if j == nlevgrnd
         then -- Bottom layer
              let fn_j  = fn VU.! jj
                  fn_jm = fn VU.! (jj - 1)
              in t_j - cnfac * (fact VU.! jj) * fn_jm + (fact VU.! jj) * fn_j
         else -- Snow interior (j < 0 && j > jtop)
              let fn_j  = fn VU.! jj
                  fn_jm = fn VU.! (jj - 1)
                  sabg_j = if ki < VU.length sabg then sabg VU.! ki else 0.0
              in t_j + cnfac * (fact VU.! jj) * (fn_j - fn_jm)
                     + (fact VU.! jj) * sabg_j

    -- h2osfc correction to soil layer 1 RHS
    fn_h2osfc_val =
      let jj1 = vix 1
          dzm_h2o = 0.5 * dz_h2o + (z VU.! jj1)
      in tk_h2o * ((t VU.! jj1) - t_h2o) / dzm_h2o

    rVec' = if fh2o /= 0.0
            then let j1k = k 1
                     jj1 = vix 1
                     corr = fh2o * (fact VU.! jj1)
                            * ((hs_soil_ - dhsdT_ * (t VU.! jj1))
                               + cnfac * fn_h2osfc_val)
                 in rVec VU.// [(j1k, (rVec VU.! j1k) - corr)]
            else rVec

    -- Solve
    tSoln = tridiagonalSolve aVec bVec' cVec rVec'

    -- Write solved temperatures back into full vector (NaN-safe)
    tNew = VU.generate (VU.length t) $ \jj ->
      let j = jj - joff
          ki_ = j - jtop
      in if j >= jtop && j <= jbot
         then let tv = tSoln VU.! ki_
              in if isNaN tv then t VU.! jj else tv
         else t VU.! jj

    -- Surface water temperature (single implicit equation)
    t_h2osfc_new =
      if fh2o == 0.0
      then tNew VU.! (joff + 1)  -- copy soil layer 1
      else let rhs_h2o = t_h2o + (dt / c_h2o)
                          * (hs_h2o - dhsdT_ * t_h2o + cnfac * fn_h2osfc_val)
               jj1     = vix 1
               dzm_h2o = 0.5 * dz_h2o + (z VU.! jj1)
               diag    = 1.0 + (1.0 - cnfac) * (dt / c_h2o) * tk_h2o / dzm_h2o
                             - (dt / c_h2o) * dhsdT_
           in rhs_h2o / diag

-- =========================================================================
-- Phase change
-- =========================================================================

data PhaseChangeInput = PhaseChangeInput
  { pci_snl          :: !Int
  , pci_t_soisno     :: !(VU.Vector Double)
  , pci_h2osoi_liq   :: !(VU.Vector Double)
  , pci_h2osoi_ice   :: !(VU.Vector Double)
  , pci_dz           :: !(VU.Vector Double)
  , pci_fact         :: !(VU.Vector Double)
  , pci_dhsdT        :: !Double
  , pci_frac_sno_eff :: !Double
  , pci_frac_h2osfc  :: !Double
  , pci_watsat       :: !(VU.Vector Double)  -- ^ Porosity (soil-only)
  , pci_bsw          :: !(VU.Vector Double)  -- ^ Clapp-Hornberger b (soil-only)
  , pci_sucsat       :: !(VU.Vector Double)  -- ^ Saturated suction [mm] (soil-only)
  , pci_h2osno_no_layers :: !Double
  , pci_snow_depth   :: !Double
  , pci_dtime        :: !Double
  } deriving (Show)

data PhaseChangeOutput = PhaseChangeOutput
  { pco_t_soisno     :: !(VU.Vector Double)
  , pco_h2osoi_liq   :: !(VU.Vector Double)
  , pco_h2osoi_ice   :: !(VU.Vector Double)
  , pco_imelt        :: !(VU.Vector Int)     -- ^ 0=none, 1=melt, 2=freeze
  , pco_xmf          :: !Double              -- ^ Total phase change energy [W/m2]
  , pco_qflx_snomelt :: !Double              -- ^ Snow melt rate [kg/m2/s]
  , pco_h2osno_no_layers :: !Double
  , pco_snow_depth   :: !Double
  } deriving (Show)

-- | Phase change within snow and soil layers.
-- Ported from phase_change_beta! (non-urban, non-excess-ice branch).
phaseChangeBeta :: PhaseChangeInput -> PhaseChangeOutput
phaseChangeBeta inp = PhaseChangeOutput
  { pco_t_soisno     = tFinal
  , pco_h2osoi_liq   = liqFinal
  , pco_h2osoi_ice   = iceFinal
  , pco_imelt        = imeltVec
  , pco_xmf          = xmfTotal
  , pco_qflx_snomelt = qflx_snomelt_total
  , pco_h2osno_no_layers = h2osno_nl_final
  , pco_snow_depth   = sd_final
  }
  where
    joff   = nlevsno - 1  -- 0-based: Fortran layer j → index j + joff
    snl_   = pci_snl inp
    t0     = pci_t_soisno inp
    liq0   = pci_h2osoi_liq inp
    ice0   = pci_h2osoi_ice inp
    dz_    = pci_dz inp
    factV  = pci_fact inp
    dhsdT_ = pci_dhsdT inp
    fse    = pci_frac_sno_eff inp
    fh2o   = pci_frac_h2osfc inp
    ws     = pci_watsat inp
    bsw_   = pci_bsw inp
    suc_   = pci_sucsat inp
    dt     = pci_dtime inp
    nlev   = VU.length t0

    -- Process each layer sequentially (bottom to top for snow, top to bottom for soil,
    -- but order doesn't matter as layers are independent in this formulation).
    -- We fold over all active layers, accumulating updated state.
    activeLayers = [j | j <- [snl_ + 1 .. nlevgrnd], True]

    initState = (t0, liq0, ice0,
                 VU.replicate nlev (0 :: Int),  -- imelt
                 0.0,  -- xmf
                 0.0,  -- qflx_snomelt
                 pci_h2osno_no_layers inp,
                 pci_snow_depth inp)

    (tFinal, liqFinal, iceFinal, imeltVec, xmfTotal,
     qflx_snomelt_total, h2osno_nl_final, sd_final) =
      foldl processLayer initState activeLayers

    processLayer (tV, liqV, iceV, imV, xmf_acc, qsm_acc, h2osno_nl, sd) j =
      let jj = j + joff
          t_j     = tV  VU.! jj
          liq_j   = liqV VU.! jj
          ice_j   = iceV VU.! jj
          fact_j  = factV VU.! jj
          dz_j    = dz_ VU.! jj

          -- Supercooled liquid (soil only)
          supercool
            | j >= 1 && t_j < tfrz =
                let smp_val = hfus * (tfrz - t_j) / (grav * t_j) * 1000.0
                    ws_j    = ws  VU.! (j - 1)
                    bsw_j   = bsw_ VU.! (j - 1)
                    suc_j   = suc_ VU.! (j - 1)
                in ws_j * (smp_val / suc_j) ** (-1.0 / bsw_j) * dz_j * 1000.0
            | otherwise = 0.0

          -- Melting identification
          doMelt  = ice_j > 0.0 && t_j > tfrz
          -- Freezing identification (snow: any liquid; soil: above supercooled)
          doFreeze | j <= 0    = liq_j > 0.0 && t_j < tfrz
                   | otherwise = liq_j > supercool && t_j < tfrz

          imelt_j | doMelt   = 1
                  | doFreeze = 2
                  | otherwise = 0

          tinc_j | imelt_j > 0 = tfrz - t_j
                 | otherwise   = 0.0

          t_clamped | imelt_j > 0 = tfrz
                    | otherwise   = t_j

          -- Energy surplus/loss
          hm_raw
            | imelt_j == 0 = 0.0
            | j == snl_ + 1 && j > 0 =
                dhsdT_ * tinc_j - tinc_j / fact_j
                - (if j == 1 && fh2o /= 0.0 then fh2o * dhsdT_ * tinc_j else 0.0)
            | j == snl_ + 1 && j <= 0 =
                fse * (dhsdT_ * tinc_j - tinc_j / fact_j)
            | j == 1 =
                (1.0 - fse - fh2o) * dhsdT_ * tinc_j - tinc_j / fact_j
            | j < 1 =
                -fse * tinc_j / fact_j
            | otherwise =
                -tinc_j / fact_j

          -- Guard: melt needs positive hm, freeze needs negative hm
          hm | imelt_j == 1 && hm_raw < 0.0 = 0.0
             | imelt_j == 2 && hm_raw > 0.0 = 0.0
             | otherwise                     = hm_raw

          imelt_final | imelt_j == 1 && hm_raw < 0.0 = 0
                      | imelt_j == 2 && hm_raw > 0.0 = 0
                      | otherwise                     = imelt_j

          -- Phase change mass
          xm | imelt_final > 0 && abs hm > 0.0 = hm * dt / hfus
             | otherwise                        = 0.0

          -- Unresolved snow melting at j==1
          (xm', hm', h2osno_nl', sd', qsm_layer)
            | j == 1 && h2osno_nl > 0.0 && xm > 0.0 =
                let temp1     = h2osno_nl
                    h2osno_new = max 0.0 (temp1 - xm)
                    propor     = h2osno_new / temp1
                    sd_new     = propor * sd
                    heatr      = hm - hfus * (temp1 - h2osno_new) / dt
                    qsm_lyr    = max 0.0 (temp1 - h2osno_new) / dt
                in if heatr > 0.0
                   then (heatr * dt / hfus, heatr, h2osno_new, sd_new, qsm_lyr)
                   else (0.0, 0.0, h2osno_new, sd_new, qsm_lyr)
            | otherwise = (xm, hm, h2osno_nl, sd, 0.0)

          -- Apply phase change to ice/liq
          wmass0_j = ice_j + liq_j
          (ice_new, liq_new, heatr2)
            | xm' > 0.0 =  -- melting
                let ice_n  = max 0.0 (ice_j - xm')
                    hr     = hm' - hfus * (ice_j - ice_n) / dt
                    liq_n  = max 0.0 (wmass0_j - ice_n)
                in (ice_n, liq_n, hr)
            | xm' < 0.0 =  -- freezing
                let ice_n  | j <= 0    = min wmass0_j (ice_j - xm')
                           | otherwise =
                               min (wmass0_j - supercool) (ice_j - xm')
                    hr = hm' - hfus * (ice_j - ice_n) / dt
                    liq_n = max 0.0 (wmass0_j - ice_n)
                in (ice_n, liq_n, hr)
            | otherwise = (ice_j, liq_j, 0.0)

          -- Temperature adjustment from residual heat
          t_adj
            | abs heatr2 > 0.0 =
                if j == snl_ + 1
                then if j == 1
                     then t_clamped + fact_j * heatr2
                          / (1.0 - (1.0 - fh2o) * fact_j * dhsdT_)
                     else if fse > 0.0
                          then t_clamped + (fact_j / fse) * heatr2
                               / (1.0 - fact_j * dhsdT_)
                          else t_clamped
                else if j == 1
                     then t_clamped + fact_j * heatr2
                          / (1.0 - (1.0 - fse - fh2o) * fact_j * dhsdT_)
                     else if j > 0
                          then t_clamped + fact_j * heatr2
                          else if fse > 0.0
                               then t_clamped + (fact_j / fse) * heatr2
                               else t_clamped
            | otherwise = t_clamped

          -- Snow layers: if both liq and ice present, clamp to tfrz
          t_final | j <= 0 && liq_new * ice_new > 0.0 = tfrz
                  | otherwise = t_adj

          -- Accumulate xmf
          xmf_layer = hfus * (ice_j - ice_new) / dt

          -- Snow melt accumulation
          qsm_snow | imelt_final == 1 && j < 1 = max 0.0 (ice_j - ice_new) / dt
                   | otherwise                  = 0.0

      in if imelt_final > 0 && abs hm > 0.0
         then ( tV   VU.// [(jj, t_final)]
              , liqV VU.// [(jj, liq_new)]
              , iceV VU.// [(jj, ice_new)]
              , imV  VU.// [(jj, imelt_final)]
              , xmf_acc + xmf_layer
              , qsm_acc + qsm_layer + qsm_snow
              , h2osno_nl'
              , sd'
              )
         else ( tV   VU.// [(jj, t_clamped)]
              , liqV
              , iceV
              , imV  VU.// [(jj, imelt_final)]
              , xmf_acc
              , qsm_acc
              , h2osno_nl'
              , sd'
              )

-- =========================================================================
-- Top-level driver
-- =========================================================================

-- | Full input for the soil temperature solver.
data SoilTempInput = SoilTempInput
  { sti_snl              :: !Int
  , sti_t_soisno         :: !(VU.Vector Double)
  , sti_t_grnd           :: !Double
  , sti_t_h2osfc         :: !Double
  , sti_h2osoi_liq       :: !(VU.Vector Double)
  , sti_h2osoi_ice       :: !(VU.Vector Double)
  , sti_dz               :: !(VU.Vector Double)
  , sti_z                :: !(VU.Vector Double)
  , sti_zi               :: !(VU.Vector Double)
  , sti_watsat           :: !(VU.Vector Double)
  , sti_bsw              :: !(VU.Vector Double)
  , sti_sucsat           :: !(VU.Vector Double)
  , sti_tkmg             :: !(VU.Vector Double)
  , sti_tkdry            :: !(VU.Vector Double)
  , sti_csol             :: !(VU.Vector Double)
  , sti_tksatu           :: !(VU.Vector Double)
  , sti_nbedrock         :: !Int
  , sti_h2osno_no_layers :: !Double
  , sti_h2osfc           :: !Double
  , sti_frac_sno_eff     :: !Double
  , sti_frac_h2osfc      :: !Double
  , sti_snow_depth       :: !Double
  , sti_hs_top           :: !Double  -- ^ Top-layer heat source [W/m2]
  , sti_dhsdT            :: !Double  -- ^ d(hs)/dT [W/m2/K]
  , sti_hs_soil          :: !Double  -- ^ Bare-soil heat source [W/m2]
  , sti_hs_h2osfc        :: !Double  -- ^ Surface water heat source [W/m2]
  , sti_sabg_lyr         :: !(VU.Vector Double)  -- ^ Absorbed solar per layer
  , sti_eflx_bot         :: !Double  -- ^ Bottom boundary heat flux [W/m2]
  , sti_dtime            :: !Double  -- ^ Timestep [s]
  , sti_snowCondMethod   :: !SnowThermalCond
  , sti_thk_override     :: !(Maybe (VU.Vector Double))
      -- ^ Optional injected per-layer thermal conductivity (see
      -- 'tpi_thk_override'). Nothing ⇒ default conductivity computation.
  , sti_cv_override      :: !(Maybe (VU.Vector Double))
      -- ^ Optional injected per-layer volumetric heat capacity (see
      -- 'tpi_cv_override'). Nothing ⇒ default heat-capacity computation.
  } deriving (Show)

-- | Output of the soil temperature solver.
data SoilTempOutput = SoilTempOutput
  { sto_t_soisno     :: !(VU.Vector Double)
  , sto_t_grnd       :: !Double
  , sto_t_h2osfc     :: !Double
  , sto_h2osoi_liq   :: !(VU.Vector Double)
  , sto_h2osoi_ice   :: !(VU.Vector Double)
  , sto_imelt        :: !(VU.Vector Int)
  , sto_xmf          :: !Double
  , sto_qflx_snomelt :: !Double
  , sto_h2osno_no_layers :: !Double
  , sto_snow_depth   :: !Double
  , sto_fact         :: !(VU.Vector Double)
  , sto_fn           :: !(VU.Vector Double)
  } deriving (Show)

-- | Main soil temperature solver for a single non-urban, non-lake column.
-- Steps:
--   1. Compute thermal properties (tk, cv)
--   2. Compute heat diffusion fluxes (fn, fact)
--   3. Compute surface water thermal properties
--   4. Build and solve tridiagonal system
--   5. Phase change
--   6. Update ground temperature
solveSoilTemperature :: SoilTempInput -> SoilTempOutput
solveSoilTemperature inp = SoilTempOutput
  { sto_t_soisno     = pco_t_soisno pcOut
  , sto_t_grnd       = t_grnd_new
  , sto_t_h2osfc     = t_h2osfc_out
  , sto_h2osoi_liq   = pco_h2osoi_liq pcOut
  , sto_h2osoi_ice   = pco_h2osoi_ice pcOut
  , sto_imelt        = pco_imelt pcOut
  , sto_xmf          = pco_xmf pcOut
  , sto_qflx_snomelt = pco_qflx_snomelt pcOut
  , sto_h2osno_no_layers = pco_h2osno_no_layers pcOut
  , sto_snow_depth   = pco_snow_depth pcOut
  , sto_fact         = hdo_fact hdOut
  , sto_fn           = hdo_fn hdOut
  }
  where
    joff = nlevsno - 1  -- 0-based: Fortran layer j → index j + joff
    snl_ = sti_snl inp
    dt   = sti_dtime inp
    fse  = sti_frac_sno_eff inp
    fh2o = sti_frac_h2osfc inp

    -- Step 1: Thermal properties
    tpInp = ThermPropInput
      { tpi_snl              = snl_
      , tpi_t_soisno         = sti_t_soisno inp
      , tpi_h2osoi_liq       = sti_h2osoi_liq inp
      , tpi_h2osoi_ice       = sti_h2osoi_ice inp
      , tpi_dz               = sti_dz inp
      , tpi_z                = sti_z inp
      , tpi_zi               = sti_zi inp
      , tpi_watsat           = sti_watsat inp
      , tpi_tkmg             = sti_tkmg inp
      , tpi_tkdry            = sti_tkdry inp
      , tpi_csol             = sti_csol inp
      , tpi_tksatu           = sti_tksatu inp
      , tpi_nbedrock         = sti_nbedrock inp
      , tpi_h2osno_no_layers = sti_h2osno_no_layers inp
      , tpi_frac_sno         = fse
      , tpi_h2osfc           = sti_h2osfc inp
      , tpi_snowCondMethod   = sti_snowCondMethod inp
      , tpi_thk_override     = sti_thk_override inp
      , tpi_cv_override      = sti_cv_override inp
      }
    tpOut = soilThermProp tpInp

    -- Step 2: Heat diffusion
    hdInp = HeatDiffInput
      { hdi_snl      = snl_
      , hdi_t_soisno = sti_t_soisno inp
      , hdi_z        = sti_z inp
      , hdi_zi       = sti_zi inp
      , hdi_dz       = sti_dz inp
      , hdi_tk       = tpo_tk tpOut
      , hdi_cv       = tpo_cv tpOut
      , hdi_eflx_bot = sti_eflx_bot inp
      , hdi_dtime    = dt
      }
    hdOut = computeHeatDiffFlux hdInp

    -- Step 3: Surface water thermal properties
    h2osfc_   = sti_h2osfc inp
    c_h2osfc_val
      | h2osfc_ > thinSfcLayer && fh2o > thinSfcLayer =
          max thinSfcLayer (cpliq * h2osfc_ / fh2o)
      | otherwise = thinSfcLayer

    dz_h2osfc_val
      | h2osfc_ > thinSfcLayer && fh2o > thinSfcLayer =
          max thinSfcLayer (1.0e-3 * h2osfc_ / fh2o)
      | otherwise = thinSfcLayer

    -- Step 4: Build and solve tridiagonal
    triInp = TridiagInput
      { tri_snl          = snl_
      , tri_t_soisno     = sti_t_soisno inp
      , tri_z            = sti_z inp
      , tri_tk           = tpo_tk tpOut
      , tri_fn           = hdo_fn hdOut
      , tri_fact         = hdo_fact hdOut
      , tri_cv           = tpo_cv tpOut
      , tri_hs_top       = sti_hs_top inp
      , tri_dhsdT        = sti_dhsdT inp
      , tri_hs_soil      = sti_hs_soil inp
      , tri_sabg_lyr     = sti_sabg_lyr inp
      , tri_frac_sno_eff = fse
      , tri_frac_h2osfc  = fh2o
      , tri_hs_h2osfc    = sti_hs_h2osfc inp
      , tri_tk_h2osfc    = tpo_tk_h2osfc tpOut
      , tri_c_h2osfc     = c_h2osfc_val
      , tri_dz_h2osfc    = dz_h2osfc_val
      , tri_t_h2osfc     = sti_t_h2osfc inp
      , tri_dtime        = dt
      }
    (tSolved, t_h2osfc_solved) = buildTridiagSystem triInp

    -- Step 5: Phase change
    pcInp = PhaseChangeInput
      { pci_snl              = snl_
      , pci_t_soisno         = tSolved
      , pci_h2osoi_liq       = sti_h2osoi_liq inp
      , pci_h2osoi_ice       = sti_h2osoi_ice inp
      , pci_dz               = sti_dz inp
      , pci_fact             = hdo_fact hdOut
      , pci_dhsdT            = sti_dhsdT inp
      , pci_frac_sno_eff     = fse
      , pci_frac_h2osfc      = fh2o
      , pci_watsat           = sti_watsat inp
      , pci_bsw              = sti_bsw inp
      , pci_sucsat           = sti_sucsat inp
      , pci_h2osno_no_layers = sti_h2osno_no_layers inp
      , pci_snow_depth       = sti_snow_depth inp
      , pci_dtime            = dt
      }
    pcOut = phaseChangeBeta pcInp

    -- Use post-phase-change t_h2osfc
    t_h2osfc_out
      | fh2o == 0.0 = (pco_t_soisno pcOut) VU.! (joff + 1)  -- soil layer 1
      | otherwise    = t_h2osfc_solved

    -- Step 6: Ground temperature
    t_pc = pco_t_soisno pcOut
    t_grnd_new
      | snl_ < 0 =
          if fh2o /= 0.0
          then fse * (t_pc VU.! (snl_ + 1 + joff))
               + (1.0 - fse - fh2o) * (t_pc VU.! (joff + 1))
               + fh2o * t_h2osfc_out
          else fse * (t_pc VU.! (snl_ + 1 + joff))
               + (1.0 - fse) * (t_pc VU.! (joff + 1))
      | otherwise =
          if fh2o /= 0.0
          then (1.0 - fh2o) * (t_pc VU.! (joff + 1))
               + fh2o * t_h2osfc_out
          else t_pc VU.! (joff + 1)
