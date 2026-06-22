{-# LANGUAGE BangPatterns #-}
-- | Lake temperature evolution.
-- Fortran: LakeTemperatureMod.F90
-- Julia:   src/biogeophys/lake_temperature.jl
--
-- Calculates temperatures in the column of (possible) snow, lake water,
-- soil, and bedrock beneath a lake. Uses a Crank-Nicholson tridiagonal
-- system, then applies phase change and convective mixing.
--
-- All functions are pure.
--
module CLM.BioGeoPhys.LakeTemperature
  ( -- * Thermal properties
    soilThermPropLake
  , ThermPropLakeInput(..)
  , ThermPropLakeOutput(..)
    -- * Phase change
  , phaseChangeLake
  , PhaseChangeLakeInput(..)
  , PhaseChangeLakeOutput(..)
    -- * Lake density
  , lakeDensity
    -- * Diffusivity and thermal conductivity
  , lakeDiffusivity
  , LakeDiffInput(..)
  , LakeDiffOutput(..)
    -- * Solar heat source
  , lakeSolarHeatSource
  , LakeSolarInput(..)
  , LakeSolarOutput(..)
    -- * Extended column tridiagonal solve
  , lakeTridiagSolve
  , LakeTridiagInput(..)
  , LakeTridiagOutput(..)
    -- * Convective mixing
  , lakeConvectiveMix
  , LakeConvMixInput(..)
  , LakeConvMixOutput(..)
    -- * Constants
  , cnfacLake
  , betavisLT
  , za_lakeLT
  , n2minLT
  , tdmaxLT
  , depthcritLT
  , mixfactLT
  , thk_bedrock
  , csol_bedrock
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants
  ( tfrz, grav, denh2o, denice, cpice, cpliq, hfus
  , tkwat, tkice, tkair
  , nlevsno, nlevgrnd, nlevsoi
  )

-- =========================================================================
-- Constants (from LakeCon.F90 / LakeTemperatureMod.F90)
-- =========================================================================

-- | Crank-Nicholson factor
cnfacLake :: Double
cnfacLake = 0.5

-- | Fraction of visible sunlight absorbed at surface
betavisLT :: Double
betavisLT = 0.0

-- | Base of surface absorption layer [m]
za_lakeLT :: Double
za_lakeLT = 0.6

-- | Min Brunt-Vaisala frequency squared [s^-2]
n2minLT :: Double
n2minLT = 7.5e-5

-- | Temperature of maximum water density [K]
tdmaxLT :: Double
tdmaxLT = 277.0

-- | Depth beneath which to increase mixing [m]
depthcritLT :: Double
depthcritLT = 25.0

-- | Mixing increase factor for deep lakes
mixfactLT :: Double
mixfactLT = 10.0

-- | Thermal conductivity of bedrock [W/m/K]
thk_bedrock :: Double
thk_bedrock = 3.0

-- | Volumetric heat capacity of bedrock [J/m3/K]
csol_bedrock :: Double
csol_bedrock = 2.0e6

-- | Von Karman constant
vkcLT :: Double
vkcLT = 0.4

-- =========================================================================
-- Thermal properties for snow/soil under lake
-- =========================================================================

-- | Input for lake snow/soil thermal property computation.
data ThermPropLakeInput = ThermPropLakeInput
  { tpli_snl         :: !Int
  , tpli_t_soisno    :: !(VU.Vector Double)
  , tpli_h2osoi_liq  :: !(VU.Vector Double)
  , tpli_h2osoi_ice  :: !(VU.Vector Double)
  , tpli_dz          :: !(VU.Vector Double)
  , tpli_z           :: !(VU.Vector Double)
  , tpli_zi          :: !(VU.Vector Double)
  , tpli_watsat      :: !(VU.Vector Double)  -- ^ Porosity (soil-only)
  , tpli_tksatu      :: !(VU.Vector Double)  -- ^ Saturated thermal cond (soil-only)
  , tpli_tkmg        :: !(VU.Vector Double)  -- ^ Mineral thermal cond (soil-only)
  , tpli_tkdry       :: !(VU.Vector Double)  -- ^ Dry thermal cond (soil-only)
  , tpli_csol        :: !(VU.Vector Double)  -- ^ Dry vol heat cap (soil-only)
  } deriving (Show)

-- | Output of lake thermal property computation.
data ThermPropLakeOutput = ThermPropLakeOutput
  { tplo_tk            :: !(VU.Vector Double)  -- ^ Interface thermal conductivity
  , tplo_cv            :: !(VU.Vector Double)  -- ^ Heat capacity
  , tplo_tktopsoillay  :: !Double              -- ^ Top soil layer thermal conductivity
  } deriving (Show)

-- | Compute thermal conductivities and heat capacities for snow/soil
-- layers beneath a lake.
-- Ported from SoilThermProp_Lake in LakeTemperatureMod.F90.
soilThermPropLake :: ThermPropLakeInput -> ThermPropLakeOutput
soilThermPropLake inp = ThermPropLakeOutput
  { tplo_tk           = tkInterf
  , tplo_cv           = cvOut
  , tplo_tktopsoillay = tktopsoillay_val
  }
  where
    !snl_  = tpli_snl inp
    !t     = tpli_t_soisno inp
    !liq   = tpli_h2osoi_liq inp
    !ice   = tpli_h2osoi_ice inp
    !dz_   = tpli_dz inp
    !z_    = tpli_z inp
    !zi_   = tpli_zi inp
    !ws    = tpli_watsat inp
    !tksatu_= tpli_tksatu inp
    !tkmg_ = tpli_tkmg inp
    !tkdry_= tpli_tkdry inp
    !csol_ = tpli_csol inp

    -- Port combined arrays place Fortran soil layer L (1-based) at 0-based
    -- index L + (nlevsno-1): top soil (L=1) at nlevsno, bottom snow (L=0) at
    -- nlevsno-1. So j = jj - (nlevsno-1) recovers the Fortran layer.
    joff = nlevsno - 1
    nlev = nlevsno + nlevgrnd

    -- Layer-centre thermal conductivity
    thkOut = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= 1 && j <= nlevsoi
         then soilThk j jj
         else if j > nlevsoi
              then thk_bedrock
              else if snl_ + 1 < 1 && j >= snl_ + 1 && j <= 0
                   then snowThk jj
                   else 0.0

    soilThk j jj =
      let ws_j    = ws VU.! (j - 1)
          tksatu_j = tksatu_ VU.! (j - 1)
          tkmg_j  = tkmg_ VU.! (j - 1)
          tkdry_j = tkdry_ VU.! (j - 1)
          t_j     = t VU.! jj
          liq_j   = liq VU.! jj
          ice_j   = ice VU.! jj
          dz_j    = dz_ VU.! jj
          -- Soil should be saturated in LakeHydrology
          satw    = 1.0
          fl      = liq_j / (ice_j + liq_j)
          (dke, dksat)
            | t_j >= tfrz = (max 0.0 (logBase 10 satw + 1.0), tksatu_j)
            | otherwise   = (satw, tkmg_j * 0.249 ** (fl * ws_j) * 2.29 ** ws_j)
          thk_val = dke * dksat + (1.0 - dke) * tkdry_j
          -- Check for over-saturation
          satw_actual = (liq_j / denh2o + ice_j / denice) / (dz_j * ws_j)
          xicevol     = (satw_actual - 1.0) * ws_j
          thk_adj | satw_actual > 1.0 = (thk_val + xicevol * tkice) / (1.0 + xicevol) / (1.0 + xicevol)
                  | otherwise         = thk_val
      in thk_adj

    snowThk jj =
      let bw = (ice VU.! jj + liq VU.! jj) / (dz_ VU.! jj)
      in tkair + (7.75e-5 * bw + 1.105e-6 * bw * bw) * (tkice - tkair)

    -- Interface thermal conductivity
    tkInterf = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= snl_ + 1 && j <= nlevgrnd - 1 && j /= 0
         then let jj1 = jj + 1
                  dzspan = (z_ VU.! jj1) - (z_ VU.! jj)
                  zi_j1  = zi_ VU.! (jj + 1)
              in (thkOut VU.! jj) * (thkOut VU.! jj1) * dzspan
                 / ((thkOut VU.! jj) * ((z_ VU.! jj1) - zi_j1) + (thkOut VU.! jj1) * (zi_j1 - (z_ VU.! jj)))
         else if j == 0 && j >= snl_ + 1
              then thkOut VU.! jj
              else 0.0

    -- Top soil layer conductivity (port index nlevsno = top soil layer)
    tktopsoillay_val = thkOut VU.! nlevsno

    -- Heat capacity
    cvOut = VU.generate nlev $ \jj ->
      let j = jj - joff
      in if j >= 1
         then soilCv j jj
         else if snl_ + 1 < 1 && j >= snl_ + 1
              then snowCv jj
              else 0.0

    soilCv j jj =
      let ws_j   = ws VU.! (j - 1)
          csol_j = csol_ VU.! (j - 1)
          dz_j   = dz_ VU.! jj
          liq_j  = liq VU.! jj
          ice_j  = ice VU.! jj
      in if j > nlevsoi
         then csol_bedrock * dz_j
         else csol_j * (1.0 - ws_j) * dz_j + ice_j * cpice + liq_j * cpliq

    snowCv jj = cpliq * (liq VU.! jj) + cpice * (ice VU.! jj)

-- =========================================================================
-- Phase change for lake layers
-- =========================================================================

-- | Input for lake phase change.
data PhaseChangeLakeInput = PhaseChangeLakeInput
  { pcli_snl              :: !Int
  , pcli_t_soisno         :: !(VU.Vector Double)
  , pcli_h2osoi_liq       :: !(VU.Vector Double)
  , pcli_h2osoi_ice       :: !(VU.Vector Double)
  , pcli_cv               :: !(VU.Vector Double)  -- ^ Snow/soil heat capacity
  , pcli_t_lake           :: !(VU.Vector Double)  -- ^ Lake layer temperatures
  , pcli_lake_icefrac     :: !(VU.Vector Double)  -- ^ Lake ice fraction
  , pcli_cv_lake          :: !(VU.Vector Double)  -- ^ Lake layer heat capacity
  , pcli_dz_lake          :: !(VU.Vector Double)  -- ^ Lake layer thickness
  , pcli_h2osno_no_layers :: !Double
  , pcli_snow_depth       :: !Double
  , pcli_nlevlak          :: !Int
  , pcli_dtime            :: !Double
  } deriving (Show)

-- | Output of lake phase change.
data PhaseChangeLakeOutput = PhaseChangeLakeOutput
  { pclo_t_soisno         :: !(VU.Vector Double)
  , pclo_h2osoi_liq       :: !(VU.Vector Double)
  , pclo_h2osoi_ice       :: !(VU.Vector Double)
  , pclo_cv               :: !(VU.Vector Double)
  , pclo_t_lake           :: !(VU.Vector Double)
  , pclo_lake_icefrac     :: !(VU.Vector Double)
  , pclo_cv_lake          :: !(VU.Vector Double)
  , pclo_h2osno_no_layers :: !Double
  , pclo_snow_depth       :: !Double
  , pclo_qflx_snomelt     :: !Double
  , pclo_qflx_snofrz      :: !Double
  , pclo_eflx_snomelt     :: !Double
  , pclo_lhabs            :: !Double
  , pclo_qflx_snow_drain  :: !Double
  , pclo_imelt            :: !(VU.Vector Int)
  } deriving (Show)

-- | Phase change within snow, soil, and lake layers.
-- Ported from PhaseChange_Lake in LakeTemperatureMod.F90.
phaseChangeLake :: PhaseChangeLakeInput -> PhaseChangeLakeOutput
phaseChangeLake inp = PhaseChangeLakeOutput
  { pclo_t_soisno         = t_soisno_fin
  , pclo_h2osoi_liq       = liq_fin
  , pclo_h2osoi_ice       = ice_fin
  , pclo_cv               = cv_fin
  , pclo_t_lake           = t_lake_fin
  , pclo_lake_icefrac     = icefrac_fin
  , pclo_cv_lake          = cv_lake_fin
  , pclo_h2osno_no_layers = h2osno_nl_fin
  , pclo_snow_depth       = sd_fin
  , pclo_qflx_snomelt     = qflx_snomelt_tot
  , pclo_qflx_snofrz      = qflx_snofrz_tot
  , pclo_eflx_snomelt     = qflx_snomelt_tot * hfus
  , pclo_lhabs            = lhabs_tot
  , pclo_qflx_snow_drain  = qflx_sd_tot
  , pclo_imelt            = imelt_fin
  }
  where
    !snl_    = pcli_snl inp
    !dt      = pcli_dtime inp
    !nlevlak = pcli_nlevlak inp
    smallnumber = 1.0e-12

    -- Step 1: Check for snow without layers + warm top lake
    (t_lake_s1, h2osno_nl_s1, sd_s1, lhabs_s1, qflx_sm_s1, qflx_sd_s1) =
      let t1     = pcli_t_lake inp VU.! 0
          h2osno = pcli_h2osno_no_layers inp
          sd0    = pcli_snow_depth inp
          cv1    = pcli_cv_lake inp VU.! 0
      in if h2osno > 0.0 && t1 > tfrz
         then let heatavail = (t1 - tfrz) * cv1
                  melt_val  = min h2osno (heatavail / hfus)
                  heatrem   = max (heatavail - melt_val * hfus) 0.0
                  t1_new    = tfrz + heatrem / cv1
                  sd_new    = sd0 * (1.0 - melt_val / h2osno)
                  h2osno_new = h2osno - melt_val
                  h2osno_clamped = if h2osno_new < smallnumber then 0.0 else h2osno_new
                  sd_clamped     = if sd_new < smallnumber then 0.0 else sd_new
              in ( pcli_t_lake inp VU.// [(0, t1_new)]
                 , h2osno_clamped, sd_clamped
                 , melt_val * hfus
                 , melt_val / dt
                 , melt_val / dt
                 )
         else (pcli_t_lake inp, h2osno, sd0, 0.0, 0.0, 0.0)

    -- Step 2: Lake layer phase change
    (t_lake_s2, icefrac_s2, cv_lake_s2, lhabs_s2) =
      foldl lakePCStep (t_lake_s1, pcli_lake_icefrac inp, pcli_cv_lake inp, lhabs_s1)
            [0 .. nlevlak - 1]

    lakePCStep (!tl, !ifr, !cvl, !lh) j =
      let t_j    = tl VU.! j
          if_j   = ifr VU.! j
          cv_j   = cvl VU.! j
          dz_j   = pcli_dz_lake inp VU.! j
          doMelt   = t_j > tfrz && if_j > 0.0
          doFreeze = t_j < tfrz && if_j < 1.0
      in if doMelt
         then let heatavail = (t_j - tfrz) * cv_j
                  melt_val  = min (if_j * denh2o * dz_j) (heatavail / hfus)
                  heatrem   = max (heatavail - melt_val * hfus) 0.0
                  if_new0   = if_j - melt_val / (denh2o * dz_j)
                  if_new    = if if_new0 < smallnumber then 0.0 else if_new0
                  cv_new    = cv_j + melt_val * (cpliq - cpice)
                  t_new     = tfrz + heatrem / cv_new
              in ( tl VU.// [(j, t_new)]
                 , ifr VU.// [(j, if_new)]
                 , cvl VU.// [(j, cv_new)]
                 , lh + melt_val * hfus
                 )
         else if doFreeze
         then let heatavail = (t_j - tfrz) * cv_j
                  melt_val  = max (negate (1.0 - if_j) * denh2o * dz_j) (heatavail / hfus)
                  heatrem   = min (heatavail - melt_val * hfus) 0.0
                  if_new0   = if_j - melt_val / (denh2o * dz_j)
                  if_new    | if_new0 > 1.0 - smallnumber = 1.0
                            | if_new0 < smallnumber       = 0.0
                            | otherwise                   = if_new0
                  cv_new    = cv_j + melt_val * (cpliq - cpice)
                  t_new     = tfrz + heatrem / cv_new
              in ( tl VU.// [(j, t_new)]
                 , ifr VU.// [(j, if_new)]
                 , cvl VU.// [(j, cv_new)]
                 , lh + melt_val * hfus
                 )
         else (tl, ifr, cvl, lh)

    -- Step 3: Snow & soil phase change
    nlev = nlevsno + nlevgrnd
    imelt_init = VU.replicate nlev (0 :: Int)

    (t_soisno_fin, liq_fin, ice_fin, cv_fin, imelt_fin,
     lhabs_tot, qflx_snomelt_tot, qflx_snofrz_tot) =
      foldl soisnoPCStep
            ( pcli_t_soisno inp, pcli_h2osoi_liq inp, pcli_h2osoi_ice inp
            , pcli_cv inp, imelt_init
            , lhabs_s2, qflx_sm_s1, 0.0
            )
            [snl_ + 1 .. nlevgrnd]

    soisnoPCStep (!tV, !liqV, !iceV, !cvV, !imV, !lh, !qsm, !qsf) j =
      let jj = j + nlevsno - 1   -- Fortran layer j -> port combined index
          t_j   = tV VU.! jj
          liq_j = liqV VU.! jj
          ice_j = iceV VU.! jj
          cv_j  = cvV VU.! jj
          doMelt   = t_j > tfrz && ice_j > 0.0
          doFreeze = t_j < tfrz && liq_j > 0.0
      in if doMelt
         then let heatavail = (t_j - tfrz) * cv_j
                  melt_val  = min ice_j (heatavail / hfus)
                  heatrem   = max (heatavail - melt_val * hfus) 0.0
                  ice_new0  = ice_j - melt_val
                  liq_new0  = liq_j + melt_val
                  ice_new   = if ice_new0 < smallnumber then 0.0 else ice_new0
                  liq_new   = if liq_new0 < smallnumber then 0.0 else liq_new0
                  cv_new    = cv_j + melt_val * (cpliq - cpice)
                  t_new     = tfrz + heatrem / cv_new
                  im_val    = if j <= 0 then 1 else 0
                  qsm'      = if j <= 0 then qsm + melt_val / dt else qsm
              in ( tV VU.// [(jj, t_new)]
                 , liqV VU.// [(jj, liq_new)]
                 , iceV VU.// [(jj, ice_new)]
                 , cvV VU.// [(jj, cv_new)]
                 , imV VU.// [(jj, im_val)]
                 , lh + melt_val * hfus
                 , qsm', qsf
                 )
         else if doFreeze
         then let heatavail = (t_j - tfrz) * cv_j
                  melt_val  = max (negate liq_j) (heatavail / hfus)
                  heatrem   = min (heatavail - melt_val * hfus) 0.0
                  ice_new0  = ice_j - melt_val
                  liq_new0  = liq_j + melt_val
                  ice_new   = if ice_new0 < smallnumber then 0.0 else ice_new0
                  liq_new   = if liq_new0 < smallnumber then 0.0 else liq_new0
                  cv_new    = cv_j + melt_val * (cpliq - cpice)
                  t_new     = tfrz + heatrem / cv_new
                  im_val    = if j <= 0 then 2 else 0
                  qsf'      = if j <= 0 then qsf + (negate melt_val) / dt else qsf
              in ( tV VU.// [(jj, t_new)]
                 , liqV VU.// [(jj, liq_new)]
                 , iceV VU.// [(jj, ice_new)]
                 , cvV VU.// [(jj, cv_new)]
                 , imV VU.// [(jj, im_val)]
                 , lh + melt_val * hfus
                 , qsm, qsf'
                 )
         else (tV, liqV, iceV, cvV, imV, lh, qsm, qsf)

    t_lake_fin   = t_lake_s2
    icefrac_fin  = icefrac_s2
    cv_lake_fin  = cv_lake_s2
    h2osno_nl_fin = h2osno_nl_s1
    sd_fin       = sd_s1
    qflx_sd_tot  = qflx_sd_s1

-- =========================================================================
-- Lake density
-- =========================================================================

-- | Compute lake water density per layer [kg/m3].
-- Ported from lake_temperature! density calculation.
lakeDensity
  :: VU.Vector Double  -- ^ t_lake
  -> VU.Vector Double  -- ^ lake_icefrac
  -> VU.Vector Double  -- ^ density per layer [kg/m3]
lakeDensity t_lake icefrac = VU.zipWith dens t_lake icefrac
  where
    dens t_j if_j =
      (1.0 - if_j) * 1000.0 * (1.0 - 1.9549e-05 * abs (t_j - tdmaxLT) ** 1.68)
      + if_j * denice

-- =========================================================================
-- Diffusivity and thermal conductivity
-- =========================================================================

-- | Input for lake diffusivity computation.
data LakeDiffInput = LakeDiffInput
  { ldi_t_grnd        :: !Double
  , ldi_t_lake        :: !(VU.Vector Double)
  , ldi_lake_icefrac  :: !(VU.Vector Double)
  , ldi_z_lake        :: !(VU.Vector Double)
  , ldi_dz_lake       :: !(VU.Vector Double)
  , ldi_snl           :: !Int
  , ldi_lakedepth     :: !Double
  , ldi_ws            :: !Double  -- ^ Wind-driven surface stress
  , ldi_ks            :: !Double  -- ^ Surface extinction coefficient
  , ldi_rhow          :: !(VU.Vector Double)  -- ^ Layer densities
  , ldi_nlevlak       :: !Int
  } deriving (Show)

-- | Output of lake diffusivity computation.
data LakeDiffOutput = LakeDiffOutput
  { ldo_tk_lake    :: !(VU.Vector Double)  -- ^ Lake thermal conductivity
  , ldo_kme        :: !(VU.Vector Double)  -- ^ Eddy diffusivity
  , ldo_savedtke1  :: !Double              -- ^ Saved TKE for top layer
  } deriving (Show)

-- | Compute lake eddy diffusivity and thermal conductivity.
-- Ported from lake_temperature! sections 3) and bottom layer.
lakeDiffusivity :: LakeDiffInput -> LakeDiffOutput
lakeDiffusivity inp = LakeDiffOutput
  { ldo_tk_lake   = tk_lake_final
  , ldo_kme       = kme_final
  , ldo_savedtke1 = (kme_final VU.! 0) * cwat
  }
  where
    !nlevlak  = ldi_nlevlak inp
    !t_grnd   = ldi_t_grnd inp
    !snl_     = ldi_snl inp
    !ws_val   = ldi_ws inp
    !ks_val   = ldi_ks inp
    !lakedepth_val = ldi_lakedepth inp
    t_lake    = ldi_t_lake inp
    icefrac   = ldi_lake_icefrac inp
    z_lake    = ldi_z_lake inp
    dz_lake   = ldi_dz_lake inp
    rhow      = ldi_rhow inp

    cwat      = cpliq * denh2o
    tkice_eff = tkice * denice / denh2o
    km        = tkwat / cwat
    p0        = 1.0

    unfrozen j = t_grnd > tfrz && (t_lake VU.! 0) > tfrz && snl_ == 0
              && (icefrac VU.! j) == 0.0

    -- Interior layers (0..nlevlak-2)
    kme_interior = VU.generate nlevlak $ \j ->
      if j < nlevlak - 1
      then let drhodz = ((rhow VU.! (j + 1)) - (rhow VU.! j)) / ((z_lake VU.! (j + 1)) - (z_lake VU.! j))
               n2     = grav / (rhow VU.! j) * drhodz
               num    = 40.0 * n2 * (vkcLT * (z_lake VU.! j)) ** 2.0
               den    = max (ws_val ** 2.0 * exp (-2.0 * ks_val * (z_lake VU.! j))) 1.0e-10
               ri     = (-1.0 + sqrt (max (1.0 + num / den) 0.0)) / 20.0
           in if unfrozen j
              then let ke = vkcLT * ws_val * (z_lake VU.! j) / p0
                            * exp (-ks_val * (z_lake VU.! j)) / (1.0 + 37.0 * ri * ri)
                       kme0 = km + ke
                       fangkm = 1.039e-8 * max n2 n2minLT ** (-0.43)
                       kme1 = kme0 + fangkm
                   in if lakedepth_val >= depthcritLT then kme1 * mixfactLT else kme1
              else let fangkm = 1.039e-8 * max n2 n2minLT ** (-0.43)
                       kme0 = km + fangkm
                   in if lakedepth_val >= depthcritLT then kme0 * mixfactLT else kme0
      else 0.0  -- placeholder for bottom, set below

    -- Bottom layer copies from layer above
    kme_final = kme_interior VU.// [(nlevlak - 1, kme_interior VU.! (nlevlak - 2))]

    -- Thermal conductivity
    tk_lake_final = VU.generate nlevlak $ \j ->
      if unfrozen j
      then (kme_final VU.! j) * cwat
      else let kme_j = kme_final VU.! j
               if_j  = icefrac VU.! j
           in kme_j * cwat * tkice_eff
              / ((1.0 - if_j) * tkice_eff + kme_j * cwat * if_j)

-- =========================================================================
-- Solar heat source
-- =========================================================================

-- | Input for lake solar heat source.
data LakeSolarInput = LakeSolarInput
  { lsi_sabg      :: !Double
  , lsi_beta      :: !Double  -- ^ NIR fraction at surface
  , lsi_etal      :: !Double  -- ^ Light extinction coefficient
  , lsi_lakedepth :: !Double
  , lsi_t_grnd    :: !Double
  , lsi_t_lake1   :: !Double
  , lsi_snl       :: !Int
  , lsi_z_lake    :: !(VU.Vector Double)
  , lsi_dz_lake   :: !(VU.Vector Double)
  , lsi_nlevlak   :: !Int
  } deriving (Show)

-- | Output of lake solar heat source.
data LakeSolarOutput = LakeSolarOutput
  { lso_phi      :: !(VU.Vector Double)  -- ^ Solar heat per lake layer [W/m2]
  , lso_phi_soil :: !Double              -- ^ Solar reaching soil beneath lake [W/m2]
  } deriving (Show)

-- | Compute solar heat source per lake layer.
-- Ported from lake_temperature! section 4).
lakeSolarHeatSource :: LakeSolarInput -> LakeSolarOutput
lakeSolarHeatSource inp = LakeSolarOutput
  { lso_phi      = phiV
  , lso_phi_soil = phi_soil_val
  }
  where
    !nlevlak  = lsi_nlevlak inp
    !sabg     = lsi_sabg inp
    !beta_val = lsi_beta inp
    !etal0    = lsi_etal inp
    !lakedepth_val = lsi_lakedepth inp
    !t_grnd   = lsi_t_grnd inp
    !t_lake1  = lsi_t_lake1 inp
    !snl_     = lsi_snl inp
    z_lake    = lsi_z_lake inp
    dz_lake   = lsi_dz_lake inp

    eta | etal0 > 0.0 = etal0
        | otherwise   = 1.1925 * max lakedepth_val 1.0 ** (-0.424)

    unfrozenNoSnow = t_grnd > tfrz && t_lake1 > tfrz && snl_ == 0

    phiV = VU.generate nlevlak $ \j ->
      if unfrozenNoSnow
      then let zin  = (z_lake VU.! j) - 0.5 * (dz_lake VU.! j)
               zout = (z_lake VU.! j) + 0.5 * (dz_lake VU.! j)
               rsfin  = exp (-eta * max (zin - za_lakeLT) 0.0)
               rsfout = exp (-eta * max (zout - za_lakeLT) 0.0)
           in (rsfin - rsfout) * sabg * (1.0 - beta_val)
      else if j == 0 && snl_ == 0
           then sabg * (1.0 - beta_val)
           else 0.0

    phi_soil_val
      | unfrozenNoSnow =
          let zout = (z_lake VU.! (nlevlak - 1)) + 0.5 * (dz_lake VU.! (nlevlak - 1))
              rsfout = exp (-eta * max (zout - za_lakeLT) 0.0)
          in rsfout * sabg * (1.0 - beta_val)
      | otherwise = 0.0

-- =========================================================================
-- Extended column tridiagonal solve
-- =========================================================================

-- | Input for the extended column tridiagonal solve.
data LakeTridiagInput = LakeTridiagInput
  { lti_snl         :: !Int
  , lti_nlevlak     :: !Int
  , lti_fin         :: !Double            -- ^ Ground heat flux into lake [W/m2]
  , lti_t_soisno    :: !(VU.Vector Double)
  , lti_t_lake      :: !(VU.Vector Double)
  , lti_z           :: !(VU.Vector Double)  -- ^ Snow/soil midpoints
  , lti_dz          :: !(VU.Vector Double)  -- ^ Snow/soil thicknesses
  , lti_z_lake      :: !(VU.Vector Double)
  , lti_dz_lake     :: !(VU.Vector Double)
  , lti_cv          :: !(VU.Vector Double)  -- ^ Snow/soil heat capacity
  , lti_tk          :: !(VU.Vector Double)  -- ^ Snow/soil interface conductivity
  , lti_cv_lake     :: !(VU.Vector Double)  -- ^ Lake heat capacity
  , lti_tk_lake     :: !(VU.Vector Double)  -- ^ Lake conductivity
  , lti_phi         :: !(VU.Vector Double)  -- ^ Solar heat per lake layer
  , lti_phi_soil    :: !Double              -- ^ Solar reaching soil
  , lti_tktopsoillay :: !Double
  , lti_sabg_lyr    :: !(VU.Vector Double)  -- ^ Absorbed solar per snow layer
  , lti_dtime       :: !Double
  } deriving (Show)

-- | Output from the extended column tridiagonal solve.
data LakeTridiagOutput = LakeTridiagOutput
  { lto_t_soisno :: !(VU.Vector Double)
  , lto_t_lake   :: !(VU.Vector Double)
  } deriving (Show)

-- | Solve the extended column tridiagonal system for lake temperature.
-- Ported from lake_temperature! sections 6-7.
lakeTridiagSolve :: LakeTridiagInput -> LakeTridiagOutput
lakeTridiagSolve inp = LakeTridiagOutput
  { lto_t_soisno = t_soisno_new
  , lto_t_lake   = t_lake_new
  }
  where
    !snl_     = lti_snl inp
    !nlevlak  = lti_nlevlak inp
    !dt       = lti_dtime inp
    !fin_val  = lti_fin inp

    -- Port combined snow+soil arrays place Fortran layer L (snow L<=0, soil
    -- L>=1) at 0-based index L + (nlevsno-1): bottom snow (L=0) at nlevsno-1,
    -- top soil (L=1) at nlevsno. So the offset is nlevsno-1, not nlevsno.
    joff    = nlevsno - 1
    jtop    = snl_ + 1
    -- Extended column: indices from (jtop) to (nlevlak + nlevgrnd)
    -- We map to 0-based array of size n
    ntotal  = nlevlak + nlevgrnd - jtop + 1
    -- But for simplicity, let's keep the snow layers separate

    -- Build extended arrays: zx, cvx, phix, tx, tkix
    -- j_ext ranges from jtop to (nlevlak + nlevgrnd)
    -- Map to local index: k = j_ext - jtop

    n = nlevlak + nlevgrnd - jtop + 1

    zxV = VU.generate n $ \k ->
      let j_ext = k + jtop
      in zxAt j_ext

    zxAt j_ext
      | j_ext < 1 =  -- snow layer
          let jj = j_ext + joff
          in lti_z inp VU.! jj
      | j_ext <= nlevlak =  -- lake layer
          lti_z_lake inp VU.! (j_ext - 1)
      | otherwise =  -- soil layer
          let jprime = j_ext - nlevlak
              jprime_jj = jprime + joff
              zLakeBot = (lti_z_lake inp VU.! (nlevlak - 1))
                       + (lti_dz_lake inp VU.! (nlevlak - 1)) / 2.0
          in zLakeBot + (lti_z inp VU.! jprime_jj)

    cvxV = VU.generate n $ \k ->
      let j_ext = k + jtop
      in if j_ext < 1
         then lti_cv inp VU.! (j_ext + joff)
         else if j_ext <= nlevlak
              then lti_cv_lake inp VU.! (j_ext - 1)
              else let jprime = j_ext - nlevlak
                   in lti_cv inp VU.! (jprime + joff)

    phixV = VU.generate n $ \k ->
      let j_ext = k + jtop
      in if j_ext < 1
         then if j_ext == jtop then 0.0
              else let idx = j_ext + nlevsno
                   in if idx >= 0 && idx < VU.length (lti_sabg_lyr inp)
                      then lti_sabg_lyr inp VU.! idx
                      else 0.0
         else if j_ext <= nlevlak
              then lti_phi inp VU.! (j_ext - 1)
              else if j_ext == nlevlak + 1
                   then lti_phi_soil inp
                   else 0.0

    txV = VU.generate n $ \k ->
      let j_ext = k + jtop
      in if j_ext < 1
         then lti_t_soisno inp VU.! (j_ext + joff)
         else if j_ext <= nlevlak
              then lti_t_lake inp VU.! (j_ext - 1)
              else let jprime = j_ext - nlevlak
                   in lti_t_soisno inp VU.! (jprime + joff)

    -- Interface conductivities
    tkixV = VU.generate n $ \k ->
      let j_ext = k + jtop
      in if j_ext < 0
         then lti_tk inp VU.! (j_ext + joff)
         else if j_ext == 0 && k > 0
         then let dzp   = (zxV VU.! (k + 1)) - (zxV VU.! k)
                  jj    = j_ext + joff
                  tk_j  = lti_tk inp VU.! jj
                  tk_l1 = lti_tk_lake inp VU.! 0
                  z_j   = lti_z inp VU.! jj
              in tk_l1 * tk_j * dzp / (tk_j * (lti_z_lake inp VU.! 0) + tk_l1 * (negate z_j))
         else if j_ext >= 1 && j_ext < nlevlak
         then let tk_j  = lti_tk_lake inp VU.! (j_ext - 1)
                  tk_j1 = lti_tk_lake inp VU.! j_ext
                  dz_j  = lti_dz_lake inp VU.! (j_ext - 1)
                  dz_j1 = lti_dz_lake inp VU.! j_ext
              in (tk_j * tk_j1 * (dz_j1 + dz_j)) / (tk_j * dz_j1 + tk_j1 * dz_j)
         else if j_ext == nlevlak
         then let dzp       = (zxV VU.! (k + 1)) - (zxV VU.! k)
                  tk_lbot   = lti_tk_lake inp VU.! (nlevlak - 1)
                  dz_lbot   = lti_dz_lake inp VU.! (nlevlak - 1)
                  z_soil1   = lti_z inp VU.! (1 + joff)
                  tktop     = lti_tktopsoillay inp
              in tktop * tk_lbot * dzp / (tktop * dz_lbot / 2.0 + tk_lbot * z_soil1)
         else if j_ext > nlevlak
         then let jprime = j_ext - nlevlak
              in lti_tk inp VU.! (jprime + joff)
         else 0.0

    -- Factor and flux
    factxV = VU.generate n $ \k ->
      let cv_k = cvxV VU.! k
      in if cv_k > 0.0 then dt / cv_k else 0.0

    fnxV = VU.generate n $ \k ->
      if k < n - 1
      then let dzp = (zxV VU.! (k + 1)) - (zxV VU.! k)
           in (tkixV VU.! k) * ((txV VU.! (k + 1)) - (txV VU.! k)) / dzp
      else 0.0

    -- Tridiagonal coefficients
    aV = VU.generate n $ \k ->
      if k == 0
      then 0.0
      else let dzm = (zxV VU.! k) - (zxV VU.! (k - 1))
           in -(1.0 - cnfacLake) * (factxV VU.! k) * (tkixV VU.! (k - 1)) / dzm

    bV = VU.generate n $ \k ->
      if k == 0
      then let dzp = (zxV VU.! 1) - (zxV VU.! 0)
           in 1.0 + (1.0 - cnfacLake) * (factxV VU.! 0) * (tkixV VU.! 0) / dzp
      else if k < n - 1
      then let dzm = (zxV VU.! k) - (zxV VU.! (k - 1))
               dzp = (zxV VU.! (k + 1)) - (zxV VU.! k)
           in 1.0 + (1.0 - cnfacLake) * (factxV VU.! k)
                  * ((tkixV VU.! k) / dzp + (tkixV VU.! (k - 1)) / dzm)
      else let dzm = (zxV VU.! k) - (zxV VU.! (k - 1))
           in 1.0 + (1.0 - cnfacLake) * (factxV VU.! k) * (tkixV VU.! (k - 1)) / dzm

    cV = VU.generate n $ \k ->
      if k >= n - 1
      then 0.0
      else let dzp = (zxV VU.! (k + 1)) - (zxV VU.! k)
           in -(1.0 - cnfacLake) * (factxV VU.! k) * (tkixV VU.! k) / dzp

    rV = VU.generate n $ \k ->
      if k == 0
      then (txV VU.! 0) + (factxV VU.! 0)
           * (fin_val + (phixV VU.! 0) + cnfacLake * (fnxV VU.! 0))
      else if k < n - 1
      then (txV VU.! k) + cnfacLake * (factxV VU.! k)
           * ((fnxV VU.! k) - (fnxV VU.! (k - 1)))
           + (factxV VU.! k) * (phixV VU.! k)
      else (txV VU.! k) - cnfacLake * (factxV VU.! k) * (fnxV VU.! (k - 1))

    -- Thomas algorithm solve
    solnV = thomasSolve aV bV cV rV

    -- Extract t_soisno and t_lake from solution
    t_soisno_new = VU.generate (nlevsno + nlevgrnd) $ \jj ->
      let j = jj - joff   -- Fortran layer L (snow L<=0, soil L>=1)
      in if j >= jtop && j < 1     -- active snow layer: extended index = L
         then let k = j - jtop
              in if k >= 0 && k < n then solnV VU.! k else lti_t_soisno inp VU.! jj
         else if j >= 1            -- soil layer beneath the lake
         then let k = (j + nlevlak) - jtop  -- extended index = soil layer + nlevlak
              in if k >= 0 && k < n then solnV VU.! k
                 else lti_t_soisno inp VU.! jj
         else lti_t_soisno inp VU.! jj   -- inactive snow above the pack

    t_lake_new = VU.generate nlevlak $ \j ->
      let k = (j + 1) - jtop  -- j_ext = j+1 (1-based lake layer)
      in if k >= 0 && k < n then solnV VU.! k
         else lti_t_lake inp VU.! j

-- | Thomas algorithm for tridiagonal solve (pure).
thomasSolve
  :: VU.Vector Double  -- ^ sub-diagonal a
  -> VU.Vector Double  -- ^ diagonal b
  -> VU.Vector Double  -- ^ super-diagonal c
  -> VU.Vector Double  -- ^ rhs r
  -> VU.Vector Double  -- ^ solution x
thomasSolve aV bV cV rV = xV
  where
    n = VU.length bV

    -- Forward sweep: build lists of c' and d' values
    fwdSweep = foldl fwdStep [] [0 .. n - 1]

    fwdStep acc i =
      let a_i = aV VU.! i
          b_i = bV VU.! i
          c_i = cV VU.! i
          r_i = rV VU.! i
          (cp_prev, dp_prev) = case acc of
            []            -> (0.0, 0.0)
            ((cp, dp):_)  -> (cp, dp)
          m    = b_i - a_i * cp_prev
          cp_i = c_i / m
          dp_i = (r_i - a_i * dp_prev) / m
      in (cp_i, dp_i) : acc

    -- fwdSweep is in reverse order (index n-1 first, index 0 last)
    -- Convert to vectors
    cP = VU.fromListN n [cp | (cp, _) <- reverse fwdSweep]
    dP = VU.fromListN n [dp | (_, dp) <- reverse fwdSweep]

    -- Back substitution: build list from last to first
    backSweep = foldl backStep [] (reverse [0 .. n - 1])

    backStep acc k =
      let x_k = case acc of
                  []    -> dP VU.! k  -- last element
                  (x_next:_) -> (dP VU.! k) - (cP VU.! k) * x_next
      in x_k : acc

    -- backSweep is in forward order (index 0 first)
    xV = VU.fromListN n backSweep

-- =========================================================================
-- Convective mixing
-- =========================================================================

-- | Input for convective mixing.
data LakeConvMixInput = LakeConvMixInput
  { lcmi_t_lake        :: !(VU.Vector Double)
  , lcmi_lake_icefrac  :: !(VU.Vector Double)
  , lcmi_dz_lake       :: !(VU.Vector Double)
  , lcmi_nlevlak       :: !Int
  } deriving (Show)

-- | Output from convective mixing.
data LakeConvMixOutput = LakeConvMixOutput
  { lcmo_t_lake        :: !(VU.Vector Double)
  , lcmo_lake_icefrac  :: !(VU.Vector Double)
  , lcmo_rhow          :: !(VU.Vector Double)
  } deriving (Show)

-- | Perform convective mixing on lake layers.
-- Ported from lake_temperature! section 10).
lakeConvectiveMix :: LakeConvMixInput -> LakeConvMixOutput
lakeConvectiveMix inp = LakeConvMixOutput
  { lcmo_t_lake       = t_final
  , lcmo_lake_icefrac = if_final
  , lcmo_rhow         = rho_final
  }
  where
    !nlevlak = lcmi_nlevlak inp
    dz_lake  = lcmi_dz_lake inp

    cwat     = cpliq * denh2o
    cice_eff = cpice * denh2o

    -- Start with current state
    t0       = lcmi_t_lake inp
    if0      = lcmi_lake_icefrac inp
    rho0     = lakeDensity t0 if0

    -- Top-down convection (layers 0 to nlevlak-3)
    (t1, if1, rho1) = foldl topDownMix (t0, if0, rho0) [0 .. nlevlak - 3]

    topDownMix (!tl, !ifr, !rh) j =
      if (rh VU.! j) > (rh VU.! (j + 1)) ||
         ((ifr VU.! j) < 1.0 && (ifr VU.! (j + 1)) > 0.0)
      then mixLayers tl ifr rh 0 (j + 1)
      else (tl, ifr, rh)

    -- Bottom-up convection check
    jbot = nlevlak - 2
    doBottomConvect = (rho1 VU.! jbot) > (rho1 VU.! (jbot + 1))
                   || ((if1 VU.! jbot) < 1.0 && (if1 VU.! (jbot + 1)) > 0.0)

    (t_final, if_final, rho_final)
      | doBottomConvect = foldl bottomUpMix (t1, if1, rho1) (reverse [0 .. nlevlak - 2])
      | otherwise       = (t1, if1, rho1)

    bottomUpMix (!tl, !ifr, !rh) j =
      if (rh VU.! j) > (rh VU.! (j + 1)) ||
         ((ifr VU.! j) < 1.0 && (ifr VU.! (j + 1)) > 0.0)
      then mixLayers tl ifr rh j (nlevlak - 1)
      else (tl, ifr, rh)

    -- Mix layers from iTop to iBot inclusive
    mixLayers tl ifr _rh iTop iBot =
      let -- Accumulate heat and ice
          indices = [iTop .. iBot]
          (qav, iceav, nav) = foldl
            (\(!q, !ia, !n_acc) i ->
              let dz_i = dz_lake VU.! i
                  t_i  = tl VU.! i
                  if_i = ifr VU.! i
              in ( q + dz_i * (t_i - tfrz) * ((1.0 - if_i) * cwat + if_i * cice_eff)
                 , ia + if_i * dz_i
                 , n_acc + dz_i
                 )
            )
            (0.0, 0.0, 0.0) indices

          qav'   = qav / nav
          iceav' = iceav / nav

          (tav_froz, tav_unfr)
            | qav' > 0.0  = (0.0, qav' / ((1.0 - iceav') * cwat))
            | qav' < 0.0  = (qav' / (iceav' * cice_eff), 0.0)
            | otherwise    = (0.0, 0.0)

          -- Redistribute
          (tl', ifr', _) = foldl
            (\(!tv, !iv, !zs) i ->
              let dz_i = dz_lake VU.! i
                  (if_new, t_new)
                    | (zs + dz_i) / nav <= iceav' =
                        (1.0, tav_froz + tfrz)
                    | zs / nav < iceav' =
                        let ifn = (iceav' * nav - zs) / dz_i
                            tn  = (ifn * tav_froz * cice_eff + (1.0 - ifn) * tav_unfr * cwat)
                                  / (ifn * cice_eff + (1.0 - ifn) * cwat) + tfrz
                        in (ifn, tn)
                    | otherwise =
                        (0.0, tav_unfr + tfrz)
              in ( tv VU.// [(i, t_new)]
                 , iv VU.// [(i, if_new)]
                 , zs + dz_i
                 )
            )
            (tl, ifr, 0.0) indices

          rho' = lakeDensity tl' ifr'

      in (tl', ifr', rho')
