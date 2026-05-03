{-# LANGUAGE BangPatterns #-}
-- | Surface water ponding dynamics.
-- Fortran: SurfaceWaterMod.F90
--
-- Provides:
--   * Submerged fraction computation via Newton-Raphson iteration
--   * Surface water runoff (connectivity-dependent linear reservoir)
--   * Surface water drainage (infiltration from h2osfc into soil)
--   * State update for h2osfc
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SurfaceWater
  ( -- * Constants
    surfaceWaterPc
  , surfaceWaterMu
  , minH2osfc
    -- * Input/output records
  , FracH2osfcInput(..)
  , FracH2osfcResult(..)
  , QflxH2osfcSurfInput(..)
  , QflxH2osfcDrainInput(..)
  , UpdateH2osfcInput(..)
  , UpdateH2osfcResult(..)
    -- * Science functions
  , computeFracH2osfc
  , computeQflxH2osfcSurf
  , computeQflxH2osfcDrain
  , updateH2osfc
  ) where

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Threshold probability for surface water connectivity
surfaceWaterPc :: Double
surfaceWaterPc = 0.5

-- | Connectivity exponent
surfaceWaterMu :: Double
surfaceWaterMu = 1.0

-- | Minimum surface water for numerical stability [mm]
minH2osfc :: Double
minH2osfc = 1.0e-8

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Inputs for submerged fraction computation
data FracH2osfcInput = FracH2osfcInput
  { fhi_dtime        :: !Double  -- ^ Time step [s]
  , fhi_micro_sigma  :: !Double  -- ^ Microtopography standard deviation [m]
  , fhi_h2osno_total :: !Double  -- ^ Total snow water equivalent [mm]
  , fhi_h2osfc       :: !Double  -- ^ Surface water storage [mm]
  , fhi_frac_sno     :: !Double  -- ^ Snow fraction
  , fhi_frac_sno_eff :: !Double  -- ^ Effective snow fraction
  } deriving (Show)

-- | Result of submerged fraction computation
data FracH2osfcResult = FracH2osfcResult
  { fhr_frac_h2osfc        :: !Double  -- ^ Submerged fraction
  , fhr_frac_h2osfc_nosnow :: !Double  -- ^ Submerged fraction ignoring snow
  , fhr_frac_sno           :: !Double  -- ^ Updated snow fraction
  , fhr_frac_sno_eff       :: !Double  -- ^ Updated effective snow fraction
  , fhr_qflx_too_small     :: !Double  -- ^ Flux of too-small h2osfc to soil [mm/s]
  } deriving (Show)

-- | Inputs for surface water runoff
data QflxH2osfcSurfInput = QflxH2osfcSurfInput
  { qhsi_dtime               :: !Double  -- ^ Time step [s]
  , qhsi_h2osfcflag          :: !Int     -- ^ Surface water flag (0=off, 1=on)
  , qhsi_h2osfc              :: !Double  -- ^ Surface water storage [mm]
  , qhsi_h2osfc_thresh       :: !Double  -- ^ Percolation threshold [mm]
  , qhsi_frac_h2osfc_nosnow  :: !Double  -- ^ Submerged fraction (no snow)
  , qhsi_topo_slope          :: !Double  -- ^ Topographic slope [degrees]
  } deriving (Show)

-- | Inputs for surface water drainage
data QflxH2osfcDrainInput = QflxH2osfcDrainInput
  { qhdi_dtime        :: !Double  -- ^ Time step [s]
  , qhdi_h2osfcflag   :: !Int     -- ^ Surface water flag
  , qhdi_h2osfc       :: !Double  -- ^ Surface water storage [mm]
  , qhdi_frac_h2osfc  :: !Double  -- ^ Submerged fraction
  , qhdi_qinmax       :: !Double  -- ^ Maximum infiltration rate [mm/s]
  } deriving (Show)

-- | Inputs for full h2osfc update
data UpdateH2osfcInput = UpdateH2osfcInput
  { uhi_dtime             :: !Double  -- ^ Time step [s]
  , uhi_h2osfcflag        :: !Int     -- ^ Surface water flag
  , uhi_h2osfc            :: !Double  -- ^ Surface water storage [mm]
  , uhi_h2osfc_thresh     :: !Double  -- ^ Percolation threshold [mm]
  , uhi_frac_h2osfc       :: !Double  -- ^ Submerged fraction
  , uhi_frac_h2osfc_nosnow :: !Double -- ^ Submerged fraction (no snow)
  , uhi_topo_slope        :: !Double  -- ^ Topographic slope [degrees]
  , uhi_qinmax            :: !Double  -- ^ Maximum infiltration rate [mm/s]
  , uhi_qflx_in_h2osfc   :: !Double  -- ^ Flux into surface water [mm/s]
  } deriving (Show)

-- | Result of h2osfc update
data UpdateH2osfcResult = UpdateH2osfcResult
  { uhr_h2osfc             :: !Double  -- ^ Updated surface water [mm]
  , uhr_qflx_h2osfc_surf   :: !Double  -- ^ Surface runoff from h2osfc [mm/s]
  , uhr_qflx_h2osfc_drain  :: !Double  -- ^ Drainage from h2osfc [mm/s]
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Determine submerged fraction using microtopography and surface water
-- storage via Newton-Raphson iteration.
-- Ported from BulkDiag_FracH2oSfc in SurfaceWaterMod.F90
computeFracH2osfc :: FracH2osfcInput -> FracH2osfcResult
computeFracH2osfc inp =
  let h2osfc_val = fhi_h2osfc inp
      dtime_val  = fhi_dtime inp
      frac_sno0  = fhi_frac_sno inp
      frac_sno_eff0 = fhi_frac_sno_eff inp
      h2osno_val = fhi_h2osno_total inp
  in if h2osfc_val > minH2osfc
     then
       let -- Convert microtopography sigma from m to mm
           sigma_raw = 1.0e3 * fhi_micro_sigma inp
           sigma = max sigma_raw 1.0e-6

           -- Newton-Raphson iteration (10 steps)
           d_final = iterateNR 10 sigma h2osfc_val 0.0

           -- Compute submerged fraction
           frac_h2osfc_val = 0.5 * (1.0 + errF (d_final / (sigma * sqrt 2.0)))
           frac_h2osfc_nosnow_val = frac_h2osfc_val

           -- Adjust fractions so frac_sno + frac_h2osfc <= 1
           (frac_sno_out, frac_sno_eff_out, frac_h2osfc_out) =
             if frac_sno0 > (1.0 - frac_h2osfc_val) && h2osno_val > 0.0
             then if frac_h2osfc_val > 0.01
                  then let fh = max (1.0 - frac_sno0) 0.01
                           fs = 1.0 - fh
                       in (fs, fs, fh)
                  else let fs = 1.0 - frac_h2osfc_val
                       in (fs, fs, frac_h2osfc_val)
             else (frac_sno0, frac_sno_eff0, frac_h2osfc_val)

       in FracH2osfcResult
            { fhr_frac_h2osfc        = frac_h2osfc_out
            , fhr_frac_h2osfc_nosnow = frac_h2osfc_nosnow_val
            , fhr_frac_sno           = frac_sno_out
            , fhr_frac_sno_eff       = frac_sno_eff_out
            , fhr_qflx_too_small     = 0.0
            }
     else
       -- Surface water too small -- transfer to soil
       FracH2osfcResult
         { fhr_frac_h2osfc        = 0.0
         , fhr_frac_h2osfc_nosnow = 0.0
         , fhr_frac_sno           = frac_sno0
         , fhr_frac_sno_eff       = frac_sno_eff0
         , fhr_qflx_too_small     = h2osfc_val / dtime_val
         }

-- | Newton-Raphson iteration for water depth
iterateNR :: Int -> Double -> Double -> Double -> Double
iterateNR 0 _sigma _h2osfc !d = d
iterateNR !n !sigma !h2osfc !d =
  let arg = d / (sigma * sqrt 2.0)
      fd  = 0.5 * d * (1.0 + errF arg)
          + sigma / sqrt (2.0 * pi) * exp (negate (d * d) / (2.0 * sigma * sigma))
          - h2osfc
      dfdd = 0.5 * (1.0 + errF arg)
      dfdd' = max dfdd 1.0e-12
      d' = d - fd / dfdd'
  in iterateNR (n - 1) sigma h2osfc d'

-- | Compute surface runoff from h2osfc using connectivity-dependent
-- linear reservoir model.
-- Ported from QflxH2osfcSurf in SurfaceWaterMod.F90
computeQflxH2osfcSurf :: QflxH2osfcSurfInput -> Double
computeQflxH2osfcSurf inp
  | qhsi_h2osfcflag inp /= 1 = 0.0
  | otherwise =
      let frac_nosnow = qhsi_frac_h2osfc_nosnow inp
          frac_infclust = if frac_nosnow <= surfaceWaterPc
                          then 0.0
                          else (frac_nosnow - surfaceWaterPc) ** surfaceWaterMu
          h2osfc_val = qhsi_h2osfc inp
          thresh = qhsi_h2osfc_thresh inp
          dtime_val = qhsi_dtime inp
      in if h2osfc_val > thresh && qhsi_h2osfcflag inp /= 0
         then let k_wet_raw = 1.0e-4 * sin (pi / 180.0 * qhsi_topo_slope inp)
                  k_wet = max k_wet_raw 1.0e-7
                  qflx_raw = k_wet * frac_infclust * (h2osfc_val - thresh)
                  qflx_lim = min qflx_raw ((h2osfc_val - thresh) / dtime_val)
              in if qflx_lim < 1.0e-8 then 0.0 else qflx_lim
         else 0.0

-- | Compute infiltration/drainage from h2osfc into soil.
-- Ported from QflxH2osfcDrain in SurfaceWaterMod.F90
computeQflxH2osfcDrain :: QflxH2osfcDrainInput -> Double
computeQflxH2osfcDrain inp
  | qhdi_h2osfc inp < 0.0 =
      -- Numerical error recovery
      qhdi_h2osfc inp / qhdi_dtime inp
  | otherwise =
      let frac_h2osfc_val = qhdi_frac_h2osfc inp
          qinmax_val = qhdi_qinmax inp
          h2osfc_val = qhdi_h2osfc inp
          dtime_val = qhdi_dtime inp
          drain = min (frac_h2osfc_val * qinmax_val) (h2osfc_val / dtime_val)
      in if qhdi_h2osfcflag inp == 0
         then max 0.0 (h2osfc_val / dtime_val)
         else drain

-- | Calculate fluxes out of h2osfc and update the h2osfc state variable.
-- Ported from UpdateH2osfc in SurfaceWaterMod.F90
updateH2osfc :: UpdateH2osfcInput -> UpdateH2osfcResult
updateH2osfc inp =
  let dtime_val = uhi_dtime inp
      h2osfc0 = uhi_h2osfc inp

      -- Step 1: Surface runoff
      surf_inp = QflxH2osfcSurfInput
        { qhsi_dtime              = dtime_val
        , qhsi_h2osfcflag         = uhi_h2osfcflag inp
        , qhsi_h2osfc             = h2osfc0
        , qhsi_h2osfc_thresh      = uhi_h2osfc_thresh inp
        , qhsi_frac_h2osfc_nosnow = uhi_frac_h2osfc_nosnow inp
        , qhsi_topo_slope         = uhi_topo_slope inp
        }
      qflx_surf = computeQflxH2osfcSurf surf_inp

      -- Step 2: Partial update
      h2osfc1_raw = h2osfc0 + (uhi_qflx_in_h2osfc inp - qflx_surf) * dtime_val
      h2osfc1 = if abs h2osfc1_raw < 1.0e-10 then 0.0 else h2osfc1_raw

      -- Step 3: Drainage
      drain_inp = QflxH2osfcDrainInput
        { qhdi_dtime       = dtime_val
        , qhdi_h2osfcflag  = uhi_h2osfcflag inp
        , qhdi_h2osfc      = h2osfc1
        , qhdi_frac_h2osfc = uhi_frac_h2osfc inp
        , qhdi_qinmax      = uhi_qinmax inp
        }
      qflx_drain = computeQflxH2osfcDrain drain_inp

      -- Step 4: Final update
      h2osfc2_raw = h2osfc1 - qflx_drain * dtime_val
      h2osfc2 = if abs h2osfc2_raw < 1.0e-10 then 0.0 else h2osfc2_raw

  in UpdateH2osfcResult
       { uhr_h2osfc            = h2osfc2
       , uhr_qflx_h2osfc_surf  = qflx_surf
       , uhr_qflx_h2osfc_drain = qflx_drain
       }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Approximate error function using Abramowitz & Stegun formula 7.1.26
errF :: Double -> Double
errF x =
  let t = 1.0 / (1.0 + 0.3275911 * abs x)
      poly = t * (0.254829592 + t * (negate 0.284496736
           + t * (1.421413741 + t * (negate 1.453152027
           + t * 1.061405429))))
      result = 1.0 - poly * exp (negate (x * x))
  in if x >= 0.0 then result else negate result
