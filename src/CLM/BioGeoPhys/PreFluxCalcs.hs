{-# LANGUAGE BangPatterns #-}
-- | Pre-flux biogeophysics calculations.
-- Fortran: BiogeophysPreFluxCalcsMod.F90
-- Julia:   src/biogeophys/pre_flux_calcs.jl
--
-- Performs calculations needed before the main flux routines:
--   1. Set momentum roughness length and displacement height
--   2. Initialize temperature and energy variables for the timestep
--   3. Compute ground temperature, emissivity, latent heat type
--
-- All functions are pure.
module CLM.BioGeoPhys.PreFluxCalcs
  ( -- * Z0m and displacement height
    SetZ0mDisplaInput(..)
  , SetZ0mDisplaOutput(..)
  , setZ0mDispla
    -- * Initial temperature and energy
  , CalcInitTempEnergyColInput(..)
  , CalcInitTempEnergyColOutput(..)
  , calcInitTempEnergyCol
  , CalcInitTempEnergyPatchInput(..)
  , CalcInitTempEnergyPatchOutput(..)
  , calcInitTempEnergyPatch
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (hvap, hsub, nlevsno)
import CLM.Constants.LandunitConstants (istice)

-- =========================================================================
-- Z0m and displacement height
-- =========================================================================

-- | Input for setting z0m and displacement height for a single patch.
data SetZ0mDisplaInput = SetZ0mDisplaInput
  { szd_htop    :: !Double  -- ^ Canopy top height [m]
  , szd_z0mr    :: !Double  -- ^ PFT-specific z0m/htop ratio
  , szd_displar :: !Double  -- ^ PFT-specific displa/htop ratio
  } deriving (Show)

-- | Output of z0m and displacement height computation.
data SetZ0mDisplaOutput = SetZ0mDisplaOutput
  { szd_z0m    :: !Double  -- ^ Momentum roughness length [m]
  , szd_displa :: !Double  -- ^ Displacement height [m]
  } deriving (Show)

-- | Compute z0m and displacement height for a single patch.
-- Uses Zeng & Wang 2007 formulation.
setZ0mDispla :: SetZ0mDisplaInput -> SetZ0mDisplaOutput
setZ0mDispla inp = SetZ0mDisplaOutput
  { szd_z0m    = z0mr_v * htop_v
  , szd_displa = displar_v * htop_v
  }
  where
    htop_v    = szd_htop inp
    z0mr_v    = if szd_z0mr inp > 0.0 then szd_z0mr inp else 0.055
    displar_v = if szd_displar inp > 0.0 then szd_displar inp else 0.67

-- =========================================================================
-- Initial temperature and energy (column level)
-- =========================================================================

-- | Column-level input for initial temperature/energy calculations.
data CalcInitTempEnergyColInput = CalcInitTempEnergyColInput
  { cite_snl           :: !Int              -- ^ Number of snow layers (<= 0)
  , cite_frac_sno_eff  :: !Double           -- ^ Effective snow cover fraction
  , cite_frac_h2osfc   :: !Double           -- ^ Surface water fraction
  , cite_frac_sno      :: !Double           -- ^ Snow cover fraction (for emissivity)
  , cite_t_soisno      :: !(VU.Vector Double) -- ^ Layer temperatures [K]
  , cite_t_h2osfc      :: !Double           -- ^ Surface water temperature [K]
  , cite_h2osoi_liq_top :: !Double          -- ^ Liquid in top layer [kg/m2]
  , cite_h2osoi_ice_top :: !Double          -- ^ Ice in top layer [kg/m2]
  , cite_forc_th       :: !Double           -- ^ Potential temperature [K]
  , cite_forc_q        :: !Double           -- ^ Specific humidity [kg/kg]
  , cite_is_urban      :: !Bool             -- ^ Is this an urban landunit?
  , cite_is_ice        :: !Bool             -- ^ Is this ISTICE?
  } deriving (Show)

-- | Column-level output of initial temperature/energy calculations.
data CalcInitTempEnergyColOutput = CalcInitTempEnergyColOutput
  { cite_t_grnd_out    :: !Double   -- ^ Ground temperature [K]
  , cite_emg_out       :: !Double   -- ^ Ground emissivity
  , cite_htvp_out      :: !Double   -- ^ Latent heat type [J/kg]
  , cite_thv_out       :: !Double   -- ^ Virtual potential temperature [K]
  , cite_beta_out      :: !Double   -- ^ Convective velocity parameter
  , cite_zii_out       :: !Double   -- ^ Convective BL height [m]
  , cite_tssbef_out    :: !(VU.Vector Double)  -- ^ Saved temperatures [K]
  } deriving (Show)

-- | Compute column-level initial temperature and energy variables.
calcInitTempEnergyCol :: CalcInitTempEnergyColInput -> CalcInitTempEnergyColOutput
calcInitTempEnergyCol inp = CalcInitTempEnergyColOutput
  { cite_t_grnd_out = t_grnd_val
  , cite_emg_out    = emg_val
  , cite_htvp_out   = htvp_val
  , cite_thv_out    = thv_val
  , cite_beta_out   = 1.0
  , cite_zii_out    = 1000.0
  , cite_tssbef_out = cite_t_soisno inp  -- save current temps
  }
  where
    snl  = cite_snl inp
    fse  = cite_frac_sno_eff inp
    fh2o = cite_frac_h2osfc inp
    tsoi = cite_t_soisno inp

    -- Ground temperature
    t_soil1 = tsoi VU.! nlevsno  -- first soil layer
    t_h2osfc_v = cite_t_h2osfc inp
    t_grnd_val
      | snl < 0 =
          let jtop = snl + 1 + nlevsno
              t_snow_top = tsoi VU.! (jtop - 1)  -- 0-indexed in VU
          in fse * t_snow_top + (1.0 - fse - fh2o) * t_soil1 + fh2o * t_h2osfc_v
      | otherwise =
          (1.0 - fh2o) * t_soil1 + fh2o * t_h2osfc_v

    -- Ground emissivity
    emg_val
      -- Urban-facet default. In Fortran (BiogeophysPreFluxCalcsMod) emg is NOT
      -- computed here for urban landunits; the per-facet urban emissivities
      -- (em_roof/em_wall/em_improad/em_perroad in UrbanParamsType) are read from
      -- the urban surface data and assigned upstream. Urban surfdata is not wired
      -- into this port, so this branch supplies a representative urban-facet
      -- ground emissivity default (urban path is not exercised).
      | cite_is_urban inp = 0.96
      | cite_is_ice inp   = 0.97
      | otherwise         =
          let fs = cite_frac_sno inp
          in (1.0 - fs) * 0.96 + fs * 0.97

    -- Latent heat type
    htvp_val
      | cite_h2osoi_liq_top inp <= 0.0 && cite_h2osoi_ice_top inp > 0.0 = hsub
      | otherwise = hvap

    -- Virtual potential temperature
    thv_val = cite_forc_th inp * (1.0 + 0.61 * cite_forc_q inp)

-- =========================================================================
-- Initial temperature and energy (patch level)
-- =========================================================================

-- | Patch-level input for initial temperature/energy calculations.
data CalcInitTempEnergyPatchInput = CalcInitTempEnergyPatchInput
  { citep_elai      :: !Double  -- ^ Exposed LAI
  , citep_esai      :: !Double  -- ^ Exposed SAI
  , citep_forc_t    :: !Double  -- ^ Air temperature at column [K]
  , citep_forc_hgt_t :: !Double -- ^ Forcing height for temperature [m]
  } deriving (Show)

-- | Patch-level output of initial temperature/energy calculations.
data CalcInitTempEnergyPatchOutput = CalcInitTempEnergyPatchOutput
  { citep_emv_out   :: !Double  -- ^ Vegetation emissivity
  , citep_thm_out   :: !Double  -- ^ Intermediate temperature variable [K]
  } deriving (Show)

-- | Compute patch-level initial temperature and energy variables.
calcInitTempEnergyPatch :: CalcInitTempEnergyPatchInput -> CalcInitTempEnergyPatchOutput
calcInitTempEnergyPatch inp = CalcInitTempEnergyPatchOutput
  { citep_emv_out = emv_val
  , citep_thm_out = thm_val
  }
  where
    avmuir = 1.0
    elai_v = citep_elai inp
    esai_v = citep_esai inp
    emv_val = 1.0 - exp (-(elai_v + esai_v) / avmuir)
    thm_val = citep_forc_t inp + 0.0098 * citep_forc_hgt_t inp
