-- | Shared CN/BGC parameters.
-- Fortran: CNSharedParamsMod.F90 — biogeochemical rate constants and control flags.
module CLM.Types.CNSharedParamsData
  ( CNSharedParamsData(..)
  , defaultCNSharedParamsData
  ) where

-- | CN shared parameters.
-- Preserves Fortran variable names with @cnsp_@ prefix.
-- All scalar fields (no arrays).
data CNSharedParamsData = CNSharedParamsData
  { cnsp_Q10                              :: !Double  -- ^ temperature dependence
  , cnsp_minpsi                           :: !Double  -- ^ minimum soil water potential for het. resp. (MPa)
  , cnsp_maxpsi                           :: !Double  -- ^ maximum soil water potential for het. resp. (MPa)
  , cnsp_rf_cwdl2                         :: !Double  -- ^ respiration fraction in CWD to litter2 transition
  , cnsp_tau_cwd                          :: !Double  -- ^ fragmentation rate constant CWD (1/yr)
  , cnsp_cwd_flig                         :: !Double  -- ^ lignin fraction of coarse woody debris
  , cnsp_froz_q10                         :: !Double  -- ^ separate Q10 for frozen soil respiration
  , cnsp_decomp_depth_efolding            :: !Double  -- ^ e-folding depth for decomposition reduction (m)
  , cnsp_mino2lim                         :: !Double  -- ^ minimum anaerobic decomp rate as fraction of aerobic
  , cnsp_organic_max                      :: !Double  -- ^ organic matter content where soil acts like peat (kg/m3)
  , cnsp_constrain_stress_deciduous_onset :: !Bool    -- ^ use additional constraint on stress deciduous onset
  , cnsp_use_matrixcn                     :: !Bool    -- ^ use CN matrix solution
  , cnsp_use_fun                          :: !Bool    -- ^ use FUN 2.0 model
  , cnsp_nlev_soildecomp_standard         :: !Int     -- ^ standard number of soil decomposition levels
  , cnsp_upper_soil_layer                 :: !Int     -- ^ upper soil layer for 10-day average in CNPhenology
  } deriving (Show)

defaultCNSharedParamsData :: CNSharedParamsData
defaultCNSharedParamsData = CNSharedParamsData
  { cnsp_Q10 = 1.5
  , cnsp_minpsi = -10.0
  , cnsp_maxpsi = -0.1
  , cnsp_rf_cwdl2 = 0.0
  , cnsp_tau_cwd = 0.0
  , cnsp_cwd_flig = 0.0
  , cnsp_froz_q10 = 1.5
  , cnsp_decomp_depth_efolding = 0.5
  , cnsp_mino2lim = 0.0
  , cnsp_organic_max = 0.0
  , cnsp_constrain_stress_deciduous_onset = False
  , cnsp_use_matrixcn = False
  , cnsp_use_fun = False
  , cnsp_nlev_soildecomp_standard = 5
  , cnsp_upper_soil_layer = -1
  }
