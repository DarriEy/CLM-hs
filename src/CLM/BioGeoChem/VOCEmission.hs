{-# LANGUAGE BangPatterns #-}
-- | Volatile organic compound emission module (MEGAN v2.1).
-- Fortran: VOCEmissionMod.F90
-- Julia:   src/biogeochem/voc_emission.jl
--
-- Calculates VOC emissions following MEGAN v2.1:
--   E = epsilon * gamma * rho
-- where epsilon = baseline emission factors, gamma = activity factors
-- (light, temperature, leaf age, LAI, soil moisture, CO2).
module CLM.BioGeoChem.VOCEmission
  ( -- * Data types
    MEGANFactors(..)
  , MEGANCompound(..)
  , MEGANMechComp(..)
    -- * Activity factor functions
  , getGammaP
  , getGammaT
  , getGammaL
  , getGammaSM
  , getGammaA
  , getGammaC
    -- * Canopy environment
  , CanopyEnvInput(..)
  , CanopyEnvOutput(..)
  , calcCanopyEnvironment
    -- * Main VOC driver
  , VOCDriverInput(..)
  , VOCDriverOutput(..)
  , vocEmissionDriver
    -- * Single compound emission
  , calcCompoundEmission
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- MEGAN types
-- =========================================================================

-- | MEGAN compound class parameters indexed by class number.
-- Ported from @MEGANFactorsMod.F90@.
data MEGANFactors = MEGANFactors
  { mf_n_classes :: !Int
  , mf_Agro :: !(VU.Vector Double)  -- ^ growing leaves factor
  , mf_Amat :: !(VU.Vector Double)  -- ^ mature leaves factor
  , mf_Anew :: !(VU.Vector Double)  -- ^ new leaves factor
  , mf_Aold :: !(VU.Vector Double)  -- ^ old leaves factor
  , mf_betaT :: !(VU.Vector Double)  -- ^ beta for T-dependent LIF
  , mf_ct1  :: !(VU.Vector Double)
  , mf_ct2  :: !(VU.Vector Double)
  , mf_Ceo  :: !(VU.Vector Double)  -- ^ Ceo coefficient for Eopt
  , mf_LDF  :: !(VU.Vector Double)  -- ^ light-dependent fraction (0-1)
  } deriving (Show)

-- | Single MEGAN mega-compound descriptor.
data MEGANCompound = MEGANCompound
  { mc_name          :: !String
  , mc_index         :: !Int
  , mc_class_number  :: !Int
  , mc_molec_weight  :: !Double
  , mc_coeff         :: !Double
  , mc_emis_factors  :: !(VU.Vector Double)  -- ^ emission factors per PFT [ug m-2 h-1]
  } deriving (Show)

-- | Mechanism compound mapping.
data MEGANMechComp = MEGANMechComp
  { mmc_name          :: !String
  , mmc_n_megan_comps :: !Int
  , mmc_megan_indices :: !(VU.Vector Int)
  } deriving (Show)

-- =========================================================================
-- Activity factor functions (pure, single-patch)
-- =========================================================================

-- | Activity factor for PPFD (photosynthetically active photon flux density).
-- Equation 4 from Guenther et al. (2006).
getGammaP :: Double  -- ^ PAR (current) [umol/m2/s]
           -> Double  -- ^ PAR 24hr avg
           -> Double  -- ^ PAR 240hr avg
           -> Double  -- ^ LDF (light-dependent fraction)
           -> Double
getGammaP par par24 par240 ldf =
  let !cp  = 0.0468 * exp (0.0005 * par24) * par240 ** 0.6
      !ct1 = if cp > 1.0e-6 then (2.46 * par / cp) * (1.0 / sqrt (1.0 + (2.46 * par / cp) ** 2)) else 0.0
  in (1.0 - ldf) + ldf * ct1

-- | Activity factor for temperature.
-- Equation 5a from Guenther et al. (2006) for light-dependent compounds.
getGammaT :: Double  -- ^ temperature [K]
           -> Double  -- ^ T 24hr avg [K]
           -> Double  -- ^ T 240hr avg [K]
           -> Double  -- ^ Eopt coefficient (Ceo)
           -> Double  -- ^ ct1 coefficient
           -> Double  -- ^ ct2 coefficient
           -> Double  -- ^ betaT
           -> Double  -- ^ LDF
           -> Double
getGammaT t t24 t240 ceo ct1c ct2c betaT ldf =
  let !topt = 313.0 + (0.6 * (t240 - 297.0))
      !x    = ((1.0 / topt) - (1.0 / t)) / 0.00831
      !eopt = ceo * exp (0.05 * (t24 - 297.0)) * exp (0.05 * (t240 - 297.0))
      !gT_ld = eopt * ct2c * exp (ct1c * x) / (ct2c - ct1c * (1.0 - exp (ct2c * x)))
      !gT_li = exp (betaT * (t - 297.0))
  in ldf * gT_ld + (1.0 - ldf) * gT_li

-- | Activity factor for LAI.
getGammaL :: Double  -- ^ LAI
           -> Double
getGammaL lai = 0.49 * lai / sqrt (1.0 + 0.2 * lai * lai)

-- | Activity factor for soil moisture (CLM implementation).
getGammaSM :: Double  -- ^ soil wetness (theta/theta_sat)
            -> Double
getGammaSM sw
  | sw <= 0.0 = 0.0
  | sw >= 1.0 = 1.0
  | otherwise = sw

-- | Activity factor for leaf age.
-- Equation 10 from Guenther et al. (2006).
getGammaA :: Double  -- ^ fnew (fraction of new leaves)
           -> Double  -- ^ fgro (fraction of growing leaves)
           -> Double  -- ^ fmat (fraction of mature leaves)
           -> Double  -- ^ fold (fraction of old leaves)
           -> Double  -- ^ Anew
           -> Double  -- ^ Agro
           -> Double  -- ^ Amat
           -> Double  -- ^ Aold
           -> Double
getGammaA fnew fgro fmat fold anew agro amat aold =
  fnew * anew + fgro * agro + fmat * amat + fold * aold

-- | Activity factor for CO2 (isoprene only).
-- Equation from Heald et al. (2009).
getGammaC :: Double  -- ^ CO2 concentration [ppm]
           -> Double
getGammaC co2 =
  let !imax = 1.344
      !h    = 1.4614
      !cstar = 585.0
  in imax - ((imax * co2 ** h) / (cstar ** h + co2 ** h))

-- =========================================================================
-- Canopy environment model
-- =========================================================================

data CanopyEnvInput = CanopyEnvInput
  { cei_t_veg        :: !Double  -- ^ vegetation temperature (K)
  , cei_par_top      :: !Double  -- ^ above-canopy PAR (W/m2)
  , cei_lai          :: !Double  -- ^ leaf area index
  , cei_sai          :: !Double  -- ^ stem area index
  , cei_kb           :: !Double  -- ^ beam extinction coefficient
  , cei_ncan_layers  :: !Int     -- ^ number of canopy layers
  } deriving (Show)

data CanopyEnvOutput = CanopyEnvOutput
  { ceo_par_sun_layers :: ![Double]  -- ^ sunlit PAR per layer (umol/m2/s)
  , ceo_par_sha_layers :: ![Double]  -- ^ shaded PAR per layer
  , ceo_t_layers       :: ![Double]  -- ^ leaf temperature per layer (K)
  , ceo_fsun_layers    :: ![Double]  -- ^ sunlit fraction per layer
  } deriving (Show)

-- | Compute within-canopy light and temperature for VOC emissions.
-- Uses exponential light extinction and assumes small T gradient.
calcCanopyEnvironment :: CanopyEnvInput -> CanopyEnvOutput
calcCanopyEnvironment !inp =
  let !nlayers = cei_ncan_layers inp
      !lai = cei_lai inp
      !kb = cei_kb inp
      !par_top = cei_par_top inp * 4.6  -- W/m2 to umol/m2/s
      !dlai = if nlayers > 0 then lai / fromIntegral nlayers else 0.0

      layerData i =
        let !cum_lai = fromIntegral i * dlai + dlai * 0.5
            !beam_frac = exp (-kb * cum_lai)
            !par_beam = par_top * beam_frac
            !par_diff = par_top * 0.2 * exp (-0.5 * cum_lai)
            !fsun = exp (-kb * cum_lai)
            !par_sun = par_beam + par_diff
            !par_sha = par_diff
            !t_leaf = cei_t_veg inp - 0.5 * fromIntegral i
        in (par_sun, par_sha, t_leaf, fsun)

      !layers = map layerData [0..nlayers-1]
  in CanopyEnvOutput
     { ceo_par_sun_layers = map (\(p,_,_,_) -> p) layers
     , ceo_par_sha_layers = map (\(_,p,_,_) -> p) layers
     , ceo_t_layers = map (\(_,_,t,_) -> t) layers
     , ceo_fsun_layers = map (\(_,_,_,f) -> f) layers
     }

-- =========================================================================
-- Single compound emission
-- =========================================================================

-- | Calculate emission for a single MEGAN compound at a single patch.
-- E = epsilon * gamma_P * gamma_T * gamma_L * gamma_SM * gamma_A * gamma_C * rho
calcCompoundEmission :: Double  -- ^ epsilon (emission factor, ug/m2/hr)
                     -> Double  -- ^ gammaP (light activity factor)
                     -> Double  -- ^ gammaT (temperature activity factor)
                     -> Double  -- ^ gammaL (LAI activity factor)
                     -> Double  -- ^ gammaSM (soil moisture factor)
                     -> Double  -- ^ gammaA (leaf age factor)
                     -> Double  -- ^ gammaC (CO2 inhibition factor, isoprene only)
                     -> Double  -- ^ rho (canopy loss/production factor, ~0.96)
                     -> Double  -- ^ emission (ug/m2/hr)
calcCompoundEmission !eps !gP !gT !gL !gSM !gA !gC !rho =
  eps * gP * gT * gL * gSM * gA * gC * rho

-- =========================================================================
-- Main VOC emission driver
-- =========================================================================

data VOCDriverInput = VOCDriverInput
  { vdi_t_veg          :: !Double   -- ^ vegetation temperature (K)
  , vdi_t_veg24        :: !Double   -- ^ 24hr average T (K)
  , vdi_t_veg240       :: !Double   -- ^ 240hr average T (K)
  , vdi_par            :: !Double   -- ^ current PAR (umol/m2/s)
  , vdi_par24          :: !Double   -- ^ 24hr average PAR
  , vdi_par240         :: !Double   -- ^ 240hr average PAR
  , vdi_lai            :: !Double   -- ^ LAI
  , vdi_co2_ppm        :: !Double   -- ^ atmospheric CO2 (ppm)
  , vdi_soil_wetness   :: !Double   -- ^ soil wetness (theta/theta_sat)
  , vdi_fnew           :: !Double   -- ^ fraction of new leaves
  , vdi_fgro           :: !Double   -- ^ fraction of growing leaves
  , vdi_fmat           :: !Double   -- ^ fraction of mature leaves
  , vdi_fold           :: !Double   -- ^ fraction of old leaves
  -- Compound-specific parameters
  , vdi_epsilon        :: !Double   -- ^ emission factor (ug/m2/hr)
  , vdi_LDF            :: !Double   -- ^ light-dependent fraction
  , vdi_betaT          :: !Double   -- ^ beta for T-dependent emission
  , vdi_ct1            :: !Double   -- ^ ct1 coefficient
  , vdi_ct2            :: !Double   -- ^ ct2 coefficient
  , vdi_Ceo            :: !Double   -- ^ Ceo (Eopt scaling)
  , vdi_Anew           :: !Double
  , vdi_Agro           :: !Double
  , vdi_Amat           :: !Double
  , vdi_Aold           :: !Double
  , vdi_is_isoprene    :: !Bool     -- ^ apply CO2 inhibition?
  } deriving (Show)

data VOCDriverOutput = VOCDriverOutput
  { vdo_emission       :: !Double   -- ^ emission rate (ug/m2/hr)
  , vdo_gammaP         :: !Double
  , vdo_gammaT         :: !Double
  , vdo_gammaL         :: !Double
  , vdo_gammaSM        :: !Double
  , vdo_gammaA         :: !Double
  , vdo_gammaC         :: !Double
  } deriving (Show)

-- | Main VOC emission driver for a single compound at a single patch.
-- Combines all activity factors and computes total emission.
vocEmissionDriver :: VOCDriverInput -> VOCDriverOutput
vocEmissionDriver !inp =
  let -- Light activity factor
      !gP = getGammaP (vdi_par inp) (vdi_par24 inp) (vdi_par240 inp) (vdi_LDF inp)
      -- Temperature activity factor
      !gT = getGammaT (vdi_t_veg inp) (vdi_t_veg24 inp) (vdi_t_veg240 inp)
               (vdi_Ceo inp) (vdi_ct1 inp) (vdi_ct2 inp) (vdi_betaT inp) (vdi_LDF inp)
      -- LAI activity factor
      !gL = getGammaL (vdi_lai inp)
      -- Soil moisture
      !gSM = getGammaSM (vdi_soil_wetness inp)
      -- Leaf age
      !gA = getGammaA (vdi_fnew inp) (vdi_fgro inp) (vdi_fmat inp) (vdi_fold inp)
               (vdi_Anew inp) (vdi_Agro inp) (vdi_Amat inp) (vdi_Aold inp)
      -- CO2 inhibition (isoprene only)
      !gC = if vdi_is_isoprene inp then getGammaC (vdi_co2_ppm inp) else 1.0
      -- Canopy loss factor
      !rho = 0.96
      -- Total emission
      !emission = calcCompoundEmission (vdi_epsilon inp) gP gT gL gSM gA gC rho
  in VOCDriverOutput
     { vdo_emission = emission
     , vdo_gammaP = gP
     , vdo_gammaT = gT
     , vdo_gammaL = gL
     , vdo_gammaSM = gSM
     , vdo_gammaA = gA
     , vdo_gammaC = gC
     }
