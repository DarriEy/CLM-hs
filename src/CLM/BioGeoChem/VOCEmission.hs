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
