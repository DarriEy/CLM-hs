{-# LANGUAGE BangPatterns #-}
-- | Soil Water Retention Curve (SWRC) implementations.
-- Fortran: SoilWaterRetentionCurveMod.F90
--          SoilWaterRetentionCurveClappHornberg1978Mod.F90
--          SoilWaterRetentionCurveVanGenuchten1980Mod.F90
--
-- Provides:
--   * Abstract SWRC interface (via sum type)
--   * Clapp-Hornberger (1978) hydraulic conductivity and soil suction
--   * Van Genuchten (1980) hydraulic conductivity and soil suction
--   * Inverse suction (saturation from matric potential)
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SWRC
  ( -- * Types
    SWRCMethod(..)
  , SoilProps(..)
  , HkResult(..)
  , SmpResult(..)
    -- * Science functions
  , soilHk
  , soilSuction
  , soilSuctionInverse
  ) where

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Soil water retention curve method (sum type replacing abstract class)
data SWRCMethod
  = ClappHornberg1978
  | VanGenuchten1980
  deriving (Show, Eq)

-- | Soil properties for a single column and layer
data SoilProps = SoilProps
  { sp_hksat  :: !Double  -- ^ saturated hydraulic conductivity [mm/s]
  , sp_bsw    :: !Double  -- ^ Clapp-Hornberger "b" parameter [-]
  , sp_sucsat :: !Double  -- ^ saturated soil suction [mm]
  } deriving (Show, Eq)

-- | Hydraulic conductivity result
data HkResult = HkResult
  { hk_val   :: !Double  -- ^ hydraulic conductivity [mm/s]
  , hk_dhkds :: !Double  -- ^ d(hk)/ds [mm/s]
  } deriving (Show, Eq)

-- | Soil suction result
data SmpResult = SmpResult
  { smp_val    :: !Double  -- ^ soil suction [mm] (negative)
  , smp_dsmpds :: !Double  -- ^ d(smp)/ds [mm]
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Clapp-Hornberger 1978
--------------------------------------------------------------------------------

-- | Hydraulic conductivity: hk = imped * hksat * s^(2*bsw + 3).
soilHkCH :: SoilProps -> Double -> Double -> HkResult
soilHkCH !props !s !imped =
  let !hksat = sp_hksat props
      !bsw   = sp_bsw props
      !hk    = imped * hksat * s ** (2.0 * bsw + 3.0)
      !dhkds = (2.0 * bsw + 3.0) * hk / s
  in HkResult hk dhkds

-- | Soil suction: smp = -sucsat * s^(-bsw).
soilSuctionCH :: SoilProps -> Double -> SmpResult
soilSuctionCH !props !s =
  let !bsw    = sp_bsw props
      !sucsat = sp_sucsat props
      !smp    = negate sucsat * s ** (negate bsw)
      !dsmpds = negate bsw * smp / s
  in SmpResult smp dsmpds

-- | Inverse suction: s = (-smp/sucsat)^(-1/bsw).
soilSuctionInverseCH :: SoilProps -> Double -> Double
soilSuctionInverseCH !props !smp_target =
  let !bsw    = sp_bsw props
      !sucsat = sp_sucsat props
  in (negate smp_target / sucsat) ** (negate 1.0 / bsw)

--------------------------------------------------------------------------------
-- Van Genuchten 1980
-- Faithful to Fortran SoilWaterRetentionCurveVanGenuchten1980Mod.F90: in CLM/CTSM
-- that module's soil_hk / soil_suction / soil_suction_inverse routines are
-- identical, line for line, to the Clapp-Hornberger 1978 module (its own header
-- states it is "Implementation ... using the Clapp-Hornberg 1978
-- parameterizations"). The genuine van Genuchten (1980) equations are not
-- present in the reference Fortran, so reusing the CH78 implementations here is
-- the correct port. Should CLM ever add the true VG curves, replace these.
--------------------------------------------------------------------------------

soilHkVG :: SoilProps -> Double -> Double -> HkResult
soilHkVG = soilHkCH

soilSuctionVG :: SoilProps -> Double -> SmpResult
soilSuctionVG = soilSuctionCH

soilSuctionInverseVG :: SoilProps -> Double -> Double
soilSuctionInverseVG = soilSuctionInverseCH

--------------------------------------------------------------------------------
-- Dispatching functions
--------------------------------------------------------------------------------

-- | Compute hydraulic conductivity for the given SWRC method.
soilHk :: SWRCMethod -> SoilProps -> Double -> Double -> HkResult
soilHk !method !props !s !imped = case method of
  ClappHornberg1978 -> soilHkCH props s imped
  VanGenuchten1980  -> soilHkVG props s imped

-- | Compute soil suction potential for the given SWRC method.
soilSuction :: SWRCMethod -> SoilProps -> Double -> SmpResult
soilSuction !method !props !s = case method of
  ClappHornberg1978 -> soilSuctionCH props s
  VanGenuchten1980  -> soilSuctionVG props s

-- | Compute relative saturation from target suction for the given SWRC method.
soilSuctionInverse :: SWRCMethod -> SoilProps -> Double -> Double
soilSuctionInverse !method !props !smp_target = case method of
  ClappHornberg1978 -> soilSuctionInverseCH props smp_target
  VanGenuchten1980  -> soilSuctionInverseVG props smp_target
