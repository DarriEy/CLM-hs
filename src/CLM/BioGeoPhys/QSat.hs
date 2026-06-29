-- | Saturation vapor pressure and specific humidity calculations.
-- Uses Flatau et al. (1992) polynomial approximations.
-- Fortran: QSatMod.F90
-- Julia:   src/biogeophys/qsat.jl
--
-- All functions are pure.  The main entry points are:
--
--   qsat          -- (qs, es, dqsdT, desdT)
--   qsatNoDerivs  -- (qs, es)
--
module CLM.BioGeoPhys.QSat
  ( -- * Data types
    QSatResult(..)
  , QSatSimple(..)
    -- * Functions
  , qsat
  , qsatNoDerivs
    -- * Polynomial helpers (exposed for testing)
  , poly8
  ) where

import CLM.Constants.PhysicalConstants (tfrz)

-- ========================================================================
-- Flatau polynomial coefficients
-- ========================================================================

-- | Saturation vapor pressure over water (0-100 deg C), 9 coefficients
qsatA :: (Double, Double, Double, Double, Double, Double, Double, Double, Double)
qsatA = ( 6.11213476
         , 0.444007856
         , 0.143064234e-1
         , 0.264461437e-3
         , 0.305903558e-5
         , 0.196237241e-7
         , 0.892344772e-10
         , -0.373208410e-12
         , 0.209339997e-15
         )

-- | d(es)/d(T) over water
qsatB :: (Double, Double, Double, Double, Double, Double, Double, Double, Double)
qsatB = ( 0.444017302
         , 0.286064092e-1
         , 0.794683137e-3
         , 0.121211669e-4
         , 0.103354611e-6
         , 0.404125005e-9
         , -0.788037859e-12
         , -0.114596802e-13
         , 0.381294516e-16
         )

-- | Saturation vapor pressure over ice (-75 to 0 deg C)
qsatC :: (Double, Double, Double, Double, Double, Double, Double, Double, Double)
qsatC = ( 6.11123516
         , 0.503109514
         , 0.188369801e-1
         , 0.420547422e-3
         , 0.614396778e-5
         , 0.602780717e-7
         , 0.387940929e-9
         , 0.149436277e-11
         , 0.262655803e-14
         )

-- | d(es)/d(T) over ice
qsatD :: (Double, Double, Double, Double, Double, Double, Double, Double, Double)
qsatD = ( 0.503277922
         , 0.377289173e-1
         , 0.126801703e-2
         , 0.249468427e-4
         , 0.313703411e-6
         , 0.257180651e-8
         , 0.133268878e-10
         , 0.394116744e-13
         , 0.498070196e-16
         )

-- ========================================================================
-- Data types
-- ========================================================================

-- | Full result of saturation humidity calculation.
data QSatResult = QSatResult
  { qsr_qs    :: !Double  -- ^ Saturation specific humidity [kg/kg]
  , qsr_es    :: !Double  -- ^ Saturation vapor pressure [Pa]
  , qsr_dqsdT :: !Double  -- ^ d(qs)/d(T) [kg/kg/K]
  , qsr_desdT :: !Double  -- ^ d(es)/d(T) [Pa/K]
  } deriving (Show, Eq)

-- | Derivative-free result (qs and es only).  The full QSat with the
-- temperature derivatives (esdT, qsdT) is provided by 'qsat' / 'QSatResult';
-- this variant corresponds to calling Fortran QSat() without the optional
-- @qsdT@ / @esdT@ arguments.
data QSatSimple = QSatSimple
  { qss_qs :: !Double  -- ^ Saturation specific humidity [kg/kg]
  , qss_es :: !Double  -- ^ Saturation vapor pressure [Pa]
  } deriving (Show, Eq)

-- ========================================================================
-- Polynomial evaluation
-- ========================================================================

-- | Evaluate 8th-degree polynomial using Horner's method.
-- c0 + x*(c1 + x*(c2 + x*(c3 + x*(c4 + x*(c5 + x*(c6 + x*(c7 + x*c8)))))))
poly8 :: Double
      -> (Double, Double, Double, Double, Double, Double, Double, Double, Double)
      -> Double
poly8 x (c0, c1, c2, c3, c4, c5, c6, c7, c8) =
  c0 + x*(c1 + x*(c2 + x*(c3 + x*(c4 + x*(c5 + x*(c6 + x*(c7 + x*c8)))))))
{-# INLINE poly8 #-}

-- ========================================================================
-- Main functions
-- ========================================================================

-- | Compute saturation specific humidity, vapor pressure, and their
-- derivatives with respect to temperature.
--
-- @t@ is temperature [K], @p@ is pressure [Pa].
--
-- Ported from QSat() in QSatMod.F90.
qsat :: Double  -- ^ Temperature [K]
     -> Double  -- ^ Pressure [Pa]
     -> QSatResult
qsat t p =
  let td = min 100.0 (max (-75.0) (t - tfrz))  -- clamp to fit range (Fortran QSat)
      (es, desdT)
        | td >= 0.0 = (100.0 * poly8 td qsatA, 100.0 * poly8 td qsatB)
        | otherwise  = (100.0 * poly8 td qsatC, 100.0 * poly8 td qsatD)
      vp  = 1.0 / (p - 0.378 * es)
      vp1 = min (es * vp) 1.0
      qs  = 0.622 * vp1
      dqsdT = 0.622 * p * desdT * vp * vp
  in QSatResult { qsr_qs = qs, qsr_es = es, qsr_dqsdT = dqsdT, qsr_desdT = desdT }
{-# INLINE qsat #-}

-- | Compute saturation specific humidity and vapor pressure without
-- derivatives.  Slightly more efficient when derivatives are not needed.
qsatNoDerivs :: Double  -- ^ Temperature [K]
             -> Double  -- ^ Pressure [Pa]
             -> QSatSimple
qsatNoDerivs t p =
  let td = min 100.0 (max (-75.0) (t - tfrz))  -- clamp to fit range (Fortran QSat)
      es | td >= 0.0 = 100.0 * poly8 td qsatA
         | otherwise  = 100.0 * poly8 td qsatC
      vp1 = min (es / (p - 0.378 * es)) 1.0
      qs  = 0.622 * vp1
  in QSatSimple { qss_qs = qs, qss_es = es }
{-# INLINE qsatNoDerivs #-}
