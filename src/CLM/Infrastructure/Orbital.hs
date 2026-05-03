-- | Orbital mechanics for solar declination and earth-sun distance.
-- Fortran: shr_orb_mod.F90
-- Simplified Berger (1978) analytical orbital parameters.
module CLM.Infrastructure.Orbital
  ( computeOrbital
  , OrbitalParams(..)
  , defaultOrbitalParams
  ) where

-- | Orbital parameter set.
data OrbitalParams = OrbitalParams
  { orbEccen  :: !Double   -- ^ Eccentricity
  , orbObliqr :: !Double   -- ^ Obliquity [radians]
  , orbMvelpp :: !Double   -- ^ Moving vernal equinox longitude of perihelion [radians]
  , orbLambm0 :: !(Maybe Double)  -- ^ Mean longitude at vernal equinox (computed if Nothing)
  } deriving (Show)

-- | Default orbital parameters (year 1990 CE, Berger 1978).
defaultOrbitalParams :: OrbitalParams
defaultOrbitalParams = OrbitalParams
  { orbEccen  = 0.016724
  , orbObliqr = 23.4441 * pi / 180.0
  , orbMvelpp = (102.7 + 180.0) * pi / 180.0
  , orbLambm0 = Nothing
  }

-- | Compute solar declination angle and earth-sun distance factor
-- for a given calendar day using Berger (1978) orbital parameters.
--
-- @calday@: Calendar day (1.0 = Jan 1, fractional for sub-daily).
--
-- Returns (declin, eccf) where:
--   declin = solar declination angle [radians]
--   eccf   = earth-sun distance factor (1/r^2)
computeOrbital :: OrbitalParams -> Double -> (Double, Double)
computeOrbital params calday =
  let eccen  = orbEccen params
      obliqr = orbObliqr params
      mvelpp = orbMvelpp params

      dayspy = 365.0
      ve     = 80.5  -- vernal equinox calendar day

      -- Compute lambm0 if not provided
      beta = sqrt (1.0 - eccen * eccen)
      lambm0Computed = 2.0 *
        ( (0.5 * eccen + 0.125 * eccen ** 3) * (1.0 + beta) * sin mvelpp
        - 0.25 * eccen ** 2 * (0.5 + beta) * sin (2.0 * mvelpp)
        + 0.125 * eccen ** 3 * (1.0 / 3.0 + beta) * sin (3.0 * mvelpp)
        )

      lambm0 = case orbLambm0 params of
                 Just v  -> v
                 Nothing -> lambm0Computed

      -- Mean longitude at present day
      lambm = lambm0 + (calday - ve) * 2.0 * pi / dayspy
      lmm   = lambm - mvelpp

      -- True longitude (Berger 1978, equation of center)
      sinl = sin lmm
      lamb = lambm + eccen *
        ( 2.0 * sinl
        + eccen * (1.25 * sin (2.0 * lmm)
        + eccen * (13.0 / 12.0 * sin (3.0 * lmm) - 0.25 * sinl))
        )

      -- Inverse normalized sun-earth distance
      invrho = (1.0 + eccen * cos (lamb - mvelpp)) / (1.0 - eccen * eccen)

      -- Solar declination
      declin = asin (sin obliqr * sin lamb)

      -- Earth-sun distance factor
      eccf = invrho * invrho

  in (declin, eccf)
