-- | Day length computation.
-- Fortran: DaylengthMod.F90
-- Julia:   src/biogeophys/daylength.jl
--
-- All functions are pure.
--
--   daylength        -- daylength for a single grid cell [seconds]
--   computeMaxDaylength -- max daylength for a latitude given obliquity
--
module CLM.BioGeoPhys.DayLength
  ( -- * Functions
    daylength
  , computeMaxDaylength
    -- * Constants
  , secsPerRadian
  , latEpsilon
  ) where

-- ========================================================================
-- Constants
-- ========================================================================

-- | Seconds per radian of hour-angle
secsPerRadian :: Double
secsPerRadian = 13750.9871

-- | Epsilon for latitudes near pole
latEpsilon :: Double
latEpsilon = 10.0 * (encodeFloat 1 (exponent (1.0 :: Double) - floatDigits (1.0 :: Double)))
-- NOTE: This is equivalent to 10 * eps(1.0) in Julia / machine epsilon * 10

-- | Half-pi (pole latitude)
pole :: Double
pole = pi / 2.0

-- | Offset from pole to avoid cos(lat) < 0
offsetPole :: Double
offsetPole = pole - latEpsilon

-- ========================================================================
-- Functions
-- ========================================================================

-- | Compute daylength in seconds for a single grid cell.
--
-- @lat@ and @decl@ are in radians.  Returns 'Nothing' when preconditions
-- are violated (|lat| >= pole + epsilon, or |decl| >= pole).
--
-- Ported from elemental function @daylength@ in @DaylengthMod.F90@.
daylength :: Double  -- ^ Latitude [radians]
          -> Double  -- ^ Solar declination [radians]
          -> Maybe Double
daylength lat decl
  | abs lat >= pole + latEpsilon = Nothing
  | abs decl >= pole             = Nothing
  | otherwise =
      let myLat = min offsetPole (max (-offsetPole) lat)
          temp0 = -(sin myLat * sin decl) / (cos myLat * cos decl)
          temp1 = min 1.0 (max (-1.0) temp0)
      in Just (2.0 * secsPerRadian * acos temp1)

-- | Compute maximum daylength for a given latitude and obliquity.
--
-- In the southern hemisphere the maximum declination is negative.
--
-- Ported from @ComputeMaxDaylength@ in @DaylengthMod.F90@.
computeMaxDaylength :: Double  -- ^ Latitude [radians]
                    -> Double  -- ^ Earth obliquity [radians]
                    -> Maybe Double
computeMaxDaylength lat obliquity =
  let maxDecl = if lat < 0.0 then -obliquity else obliquity
  in daylength lat maxDecl
