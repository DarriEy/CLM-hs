{-# LANGUAGE BangPatterns #-}
-- | Carbon Isotope Tracer Transport (C13/C14).
--
-- Provides C14 radioactive decay, C13/C14 fractionation during
-- photosynthesis, and isotope ratio conversion utilities.
--
-- Ported from:
--   CNC14DecayMod.F90          -- C14 radioactive decay
--   PhotosynthesisMod.F90      -- C13/C14 fractionation during photosynthesis
--   SpeciesIsotopeType.F90     -- isotope ratio type
--   SpeciesNonIsotopeType.F90  -- C12 version
--
-- Julia: src/biogeochem/carbon_isotopes.jl
--
-- All functions are pure.
--
module CLM.BioGeoChem.CarbonIsotopes
  ( -- * Constants
    c14HalfLifeYears
  , c13PdbRatio
  , c14AtmRatioPrebomb
    -- * Conversion utilities
  , delta13CToRatio
  , ratioToDelta13C
    -- * Atmospheric C14 factor
  , c14BombFactor
    -- * C13/C14 photosynthesis
  , C13C14PsnResult(..)
  , c13C14Photosynthesis
    -- * C14 radioactive decay
  , c14DecayFactor
  , c14DecayPool
  , c14DecaySoilPool
  ) where

-- ========================================================================
-- Physical constants
-- ========================================================================

-- | C14 half-life in years
c14HalfLifeYears :: Double
c14HalfLifeYears = 5730.0

-- | PDB standard ratio for C13/C12 (Vienna PDB)
c13PdbRatio :: Double
c13PdbRatio = 0.0112372

-- | Standard atmospheric C14/C ratio (pre-bomb, ~1950)
c14AtmRatioPrebomb :: Double
c14AtmRatioPrebomb = 1.0e-12

-- | Seconds per day
secspday :: Double
secspday = 86400.0

-- ========================================================================
-- Conversion utilities
-- ========================================================================

-- | Convert delta-13C (per mil, VPDB) to the mass ratio C13/(C12+C13).
--
-- delta = (R_sample/R_standard - 1) * 1000 where R = C13/C12
-- Returns C13/(C12+C13) = R/(1+R).
delta13CToRatio :: Double -> Double
delta13CToRatio !delta13C =
  let !r = c13PdbRatio * (1.0 + delta13C / 1000.0)
  in r / (1.0 + r)

-- | Convert C13/(C12+C13) mass ratio to delta-13C (per mil, VPDB).
ratioToDelta13C :: Double -> Double
ratioToDelta13C !ratio
  | ratio <= 0.0 || ratio >= 1.0 = 0.0
  | otherwise =
      let !r = ratio / (1.0 - ratio)
      in (r / c13PdbRatio - 1.0) * 1000.0

-- ========================================================================
-- Atmospheric C14 factor
-- ========================================================================

-- | Get atmospheric C14/C ratio by latitude sector.
--
-- Latitude sectors (Fortran convention):
--   sector 1: lat >= 30N
--   sector 2: -30 <= lat < 30
--   sector 3: lat < -30S
--
-- In the simple (non-timeseries) case, rc14_atm = [1,1,1].
c14BombFactor
  :: Double               -- latitude [degrees]
  -> (Double, Double, Double) -- rc14_atm (sector 1, 2, 3)
  -> Double
c14BombFactor !latdeg (!rc1, !rc2, !rc3)
  | latdeg >= 30.0  = rc1
  | latdeg >= -30.0 = rc2
  | otherwise       = rc3

-- ========================================================================
-- C13/C14 photosynthesis
-- ========================================================================

-- | Result of C13/C14 photosynthesis calculation for a single patch.
data C13C14PsnResult = C13C14PsnResult
  { cppr_rc13_canair  :: !Double   -- ^ C13 canopy air ratio
  , cppr_rc13_psnsun  :: !Double   -- ^ C13 sunlit psn ratio
  , cppr_rc13_psnsha  :: !Double   -- ^ C13 shaded psn ratio
  , cppr_c13_psnsun   :: !Double   -- ^ C13 sunlit photosynthesis [umol/m2/s]
  , cppr_c13_psnsha   :: !Double   -- ^ C13 shaded photosynthesis [umol/m2/s]
  , cppr_c14_psnsun   :: !Double   -- ^ C14 sunlit photosynthesis [umol/m2/s]
  , cppr_c14_psnsha   :: !Double   -- ^ C14 shaded photosynthesis [umol/m2/s]
  } deriving (Show, Eq)

-- | Compute C13 and C14 photosynthesis fluxes for a single patch. Pure.
--
-- For C13:
--   rc13_canair = forc_pc13o2 / (forc_pco2 - forc_pc13o2)
--   rc13_psnsun = rc13_canair / alphapsnsun
--   c13_psnsun  = psnsun * rc13_psnsun / (1 + rc13_psnsun)
--
-- For C14:
--   c14_psnsun = rc14_atm(sector) * psnsun
c13C14Photosynthesis
  :: Bool                    -- use_c13
  -> Bool                    -- use_c14
  -> Bool                    -- use_c13_timeseries
  -> Double                  -- rc13_atm (for timeseries mode)
  -> (Double, Double, Double) -- rc14_atm (sector 1, 2, 3)
  -> Double                  -- forc_pco2_grc
  -> Double                  -- forc_pc13o2_grc
  -> Double                  -- latdeg_grc
  -> Double                  -- alphapsnsun_patch
  -> Double                  -- alphapsnsha_patch
  -> Double                  -- psnsun_patch
  -> Double                  -- psnsha_patch
  -> C13C14PsnResult
c13C14Photosynthesis !useC13 !useC14 !useC13TS !rc13Atm !rc14Atm
  !pco2 !pc13o2 !latdeg !alphaSun !alphaSha !psnsun !psnsha =
  let
    -- C13 fractionation
    (rc13Canair, rc13Sun, rc13Sha, c13Sun, c13Sha)
      | not useC13 = (0.0, 0.0, 0.0, 0.0, 0.0)
      | otherwise =
          let ca = if useC13TS
                   then rc13Atm
                   else if pco2 > pc13o2 && pc13o2 > 0.0
                        then pc13o2 / (pco2 - pc13o2)
                        else 0.0
              rSun = if alphaSun /= 0.0 then ca / alphaSun else 0.0
              rSha = if alphaSha /= 0.0 then ca / alphaSha else 0.0
              s = psnsun * (rSun / (1.0 + rSun))
              h = psnsha * (rSha / (1.0 + rSha))
          in (ca, rSun, rSha, s, h)

    -- C14 photosynthesis
    (c14Sun, c14Sha)
      | not useC14 = (0.0, 0.0)
      | otherwise =
          let rc14 = c14BombFactor latdeg rc14Atm
          in (rc14 * psnsun, rc14 * psnsha)

  in C13C14PsnResult
       { cppr_rc13_canair = rc13Canair
       , cppr_rc13_psnsun = rc13Sun
       , cppr_rc13_psnsha = rc13Sha
       , cppr_c13_psnsun  = c13Sun
       , cppr_c13_psnsha  = c13Sha
       , cppr_c14_psnsun  = c14Sun
       , cppr_c14_psnsha  = c14Sha
       }

-- ========================================================================
-- C14 radioactive decay
-- ========================================================================

-- | Compute C14 decay factor for a given timestep.
--
-- half_life_seconds = 5730 * 86400 * days_per_year
-- decay_const = -ln(0.5) / half_life_seconds
-- decay_factor = 1 - decay_const * dt
c14DecayFactor
  :: Double    -- dt [seconds]
  -> Double    -- days_per_year
  -> Double    -- decay_factor
c14DecayFactor !dt !daysPerYear =
  let !halfLife = c14HalfLifeYears * secspday * daysPerYear
      !decayConst = negate (log 0.5) / halfLife
  in 1.0 - decayConst * dt

-- | Apply decay factor to a single C14 pool value. Pure.
c14DecayPool :: Double -> Double -> Double
c14DecayPool !decayFactor !poolValue = poolValue * decayFactor

-- | Apply decay to a soil decomposition pool element, with optional
-- spinup acceleration.
--
-- If spinup: pool *= (1 - decay_const * spinup_factor * dt)
-- Otherwise: pool *= decay_factor
c14DecaySoilPool
  :: Double    -- dt [seconds]
  -> Double    -- days_per_year
  -> Double    -- spinup_factor (1.0 if not in spinup)
  -> Double    -- latitude_term (1.0 if spinup_factor == 1)
  -> Double    -- pool_value
  -> Double
c14DecaySoilPool !dt !daysPerYear !spinupFactor !latTerm !poolValue =
  let !halfLife = c14HalfLifeYears * secspday * daysPerYear
      !decayConst = negate (log 0.5) / halfLife
      !spTerm = if abs (spinupFactor - 1.0) > 1.0e-6
                then spinupFactor * latTerm
                else spinupFactor
  in poolValue * (1.0 - decayConst * spTerm * dt)
