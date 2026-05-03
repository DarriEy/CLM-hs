{-# LANGUAGE BangPatterns #-}
-- | Dry deposition velocity calculation using Wesely (1989) resistance model.
-- Fortran: DryDepVelocity.F90
--
-- Provides:
--   * Season determination from latitude and month
--   * PFT to Wesely land type mapping
--   * Quasi-laminar boundary layer resistance (R_b)
--   * Surface (canopy) resistance (R_c)
--   * Deposition velocity V_d = 1/(R_a + R_b + R_c)
--   * Wesely resistance table lookup
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.DryDepVelocity
  ( -- * Constants
    nLandType
  , dh2o
    -- * Types
  , DryDepSpecies(..)
  , DepVelInput(..)
    -- * Science functions
  , weselySeason
  , pftToWesely
  , calcRb
  , calcRc
  , depvelCompute
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (tfrz)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Number of Wesely land use types
nLandType :: Int
nLandType = 11

-- | Diffusivity of H2O in air [cm^2/s]
dh2o :: Double
dh2o = 0.25

-- | von Karman constant
vkc :: Double
vkc = 0.4

-- | Season indices
midsummerLushVeg, autumnUnharvested, lateAutumnLeafless, winterSnowIce, transitionalSpring :: Int
midsummerLushVeg = 1
autumnUnharvested = 2
lateAutumnLeafless = 3
winterSnowIce = 4
transitionalSpring = 5

--------------------------------------------------------------------------------
-- Wesely resistance tables (11 land types x 5 seasons)
--------------------------------------------------------------------------------

-- | Minimum stomatal resistance R_i [s/m]
riTable :: VU.Vector Double
riTable = VU.fromList
  [ 9999, 9999, 9999, 9999, 9999   -- 1: urban
  ,   60, 9999, 9999, 9999,  120   -- 2: agricultural
  ,  120,  200, 9999, 9999,  240   -- 3: range
  ,   70,  150, 9999, 9999,  130   -- 4: deciduous forest
  ,  130,  200, 9999, 9999,  250   -- 5: coniferous forest
  ,  100,  150, 9999, 9999,  190   -- 6: mixed forest
  , 9999, 9999, 9999, 9999, 9999   -- 7: water
  , 9999, 9999, 9999, 9999, 9999   -- 8: barren
  , 9999, 9999, 9999, 9999, 9999   -- 9: wetland
  ,   80,  100, 9999, 9999,  160   -- 10: mixed ag+range
  ,   60,  150, 9999, 9999,  120   -- 11: rocky
  ]

-- | Ground surface resistance for SO2 [s/m]
rgsSo2Table :: VU.Vector Double
rgsSo2Table = VU.fromList
  [ 400, 150, 350, 500, 200
  , 200, 150, 200, 100, 200
  , 350, 200, 200, 100, 300
  , 500, 200, 200, 100, 300
  , 500, 200, 200, 100, 300
  , 100, 100, 200, 100, 200
  ,   0,1000,   0,   0,   0
  ,1000, 400, 400, 400, 600
  ,   0,1000,   0,   0,   0
  , 300, 150, 200, 100, 200
  , 400, 200, 200, 100, 300
  ]

-- | Ground surface resistance for O3 [s/m]
rgsO3Table :: VU.Vector Double
rgsO3Table = VU.fromList
  [ 300, 150, 200, 200, 200
  , 200, 150, 200, 200, 200
  , 200, 200, 200, 200, 200
  , 200, 200, 200, 400, 200
  , 200, 200, 200, 400, 200
  , 200, 200, 200, 400, 200
  ,2000,1000,2000,2000,2000
  , 400, 200, 200, 200, 300
  ,2000,1000,2000,2000,2000
  , 200, 150, 200, 200, 200
  , 200, 200, 200, 200, 200
  ]

-- | Cuticular resistance R_lu [s/m]
rluTable :: VU.Vector Double
rluTable = VU.fromList
  [ 9999, 9999, 9999, 9999, 9999
  , 2000, 9000, 9999, 9999, 4000
  , 2000, 9000, 9999, 9999, 4000
  , 2000, 4000, 9999, 9999, 8000
  , 2000, 4000, 9999, 9999, 8000
  , 2000, 4000, 9999, 9999, 8000
  , 9999, 9999, 9999, 9999, 9999
  , 9999, 9999, 9999, 9999, 9999
  , 9999, 9999, 9999, 9999, 9999
  , 2000, 9000, 9999, 9999, 4000
  , 4000, 9000, 9999, 9999, 8000
  ]

-- | In-canopy aerodynamic resistance R_ac [s/m]
racTable :: VU.Vector Double
racTable = VU.fromList
  [  100,  100,  100,  100,  100
  ,  200,  150,  100,  100,  150
  ,  100,  100,  100,  100,  100
  , 2000, 1500, 1000, 1000, 1500
  , 2000, 1500, 1000, 1000, 1500
  , 2000, 1500, 1000, 1000, 1500
  ,    0,    0,    0,    0,    0
  ,    0,    0,    0,    0,    0
  ,    0,    0,    0,    0,    0
  ,  200,  150,  100,  100,  150
  ,  100,  100,  100,  100,  100
  ]

-- | Table lookup helper: index = (lt-1)*5 + (season-1)
tableLookup :: VU.Vector Double -> Int -> Int -> Double
tableLookup !tbl !lt !season = tbl VU.! ((lt - 1) * 5 + (season - 1))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Per-species dry deposition properties
data DryDepSpecies = DryDepSpecies
  { dds_foxd :: !Double  -- ^ reactivity factor [0-1]
  , dds_dv   :: !Double  -- ^ diffusivity in air [cm^2/s]
  , dds_heff :: !Double  -- ^ effective Henry's law constant [M/atm]
  } deriving (Show, Eq)

-- | Input for deposition velocity at a single patch
data DepVelInput = DepVelInput
  { dvi_lat        :: !Double  -- ^ latitude (degrees)
  , dvi_month      :: !Int     -- ^ current month (1-12)
  , dvi_pft_itype  :: !Int     -- ^ PFT type (0-based)
  , dvi_ram1       :: !Double  -- ^ aerodynamic resistance (s/m)
  , dvi_ustar      :: !Double  -- ^ friction velocity (m/s)
  , dvi_elai       :: !Double  -- ^ exposed LAI
  , dvi_t_sfc      :: !Double  -- ^ surface temperature (K)
  , dvi_solar_flux :: !Double  -- ^ incident solar (W/m2)
  , dvi_frac_sno   :: !Double  -- ^ snow fraction
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Determine Wesely season index (1-5) from latitude and month.
weselySeason :: Double -> Int -> Int
weselySeason !lat !month =
  let nhSeason = VU.fromList [4,4,5,5,1,1,1,1,2,3,3,4 :: Int]
      m' | lat >= 0.0 = month
         | otherwise   = ((month + 5 - 1) `mod` 12) + 1
  in nhSeason VU.! (m' - 1)

-- | Map CLM PFT type (0-based) to Wesely land use type (1-11).
pftToWesely :: Int -> Int
pftToWesely !itype
  | itype == 0  = 8   -- barren
  | itype <= 3  = 5   -- coniferous
  | itype <= 8  = 4   -- deciduous
  | itype <= 14 = 3   -- range
  | otherwise   = 2   -- agricultural

-- | Quasi-laminar boundary layer resistance R_b [s/m].
calcRb :: Double -> Double -> Double
calcRb !ustar !dv_species =
  let !ustar_safe = max ustar 0.001
      !sc_over_pr = dh2o / max dv_species 1.0e-10
  in (2.0 / (vkc * ustar_safe)) * sc_over_pr ** (2.0 / 3.0)

-- | Surface (canopy) resistance R_c [s/m].
calcRc :: Int -> Int -> Double -> Double -> Double -> Double -> Double -> Bool -> Double
calcRc !season !lt !_lai !foxd !heff !t_sfc !solar !has_snow =
  let !ri  = tableLookup riTable lt season
      !rlu = tableLookup rluTable lt season
      !rac = tableLookup racTable lt season
      !rgs_so2 = tableLookup rgsSo2Table lt season
      !rgs_o3  = tableLookup rgsO3Table lt season

      -- Stomatal resistance
      !rs | ri < 9999 && solar > 1.0 && t_sfc > tfrz - 5.0 =
            max ri (ri * (1.0 + (200.0 / (solar + 0.1)) ** 2) * (400.0 / (t_sfc - (tfrz - 40.0))))
          | otherwise = 1.0e10
      !rs' | t_sfc < tfrz = 1.0e10
           | otherwise = rs

      -- Mesophyll resistance
      !rm_denom = heff / 3000.0 + 100.0 * foxd
      !rm | rm_denom > 1.0e-10 = max (1.0 / rm_denom) 0.0
          | otherwise = 1.0e10

      -- Cuticular resistance
      !rlu_eff
        | rlu < 9999 =
          let !d = 1.0e-5 * heff + foxd
          in if d > 1.0e-10 then max (rlu / d) 1.0 else 1.0e10
        | otherwise = 1.0e10

      -- Ground surface resistance
      !rgs =
        let !d = heff / (1.0e5 * max rgs_so2 1.0e-10) + foxd / max rgs_o3 1.0e-10
        in if d > 1.0e-10 then max (1.0 / d) 1.0 else 1000.0

      !rgs' | has_snow = max rgs 500.0
            | otherwise = rgs

      !rac_eff = max rac 0.0

      -- Combine
      !r_stom | rs' < 1.0e9 = rs' + rm
              | otherwise = 1.0e10

      !r_upper | r_stom < 1.0e9 && rlu_eff < 1.0e9 = 1.0 / (1.0 / r_stom + 1.0 / rlu_eff)
               | r_stom < 1.0e9 = r_stom
               | rlu_eff < 1.0e9 = rlu_eff
               | otherwise = 1.0e10

      !r_lower = rac_eff + rgs'

      !rc | r_upper < 1.0e9 && r_lower > 0.0 = 1.0 / (1.0 / r_upper + 1.0 / r_lower)
          | r_upper < 1.0e9 = r_upper
          | r_lower > 0.0 = r_lower
          | otherwise = 1.0

  in max rc 1.0

-- | Calculate dry deposition velocity for a single species at a single patch.
-- Returns V_d in cm/s.
depvelCompute :: DepVelInput -> DryDepSpecies -> Double
depvelCompute !inp !sp =
  let !season = weselySeason (dvi_lat inp) (dvi_month inp)
      !lt     = pftToWesely (dvi_pft_itype inp)
      !ra     = max (dvi_ram1 inp) 1.0
      !ustar  = max (dvi_ustar inp) 0.001
      !rb     = calcRb ustar (dds_dv sp)
      !rc     = calcRc season lt (max (dvi_elai inp) 0.0)
                       (dds_foxd sp) (dds_heff sp)
                       (dvi_t_sfc inp) (dvi_solar_flux inp)
                       (dvi_frac_sno inp > 0.5)
      !vd     = 1.0 / (ra + rb + rc)
  in vd * 100.0  -- m/s -> cm/s
