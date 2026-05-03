-- | Lake physics constants and initialization.
-- Fortran: LakeCon.F90
-- Julia:   src/biogeophys/lake_con.jl
--
-- Contains non-tuneable and tuneable constants, namelist-controlled
-- parameters, and the initialization function for lake physics.
-- Documented in Subin et al. 2011, JAMES.
--
module CLM.BioGeoPhys.LakeCon
  ( -- * Non-tuneable constants
    tdmax
  , emg_lake
  , z0frzlake
  , za_lake
  , cur0
  , cus
  , curm
  , n2min
    -- * Tuneable parameters record
  , LakeConParams(..)
  , defaultLakeConParams
    -- * Initialization (pure)
  , lakeConInit
  ) where

-- =========================================================================
-- Non-tuneable constants
-- =========================================================================

-- | Temperature of maximum water density [K]
-- From Hostetler and Bartlein (1990); more updated sources suggest 277.13 K.
tdmax :: Double
tdmax = 277.0

-- | Lake emissivity (used for both frozen and unfrozen lakes)
emg_lake :: Double
emg_lake = 0.97

-- | Momentum roughness length over frozen lakes without snow [m]
z0frzlake :: Double
z0frzlake = 0.001

-- | Base of surface light absorption layer for lakes [m]
za_lake :: Double
za_lake = 0.6

-- | Minimum Charnock parameter
cur0 :: Double
cur0 = 0.01

-- | Empirical constant for roughness under smooth flow
cus :: Double
cus = 0.1

-- | Maximum Charnock parameter
curm :: Double
curm = 0.1

-- | Minimum Brunt-Vaisala frequency squared for enhanced diffusivity [s^-2]
-- Yields diffusivity about 6 times molecular; from Fang & Stefan 1996
n2min :: Double
n2min = 7.5e-5

-- =========================================================================
-- Tuneable parameters (namelist-controlled and derived)
-- =========================================================================

-- | Mutable container for lake model parameters.
-- Fields correspond to Fortran module-level variables in LakeCon.F90.
data LakeConParams = LakeConParams
  { lcp_betavis               :: !Double  -- ^ Fraction of visible sunlight absorbed in ~1m of water
  , lcp_lsadz                 :: !Double  -- ^ Additional snow layer thickness for lake numerics [m]
  , lcp_lake_use_old_fcrit_minz0 :: !Bool -- ^ Use old fcrit & minz0 as per Subin et al 2011
  , lcp_deepmixing_depthcrit  :: !Double  -- ^ Minimum lake depth to invoke deep mixing [m]
  , lcp_deepmixing_mixfact    :: !Double  -- ^ Factor to increase mixing for deep lakes
  , lcp_lake_no_ed            :: !Bool    -- ^ Suppress enhanced diffusion
  , lcp_lakepuddling          :: !Bool    -- ^ Suppress convection when ice present
  , lcp_lake_puddle_thick     :: !Double  -- ^ Min total ice nominal thickness for puddling [m]
    -- Derived parameters (set by lakeConInit)
  , lcp_fcrit                 :: !Double  -- ^ Critical dimensionless fetch for Charnock parameter
  , lcp_minz0lake             :: !Double  -- ^ Minimum allowed roughness length for unfrozen lakes [m]
  , lcp_pudz                  :: !Double  -- ^ Min total ice thickness for lake puddling [m]
  , lcp_depthcrit             :: !Double  -- ^ Depth beneath which to increase mixing [m]
  , lcp_mixfact               :: !Double  -- ^ Mixing increase factor for deep lakes
  } deriving (Show, Eq)

-- | Default lake parameters (before initialization).
defaultLakeConParams :: LakeConParams
defaultLakeConParams = LakeConParams
  { lcp_betavis               = 0.0
  , lcp_lsadz                 = 0.03
  , lcp_lake_use_old_fcrit_minz0 = False
  , lcp_deepmixing_depthcrit  = 25.0
  , lcp_deepmixing_mixfact    = 10.0
  , lcp_lake_no_ed            = False
  , lcp_lakepuddling          = False
  , lcp_lake_puddle_thick     = 0.2
  , lcp_fcrit                 = 0.0 / 0.0  -- NaN before init
  , lcp_minz0lake             = 0.0 / 0.0
  , lcp_pudz                  = 0.0 / 0.0
  , lcp_depthcrit             = 0.0 / 0.0
  , lcp_mixfact               = 0.0 / 0.0
  }

-- | Initialize time-invariant lake variables (pure).
-- Sets fcrit, minz0lake, pudz, depthcrit, and mixfact from namelist fields.
-- Ported from LakeConInit in LakeCon.F90.
lakeConInit :: LakeConParams -> LakeConParams
lakeConInit p = p
  { lcp_fcrit     = fcrit'
  , lcp_minz0lake = minz0'
  , lcp_pudz      = pudz'
  , lcp_depthcrit = lcp_deepmixing_depthcrit p
  , lcp_mixfact   = lcp_deepmixing_mixfact p
  }
  where
    (fcrit', minz0')
      | lcp_lake_use_old_fcrit_minz0 p = (22.0,  1.0e-5)
      | otherwise                      = (100.0, 1.0e-10)

    pudz'
      | lcp_lakepuddling p = lcp_lake_puddle_thick p
      | otherwise          = 0.0
