{-# LANGUAGE BangPatterns #-}
-- | Root biophysical properties: root fraction profiles.
-- Fortran: RootBiophysMod.F90
-- Julia:   src/biogeophys/root_biophys.jl
--
-- Computes root fraction distributions using three methods:
--   - Zeng (2001) two-parameter exponential profile
--   - Jackson (1996) beta-function profile
--   - Koven exponential profile
--
-- After computing the raw profile, roots below bedrock are redistributed
-- equally among the layers above bedrock.
--
-- All functions are pure.
module CLM.BioGeoPhys.RootBioPhys
  ( -- * Rooting profile methods
    RootingProfileMethod(..)
    -- * Root fraction computation
  , RootFrInput(..)
  , computeRootFr
    -- * Individual method functions
  , zeng2001RootFr
  , jackson1996RootFr
  , exponentialRootFr
    -- * Bedrock redistribution
  , redistributeAboveBedrock
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Rooting profile method
-- =========================================================================

-- | Rooting profile parameterization method.
data RootingProfileMethod
  = Zeng2001Root       -- ^ Zeng 2001 two-parameter exponential
  | Jackson1996Root    -- ^ Jackson 1996 beta-function
  | KovenExpRoot       -- ^ Koven exponential
  deriving (Show, Eq)

-- =========================================================================
-- Input record
-- =========================================================================

-- | Input for root fraction computation (single patch).
data RootFrInput = RootFrInput
  { rfi_method    :: !RootingProfileMethod
  , rfi_nlevsoi   :: !Int     -- ^ Number of hydrologically active soil layers
  , rfi_nlevgrnd  :: !Int     -- ^ Total ground layers (soil + bedrock)
  , rfi_nbedrock  :: !Int     -- ^ Bedrock top layer index (1-based)
  , rfi_is_fates  :: !Bool    -- ^ FATES flag
    -- PFT parameters (Zeng 2001)
  , rfi_roota_par :: !Double  -- ^ Root distribution parameter a [1/m]
  , rfi_rootb_par :: !Double  -- ^ Root distribution parameter b [1/m]
    -- PFT parameters (Jackson 1996)
  , rfi_rootprof_beta :: !Double  -- ^ Beta shape parameter
    -- Column geometry
  , rfi_col_zi    :: !(VU.Vector Double) -- ^ Interface depths [m] (nlevsoi, soil-only)
  , rfi_col_z     :: !(VU.Vector Double) -- ^ Layer center depths [m] (nlevsoi, soil-only)
  , rfi_col_dz    :: !(VU.Vector Double) -- ^ Layer thicknesses [m] (nlevsoi, soil-only)
  } deriving (Show)

-- =========================================================================
-- Top-level computation
-- =========================================================================

-- | Compute root fractions for a single patch.
-- Returns a vector of length nlevgrnd with root fractions.
computeRootFr :: RootFrInput -> VU.Vector Double
computeRootFr inp =
  let nlevsoi_v  = rfi_nlevsoi inp
      nlevgrnd_v = rfi_nlevgrnd inp
      nb         = rfi_nbedrock inp

      -- Compute raw profile
      rawProfile = case rfi_method inp of
        Zeng2001Root    -> zeng2001RootFr inp
        Jackson1996Root -> jackson1996RootFr inp
        KovenExpRoot    -> exponentialRootFr inp

      -- Zero out layers below nlevsoi
      profile1 = VU.generate nlevgrnd_v $ \j ->
        if j < nlevsoi_v then rawProfile VU.! j else 0.0

      -- Redistribute above bedrock
  in redistributeAboveBedrock profile1 nb nlevsoi_v

-- =========================================================================
-- Zeng 2001 root fraction
-- =========================================================================

-- | Zeng (2001) two-parameter exponential root profile.
-- rootfr(lev) = 0.5*(exp(-a*zi_upper) + exp(-b*zi_upper)
--                   - exp(-a*zi_lower) - exp(-b*zi_lower))
-- Bottom layer: rootfr(ubj) = 0.5*(exp(-a*zi_upper) + exp(-b*zi_upper))
zeng2001RootFr :: RootFrInput -> VU.Vector Double
zeng2001RootFr inp
  | rfi_is_fates inp = VU.replicate (rfi_nlevsoi inp) 0.0
  | otherwise =
      let a   = rfi_roota_par inp
          b   = rfi_rootb_par inp
          ubj = rfi_nlevsoi inp
          zi  = rfi_col_zi inp
      in VU.generate ubj $ \lev ->
           let zi_upper = if lev == 0 then 0.0 else zi VU.! (lev - 1)
           in if lev < ubj - 1
              then let zi_lower = zi VU.! lev
                   in 0.5 * (exp (-a * zi_upper) + exp (-b * zi_upper)
                           - exp (-a * zi_lower) - exp (-b * zi_lower))
              else -- Bottom layer
                   0.5 * (exp (-a * zi_upper) + exp (-b * zi_upper))

-- =========================================================================
-- Jackson 1996 root fraction
-- =========================================================================

-- | Jackson (1996) beta-function root profile.
-- rootfr(lev) = beta^(zi_upper*100) - beta^(zi_lower*100)
jackson1996RootFr :: RootFrInput -> VU.Vector Double
jackson1996RootFr inp
  | rfi_is_fates inp = VU.replicate (rfi_nlevsoi inp) 0.0
  | otherwise =
      let beta   = rfi_rootprof_beta inp
          ubj    = rfi_nlevsoi inp
          zi     = rfi_col_zi inp
          m2cm   = 100.0
      in VU.generate ubj $ \lev ->
           let zi_upper = if lev == 0 then 0.0 else zi VU.! (lev - 1)
               zi_lower = zi VU.! lev
           in beta ** (zi_upper * m2cm) - beta ** (zi_lower * m2cm)

-- =========================================================================
-- Koven exponential root fraction
-- =========================================================================

-- | Koven exponential root profile.
-- rootfr(lev) = exp(-rootprof_exp * z(lev)) * dz(lev) / norm
exponentialRootFr :: RootFrInput -> VU.Vector Double
exponentialRootFr inp
  | rfi_is_fates inp = VU.replicate (rfi_nlevsoi inp) 0.0
  | otherwise =
      let rootprofExp = 3.0  -- e-folding depth [1/m]
          ubj  = rfi_nlevsoi inp
          z    = rfi_col_z inp
          dz   = rfi_col_dz inp
          -- Raw profile
          raw  = VU.generate ubj $ \lev ->
                   exp (-rootprofExp * z VU.! lev) * (dz VU.! lev)
          -- Normalization factor
          norm = -1.0 / rootprofExp * (exp (-rootprofExp * z VU.! (ubj - 1)) - 1.0)
      in VU.map (/ norm) raw

-- =========================================================================
-- Bedrock redistribution
-- =========================================================================

-- | Redistribute root fractions above bedrock.
-- Roots below bedrock are summed and distributed equally among layers
-- above bedrock; layers below bedrock are zeroed.
redistributeAboveBedrock :: VU.Vector Double -> Int -> Int -> VU.Vector Double
redistributeAboveBedrock profile nb nlevsoi
  | nb >= nlevsoi = profile  -- bedrock is at or below bottom
  | nb <= 0       = profile  -- no redistribution possible
  | otherwise =
      let rootBelow = VU.sum $ VU.slice nb (nlevsoi - nb) profile
          addPerLayer = rootBelow / fromIntegral nb
      in VU.generate (VU.length profile) $ \j ->
           if j < nb
           then profile VU.! j + addPerLayer
           else if j < nlevsoi
                then 0.0
                else profile VU.! j  -- layers beyond nlevsoi unchanged
