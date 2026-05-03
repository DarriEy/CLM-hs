{-# LANGUAGE BangPatterns #-}
-- | Resolve plant/heterotroph competition for mineral N.
-- Fortran: SoilBiogeochemCompetitionMod.F90
-- Julia:   src/biogeochem/decomp_competition.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompCompetition
  ( -- * Data types
    CompetitionParams(..)
  , defaultCompetitionParams
  , CompetitionState(..)
  , defaultCompetitionState
    -- * Initialization
  , competitionInit
    -- * Main computation
  , CompetitionInput(..)
  , CompetitionOutput(..)
  , soilBGCCompetition
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Competition parameters.
data CompetitionParams = CompetitionParams
  { cp_bdnr              :: !Double  -- ^ bulk denitrification rate (1/s)
  , cp_compet_plant_no3  :: !Double
  , cp_compet_plant_nh4  :: !Double
  , cp_compet_decomp_no3 :: !Double
  , cp_compet_decomp_nh4 :: !Double
  , cp_compet_denit      :: !Double
  , cp_compet_nit        :: !Double
  } deriving (Show, Eq)

defaultCompetitionParams :: CompetitionParams
defaultCompetitionParams = CompetitionParams
  { cp_bdnr = 0.5, cp_compet_plant_no3 = 1.0, cp_compet_plant_nh4 = 1.0
  , cp_compet_decomp_no3 = 1.0, cp_compet_decomp_nh4 = 1.0
  , cp_compet_denit = 1.0, cp_compet_nit = 1.0
  }

-- | Competition module state.
data CompetitionState = CompetitionState
  { cs_dt          :: !Double  -- ^ decomp timestep (seconds)
  , cs_bdnr        :: !Double  -- ^ bulk denitrification rate scaled to timestep
  , cs_carbon_only :: !Bool    -- ^ supplement N to eliminate limitation
  } deriving (Show, Eq)

defaultCompetitionState :: CompetitionState
defaultCompetitionState = CompetitionState
  { cs_dt = 1800.0, cs_bdnr = 0.0, cs_carbon_only = False }

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | Initialize competition state.
competitionInit :: CompetitionParams -> Double -> Bool -> CompetitionState
competitionInit params dt carbonOnly = CompetitionState
  { cs_dt = dt
  , cs_bdnr = cp_bdnr params * (dt / secspday)
  , cs_carbon_only = carbonOnly
  }

-- | Input to competition routine.
data CompetitionInput = CompetitionInput
  { ci_nc                :: !Int
  , ci_nlevdecomp        :: !Int
  , ci_mask              :: !(VU.Vector Bool)
  , ci_dzsoi_decomp      :: !(VU.Vector Double)
  , ci_sminn_vr          :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , ci_potential_immob_vr :: !(VU.Vector Double)
  , ci_plant_ndemand     :: !(VU.Vector Double)  -- ^ (nc)
  , ci_nfixation_prof    :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , ci_use_nitrif_denitrif :: !Bool
  , ci_state             :: !CompetitionState
  , ci_params            :: !CompetitionParams
  } deriving (Show)

-- | Output of competition routine.
data CompetitionOutput = CompetitionOutput
  { co_fpi_vr            :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , co_fpg               :: !(VU.Vector Double)  -- ^ (nc)
  , co_fpi               :: !(VU.Vector Double)  -- ^ (nc)
  , co_actual_immob_vr   :: !(VU.Vector Double)
  , co_sminn_to_plant_vr :: !(VU.Vector Double)
  , co_sminn_to_plant    :: !(VU.Vector Double)
  , co_actual_immob      :: !(VU.Vector Double)
  } deriving (Show)

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Main competition routine (non-nitrif_denitrif pathway only).
soilBGCCompetition :: CompetitionInput -> CompetitionOutput
soilBGCCompetition inp =
  let !nc   = ci_nc inp
      !nlev = ci_nlevdecomp inp
      mask  = ci_mask inp
      dzsoi = ci_dzsoi_decomp inp
      sminn = ci_sminn_vr inp
      potImmob = ci_potential_immob_vr inp
      pDemand = ci_plant_ndemand inp
      nfixProf = ci_nfixation_prof inp
      dt    = cs_dt (ci_state inp)
      carbonOnly = cs_carbon_only (ci_state inp)
      bdnr  = cs_bdnr (ci_state inp)

      size2 = nc * nlev

      -- Total mineral N per column
      sminn_tot = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ sminn VU.! idx2 nc c j * dzsoi VU.! j | j <- [0..nlev-1] ]

      -- N uptake profile
      nuptake_prof = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else if sminn_tot VU.! c > 0.0
                then sminn VU.! ix / sminn_tot VU.! c
                else nfixProf VU.! ix

      -- Total N demand at each level
      sum_ndemand = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else pDemand VU.! c * nuptake_prof VU.! ix + potImmob VU.! ix

      -- Resolve competition
      fpi_vr_out = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else if sum_ndemand VU.! ix * dt < sminn VU.! ix then 1.0
           else if carbonOnly then 1.0
           else if sum_ndemand VU.! ix > 0.0
                then let ai = (sminn VU.! ix / dt) * (potImmob VU.! ix / sum_ndemand VU.! ix)
                     in if potImmob VU.! ix > 0.0 then ai / potImmob VU.! ix else 0.0
                else 0.0

      actual_immob_out = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else if sum_ndemand VU.! ix * dt < sminn VU.! ix then potImmob VU.! ix
           else if carbonOnly then potImmob VU.! ix
           else if sum_ndemand VU.! ix > 0.0
                then (sminn VU.! ix / dt) * (potImmob VU.! ix / sum_ndemand VU.! ix)
                else 0.0

      sminn_to_plant_out = VU.generate size2 $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else if sum_ndemand VU.! ix * dt < sminn VU.! ix
                then pDemand VU.! c * nuptake_prof VU.! ix
                else if carbonOnly then pDemand VU.! c * nuptake_prof VU.! ix
                else (sminn VU.! ix / dt) - actual_immob_out VU.! ix

      -- Column sums
      sminn_to_plant_col = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ sminn_to_plant_out VU.! idx2 nc c j * dzsoi VU.! j | j <- [0..nlev-1] ]

      actual_immob_col = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ actual_immob_out VU.! idx2 nc c j * dzsoi VU.! j | j <- [0..nlev-1] ]

      potential_immob_col = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ potImmob VU.! idx2 nc c j * dzsoi VU.! j | j <- [0..nlev-1] ]

      fpg_out = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else if pDemand VU.! c > 0.0
             then sminn_to_plant_col VU.! c / pDemand VU.! c
             else 1.0

      fpi_out = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else if potential_immob_col VU.! c > 0.0
             then actual_immob_col VU.! c / potential_immob_col VU.! c
             else 1.0

  in CompetitionOutput
    { co_fpi_vr = fpi_vr_out
    , co_fpg = fpg_out
    , co_fpi = fpi_out
    , co_actual_immob_vr = actual_immob_out
    , co_sminn_to_plant_vr = sminn_to_plant_out
    , co_sminn_to_plant = sminn_to_plant_col
    , co_actual_immob = actual_immob_col
    }
