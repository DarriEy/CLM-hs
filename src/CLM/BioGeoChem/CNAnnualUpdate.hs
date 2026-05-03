{-# LANGUAGE BangPatterns #-}
-- | Annual summation variable update.
-- Fortran: CNAnnualUpdateMod.F90
-- Julia:   src/biogeochem/cn_annual_update.jl
--
-- On the radiation time step, update annual summation variables.
-- When the counter reaches seconds-per-year, promote temp accumulators
-- to annual values and reset.
module CLM.BioGeoChem.CNAnnualUpdate
  ( -- * Types
    AnnualCounterInput(..)
  , AnnualCounterOutput(..)
  , annualCounterUpdate
  , AnnualPatchInput(..)
  , AnnualPatchOutput(..)
  , annualPatchUpdate
  ) where

-- =========================================================================
-- Column-level annual counter
-- =========================================================================

data AnnualCounterInput = AnnualCounterInput
  { aci_counter     :: !Double  -- ^ current annual sum counter [s]
  , aci_dt          :: !Double  -- ^ timestep [s]
  , aci_secspyear   :: !Double  -- ^ seconds per year
  } deriving (Show)

data AnnualCounterOutput = AnnualCounterOutput
  { aco_counter      :: !Double  -- ^ updated counter
  , aco_end_of_year  :: !Bool    -- ^ true if year boundary was crossed
  } deriving (Show)

-- | Increment the annual counter and detect end-of-year.
annualCounterUpdate :: AnnualCounterInput -> AnnualCounterOutput
annualCounterUpdate inp =
  let !new = aci_counter inp + aci_dt inp
  in if new >= aci_secspyear inp
     then AnnualCounterOutput { aco_counter = 0.0, aco_end_of_year = True }
     else AnnualCounterOutput { aco_counter = new, aco_end_of_year = False }

-- =========================================================================
-- Patch-level annual update (single patch)
-- =========================================================================

data AnnualPatchInput = AnnualPatchInput
  { api_tempsum_potential_gpp :: !Double
  , api_tempmax_retransn      :: !Double
  , api_tempavg_t2m           :: !Double
  , api_tempsum_npp           :: !Double  -- ^ temporary NPP accumulator [gC/m2/s]
  , api_tempsum_litfall       :: !Double
  , api_dt                    :: !Double  -- ^ timestep for conversion to annual total
  } deriving (Show)

data AnnualPatchOutput = AnnualPatchOutput
  { apo_annsum_potential_gpp :: !Double
  , apo_annmax_retransn      :: !Double
  , apo_annavg_t2m           :: !Double
  , apo_annsum_npp           :: !Double  -- ^ annual NPP [gC/m2/yr]
  , apo_annsum_litfall       :: !Double
  } deriving (Show)

-- | Promote temporary accumulators to annual values at end-of-year.
annualPatchUpdate :: AnnualPatchInput -> AnnualPatchOutput
annualPatchUpdate inp = AnnualPatchOutput
  { apo_annsum_potential_gpp = api_tempsum_potential_gpp inp
  , apo_annmax_retransn      = api_tempmax_retransn inp
  , apo_annavg_t2m           = api_tempavg_t2m inp
  , apo_annsum_npp           = api_tempsum_npp inp * api_dt inp
  , apo_annsum_litfall       = api_tempsum_litfall inp * api_dt inp
  }
