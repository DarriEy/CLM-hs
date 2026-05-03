{-# LANGUAGE BangPatterns #-}
-- | Carbon/nitrogen mass balance checking.
-- Fortran: CNBalanceCheckMod.F90
-- Julia:   src/biogeochem/cn_balance_check.jl
--
-- Pure functions for computing mass balance errors at column and gridcell
-- levels for both carbon and nitrogen.
module CLM.BioGeoChem.CNBalanceCheck
  ( -- * Data types
    CNBalanceThresholds(..)
  , defaultBalanceThresholds
    -- * Column-level balance
  , CBalanceColInput(..)
  , CBalanceColOutput(..)
  , cBalanceCheckCol
  , NBalanceColInput(..)
  , NBalanceColOutput(..)
  , nBalanceCheckCol
    -- * Gridcell-level balance
  , CBalanceGrcInput(..)
  , CBalanceGrcOutput(..)
  , cBalanceCheckGrc
  ) where

-- =========================================================================
-- Thresholds
-- =========================================================================

data CNBalanceThresholds = CNBalanceThresholds
  { bth_cwarning :: !Double  -- ^ (gC/m2) carbon balance warning threshold
  , bth_nwarning :: !Double  -- ^ (gN/m2) nitrogen balance warning threshold
  , bth_cerror   :: !Double  -- ^ (gC/m2) carbon balance error threshold
  , bth_nerror   :: !Double  -- ^ (gN/m2) nitrogen balance error threshold
  } deriving (Show)

defaultBalanceThresholds :: CNBalanceThresholds
defaultBalanceThresholds = CNBalanceThresholds
  { bth_cwarning = 1.0e-8
  , bth_nwarning = 1.0e-7
  , bth_cerror   = 1.0e-7
  , bth_nerror   = 1.0e-3
  }

-- =========================================================================
-- Column-level carbon balance check
-- =========================================================================

data CBalanceColInput = CBalanceColInput
  { cbci_begcb         :: !Double  -- ^ beginning-of-step column C mass [gC/m2]
  , cbci_endcb         :: !Double  -- ^ end-of-step column C mass [gC/m2]
  , cbci_gpp           :: !Double  -- ^ GPP flux [gC/m2/s]
  , cbci_er            :: !Double  -- ^ ecosystem respiration [gC/m2/s]
  , cbci_fire_closs    :: !Double  -- ^ fire C loss [gC/m2/s]
  , cbci_hrv_xsmrpool_to_atm :: !Double
  , cbci_xsmrpool_to_atm     :: !Double
  , cbci_wood_harvestc        :: !Double
  , cbci_gru_conv_cflux       :: !Double
  , cbci_gru_wood_productc_gain :: !Double
  , cbci_crop_harvestc_to_cropprodc :: !Double
  , cbci_som_c_leached :: !Double  -- ^ SOM C leaching [gC/m2/s]
  , cbci_dt            :: !Double
  } deriving (Show)

data CBalanceColOutput = CBalanceColOutput
  { cbco_errcb :: !Double  -- ^ carbon balance error [gC/m2]
  } deriving (Show)

-- | Compute column-level carbon balance error.
cBalanceCheckCol :: CBalanceColInput -> CBalanceColOutput
cBalanceCheckCol inp =
  let !dt = cbci_dt inp
      !cinputs = cbci_gpp inp
      !coutputs = cbci_er inp + cbci_fire_closs inp
                + cbci_hrv_xsmrpool_to_atm inp + cbci_xsmrpool_to_atm inp
                + cbci_gru_conv_cflux inp + cbci_wood_harvestc inp
                + cbci_gru_wood_productc_gain inp
                + cbci_crop_harvestc_to_cropprodc inp
                - cbci_som_c_leached inp
      !err = (cinputs - coutputs) * dt - (cbci_endcb inp - cbci_begcb inp)
  in CBalanceColOutput { cbco_errcb = err }

-- =========================================================================
-- Column-level nitrogen balance check
-- =========================================================================

data NBalanceColInput = NBalanceColInput
  { nbci_begnb              :: !Double
  , nbci_endnb              :: !Double
  , nbci_ndep_to_sminn      :: !Double
  , nbci_nfix_to_sminn      :: !Double
  , nbci_supplement_to_sminn :: !Double
  , nbci_ffix_to_sminn      :: !Double
  , nbci_fert_to_sminn      :: !Double
  , nbci_soyfixn_to_sminn   :: !Double
  , nbci_denit               :: !Double
  , nbci_fire_nloss          :: !Double
  , nbci_gru_conv_nflux      :: !Double
  , nbci_wood_harvestn        :: !Double
  , nbci_gru_wood_productn_gain :: !Double
  , nbci_crop_harvestn_to_cropprodn :: !Double
  , nbci_sminn_leached        :: !Double
  , nbci_smin_no3_leached     :: !Double
  , nbci_smin_no3_runoff      :: !Double
  , nbci_f_n2o_nit            :: !Double
  , nbci_som_n_leached        :: !Double
  , nbci_use_nitrif_denitrif  :: !Bool
  , nbci_use_fun              :: !Bool
  , nbci_use_crop             :: !Bool
  , nbci_dt                   :: !Double
  } deriving (Show)

data NBalanceColOutput = NBalanceColOutput
  { nbco_errnb :: !Double  -- ^ nitrogen balance error [gN/m2]
  } deriving (Show)

-- | Compute column-level nitrogen balance error.
nBalanceCheckCol :: NBalanceColInput -> NBalanceColOutput
nBalanceCheckCol inp =
  let !dt = nbci_dt inp
      !ninputs0 = nbci_ndep_to_sminn inp + nbci_nfix_to_sminn inp
                + nbci_supplement_to_sminn inp
      !ninputs1 = if nbci_use_fun inp then ninputs0 + nbci_ffix_to_sminn inp else ninputs0
      !ninputs  = if nbci_use_crop inp
                  then ninputs1 + nbci_fert_to_sminn inp + nbci_soyfixn_to_sminn inp
                  else ninputs1
      !noutputs0 = nbci_denit inp + nbci_fire_nloss inp + nbci_gru_conv_nflux inp
                 + nbci_wood_harvestn inp + nbci_gru_wood_productn_gain inp
                 + nbci_crop_harvestn_to_cropprodn inp
      !noutputs = if nbci_use_nitrif_denitrif inp
                  then noutputs0 + nbci_f_n2o_nit inp
                     + nbci_smin_no3_leached inp + nbci_smin_no3_runoff inp
                     - nbci_som_n_leached inp
                  else noutputs0 + nbci_sminn_leached inp - nbci_som_n_leached inp
      !err = (ninputs - noutputs) * dt - (nbci_endnb inp - nbci_begnb inp)
  in NBalanceColOutput { nbco_errnb = err }

-- =========================================================================
-- Gridcell-level carbon balance check
-- =========================================================================

data CBalanceGrcInput = CBalanceGrcInput
  { cbgi_begcb   :: !Double  -- ^ beginning-of-step gridcell C [gC/m2]
  , cbgi_endcb   :: !Double  -- ^ end-of-step gridcell C [gC/m2]
  , cbgi_nbp     :: !Double  -- ^ net biome production [gC/m2/s]
  , cbgi_dwt_seedc_to_leaf    :: !Double
  , cbgi_dwt_seedc_to_deadstem :: !Double
  , cbgi_som_c_leached        :: !Double
  , cbgi_dt      :: !Double
  } deriving (Show)

data CBalanceGrcOutput = CBalanceGrcOutput
  { cbgo_errcb :: !Double
  } deriving (Show)

-- | Compute gridcell-level carbon balance error.
cBalanceCheckGrc :: CBalanceGrcInput -> CBalanceGrcOutput
cBalanceCheckGrc inp =
  let !dt = cbgi_dt inp
      !cinputs = cbgi_nbp inp + cbgi_dwt_seedc_to_leaf inp
               + cbgi_dwt_seedc_to_deadstem inp
      !coutputs = negate (cbgi_som_c_leached inp)
      !err = (cinputs - coutputs) * dt - (cbgi_endcb inp - cbgi_begcb inp)
  in CBalanceGrcOutput { cbgo_errcb = err }
