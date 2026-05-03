{-# LANGUAGE BangPatterns #-}
-- | Controls on very low values in critical CN vegetation state variables.
-- Fortran: CNPrecisionControlMod.F90
-- Julia:   src/biogeochem/cn_precision_control.jl
--
-- Truncates small C/N pool values to zero for numerical stability and
-- accumulates the truncated mass into truncation sink terms.
module CLM.BioGeoChem.CNPrecisionControl
  ( -- * Constants
    cnCcritDefault
  , cnCnegcritDefault
  , cnNcritDefault
  , cnNnegcritDefault
  , cnNMin
    -- * Truncation functions
  , TruncateCNInput(..)
  , TruncateCNOutput(..)
  , truncateCandN
  , TruncateCInput(..)
  , TruncateCOutput(..)
  , truncateC
  , TruncateNInput(..)
  , TruncateNOutput(..)
  , truncateN
  ) where

-- =========================================================================
-- Module-level constants
-- =========================================================================

-- | Critical carbon state value for truncation (gC/m2).
cnCcritDefault :: Double
cnCcritDefault = 1.0e-8

-- | Critical negative carbon state value for abort (gC/m2).
cnCnegcritDefault :: Double
cnCnegcritDefault = -6.0e+1

-- | Critical nitrogen state value for truncation (gN/m2).
cnNcritDefault :: Double
cnNcritDefault = 1.0e-8

-- | Critical negative nitrogen state value for abort (gN/m2).
cnNnegcritDefault :: Double
cnNnegcritDefault = -7.0e+0

-- | Minimum nitrogen value used when calculating CN ratio (gN/m2).
cnNMin :: Double
cnNMin = 1.0e-9

-- =========================================================================
-- Paired C and N truncation (single pool)
-- =========================================================================

data TruncateCNInput = TruncateCNInput
  { tcni_carbon   :: !Double
  , tcni_nitrogen :: !Double
  , tcni_ccrit    :: !Double
  , tcni_ncrit    :: !Double
  , tcni_cnegcrit :: !Double
  , tcni_nnegcrit :: !Double
  , tcni_use_nguardrail :: !Bool
  , tcni_use_matrixcn   :: !Bool
  , tcni_allowneg       :: !Bool
  } deriving (Show)

data TruncateCNOutput = TruncateCNOutput
  { tcno_carbon    :: !Double  -- ^ updated carbon (0.0 if truncated)
  , tcno_nitrogen  :: !Double  -- ^ updated nitrogen (0.0 if truncated)
  , tcno_pc        :: !Double  -- ^ carbon truncation accumulator delta
  , tcno_pn        :: !Double  -- ^ nitrogen truncation accumulator delta
  , tcno_truncated :: !Bool    -- ^ whether truncation occurred
  } deriving (Show)

-- | Truncate paired carbon and nitrogen state. Returns updated values
-- and the mass removed.
truncateCandN :: TruncateCNInput -> TruncateCNOutput
truncateCandN inp
  | not (tcni_allowneg inp) &&
    (tcni_carbon inp < tcni_cnegcrit inp || tcni_nitrogen inp < tcni_nnegcrit inp) =
      -- Would abort in Fortran; here we pass through unchanged
      TruncateCNOutput
        { tcno_carbon = tcni_carbon inp, tcno_nitrogen = tcni_nitrogen inp
        , tcno_pc = 0.0, tcno_pn = 0.0, tcno_truncated = False }
  | tcni_use_matrixcn inp =
      if (tcni_carbon inp < tcni_ccrit inp && tcni_carbon inp > negate (tcni_ccrit inp) * 1.0e6) ||
         (tcni_use_nguardrail inp && tcni_nitrogen inp < tcni_ncrit inp && tcni_nitrogen inp > negate (tcni_ncrit inp) * 1.0e6)
      then TruncateCNOutput
        { tcno_carbon = 0.0, tcno_nitrogen = 0.0
        , tcno_pc = tcni_carbon inp, tcno_pn = tcni_nitrogen inp
        , tcno_truncated = True }
      else TruncateCNOutput
        { tcno_carbon = tcni_carbon inp, tcno_nitrogen = tcni_nitrogen inp
        , tcno_pc = 0.0, tcno_pn = 0.0, tcno_truncated = False }
  | abs (tcni_carbon inp) < tcni_ccrit inp ||
    (tcni_use_nguardrail inp && abs (tcni_nitrogen inp) < tcni_ncrit inp) =
      TruncateCNOutput
        { tcno_carbon = 0.0, tcno_nitrogen = 0.0
        , tcno_pc = tcni_carbon inp, tcno_pn = tcni_nitrogen inp
        , tcno_truncated = True }
  | otherwise =
      TruncateCNOutput
        { tcno_carbon = tcni_carbon inp, tcno_nitrogen = tcni_nitrogen inp
        , tcno_pc = 0.0, tcno_pn = 0.0, tcno_truncated = False }

-- =========================================================================
-- Carbon-only truncation (single pool)
-- =========================================================================

data TruncateCInput = TruncateCInput
  { tci_carbon   :: !Double
  , tci_ccrit    :: !Double
  , tci_cnegcrit :: !Double
  , tci_allowneg :: !Bool
  } deriving (Show)

data TruncateCOutput = TruncateCOutput
  { tco_carbon    :: !Double
  , tco_pc        :: !Double
  , tco_truncated :: !Bool
  } deriving (Show)

-- | Truncate a carbon-only state.
truncateC :: TruncateCInput -> TruncateCOutput
truncateC inp
  | not (tci_allowneg inp) && tci_carbon inp < tci_cnegcrit inp =
      -- Would abort; pass through unchanged
      TruncateCOutput { tco_carbon = tci_carbon inp, tco_pc = 0.0, tco_truncated = False }
  | abs (tci_carbon inp) < tci_ccrit inp =
      TruncateCOutput { tco_carbon = 0.0, tco_pc = tci_carbon inp, tco_truncated = True }
  | otherwise =
      TruncateCOutput { tco_carbon = tci_carbon inp, tco_pc = 0.0, tco_truncated = False }

-- =========================================================================
-- Nitrogen-only truncation (single pool)
-- =========================================================================

data TruncateNInput = TruncateNInput
  { tni_nitrogen :: !Double
  , tni_ncrit    :: !Double
  , tni_nnegcrit :: !Double
  } deriving (Show)

data TruncateNOutput = TruncateNOutput
  { tno_nitrogen :: !Double
  , tno_pn       :: !Double
  } deriving (Show)

-- | Truncate a nitrogen-only state.
truncateN :: TruncateNInput -> TruncateNOutput
truncateN inp
  | tni_nitrogen inp < tni_nnegcrit inp =
      -- Fortran has abort commented out; do nothing
      TruncateNOutput { tno_nitrogen = tni_nitrogen inp, tno_pn = 0.0 }
  | abs (tni_nitrogen inp) < tni_ncrit inp =
      TruncateNOutput { tno_nitrogen = 0.0, tno_pn = tni_nitrogen inp }
  | otherwise =
      TruncateNOutput { tno_nitrogen = tni_nitrogen inp, tno_pn = 0.0 }
