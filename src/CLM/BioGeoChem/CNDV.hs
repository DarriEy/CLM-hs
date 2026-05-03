{-# LANGUAGE BangPatterns #-}
-- | Dynamic Global Vegetation Model (CNDV).
-- Fortran: CNDVType.F90, CNDVLightMod.F90, CNDVEstablishmentMod.F90,
--          CNDVDriverMod.F90, dynCNDVMod.F90
-- Julia:   src/biogeochem/cndv.jl
--
-- The coupled CN-DGVM mode that computes annual establishment, light
-- competition, and mortality, then interpolates patch weights.
module CLM.BioGeoChem.CNDV
  ( -- * Data types
    DGVEcophysCon(..)
  , DGVSPatchData(..)
    -- * Constants
  , fpcTreeMax
    -- * Light competition (single patch)
  , LightCompInput(..)
  , LightCompOutput(..)
  , calcCrownArea
  , calcFPCInd
    -- * Establishment helpers
  , EstablishmentInput(..)
  , EstablishmentOutput(..)
  , calcEstablishment
    -- * Accumulator update (single patch)
  , AccumInput(..)
  , AccumOutput(..)
  , cndvAccumUpdate
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Constants
-- =========================================================================

-- | Maximum tree FPC.
fpcTreeMax :: Double
fpcTreeMax = 0.95

-- =========================================================================
-- DGV ecophysiology constants (per PFT)
-- =========================================================================

-- | DGV ecophysiology constants per PFT.
-- Ported from @dgv_ecophyscon@ in @CNDVType.F90@.
data DGVEcophysCon = DGVEcophysCon
  { dec_crownarea_max :: !(VU.Vector Double)  -- ^ maximum crown area [m2]
  , dec_reinickerp    :: !(VU.Vector Double)  -- ^ Reinicke's p parameter
  , dec_allom1        :: !(VU.Vector Double)  -- ^ allometry parameter 1
  , dec_tcmax         :: !(VU.Vector Double)  -- ^ maximum coldest-month T [C]
  , dec_tcmin         :: !(VU.Vector Double)  -- ^ minimum coldest-month T [C]
  , dec_gddmin        :: !(VU.Vector Double)  -- ^ minimum GDD for establishment
  , dec_twmax         :: !(VU.Vector Double)  -- ^ maximum warmest-month T [C]
  } deriving (Show)

-- =========================================================================
-- DGVS patch data
-- =========================================================================

-- | Per-patch DGVS state variables.
-- Ported from @DGVSData@ in @src/types/dgvs.jl@.
data DGVSPatchData = DGVSPatchData
  { dgvs_present    :: !Bool    -- ^ whether PFT is present
  , dgvs_nind       :: !Double  -- ^ number of individuals [#/m2]
  , dgvs_fpcgrid    :: !Double  -- ^ foliar projective cover on gridcell
  , dgvs_crownarea  :: !Double  -- ^ crown area [m2]
  , dgvs_greffic    :: !Double  -- ^ growth efficiency
  , dgvs_heatstress :: !Double  -- ^ heat stress counter
  } deriving (Show)

-- =========================================================================
-- Light competition helpers
-- =========================================================================

data LightCompInput = LightCompInput
  { lci_fpcgrid     :: !Double  -- ^ current FPC on gridcell
  , lci_nind        :: !Double  -- ^ individual density
  , lci_deadstemc   :: !Double  -- ^ dead stem C [gC/m2]
  , lci_leafcmax    :: !Double  -- ^ maximum leaf C [gC/m2]
  , lci_dwood       :: !Double  -- ^ wood density [gC/m3]
  , lci_slatop      :: !Double  -- ^ specific leaf area at top [m2/gC]
  , lci_dsladlai    :: !Double  -- ^ change in SLA per LAI
  , lci_crownarea_max :: !Double
  , lci_allom1      :: !Double
  , lci_reinickerp  :: !Double
  , lci_is_woody    :: !Bool
  } deriving (Show)

data LightCompOutput = LightCompOutput
  { lco_crownarea   :: !Double
  , lco_fpcgrid     :: !Double
  , lco_fpc_inc     :: !Double
  } deriving (Show)

-- | Calculate crown area from dead stem C and individual density.
calcCrownArea :: Double  -- ^ deadstemc
              -> Double  -- ^ nind
              -> Double  -- ^ fpcgrid
              -> Double  -- ^ dwood
              -> Double  -- ^ crownarea_max
              -> Double  -- ^ allom1
              -> Double  -- ^ reinickerp
              -> Double  -- ^ taper (typically 200)
              -> Double
calcCrownArea deadstemc nind fpc dwood ca_max allom1 reinick taper
  | fpc > 0.0 && nind > 0.0 =
      let !stocking = nind / fpc
          !stemdiam = (24.0 * deadstemc / (pi * stocking * dwood * taper)) ** (1.0/3.0)
      in min ca_max (allom1 * stemdiam ** reinick)
  | otherwise = 0.0

-- | Calculate FPC for an individual from LAI.
calcFPCInd :: Double  -- ^ lai_ind
           -> Double
calcFPCInd lai = 1.0 - exp (-0.5 * lai)

-- =========================================================================
-- Establishment helpers (single PFT on gridcell)
-- =========================================================================

data EstablishmentInput = EstablishmentInput
  { esi_present    :: !Bool
  , esi_prec365    :: !Double   -- ^ 365-day mean precip [mm/s]
  , esi_tcold      :: !Double   -- ^ coldest month temperature [C]
  , esi_twarm      :: !Double   -- ^ warmest month temperature [C]
  , esi_gdd        :: !Double   -- ^ growing degree days
  , esi_tcmax      :: !Double
  , esi_tcmin      :: !Double
  , esi_twmax      :: !Double
  , esi_gddmin     :: !Double
  , esi_is_woody   :: !Bool
  , esi_annsum_npp :: !Double   -- ^ annual NPP [gC/m2/yr]
  } deriving (Show)

data EstablishmentOutput = EstablishmentOutput
  { eso_survive  :: !Bool   -- ^ passes bioclimatic filter
  , eso_estab    :: !Bool   -- ^ can establish
  } deriving (Show)

-- | Check if PFT passes bioclimatic filters for establishment.
calcEstablishment :: EstablishmentInput -> EstablishmentOutput
calcEstablishment inp =
  let !prec_ok = esi_prec365 inp > 100.0 / (365.0 * 86400.0)
      !tc_ok   = esi_tcold inp >= esi_tcmin inp && esi_tcold inp <= esi_tcmax inp
      !tw_ok   = esi_twarm inp <= esi_twmax inp
      !gdd_ok  = esi_gdd inp >= esi_gddmin inp
      !survive = tc_ok && tw_ok
      !estab   = survive && prec_ok && gdd_ok && esi_annsum_npp inp > 0.0
  in EstablishmentOutput
    { eso_survive = survive
    , eso_estab = estab
    }

-- =========================================================================
-- Accumulator update (single patch per timestep)
-- =========================================================================

data AccumInput = AccumInput
  { ai_agddtw     :: !Double  -- ^ accumulated GDD above twmax
  , ai_agdd       :: !Double  -- ^ accumulated GDD for establishment
  , ai_t_ref2m    :: !Double  -- ^ 2m reference temperature [K]
  , ai_twmax      :: !Double  -- ^ warmest month T threshold [C]
  , ai_tfrz       :: !Double  -- ^ freezing point [K]
  , ai_dt         :: !Double  -- ^ timestep [s]
  , ai_secspday   :: !Double
  } deriving (Show)

data AccumOutput = AccumOutput
  { ao_agddtw :: !Double
  , ao_agdd   :: !Double
  } deriving (Show)

-- | Update AGDDTW and AGDD accumulators for a single patch.
cndvAccumUpdate :: AccumInput -> AccumOutput
cndvAccumUpdate inp =
  let !t2m_c = ai_t_ref2m inp - ai_tfrz inp
      !agddtw' = if t2m_c > ai_twmax inp
                 then ai_agddtw inp + (t2m_c - ai_twmax inp) * ai_dt inp / ai_secspday inp
                 else ai_agddtw inp
      !agdd' = if t2m_c > 0.0
               then ai_agdd inp + t2m_c * ai_dt inp / ai_secspday inp
               else ai_agdd inp
  in AccumOutput { ao_agddtw = agddtw', ao_agdd = agdd' }
