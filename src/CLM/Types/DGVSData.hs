-- | Dynamic Global Vegetation Simulator data structures.
-- Fortran: CNDVType.F90 — patch-level DGVS state variables and ecophysiological constants.
module CLM.Types.DGVSData
  ( DGVSData(..)
  , defaultDGVSData
  , DGVEcophysCon(..)
  , defaultDGVEcophysCon
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Patch-level DGVS state variables.
-- Preserves Fortran variable names with @dgvs_@ prefix.
-- Bool vectors (present_patch, pftmayexist_patch) are omitted per convention.
data DGVSData = DGVSData
  { -- Accumulated growing degree days / temperature (patch)
    dgvs_agdd_patch        :: !(VU.Vector Double)  -- ^ accumulated GDD this year (degree-days)
  , dgvs_agddtw_patch      :: !(VU.Vector Double)  -- ^ accumulated GDD for twmax check (degree-days)
  , dgvs_agdd20_patch      :: !(VU.Vector Double)  -- ^ 20-year running mean annual GDD (degree-days)
  , dgvs_tmomin20_patch    :: !(VU.Vector Double)  -- ^ 20-year running mean of annual min monthly temp (K)
    -- Vegetation state (patch)
  , dgvs_nind_patch         :: !(VU.Vector Double)  -- ^ number of individuals (#/m2)
  , dgvs_lm_ind_patch       :: !(VU.Vector Double)  -- ^ individual leaf mass (gC/ind)
  , dgvs_lai_ind_patch      :: !(VU.Vector Double)  -- ^ individual LAI (m2/m2)
  , dgvs_fpcinc_patch       :: !(VU.Vector Double)  -- ^ foliar projective cover increment (fraction)
  , dgvs_fpcgrid_patch      :: !(VU.Vector Double)  -- ^ foliar projective cover on gridcell (fraction)
  , dgvs_fpcgridold_patch   :: !(VU.Vector Double)  -- ^ previous-year fpcgrid (fraction)
  , dgvs_crownarea_patch    :: !(VU.Vector Double)  -- ^ crown area per individual (m2)
  , dgvs_greffic_patch      :: !(VU.Vector Double)  -- ^ growth efficiency (gC/m2 leaf/yr)
  , dgvs_heatstress_patch   :: !(VU.Vector Double)  -- ^ heat stress mortality factor
  } deriving (Show)

defaultDGVSData :: DGVSData
defaultDGVSData = DGVSData
  { dgvs_agdd_patch = VU.empty, dgvs_agddtw_patch = VU.empty
  , dgvs_agdd20_patch = VU.empty, dgvs_tmomin20_patch = VU.empty
  , dgvs_nind_patch = VU.empty, dgvs_lm_ind_patch = VU.empty
  , dgvs_lai_ind_patch = VU.empty, dgvs_fpcinc_patch = VU.empty
  , dgvs_fpcgrid_patch = VU.empty, dgvs_fpcgridold_patch = VU.empty
  , dgvs_crownarea_patch = VU.empty, dgvs_greffic_patch = VU.empty
  , dgvs_heatstress_patch = VU.empty
  }

-- | Ecophysiological constants for DGVS.
-- Preserves Fortran variable names with @dgveco_@ prefix.
-- Maps pftcon parameters (pftpar20/28/29/30/31) to semantic DGVS names,
-- plus allometric constants.
data DGVEcophysCon = DGVEcophysCon
  { dgveco_crownarea_max :: !(VU.Vector Double)  -- ^ max crown area (m2) from pftpar20
  , dgveco_tcmin         :: !(VU.Vector Double)  -- ^ min coldest monthly mean T from pftpar28
  , dgveco_tcmax         :: !(VU.Vector Double)  -- ^ max coldest monthly mean T from pftpar29
  , dgveco_gddmin        :: !(VU.Vector Double)  -- ^ min GDD (>= 5 deg C) from pftpar30
  , dgveco_twmax         :: !(VU.Vector Double)  -- ^ upper limit warmest month T from pftpar31
  , dgveco_reinickerp    :: !(VU.Vector Double)  -- ^ Reinicke's p (allometric) constant 1.6
  , dgveco_allom1        :: !(VU.Vector Double)  -- ^ allometric param 1 (100; 250 shrubs)
  , dgveco_allom2        :: !(VU.Vector Double)  -- ^ allometric param 2 (40; 8 shrubs)
  , dgveco_allom3        :: !(VU.Vector Double)  -- ^ allometric param 3 (0.5)
  } deriving (Show)

defaultDGVEcophysCon :: DGVEcophysCon
defaultDGVEcophysCon = DGVEcophysCon
  { dgveco_crownarea_max = VU.empty, dgveco_tcmin = VU.empty
  , dgveco_tcmax = VU.empty, dgveco_gddmin = VU.empty
  , dgveco_twmax = VU.empty, dgveco_reinickerp = VU.empty
  , dgveco_allom1 = VU.empty, dgveco_allom2 = VU.empty
  , dgveco_allom3 = VU.empty
  }
