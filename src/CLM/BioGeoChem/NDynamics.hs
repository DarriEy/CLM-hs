{-# LANGUAGE BangPatterns #-}
-- | Mineral nitrogen dynamics: deposition, fixation, fertilization, soy fixation.
-- Fortran: CNNDynamicsMod.F90
-- Julia:   src/biogeochem/n_dynamics.jl
--
-- All functions are pure.
module CLM.BioGeoChem.NDynamics
  ( -- * Data types
    NDynamicsParams(..)
  , defaultNDynamicsParams
    -- * N deposition
  , NDepositionInput(..)
  , nDeposition
    -- * Free-living N fixation
  , FreeLivingFixInput(..)
  , nFreeLivingFixation
    -- * Symbiotic N fixation
  , NFixationInput(..)
  , nFixation
    -- * N fertilization
  , NFertInput(..)
  , nFert
    -- * Soybean N fixation
  , NSoyfixInput(..)
  , NSoyfixOutput(..)
  , nSoyfix
    -- * Plant N uptake vertical profile
  , nitrogenUptakeProfile
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Vertical profile [1/m] for distributing the column plant N uptake across
-- soil layers, weighting by available mineral N. Integrates to 1 over depth
-- (sum_j prof(j)*dzsoi(j) = 1), so multiplying a column uptake flux [gN/m2/s] by
-- prof(j) gives the per-layer uptake [gN/m3/s] that conserves the column total.
-- Falls back to the fixation profile where there is no mineral N.
-- Ported from SoilBiogeochemNitrogenUptakeMod.F90 (SoilBiogeochemNitrogenUptake).
nitrogenUptakeProfile
  :: Int               -- ^ nlevdecomp
  -> VU.Vector Double  -- ^ sminn_vr (per layer) [gN/m3]
  -> VU.Vector Double  -- ^ dzsoi (per layer) [m]
  -> VU.Vector Double  -- ^ nfixation_prof (fallback profile) [1/m]
  -> VU.Vector Double
nitrogenUptakeProfile nlev sminnVr dzsoi nfixProf =
  let at v j = if j >= 0 && j < VU.length v then v VU.! j else 0.0
      sminnTot = sum [ at sminnVr j * at dzsoi j | j <- [0 .. nlev - 1] ]
  in VU.generate nlev $ \j ->
       if sminnTot > 0.0 then at sminnVr j / sminnTot else at nfixProf j

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | SPVAL sentinel value.
spval :: Double
spval = 1.0e36

-- | Parameters for mineral nitrogen dynamics.
data NDynamicsParams = NDynamicsParams
  { ndp_freelivfix_intercept :: !Double  -- ^ intercept of free living fixation vs annual ET
  , ndp_freelivfix_slope_wET :: !Double  -- ^ slope of free living fixation vs annual ET
  } deriving (Show, Eq)

defaultNDynamicsParams :: NDynamicsParams
defaultNDynamicsParams = NDynamicsParams
  { ndp_freelivfix_intercept = 0.0117
  , ndp_freelivfix_slope_wET = 0.0006
  }

-- ============================================================
-- N Deposition
-- ============================================================

-- | Input to N deposition.
data NDepositionInput = NDepositionInput
  { ndi2_nc           :: !Int
  , ndi2_forc_ndep    :: !(VU.Vector Double)  -- ^ (ngridcell) forcing deposition rate
  , ndi2_col_gridcell :: !(VU.Vector Int)     -- ^ (nc) column to gridcell mapping
  } deriving (Show)

-- | Calculate N deposition rate from atmospheric forcing.
-- Returns ndep_to_sminn per column (nc).
nDeposition :: NDepositionInput -> VU.Vector Double
nDeposition inp =
  let !nc = ndi2_nc inp
  in VU.generate nc $ \c ->
       let g = ndi2_col_gridcell inp VU.! c
       in ndi2_forc_ndep inp VU.! g

-- ============================================================
-- Free-living N fixation
-- ============================================================

-- | Input to free-living N fixation.
data FreeLivingFixInput = FreeLivingFixInput
  { flf_nc       :: !Int
  , flf_mask     :: !(VU.Vector Bool)
  , flf_params   :: !NDynamicsParams
  , flf_annET    :: !(VU.Vector Double)  -- ^ (nc) annual ET
  , flf_dayspyr  :: !Double
  } deriving (Show)

-- | Calculate free-living N fixation to soil mineral N as f(annual ET).
-- Returns ffix_to_sminn per column (nc).
nFreeLivingFixation :: FreeLivingFixInput -> VU.Vector Double
nFreeLivingFixation inp =
  let !nc  = flf_nc inp
      mask = flf_mask inp
      params = flf_params inp
      slope = ndp_freelivfix_slope_wET params
      inter = ndp_freelivfix_intercept params
      dayspyr = flf_dayspyr inp
      secs_per_year = dayspyr * 24.0 * 3600.0
  in VU.generate nc $ \c ->
       if not (mask VU.! c) then 0.0
       else (slope * (max 0.0 (flf_annET inp VU.! c) * secs_per_year) + inter) / secs_per_year

-- ============================================================
-- Symbiotic N fixation
-- ============================================================

-- | Input to symbiotic N fixation.
data NFixationInput = NFixationInput
  { nfi_nc              :: !Int
  , nfi_mask            :: !(VU.Vector Bool)
  , nfi_dayspyr         :: !Double
  , nfi_nfix_timeconst  :: !Double
  , nfi_use_fun         :: !Bool
  , nfi_col_is_fates    :: !(VU.Vector Bool)
  , nfi_annsum_npp      :: !(VU.Vector Double)   -- ^ annual-mean NPP (gC/m2/s)
  , nfi_lag_npp         :: !(VU.Vector Double)    -- ^ lagged NPP (gC/m2/s)
  } deriving (Show)

-- | Calculate N fixation rate as f(annual total NPP).
-- Returns nfix_to_sminn per column (nc).
nFixation :: NFixationInput -> VU.Vector Double
nFixation inp =
  let !nc = nfi_nc inp
      mask = nfi_mask inp
      dayspyr = nfi_dayspyr inp
      tc = nfi_nfix_timeconst inp
      use_fun = nfi_use_fun inp
      secs_yr = secspday * dayspyr
  in VU.generate nc $ \c ->
       if not (mask VU.! c) || use_fun then 0.0
       else let npp = if nfi_col_is_fates inp VU.! c then 0.0
                      else if tc > 0.0 && tc < 500.0
                           then nfi_lag_npp inp VU.! c
                           else nfi_annsum_npp inp VU.! c

                val = if tc > 0.0 && tc < 500.0
                      then if npp /= spval
                           then (1.8 * (1.0 - exp (-0.003 * npp * secs_yr))) / secs_yr
                           else 0.0
                      else (1.8 * (1.0 - exp (-0.003 * npp))) / secs_yr
            in max 0.0 val

-- ============================================================
-- N Fertilization
-- ============================================================

-- | Input to N fertilization.
data NFertInput = NFertInput
  { nfe_nc        :: !Int
  , nfe_np        :: !Int
  , nfe_mask_c    :: !(VU.Vector Bool)
  , nfe_mask_p    :: !(VU.Vector Bool)
  , nfe_fert      :: !(VU.Vector Double)  -- ^ patch-level fertilizer flux (np)
  , nfe_column    :: !(VU.Vector Int)     -- ^ patch-to-column mapping (np)
  , nfe_wtcol     :: !(VU.Vector Double)  -- ^ patch weight on column (np)
  } deriving (Show)

-- | Aggregate patch-level fertilizer N flux to column level.
-- Returns fert_to_sminn per column (nc).
nFert :: NFertInput -> VU.Vector Double
nFert inp =
  let !nc = nfe_nc inp
      !np = nfe_np inp
      maskP = nfe_mask_p inp
      col   = nfe_column inp
      wt    = nfe_wtcol inp
      fert  = nfe_fert inp
  in VU.generate nc $ \c ->
       sum [ fert VU.! p * wt VU.! p
           | p <- [0..np-1], maskP VU.! p, col VU.! p == c ]

-- ============================================================
-- Soybean N fixation
-- ============================================================

-- | Input to soybean N fixation.
data NSoyfixInput = NSoyfixInput
  { nsf_nc                :: !Int
  , nsf_np                :: !Int
  , nsf_mask_c            :: !(VU.Vector Bool)
  , nsf_mask_p            :: !(VU.Vector Bool)
  , nsf_ivt               :: !(VU.Vector Int)       -- ^ PFT type (np)
  , nsf_column            :: !(VU.Vector Int)       -- ^ patch-to-column (np)
  , nsf_wtcol             :: !(VU.Vector Double)    -- ^ patch weight on column (np)
  , nsf_croplive          :: !(VU.Vector Bool)      -- ^ crop live flag (np)
  , nsf_hui               :: !(VU.Vector Double)    -- ^ heat unit index (np)
  , nsf_gddmaturity       :: !(VU.Vector Double)    -- ^ GDD at maturity (np)
  , nsf_plant_ndemand     :: !(VU.Vector Double)    -- ^ plant N demand (np)
  , nsf_fpg               :: !(VU.Vector Double)    -- ^ fraction of N demand met (nc)
  , nsf_sminn             :: !(VU.Vector Double)    -- ^ soil mineral N (nc)
  , nsf_wf                :: !(VU.Vector Double)    -- ^ water factor (nc)
  -- Soybean PFT indices
  , nsf_ntmp_soybean        :: !Int
  , nsf_nirrig_tmp_soybean  :: !Int
  , nsf_ntrp_soybean        :: !Int
  , nsf_nirrig_trp_soybean  :: !Int
  } deriving (Show)

-- | Output of soybean N fixation.
data NSoyfixOutput = NSoyfixOutput
  { nso_soyfixn           :: !(VU.Vector Double)  -- ^ patch-level soy N fixation (np)
  , nso_soyfixn_to_sminn  :: !(VU.Vector Double)  -- ^ column-level aggregated (nc)
  } deriving (Show)

-- | Calculate N fixation for soybeans.
nSoyfix :: NSoyfixInput -> NSoyfixOutput
nSoyfix inp =
  let !nc = nsf_nc inp
      !np = nsf_np inp
      maskP = nsf_mask_p inp
      ivt   = nsf_ivt inp
      col   = nsf_column inp
      wt    = nsf_wtcol inp

      isSoybean p = let v = ivt VU.! p
                    in v == nsf_ntmp_soybean inp
                       || v == nsf_nirrig_tmp_soybean inp
                       || v == nsf_ntrp_soybean inp
                       || v == nsf_nirrig_trp_soybean inp

      sminnT1 = 30.0
      sminnT2 = 10.0
      gddFT1 = 0.15; gddFT2 = 0.30; gddFT3 = 0.55; gddFT4 = 0.75

      -- Patch-level fixation
      soyfixn = VU.generate np $ \p ->
        if not (maskP VU.! p) then 0.0
        else if nsf_croplive inp VU.! p && isSoybean p
             then let c = col VU.! p
                      fpg = nsf_fpg inp VU.! c
                  in if fpg < 1.0
                     then let soy_ndemand = nsf_plant_ndemand inp VU.! p * (1.0 - fpg)
                              fxw = nsf_wf inp VU.! c / 0.85
                              sminn_c = nsf_sminn inp VU.! c
                              fxn = if sminn_c > sminnT1 then 0.0
                                    else if sminn_c > sminnT2
                                         then 1.5 - 0.005 * (sminn_c * 10.0)
                                         else 1.0
                              gddFrac = nsf_hui inp VU.! p / nsf_gddmaturity inp VU.! p
                              fxg = if gddFrac <= gddFT1 then 0.0
                                    else if gddFrac <= gddFT2 then 6.67 * gddFrac - 1.0
                                    else if gddFrac <= gddFT3 then 1.0
                                    else if gddFrac <= gddFT4 then 3.75 - 5.0 * gddFrac
                                    else 0.0
                              fxr = max 0.0 (min 1.0 (min fxw fxn) * fxg)
                          in min (fxr * soy_ndemand) soy_ndemand
                     else 0.0
             else 0.0

      -- Column-level aggregation
      soyfixn_to_sminn = VU.generate nc $ \c ->
        sum [ soyfixn VU.! p * wt VU.! p
            | p <- [0..np-1], maskP VU.! p, col VU.! p == c ]

  in NSoyfixOutput
    { nso_soyfixn          = soyfixn
    , nso_soyfixn_to_sminn = soyfixn_to_sminn
    }
