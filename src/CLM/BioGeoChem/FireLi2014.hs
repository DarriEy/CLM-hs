{-# LANGUAGE BangPatterns #-}
-- | Li et al. (2014) fire dynamics module.
-- Fortran: CNFireLi2014Mod.F90
-- Julia:   src/biogeochem/fire_li2014.jl
--
-- Contains Li2014-specific data types, burned area calculation
-- (cropland, peatland, deforestation, natural fire), and
-- fire C/N flux wrapper with Li2014 combustion completeness factors.
module CLM.BioGeoChem.FireLi2014
  ( -- * Data types
    CNFireLi2014Data(..)
  , PftConFireLi2014(..)
    -- * Configuration query
  , needLightningAndPopdens
    -- * Burned area components (pure, single-column)
  , CropFireInput(..)
  , CropFireOutput(..)
  , calcCropBurnedArea
  , PeatFireInput(..)
  , PeatFireOutput(..)
  , calcPeatBurnedArea
  , NaturalFireInput(..)
  , NaturalFireOutput(..)
  , calcNaturalBurnedArea
    -- * Li2014-specific combustion completeness
  , li2014CmbCmpltLitter
  , li2014CmbCmpltCwd
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Li2014-specific data
-- =========================================================================

-- | Li2014-specific fire data: population density, lightning frequency,
-- GDP, peatland fraction, and crop fire timing.
-- Ported from member data of @cnfire_li2014_type@ in @CNFireLi2014Mod.F90@.
data CNFireLi2014Data = CNFireLi2014Data
  { li_forc_hdm   :: !(VU.Vector Double)  -- ^ human population density (#/km2) (gridcell)
  , li_forc_lnfm  :: !(VU.Vector Double)  -- ^ lightning frequency (#/km2/hr) (gridcell)
  , li_gdp_lf_col :: !(VU.Vector Double)  -- ^ GDP data (k$/capita) (column)
  , li_peatf_lf_col :: !(VU.Vector Double)  -- ^ peatland fraction (0-1) (column)
  , li_abm_lf_col :: !(VU.Vector Int)     -- ^ prescribed crop fire month (1-12) (column)
  } deriving (Show)

-- | PFT-level fire parameters specific to Li2014.
data PftConFireLi2014 = PftConFireLi2014
  { li_fsr_pft :: !(VU.Vector Double)  -- ^ fire spread rate by PFT (km/hr)
  , li_fd_pft  :: !(VU.Vector Double)  -- ^ fire duration by PFT (hr)
  } deriving (Show)

-- | The Li2014 fire model requires lightning and population density data.
needLightningAndPopdens :: Bool
needLightningAndPopdens = True

-- | Li2014-specific combustion completeness factor for litter.
li2014CmbCmpltLitter :: Double
li2014CmbCmpltLitter = 0.5

-- | Li2014-specific combustion completeness factor for CWD.
li2014CmbCmpltCwd :: Double
li2014CmbCmpltCwd = 0.25

-- =========================================================================
-- Crop burned area (single column)
-- =========================================================================

data CropFireInput = CropFireInput
  { cfi_fuelc_crop   :: !Double  -- ^ crop fuel [gC/m2]
  , cfi_lfuel        :: !Double  -- ^ lower fuel threshold [gC/m2]
  , cfi_ufuel        :: !Double  -- ^ upper fuel threshold [gC/m2]
  , cfi_cropfire_a1  :: !Double  -- ^ cropfire constant [/d]
  , cfi_hdm          :: !Double  -- ^ human population density (#/km2)
  , cfi_gdp          :: !Double  -- ^ GDP (k$/capita)
  , cfi_wtcol        :: !Double  -- ^ patch weight on column
  , cfi_secsphr      :: !Double  -- ^ seconds per hour
  } deriving (Show)

data CropFireOutput = CropFireOutput
  { cfo_baf_crop :: !Double  -- ^ burned area fraction from cropland
  } deriving (Show)

-- | Calculate burned area from cropland fire for a single patch.
calcCropBurnedArea :: CropFireInput -> CropFireOutput
calcCropBurnedArea inp =
  let !fb = max 0.0 (min 1.0 ((cfi_fuelc_crop inp - cfi_lfuel inp)
                              / (cfi_ufuel inp - cfi_lfuel inp)))
      !fhd = 0.04 + 0.96 * exp (-pi * sqrt (cfi_hdm inp / 350.0))
      !fgdp = 0.01 + 0.99 * exp (-pi * (cfi_gdp inp / 10.0))
  in CropFireOutput
    { cfo_baf_crop = cfi_cropfire_a1 inp / cfi_secsphr inp * fb * fhd * fgdp * cfi_wtcol inp
    }

-- =========================================================================
-- Peatland burned area (single column)
-- =========================================================================

data PeatFireInput = PeatFireInput
  { pfi_lat                  :: !Double  -- ^ latitude [deg]
  , pfi_borealat             :: !Double  -- ^ boreal latitude threshold [deg]
  , pfi_non_boreal_peatfire_c :: !Double
  , pfi_boreal_peatfire_c    :: !Double
  , pfi_prec60               :: !Double  -- ^ 60-day mean precip [mm/s]
  , pfi_wf2                  :: !Double  -- ^ soil wetness factor
  , pfi_tsoi17               :: !Double  -- ^ 17cm soil temperature [K]
  , pfi_tfrz                 :: !Double  -- ^ freezing temperature [K]
  , pfi_peatf                :: !Double  -- ^ peatland fraction [0-1]
  , pfi_fsat                 :: !Double  -- ^ saturated fraction
  , pfi_secspday             :: !Double
  , pfi_secsphr              :: !Double
  , pfi_nonborpeat_precip_denom :: !Double
  , pfi_borpeat_soilmoist_denom :: !Double
  } deriving (Show)

data PeatFireOutput = PeatFireOutput
  { pfo_baf_peatf :: !Double  -- ^ peatland burned area fraction
  } deriving (Show)

-- | Calculate peatland burned area fraction for a single column.
calcPeatBurnedArea :: PeatFireInput -> PeatFireOutput
calcPeatBurnedArea inp
  | pfi_lat inp < pfi_borealat inp =
      let !x = max 0.0 (min 1.0
                ((4.0 - pfi_prec60 inp * pfi_secspday inp / pfi_nonborpeat_precip_denom inp)
                 / 4.0))
      in PeatFireOutput
        { pfo_baf_peatf = pfi_non_boreal_peatfire_c inp / pfi_secsphr inp
                         * x * x * pfi_peatf inp * (1.0 - pfi_fsat inp)
        }
  | otherwise =
      PeatFireOutput
        { pfo_baf_peatf = pfi_boreal_peatfire_c inp / pfi_secsphr inp
                         * exp (-pi * (max 0.0 (pfi_wf2 inp) / pfi_borpeat_soilmoist_denom inp))
                         * max 0.0 (min 1.0 ((pfi_tsoi17 inp - pfi_tfrz inp) / 10.0))
                         * pfi_peatf inp * (1.0 - pfi_fsat inp)
        }

-- =========================================================================
-- Natural fire burned area (single column)
-- =========================================================================

data NaturalFireInput = NaturalFireInput
  { nfi_fuelc       :: !Double  -- ^ fuel load [gC/m2]
  , nfi_lfuel       :: !Double
  , nfi_ufuel       :: !Double
  , nfi_wf          :: !Double  -- ^ soil wetness factor
  , nfi_forc_rh     :: !Double  -- ^ relative humidity [%]
  , nfi_rh_low      :: !Double
  , nfi_rh_hgh      :: !Double
  , nfi_forc_t      :: !Double  -- ^ air temperature [K]
  , nfi_tfrz        :: !Double
  , nfi_hdm         :: !Double  -- ^ human population density
  , nfi_lnfm        :: !Double  -- ^ lightning frequency
  , nfi_lat         :: !Double  -- ^ latitude [rad]
  , nfi_ign_eff     :: !Double  -- ^ ignition efficiency
  , nfi_pot_hmn_ign :: !Double  -- ^ pot_hmn_ign_counts_alpha
  , nfi_cropf       :: !Double  -- ^ crop fraction
  , nfi_lgdp        :: !Double  -- ^ GDP factor
  , nfi_lgdp1       :: !Double
  , nfi_lpop        :: !Double  -- ^ population factor
  , nfi_btran_col   :: !Double  -- ^ root weighted wetness
  , nfi_wtlf        :: !Double
  , nfi_bt_min      :: !Double
  , nfi_bt_max      :: !Double
  , nfi_g0          :: !Double
  , nfi_fsr         :: !Double  -- ^ fire spread rate (col avg)
  , nfi_fd          :: !Double  -- ^ fire duration (col avg) [s]
  , nfi_forc_wind   :: !Double  -- ^ wind speed [m/s]
  , nfi_secsphr     :: !Double
  } deriving (Show)

data NaturalFireOutput = NaturalFireOutput
  { nfo_nfire        :: !Double  -- ^ fire count
  , nfo_farea_burned :: !Double  -- ^ fractional area burned (natural component)
  } deriving (Show)

-- | Calculate natural fire burned area for a single non-tropical column.
calcNaturalBurnedArea :: NaturalFireInput -> NaturalFireOutput
calcNaturalBurnedArea inp =
  let !fb = max 0.0 (min 1.0 ((nfi_fuelc inp - nfi_lfuel inp)
                              / (nfi_ufuel inp - nfi_lfuel inp)))
      !m = max 0.0 (nfi_wf inp)
      !fire_m = exp (-pi * (m / 0.69) ** 2)
              * (1.0 - max 0.0 (min 1.0 ((nfi_forc_rh inp - nfi_rh_low inp)
                                         / (nfi_rh_hgh inp - nfi_rh_low inp))))
              * min 1.0 (exp (pi * (nfi_forc_t inp - nfi_tfrz inp) / 10.0))
      !lh = nfi_pot_hmn_ign inp * 6.8 * nfi_hdm inp ** 0.43 / 30.0 / 24.0
      !fs = 1.0 - (0.01 + 0.98 * exp (-0.025 * nfi_hdm inp))
      !ig = (lh + nfi_lnfm inp / (5.16 + 2.16 * cos (3.0 * nfi_lat inp))
             * nfi_ign_eff inp) * (1.0 - fs) * (1.0 - nfi_cropf inp)
      !nfire = ig / nfi_secsphr inp * fb * fire_m * nfi_lgdp inp
      !lb_lf = 1.0 + 10.0 * (1.0 - exp (-0.06 * nfi_forc_wind inp))
      !spread_m = if nfi_wtlf inp > 0.0
                  then (1.0 - max 0.0 (min 1.0
                        ((nfi_btran_col inp / nfi_wtlf inp - nfi_bt_min inp)
                         / (nfi_bt_max inp - nfi_bt_min inp))))
                     * (1.0 - max 0.0 (min 1.0
                        ((nfi_forc_rh inp - nfi_rh_low inp)
                         / (nfi_rh_hgh inp - nfi_rh_low inp))))
                  else 0.0
      !ba = (nfi_g0 inp * spread_m * nfi_fsr inp * nfi_fd inp / 1000.0) ** 2
          * nfi_lgdp1 inp * nfi_lpop inp * nfire * pi * lb_lf
  in NaturalFireOutput
    { nfo_nfire = nfire
    , nfo_farea_burned = ba
    }
