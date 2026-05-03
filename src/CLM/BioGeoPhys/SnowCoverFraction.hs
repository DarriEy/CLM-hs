{-# LANGUAGE BangPatterns #-}
-- | Snow cover fraction parameterizations.
-- Fortran: SnowCoverFractionBaseMod.F90,
--          SnowCoverFractionSwensonLawrence2012Mod.F90,
--          SnowCoverFractionNiuYang2007Mod.F90,
--          SnowCoverFractionFactoryMod.F90
-- Julia:   src/biogeophys/snow_cover_fraction.jl
--
-- Provides two concrete parameterizations via a sum type:
--   - SwensonLawrence2012 (default in CLM5)
--   - NiuYang2007
--
-- All functions are pure.
--
module CLM.BioGeoPhys.SnowCoverFraction
  ( -- * Snow cover fraction method
    SCFMethod(..)
    -- * SwensonLawrence2012 parameters
  , SL2012Params(..)
  , defaultSL2012Params
    -- * NiuYang2007 parameters
  , NY2007Params(..)
  , defaultNY2007Params
    -- * Input / output records
  , SCFInput(..)
  , SCFOutput(..)
  , IntSnowInput(..)
  , IntSnowOutput(..)
  , FracSnoEffInput(..)
    -- * Pure computation functions
  , updateSnowDepthAndFrac
  , addNewsnowToIntsnow
  , fracSnowDuringMelt
  , calcFracSnoEff
    -- * Initialization
  , initNMelt
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.LandunitConstants (istdlak)

-- =========================================================================
-- Method tag
-- =========================================================================

-- | Snow cover fraction method.
data SCFMethod
  = SwensonLawrence2012
  | NiuYang2007
  deriving (Show, Eq)

-- =========================================================================
-- SwensonLawrence2012 parameters
-- =========================================================================

-- | Parameters for the SwensonLawrence2012 parameterization.
data SL2012Params = SL2012Params
  { sl_int_snow_max :: !Double            -- ^ Limit on integrated snowfall during melt [mm H2O]
  , sl_accum_factor :: !Double            -- ^ Accumulation constant for fsca [unitless]
  , sl_n_melt       :: !(VU.Vector Double) -- ^ SCA shape parameter per column [unitless]
  } deriving (Show)

-- | Default SwensonLawrence2012 parameters (no columns initialized).
defaultSL2012Params :: SL2012Params
defaultSL2012Params = SL2012Params
  { sl_int_snow_max = 2000.0
  , sl_accum_factor = 0.1
  , sl_n_melt       = VU.empty
  }

-- | Initialize n_melt vector from topographic standard deviation.
-- Ported from Init/SetDerivedParameters in
-- SnowCoverFractionSwensonLawrence2012Mod.F90.
initNMelt
  :: Int               -- ^ Number of columns
  -> VU.Vector Double  -- ^ col_topo_std per column
  -> Double            -- ^ n_melt_coef (default 200.0)
  -> VU.Vector Double  -- ^ n_melt per column
initNMelt ncols topoStd nMeltCoef =
  VU.generate ncols $ \c ->
    nMeltCoef / max 10.0 (topoStd VU.! c)

-- =========================================================================
-- NiuYang2007 parameters
-- =========================================================================

-- | Parameters for the NiuYang2007 parameterization.
data NY2007Params = NY2007Params
  { ny_zlnd :: !Double  -- ^ Roughness length for soil [m]
  } deriving (Show)

-- | Default NiuYang2007 parameters.
defaultNY2007Params :: NY2007Params
defaultNY2007Params = NY2007Params { ny_zlnd = 0.01 }

-- =========================================================================
-- Input / Output records
-- =========================================================================

-- | Input for updateSnowDepthAndFrac (single column).
data SCFInput = SCFInput
  { scfi_h2osno_total  :: !Double  -- ^ Total SWE [mm H2O]
  , scfi_snowmelt      :: !Double  -- ^ Snow melt rate [kg/m2/s]
  , scfi_int_snow      :: !Double  -- ^ Integrated snowfall [mm H2O]
  , scfi_newsnow       :: !Double  -- ^ New snowfall [kg/m2]
  , scfi_bifall        :: !Double  -- ^ Bulk density of new snow [kg/m3]
  , scfi_frac_sno      :: !Double  -- ^ Current frac_sno
  , scfi_snow_depth    :: !Double  -- ^ Current snow depth [m]
  , scfi_lun_itype     :: !Int     -- ^ Landunit type
  , scfi_urbpoi        :: !Bool    -- ^ Urban point flag
  , scfi_use_subgrid_fluxes :: !Bool -- ^ Use subgrid fluxes
  } deriving (Show)

-- | Output of updateSnowDepthAndFrac (single column).
data SCFOutput = SCFOutput
  { scfo_frac_sno     :: !Double  -- ^ Updated frac_sno
  , scfo_frac_sno_eff :: !Double  -- ^ Effective frac_sno
  , scfo_snow_depth   :: !Double  -- ^ Updated snow depth [m]
  } deriving (Show)

-- | Input for addNewsnowToIntsnow (single column).
data IntSnowInput = IntSnowInput
  { isi_int_snow     :: !Double  -- ^ Current integrated snowfall [mm H2O]
  , isi_newsnow      :: !Double  -- ^ New snowfall [kg/m2]
  , isi_h2osno_total :: !Double  -- ^ Total SWE [mm H2O]
  , isi_frac_sno     :: !Double  -- ^ Current frac_sno
  } deriving (Show)

-- | Output of addNewsnowToIntsnow (single column).
newtype IntSnowOutput = IntSnowOutput
  { iso_int_snow :: Double  -- ^ Updated integrated snowfall [mm H2O]
  } deriving (Show)

-- | Input for calcFracSnoEff (single column).
data FracSnoEffInput = FracSnoEffInput
  { fsei_frac_sno    :: !Double  -- ^ frac_sno
  , fsei_lun_itype   :: !Int     -- ^ Landunit type
  , fsei_urbpoi      :: !Bool    -- ^ Urban point
  , fsei_use_subgrid :: !Bool    -- ^ Use subgrid fluxes
  } deriving (Show)

-- =========================================================================
-- CalcFracSnoEff (shared by both methods)
-- =========================================================================

-- | Calculate effective snow-covered fraction.
-- For urban/lake or when use_subgrid_fluxes is false, frac_sno_eff is 0 or 1.
-- Otherwise it equals frac_sno.
-- Ported from CalcFracSnoEff in SnowCoverFractionBaseMod.F90.
calcFracSnoEff :: FracSnoEffInput -> Double
calcFracSnoEff inp
  | allowFractional = fsei_frac_sno inp
  | fsei_frac_sno inp > 0.0 = 1.0
  | otherwise = 0.0
  where
    allowFractional = not (fsei_urbpoi inp)
                   && fsei_lun_itype inp /= istdlak
                   && fsei_use_subgrid inp

-- =========================================================================
-- FracSnowDuringMelt
-- =========================================================================

-- | Single-point fractional snow cover during melt.
-- Ported from FracSnowDuringMelt in
-- SnowCoverFractionSwensonLawrence2012Mod.F90.
fracSnowDuringMelt
  :: SL2012Params
  -> Int     -- ^ Column index (0-based into n_melt)
  -> Double  -- ^ h2osno_total
  -> Double  -- ^ int_snow
  -> Double  -- ^ frac_sno during melt
fracSnowDuringMelt params c h2osno_total int_snow_val =
  let int_snow_limited = min int_snow_val (sl_int_snow_max params)
      smr = min 1.0 (h2osno_total / int_snow_limited)
      n_melt_c = sl_n_melt params VU.! c
  in 1.0 - (acos (min 1.0 (2.0 * smr - 1.0)) / pi) ** n_melt_c

-- =========================================================================
-- UpdateSnowDepthAndFrac
-- =========================================================================

-- | Update snow depth and snow fraction for a single column.
-- Dispatches on SCFMethod.
-- Ported from UpdateSnowDepthAndFrac in
-- SnowCoverFractionSwensonLawrence2012Mod.F90 and
-- SnowCoverFractionNiuYang2007Mod.F90.
updateSnowDepthAndFrac
  :: SCFMethod
  -> Either SL2012Params NY2007Params  -- ^ Method parameters
  -> Int                               -- ^ Column index (0-based, for n_melt)
  -> SCFInput
  -> SCFOutput
updateSnowDepthAndFrac SwensonLawrence2012 (Left sl) c inp = sl2012Update sl c inp
updateSnowDepthAndFrac NiuYang2007 (Right ny) _c inp = ny2007Update ny inp
updateSnowDepthAndFrac _ _ _ inp = SCFOutput
  { scfo_frac_sno = scfi_frac_sno inp
  , scfo_frac_sno_eff = scfi_frac_sno inp
  , scfo_snow_depth = scfi_snow_depth inp
  }

-- | SwensonLawrence2012 update.
sl2012Update :: SL2012Params -> Int -> SCFInput -> SCFOutput
sl2012Update sl c inp = SCFOutput
  { scfo_frac_sno     = frac_sno_new
  , scfo_frac_sno_eff = frac_sno_eff_new
  , scfo_snow_depth   = snow_depth_new
  }
  where
    !h2osno    = scfi_h2osno_total inp
    !snowmelt  = scfi_snowmelt inp
    !int_snow  = scfi_int_snow inp
    !newsnow   = scfi_newsnow inp
    !bifall    = scfi_bifall inp
    !frac_sno0 = scfi_frac_sno inp
    !snow_depth0 = scfi_snow_depth inp

    -- Update frac_sno
    frac_sno_step1
      | h2osno == 0.0 =
          if newsnow > 0.0
          then tanh (sl_accum_factor sl * newsnow)
          else 0.0
      | otherwise =
          let fs1 = if snowmelt > 0.0
                    then fracSnowDuringMelt sl c h2osno int_snow
                    else frac_sno0
          in if newsnow > 0.0
             then fs1 + tanh (sl_accum_factor sl * newsnow) * (1.0 - fs1)
             else fs1

    frac_sno_new = frac_sno_step1

    -- Effective fraction
    frac_sno_eff_new = calcFracSnoEff FracSnoEffInput
      { fsei_frac_sno    = frac_sno_new
      , fsei_lun_itype   = scfi_lun_itype inp
      , fsei_urbpoi      = scfi_urbpoi inp
      , fsei_use_subgrid = scfi_use_subgrid_fluxes inp
      }

    -- Update snow depth
    snow_depth_new
      | h2osno > 0.0 =
          if frac_sno_eff_new > 0.0
          then snow_depth0 + newsnow / (bifall * frac_sno_eff_new)
          else 0.0
      | newsnow > 0.0 =
          let z_avg = newsnow / bifall
          in z_avg / frac_sno_eff_new
      | otherwise = 0.0

-- | NiuYang2007 update.
ny2007Update :: NY2007Params -> SCFInput -> SCFOutput
ny2007Update ny inp = SCFOutput
  { scfo_frac_sno     = frac_sno_new
  , scfo_frac_sno_eff = frac_sno_eff_new
  , scfo_snow_depth   = sd_new
  }
  where
    !h2osno  = scfi_h2osno_total inp
    !newsnow = scfi_newsnow inp
    !bifall  = scfi_bifall inp
    sd0      = scfi_snow_depth inp

    sd_reset | h2osno == 0.0 = 0.0
             | otherwise     = sd0

    sd_new = sd_reset + newsnow / bifall

    frac_sno_raw
      | sd_new > 0.0 =
          let dens = min 800.0 ((h2osno + newsnow) / sd_new)
          in tanh (sd_new / (2.5 * ny_zlnd ny * (dens / 100.0)))
      | otherwise = 0.0

    frac_sno_new
      | h2osno > 0.0 && h2osno < 1.0 = min frac_sno_raw h2osno
      | otherwise = frac_sno_raw

    frac_sno_eff_new = calcFracSnoEff FracSnoEffInput
      { fsei_frac_sno    = frac_sno_new
      , fsei_lun_itype   = scfi_lun_itype inp
      , fsei_urbpoi      = scfi_urbpoi inp
      , fsei_use_subgrid = scfi_use_subgrid_fluxes inp
      }

-- =========================================================================
-- AddNewsnowToIntsnow
-- =========================================================================

-- | Add new snow to integrated snowfall for a single column.
-- Dispatches on SCFMethod.
-- Ported from AddNewsnowToIntsnow in the respective Fortran modules.
addNewsnowToIntsnow
  :: SCFMethod
  -> Either SL2012Params NY2007Params
  -> Int                -- ^ Column index (0-based)
  -> IntSnowInput
  -> IntSnowOutput
addNewsnowToIntsnow SwensonLawrence2012 (Left sl) c inp = sl2012IntSnow sl c inp
addNewsnowToIntsnow NiuYang2007 _ _ inp = ny2007IntSnow inp
addNewsnowToIntsnow _ _ _ inp = IntSnowOutput { iso_int_snow = isi_int_snow inp }

-- | SwensonLawrence2012 int_snow update.
sl2012IntSnow :: SL2012Params -> Int -> IntSnowInput -> IntSnowOutput
sl2012IntSnow sl c inp = IntSnowOutput { iso_int_snow = int_snow_final }
  where
    !int_snow0  = isi_int_snow inp
    !newsnow    = isi_newsnow inp
    !h2osno_tot = isi_h2osno_total inp
    !frac_sno   = isi_frac_sno inp
    n_melt_c    = sl_n_melt sl VU.! c

    int_snow_reset
      | newsnow > 0.0 =
          let temp = (h2osno_tot + newsnow) /
                     (0.5 * (cos (pi * (1.0 - max frac_sno 1.0e-6) ** (1.0 / n_melt_c)) + 1.0))
          in min 1.0e8 temp
      | otherwise = int_snow0

    int_snow_final = int_snow_reset + newsnow

-- | NiuYang2007 int_snow update (simple addition).
ny2007IntSnow :: IntSnowInput -> IntSnowOutput
ny2007IntSnow inp = IntSnowOutput
  { iso_int_snow = isi_int_snow inp + isi_newsnow inp }
