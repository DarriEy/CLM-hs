{-# LANGUAGE BangPatterns #-}
-- | Saturation-excess surface runoff.
-- Fortran: SaturatedExcessRunoffMod.F90
--
-- Provides:
--   * TOPModel-based saturated fraction (fsat) computation
--   * VIC-based saturated fraction computation
--   * Saturated excess surface runoff
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SatExcessRunoff
  ( -- * Parameters
    SatExcessRunoffParams(..)
  , defaultSatExcessParams
    -- * Method constants
  , FsatMethod(..)
    -- * Input/output records
  , FsatTopmodelInput(..)
  , FsatVicInput(..)
  , SatExcessRunoffInput(..)
  , SatExcessRunoffResult(..)
    -- * Science functions
  , computeFsatTopmodel
  , computeFsatVic
  , saturatedExcessRunoff
  ) where

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

-- | Parameters for saturated excess runoff (from params file)
data SatExcessRunoffParams = SatExcessRunoffParams
  { serp_fff :: !Double  -- ^ Decay factor for fractional saturated area [1/m]
  } deriving (Show, Eq)

-- | Default parameters (fff must be read from params file)
defaultSatExcessParams :: SatExcessRunoffParams
defaultSatExcessParams = SatExcessRunoffParams
  { serp_fff = 0.5
  }

-- | Method for computing fsat
data FsatMethod
  = FsatTopmodel  -- ^ TOPModel-based (CLM default)
  | FsatVic       -- ^ VIC-based
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Inputs for TOPModel fsat computation
data FsatTopmodelInput = FsatTopmodelInput
  { fti_frost_table  :: !Double  -- ^ Frost table depth [m]
  , fti_zwt          :: !Double  -- ^ Water table depth [m]
  , fti_zwt_perched  :: !Double  -- ^ Perched water table depth [m]
  , fti_wtfact       :: !Double  -- ^ Maximum fractional saturated area
  , fti_fff          :: !Double  -- ^ Decay factor [1/m]
  } deriving (Show)

-- | Inputs for VIC fsat computation
data FsatVicInput = FsatVicInput
  { fvi_b_infil            :: !Double  -- ^ VIC infiltration parameter
  , fvi_top_max_moist      :: !Double  -- ^ Maximum moisture in top layers
  , fvi_top_moist_limited  :: !Double  -- ^ Current moisture in top layers (limited)
  } deriving (Show)

-- | Inputs for the full saturated excess runoff computation
data SatExcessRunoffInput = SatExcessRunoffInput
  { seri_fsat_method             :: !FsatMethod
  , seri_topmodel_input          :: !(Maybe FsatTopmodelInput)
  , seri_vic_input               :: !(Maybe FsatVicInput)
  , seri_qflx_rain_plus_snomelt  :: !Double  -- ^ Rain + snowmelt flux [mm/s]
  , seri_qflx_floodc             :: !Double  -- ^ Flood water flux [mm/s]
  , seri_is_crop                 :: !Bool    -- ^ True if crop landunit
  , seri_is_urban                :: !Bool    -- ^ True if urban column
  , seri_crop_fsat_equals_zero   :: !Bool    -- ^ Zero fsat for crops
  } deriving (Show)

-- | Result of saturated excess runoff computation
data SatExcessRunoffResult = SatExcessRunoffResult
  { serr_fsat                 :: !Double  -- ^ Fractional saturated area
  , serr_fcov                 :: !Double  -- ^ Fractional impermeable area (= fsat)
  , serr_qflx_sat_excess_surf :: !Double  -- ^ Saturation excess runoff [mm/s]
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute fsat using the TOPModel-based parameterization (CLM default).
-- If the frost table is between the perched water table and the main water
-- table, uses the perched water table depth; otherwise uses the main water
-- table depth.
-- Ported from ComputeFsatTopmodel in SaturatedExcessRunoffMod.F90
computeFsatTopmodel :: FsatTopmodelInput -> Double
computeFsatTopmodel inp =
  let ft  = fti_frost_table inp
      zwt = fti_zwt inp
      zwp = fti_zwt_perched inp
      wf  = fti_wtfact inp
      fff = fti_fff inp
  in if ft > zwp && ft <= zwt
     then wf * exp (negate 0.5 * fff * zwp)
     else wf * exp (negate 0.5 * fff * zwt)

-- | Compute fsat using the VIC-based parameterization.
-- Ported from ComputeFsatVic in SaturatedExcessRunoffMod.F90
computeFsatVic :: FsatVicInput -> Double
computeFsatVic inp =
  let ex = fvi_b_infil inp / (1.0 + fvi_b_infil inp)
  in 1.0 - (1.0 - fvi_top_moist_limited inp / fvi_top_max_moist inp) ** ex

-- | Calculate surface runoff due to saturated surface excess.
-- Ported from SaturatedExcessRunoff in SaturatedExcessRunoffMod.F90
saturatedExcessRunoff
  :: SatExcessRunoffParams
  -> SatExcessRunoffInput
  -> SatExcessRunoffResult
saturatedExcessRunoff _params inp =
  let -- Step 1: Compute fsat
      fsat0 = case seri_fsat_method inp of
        FsatTopmodel -> case seri_topmodel_input inp of
          Just ti -> computeFsatTopmodel ti
          Nothing -> 0.0
        FsatVic -> case seri_vic_input inp of
          Just vi -> computeFsatVic vi
          Nothing -> 0.0

      -- Step 2: Zero fsat for crop columns if requested
      fsat1 = if seri_crop_fsat_equals_zero inp && seri_is_crop inp
              then 0.0
              else fsat0

      -- Step 3: Compute saturation excess runoff
      qflx_sat_excess = fsat1 * seri_qflx_rain_plus_snomelt inp

      -- Step 4: Add flood water flux for urban columns
      qflx_final = if seri_is_urban inp
                   then qflx_sat_excess + seri_qflx_floodc inp
                   else qflx_sat_excess

  in SatExcessRunoffResult
       { serr_fsat                 = fsat1
       , serr_fcov                 = fsat1
       , serr_qflx_sat_excess_surf = qflx_final
       }
