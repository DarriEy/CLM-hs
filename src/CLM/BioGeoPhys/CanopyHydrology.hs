{-# LANGUAGE BangPatterns #-}
-- | Canopy hydrology: interception, throughfall, drip.
-- Fortran: CanopyHydrologyMod.F90
--
-- Provides:
--   * Canopy hydrology parameters
--   * Top-of-canopy precipitation inputs
--   * Canopy interception and throughfall (bulk and tracer)
--   * Canopy excess (drip) computation
--   * Snow unloading from canopy
--   * Fluxes onto ground (p2c aggregation)
--   * Fraction wet/dry diagnostic
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.CanopyHydrology
  ( -- * Parameters
    CanopyHydrologyParams(..)
  , defaultCanopyHydroParams
    -- * Input/output records
  , TopOfCanopyInput(..)
  , TopOfCanopyResult(..)
  , InterceptionInput(..)
  , InterceptionResult(..)
  , CanopyExcessInput(..)
  , CanopyExcessResult(..)
  , SnowUnloadingInput(..)
  , SnowUnloadingResult(..)
  , FluxesOntoGroundInput(..)
  , FluxesOntoGroundResult(..)
  , FracWetInput(..)
  , FracWetResult(..)
  , CanopyHydrologyInput(..)
  , CanopyHydrologyResult(..)
    -- * Science functions
  , sumFluxTopOfCanopyInputs
  , bulkFluxCanopyInterceptionAndThroughfall
  , updateStateAddInterceptionToCanopy
  , bulkFluxCanopyExcess
  , updateStateRemoveCanfallFromCanopy
  , bulkFluxSnowUnloading
  , updateStateRemoveSnowUnloading
  , sumFluxFluxesOntoGround
  , bulkDiagFracWet
  , canopyInterceptionAndThroughfall
  ) where

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Column types for urban wall exclusion
icolSunwall :: Int
icolSunwall = 61

icolShadewall :: Int
icolShadewall = 62

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

-- | Canopy hydrology parameters (from params file)
data CanopyHydrologyParams = CanopyHydrologyParams
  { chp_liq_canopy_storage_scalar   :: !Double  -- ^ Canopy liquid storage [kg/m2]
  , chp_snow_canopy_storage_scalar  :: !Double  -- ^ Canopy snow storage [kg/m2]
  , chp_snowcan_unload_temp_fact    :: !Double  -- ^ Temperature unloading factor [K*s]
  , chp_snowcan_unload_wind_fact    :: !Double  -- ^ Wind unloading timescale [m]
  , chp_interception_fraction       :: !Double  -- ^ Fraction of intercepted precip [-]
  , chp_maximum_leaf_wetted_fraction :: !Double -- ^ Max fraction of leaf that may be wet [-]
  , chp_use_clm5_fpi                :: !Bool    -- ^ Use CLM5 interception formula
  } deriving (Show, Eq)

-- | Default parameters
-- | Generic CLM5 fallback defaults. Case-specific values are read from the CLM
-- parameter file (chyd_*.bin) by 'readCanopyHydroParamsFromDir'; these defaults
-- apply only when a param file is absent. maximum_leaf_wetted_fraction caps fwet,
-- which scales canopy (snow) evaporation; the prior default of 1.0 let an
-- all-snow canopy reach fwet=1 and sublimate ~14x too fast vs Fortran (whose
-- canopy QVEGE is ~0 and sheds snow by unloading, not sublimation).
defaultCanopyHydroParams :: CanopyHydrologyParams
defaultCanopyHydroParams = CanopyHydrologyParams
  { chp_liq_canopy_storage_scalar   = 0.1
  , chp_snow_canopy_storage_scalar  = 6.0
  , chp_snowcan_unload_temp_fact    = 1.87e5
  , chp_snowcan_unload_wind_fact    = 1.56e5
  , chp_interception_fraction       = 1.0
  , chp_maximum_leaf_wetted_fraction = 0.05
  , chp_use_clm5_fpi                = False
  }

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Inputs for top-of-canopy precipitation
data TopOfCanopyInput = TopOfCanopyInput
  { tci_forc_rain              :: !Double  -- ^ Rain rate [mm/s] (column)
  , tci_forc_snow_col          :: !Double  -- ^ Snow rate [mm/s] (column)
  , tci_qflx_irrig_sprinkler   :: !Double  -- ^ Sprinkler irrigation [mm/s] (patch)
  } deriving (Show)

-- | Result of top-of-canopy
data TopOfCanopyResult = TopOfCanopyResult
  { tcr_qflx_liq_above_canopy :: !Double  -- ^ Liquid above canopy [mm/s]
  , tcr_forc_snow_patch        :: !Double  -- ^ Snow at patch level [mm/s]
  } deriving (Show)

-- | Inputs for canopy interception and throughfall
data InterceptionInput = InterceptionInput
  { ii_frac_veg_nosno          :: !Int     -- ^ Vegetation fraction flag (0 or 1)
  , ii_elai                    :: !Double  -- ^ Exposed LAI
  , ii_esai                    :: !Double  -- ^ Exposed SAI
  , ii_forc_snow               :: !Double  -- ^ Snow at patch [mm/s]
  , ii_qflx_liq_above_canopy   :: !Double  -- ^ Liquid above canopy [mm/s]
  , ii_col_itype               :: !Int     -- ^ Column type
  , ii_params                  :: !CanopyHydrologyParams
  } deriving (Show)

-- | Result of interception
data InterceptionResult = InterceptionResult
  { ir_qflx_through_snow        :: !Double  -- ^ Snow throughfall [mm/s]
  , ir_qflx_through_liq         :: !Double  -- ^ Liquid throughfall [mm/s]
  , ir_qflx_intercepted_snow    :: !Double  -- ^ Intercepted snow [mm/s]
  , ir_qflx_intercepted_liq     :: !Double  -- ^ Intercepted liquid [mm/s]
  , ir_check_interception_excess :: !Bool   -- ^ Flag: should compute canopy excess
  } deriving (Show)

-- | Inputs for canopy excess computation
data CanopyExcessInput = CanopyExcessInput
  { cei_elai        :: !Double  -- ^ Exposed LAI
  , cei_esai        :: !Double  -- ^ Exposed SAI
  , cei_snocan      :: !Double  -- ^ Canopy snow [kg/m2]
  , cei_liqcan      :: !Double  -- ^ Canopy liquid [kg/m2]
  , cei_dtime       :: !Double  -- ^ Time step [s]
  , cei_check_flag  :: !Bool    -- ^ Interception/excess flag
  , cei_params      :: !CanopyHydrologyParams
  } deriving (Show)

-- | Result of canopy excess
data CanopyExcessResult = CanopyExcessResult
  { cer_qflx_snocanfall :: !Double  -- ^ Snow canopy fall [mm/s]
  , cer_qflx_liqcanfall :: !Double  -- ^ Liquid canopy fall [mm/s]
  } deriving (Show)

-- | Inputs for snow unloading
data SnowUnloadingInput = SnowUnloadingInput
  { sui_frac_veg_nosno :: !Int     -- ^ Vegetation fraction flag
  , sui_snocan         :: !Double  -- ^ Canopy snow [kg/m2]
  , sui_forc_t         :: !Double  -- ^ Forcing temperature [K] (column)
  , sui_forc_wind      :: !Double  -- ^ Forcing wind speed [m/s] (gridcell)
  , sui_dtime          :: !Double  -- ^ Time step [s]
  , sui_params         :: !CanopyHydrologyParams
  } deriving (Show)

-- | Result of snow unloading
data SnowUnloadingResult = SnowUnloadingResult
  { sur_qflx_snotempunload :: !Double  -- ^ Temperature unloading [mm/s]
  , sur_qflx_snowindunload :: !Double  -- ^ Wind unloading [mm/s]
  , sur_qflx_snow_unload   :: !Double  -- ^ Total unloading [mm/s]
  } deriving (Show)

-- | Inputs for fluxes onto ground aggregation
data FluxesOntoGroundInput = FluxesOntoGroundInput
  { fgi_qflx_through_snow :: !Double  -- ^ Snow throughfall [mm/s]
  , fgi_qflx_snocanfall   :: !Double  -- ^ Snow canopy fall [mm/s]
  , fgi_qflx_snow_unload  :: !Double  -- ^ Snow unloading [mm/s]
  , fgi_qflx_through_liq  :: !Double  -- ^ Liquid throughfall [mm/s]
  , fgi_qflx_liqcanfall   :: !Double  -- ^ Liquid canopy fall [mm/s]
  , fgi_qflx_irrig_drip   :: !Double  -- ^ Drip irrigation [mm/s]
  , fgi_wtcol              :: !Double  -- ^ Patch weight on column
  } deriving (Show)

-- | Result of fluxes onto ground (column-level)
data FluxesOntoGroundResult = FluxesOntoGroundResult
  { fgr_qflx_snow_grnd  :: !Double  -- ^ Snow flux onto ground [mm/s]
  , fgr_qflx_liq_grnd   :: !Double  -- ^ Liquid flux onto ground [mm/s]
  , fgr_qflx_snow_h2osfc :: !Double -- ^ Snow onto surface water [mm/s]
  } deriving (Show)

-- | Inputs for fraction wet/dry diagnostic
data FracWetInput = FracWetInput
  { fwi_frac_veg_nosno :: !Int     -- ^ Vegetation fraction flag
  , fwi_elai           :: !Double  -- ^ Exposed LAI
  , fwi_esai           :: !Double  -- ^ Exposed SAI
  , fwi_snocan         :: !Double  -- ^ Canopy snow [kg/m2]
  , fwi_liqcan         :: !Double  -- ^ Canopy liquid [kg/m2]
  , fwi_params         :: !CanopyHydrologyParams
  } deriving (Show)

-- | Result of fraction wet/dry
data FracWetResult = FracWetResult
  { fwr_fwet    :: !Double  -- ^ Fraction of canopy that is wet
  , fwr_fdry    :: !Double  -- ^ Fraction of canopy that is dry
  , fwr_fcansno :: !Double  -- ^ Fraction of canopy covered by snow
  } deriving (Show)

-- | Full canopy hydrology input (single patch)
data CanopyHydrologyInput = CanopyHydrologyInput
  { chi_params                :: !CanopyHydrologyParams
  , chi_dtime                 :: !Double
  , chi_frac_veg_nosno        :: !Int
  , chi_elai                  :: !Double
  , chi_esai                  :: !Double
  , chi_forc_rain             :: !Double  -- ^ column
  , chi_forc_snow_col         :: !Double  -- ^ column
  , chi_forc_t                :: !Double  -- ^ column
  , chi_forc_wind             :: !Double  -- ^ gridcell
  , chi_col_itype             :: !Int
  , chi_qflx_irrig_sprinkler  :: !Double
  , chi_qflx_irrig_drip       :: !Double
  , chi_snocan_in             :: !Double  -- ^ Canopy snow state [kg/m2]
  , chi_liqcan_in             :: !Double  -- ^ Canopy liquid state [kg/m2]
  , chi_wtcol                 :: !Double  -- ^ Patch weight on column
  } deriving (Show)

-- | Full canopy hydrology result (single patch + column contributions)
data CanopyHydrologyResult = CanopyHydrologyResult
  { chr_snocan              :: !Double  -- ^ Updated canopy snow [kg/m2]
  , chr_liqcan              :: !Double  -- ^ Updated canopy liquid [kg/m2]
  , chr_fwet                :: !Double
  , chr_fdry                :: !Double
  , chr_fcansno             :: !Double
  , chr_qflx_through_snow   :: !Double
  , chr_qflx_through_liq    :: !Double
  , chr_qflx_intercepted_snow :: !Double
  , chr_qflx_intercepted_liq  :: !Double
  , chr_qflx_snocanfall     :: !Double
  , chr_qflx_liqcanfall     :: !Double
  , chr_qflx_snow_unload    :: !Double
  , chr_qflx_snotempunload  :: !Double
  , chr_qflx_snowindunload  :: !Double
  , chr_qflx_snow_grnd_col  :: !Double  -- ^ Column-level snow onto ground [mm/s]
  , chr_qflx_liq_grnd_col   :: !Double  -- ^ Column-level liquid onto ground [mm/s]
  , chr_qflx_snow_h2osfc_col :: !Double
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute patch-level precipitation inputs for bulk water.
-- Ported from SumFlux_TopOfCanopyInputs in CanopyHydrologyMod.F90
sumFluxTopOfCanopyInputs :: TopOfCanopyInput -> TopOfCanopyResult
sumFluxTopOfCanopyInputs inp = TopOfCanopyResult
  { tcr_qflx_liq_above_canopy = tci_forc_rain inp + tci_qflx_irrig_sprinkler inp
  , tcr_forc_snow_patch        = tci_forc_snow_col inp
  }

-- | Compute canopy interception and throughfall for bulk water.
-- Ported from BulkFlux_CanopyInterceptionAndThroughfall in CanopyHydrologyMod.F90
bulkFluxCanopyInterceptionAndThroughfall :: InterceptionInput -> InterceptionResult
bulkFluxCanopyInterceptionAndThroughfall inp =
  let fvn = ii_frac_veg_nosno inp
      total_precip = ii_forc_snow inp + ii_qflx_liq_above_canopy inp
      check = fvn == 1 && total_precip > 0.0
  in if check
     then
       let params = ii_params inp
           lai_sai = ii_elai inp + ii_esai inp
           fpiliq = if chp_use_clm5_fpi params
                    then chp_interception_fraction params * tanh lai_sai
                    else 0.25 * (1.0 - exp (negate 0.5 * lai_sai))
           fpisnow = 1.0 - exp (negate 0.5 * lai_sai)
       in InterceptionResult
            { ir_qflx_through_snow     = ii_forc_snow inp * (1.0 - fpisnow)
            , ir_qflx_through_liq      = ii_qflx_liq_above_canopy inp * (1.0 - fpiliq)
            , ir_qflx_intercepted_snow = ii_forc_snow inp * fpisnow
            , ir_qflx_intercepted_liq  = ii_qflx_liq_above_canopy inp * fpiliq
            , ir_check_interception_excess = True
            }
     else
       let itype = ii_col_itype inp
       in if itype == icolSunwall || itype == icolShadewall
          then InterceptionResult
                 { ir_qflx_through_snow     = 0.0
                 , ir_qflx_through_liq      = 0.0
                 , ir_qflx_intercepted_snow = 0.0
                 , ir_qflx_intercepted_liq  = 0.0
                 , ir_check_interception_excess = False
                 }
          else InterceptionResult
                 { ir_qflx_through_snow     = ii_forc_snow inp
                 , ir_qflx_through_liq      = ii_qflx_liq_above_canopy inp
                 , ir_qflx_intercepted_snow = 0.0
                 , ir_qflx_intercepted_liq  = 0.0
                 , ir_check_interception_excess = False
                 }

-- | Update snocan and liqcan based on interception.
-- Ported from UpdateState_AddInterceptionToCanopy in CanopyHydrologyMod.F90
updateStateAddInterceptionToCanopy
  :: Double  -- ^ dtime
  -> Double  -- ^ qflx_intercepted_snow
  -> Double  -- ^ qflx_intercepted_liq
  -> Double  -- ^ snocan_in
  -> Double  -- ^ liqcan_in
  -> (Double, Double)  -- ^ (snocan_out, liqcan_out)
updateStateAddInterceptionToCanopy dtime qi_snow qi_liq snocan liqcan =
  ( max 0.0 (snocan + dtime * qi_snow)
  , max 0.0 (liqcan + dtime * qi_liq)
  )

-- | Compute canopy excess (drip) for bulk water.
-- Ported from BulkFlux_CanopyExcess in CanopyHydrologyMod.F90
bulkFluxCanopyExcess :: CanopyExcessInput -> CanopyExcessResult
bulkFluxCanopyExcess inp
  | not (cei_check_flag inp) =
      CanopyExcessResult { cer_qflx_snocanfall = 0.0, cer_qflx_liqcanfall = 0.0 }
  | otherwise =
      let params = cei_params inp
          lai_sai = cei_elai inp + cei_esai inp
          liqcanmx = chp_liq_canopy_storage_scalar params * lai_sai
          snocanmx = chp_snow_canopy_storage_scalar params * lai_sai
          dtime_val = cei_dtime inp
      in CanopyExcessResult
           { cer_qflx_liqcanfall = max ((cei_liqcan inp - liqcanmx) / dtime_val) 0.0
           , cer_qflx_snocanfall = max ((cei_snocan inp - snocanmx) / dtime_val) 0.0
           }

-- | Remove canfall from canopy state.
-- Ported from UpdateState_RemoveCanfallFromCanopy in CanopyHydrologyMod.F90
updateStateRemoveCanfallFromCanopy
  :: Double  -- ^ dtime
  -> Double  -- ^ qflx_liqcanfall
  -> Double  -- ^ qflx_snocanfall
  -> Double  -- ^ liqcan_in
  -> Double  -- ^ snocan_in
  -> (Double, Double)  -- ^ (liqcan_out, snocan_out)
updateStateRemoveCanfallFromCanopy dtime qliq qsno liqcan snocan =
  ( liqcan - dtime * qliq
  , snocan - dtime * qsno
  )

-- | Compute snow unloading for bulk water.
-- Ported from BulkFlux_SnowUnloading in CanopyHydrologyMod.F90
bulkFluxSnowUnloading :: SnowUnloadingInput -> SnowUnloadingResult
bulkFluxSnowUnloading inp
  | sui_frac_veg_nosno inp == 1 && sui_snocan inp > 0.0 =
      let params = sui_params inp
          snocan_val = sui_snocan inp
          forc_t_val = sui_forc_t inp
          forc_wind_val = sui_forc_wind inp
          dtime_val = sui_dtime inp
          qtempunload = max 0.0 (snocan_val * (forc_t_val - 270.15) /
                                  chp_snowcan_unload_temp_fact params)
          qwindunload = snocan_val * forc_wind_val /
                        chp_snowcan_unload_wind_fact params
          qtotal = min (qtempunload + qwindunload) (snocan_val / dtime_val)
      in SnowUnloadingResult
           { sur_qflx_snotempunload = qtempunload
           , sur_qflx_snowindunload = qwindunload
           , sur_qflx_snow_unload   = qtotal
           }
  | otherwise =
      SnowUnloadingResult
        { sur_qflx_snotempunload = 0.0
        , sur_qflx_snowindunload = 0.0
        , sur_qflx_snow_unload   = 0.0
        }

-- | Remove snow unloading from canopy.
-- Ported from UpdateState_RemoveSnowUnloading in CanopyHydrologyMod.F90
updateStateRemoveSnowUnloading :: Double -> Double -> Double -> Double
updateStateRemoveSnowUnloading dtime qflx_snow_unload snocan =
  if qflx_snow_unload == snocan / dtime
  then 0.0
  else snocan - qflx_snow_unload * dtime

-- | Compute summed fluxes onto ground for a single patch.
-- Returns (snow_grnd_patch, liq_grnd_patch) before p2c weighting.
-- Ported from SumFlux_FluxesOntoGround in CanopyHydrologyMod.F90
sumFluxFluxesOntoGround :: FluxesOntoGroundInput -> FluxesOntoGroundResult
sumFluxFluxesOntoGround inp =
  let snow_grnd = (fgi_qflx_through_snow inp + fgi_qflx_snocanfall inp +
                   fgi_qflx_snow_unload inp) * fgi_wtcol inp
      liq_grnd  = (fgi_qflx_through_liq inp + fgi_qflx_liqcanfall inp +
                   fgi_qflx_irrig_drip inp) * fgi_wtcol inp
  in FluxesOntoGroundResult
       { fgr_qflx_snow_grnd   = snow_grnd
       , fgr_qflx_liq_grnd    = liq_grnd
       , fgr_qflx_snow_h2osfc = 0.0  -- no snow on surface water
       }

-- | Determine fraction of vegetated surface that is wet/dry.
-- Ported from BulkDiag_FracWet in CanopyHydrologyMod.F90
bulkDiagFracWet :: FracWetInput -> FracWetResult
bulkDiagFracWet inp
  | fwi_frac_veg_nosno inp == 1 =
      let h2ocan = fwi_snocan inp + fwi_liqcan inp
          params = fwi_params inp
      in if h2ocan > 0.0
         then let vegt = fromIntegral (fwi_frac_veg_nosno inp) * (fwi_elai inp + fwi_esai inp)
                  fwet_raw = (h2ocan / (vegt * chp_liq_canopy_storage_scalar params)) ** 0.666666666666
                  fwet_val = min fwet_raw (chp_maximum_leaf_wetted_fraction params)
                  fcansno_val = if fwi_snocan inp > 0.0
                                then min ((fwi_snocan inp / (vegt * chp_snow_canopy_storage_scalar params)) ** 0.15) 1.0
                                else 0.0
                  fdry_val = (1.0 - fwet_val) * fwi_elai inp / (fwi_elai inp + fwi_esai inp)
              in FracWetResult
                   { fwr_fwet    = fwet_val
                   , fwr_fdry    = fdry_val
                   , fwr_fcansno = fcansno_val
                   }
         else FracWetResult
                { fwr_fwet    = 0.0
                , fwr_fdry    = (1.0 - 0.0) * fwi_elai inp / (fwi_elai inp + fwi_esai inp)
                , fwr_fcansno = 0.0
                }
  | otherwise =
      FracWetResult { fwr_fwet = 0.0, fwr_fdry = 0.0, fwr_fcansno = 0.0 }

-- | Full canopy interception and throughfall orchestrator (single patch).
-- Coordinates all sub-routines for canopy hydrology.
-- Ported from CanopyInterceptionAndThroughfall in CanopyHydrologyMod.F90
canopyInterceptionAndThroughfall :: CanopyHydrologyInput -> CanopyHydrologyResult
canopyInterceptionAndThroughfall inp =
  let params = chi_params inp
      dtime_val = chi_dtime inp

      -- Step 1: Top-of-canopy inputs
      tc = sumFluxTopOfCanopyInputs TopOfCanopyInput
             { tci_forc_rain            = chi_forc_rain inp
             , tci_forc_snow_col        = chi_forc_snow_col inp
             , tci_qflx_irrig_sprinkler = chi_qflx_irrig_sprinkler inp
             }

      -- Step 2: Interception and throughfall
      ir = bulkFluxCanopyInterceptionAndThroughfall InterceptionInput
             { ii_frac_veg_nosno       = chi_frac_veg_nosno inp
             , ii_elai                 = chi_elai inp
             , ii_esai                 = chi_esai inp
             , ii_forc_snow            = tcr_forc_snow_patch tc
             , ii_qflx_liq_above_canopy = tcr_qflx_liq_above_canopy tc
             , ii_col_itype            = chi_col_itype inp
             , ii_params               = params
             }

      -- Step 3: Add interception to canopy
      (snocan1, liqcan1) = updateStateAddInterceptionToCanopy dtime_val
                              (ir_qflx_intercepted_snow ir) (ir_qflx_intercepted_liq ir)
                              (chi_snocan_in inp) (chi_liqcan_in inp)

      -- Step 4: Canopy excess
      ce = bulkFluxCanopyExcess CanopyExcessInput
             { cei_elai       = chi_elai inp
             , cei_esai       = chi_esai inp
             , cei_snocan     = snocan1
             , cei_liqcan     = liqcan1
             , cei_dtime      = dtime_val
             , cei_check_flag = ir_check_interception_excess ir
             , cei_params     = params
             }

      -- Step 5: Remove canfall from canopy
      (liqcan2, snocan2) = updateStateRemoveCanfallFromCanopy dtime_val
                              (cer_qflx_liqcanfall ce) (cer_qflx_snocanfall ce)
                              liqcan1 snocan1

      -- Step 6: Snow unloading
      su = bulkFluxSnowUnloading SnowUnloadingInput
             { sui_frac_veg_nosno = chi_frac_veg_nosno inp
             , sui_snocan         = snocan2
             , sui_forc_t         = chi_forc_t inp
             , sui_forc_wind      = chi_forc_wind inp
             , sui_dtime          = dtime_val
             , sui_params         = params
             }

      -- Step 7: Remove snow unloading from canopy
      snocan3 = updateStateRemoveSnowUnloading dtime_val
                  (sur_qflx_snow_unload su) snocan2

      -- Step 8: Fluxes onto ground
      fg = sumFluxFluxesOntoGround FluxesOntoGroundInput
             { fgi_qflx_through_snow = ir_qflx_through_snow ir
             , fgi_qflx_snocanfall   = cer_qflx_snocanfall ce
             , fgi_qflx_snow_unload  = sur_qflx_snow_unload su
             , fgi_qflx_through_liq  = ir_qflx_through_liq ir
             , fgi_qflx_liqcanfall   = cer_qflx_liqcanfall ce
             , fgi_qflx_irrig_drip   = chi_qflx_irrig_drip inp
             , fgi_wtcol             = chi_wtcol inp
             }

      -- Step 9: Fraction wet/dry
      fw = bulkDiagFracWet FracWetInput
             { fwi_frac_veg_nosno = chi_frac_veg_nosno inp
             , fwi_elai           = chi_elai inp
             , fwi_esai           = chi_esai inp
             , fwi_snocan         = snocan3
             , fwi_liqcan         = liqcan2
             , fwi_params         = params
             }

  in CanopyHydrologyResult
       { chr_snocan               = snocan3
       , chr_liqcan               = liqcan2
       , chr_fwet                 = fwr_fwet fw
       , chr_fdry                 = fwr_fdry fw
       , chr_fcansno              = fwr_fcansno fw
       , chr_qflx_through_snow    = ir_qflx_through_snow ir
       , chr_qflx_through_liq     = ir_qflx_through_liq ir
       , chr_qflx_intercepted_snow = ir_qflx_intercepted_snow ir
       , chr_qflx_intercepted_liq  = ir_qflx_intercepted_liq ir
       , chr_qflx_snocanfall      = cer_qflx_snocanfall ce
       , chr_qflx_liqcanfall      = cer_qflx_liqcanfall ce
       , chr_qflx_snow_unload     = sur_qflx_snow_unload su
       , chr_qflx_snotempunload   = sur_qflx_snotempunload su
       , chr_qflx_snowindunload   = sur_qflx_snowindunload su
       , chr_qflx_snow_grnd_col   = fgr_qflx_snow_grnd fg
       , chr_qflx_liq_grnd_col    = fgr_qflx_liq_grnd fg
       , chr_qflx_snow_h2osfc_col = fgr_qflx_snow_h2osfc fg
       }
