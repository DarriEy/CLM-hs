{-# LANGUAGE BangPatterns #-}
-- | Total water and heat content routines.
-- Fortran: TotalWaterAndHeatMod.F90
-- Julia:   src/biogeophys/total_water_heat.jl
--
-- Computes total column water mass (separated into liquid and ice)
-- and total heat content for non-lake and lake columns.
--
-- All functions are pure.
module CLM.BioGeoPhys.TotalWaterHeat
  ( -- * Heat helpers
    heatBaseTemp
  , tempToHeat
  , accumulateLiquidWaterHeat
  , liquidWaterHeat
    -- * Water mass (non-lake)
  , WaterMassNonLakeInput(..)
  , WaterMassNonLakeOutput(..)
  , computeWaterMassNonLake
    -- * Water mass (lake)
  , WaterMassLakeInput(..)
  , WaterMassLakeOutput(..)
  , computeWaterMassLake
    -- * Heat content (non-lake)
  , HeatNonLakeInput(..)
  , HeatNonLakeOutput(..)
  , computeHeatNonLake
    -- * Heat content (lake)
  , HeatLakeInput(..)
  , HeatLakeOutput(..)
  , computeHeatLake
    -- * Delta heat adjustment
  , adjustDeltaHeatForDeltaLiq
  , deltaLiqMinTemp
  , deltaLiqMaxTemp
    -- * Begin/end timestep balance
  , WaterHeatSnapshot(..)
  , WaterHeatBalance(..)
  , computeWaterHeatBalance
    -- * Flux accumulation
  , WaterFluxAccum(..)
  , HeatFluxAccum(..)
  , accumulateWaterFluxes
  , accumulateHeatFluxes
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants
  ( tfrz, denh2o, denice, cpice, cpliq, hfus, nlevsno, nlevgrnd )
import CLM.Constants.ColumnConstants
  ( icol_roof, icol_sunwall, icol_shadewall, icol_road_imperv )

-- =========================================================================
-- Constants
-- =========================================================================

-- | Base temperature for heat sums [K]
heatBaseTemp :: Double
heatBaseTemp = tfrz

-- | Minimum temperature for delta-liq heat adjustment [K]
deltaLiqMinTemp :: Double
deltaLiqMinTemp = tfrz

-- | Maximum temperature for delta-liq heat adjustment [K]
deltaLiqMaxTemp :: Double
deltaLiqMaxTemp = tfrz + 35.0

-- Urban column type constants are imported from CLM.Constants.ColumnConstants

-- =========================================================================
-- Heat helpers
-- =========================================================================

-- | Convert temperature to heat content relative to heatBaseTemp.
tempToHeat :: Double -> Double -> Double
tempToHeat temp cv = cv * (temp - heatBaseTemp)

-- | Accumulate heat for liquid water.
-- Returns (heat_liquid, latent_heat_liquid, cv_liquid).
accumulateLiquidWaterHeat :: Double -> Double -> Double -> Double -> Double
                          -> (Double, Double, Double)
accumulateLiquidWaterHeat temp h2o hl lhl cvl =
  let !cv  = h2o * cpliq
      !cvl' = cvl + cv
      !hl'  = hl + tempToHeat temp cv
      !lhl' = lhl + h2o * hfus
  in (hl', lhl', cvl')

-- | Total heat content of liquid water at a given temperature.
liquidWaterHeat :: Double -> Double -> Double
liquidWaterHeat temp h2o =
  let !cv  = h2o * cpliq
      !hl  = tempToHeat temp cv
      !lhl = h2o * hfus
  in hl + lhl

-- =========================================================================
-- Water mass (non-lake)
-- =========================================================================

-- | Input for non-lake water mass computation (single column).
data WaterMassNonLakeInput = WaterMassNonLakeInput
  { wmnl_snl               :: !Int     -- ^ Number of snow layers (<= 0)
  , wmnl_col_itype          :: !Int     -- ^ Column type
  , wmnl_hydrologically_active :: !Bool
  , wmnl_h2osno_no_layers   :: !Double  -- ^ Unresolved snow [kg/m2]
  , wmnl_h2osfc             :: !Double  -- ^ Surface water [kg/m2]
  , wmnl_wa                 :: !Double  -- ^ Aquifer water [kg/m2]
  , wmnl_aquifer_water_baseline :: !Double
  , wmnl_h2osoi_liq         :: !(VU.Vector Double) -- ^ Liquid water (nlevsno+nlevgrnd)
  , wmnl_h2osoi_ice         :: !(VU.Vector Double) -- ^ Ice water (nlevsno+nlevgrnd)
  , wmnl_total_plant_stored :: !Double  -- ^ Total plant stored water [kg/m2]
  , wmnl_liqcan             :: !Double  -- ^ Canopy liquid [kg/m2]
  , wmnl_snocan             :: !Double  -- ^ Canopy snow [kg/m2]
  } deriving (Show)

-- | Output of non-lake water mass computation.
data WaterMassNonLakeOutput = WaterMassNonLakeOutput
  { wmnl_liquid_mass :: !Double  -- ^ Total liquid mass [kg/m2]
  , wmnl_ice_mass    :: !Double  -- ^ Total ice mass [kg/m2]
  , wmnl_total_mass  :: !Double  -- ^ Total water mass [kg/m2]
  } deriving (Show)

-- | Compute water mass for a single non-lake column.
computeWaterMassNonLake :: WaterMassNonLakeInput -> WaterMassNonLakeOutput
computeWaterMassNonLake inp =
  let snl  = wmnl_snl inp
      liq  = wmnl_h2osoi_liq inp
      ice  = wmnl_h2osoi_ice inp
      ctype = wmnl_col_itype inp

      -- Canopy + plant stored water
      !liq0 = wmnl_liqcan inp + wmnl_total_plant_stored inp
      !ice0 = wmnl_snocan inp

      -- Unresolved snow
      !ice1 = ice0 + wmnl_h2osno_no_layers inp

      -- Snow layers
      snowAccum (!la, !ia) j =
        let jj = j + nlevsno
        in (la + liq VU.! jj, ia + ice VU.! jj)
      (!liq2, !ice2) = foldl snowAccum (liq0, ice1) [snl + 1 .. 0]

      -- Aquifer water
      !liq3 = if wmnl_hydrologically_active inp
              then liq2 + (wmnl_wa inp - wmnl_aquifer_water_baseline inp)
              else liq2

      -- Surface water (exclude certain urban types)
      !liq4 = if ctype /= icol_roof && ctype /= icol_sunwall &&
                 ctype /= icol_shadewall && ctype /= icol_road_imperv
              then liq3 + wmnl_h2osfc inp
              else liq3

      -- Soil water
      soilAccum (!la, !ia) j =
        let jj = j + nlevsno
        in (la + liq VU.! jj, ia + ice VU.! jj)
      (!liq5, !ice3) = foldl soilAccum (liq4, ice2) [1 .. nlevgrnd]

  in WaterMassNonLakeOutput
       { wmnl_liquid_mass = liq5
       , wmnl_ice_mass    = ice3
       , wmnl_total_mass  = liq5 + ice3
       }

-- =========================================================================
-- Water mass (lake)
-- =========================================================================

-- | Input for lake water mass computation (single column).
data WaterMassLakeInput = WaterMassLakeInput
  { wml_snl              :: !Int
  , wml_h2osno_no_layers :: !Double
  , wml_h2osoi_liq       :: !(VU.Vector Double)
  , wml_h2osoi_ice       :: !(VU.Vector Double)
  , wml_dz_lake          :: !(VU.Vector Double) -- ^ Lake layer thicknesses [m]
  , wml_lake_icefrac     :: !(VU.Vector Double) -- ^ Lake ice fraction per layer
  , wml_nlevlak          :: !Int                -- ^ Number of lake levels
  } deriving (Show)

-- | Output of lake water mass computation.
data WaterMassLakeOutput = WaterMassLakeOutput
  { wml_liquid_mass :: !Double
  , wml_ice_mass    :: !Double
  , wml_total_mass  :: !Double
  } deriving (Show)

-- | Compute water mass for a single lake column.
computeWaterMassLake :: WaterMassLakeInput -> WaterMassLakeOutput
computeWaterMassLake inp =
  let snl = wml_snl inp

      -- Snow
      !ice0 = wml_h2osno_no_layers inp
      snowAccum (!la, !ia) j =
        let jj = j + nlevsno
        in (la + wml_h2osoi_liq inp VU.! jj, ia + wml_h2osoi_ice inp VU.! jj)
      (!liq1, !ice1) = foldl snowAccum (0.0, ice0) [snl + 1 .. 0]

      -- Soil under lake
      soilAccum (!la, !ia) j =
        let jj = j + nlevsno
        in (la + wml_h2osoi_liq inp VU.! jj, ia + wml_h2osoi_ice inp VU.! jj)
      (!liq2, !ice2) = foldl soilAccum (liq1, ice1) [1 .. nlevgrnd]

  in WaterMassLakeOutput
       { wml_liquid_mass = liq2
       , wml_ice_mass    = ice2
       , wml_total_mass  = liq2 + ice2
       }

-- =========================================================================
-- Heat content (non-lake)
-- =========================================================================

-- | Input for non-lake heat content computation (single column).
data HeatNonLakeInput = HeatNonLakeInput
  { hnl_snl              :: !Int
  , hnl_col_itype        :: !Int
  , hnl_hydrologically_active :: !Bool
  , hnl_t_soisno         :: !(VU.Vector Double) -- ^ Layer temperatures [K]
  , hnl_t_h2osfc         :: !Double             -- ^ Surface water temperature [K]
  , hnl_h2osoi_liq       :: !(VU.Vector Double)
  , hnl_h2osoi_ice       :: !(VU.Vector Double)
  , hnl_h2osno_no_layers :: !Double
  , hnl_h2osfc           :: !Double
  , hnl_wa               :: !Double
  , hnl_aquifer_water_baseline :: !Double
  , hnl_total_plant_stored :: !Double
  , hnl_liqcan           :: !Double
  , hnl_dz               :: !(VU.Vector Double) -- ^ Layer thickness [m]
  , hnl_csol             :: !(VU.Vector Double) -- ^ Dry vol heat cap (soil-only)
  , hnl_watsat           :: !(VU.Vector Double) -- ^ Porosity (soil-only)
  , hnl_dynbal_baseline_heat :: !Double
  } deriving (Show)

-- | Output of non-lake heat content computation.
data HeatNonLakeOutput = HeatNonLakeOutput
  { hnl_heat        :: !Double  -- ^ Total heat content [J/m2]
  , hnl_heat_liquid :: !Double  -- ^ Liquid water heat [J/m2]
  , hnl_cv_liquid   :: !Double  -- ^ Liquid water heat capacity [J/K/m2]
  } deriving (Show)

-- | Compute heat content for a single non-lake column.
computeHeatNonLake :: HeatNonLakeInput -> HeatNonLakeOutput
computeHeatNonLake inp =
  let snl  = hnl_snl inp
      tsoi = hnl_t_soisno inp
      liq  = hnl_h2osoi_liq inp
      ice  = hnl_h2osoi_ice inp
      ctype = hnl_col_itype inp

      -- Canopy liquid + plant stored
      !liqveg = hnl_liqcan inp + hnl_total_plant_stored inp
      (!hl0, !lhl0, !cvl0) = accumulateLiquidWaterHeat heatBaseTemp liqveg 0.0 0.0 0.0

      -- Unresolved snow ice heat
      j_top = nlevsno  -- 0-indexed VU slot for soil layer 1
      !heat_ice0 = tempToHeat (tsoi VU.! j_top) (hnl_h2osno_no_layers inp * cpice)

      -- Snow layers
      snowStep (!hl, !lhl, !cvl, !hi) j =
        let jj = j + nlevsno
            t_j = tsoi VU.! jj
            (!hl', !lhl', !cvl') = accumulateLiquidWaterHeat t_j (liq VU.! jj) hl lhl cvl
            !hi' = hi + tempToHeat t_j (ice VU.! jj * cpice)
        in (hl', lhl', cvl', hi')
      (!hl1, !lhl1, !cvl1, !hi1) = foldl snowStep (hl0, lhl0, cvl0, heat_ice0) [snl + 1 .. 0]

      -- Aquifer water
      (!hl2, !lhl2, !cvl2) =
        if hnl_hydrologically_active inp
        then accumulateLiquidWaterHeat heatBaseTemp
               (hnl_wa inp - hnl_aquifer_water_baseline inp) hl1 lhl1 cvl1
        else (hl1, lhl1, cvl1)

      -- Surface water
      (!hl3, !lhl3, !cvl3) =
        if ctype /= icol_roof && ctype /= icol_sunwall &&
           ctype /= icol_shadewall && ctype /= icol_road_imperv
        then accumulateLiquidWaterHeat (hnl_t_h2osfc inp) (hnl_h2osfc inp) hl2 lhl2 cvl2
        else (hl2, lhl2, cvl2)

      -- Soil layers
      soilStep (!hl, !lhl, !cvl, !hdm, !hi) j =
        let jj = j + nlevsno
            t_j = tsoi VU.! jj
            dz_j = hnl_dz inp VU.! jj
            csol_j = hnl_csol inp VU.! (j - 1)
            ws_j = hnl_watsat inp VU.! (j - 1)
            -- Dry mass heat
            !hdm' = hdm + tempToHeat t_j (csol_j * (1.0 - ws_j) * dz_j)
            -- Liquid water heat
            (!hl', !lhl', !cvl') = accumulateLiquidWaterHeat t_j (liq VU.! jj) hl lhl cvl
            -- Ice heat
            !hi' = hi + tempToHeat t_j (ice VU.! jj * cpice)
        in (hl', lhl', cvl', hdm', hi')
      (!hl4, !lhl4, !cvl4, !hdm4, !hi4) =
        foldl soilStep (hl3, lhl3, cvl3, 0.0, hi1) [1 .. nlevgrnd]

      !heat_total = hdm4 + hi4 + hl4 + lhl4 - hnl_dynbal_baseline_heat inp

  in HeatNonLakeOutput
       { hnl_heat        = heat_total
       , hnl_heat_liquid = hl4
       , hnl_cv_liquid   = cvl4
       }

-- =========================================================================
-- Heat content (lake)
-- =========================================================================

-- | Input for lake heat content computation (single column).
data HeatLakeInput = HeatLakeInput
  { hli_snl              :: !Int
  , hli_t_soisno         :: !(VU.Vector Double)
  , hli_t_lake           :: !(VU.Vector Double) -- ^ Lake layer temperatures [K]
  , hli_h2osoi_liq       :: !(VU.Vector Double)
  , hli_h2osoi_ice       :: !(VU.Vector Double)
  , hli_dz               :: !(VU.Vector Double)
  , hli_dz_lake          :: !(VU.Vector Double)
  , hli_lake_icefrac     :: !(VU.Vector Double)
  , hli_csol             :: !(VU.Vector Double) -- ^ Soil-only
  , hli_watsat           :: !(VU.Vector Double) -- ^ Soil-only
  , hli_nlevlak          :: !Int
  , hli_dynbal_baseline_heat :: !Double
  } deriving (Show)

-- | Output of lake heat content computation.
data HeatLakeOutput = HeatLakeOutput
  { hli_heat        :: !Double
  , hli_heat_liquid :: !Double
  , hli_cv_liquid   :: !Double
  } deriving (Show)

-- | Compute heat content for a single lake column.
computeHeatLake :: HeatLakeInput -> HeatLakeOutput
computeHeatLake inp =
  let snl  = hli_snl inp
      tsoi = hli_t_soisno inp
      liq  = hli_h2osoi_liq inp
      ice  = hli_h2osoi_ice inp

      -- Lake water heat (not in heat_liquid/cv_liquid)
      lakeStep (!lhl_lk, !hi_lk, !llhl_lk) j =
        let dz_j = hli_dz_lake inp VU.! (j - 1)
            fice = hli_lake_icefrac inp VU.! (j - 1)
            t_j  = hli_t_lake inp VU.! (j - 1)
            h2o_liq = dz_j * denh2o * (1.0 - fice)
            h2o_ice = dz_j * denh2o * fice
            !cv_liq = h2o_liq * cpliq
            !lhl_lk' = lhl_lk + tempToHeat t_j cv_liq
            !llhl_lk' = llhl_lk + h2o_liq * hfus
            !hi_lk' = hi_lk + tempToHeat t_j (h2o_ice * cpice)
        in (lhl_lk', hi_lk', llhl_lk')
      (!lake_hl, !lake_hi, !lake_lhl) =
        foldl lakeStep (0.0, 0.0, 0.0) [1 .. hli_nlevlak inp]

      -- Snow layers
      snowStep (!hl, !lhl, !cvl, !hi) j =
        let jj = j + nlevsno
            t_j = tsoi VU.! jj
            (!hl', !lhl', !cvl') = accumulateLiquidWaterHeat t_j (liq VU.! jj) hl lhl cvl
            !hi' = hi + tempToHeat t_j (ice VU.! jj * cpice)
        in (hl', lhl', cvl', hi')
      (!hl1, !lhl1, !cvl1, !hi1) = foldl snowStep (0.0, 0.0, 0.0, 0.0) [snl + 1 .. 0]

      -- Soil under lake
      soilStep (!hl, !lhl, !cvl, !hdm, !hi) j =
        let jj = j + nlevsno
            t_j = tsoi VU.! jj
            dz_j = hli_dz inp VU.! jj
            csol_j = hli_csol inp VU.! (j - 1)
            ws_j = hli_watsat inp VU.! (j - 1)
            !hdm' = hdm + tempToHeat t_j (csol_j * (1.0 - ws_j) * dz_j)
            (!hl', !lhl', !cvl') = accumulateLiquidWaterHeat t_j (liq VU.! jj) hl lhl cvl
            !hi' = hi + tempToHeat t_j (ice VU.! jj * cpice)
        in (hl', lhl', cvl', hdm', hi')
      (!hl2, !lhl2, !cvl2, !hdm2, !hi2) =
        foldl soilStep (hl1, lhl1, cvl1, 0.0, hi1) [1 .. nlevgrnd]

      !heat_total = -(hli_dynbal_baseline_heat inp)
                  + lake_hi + lake_hl + lake_lhl
                  + hdm2 + hi2 + hl2 + lhl2

  in HeatLakeOutput
       { hli_heat        = heat_total
       , hli_heat_liquid = hl2
       , hli_cv_liquid   = cvl2
       }

-- =========================================================================
-- Delta heat adjustment
-- =========================================================================

-- | Adjust delta_heat to account for implicit heat flux from delta_liq.
adjustDeltaHeatForDeltaLiq :: Double  -- ^ delta_liq [kg/m2]
                           -> Double  -- ^ liquid_water_temp1 [K]
                           -> Double  -- ^ liquid_water_temp2 [K]
                           -> Double  -- ^ delta_heat [J/m2] (input)
                           -> Double  -- ^ adjusted delta_heat [J/m2]
adjustDeltaHeatForDeltaLiq delta_liq lwt1 lwt2 delta_heat
  | delta_liq == 0.0 = delta_heat
  | otherwise =
      let wt | delta_liq < 0.0 = lwt1
             | otherwise       = lwt2
          !wt_c = max deltaLiqMinTemp (min deltaLiqMaxTemp wt)
          !total_liq_heat = liquidWaterHeat wt_c delta_liq
      in delta_heat - total_liq_heat

-- =========================================================================
-- Begin/end timestep water and heat balance
-- =========================================================================

-- | Snapshot of column water and heat state at a given time.
data WaterHeatSnapshot = WaterHeatSnapshot
  { whs_total_water_liq :: !Double  -- ^ total liquid water (kg/m2)
  , whs_total_water_ice :: !Double  -- ^ total ice water (kg/m2)
  , whs_total_heat      :: !Double  -- ^ total heat content (J/m2)
  , whs_heat_liquid     :: !Double  -- ^ liquid-water heat component (J/m2)
  , whs_cv_liquid       :: !Double  -- ^ liquid-water heat capacity (J/m2/K)
  } deriving (Show, Eq)

-- | Water and heat balance between two snapshots.
data WaterHeatBalance = WaterHeatBalance
  { whb_delta_water_liq :: !Double  -- ^ change in liquid water (kg/m2)
  , whb_delta_water_ice :: !Double  -- ^ change in ice water (kg/m2)
  , whb_delta_water_tot :: !Double  -- ^ total water change (kg/m2)
  , whb_delta_heat      :: !Double  -- ^ total heat change (J/m2)
  , whb_delta_heat_adj  :: !Double  -- ^ heat change adjusted for delta-liq (J/m2)
  , whb_errh2o          :: !Double  -- ^ water balance error (kg/m2)
  , whb_errseb          :: !Double  -- ^ energy balance error (W/m2)
  } deriving (Show, Eq)

-- | Compute water and heat balance between begin and end timestep snapshots.
-- errh2o = delta_water - (precip - evap - runoff) * dt
-- errseb = delta_heat/dt - net_radiation + sensible + latent + ground
computeWaterHeatBalance :: WaterHeatSnapshot  -- ^ begin of timestep
                        -> WaterHeatSnapshot  -- ^ end of timestep
                        -> Double  -- ^ net water flux into column (kg/m2/s)
                        -> Double  -- ^ net heat flux into column (W/m2)
                        -> Double  -- ^ dt (s)
                        -> WaterHeatBalance
computeWaterHeatBalance !beg !end_ !net_water_flux !net_heat_flux !dt =
  let !dw_liq = whs_total_water_liq end_ - whs_total_water_liq beg
      !dw_ice = whs_total_water_ice end_ - whs_total_water_ice beg
      !dw_tot = dw_liq + dw_ice
      !dh = whs_total_heat end_ - whs_total_heat beg

      -- Adjust heat for implicit delta-liq contribution
      !lwt1 = if whs_cv_liquid beg > 0.0
              then whs_heat_liquid beg / whs_cv_liquid beg + heatBaseTemp
              else heatBaseTemp
      !lwt2 = if whs_cv_liquid end_ > 0.0
              then whs_heat_liquid end_ / whs_cv_liquid end_ + heatBaseTemp
              else heatBaseTemp
      !dh_adj = adjustDeltaHeatForDeltaLiq dw_liq lwt1 lwt2 dh

      -- Water balance error
      !errh2o = dw_tot - net_water_flux * dt

      -- Energy balance error
      !errseb = if dt > 0.0 then dh_adj / dt - net_heat_flux else 0.0

  in WaterHeatBalance
     { whb_delta_water_liq = dw_liq
     , whb_delta_water_ice = dw_ice
     , whb_delta_water_tot = dw_tot
     , whb_delta_heat = dh
     , whb_delta_heat_adj = dh_adj
     , whb_errh2o = errh2o
     , whb_errseb = errseb
     }

-- =========================================================================
-- Flux accumulation
-- =========================================================================

-- | Accumulated water fluxes for a column over a timestep (all in kg/m2/s).
data WaterFluxAccum = WaterFluxAccum
  { wfa_qflx_prec_grnd    :: !Double  -- ^ precipitation reaching ground
  , wfa_qflx_evap_tot     :: !Double  -- ^ total evapotranspiration
  , wfa_qflx_surf         :: !Double  -- ^ surface runoff
  , wfa_qflx_drain        :: !Double  -- ^ subsurface drainage
  , wfa_qflx_irrig        :: !Double  -- ^ irrigation applied
  , wfa_qflx_snwcp_ice    :: !Double  -- ^ snow capping (excess snow to ice)
  , wfa_qflx_lateral      :: !Double  -- ^ lateral flow
  } deriving (Show)

-- | Accumulated heat fluxes for a column over a timestep (all in W/m2).
data HeatFluxAccum = HeatFluxAccum
  { hfa_eflx_sabg         :: !Double  -- ^ absorbed ground solar
  , hfa_eflx_sabv         :: !Double  -- ^ absorbed vegetation solar
  , hfa_eflx_lwrad_net    :: !Double  -- ^ net longwave
  , hfa_eflx_sh_tot       :: !Double  -- ^ total sensible heat
  , hfa_eflx_lh_tot       :: !Double  -- ^ total latent heat
  , hfa_eflx_soil_grnd    :: !Double  -- ^ ground heat flux
  } deriving (Show)

-- | Compute net water flux into column from accumulated fluxes.
accumulateWaterFluxes :: WaterFluxAccum -> Double
accumulateWaterFluxes !wfa =
  wfa_qflx_prec_grnd wfa
  + wfa_qflx_irrig wfa
  - wfa_qflx_evap_tot wfa
  - wfa_qflx_surf wfa
  - wfa_qflx_drain wfa
  - wfa_qflx_snwcp_ice wfa
  - wfa_qflx_lateral wfa

-- | Compute net heat flux into column from accumulated fluxes.
accumulateHeatFluxes :: HeatFluxAccum -> Double
accumulateHeatFluxes !hfa =
  hfa_eflx_sabg hfa
  + hfa_eflx_sabv hfa
  - hfa_eflx_lwrad_net hfa
  - hfa_eflx_sh_tot hfa
  - hfa_eflx_lh_tot hfa
  - hfa_eflx_soil_grnd hfa
