{-# LANGUAGE BangPatterns #-}
-- | Lake hydrology calculations.
-- Fortran: LakeHydrologyMod.F90
-- Julia:   src/biogeophys/lake_hydrology.jl
--
-- Calculation of lake hydrology. Full hydrology of snow layers is done.
-- However, there is no infiltration, and the water budget is balanced with
-- qflx_qrgwl. Lake water mass is kept constant. The soil is simply maintained
-- at volumetric saturation if ice melting frees up pore space.
--
-- All functions are pure (no IO/mutation).
--
module CLM.BioGeoPhys.LakeHydrology
  ( -- * Top-level single-column functions
    sumFluxFluxesOntoGround
  , SumFluxInput(..)
  , SumFluxOutput(..)
    -- * Sublimation and dew
  , lakeSublimationDew
  , SubDewInput(..)
  , SubDewOutput(..)
    -- * Soil hydrology
  , lakeSoilHydrology
  , SoilHydInput(..)
  , SoilHydOutput(..)
    -- * Single snow layer check
  , lakeCheckSingleSnowLayer
  , SingleSnowInput(..)
  , SingleSnowOutput(..)
    -- * Snow above unfrozen lake
  , lakeSnowAboveUnfrozen
  , SnowAboveInput(..)
  , SnowAboveOutput(..)
    -- * Snow diagnostics
  , lakeSnowDiagnostics
  , SnowDiagInput(..)
  , SnowDiagOutput(..)
    -- * Water balance
  , lakeWaterBalance
  , WaterBalInput(..)
  , WaterBalOutput(..)
    -- * Volumetric soil water update
  , lakeUpdateH2osoiVol
    -- * Constants
  , fracSnoSmall
  , snowBdLake
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants
  ( tfrz, denh2o, denice, cpice, cpliq, hfus
  , nlevsno
  )

-- =========================================================================
-- Module-level constants
-- =========================================================================

-- | Small value of frac_sno for frost initiation
fracSnoSmall :: Double
fracSnoSmall = 1.0e-6

-- | Assumed snow bulk density for lakes w/out resolved layers [kg/m3]
snowBdLake :: Double
snowBdLake = 250.0

-- =========================================================================
-- sumFluxFluxesOntoGround
-- =========================================================================

-- | Input for summing fluxes onto ground.
data SumFluxInput = SumFluxInput
  { sfi_forc_snow :: !Double  -- ^ Snow precipitation [kg/m2/s]
  , sfi_forc_rain :: !Double  -- ^ Rain precipitation [kg/m2/s]
  } deriving (Show)

-- | Output of summing fluxes onto ground.
data SumFluxOutput = SumFluxOutput
  { sfo_qflx_snow_grnd :: !Double  -- ^ Snow flux to ground [kg/m2/s]
  , sfo_qflx_liq_grnd  :: !Double  -- ^ Liquid flux to ground [kg/m2/s]
  } deriving (Show)

-- | Copy precipitation fluxes onto lake surface.
-- Ported from SumFlux_FluxesOntoGround in LakeHydrologyMod.F90.
sumFluxFluxesOntoGround :: SumFluxInput -> SumFluxOutput
sumFluxFluxesOntoGround inp = SumFluxOutput
  { sfo_qflx_snow_grnd = sfi_forc_snow inp
  , sfo_qflx_liq_grnd  = sfi_forc_rain inp
  }

-- =========================================================================
-- lakeSublimationDew
-- =========================================================================

-- | Input for sublimation/dew calculation.
data SubDewInput = SubDewInput
  { sdi_qflx_evap_soi   :: !Double  -- ^ Evaporation rate [kg/m2/s]
  , sdi_h2osoi_liq_top   :: !Double  -- ^ Liquid water in top snow layer [kg/m2]
  , sdi_h2osoi_ice_top   :: !Double  -- ^ Ice in top snow layer [kg/m2]
  , sdi_h2osno_no_layers :: !Double  -- ^ Unresolved snow SWE [kg/m2]
  , sdi_snow_depth       :: !Double  -- ^ Snow depth [m]
  , sdi_frac_sno         :: !Double  -- ^ Snow fraction
  , sdi_t_grnd           :: !Double  -- ^ Ground temperature [K]
  , sdi_t_soisno_top     :: !Double  -- ^ Temperature of top snow layer [K]
  , sdi_snl              :: !Int     -- ^ Number of snow layers (<= 0)
  , sdi_dtime            :: !Double  -- ^ Timestep [s]
  } deriving (Show)

-- | Output from sublimation/dew calculation.
data SubDewOutput = SubDewOutput
  { sdo_qflx_liqevap_from_top   :: !Double  -- ^ Liquid evaporation from top [kg/m2/s]
  , sdo_qflx_solidevap_from_top :: !Double  -- ^ Ice sublimation from top [kg/m2/s]
  , sdo_qflx_soliddew_to_top    :: !Double  -- ^ Frost to top [kg/m2/s]
  , sdo_qflx_liqdew_to_top      :: !Double  -- ^ Dew to top [kg/m2/s]
  , sdo_qflx_ev_snow            :: !Double  -- ^ Evaporation from snow [kg/m2/s]
  , sdo_h2osno_no_layers        :: !Double  -- ^ Updated unresolved SWE [kg/m2]
  , sdo_snow_depth              :: !Double  -- ^ Updated snow depth [m]
  , sdo_frac_sno                :: !Double  -- ^ Updated frac_sno
  } deriving (Show)

-- | Calculate sublimation and dew for a single lake column.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeSublimationDew :: SubDewInput -> SubDewOutput
lakeSublimationDew inp
  | jtop <= 0 = snowLayerCase  -- snow layers present
  | otherwise = noSnowLayerCase
  where
    !qflx_evap = sdi_qflx_evap_soi inp
    !liq_top   = sdi_h2osoi_liq_top inp
    !ice_top   = sdi_h2osoi_ice_top inp
    !h2osno_nl = sdi_h2osno_no_layers inp
    !sd        = sdi_snow_depth inp
    !frac_sno0 = sdi_frac_sno inp
    !t_grnd    = sdi_t_grnd inp
    !t_top     = sdi_t_soisno_top inp
    !snl       = sdi_snl inp
    !dt        = sdi_dtime inp
    jtop       = snl + 1

    -- Snow layers present
    snowLayerCase
      | qflx_evap >= 0.0 =
          let evap_lim = min qflx_evap ((liq_top + ice_top) / dt)
              total_w  = liq_top + ice_top
              liq_frac = if total_w > 0.0
                         then max 0.0 (evap_lim * (liq_top / total_w))
                         else 0.0
          in SubDewOutput
            { sdo_qflx_liqevap_from_top   = liq_frac
            , sdo_qflx_solidevap_from_top = evap_lim - liq_frac
            , sdo_qflx_soliddew_to_top    = 0.0
            , sdo_qflx_liqdew_to_top      = 0.0
            , sdo_qflx_ev_snow            = evap_lim
            , sdo_h2osno_no_layers        = h2osno_nl
            , sdo_snow_depth              = sd
            , sdo_frac_sno                = frac_sno0
            }
      | otherwise =
          let (solid_dew, liq_dew)
                | t_grnd < tfrz && t_top < tfrz = (abs qflx_evap, 0.0)
                | jtop < 0 || (t_grnd == tfrz && t_top == tfrz) = (0.0, abs qflx_evap)
                | otherwise = (0.0, 0.0)
          in SubDewOutput
            { sdo_qflx_liqevap_from_top   = 0.0
            , sdo_qflx_solidevap_from_top = 0.0
            , sdo_qflx_soliddew_to_top    = solid_dew
            , sdo_qflx_liqdew_to_top      = liq_dew
            , sdo_qflx_ev_snow            = qflx_evap
            , sdo_h2osno_no_layers        = h2osno_nl
            , sdo_snow_depth              = sd
            , sdo_frac_sno                = frac_sno0
            }

    -- No snow layers
    noSnowLayerCase =
      let (solid_evap, liq_evap, solid_dew, liq_dew)
            | qflx_evap >= 0.0 =
                let se = min qflx_evap (h2osno_nl / dt)
                    le = qflx_evap - se
                in (se, le, 0.0, 0.0)
            | t_grnd < tfrz - 0.1 = (0.0, 0.0, abs qflx_evap, 0.0)
            | otherwise            = (0.0, 0.0, 0.0, abs qflx_evap)

          dew_minus_sub = negate solid_evap + solid_dew
          h2osno_new = max 0.0 (h2osno_nl + dew_minus_sub * dt)

          frac_sno_new
            | dew_minus_sub > 0.0 && frac_sno0 <= 0.0 = fracSnoSmall
            | dew_minus_sub < 0.0 && h2osno_new == 0.0 = 0.0
            | otherwise = frac_sno0

          sd_new
            | h2osno_nl > 0.0 = sd * h2osno_new / h2osno_nl
            | otherwise       = h2osno_new / snowBdLake

      in SubDewOutput
        { sdo_qflx_liqevap_from_top   = liq_evap
        , sdo_qflx_solidevap_from_top = solid_evap
        , sdo_qflx_soliddew_to_top    = solid_dew
        , sdo_qflx_liqdew_to_top      = liq_dew
        , sdo_qflx_ev_snow            = qflx_evap
        , sdo_h2osno_no_layers        = h2osno_new
        , sdo_snow_depth              = sd_new
        , sdo_frac_sno                = frac_sno_new
        }

-- =========================================================================
-- lakeSoilHydrology
-- =========================================================================

-- | Input for lake soil hydrology (single column, all soil layers).
data SoilHydInput = SoilHydInput
  { shi_h2osoi_liq :: !(VU.Vector Double) -- ^ Liquid water per layer [kg/m2] (snow+soil)
  , shi_h2osoi_ice :: !(VU.Vector Double) -- ^ Ice per layer [kg/m2] (snow+soil)
  , shi_dz         :: !(VU.Vector Double) -- ^ Layer thickness [m] (snow+soil)
  , shi_watsat     :: !(VU.Vector Double) -- ^ Porosity (soil-only, 0-indexed)
  , shi_nlevsoi    :: !Int                -- ^ Number of soil layers
  } deriving (Show)

-- | Output from lake soil hydrology.
data SoilHydOutput = SoilHydOutput
  { sho_h2osoi_liq :: !(VU.Vector Double) -- ^ Updated liquid water [kg/m2]
  , sho_h2osoi_vol :: !(VU.Vector Double) -- ^ Updated volumetric soil water (soil-only)
  } deriving (Show)

-- | Maintain soil at volumetric saturation for a single lake column.
-- If melting has left open pore space, fill with water.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeSoilHydrology :: SoilHydInput -> SoilHydOutput
lakeSoilHydrology inp = SoilHydOutput
  { sho_h2osoi_liq = liq_new
  , sho_h2osoi_vol = vol_new
  }
  where
    !liq0   = shi_h2osoi_liq inp
    !ice0   = shi_h2osoi_ice inp
    !dz_    = shi_dz inp
    !ws     = shi_watsat inp
    !nsoil  = shi_nlevsoi inp

    -- Process each soil layer
    processLayer j (liqAcc, volAcc) =
      let jj = j + nlevsno  -- snow+soil index
          liq_j = liqAcc VU.! jj
          ice_j = ice0 VU.! jj
          dz_j  = dz_ VU.! jj
          ws_j  = ws VU.! j
          vol_j = liq_j / (dz_j * denh2o) + ice_j / (dz_j * denice)
          liq_new_j
            | vol_j < ws_j = (ws_j * dz_j - ice_j / denice) * denh2o
            | liq_j > ws_j * denh2o * dz_j = ws_j * denh2o * dz_j
            | otherwise = liq_j
          vol_new_j = liq_new_j / (dz_j * denh2o) + ice_j / (dz_j * denice)
      in (liqAcc VU.// [(jj, liq_new_j)], volAcc VU.// [(j, vol_new_j)])

    (liq_new, vol_new) = foldr
      (\j acc -> processLayer j acc)
      (liq0, VU.replicate nsoil 0.0)
      [0 .. nsoil - 1]

-- =========================================================================
-- lakeCheckSingleSnowLayer
-- =========================================================================

-- | Input for single snow layer check.
data SingleSnowInput = SingleSnowInput
  { ssi_snl               :: !Int
  , ssi_h2osoi_ice_top    :: !Double  -- ^ Ice in top snow layer [kg/m2]
  , ssi_h2osoi_liq_top    :: !Double  -- ^ Liquid in top snow layer [kg/m2]
  , ssi_t_soisno_top      :: !Double  -- ^ Temperature of top snow layer [K]
  , ssi_eflx_sh_tot       :: !Double
  , ssi_eflx_sh_grnd      :: !Double
  , ssi_eflx_soil_grnd    :: !Double
  , ssi_eflx_gnet         :: !Double
  , ssi_h2osno_no_layers  :: !Double
  , ssi_h2osno_total      :: !Double
  , ssi_snow_depth        :: !Double
  , ssi_qflx_snow_drain   :: !Double
  , ssi_dtime             :: !Double
  } deriving (Show)

-- | Output from single snow layer check.
data SingleSnowOutput = SingleSnowOutput
  { sso_snl              :: !Int
  , sso_eflx_sh_tot      :: !Double
  , sso_eflx_sh_grnd     :: !Double
  , sso_eflx_soil_grnd   :: !Double
  , sso_eflx_gnet        :: !Double
  , sso_eflx_grnd_lake   :: !Double
  , sso_t_soisno_top     :: !Double
  , sso_h2osno_no_layers :: !Double
  , sso_h2osno_total     :: !Double
  , sso_snow_depth       :: !Double
  , sso_qflx_snow_drain  :: !Double
  } deriving (Show)

-- | Check for single completely unfrozen snow layer over lake.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeCheckSingleSnowLayer :: SingleSnowInput -> SingleSnowOutput
lakeCheckSingleSnowLayer inp
  | ssi_snl inp == -1 && ice > 0.0 && t_top > tfrz =
      -- Ice present, above freezing: release heat
      let heatrem = cpliq * liq * (t_top - tfrz)
          dt = ssi_dtime inp
      in SingleSnowOutput
        { sso_snl              = -1
        , sso_eflx_sh_tot      = ssi_eflx_sh_tot inp + heatrem / dt
        , sso_eflx_sh_grnd     = ssi_eflx_sh_grnd inp + heatrem / dt
        , sso_eflx_soil_grnd   = ssi_eflx_soil_grnd inp - heatrem / dt
        , sso_eflx_gnet        = ssi_eflx_gnet inp - heatrem / dt
        , sso_eflx_grnd_lake   = ssi_eflx_gnet inp - heatrem / dt
        , sso_t_soisno_top     = tfrz
        , sso_h2osno_no_layers = ssi_h2osno_no_layers inp
        , sso_h2osno_total     = ssi_h2osno_total inp
        , sso_snow_depth       = ssi_snow_depth inp
        , sso_qflx_snow_drain  = ssi_qflx_snow_drain inp
        }
  | ssi_snl inp == -1 && ice == 0.0 =
      -- No ice: remove layer
      let heatrem = cpliq * liq * (t_top - tfrz)
          dt = ssi_dtime inp
      in SingleSnowOutput
        { sso_snl              = 0
        , sso_eflx_sh_tot      = ssi_eflx_sh_tot inp + heatrem / dt
        , sso_eflx_sh_grnd     = ssi_eflx_sh_grnd inp + heatrem / dt
        , sso_eflx_soil_grnd   = ssi_eflx_soil_grnd inp - heatrem / dt
        , sso_eflx_gnet        = ssi_eflx_gnet inp - heatrem / dt
        , sso_eflx_grnd_lake   = ssi_eflx_gnet inp - heatrem / dt
        , sso_t_soisno_top     = t_top
        , sso_h2osno_no_layers = 0.0
        , sso_h2osno_total     = 0.0
        , sso_snow_depth       = 0.0
        , sso_qflx_snow_drain  = ssi_qflx_snow_drain inp + ssi_h2osno_total inp / ssi_dtime inp
        }
  | otherwise =
      SingleSnowOutput
        { sso_snl              = ssi_snl inp
        , sso_eflx_sh_tot      = ssi_eflx_sh_tot inp
        , sso_eflx_sh_grnd     = ssi_eflx_sh_grnd inp
        , sso_eflx_soil_grnd   = ssi_eflx_soil_grnd inp
        , sso_eflx_gnet        = ssi_eflx_gnet inp
        , sso_eflx_grnd_lake   = ssi_eflx_gnet inp
        , sso_t_soisno_top     = t_top
        , sso_h2osno_no_layers = ssi_h2osno_no_layers inp
        , sso_h2osno_total     = ssi_h2osno_total inp
        , sso_snow_depth       = ssi_snow_depth inp
        , sso_qflx_snow_drain  = ssi_qflx_snow_drain inp
        }
  where
    !ice   = ssi_h2osoi_ice_top inp
    !liq   = ssi_h2osoi_liq_top inp
    !t_top = ssi_t_soisno_top inp

-- =========================================================================
-- lakeSnowAboveUnfrozen
-- =========================================================================

-- | Input for snow-above-unfrozen-lake check.
data SnowAboveInput = SnowAboveInput
  { sai_t_lake1         :: !Double  -- ^ Temperature of top lake layer [K]
  , sai_lake_icefrac1   :: !Double  -- ^ Ice fraction of top lake layer
  , sai_snl             :: !Int     -- ^ Number of snow layers
  , sai_h2osoi_ice      :: !(VU.Vector Double)  -- ^ Snow layer ice [kg/m2]
  , sai_h2osoi_liq      :: !(VU.Vector Double)  -- ^ Snow layer liquid [kg/m2]
  , sai_t_soisno        :: !(VU.Vector Double)  -- ^ Snow layer temperatures [K]
  , sai_h2osno_no_layers :: !Double
  , sai_h2osno_total    :: !Double
  , sai_snow_depth      :: !Double
  , sai_qflx_snomelt    :: !Double
  , sai_eflx_snomelt    :: !Double
  , sai_qflx_snow_drain :: !Double
  , sai_dz_lake1        :: !Double  -- ^ Thickness of top lake layer [m]
  , sai_dtime           :: !Double
  } deriving (Show)

-- | Output from snow-above-unfrozen-lake check.
data SnowAboveOutput = SnowAboveOutput
  { sao_t_lake1         :: !Double
  , sao_lake_icefrac1   :: !Double
  , sao_snl             :: !Int
  , sao_h2osno_no_layers :: !Double
  , sao_snow_depth      :: !Double
  , sao_qflx_snomelt    :: !Double
  , sao_eflx_snomelt    :: !Double
  , sao_qflx_snow_drain :: !Double
  } deriving (Show)

-- | Check for snow layers above unfrozen lake top.
-- If the top lake layer has sufficient heat to melt the snow, do so.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeSnowAboveUnfrozen :: SnowAboveInput -> SnowAboveOutput
lakeSnowAboveUnfrozen inp
  | sai_t_lake1 inp > tfrz && sai_lake_icefrac1 inp == 0.0 && sai_snl inp < 0 =
      let dt = sai_dtime inp
          -- Sum snow ice and heat content
          (sumIce, heatsum0) = VU.ifoldl' accumSnow (0.0, 0.0) (sai_h2osoi_ice inp)
          heatsum = heatsum0 + sumIce * hfus
          heatrem = (sai_t_lake1 inp - tfrz) * cpliq * denh2o * sai_dz_lake1 inp - heatsum
      in if heatrem + denh2o * sai_dz_lake1 inp * hfus > 0.0
         then let t_lake_new
                    | heatrem > 0.0 = sai_t_lake1 inp - heatsum / (cpliq * denh2o * sai_dz_lake1 inp)
                    | otherwise     = tfrz
                  icefrac_new
                    | heatrem > 0.0 = 0.0
                    | otherwise     = negate heatrem / (denh2o * sai_dz_lake1 inp * hfus)
              in SnowAboveOutput
                { sao_t_lake1         = t_lake_new
                , sao_lake_icefrac1   = icefrac_new
                , sao_snl             = 0
                , sao_h2osno_no_layers = 0.0
                , sao_snow_depth      = 0.0
                , sao_qflx_snomelt    = sai_qflx_snomelt inp + sumIce / dt
                , sao_eflx_snomelt    = sai_eflx_snomelt inp + sumIce * hfus / dt
                , sao_qflx_snow_drain = sai_qflx_snow_drain inp + sai_h2osno_total inp / dt
                }
         else noChange
  | otherwise = noChange
  where
    noChange = SnowAboveOutput
      { sao_t_lake1         = sai_t_lake1 inp
      , sao_lake_icefrac1   = sai_lake_icefrac1 inp
      , sao_snl             = sai_snl inp
      , sao_h2osno_no_layers = sai_h2osno_no_layers inp
      , sao_snow_depth      = sai_snow_depth inp
      , sao_qflx_snomelt    = sai_qflx_snomelt inp
      , sao_eflx_snomelt    = sai_eflx_snomelt inp
      , sao_qflx_snow_drain = sai_qflx_snow_drain inp
      }

    snl = sai_snl inp
    iceV = sai_h2osoi_ice inp
    liqV = sai_h2osoi_liq inp
    tV   = sai_t_soisno inp

    accumSnow (!si, !hs) jj _iceVal =
      let j_fort = jj - nlevsno  -- Fortran-style index
      in if j_fort >= snl + 1 && j_fort <= 0
         then ( si + iceV VU.! jj
              , hs + (iceV VU.! jj) * cpice * (tfrz - tV VU.! jj)
                   + (liqV VU.! jj) * cpliq * (tfrz - tV VU.! jj)
              )
         else (si, hs)

-- =========================================================================
-- lakeSnowDiagnostics
-- =========================================================================

-- | Input for snow diagnostics.
data SnowDiagInput = SnowDiagInput
  { sndi_h2osoi_ice :: !(VU.Vector Double)
  , sndi_h2osoi_liq :: !(VU.Vector Double)
  , sndi_t_soisno   :: !(VU.Vector Double)
  , sndi_snl        :: !Int
  } deriving (Show)

-- | Output from snow diagnostics.
data SnowDiagOutput = SnowDiagOutput
  { sndo_snowice       :: !Double  -- ^ Total snow ice [kg/m2]
  , sndo_snowliq       :: !Double  -- ^ Total snow liquid [kg/m2]
  , sndo_t_sno_mul_mss :: !Double  -- ^ Snow internal temperature * mass [K kg/m2]
  } deriving (Show)

-- | Accumulate snow diagnostics for a single column.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeSnowDiagnostics :: SnowDiagInput -> SnowDiagOutput
lakeSnowDiagnostics inp = SnowDiagOutput
  { sndo_snowice       = si
  , sndo_snowliq       = sl
  , sndo_t_sno_mul_mss = tmm
  }
  where
    !snl = sndi_snl inp
    iceV = sndi_h2osoi_ice inp
    liqV = sndi_h2osoi_liq inp
    tV   = sndi_t_soisno inp

    (si, sl, tmm) = foldl accum (0.0, 0.0, 0.0) [(-nlevsno + 1) .. 0]

    accum (!si0, !sl0, !tmm0) j_fort =
      let jj = j_fort + nlevsno
      in if j_fort >= snl + 1
         then ( si0 + iceV VU.! jj
              , sl0 + liqV VU.! jj
              , tmm0 + (iceV VU.! jj) * (tV VU.! jj) + (liqV VU.! jj) * tfrz
              )
         else (si0, sl0, tmm0)

-- =========================================================================
-- lakeWaterBalance
-- =========================================================================

-- | Input for lake water balance.
data WaterBalInput = WaterBalInput
  { wbi_forc_rain    :: !Double
  , wbi_forc_snow    :: !Double
  , wbi_qflx_evap_tot :: !Double
  , wbi_qflx_snwcp_ice :: !Double
  , wbi_qflx_snwcp_discarded_ice :: !Double
  , wbi_qflx_snwcp_discarded_liq :: !Double
  , wbi_qflx_liq_grnd     :: !Double
  , wbi_qflx_snow_drain   :: !Double
  , wbi_qflx_floodg       :: !Double
  , wbi_begwb              :: !Double
  , wbi_endwb              :: !Double
  , wbi_dtime              :: !Double
  } deriving (Show)

-- | Output from lake water balance.
data WaterBalOutput = WaterBalOutput
  { wbo_qflx_drain_perched      :: !Double
  , wbo_qflx_rsub_sat           :: !Double
  , wbo_qflx_infl               :: !Double
  , wbo_qflx_surf               :: !Double
  , wbo_qflx_drain              :: !Double
  , wbo_qflx_qrgwl              :: !Double
  , wbo_qflx_floodc             :: !Double
  , wbo_qflx_runoff             :: !Double
  , wbo_qflx_rain_plus_snomelt  :: !Double
  , wbo_qflx_top_soil           :: !Double
  , wbo_qflx_ice_runoff_snwcp   :: !Double
  } deriving (Show)

-- | Compute water balance for a single lake column.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeWaterBalance :: WaterBalInput -> WaterBalOutput
lakeWaterBalance inp = WaterBalOutput
  { wbo_qflx_drain_perched    = 0.0
  , wbo_qflx_rsub_sat         = 0.0
  , wbo_qflx_infl             = 0.0
  , wbo_qflx_surf             = 0.0
  , wbo_qflx_drain            = 0.0
  , wbo_qflx_qrgwl            = qrgwl
  , wbo_qflx_floodc           = wbi_qflx_floodg inp
  , wbo_qflx_runoff           = qrgwl  -- drain=0 + qrgwl
  , wbo_qflx_rain_plus_snomelt = rps
  , wbo_qflx_top_soil         = rps
  , wbo_qflx_ice_runoff_snwcp = wbi_qflx_snwcp_ice inp
  }
  where
    qrgwl = wbi_forc_rain inp + wbi_forc_snow inp
          - wbi_qflx_evap_tot inp
          - wbi_qflx_snwcp_ice inp
          - wbi_qflx_snwcp_discarded_ice inp
          - wbi_qflx_snwcp_discarded_liq inp
          - (wbi_endwb inp - wbi_begwb inp) / wbi_dtime inp
          + wbi_qflx_floodg inp
    rps = wbi_qflx_liq_grnd inp + wbi_qflx_snow_drain inp

-- =========================================================================
-- lakeUpdateH2osoiVol
-- =========================================================================

-- | Update volumetric soil water content from liquid and ice.
-- Returns soil-only vector of h2osoi_vol.
-- Ported from inline code in LakeHydrology in LakeHydrologyMod.F90.
lakeUpdateH2osoiVol
  :: VU.Vector Double  -- ^ h2osoi_liq (snow+soil)
  -> VU.Vector Double  -- ^ h2osoi_ice (snow+soil)
  -> VU.Vector Double  -- ^ dz (snow+soil)
  -> Int               -- ^ nlevgrnd
  -> VU.Vector Double  -- ^ h2osoi_vol (soil-only, length nlevgrnd)
lakeUpdateH2osoiVol liq ice dz nlg = VU.generate nlg $ \j ->
  let jj = j + nlevsno
  in (liq VU.! jj) / ((dz VU.! jj) * denh2o)
   + (ice VU.! jj) / ((dz VU.! jj) * denice)
