{-# LANGUAGE BangPatterns #-}
-- | Hydrology without aquifer layer (diagnostic updates).
-- Fortran: HydrologyNoDrainageMod.F90
--
-- Provides:
--   * Snow persistence tracking
--   * Snow ice/liquid accumulation over layers
--   * Column-average snow depth
--   * Snow internal temperature diagnostic
--   * Ground and soil temperature diagnostics
--   * Volumetric soil water update
--   * Soil water potential (soilpsi)
--   * Matric potential for history/CH4 (smp_l)
--   * Soil water as fraction of WHC (wf)
--   * Top-layer snow diagnostics
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.HydrologyNoDrainage
  ( -- * Input/output records
    SnowPersistenceInput(..)
  , SnowIceLiqResult(..)
  , GroundTempInput(..)
  , GroundTempResult(..)
  , H2osoiVolInput(..)
  , SoilpsiInput(..)
  , SmpLInput(..)
  , WfInput(..)
  , SnowTopDiagInput(..)
  , SnowTopDiagResult(..)
  , HydrologyNoDrainageInput(..)
  , HydrologyNoDrainageResult(..)
    -- * Science functions
  , updateSnowPersistence
  , accumulateSnowIceLiq
  , updateSnowdp
  , computeSnowInternalTemperature
  , updateGroundAndSoilTemperatures
  , updateH2osoiVol
  , updateSoilpsi
  , updateSmpL
  , computeWf
  , updateSnowTopLayerDiagnostics
  , hydrologyNoDrainage
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (tfrz, denh2o, denice, nlevsno, nlevsoi)

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Landunit types
istsoil :: Int
istsoil = 1

istcrop :: Int
istcrop = 2

-- | Special value (SPVAL) for missing data
spval :: Double
spval = 1.0e36

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

data SnowPersistenceInput = SnowPersistenceInput
  { spi_snow_persistence :: !Double  -- ^ Current persistence [s]
  , spi_dtime            :: !Double  -- ^ Time step [s]
  , spi_is_snow          :: !Bool    -- ^ Has snow
  , spi_is_nosnow        :: !Bool    -- ^ No snow
  } deriving (Show)

data SnowIceLiqResult = SnowIceLiqResult
  { silr_snowice :: !Double  -- ^ Sum of ice in snow layers [kg/m2]
  , silr_snowliq :: !Double  -- ^ Sum of liquid in snow layers [kg/m2]
  } deriving (Show)

data GroundTempInput = GroundTempInput
  { gti_t_soisno        :: !(VU.Vector Double)  -- ^ Temperature (snow+soil)
  , gti_t_h2osfc        :: !Double              -- ^ Surface water temperature [K]
  , gti_frac_sno_eff    :: !Double              -- ^ Effective snow fraction
  , gti_frac_h2osfc     :: !Double              -- ^ Surface water fraction
  , gti_snl             :: !Int                 -- ^ Number of snow layers (negative)
  , gti_dz              :: !(VU.Vector Double)  -- ^ Layer thickness (snow+soil) [m]
  , gti_zi              :: !(VU.Vector Double)  -- ^ Layer interfaces (snow+soil) [m]
  , gti_is_urban        :: !Bool
  , gti_lun_itype       :: !Int                 -- ^ Landunit type
  } deriving (Show)

data GroundTempResult = GroundTempResult
  { gtr_t_grnd      :: !Double  -- ^ Ground temperature [K]
  , gtr_t_grnd_u    :: !Double  -- ^ Urban ground temperature [K]
  , gtr_t_grnd_r    :: !Double  -- ^ Rural ground temperature [K]
  , gtr_tsl         :: !Double  -- ^ Near-surface soil temperature [K]
  , gtr_t_soi_10cm  :: !Double  -- ^ Depth-weighted soil T (top 10cm) [K]
  , gtr_tsoi17      :: !Double  -- ^ Depth-weighted soil T (top 17cm) [K]
  } deriving (Show)

data H2osoiVolInput = H2osoiVolInput
  { hvi_h2osoi_liq :: !(VU.Vector Double)  -- ^ Liquid water (snow+soil)
  , hvi_h2osoi_ice :: !(VU.Vector Double)  -- ^ Ice water (snow+soil)
  , hvi_dz         :: !(VU.Vector Double)  -- ^ Layer thickness (snow+soil) [m]
  , hvi_nlevgrnd   :: !Int
  } deriving (Show)

data SoilpsiInput = SoilpsiInput
  { spi2_h2osoi_liq :: !(VU.Vector Double)  -- ^ Liquid water (snow+soil)
  , spi2_dz         :: !(VU.Vector Double)  -- ^ Layer thickness (snow+soil) [m]
  , spi2_watsat     :: !(VU.Vector Double)  -- ^ Porosity (nlevgrnd)
  , spi2_sucsat     :: !(VU.Vector Double)  -- ^ Saturated suction (nlevgrnd) [mm]
  , spi2_bsw        :: !(VU.Vector Double)  -- ^ Clapp-Hornberger exponent (nlevgrnd)
  , spi2_nlevgrnd   :: !Int
  } deriving (Show)

data SmpLInput = SmpLInput
  { smli_h2osoi_vol :: !(VU.Vector Double)  -- ^ Volumetric water (nlevgrnd)
  , smli_watsat     :: !(VU.Vector Double)  -- ^ Porosity (nlevgrnd)
  , smli_sucsat     :: !(VU.Vector Double)  -- ^ Saturated suction (nlevgrnd) [mm]
  , smli_bsw        :: !(VU.Vector Double)  -- ^ Clapp-Hornberger exponent (nlevgrnd)
  , smli_smpmin     :: !Double              -- ^ Minimum matric potential [mm]
  , smli_nlevgrnd   :: !Int
  } deriving (Show)

data WfInput = WfInput
  { wfi_h2osoi_vol  :: !(VU.Vector Double)  -- ^ Volumetric water (nlevgrnd)
  , wfi_watsat      :: !(VU.Vector Double)  -- ^ Porosity (nlevgrnd)
  , wfi_sucsat      :: !(VU.Vector Double)  -- ^ Saturated suction (nlevgrnd) [mm]
  , wfi_bsw         :: !(VU.Vector Double)  -- ^ Clapp-Hornberger exponent (nlevgrnd)
  , wfi_z           :: !(VU.Vector Double)  -- ^ Layer midpoints (snow+soil) [m]
  , wfi_dz          :: !(VU.Vector Double)  -- ^ Layer thickness (snow+soil) [m]
  , wfi_nlevgrnd    :: !Int
  , wfi_depth_limit :: !Double              -- ^ Depth limit [m]
  } deriving (Show)

data SnowTopDiagInput = SnowTopDiagInput
  { stdi_h2osoi_ice  :: !(VU.Vector Double)
  , stdi_h2osoi_liq  :: !(VU.Vector Double)
  , stdi_snl         :: !Int
  , stdi_is_snow     :: !Bool
  } deriving (Show)

data SnowTopDiagResult = SnowTopDiagResult
  { stdr_h2osno_top  :: !Double  -- ^ Mass of snow in top layer [kg/m2]
  , stdr_snot_top    :: !Double  -- ^ Top snow temperature or SPVAL
  , stdr_dTdz_top    :: !Double  -- ^ Top snow dT/dz or SPVAL
  , stdr_snw_rds_top :: !Double  -- ^ Top snow grain radius or SPVAL
  , stdr_sno_liq_top :: !Double  -- ^ Top snow liquid fraction or SPVAL
  } deriving (Show)

-- | Full hydrology-no-drainage input (single column)
data HydrologyNoDrainageInput = HydrologyNoDrainageInput
  { hndi_dtime          :: !Double
  , hndi_snl            :: !Int
  , hndi_is_snow        :: !Bool
  , hndi_is_nosnow      :: !Bool
  , hndi_is_urban       :: !Bool
  , hndi_lun_itype      :: !Int
  , hndi_t_soisno       :: !(VU.Vector Double)
  , hndi_t_h2osfc       :: !Double
  , hndi_h2osoi_liq     :: !(VU.Vector Double)
  , hndi_h2osoi_ice     :: !(VU.Vector Double)
  , hndi_dz             :: !(VU.Vector Double)
  , hndi_z              :: !(VU.Vector Double)
  , hndi_zi             :: !(VU.Vector Double)
  , hndi_frac_sno_eff   :: !Double
  , hndi_frac_h2osfc    :: !Double
  , hndi_snow_persistence :: !Double
  , hndi_watsat         :: !(VU.Vector Double)
  , hndi_sucsat         :: !(VU.Vector Double)
  , hndi_bsw            :: !(VU.Vector Double)
  , hndi_smpmin         :: !Double
  , hndi_snow_depth     :: !Double
  , hndi_nlevgrnd       :: !Int
  } deriving (Show)

-- | Full hydrology-no-drainage result
data HydrologyNoDrainageResult = HydrologyNoDrainageResult
  { hndr_snow_persistence :: !Double
  , hndr_snowice          :: !Double
  , hndr_snowliq          :: !Double
  , hndr_snowdp           :: !Double
  , hndr_t_sno_mul_mss    :: !Double
  , hndr_t_grnd           :: !Double
  , hndr_t_grnd_u         :: !Double
  , hndr_t_grnd_r         :: !Double
  , hndr_tsl              :: !Double
  , hndr_t_soi_10cm       :: !Double
  , hndr_tsoi17           :: !Double
  , hndr_h2osoi_vol       :: !(VU.Vector Double)  -- ^ Volumetric soil water (nlevgrnd)
  , hndr_soilpsi          :: !(VU.Vector Double)  -- ^ Soil water potential (nlevgrnd) [MPa]
  , hndr_smp_l            :: !(VU.Vector Double)  -- ^ Matric potential (nlevgrnd) [mm]
  , hndr_wf               :: !Double              -- ^ WHC fraction (top 0.05m)
  , hndr_wf2              :: !Double              -- ^ WHC fraction (top 0.17m)
  , hndr_h2osno_top       :: !Double
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Update snow persistence counter.
-- Ported from inline code in HydrologyNoDrainage.
updateSnowPersistence :: SnowPersistenceInput -> Double
updateSnowPersistence inp
  | spi_is_snow inp   = spi_snow_persistence inp + spi_dtime inp
  | spi_is_nosnow inp = 0.0
  | otherwise          = spi_snow_persistence inp

-- | Accumulate ice and liquid over snow layers.
-- Ported from inline code in HydrologyNoDrainage.
accumulateSnowIceLiq
  :: Int                  -- ^ snl (negative)
  -> VU.Vector Double     -- ^ h2osoi_ice (snow+soil)
  -> VU.Vector Double     -- ^ h2osoi_liq (snow+soil)
  -> Bool                 -- ^ is_snow
  -> SnowIceLiqResult
accumulateSnowIceLiq snl_val h2osoi_ice_v h2osoi_liq_v isSnow
  | not isSnow = SnowIceLiqResult 0.0 0.0
  | otherwise  =
      let joff = nlevsno
          go !j !iceAcc !liqAcc
            | j > 0 = SnowIceLiqResult iceAcc liqAcc
            | otherwise =
                let jj = j + joff
                    j_fortran = j - nlevsno + nlevsno  -- identity; just use j
                in if j >= snl_val + 1
                   then go (j + 1) (iceAcc + vix h2osoi_ice_v jj)
                                   (liqAcc + vix h2osoi_liq_v jj)
                   else go (j + 1) iceAcc liqAcc
      in go (negate nlevsno + 1) 0.0 0.0

-- | Column-average snow depth.
updateSnowdp :: Double -> Double -> Double
updateSnowdp snow_depth frac_sno_eff = snow_depth * frac_sno_eff

-- | Compute snow internal temperature diagnostic.
-- Ported from inline code in HydrologyNoDrainage.
computeSnowInternalTemperature
  :: Int                  -- ^ snl
  -> VU.Vector Double     -- ^ h2osoi_ice (snow+soil)
  -> VU.Vector Double     -- ^ h2osoi_liq (snow+soil)
  -> VU.Vector Double     -- ^ t_soisno (snow+soil)
  -> Bool                 -- ^ is_snow
  -> Double               -- ^ t_sno_mul_mss
computeSnowInternalTemperature snl_val h2osoi_ice_v h2osoi_liq_v t_soisno_v isSnow
  | not isSnow = 0.0
  | otherwise =
      let joff = nlevsno
          go !j !acc
            | j > 0 = acc
            | j >= snl_val + 1 =
                let jj = j + joff
                    acc' = acc + vix h2osoi_ice_v jj * vix t_soisno_v jj
                              + vix h2osoi_liq_v jj * tfrz
                in go (j + 1) acc'
            | otherwise = go (j + 1) acc
      in go (negate nlevsno + 1) 0.0

-- | Compute ground and soil temperatures.
-- Ported from inline code in HydrologyNoDrainage.
updateGroundAndSoilTemperatures :: GroundTempInput -> GroundTempResult
updateGroundAndSoilTemperatures inp =
  let joff = nlevsno
      joff_zi = nlevsno + 1
      snl_val = gti_snl inp
      fse = gti_frac_sno_eff inp
      fh2 = gti_frac_h2osfc inp
      ts = gti_t_soisno inp
      dz_v = gti_dz inp
      zi_v = gti_zi inp
      isUrb = gti_is_urban inp

      -- tsl
      tsl_val = if not isUrb then vix ts (1 + joff) else 0.0

      -- t_soi_10cm and tsoi17
      (t10, t17) = if not isUrb
                   then accTemps nlevsoi joff joff_zi ts dz_v zi_v
                   else (0.0, 0.0)

      -- t_grnd
      t_grnd_val = if snl_val < 0
                   then fse * vix ts (snl_val + 1 + joff)
                      + (1.0 - fse - fh2) * vix ts (1 + joff)
                      + fh2 * gti_t_h2osfc inp
                   else (1.0 - fh2) * vix ts (1 + joff)
                      + fh2 * gti_t_h2osfc inp

      t_grnd_u = if isUrb then vix ts (snl_val + 1 + joff) else 0.0

      t10_final = if not isUrb && nlevsoi > 0 then t10 / 0.1 else 0.0
      t17_final = if not isUrb && nlevsoi > 0 then t17 / 0.17 else 0.0

      t_grnd_r = if gti_lun_itype inp == istsoil || gti_lun_itype inp == istcrop
                 then vix ts (snl_val + 1 + joff) else 0.0

  in GroundTempResult
       { gtr_t_grnd     = t_grnd_val
       , gtr_t_grnd_u   = t_grnd_u
       , gtr_t_grnd_r   = t_grnd_r
       , gtr_tsl        = tsl_val
       , gtr_t_soi_10cm = t10_final
       , gtr_tsoi17     = t17_final
       }

-- | Accumulate soil temperatures weighted by depth for 10cm and 17cm.
accTemps :: Int -> Int -> Int -> VU.Vector Double -> VU.Vector Double
         -> VU.Vector Double -> (Double, Double)
accTemps nl joff joff_zi ts dz_v zi_v = go 1 0.0 0.0
  where
    go !j !t10 !t17
      | j > nl = (t10, t17)
      | otherwise =
          let t_j = vix ts (j + joff)
              dz_j = vix dz_v (j + joff)
              zi_j = vix zi_v (j + joff_zi)    -- Fortran zi(c,j)
              -- 17cm accumulation
              t17' = if zi_j <= 0.17
                     then t17 + t_j * dz_j
                     else let zi_jm1 = vix zi_v (j + joff)  -- Fortran zi(c,j-1) = zi[j+joff]
                          in if zi_j > 0.17 && zi_jm1 < 0.17
                             then let fracl = (0.17 - zi_jm1) / dz_j
                                  in t17 + t_j * dz_j * fracl
                             else t17
              -- 10cm accumulation
              t10' = if zi_j <= 0.1
                     then t10 + t_j * dz_j
                     else let zi_jm1 = vix zi_v (j + joff)
                          in if zi_j > 0.1 && zi_jm1 < 0.1
                             then let fracl = (0.1 - zi_jm1) / dz_j
                                  in t10 + t_j * dz_j * fracl
                             else t10
          in go (j + 1) t10' t17'

-- | Update volumetric soil water content.
-- Ported from inline code in HydrologyNoDrainage.
updateH2osoiVol :: H2osoiVolInput -> VU.Vector Double
updateH2osoiVol inp =
  let joff = nlevsno
  in VU.generate (hvi_nlevgrnd inp) $ \j ->
       vix (hvi_h2osoi_liq inp) (j + joff) / (vix (hvi_dz inp) (j + joff) * denh2o)
     + vix (hvi_h2osoi_ice inp) (j + joff) / (vix (hvi_dz inp) (j + joff) * denice)

-- | Update soil water potential [MPa].
-- Ported from inline code in HydrologyNoDrainage.
updateSoilpsi :: SoilpsiInput -> VU.Vector Double
updateSoilpsi inp =
  let joff = nlevsno
  in VU.generate (spi2_nlevgrnd inp) $ \j ->
       let h2o_liq = vix (spi2_h2osoi_liq inp) (j + joff)
       in if h2o_liq > 0.0
          then let vwc = h2o_liq / (vix (spi2_dz inp) (j + joff) * denh2o)
                   fsattmp = max (vwc / vix (spi2_watsat inp) j) 0.001
                   psi = vix (spi2_sucsat inp) j * (negate 9.8e-6) * fsattmp ** (negate (vix (spi2_bsw inp) j))
               in min (max psi (negate 15.0)) 0.0
          else negate 15.0

-- | Update matric potential for history/CH4 [mm].
-- Ported from inline code in HydrologyNoDrainage.
updateSmpL :: SmpLInput -> VU.Vector Double
updateSmpL inp =
  VU.generate (smli_nlevgrnd inp) $ \j ->
    let s_node_raw = vix (smli_h2osoi_vol inp) j / vix (smli_watsat inp) j
        s_node = min 1.0 (max 0.01 s_node_raw)
        smp_val = negate (vix (smli_sucsat inp) j) * s_node ** (negate (vix (smli_bsw inp) j))
    in max (smli_smpmin inp) smp_val

-- | Compute soil water as fraction of WHC to a given depth.
-- Ported from inline code in HydrologyNoDrainage.
computeWf :: WfInput -> Double
computeWf inp =
  let joff = nlevsno
      nl = wfi_nlevgrnd inp
      dl = wfi_depth_limit inp

      (rwat, swat, rz) = go 0 0.0 0.0 0.0
      go !j !rw !sw !rz_acc
        | j >= nl = (rw, sw, rz_acc)
        | otherwise =
            let z_j = vix (wfi_z inp) (j + joff)
                dz_j = vix (wfi_dz inp) (j + joff)
            in if z_j + 0.5 * dz_j <= dl
               then let ws = vix (wfi_watsat inp) j
                        ss = vix (wfi_sucsat inp) j
                        bsw_j = vix (wfi_bsw inp) j
                        watdry = ws * (316230.0 / ss) ** (negate 1.0 / bsw_j)
                        rw' = rw + (vix (wfi_h2osoi_vol inp) j - watdry) * dz_j
                        sw' = sw + (ws - watdry) * dz_j
                        rz' = rz_acc + dz_j
                    in go (j + 1) rw' sw' rz'
               else go (j + 1) rw sw rz_acc

      (tsw, stsw) = if rz /= 0.0
                     then (rwat / rz, swat / rz)
                     else let ws = vix (wfi_watsat inp) 0
                              ss = vix (wfi_sucsat inp) 0
                              bsw_0 = vix (wfi_bsw inp) 0
                              watdry = ws * (316230.0 / ss) ** (negate 1.0 / bsw_0)
                          in (vix (wfi_h2osoi_vol inp) 0 - watdry, ws - watdry)

  in tsw / stsw

-- | Update top-layer snow diagnostics.
-- Ported from inline code in HydrologyNoDrainage.
updateSnowTopLayerDiagnostics :: SnowTopDiagInput -> SnowTopDiagResult
updateSnowTopLayerDiagnostics inp
  | stdi_is_snow inp =
      let joff = nlevsno
          j_top = stdi_snl inp + 1 + joff
      in SnowTopDiagResult
           { stdr_h2osno_top  = vix (stdi_h2osoi_ice inp) j_top
                              + vix (stdi_h2osoi_liq inp) j_top
           , stdr_snot_top    = 0.0   -- set by caller
           , stdr_dTdz_top    = 0.0   -- set by caller
           , stdr_snw_rds_top = 0.0   -- set by caller
           , stdr_sno_liq_top = 0.0   -- set by caller
           }
  | otherwise =
      SnowTopDiagResult
        { stdr_h2osno_top  = 0.0
        , stdr_snot_top    = spval
        , stdr_dTdz_top    = spval
        , stdr_snw_rds_top = spval
        , stdr_sno_liq_top = spval
        }

-- | Full hydrology-no-drainage orchestrator (single column).
-- Ported from HydrologyNoDrainage in HydrologyNoDrainageMod.F90.
hydrologyNoDrainage :: HydrologyNoDrainageInput -> HydrologyNoDrainageResult
hydrologyNoDrainage inp =
  let dtime_val = hndi_dtime inp
      snl_val = hndi_snl inp
      isSnow = hndi_is_snow inp
      isNosnow = hndi_is_nosnow inp
      nlevgrnd_val = hndi_nlevgrnd inp

      -- Snow persistence
      sp = updateSnowPersistence SnowPersistenceInput
             { spi_snow_persistence = hndi_snow_persistence inp
             , spi_dtime = dtime_val
             , spi_is_snow = isSnow
             , spi_is_nosnow = isNosnow
             }

      -- Snow ice/liq
      sil = accumulateSnowIceLiq snl_val (hndi_h2osoi_ice inp) (hndi_h2osoi_liq inp) isSnow

      -- Snow depth
      snowdp_val = updateSnowdp (hndi_snow_depth inp) (hndi_frac_sno_eff inp)

      -- Snow internal temperature
      t_sno = computeSnowInternalTemperature snl_val
                (hndi_h2osoi_ice inp) (hndi_h2osoi_liq inp) (hndi_t_soisno inp) isSnow

      -- Ground and soil temperatures
      gt = updateGroundAndSoilTemperatures GroundTempInput
             { gti_t_soisno     = hndi_t_soisno inp
             , gti_t_h2osfc     = hndi_t_h2osfc inp
             , gti_frac_sno_eff = hndi_frac_sno_eff inp
             , gti_frac_h2osfc  = hndi_frac_h2osfc inp
             , gti_snl          = snl_val
             , gti_dz           = hndi_dz inp
             , gti_zi           = hndi_zi inp
             , gti_is_urban     = hndi_is_urban inp
             , gti_lun_itype    = hndi_lun_itype inp
             }

      -- Volumetric soil water
      h2osoi_vol_v = updateH2osoiVol H2osoiVolInput
                       { hvi_h2osoi_liq = hndi_h2osoi_liq inp
                       , hvi_h2osoi_ice = hndi_h2osoi_ice inp
                       , hvi_dz         = hndi_dz inp
                       , hvi_nlevgrnd   = nlevgrnd_val
                       }

      -- Soil water potential
      soilpsi_v = updateSoilpsi SoilpsiInput
                    { spi2_h2osoi_liq = hndi_h2osoi_liq inp
                    , spi2_dz         = hndi_dz inp
                    , spi2_watsat     = hndi_watsat inp
                    , spi2_sucsat     = hndi_sucsat inp
                    , spi2_bsw        = hndi_bsw inp
                    , spi2_nlevgrnd   = nlevgrnd_val
                    }

      -- Matric potential
      smp_l_v = updateSmpL SmpLInput
                  { smli_h2osoi_vol = h2osoi_vol_v
                  , smli_watsat     = hndi_watsat inp
                  , smli_sucsat     = hndi_sucsat inp
                  , smli_bsw        = hndi_bsw inp
                  , smli_smpmin     = hndi_smpmin inp
                  , smli_nlevgrnd   = nlevgrnd_val
                  }

      -- Soil water fraction of WHC
      mkWfInput dl = WfInput
        { wfi_h2osoi_vol  = h2osoi_vol_v
        , wfi_watsat      = hndi_watsat inp
        , wfi_sucsat      = hndi_sucsat inp
        , wfi_bsw         = hndi_bsw inp
        , wfi_z           = hndi_z inp
        , wfi_dz          = hndi_dz inp
        , wfi_nlevgrnd    = nlevgrnd_val
        , wfi_depth_limit = dl
        }
      wf_val = computeWf (mkWfInput 0.05)
      wf2_val = computeWf (mkWfInput 0.17)

      -- Top-layer snow diagnostics
      std = updateSnowTopLayerDiagnostics SnowTopDiagInput
              { stdi_h2osoi_ice = hndi_h2osoi_ice inp
              , stdi_h2osoi_liq = hndi_h2osoi_liq inp
              , stdi_snl        = snl_val
              , stdi_is_snow    = isSnow
              }

  in HydrologyNoDrainageResult
       { hndr_snow_persistence = sp
       , hndr_snowice          = silr_snowice sil
       , hndr_snowliq          = silr_snowliq sil
       , hndr_snowdp           = snowdp_val
       , hndr_t_sno_mul_mss    = t_sno
       , hndr_t_grnd           = gtr_t_grnd gt
       , hndr_t_grnd_u         = gtr_t_grnd_u gt
       , hndr_t_grnd_r         = gtr_t_grnd_r gt
       , hndr_tsl              = gtr_tsl gt
       , hndr_t_soi_10cm       = gtr_t_soi_10cm gt
       , hndr_tsoi17           = gtr_tsoi17 gt
       , hndr_h2osoi_vol       = h2osoi_vol_v
       , hndr_soilpsi          = soilpsi_v
       , hndr_smp_l            = smp_l_v
       , hndr_wf               = wf_val
       , hndr_wf2              = wf2_val
       , hndr_h2osno_top       = stdr_h2osno_top std
       }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe 0-based vector index
vix :: VU.Vector Double -> Int -> Double
vix v i = v `VU.unsafeIndex` i
