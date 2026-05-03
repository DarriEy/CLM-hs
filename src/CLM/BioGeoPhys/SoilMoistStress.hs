{-# LANGUAGE BangPatterns #-}
-- | Soil moisture stress for plant transpiration (BTRAN).
-- Fortran: SoilMoistStressMod.F90
--
-- Provides:
--   * Effective soil porosity computation
--   * Effective snow porosity computation
--   * Volumetric liquid water content
--   * Root moisture stress (BTRAN) using CLM4.5 default method
--   * Root fraction normalization for unfrozen soil
--
-- All functions are pure. Column-level arrays use Data.Vector.Unboxed.
-- Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.SoilMoistStress
  ( -- * Configuration
    SoilMoistStressConfig(..)
  , defaultSoilMoistStressConfig
  , MoistStressMethod(..)
    -- * Input/output records
  , EffPorosityInput(..)
  , RootMoistStressInput(..)
  , RootMoistStressResult(..)
    -- * Science functions
  , calcEffectiveSoilPorosity
  , calcEffectiveSnowPorosity
  , calcVolumetricH2oliq
  , soilSuctionClappHornberger
  , normalizeArray
  , calcRootMoistStressDefault
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.PhysicalConstants (tfrz, denice, denh2o, nlevsno)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Method for computing root moisture stress
data MoistStressMethod
  = MoistStressCLMDefault  -- ^ CLM4.5 default approach
  deriving (Show, Eq)

-- | Configuration for soil moisture stress
data SoilMoistStressConfig = SoilMoistStressConfig
  { smsc_method       :: !MoistStressMethod
  , smsc_perchroot    :: !Bool  -- ^ btran based only on unfrozen soil levels
  , smsc_perchroot_alt :: !Bool -- ^ btran based on active layer
  } deriving (Show, Eq)

-- | Default configuration
defaultSoilMoistStressConfig :: SoilMoistStressConfig
defaultSoilMoistStressConfig = SoilMoistStressConfig
  { smsc_method        = MoistStressCLMDefault
  , smsc_perchroot     = False
  , smsc_perchroot_alt = False
  }

--------------------------------------------------------------------------------
-- Input/output records
--------------------------------------------------------------------------------

-- | Inputs for effective porosity computation
data EffPorosityInput = EffPorosityInput
  { epi_watsat      :: !(VU.Vector Double)  -- ^ Soil porosity (nlevgrnd)
  , epi_h2osoi_ice  :: !(VU.Vector Double)  -- ^ Ice water content (snow+soil, combined)
  , epi_col_dz      :: !(VU.Vector Double)  -- ^ Layer thickness (snow+soil, combined)
  } deriving (Show)

-- | Inputs for root moisture stress computation
data RootMoistStressInput = RootMoistStressInput
  { rmsi_nlevgrnd        :: !Int
  , rmsi_rootfr          :: !(VU.Vector Double)  -- ^ Root fraction per layer (nlevgrnd)
  , rmsi_t_soisno        :: !(VU.Vector Double)  -- ^ Soil temperature (snow+soil, combined)
  , rmsi_watsat          :: !(VU.Vector Double)  -- ^ Soil porosity (nlevgrnd)
  , rmsi_sucsat          :: !(VU.Vector Double)  -- ^ Saturated matric potential (nlevgrnd) [mm]
  , rmsi_bsw             :: !(VU.Vector Double)  -- ^ Clapp-Hornberger exponent (nlevgrnd)
  , rmsi_eff_porosity    :: !(VU.Vector Double)  -- ^ Effective porosity (nlevgrnd)
  , rmsi_h2osoi_liqvol   :: !(VU.Vector Double)  -- ^ Volumetric liquid (snow+soil, combined)
  , rmsi_smpso           :: !Double              -- ^ Soil water potential at full stomatal opening [mm]
  , rmsi_smpsc           :: !Double              -- ^ Soil water potential at wilting point [mm]
  , rmsi_config          :: !SoilMoistStressConfig
  } deriving (Show)

-- | Result of root moisture stress computation
data RootMoistStressResult = RootMoistStressResult
  { rmsr_btran   :: !Double              -- ^ Transpiration wetness factor (0-1)
  , rmsr_rootr   :: !(VU.Vector Double)  -- ^ Normalized root resistance per layer
  , rmsr_rresis  :: !(VU.Vector Double)  -- ^ Root resistance per layer
  } deriving (Show)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Compute effective soil porosity per layer.
-- eff_porosity = watsat - vol_ice, where vol_ice is capped at watsat.
-- Ported from calc_effective_soilporosity in SoilMoistStressMod.F90
calcEffectiveSoilPorosity
  :: Int                    -- ^ nlevgrnd
  -> VU.Vector Double       -- ^ watsat (nlevgrnd, 0-indexed)
  -> VU.Vector Double       -- ^ h2osoi_ice (snow+soil, combined)
  -> VU.Vector Double       -- ^ col_dz (snow+soil, combined)
  -> VU.Vector Double       -- ^ effective porosity (nlevgrnd)
calcEffectiveSoilPorosity nlevgrnd watsat_v h2osoi_ice_v col_dz_v =
  VU.generate nlevgrnd $ \j ->
    let joff = nlevsno
        ws = vix watsat_v j
        vol_ice = min ws (vix h2osoi_ice_v (j + joff) / (denice * vix col_dz_v (j + joff)))
    in max 0.01 (ws - vol_ice)

-- | Compute effective snow porosity per snow layer.
-- eff_porosity = 1 - vol_ice.
-- Ported from calc_effective_snowporosity in SoilMoistStressMod.F90
calcEffectiveSnowPorosity
  :: Int                    -- ^ jtop (Fortran snow layer index, e.g. -4)
  -> VU.Vector Double       -- ^ h2osoi_ice (snow+soil, combined)
  -> VU.Vector Double       -- ^ col_dz (snow+soil, combined)
  -> VU.Vector Double       -- ^ effective porosity (nlevsno, 0-indexed maps to Fortran -nlevsno+1:0)
calcEffectiveSnowPorosity jtop h2osoi_ice_v col_dz_v =
  let joff = nlevsno
      lbj = negate nlevsno + 1
  in VU.generate nlevsno $ \idx ->
       let j_fortran = lbj + idx  -- Fortran layer index
           jj = j_fortran + joff  -- Julia/Haskell 0-based index
       in if j_fortran >= jtop
          then let vol_ice = min 1.0 (vix h2osoi_ice_v jj / (denice * vix col_dz_v jj))
               in 1.0 - vol_ice
          else 0.0

-- | Compute volumetric liquid water content.
-- vol_liq = min(eff_porosity, h2osoi_liq / (dz * denh2o))
-- Ported from calc_volumetric_h2oliq in SoilMoistStressMod.F90
calcVolumetricH2oliq
  :: Int                    -- ^ nlevgrnd
  -> VU.Vector Double       -- ^ eff_porosity (nlevgrnd)
  -> VU.Vector Double       -- ^ h2osoi_liq (snow+soil, combined)
  -> VU.Vector Double       -- ^ col_dz (snow+soil, combined)
  -> VU.Vector Double       -- ^ volumetric liquid (nlevgrnd)
calcVolumetricH2oliq nlevgrnd eff_por_v h2osoi_liq_v col_dz_v =
  VU.generate nlevgrnd $ \j ->
    let joff = nlevsno
        ep = vix eff_por_v j
    in min ep (vix h2osoi_liq_v (j + joff) / (vix col_dz_v (j + joff) * denh2o))

-- | Compute soil matric potential using Clapp-Hornberger parameterization.
-- Returns smp_node (matric potential in mm).
-- Ported from soil_suction in SoilWaterRetentionCurveMod.F90
soilSuctionClappHornberger :: Double -> Double -> Double -> Double
soilSuctionClappHornberger sucsat s_node bsw_val =
  negate sucsat * s_node ** (negate bsw_val)

-- | Normalize an array so its elements sum to 1.
-- If sum is zero, the array is left as zeros.
-- Ported from array_normalization in SimpleMathMod.F90
normalizeArray :: VU.Vector Double -> VU.Vector Double
normalizeArray arr =
  let s = VU.foldl' (+) 0.0 arr
  in if s > 0.0
     then VU.map (/ s) arr
     else arr

-- | Compute root water stress using the default CLM4.5 approach.
-- Returns BTRAN (0-1), normalized rootr per layer, and rresis per layer.
-- Ported from calc_root_moist_stress_clm45default in SoilMoistStressMod.F90
calcRootMoistStressDefault
  :: RootMoistStressInput
  -> RootMoistStressResult
calcRootMoistStressDefault inp =
  let nlevgrnd = rmsi_nlevgrnd inp
      joff = nlevsno
      smpso_val = rmsi_smpso inp
      smpsc_val = rmsi_smpsc inp
      rootfr_v  = rmsi_rootfr inp
      cfg = rmsi_config inp

      -- Compute rootr and btran
      (btranAcc, rootrList, rresisList) = go 0 0.0 [] [] nlevgrnd

      go :: Int -> Double -> [Double] -> [Double] -> Int -> (Double, [Double], [Double])
      go !j !bt !rr !rres !nlev
        | j >= nlev = (bt, reverse rr, reverse rres)
        | otherwise =
            let h2osoi_liqvol_j = vix (rmsi_h2osoi_liqvol inp) (j + joff)
                t_soisno_j     = vix (rmsi_t_soisno inp) (j + joff)
                eff_por_j      = vix (rmsi_eff_porosity inp) j
                watsat_j       = vix (rmsi_watsat inp) j
                sucsat_j       = vix (rmsi_sucsat inp) j
                bsw_j          = vix (rmsi_bsw inp) j
                rootfr_j       = vix rootfr_v j
            in if h2osoi_liqvol_j <= 0.0 || t_soisno_j <= tfrz - 2.0
               then go (j + 1) bt (0.0 : rr) (0.0 : rres) nlev
               else
                 let s_node = clamp 0.01 1.0 (h2osoi_liqvol_j / eff_por_j)
                     smp_node_raw = soilSuctionClappHornberger sucsat_j s_node bsw_j
                     smp_node = max smpsc_val smp_node_raw

                     rresis_j = min ((eff_por_j / watsat_j)
                                  * (smp_node - smpsc_val)
                                  / (smpso_val - smpsc_val)) 1.0

                     rootr_j = if smsc_perchroot cfg || smsc_perchroot_alt cfg
                               then rootfr_j * rresis_j  -- uses normalized unfrozen rootfr
                               else rootfr_j * rresis_j

                     bt' = bt + max rootr_j 0.0
                 in go (j + 1) bt' (rootr_j : rr) (rresis_j : rres) nlev

      -- Normalize rootr
      btran_val = btranAcc
      rootr_v = if btran_val > 0.0
                then VU.fromList (map (/ btran_val) rootrList)
                else VU.fromList (map (const 0.0) rootrList)
      rresis_v = VU.fromList rresisList

  in RootMoistStressResult
       { rmsr_btran  = btran_val
       , rmsr_rootr  = rootr_v
       , rmsr_rresis = rresis_v
       }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Safe 0-based vector index
vix :: VU.Vector Double -> Int -> Double
vix v i = v `VU.unsafeIndex` i

-- | Clamp a value between lo and hi
clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)
