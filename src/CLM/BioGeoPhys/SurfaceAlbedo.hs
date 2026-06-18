-- | Surface albedo computation: two-stream canopy radiation, soil albedo,
-- snow albedo interface, and top-level driver.
-- Fortran: SurfaceAlbedoMod.F90
-- Julia:   src/biogeophys/surface_albedo.jl
module CLM.BioGeoPhys.SurfaceAlbedo
  ( -- * Constants
    albice
  , alblak
  , calb
  , mpeAlbedo
  , omegas
  , betads
  , betais
    -- * Data types
  , SurfaceAlbedoConstants(..)
  , defaultSurfAlbConstants
  , SoilAlbedoInput(..)
  , SoilAlbedoResult(..)
  , TwoStreamParams(..)
  , TwoStreamInput(..)
  , TwoStreamResult(..)
  , defaultTwoStreamResult
  , GroundAlbedoResult(..)
  , CanopyLayerResult(..)
  , VcmaxScaling(..)
    -- * Driver types
  , SurfAlbDriverInput(..)
  , SurfAlbDriverOutput(..)
    -- * Driver function
  , surfaceAlbedoDriver
    -- * Functions
  , initSoilAlbedoTables
  , soilAlbedo
  , twoStreamParams
  , twoStreamWaveband
  , groundAlbedo
  , canopyLayering
  , defaultVcmaxScaling
  , snowAlbedoFallback
  , weightReflTrans
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (tfrz, numrad)

-- ========================================================================
-- Constants (Fortran module-level, Julia const)
-- ========================================================================

-- | Albedo of land ice by waveband (1=vis, 2=nir)
albice :: VU.Vector Double
albice = VU.fromList [0.80, 0.55]

-- | Albedo of frozen lakes by waveband (1=vis, 2=nir)
alblak :: VU.Vector Double
alblak = VU.fromList [0.60, 0.40]

-- | Coefficient for ice fraction in lake albedo (Mironov 2010)
calb :: Double
calb = 95.6

-- | Machine precision epsilon for albedo divisions
mpeAlbedo :: Double
mpeAlbedo = 1.0e-06

-- | Single-scattering albedo of snow for each waveband [vis, nir]
omegas :: VU.Vector Double
omegas = VU.fromList [0.8, 0.4]

-- | Upscatter for direct beam by snow
betads :: Double
betads = 0.5

-- | Upscatter for diffuse radiation by snow
betais :: Double
betais = 0.5

-- | Number of canopy layers (1 = big-leaf)
nlevcan :: Int
nlevcan = 1

-- ========================================================================
-- Landunit type constants (matching Fortran istsoil etc.)
-- ========================================================================

istsoil, istcrop, istice, istdlak :: Int
istsoil = 1
istcrop = 2
istice  = 4  -- ISTICE = 4 (land ice/glacier); 3 = ocean (unused)
istdlak = 5  -- ISTDLAK = 5 (deep lake)

-- ========================================================================
-- Data types
-- ========================================================================

-- | Time-constant albedo data initialized once per simulation.
-- Corresponds to Fortran module variables albsat, albdry, isoicol, alblakwi.
data SurfaceAlbedoConstants = SurfaceAlbedoConstants
  { albsat   :: !(VU.Vector Double)  -- ^ Wet soil albedo (mxsoil_color * numrad), row-major
  , albdry   :: !(VU.Vector Double)  -- ^ Dry soil albedo (mxsoil_color * numrad), row-major
  , mxsoilColor :: !Int              -- ^ Number of soil color classes
  , isoicol  :: !(VU.Vector Int)     -- ^ Soil color class per column
  , alblakwi :: !(VU.Vector Double)  -- ^ Lake melt albedo by waveband
  , snowveg_affects_radiation :: !Bool
  } deriving (Show)

-- | Index into row-major (mxsoil_color x numrad) table
albIdx :: Int -> Int -> Int -> Int
albIdx mxsc colorClass band = (colorClass - 1) * numrad + (band - 1)
{-# INLINE albIdx #-}

-- | Look up a value from the albedo table
albLookup :: VU.Vector Double -> Int -> Int -> Int -> Double
albLookup tbl mxsc colorClass band = tbl VU.! albIdx mxsc colorClass band
{-# INLINE albLookup #-}

defaultSurfAlbConstants :: SurfaceAlbedoConstants
defaultSurfAlbConstants = SurfaceAlbedoConstants
  { albsat   = VU.empty
  , albdry   = VU.empty
  , mxsoilColor = 0
  , isoicol  = VU.empty
  , alblakwi = VU.fromList [0.10, 0.10]
  , snowveg_affects_radiation = True
  }

-- | Input for single-column soil albedo computation
data SoilAlbedoInput = SoilAlbedoInput
  { sai_coszen    :: !Double   -- ^ Cosine of solar zenith angle
  , sai_lunType   :: !Int      -- ^ Landunit type (istsoil, istice, istdlak, etc.)
  , sai_h2osoi_vol1 :: !Double -- ^ Volumetric soil water content, top layer
  , sai_soilColor :: !Int      -- ^ Soil color class for this column
  , sai_t_grnd    :: !Double   -- ^ Ground temperature [K]
  , sai_snl       :: !Int      -- ^ Number of snow layers (negative or 0)
  , sai_lakePuddling    :: !Bool
  , sai_lakeIcefrac1    :: !Double  -- ^ Lake ice fraction layer 1
  , sai_lakeIcefrac2    :: !Double  -- ^ Lake ice fraction layer 2
  } deriving (Show)

-- | Result of soil albedo for a single column (per waveband)
data SoilAlbedoResult = SoilAlbedoResult
  { sar_albsod :: !(VU.Vector Double)  -- ^ Direct soil albedo by waveband
  , sar_albsoi :: !(VU.Vector Double)  -- ^ Diffuse soil albedo by waveband
  } deriving (Show)

-- | Two-stream parameters computed once per patch (waveband-independent)
data TwoStreamParams = TwoStreamParams
  { tsp_chil     :: !Double
  , tsp_gdir     :: !Double
  , tsp_twostext :: !Double
  , tsp_avmu     :: !Double
  , tsp_temp0    :: !Double
  , tsp_temp2    :: !Double
  } deriving (Show)

-- | Input for two-stream computation (per patch, per waveband)
data TwoStreamInput = TwoStreamInput
  { tsi_coszen   :: !Double   -- ^ cos(solar zenith)
  , tsi_elai     :: !Double   -- ^ Exposed LAI
  , tsi_esai     :: !Double   -- ^ Exposed SAI
  , tsi_rho      :: !Double   -- ^ Leaf/stem reflectance for this waveband
  , tsi_tau      :: !Double   -- ^ Leaf/stem transmittance for this waveband
  , tsi_xl       :: !Double   -- ^ Departure of leaf angles from spherical
  , tsi_t_veg    :: !Double   -- ^ Vegetation temperature [K]
  , tsi_fwet     :: !Double   -- ^ Fraction of canopy that is wet
  , tsi_fcansno  :: !Double   -- ^ Fraction of canopy that is snow-covered
  , tsi_albgrd   :: !Double   -- ^ Ground direct albedo (this waveband)
  , tsi_albgri   :: !Double   -- ^ Ground diffuse albedo (this waveband)
  , tsi_albsod   :: !Double   -- ^ Soil direct albedo (snow-free, this waveband)
  , tsi_albsoi   :: !Double   -- ^ Soil diffuse albedo (snow-free, this waveband)
  , tsi_ib       :: !Int      -- ^ Waveband index (0=vis, 1=nir)
  , tsi_sfOnly   :: !Bool     -- ^ If True, only compute snow-free albedo
  , tsi_snowveg  :: !Bool     -- ^ snowveg_affects_radiation flag
  } deriving (Show)

-- | Result of two-stream for one patch, one waveband
data TwoStreamResult = TwoStreamResult
  { tsr_albd    :: !Double  -- ^ Direct beam albedo above canopy
  , tsr_albi    :: !Double  -- ^ Diffuse albedo above canopy
  , tsr_fabd    :: !Double  -- ^ Direct beam absorbed fraction
  , tsr_fabi    :: !Double  -- ^ Diffuse absorbed fraction
  , tsr_ftdd    :: !Double  -- ^ Direct beam down-through fraction
  , tsr_ftid    :: !Double  -- ^ Diffuse from direct, down-through
  , tsr_ftii    :: !Double  -- ^ Diffuse down-through fraction
  , tsr_fabd_sun :: !Double -- ^ Sunlit direct absorbed
  , tsr_fabd_sha :: !Double -- ^ Shaded direct absorbed
  , tsr_fabi_sun :: !Double -- ^ Sunlit diffuse absorbed
  , tsr_fabi_sha :: !Double -- ^ Shaded diffuse absorbed
  , tsr_albdSF  :: !Double  -- ^ Snow-free direct albedo (SSRE)
  , tsr_albiSF  :: !Double  -- ^ Snow-free diffuse albedo (SSRE)
  -- Big-leaf PAR diagnostics (ib==0, nlevcan==1)
  , tsr_fsun_z1   :: !Double  -- ^ Sunlit fraction at layer 1
  , tsr_fabd_sun_z1 :: !Double
  , tsr_fabi_sun_z1 :: !Double
  , tsr_fabd_sha_z1 :: !Double
  , tsr_fabi_sha_z1 :: !Double
  , tsr_vcmaxcintsun :: !Double
  , tsr_vcmaxcintsha :: !Double
  } deriving (Show)

defaultTwoStreamResult :: TwoStreamResult
defaultTwoStreamResult = TwoStreamResult
  { tsr_albd = 1.0, tsr_albi = 1.0
  , tsr_fabd = 0.0, tsr_fabi = 0.0
  , tsr_ftdd = 0.0, tsr_ftid = 0.0, tsr_ftii = 0.0
  , tsr_fabd_sun = 0.0, tsr_fabd_sha = 0.0
  , tsr_fabi_sun = 0.0, tsr_fabi_sha = 0.0
  , tsr_albdSF = 1.0, tsr_albiSF = 1.0
  , tsr_fsun_z1 = 0.0
  , tsr_fabd_sun_z1 = 0.0, tsr_fabi_sun_z1 = 0.0
  , tsr_fabd_sha_z1 = 0.0, tsr_fabi_sha_z1 = 0.0
  , tsr_vcmaxcintsun = 0.0, tsr_vcmaxcintsha = 0.0
  }

-- | Ground albedo (snow-fraction weighted soil + snow)
data GroundAlbedoResult = GroundAlbedoResult
  { gar_albgrd :: !(VU.Vector Double)  -- ^ Ground direct albedo by waveband
  , gar_albgri :: !(VU.Vector Double)  -- ^ Ground diffuse albedo by waveband
  } deriving (Show)

-- | Canopy layer discretization result
data CanopyLayerResult = CanopyLayerResult
  { clr_nrad   :: !Int                -- ^ Number of radiatively active layers
  , clr_ncan   :: !Int                -- ^ Number of canopy layers (incl buried)
  , clr_tlai_z :: !(VU.Vector Double) -- ^ LAI per canopy layer
  , clr_tsai_z :: !(VU.Vector Double) -- ^ SAI per canopy layer
  } deriving (Show)

-- | Vcmax scaling coefficients for sun/shade
data VcmaxScaling = VcmaxScaling
  { vs_sun :: !Double
  , vs_sha :: !Double
  } deriving (Show)

defaultVcmaxScaling :: VcmaxScaling
defaultVcmaxScaling = VcmaxScaling 0.0 0.0

-- ========================================================================
-- initSoilAlbedoTables
-- ========================================================================

-- | Initialize time-constant soil albedo tables.
-- Ported from SurfaceAlbedoInitTimeConst.
--
-- Takes the maximum soil color class count (8 or 20), soil color per
-- gridcell, and column-to-gridcell mapping. Returns populated constants.
initSoilAlbedoTables
  :: Int                  -- ^ mxsoil_color (8 or 20)
  -> VU.Vector Int        -- ^ soic2d: soil color class per gridcell
  -> VU.Vector Int        -- ^ col_gridcell: gridcell index per column
  -> SurfaceAlbedoConstants
initSoilAlbedoTables mxsc soic2d col_gc =
  let nc = VU.length col_gc
      isoicol' = VU.generate nc (\c -> soic2d VU.! (col_gc VU.! c))
      -- Build row-major (mxsc x numrad) tables
      (satV, dryV)
        | mxsc == 8  = (mkTable8 albsat8vis albsat8nir, mkTable8 albdry8vis albdry8nir)
        | mxsc == 20 = (mkTable20 albsat20vis albsat20nir, mkTable20 albdry20vis albdry20nir)
        | otherwise  = error $ "initSoilAlbedoTables: mxsoil_color=" ++ show mxsc ++ " not supported"
  in  defaultSurfAlbConstants
        { albsat = satV
        , albdry = dryV
        , mxsoilColor = mxsc
        , isoicol = isoicol'
        }

-- Soil albedo lookup tables (Fortran hardcoded values)
albsat8vis, albsat8nir, albdry8vis, albdry8nir :: [Double]
albsat8vis = [0.12, 0.11, 0.10, 0.09, 0.08, 0.07, 0.06, 0.05]
albsat8nir = [0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12, 0.10]
albdry8vis = [0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12, 0.10]
albdry8nir = [0.48, 0.44, 0.40, 0.36, 0.32, 0.28, 0.24, 0.20]

albsat20vis, albsat20nir, albdry20vis, albdry20nir :: [Double]
albsat20vis = [0.25, 0.23, 0.21, 0.20, 0.19, 0.18, 0.17, 0.16,
               0.15, 0.14, 0.13, 0.12, 0.11, 0.10, 0.09, 0.08, 0.07, 0.06, 0.05, 0.04]
albsat20nir = [0.50, 0.46, 0.42, 0.40, 0.38, 0.36, 0.34, 0.32,
               0.30, 0.28, 0.26, 0.24, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12, 0.10, 0.08]
albdry20vis = [0.36, 0.34, 0.32, 0.31, 0.30, 0.29, 0.28, 0.27,
               0.26, 0.25, 0.24, 0.23, 0.22, 0.20, 0.18, 0.16, 0.14, 0.12, 0.10, 0.08]
albdry20nir = [0.61, 0.57, 0.53, 0.51, 0.49, 0.48, 0.45, 0.43,
               0.41, 0.39, 0.37, 0.35, 0.33, 0.31, 0.29, 0.27, 0.25, 0.23, 0.21, 0.16]

-- Build row-major table from vis and nir lists
mkTable8 :: [Double] -> [Double] -> VU.Vector Double
mkTable8 vis nir = VU.fromList $ concatMap (\(v, n) -> [v, n]) (zip vis nir)

mkTable20 :: [Double] -> [Double] -> VU.Vector Double
mkTable20 = mkTable8  -- same interleaving logic

-- ========================================================================
-- soilAlbedo
-- ========================================================================

-- | Compute ground surface albedo for a single column.
-- Ported from subroutine SoilAlbedo in SurfaceAlbedoMod.F90.
--
-- Returns direct and diffuse soil albedo for each waveband.
-- Only active when coszen > 0.
soilAlbedo
  :: SurfaceAlbedoConstants
  -> SoilAlbedoInput
  -> SoilAlbedoResult
soilAlbedo con inp
  | sai_coszen inp <= 0.0 =
      SoilAlbedoResult (VU.replicate numrad 0.0) (VU.replicate numrad 0.0)
  | otherwise =
      let lt = sai_lunType inp
          cosz' = sai_coszen inp
          tg = sai_t_grnd inp
          sc = sai_soilColor inp
          mxsc = mxsoilColor con
          lp = sai_lakePuddling inp
      in  SoilAlbedoResult
            { sar_albsod = VU.generate numrad (\ib -> computeSod con inp lt cosz' tg sc mxsc lp ib)
            , sar_albsoi = VU.generate numrad (\ib -> computeSoi con inp lt cosz' tg sc mxsc lp ib)
            }

computeSod :: SurfaceAlbedoConstants -> SoilAlbedoInput -> Int -> Double -> Double
           -> Int -> Int -> Bool -> Int -> Double
computeSod con inp lt cosz tg sc mxsc lp ib
  | lt == istsoil || lt == istcrop =
      let inc = max (0.11 - 0.40 * sai_h2osoi_vol1 inp) 0.0
          satVal = albLookup (albsat con) mxsc sc (ib + 1)
          dryVal = albLookup (albdry con) mxsc sc (ib + 1)
      in  min (satVal + inc) dryVal
  | lt == istice = albice VU.! ib
  | tg > tfrz || (lp && lt == istdlak && tg == tfrz
                   && sai_lakeIcefrac1 inp < 1.0
                   && sai_lakeIcefrac2 inp > 0.0) =
      -- Unfrozen lake/wetland
      0.05 / (max 0.001 cosz + 0.15)
  | lt == istdlak && (not lp) && sai_snl inp == 0 =
      -- Frozen lake without snow
      let sicefr = 1.0 - exp (-calb * (tfrz - tg) / tfrz)
          albLk  = alblak VU.! ib
          albMlt = (alblakwi con) VU.! ib
      in  sicefr * albLk + (1.0 - sicefr) * max albMlt (0.05 / (max 0.001 cosz + 0.15))
  | otherwise = alblak VU.! ib

computeSoi :: SurfaceAlbedoConstants -> SoilAlbedoInput -> Int -> Double -> Double
           -> Int -> Int -> Bool -> Int -> Double
computeSoi con inp lt cosz tg sc mxsc lp ib
  | lt == istsoil || lt == istcrop =
      computeSod con inp lt cosz tg sc mxsc lp ib  -- albsoi = albsod for soil
  | lt == istice = albice VU.! ib
  | tg > tfrz || (lp && lt == istdlak && tg == tfrz
                   && sai_lakeIcefrac1 inp < 1.0
                   && sai_lakeIcefrac2 inp > 0.0) =
      if lt == istdlak then 0.10
      else 0.05 / (max 0.001 cosz + 0.15)
  | lt == istdlak && (not lp) && sai_snl inp == 0 =
      let sicefr = 1.0 - exp (-calb * (tfrz - tg) / tfrz)
          albLk  = alblak VU.! ib
          albMlt = (alblakwi con) VU.! ib
      in  sicefr * albLk + (1.0 - sicefr) * max albMlt 0.10
  | otherwise = alblak VU.! ib

-- ========================================================================
-- Two-stream: waveband-independent parameters
-- ========================================================================

-- | Compute two-stream parameters that are independent of waveband.
-- Ported from the first loop in TwoStream (SurfaceAlbedoMod.F90).
twoStreamParams
  :: Double   -- ^ cos(solar zenith), already clamped > 0
  -> Double   -- ^ pftcon_xl for this PFT
  -> TwoStreamParams
twoStreamParams cosz0 xl0 =
  let cosz = max 0.001 cosz0
      chil0 = clamp (-0.4) 0.6 xl0
      chil  = if abs chil0 <= 0.01 then 0.01 else chil0
      phi1  = 0.5 - 0.633 * chil - 0.330 * chil * chil
      phi2  = 0.877 * (1.0 - 2.0 * phi1)
      gdir  = phi1 + phi2 * cosz
      twostext' = gdir / cosz
      avmu' = (1.0 - phi1 / phi2 * log ((phi1 + phi2) / phi1)) / phi2
      temp0' = max (gdir + phi2 * cosz) 1.0e-6
      temp1v = phi1 * cosz
      temp2' = 1.0 - temp1v / temp0' * log ((temp1v + temp0') / temp1v)
  in  TwoStreamParams chil gdir twostext' avmu' temp0' temp2'

-- ========================================================================
-- Two-stream: per-waveband computation
-- ========================================================================

-- | Compute two-stream fluxes for a single patch and waveband.
-- Ported from the waveband loop in TwoStream (SurfaceAlbedoMod.F90).
--
-- This is a pure function operating on a single patch and single waveband.
-- For nlevcan=1 (big-leaf), also computes PAR diagnostics when ib==0 (vis).
twoStreamWaveband
  :: TwoStreamParams
  -> TwoStreamInput
  -> TwoStreamResult
twoStreamWaveband tsp tsi =
  let cosz = max 0.001 (tsi_coszen tsi)
      elai' = tsi_elai tsi
      esai' = tsi_esai tsi
      ib = tsi_ib tsi
      sfOnly = tsi_sfOnly tsi
      snowveg = tsi_snowveg tsi
      t_veg' = tsi_t_veg tsi
      fwet' = tsi_fwet tsi
      fcansno' = tsi_fcansno tsi

      chil' = tsp_chil tsp
      gdir' = tsp_gdir tsp
      twostext' = tsp_twostext tsp
      avmu' = tsp_avmu tsp
      temp0' = tsp_temp0 tsp
      temp2' = tsp_temp2 tsp

      rhoIb = tsi_rho tsi
      tauIb = tsi_tau tsi

      -- Two-stream parameters: omega, betad, betai
      omegal = rhoIb + tauIb
      asu = 0.5 * omegal * gdir' / temp0' * temp2'
      betadl = (1.0 + avmu' * twostext') / (omegal * avmu' * twostext') * asu
      betail = 0.5 * ((rhoIb + tauIb) + (rhoIb - tauIb)
               * ((1.0 + chil') / 2.0) ^ (2 :: Int)) / omegal

      -- Adjust for intercepted snow
      omegaSn = omegas VU.! ib
      (tmp0, tmp1, tmp2)
        | sfOnly || (not snowveg && t_veg' > tfrz) =
            (omegal, betadl, betail)
        | snowveg =
            let o = (1.0 - fcansno') * omegal + fcansno' * omegaSn
                bd = ((1.0 - fcansno') * omegal * betadl + fcansno' * omegaSn * betads) / o
                bi = ((1.0 - fcansno') * omegal * betail + fcansno' * omegaSn * betais) / o
            in  (o, bd, bi)
        | otherwise =
            let o = (1.0 - fwet') * omegal + fwet' * omegaSn
                bd = ((1.0 - fwet') * omegal * betadl + fwet' * omegaSn * betads) / o
                bi = ((1.0 - fwet') * omegal * betail + fwet' * omegaSn * betais) / o
            in  (o, bd, bi)

      omegaW = tmp0
      betad  = tmp1
      betai' = tmp2

      -- Common terms
      b  = 1.0 - omegaW + omegaW * betai'
      c1 = omegaW * betai'
      tmp0v = avmu' * twostext'
      d  = tmp0v * omegaW * betad
      f  = tmp0v * omegaW * (1.0 - betad)
      tmp1v = b * b - c1 * c1
      h  = sqrt tmp1v / avmu'
      sigma = tmp0v * tmp0v - tmp1v

      p1 = b + avmu' * h
      p2 = b - avmu' * h
      p3 = b + tmp0v
      p4 = b - tmp0v

      -- Absorbed/reflected/transmitted fluxes for full canopy
      t1a = min (h * (elai' + esai')) 40.0
      s1  = exp (-t1a)
      t1b = min (twostext' * (elai' + esai')) 40.0
      s2  = exp (-t1b)

      -- ---- Direct beam ----
      (u1d, u2d, u3d)
        | not sfOnly = ( b - c1 / tsi_albgrd tsi
                       , b - c1 * tsi_albgrd tsi
                       , f + c1 * tsi_albgrd tsi )
        | otherwise  = ( b - c1 / tsi_albsod tsi
                       , b - c1 * tsi_albsod tsi
                       , f + c1 * tsi_albsod tsi )

      tmp2d = u1d - avmu' * h
      tmp3d = u1d + avmu' * h
      d1d   = p1 * tmp2d / s1 - p2 * tmp3d * s1
      tmp4d = u2d + avmu' * h
      tmp5d = u2d - avmu' * h
      d2d   = tmp4d / s1 - tmp5d * s1
      h1    = -d * p4 - c1 * f
      tmp6d = d - h1 * p3 / sigma
      tmp7d = (d - c1 - h1 / sigma * (u1d + tmp0v)) * s2
      h2    = (tmp6d * tmp2d / s1 - p2 * tmp7d) / d1d
      h3    = -(tmp6d * tmp3d * s1 - p1 * tmp7d) / d1d
      h4    = -f * p3 - c1 * d
      tmp8d = h4 / sigma
      tmp9d = (u3d - tmp8d * (u2d - tmp0v)) * s2
      h5    = -(tmp8d * tmp4d / s1 + tmp9d) / d2d
      h6    = (tmp8d * tmp5d * s1 + tmp9d) / d2d

      albdVal  = h1 / sigma + h2 + h3
      ftidVal  = h4 * s2 / sigma + h5 * s1 + h6 / s1
      ftddVal  = s2
      fabdVal
        | not sfOnly = 1.0 - albdVal
                       - (1.0 - tsi_albgrd tsi) * ftddVal
                       - (1.0 - tsi_albgri tsi) * ftidVal
        | otherwise  = 0.0

      -- Sunlit/shaded direct absorbed
      a1d = h1 / sigma * (1.0 - s2 * s2) / (2.0 * twostext')
          + h2 * (1.0 - s2 * s1) / (twostext' + h)
          + h3 * (1.0 - s2 / s1) / (twostext' - h)
      a2d = h4 / sigma * (1.0 - s2 * s2) / (2.0 * twostext')
          + h5 * (1.0 - s2 * s1) / (twostext' + h)
          + h6 * (1.0 - s2 / s1) / (twostext' - h)
      fabdSunVal
        | not sfOnly = (1.0 - omegaW) * (1.0 - s2 + 1.0 / avmu' * (a1d + a2d))
        | otherwise  = 0.0
      fabdShaVal
        | not sfOnly = fabdVal - fabdSunVal
        | otherwise  = 0.0

      -- ---- Diffuse ----
      (u1i, u2i)
        | not sfOnly = ( b - c1 / tsi_albgri tsi
                       , b - c1 * tsi_albgri tsi )
        | otherwise  = ( b - c1 / tsi_albsoi tsi
                       , b - c1 * tsi_albsoi tsi )

      tmp2i = u1i - avmu' * h
      tmp3i = u1i + avmu' * h
      d1i   = p1 * tmp2i / s1 - p2 * tmp3i * s1
      tmp4i = u2i + avmu' * h
      tmp5i = u2i - avmu' * h
      d2i   = tmp4i / s1 - tmp5i * s1
      h7    = (c1 * tmp2i) / (d1i * s1)
      h8    = (-c1 * tmp3i * s1) / d1i
      h9    = tmp4i / (d2i * s1)
      h10   = (-tmp5i * s1) / d2i

      albiVal
        | sfOnly    = h7 + h8
        | otherwise = h7 + h8
      ftiiVal
        | not sfOnly = h9 * s1 + h10 / s1
        | otherwise  = 0.0
      fabiVal
        | not sfOnly = 1.0 - albiVal - (1.0 - tsi_albgri tsi) * ftiiVal
        | otherwise  = 0.0

      -- Sunlit/shaded diffuse absorbed
      a1i = h7 * (1.0 - s2 * s1) / (twostext' + h)
          + h8 * (1.0 - s2 / s1) / (twostext' - h)
      a2i = h9 * (1.0 - s2 * s1) / (twostext' + h)
          + h10 * (1.0 - s2 / s1) / (twostext' - h)
      fabiSunVal
        | not sfOnly = (1.0 - omegaW) / avmu' * (a1i + a2i)
        | otherwise  = 0.0
      fabiShaVal
        | not sfOnly = fabiVal - fabiSunVal
        | otherwise  = 0.0

      -- ---- PAR diagnostics (ib==0, nlevcan==1) ----
      (fsunZ1, fabdSunZ1, fabiSunZ1, fabdShaZ1, fabiShaZ1, vcSun, vcSha)
        | not sfOnly && ib == 0 && nlevcan == 1 =
            let fsun = (1.0 - s2) / (max t1b mpeAlbedo)
                laisum = elai' + esai'
                fsunSafe = max fsun mpeAlbedo
                laisumSafe = max laisum mpeAlbedo
                fdSunZ = fabdSunVal / (fsunSafe * laisumSafe)
                fiSunZ = fabiSunVal / (fsunSafe * laisumSafe)
                fdShaZ = fabdShaVal / (max (1.0 - fsun) mpeAlbedo * laisumSafe)
                fiShaZ = fabiShaVal / (max (1.0 - fsun) mpeAlbedo * laisumSafe)
                -- Vcmax scaling
                extkn = 0.30
                extkb = twostext'
                vcSun' = (1.0 - exp (-(extkn + extkb) * elai')) / (extkn + extkb)
                vcSha' = (1.0 - exp (-extkn * elai')) / extkn - vcSun'
                (vcSunF, vcShaF)
                  | elai' > 0.0 = ( vcSun' / (fsunSafe * elai')
                                  , vcSha' / (max (1.0 - fsun) mpeAlbedo * elai') )
                  | otherwise   = (0.0, 0.0)
            in  (fsun, fdSunZ, fiSunZ, fdShaZ, fiShaZ, vcSunF, vcShaF)
        | otherwise = (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  in  TwoStreamResult
        { tsr_albd = if sfOnly then 1.0 else albdVal
        , tsr_albi = if sfOnly then 1.0 else albiVal
        , tsr_fabd = fabdVal
        , tsr_fabi = fabiVal
        , tsr_ftdd = if sfOnly then 0.0 else ftddVal
        , tsr_ftid = if sfOnly then 0.0 else ftidVal
        , tsr_ftii = if sfOnly then 0.0 else ftiiVal
        , tsr_fabd_sun = fabdSunVal
        , tsr_fabd_sha = fabdShaVal
        , tsr_fabi_sun = fabiSunVal
        , tsr_fabi_sha = fabiShaVal
        , tsr_albdSF = if sfOnly then albdVal else 1.0
        , tsr_albiSF = if sfOnly then albiVal else 1.0
        , tsr_fsun_z1 = fsunZ1
        , tsr_fabd_sun_z1 = fabdSunZ1
        , tsr_fabi_sun_z1 = fabiSunZ1
        , tsr_fabd_sha_z1 = fabdShaZ1
        , tsr_fabi_sha_z1 = fabiShaZ1
        , tsr_vcmaxcintsun = vcSun
        , tsr_vcmaxcintsha = vcSha
        }

-- ========================================================================
-- Snow albedo fallback (age-based, no SNICAR)
-- ========================================================================

-- | Fallback age-based snow albedo when SNICAR optics are not loaded.
-- Returns (albsnd, albsni) vectors of length numrad.
snowAlbedoFallback
  :: Double  -- ^ cos(solar zenith)
  -> Double  -- ^ frac_sno
  -> Double  -- ^ snow_persistence [seconds]
  -> (VU.Vector Double, VU.Vector Double)
snowAlbedoFallback cosz fracSno snowPersist
  | cosz <= 0.0 || fracSno <= 0.0 =
      (VU.replicate numrad 0.0, VU.replicate numrad 0.0)
  | otherwise =
      let ageDays = snowPersist / 86400.0
          fage = 1.0 - exp (-ageDays / 5.0)
          visD = clamp 0.1 0.99 (0.95 * (1.0 - 0.15 * fage))
          nirD = clamp 0.1 0.99 (0.65 * (1.0 - 0.50 * fage))
          visI = visD
          nirI = nirD
      in  (VU.fromList [visD, nirD], VU.fromList [visI, nirI])

-- ========================================================================
-- Ground albedo (snow-fraction weighting)
-- ========================================================================

-- | Compute ground albedo as snow-fraction weighted average of soil and snow.
-- Ported from the ground albedo section of SurfaceAlbedo.
groundAlbedo
  :: Double              -- ^ cos(solar zenith) for this column
  -> Double              -- ^ frac_sno
  -> VU.Vector Double    -- ^ albsod (direct soil albedo, per waveband)
  -> VU.Vector Double    -- ^ albsoi (diffuse soil albedo, per waveband)
  -> VU.Vector Double    -- ^ albsnd (direct snow albedo, per waveband)
  -> VU.Vector Double    -- ^ albsni (diffuse snow albedo, per waveband)
  -> GroundAlbedoResult
groundAlbedo cosz fracSno albsod' albsoi' albsnd' albsni'
  | cosz <= 0.0 =
      GroundAlbedoResult (VU.replicate numrad 0.0) (VU.replicate numrad 0.0)
  | otherwise =
      let grd = VU.generate numrad $ \ib ->
                  (albsod' VU.! ib) * (1.0 - fracSno) + (albsnd' VU.! ib) * fracSno
          gri = VU.generate numrad $ \ib ->
                  (albsoi' VU.! ib) * (1.0 - fracSno) + (albsni' VU.! ib) * fracSno
      in  GroundAlbedoResult grd gri

-- ========================================================================
-- Canopy layering
-- ========================================================================

-- | Discretize canopy into radiative layers.
-- Ported from the canopy layering section of SurfaceAlbedo.
-- For nlevcan=1 (big-leaf), returns single layer with full elai/esai.
canopyLayering
  :: Double   -- ^ elai (exposed LAI)
  -> Double   -- ^ esai (exposed SAI)
  -> Double   -- ^ tlai (total LAI, including buried)
  -> Double   -- ^ tsai (total SAI, including buried)
  -> CanopyLayerResult
canopyLayering elai' esai' tlai' tsai'
  | nlevcan == 1 =
      CanopyLayerResult
        { clr_nrad = 1
        , clr_ncan = 1
        , clr_tlai_z = VU.singleton elai'
        , clr_tsai_z = VU.singleton esai'
        }
  | otherwise =
      -- Multi-layer canopy (nlevcan > 1)
      let dincmax = 0.25
          mpe = mpeAlbedo
          total = elai' + esai'
          -- Exposed layers
          (nrad', exposed) = buildLayers total elai' esai' dincmax mpe nlevcan
          -- Enforce minimum 4 layers
          (nradF, exposedF)
            | nrad' < 4 && total > 0.0 =
                let n = 4
                    lz = VU.replicate n (elai' / fromIntegral n)
                    sz = VU.replicate n (esai' / fromIntegral n)
                in  (n, zip (VU.toList lz) (VU.toList sz))
            | otherwise = (nrad', exposed)
          -- Buried layers
          blai = tlai' - elai'
          bsai = tsai' - esai'
          btotal = blai + bsai
          (ncan', buried)
            | btotal == 0.0 = (nradF, [])
            | otherwise =
                let (nb, bls) = buildLayers btotal blai bsai dincmax mpe (nlevcan - nradF)
                in  (nradF + nb, bls)
          allLai = map fst exposedF ++ map fst buried
          allSai = map snd exposedF ++ map snd buried
      in  CanopyLayerResult
            { clr_nrad = nradF
            , clr_ncan = ncan'
            , clr_tlai_z = VU.fromList allLai
            , clr_tsai_z = VU.fromList allSai
            }

-- Helper: build canopy layers using dincmax increment
buildLayers :: Double -> Double -> Double -> Double -> Double -> Int
            -> (Int, [(Double, Double)])
buildLayers total lai sai dincmax mpe maxLayers = go 0 0.0 []
  where
    go n dsum acc
      | n >= maxLayers = (n, reverse acc)
      | otherwise =
          let dsum' = dsum + dincmax
              safeTot = max total mpe
          in  if (total - dsum') > 1.0e-6
              then let tl = dincmax * lai / safeTot
                       ts = dincmax * sai / safeTot
                   in  go (n + 1) dsum' ((tl, ts) : acc)
              else let dinc = dincmax - (dsum' - total)
                       tl = dinc * lai / safeTot
                       ts = dinc * sai / safeTot
                   in  (n + 1, reverse ((tl, ts) : acc))

-- ========================================================================
-- Weight reflectance/transmittance by LAI and SAI fractions
-- ========================================================================

-- | Compute LAI/SAI weighted leaf/stem reflectance and transmittance.
-- Returns (rho, tau) for a single waveband.
weightReflTrans
  :: Double  -- ^ elai
  -> Double  -- ^ esai
  -> Double  -- ^ pftcon_rhol for this PFT and waveband
  -> Double  -- ^ pftcon_rhos for this PFT and waveband
  -> Double  -- ^ pftcon_taul for this PFT and waveband
  -> Double  -- ^ pftcon_taus for this PFT and waveband
  -> (Double, Double)
weightReflTrans elai' esai' rhol rhos taul taus =
  let mpe = mpeAlbedo
      safeTot = max (elai' + esai') mpe
      wl = elai' / safeTot
      ws = esai' / safeTot
      rho' = max (rhol * wl + rhos * ws) mpe
      tau' = max (taul * wl + taus * ws) mpe
  in  (rho', tau')

-- ========================================================================
-- Utility
-- ========================================================================

clamp :: Double -> Double -> Double -> Double
clamp lo hi x = max lo (min hi x)
{-# INLINE clamp #-}

-- ========================================================================
-- Top-level driver: single column + single patch
-- ========================================================================

-- | Input for the surface albedo driver (one column, one patch).
data SurfAlbDriverInput = SurfAlbDriverInput
  { sadi_coszen      :: !Double           -- ^ Cosine solar zenith angle
  , sadi_soilAlbIn   :: !SoilAlbedoInput  -- ^ Column-level soil albedo inputs
  , sadi_fracSno     :: !Double           -- ^ Snow cover fraction
  , sadi_snowPersist :: !Double           -- ^ Snow persistence [seconds]
  -- Patch-level vegetation state
  , sadi_elai        :: !Double           -- ^ Exposed LAI
  , sadi_esai        :: !Double           -- ^ Exposed SAI
  , sadi_tlai        :: !Double           -- ^ Total LAI
  , sadi_tsai        :: !Double           -- ^ Total SAI
  , sadi_t_veg       :: !Double           -- ^ Vegetation temperature [K]
  , sadi_fwet        :: !Double           -- ^ Wet fraction of canopy
  , sadi_fcansno     :: !Double           -- ^ Canopy snow fraction
  -- PFT optical properties (length numrad each)
  , sadi_rhol        :: !(VU.Vector Double) -- ^ Leaf reflectance per waveband
  , sadi_rhos        :: !(VU.Vector Double) -- ^ Stem reflectance per waveband
  , sadi_taul        :: !(VU.Vector Double) -- ^ Leaf transmittance per waveband
  , sadi_taus        :: !(VU.Vector Double) -- ^ Stem transmittance per waveband
  , sadi_xl          :: !Double             -- ^ Leaf angle departure from spherical
  } deriving (Show)

-- | Output of the surface albedo driver (one column, one patch).
data SurfAlbDriverOutput = SurfAlbDriverOutput
  { sado_soilAlb   :: !SoilAlbedoResult         -- ^ Soil albedo (direct, diffuse)
  , sado_snowAlbD  :: !(VU.Vector Double)       -- ^ Snow direct albedo per waveband
  , sado_snowAlbI  :: !(VU.Vector Double)       -- ^ Snow diffuse albedo per waveband
  , sado_groundAlb :: !GroundAlbedoResult       -- ^ Ground albedo (snow-weighted)
  , sado_canopy    :: !CanopyLayerResult        -- ^ Canopy layer structure
  -- Patch-level results per waveband
  , sado_albd      :: !(VU.Vector Double)       -- ^ Direct beam albedo
  , sado_albi      :: !(VU.Vector Double)       -- ^ Diffuse albedo
  , sado_fabd      :: !(VU.Vector Double)       -- ^ Direct absorbed fraction
  , sado_fabi      :: !(VU.Vector Double)       -- ^ Diffuse absorbed fraction
  , sado_ftdd      :: !(VU.Vector Double)       -- ^ Direct-through fraction
  , sado_ftid      :: !(VU.Vector Double)       -- ^ Diffuse-from-direct through
  , sado_ftii      :: !(VU.Vector Double)       -- ^ Diffuse-through fraction
  -- Sun/shade (from vis band two-stream)
  , sado_fsun      :: !Double                   -- ^ Sunlit fraction
  , sado_fabd_sun_z :: !Double                  -- ^ Sunlit direct absorbed per lai (VIS, layer 1)
  , sado_fabi_sun_z :: !Double                  -- ^ Sunlit diffuse absorbed per lai (VIS, layer 1)
  , sado_fabd_sha_z :: !Double                  -- ^ Shaded direct absorbed per lai (VIS, layer 1)
  , sado_fabi_sha_z :: !Double                  -- ^ Shaded diffuse absorbed per lai (VIS, layer 1)
  , sado_vcmaxcintsun :: !Double                -- ^ Vcmax scaling, sunlit
  , sado_vcmaxcintsha :: !Double                -- ^ Vcmax scaling, shaded
  } deriving (Show)

-- | Full surface albedo computation for one column + one patch.
-- Orchestrates: soil albedo → snow albedo → ground albedo →
-- canopy layering → two-stream radiative transfer.
--
-- For bare ground or nighttime patches, bypasses two-stream and
-- sets patch albedo equal to ground albedo with full transmission.
surfaceAlbedoDriver
  :: SurfaceAlbedoConstants
  -> SurfAlbDriverInput
  -> SurfAlbDriverOutput
surfaceAlbedoDriver con inp =
  let coszen = sadi_coszen inp

      -- Step 1: Soil albedo
      soilRes = soilAlbedo con (sadi_soilAlbIn inp)

      -- Step 2: Snow albedo (age-based fallback, no SNICAR optics)
      (albsnd, albsni) = snowAlbedoFallback coszen (sadi_fracSno inp) (sadi_snowPersist inp)

      -- Step 3: Ground albedo (snow-fraction weighted)
      groundRes = groundAlbedo coszen (sadi_fracSno inp)
                    (sar_albsod soilRes) (sar_albsoi soilRes)
                    albsnd albsni

      -- Step 4: Check if vegetated and daytime
      elai' = sadi_elai inp
      esai' = sadi_esai inp
      hasVeg = (elai' + esai') > 0.0 && coszen > 0.0

  in if not hasVeg
     then -- Bare ground / nighttime: patch albedo = ground albedo
       SurfAlbDriverOutput
         { sado_soilAlb   = soilRes
         , sado_snowAlbD  = albsnd
         , sado_snowAlbI  = albsni
         , sado_groundAlb = groundRes
         , sado_canopy    = CanopyLayerResult 0 0 VU.empty VU.empty
         , sado_albd      = gar_albgrd groundRes
         , sado_albi      = gar_albgri groundRes
         , sado_fabd      = VU.replicate numrad 0.0
         , sado_fabi      = VU.replicate numrad 0.0
         , sado_ftdd      = VU.replicate numrad 1.0
         , sado_ftid      = VU.replicate numrad 0.0
         , sado_ftii      = VU.replicate numrad 1.0
         , sado_fsun      = 0.0
         , sado_fabd_sun_z = 0.0
         , sado_fabi_sun_z = 0.0
         , sado_fabd_sha_z = 0.0
         , sado_fabi_sha_z = 0.0
         , sado_vcmaxcintsun = 0.0
         , sado_vcmaxcintsha = 0.0
         }
     else -- Vegetated: run two-stream
       let -- Step 5: Canopy layering
           canopy = canopyLayering elai' esai' (sadi_tlai inp) (sadi_tsai inp)

           -- Step 6: Two-stream parameters (waveband-independent)
           tsp = twoStreamParams coszen (sadi_xl inp)

           -- Step 7: Two-stream per waveband
           tsResults = map (\ib ->
             let (rho, tau) = weightReflTrans elai' esai'
                               (sadi_rhol inp VU.! ib) (sadi_rhos inp VU.! ib)
                               (sadi_taul inp VU.! ib) (sadi_taus inp VU.! ib)
                 tsInp = TwoStreamInput
                   { tsi_coszen  = coszen
                   , tsi_elai    = elai'
                   , tsi_esai    = esai'
                   , tsi_rho     = rho
                   , tsi_tau     = tau
                   , tsi_xl      = sadi_xl inp
                   , tsi_t_veg   = sadi_t_veg inp
                   , tsi_fwet    = sadi_fwet inp
                   , tsi_fcansno = sadi_fcansno inp
                   , tsi_albgrd  = gar_albgrd groundRes VU.! ib
                   , tsi_albgri  = gar_albgri groundRes VU.! ib
                   , tsi_albsod  = sar_albsod soilRes VU.! ib
                   , tsi_albsoi  = sar_albsoi soilRes VU.! ib
                   , tsi_ib      = ib
                   , tsi_sfOnly  = False
                   , tsi_snowveg = snowveg_affects_radiation con
                   }
             in twoStreamWaveband tsp tsInp
             ) [0 .. numrad - 1]

           -- Extract per-waveband vectors
           albd = VU.fromList $ map tsr_albd tsResults
           albi = VU.fromList $ map tsr_albi tsResults
           fabd = VU.fromList $ map tsr_fabd tsResults
           fabi = VU.fromList $ map tsr_fabi tsResults
           ftdd = VU.fromList $ map tsr_ftdd tsResults
           ftid = VU.fromList $ map tsr_ftid tsResults
           ftii = VU.fromList $ map tsr_ftii tsResults

           -- Sun/shade from VIS band result
           visRes = head tsResults
           fsun = tsr_fsun_z1 visRes

       in SurfAlbDriverOutput
            { sado_soilAlb   = soilRes
            , sado_snowAlbD  = albsnd
            , sado_snowAlbI  = albsni
            , sado_groundAlb = groundRes
            , sado_canopy    = canopy
            , sado_albd      = albd
            , sado_albi      = albi
            , sado_fabd      = fabd
            , sado_fabi      = fabi
            , sado_ftdd      = ftdd
            , sado_ftid      = ftid
            , sado_ftii      = ftii
            , sado_fsun      = fsun
            , sado_fabd_sun_z = tsr_fabd_sun_z1 visRes
            , sado_fabi_sun_z = tsr_fabi_sun_z1 visRes
            , sado_fabd_sha_z = tsr_fabd_sha_z1 visRes
            , sado_fabi_sha_z = tsr_fabi_sha_z1 visRes
            , sado_vcmaxcintsun = tsr_vcmaxcintsun visRes
            , sado_vcmaxcintsha = tsr_vcmaxcintsha visRes
            }

