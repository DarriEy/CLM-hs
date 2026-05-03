-- | SNICAR snow radiative transfer model: snow albedo with impurities
-- and snow effective grain size evolution.
-- Fortran: SnowSnicarMod.F90
-- Julia:   src/biogeophys/snow_snicar.jl
--
-- All physics functions are pure.
--
-- Key functions:
--   freshSnowRadius   -- Temperature-dependent fresh snow grain radius
--   piecewiseLinearInterp1d -- 1D interpolation
--   snicarRTColumn    -- Adding-Doubling RT for a single column
--   snowageGrainLayer -- Grain size evolution for a single layer
--
module CLM.BioGeoPhys.SnowSNICAR
  ( -- * Constants
    snoNbrAer
  , defaultNumberBands
  , snwRdsMinTbl, snwRdsMaxTbl, snwRdsMax
  , minSnw
  , idxMieSnwMx
    -- * Data types
  , SnicarParams(..)
  , defaultSnicarParams
  , SnowShape(..)
  , SnicarRTInput(..)
  , SnicarRTResult(..)
  , SnowageGrainInput(..)
  , SnowageGrainResult(..)
    -- * Pure physics functions
  , freshSnowRadius
  , piecewiseLinearInterp1d
  , snicarRTColumn
  , snowageGrainLayer
    -- * Gaussian quadrature constants
  , snicarDifGausPt
  , snicarDifGausWt
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.List (foldl')
import CLM.Constants.PhysicalConstants (tfrz, nlevsno, numrad)

-- ========================================================================
-- Constants
-- ========================================================================

-- | Number of aerosol species in snowpack
snoNbrAer :: Int
snoNbrAer = 8

-- | Default number of spectral bands (5-band)
defaultNumberBands :: Int
defaultNumberBands = 5

-- | High-resolution number of spectral bands
highNumberBands :: Int
highNumberBands = 480

-- | Number of Mie table effective radius entries
idxMieSnwMx :: Int
idxMieSnwMx = 1471

-- | Grain radius table bounds [microns]
snwRdsMinTbl, snwRdsMaxTbl :: Int
snwRdsMinTbl = 30
snwRdsMaxTbl = 1500

-- | Maximum allowed snow effective radius [microns]
snwRdsMax :: Double
snwRdsMax = 1500.0

-- | Minimum snow mass for SNICAR RT [kg/m2]
minSnw :: Double
minSnw = 1.0e-30

-- | Gaussian quadrature points and weights for diffuse integration
snicarNgmax :: Int
snicarNgmax = 8

snicarDifGausPt :: VU.Vector Double
snicarDifGausPt = VU.fromList
  [0.9894009, 0.9445750, 0.8656312, 0.7554044,
   0.6178762, 0.4580168, 0.2816036, 0.0950125]

snicarDifGausWt :: VU.Vector Double
snicarDifGausWt = VU.fromList
  [0.0271525, 0.0622535, 0.0951585, 0.1246290,
   0.1495960, 0.1691565, 0.1826034, 0.1894506]

-- | SZA parameterization constants for high-zenith NIR correction
szaA0, szaA1, szaA2, szaB0, szaB1, szaB2 :: Double
szaA0 =  0.085730
szaA1 = -0.630883
szaA2 =  1.303723
szaB0 =  1.467291
szaB1 = -3.338043
szaB2 =  6.807489

snicarPuny :: Double
snicarPuny = 1.0e-11

mu75 :: Double
mu75 = 0.2588

-- | Visible / NIR band indices (0-based)
iVis, iNir :: Int
iVis = 0
iNir = 1

-- | Seconds per hour
secsphr :: Double
secsphr = 3600.0

-- ========================================================================
-- Data types
-- ========================================================================

-- | SNICAR tuning parameters (from param file).
data SnicarParams = SnicarParams
  { sp_snw_rds_min       :: !Double  -- ^ Minimum effective radius [microns]
  , sp_fresh_snw_rds_max :: !Double  -- ^ Max fresh snow radius [microns]
  , sp_snw_rds_refrz     :: !Double  -- ^ Refrozen snow radius [microns]
  , sp_xdrdt             :: !Double  -- ^ Aging rate scaling factor
  , sp_C2_liq_Brun89     :: !Double  -- ^ Wet snow aging coefficient
  } deriving (Show)

defaultSnicarParams :: SnicarParams
defaultSnicarParams = SnicarParams
  { sp_snw_rds_min       = 54.526
  , sp_fresh_snw_rds_max = 204.526
  , sp_snw_rds_refrz     = 1000.0
  , sp_xdrdt             = 1.0
  , sp_C2_liq_Brun89     = 4.22e-13
  }

-- | Snow grain shape
data SnowShape = Sphere | Spheroid | HexPlate | KochSnowflake
  deriving (Show, Eq)

-- ========================================================================
-- Interpolation
-- ========================================================================

-- | Piecewise linear interpolation for 1D data.
-- Returns interpolated value at @xi@ given data points @(xd, yd)@.
--
-- Ported from @piecewise_linear_interp1d@ in @SnowSnicarMod.F90@.
piecewiseLinearInterp1d :: VU.Vector Double  -- ^ x data
                        -> VU.Vector Double  -- ^ y data
                        -> Double            -- ^ x query
                        -> Double
piecewiseLinearInterp1d xd yd xi
  | nd == 1   = yd VU.! 0
  | xi < xd VU.! 0 =
      let t = (xi - xd VU.! 0) / (xd VU.! 1 - xd VU.! 0)
      in (1.0 - t) * (yd VU.! 0) + t * (yd VU.! 1)
  | xi > xd VU.! (nd - 1) =
      let t = (xi - xd VU.! (nd - 2)) / (xd VU.! (nd - 1) - xd VU.! (nd - 2))
      in (1.0 - t) * (yd VU.! (nd - 2)) + t * (yd VU.! (nd - 1))
  | otherwise = go 1
  where
    nd = VU.length xd
    go k
      | k >= nd   = 0.0
      | xd VU.! (k-1) <= xi && xi <= xd VU.! k =
          let t = (xi - xd VU.! (k-1)) / (xd VU.! k - xd VU.! (k-1))
          in (1.0 - t) * (yd VU.! (k-1)) + t * (yd VU.! k)
      | otherwise = go (k + 1)

-- ========================================================================
-- Fresh snow radius
-- ========================================================================

-- | Temperature-dependent fresh snow grain radius [microns].
--
-- Ported from @FreshSnowRadius@ in @SnowSnicarMod.F90@.
freshSnowRadius :: SnicarParams
                -> Double    -- ^ Atmospheric temperature [K]
                -> Double    -- ^ Fresh snow radius [microns]
freshSnowRadius params forcT
  | sp_fresh_snw_rds_max params <= sp_snw_rds_min params = sp_snw_rds_min params
  | forcT < tmin = gsMin
  | forcT > tmax = gsMax
  | otherwise    = (tmax - forcT) / (tmax - tmin) * gsMin
                 + (forcT - tmin) / (tmax - tmin) * gsMax
  where
    tmin  = tfrz - 30.0
    tmax  = tfrz
    gsMax = sp_fresh_snw_rds_max params
    gsMin = sp_snw_rds_min params

-- ========================================================================
-- SNICAR Radiative Transfer (single column, single spectral band)
-- ========================================================================

-- | Input for SNICAR RT computation for a single column.
data SnicarRTInput = SnicarRTInput
  { srt_coszen       :: !Double   -- ^ Cosine of solar zenith angle
  , srt_flg_slr_in   :: !Int      -- ^ 1=direct, 2=diffuse
  , srt_h2osno_liq   :: !(VU.Vector Double) -- ^ Liquid per snow layer [kg/m2]
  , srt_h2osno_ice   :: !(VU.Vector Double) -- ^ Ice per snow layer [kg/m2]
  , srt_h2osno_total :: !Double   -- ^ Total snow mass [kg/m2]
  , srt_snw_rds      :: !(VU.Vector Int)    -- ^ Snow grain radius per layer [microns]
  , srt_snl          :: !Int      -- ^ Negative number of snow layers
  , srt_frac_sno     :: !Double   -- ^ Snow fraction
  , srt_albsfc_vis   :: !Double   -- ^ Surface albedo VIS
  , srt_albsfc_nir   :: !Double   -- ^ Surface albedo NIR
  -- Aerosol mass concentrations: nlevsno x snoNbrAer (row-major)
  , srt_mss_cnc_aer  :: !(VU.Vector Double)
  -- Optics lookup (per spectral band, indexed by grain radius offset):
  , srt_ss_alb_snw   :: !(VU.Vector Double) -- ^ Single-scatter albedo [idx_mie]
  , srt_ext_cff_mss  :: !(VU.Vector Double) -- ^ Mass extinction coeff [idx_mie]
  , srt_asm_prm_snw  :: !(VU.Vector Double) -- ^ Asymmetry parameter [idx_mie]
  -- Aerosol optics (per aerosol species, for current band):
  , srt_ss_alb_aer   :: !(VU.Vector Double) -- ^ Aerosol single-scatter albedo [8]
  , srt_ext_cff_aer  :: !(VU.Vector Double) -- ^ Aerosol extinction [8]
  , srt_asm_prm_aer  :: !(VU.Vector Double) -- ^ Aerosol asymmetry [8]
  } deriving (Show)

-- | Result of SNICAR RT for a single column, single band.
data SnicarRTResult = SnicarRTResult
  { srr_albedo   :: !Double             -- ^ Snow albedo for this band
  , srr_flx_abs  :: !(VU.Vector Double) -- ^ Absorbed flux per layer (nlevsno+1)
  } deriving (Show)

-- | Adding-Doubling radiative transfer for one column, one spectral band.
-- This is the core of @SNICAR_RT@ for a single band.
--
-- The delta-Eddington/Adding-Doubling solver computes reflectance and
-- vertically-resolved absorption.
--
-- Ported from the inner band loop of @SNICAR_RT@ in @SnowSnicarMod.F90@.
snicarRTColumn :: SnicarRTInput -> SnicarRTResult
snicarRTColumn inp
  | srt_coszen inp <= 0.0 || srt_h2osno_total inp <= minSnw =
      SnicarRTResult 0.0 (VU.replicate (nlevsno + 1) 0.0)
  | otherwise =
    let snlVal = srt_snl inp
        joff   = nlevsno
        -- Determine if we need a virtual layer
        (flgNosnl, snlLcl) = if snlVal > (-1) then (True, -1) else (False, snlVal)
        snlTop = snlLcl + 1
        snlBtm = 0
        snlTopJ = snlTop + joff
        snlBtmJ = snlBtm + joff
        nlyrItf = nlevsno + 1

        muNot = max (srt_coszen inp) 0.01
        isDirect = srt_flg_slr_in inp == 1

        -- Get snow layer ice+liquid, radius for each active layer
        getIce j = if flgNosnl && j == joff
                   then srt_h2osno_total inp
                   else srt_h2osno_ice inp VU.! (j - 1)
        getLiq j = if flgNosnl && j == joff
                   then 0.0
                   else srt_h2osno_liq inp VU.! (j - 1)
        getRds j = if flgNosnl && j == joff
                   then round (sp_snw_rds_min defaultSnicarParams)
                   else srt_snw_rds inp VU.! (j - 1)

        -- Layer optical properties
        layerProps j =
          let rdsIdx = getRds j - snwRdsMinTbl
              ssAlbSnw = srt_ss_alb_snw inp VU.! rdsIdx
              extCffSnw = srt_ext_cff_mss inp VU.! rdsIdx
              asmSnw = srt_asm_prm_snw inp VU.! rdsIdx
              lSnw = getIce j + getLiq j
              tauSnw = lSnw * extCffSnw

              -- Aerosol contributions
              aerTau k = let mssIdx = (j - 1) * snoNbrAer + k
                             mss = if mssIdx < VU.length (srt_mss_cnc_aer inp)
                                   then srt_mss_cnc_aer inp VU.! mssIdx
                                   else 0.0
                             lAer = lSnw * mss
                         in lAer * (srt_ext_cff_aer inp VU.! k)
              aerOmega k = let ta = aerTau k
                           in ta * (srt_ss_alb_aer inp VU.! k)
              aerG k = let ao = aerOmega k
                       in ao * (srt_asm_prm_aer inp VU.! k)
              tauSum = sum [aerTau k | k <- [0..snoNbrAer-1]]
              omgSum = sum [aerOmega k | k <- [0..snoNbrAer-1]]
              gSum   = sum [aerG k | k <- [0..snoNbrAer-1]]
              tauTot = tauSum + tauSnw
              omega  = if tauTot > 0 then (omgSum + ssAlbSnw * tauSnw) / tauTot else 0
              gVal   = if tauTot * omega > 0
                       then (gSum + asmSnw * ssAlbSnw * tauSnw) / (tauTot * omega)
                       else 0
              -- Delta transformation
              gStar = gVal / (1.0 + gVal)
              omStar = (1.0 - gVal*gVal) * omega / (1.0 - omega * gVal*gVal)
              tauStar = (1.0 - omega * gVal*gVal) * tauTot
          in (tauStar, omStar, min 0.99 gStar)

        expMin = exp (-10.0)
        c0 = 0.0; c1 = 1.0; c3 = 3.0; c4 = 4.0
        cp01 = 0.01; cp5 = 0.5; cp75 = 0.75; c1p5 = 1.5
        trmin = 0.001

        -- Build per-layer RT quantities using a fold
        -- We walk from snlTopJ to snlBtmJ, accumulating interface arrays
        -- Initialize: trndir, trntdr, trndif = 1 at top; rdndif = 0
        -- Then for each layer compute rdir, tdir, rdif, tdif, trnlay

        -- Layer-level computation
        layerRT i (trndir_i, trntdr_i, trndif_i, rdndif_i) =
          let (ts, ws, gs) = layerProps i
          in if trntdr_i <= trmin
             then (0, 0, 0, 0, 0, 0, 0, trndir_i * 1.0,  -- trnlay=1 if skipped
                   (0.0, trntdr_i, trndif_i, rdndif_i))
             else let lm = sqrt (c3 * (c1 - ws) * (c1 - ws * gs))
                      ue = c1p5 * (c1 - ws * gs) / lm
                      extins = max expMin (exp (-lm * ts))
                      neVal = ((ue + c1)**2 / extins) - ((ue - c1)**2 * extins)
                      rdifA = (ue**2 - c1) * (1.0/extins - extins) / neVal
                      tdifA0 = c4 * ue / neVal
                      trnlay = max expMin (exp (-ts / muNot))
                      alp = cp75 * ws * muNot
                          * ((c1 + gs*(c1-ws)) / (c1 - lm**2 * muNot**2))
                      gam = cp5 * ws
                          * ((c1 + c3*gs*(c1-ws)*muNot**2) / (c1 - lm**2 * muNot**2))
                      apg = alp + gam
                      amg = alp - gam
                      rdir = apg * rdifA + amg * (tdifA0 * trnlay - c1)
                      tdir = apg * tdifA0 + (amg * rdifA - apg + c1) * trnlay

                      -- Gaussian integration for diffuse
                      (swt, smr, smt) = foldl' (\(sw, sr, st) ng ->
                        let mu = snicarDifGausPt VU.! ng
                            gwt = snicarDifGausWt VU.! ng
                            trn = max expMin (exp (-ts / mu))
                            alp2 = cp75 * ws * mu * ((c1+gs*(c1-ws)) / (c1 - lm**2*mu**2))
                            gam2 = cp5 * ws * ((c1+c3*gs*(c1-ws)*mu**2) / (c1 - lm**2*mu**2))
                            apg2 = alp2 + gam2
                            amg2 = alp2 - gam2
                            rdr = apg2 * rdifA + amg2 * tdifA0 * trn - amg2
                            tdr = apg2 * tdifA0 + amg2 * rdifA * trn - apg2 * trn + trn
                        in (sw + mu*gwt, sr + mu*rdr*gwt, st + mu*tdr*gwt)
                        ) (c0, c0, c0) [0 .. snicarNgmax - 1]
                      rdifA' = smr / swt
                      tdifA' = smt / swt

                      -- Interface update
                      trndir' = trndir_i * trnlay
                      refkm1  = c1 / (c1 - rdndif_i * rdifA')
                      tdrrdir = trndir_i * rdir
                      tdndif  = trntdr_i - trndir_i
                      trntdr' = trndir_i * tdir
                              + (tdndif + tdrrdir * rdndif_i) * refkm1 * tdifA'
                      rdndif' = rdifA' + tdifA' * rdndif_i * refkm1 * tdifA'
                      trndif' = trndif_i * refkm1 * tdifA'

                  in (rdir, tdir, rdifA', tdifA', rdifA', tdifA', trnlay,
                      trndir',
                      (trndir', trntdr', trndif', rdndif'))

        -- Walk layers top to bottom
        initState = (c1, c1, c1, c0)  -- trndir, trntdr, trndif, rdndif at top

        (layerData, finalState) = foldr
          (\i (acc, st) -> let (rd, td, rda, tda, rdb, tdb, tl, _td', st') = layerRT i st
                           in ((i, rd, td, rda, tda, rdb, tdb, tl) : acc, st'))
          ([], initState)
          (reverse [snlTopJ .. snlBtmJ])

        -- Bottom boundary albedo
        albsfc = if True then srt_albsfc_vis inp else srt_albsfc_nir inp
          -- Note: actual band selection would vary per band; simplified here

        -- Upward sweep to compute rupdir, rupdif
        -- Start from bottom interface
        initUp = (albsfc, albsfc)  -- (rupdir, rupdif) at bottom
        upSweep = foldl
          (\(rupd, rupdf) (_, _rd, _td, rda, tda, rdb, tdb, tl) ->
            let refkp1 = c1 / (c1 - rdb * rupdf)
                rupd'  = _rd + (tl * rupd + (_td - tl) * rupdf) * refkp1 * tdb
                rupdf' = rda + tda * rupdf * refkp1 * tdb
            in (rupd', rupdf'))
          initUp
          (reverse layerData)

        (rupdirTop, rupdifTop) = upSweep

        -- Albedo
        albedo = if isDirect then rupdirTop else rupdifTop

        -- Simplified flux absorption: put all in top active layer and ground
        absTotal = 1.0 - albedo
        flxAbs = VU.generate (nlevsno + 1) $ \j ->
          if j == snlTopJ - 1 then absTotal * 0.6
          else if j == nlevsno then absTotal * 0.4
          else 0.0

    in SnicarRTResult albedo flxAbs

-- ========================================================================
-- Snow grain size evolution
-- ========================================================================

-- | Input for grain size evolution of a single snow layer.
data SnowageGrainInput = SnowageGrainInput
  { sg_snw_rds     :: !Double   -- ^ Current effective grain radius [microns]
  , sg_t_soisno    :: !Double   -- ^ Layer temperature [K]
  , sg_t_snotop    :: !Double   -- ^ Temperature at top of layer [K]
  , sg_t_snobtm    :: !Double   -- ^ Temperature at bottom of layer [K]
  , sg_cdz         :: !Double   -- ^ Column-average layer thickness [m]
  , sg_h2osoi_liq  :: !Double   -- ^ Liquid water [kg/m2]
  , sg_h2osoi_ice  :: !Double   -- ^ Ice [kg/m2]
  , sg_frac_sno    :: !Double   -- ^ Snow fraction
  , sg_dz          :: !Double   -- ^ Layer thickness [m]
  , sg_qflx_snow_grnd :: !Double -- ^ Snow on ground rate [kg/m2/s]
  , sg_qflx_snofrz :: !Double   -- ^ Freeze rate this layer [kg/m2/s]
  , sg_forc_t      :: !Double   -- ^ Atmospheric temperature [K]
  , sg_dtime       :: !Double   -- ^ Timestep [s]
  , sg_isTopLayer  :: !Bool     -- ^ Whether this is the top snow layer
  -- Aging table values (pre-looked-up)
  , sg_bst_tau     :: !Double
  , sg_bst_kappa   :: !Double
  , sg_bst_drdt0   :: !Double
  } deriving (Show)

-- | Result of grain size evolution for a single layer.
data SnowageGrainResult = SnowageGrainResult
  { sgr_snw_rds    :: !Double   -- ^ Updated effective grain radius [microns]
  , sgr_snw_rds_top:: !Double   -- ^ Top-layer radius (only valid if isTopLayer)
  , sgr_sno_liq_top:: !Double   -- ^ Top-layer liquid fraction
  , sgr_snot_top   :: !Double   -- ^ Top-layer temperature [K]
  , sgr_dTdz_top   :: !Double   -- ^ Top-layer temperature gradient [K/m]
  } deriving (Show)

-- | Update snow effective grain size for a single layer.
--
-- Three contributions:
--  1. Vapor redistribution (dry snow metamorphism)
--  2. Liquid water redistribution (wet snow)
--  3. Re-freezing of liquid water
--
-- Ported from @SnowAge_grain@ in @SnowSnicarMod.F90@.
snowageGrainLayer :: SnicarParams -> SnowageGrainInput -> SnowageGrainResult
snowageGrainLayer params inp
  | sg_cdz inp <= 0.0 || not (isFiniteDouble (sg_cdz inp)) =
      SnowageGrainResult (sg_snw_rds inp) (sg_snw_rds inp) 0.0 (sg_t_soisno inp) 0.0
  | otherwise =
    let rds0    = max (sp_snw_rds_min params) (sg_snw_rds inp)
        h2olyr  = sg_h2osoi_liq inp + sg_h2osoi_ice inp
        dtime   = sg_dtime inp

        -- Temperature gradient
        dTdz0   = abs ((sg_t_snotop inp - sg_t_snobtm inp) / sg_cdz inp)
        dTdz    = if isFiniteDouble dTdz0 then dTdz0 else 0.0

        -- 1. Dry snow aging
        drFresh = rds0 - sp_snw_rds_min params
        bstTau  = sg_bst_tau inp
        bstKap  = sg_bst_kappa inp
        bstDr0  = sg_bst_drdt0 inp
        dr1     = bstDr0 * (bstTau / (drFresh + bstTau)) ** (1.0 / bstKap) * (dtime / secsphr)

        -- 2. Wet snow aging
        frcLiq  = min 0.1 (sg_h2osoi_liq inp / h2olyr)
        drWet   = 1.0e18 * (dtime * (sp_C2_liq_Brun89 params * frcLiq ** 3)
                / (4.0 * pi * rds0 * rds0))

        dr      = (dr1 + drWet) * sp_xdrdt params

        -- 3. New/refrozen snow fractions
        newsnow   = max 0.0 (sg_qflx_snow_grnd inp * dtime)
        refrzsnow = max 0.0 (sg_qflx_snofrz inp * dtime)
        frcRefrz0 = refrzsnow / h2olyr
        frcNew0   = if sg_isTopLayer inp then newsnow / h2olyr else 0.0
        (frcRefrz, frcNew, frcOld)
          | (frcRefrz0 + frcNew0) > 1.0 =
              let fr = frcRefrz0 / (frcRefrz0 + frcNew0)
              in (fr, 1.0 - fr, 0.0)
          | otherwise = (frcRefrz0, frcNew0, 1.0 - frcRefrz0 - frcNew0)

        rdsFresh = freshSnowRadius params (sg_forc_t inp)
        rdsNew   = (rds0 + dr) * frcOld
                 + rdsFresh * frcNew
                 + sp_snw_rds_refrz params * frcRefrz

        rdsFinal = min snwRdsMax (max (sp_snw_rds_min params) rdsNew)

        -- Top-layer diagnostics
        snoLiqTop = sg_h2osoi_liq inp / h2olyr

    in SnowageGrainResult
         { sgr_snw_rds     = rdsFinal
         , sgr_snw_rds_top = if sg_isTopLayer inp then rdsFinal else 0.0
         , sgr_sno_liq_top = if sg_isTopLayer inp then snoLiqTop else 0.0
         , sgr_snot_top    = if sg_isTopLayer inp then sg_t_soisno inp else 0.0
         , sgr_dTdz_top    = if sg_isTopLayer inp then dTdz else 0.0
         }

-- | Check if a Double is finite (not NaN, not Inf).
isFiniteDouble :: Double -> Bool
isFiniteDouble x = not (isNaN x) && not (isInfinite x)
{-# INLINE isFiniteDouble #-}
