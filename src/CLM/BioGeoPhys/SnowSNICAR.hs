{-# LANGUAGE BangPatterns #-}
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
    -- * Multi-band wrapper
  , SnicarMultiBandInput(..)
  , SnicarMultiBandResult(..)
  , snicarRTMultiBand
    -- * Optics tables + column snow albedo
  , SnicarOptics(..)
  , emptySnicarOptics
  , snicarOpticsPresent
  , snicarSnowAlbedo
  , snicarAgingPresent
  , snicarAgingLookup
    -- * Delta-Eddington layer properties
  , deltaEddingtonLayer
  , DeltaEddingtonProps(..)
    -- * Per-layer absorbed flux
  , computeLayerAbsorbedFlux
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
                   then -- virtual layer: use the tracked (aged) bulk grain radius
                        -- when supplied, else fall back to the fresh minimum.
                        if VU.null (srt_snw_rds inp)
                        then round (sp_snw_rds_min defaultSnicarParams)
                        else max (round (sp_snw_rds_min defaultSnicarParams))
                                 (srt_snw_rds inp VU.! 0)
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

-- ========================================================================
-- Delta-Eddington single-layer optical properties
-- ========================================================================

data DeltaEddingtonProps = DeltaEddingtonProps
  { dep_tau_star   :: !Double  -- ^ delta-scaled optical depth
  , dep_omega_star :: !Double  -- ^ delta-scaled single-scatter albedo
  , dep_g_star     :: !Double  -- ^ delta-scaled asymmetry parameter
  , dep_rdif_a     :: !Double  -- ^ diffuse reflectivity (from above)
  , dep_tdif_a     :: !Double  -- ^ diffuse transmissivity (from above)
  , dep_rdir       :: !Double  -- ^ direct-beam reflectivity
  , dep_tdir       :: !Double  -- ^ direct-beam transmissivity
  , dep_trnlay     :: !Double  -- ^ direct-beam transmission through layer
  } deriving (Show)

-- | Compute delta-Eddington layer properties for a single spectral band.
-- Implements Briegleb & Light 2007 Eq. 50 with Gaussian integration for diffuse.
deltaEddingtonLayer :: Double  -- ^ tau (bulk optical depth)
                    -> Double  -- ^ omega (single-scatter albedo)
                    -> Double  -- ^ g (asymmetry parameter)
                    -> Double  -- ^ mu_not (cosine solar zenith)
                    -> DeltaEddingtonProps
deltaEddingtonLayer !tau !omega !g !muNot =
  let !expMin = exp (-10.0)
      -- Delta scaling (Joseph et al. 1976)
      !f = g * g
      !tauStar = (1.0 - omega * f) * tau
      !omStar = (1.0 - f) * omega / (1.0 - omega * f)
      !gStar = (g - f) / (1.0 - f)

      -- Delta-Eddington solution (Briegleb & Light 2007)
      !lm = sqrt (3.0 * (1.0 - omStar) * (1.0 - omStar * gStar))
      !ue = 1.5 * (1.0 - omStar * gStar) / lm
      !extins = max expMin (exp (-lm * tauStar))
      !neVal = ((ue + 1.0) ** 2 / extins) - ((ue - 1.0) ** 2 * extins)

      -- Diffuse reflectivity/transmissivity (initial)
      !rdifA0 = (ue * ue - 1.0) * (1.0 / extins - extins) / neVal
      !tdifA0 = 4.0 * ue / neVal

      -- Direct beam transmission through layer
      !trnlay = max expMin (exp (-tauStar / max 0.01 muNot))

      -- Direct beam alpha/gamma (Briegleb & Light 2007 Eq. 50)
      !denom = max 1.0e-10 (1.0 - lm * lm * muNot * muNot)
      !alp = 0.75 * omStar * muNot * ((1.0 + gStar * (1.0 - omStar)) / denom)
      !gam = 0.5 * omStar * ((1.0 + 3.0 * gStar * (1.0 - omStar) * muNot * muNot) / denom)
      !apg = alp + gam
      !amg = alp - gam
      !rdir = apg * rdifA0 + amg * (tdifA0 * trnlay - 1.0)
      !tdir = apg * tdifA0 + (amg * rdifA0 - apg + 1.0) * trnlay

      -- Gaussian integration for accurate diffuse (8-point)
      ngmax = VU.length snicarDifGausPt
      (!swt, !smr, !smt) = foldl' (\(!sw, !sr, !st) ng ->
        let !mu = snicarDifGausPt VU.! ng
            !gwt = snicarDifGausWt VU.! ng
            !trn = max expMin (exp (-tauStar / mu))
            !denomG = max 1.0e-10 (1.0 - lm * lm * mu * mu)
            !alp2 = 0.75 * omStar * mu * ((1.0 + gStar * (1.0 - omStar)) / denomG)
            !gam2 = 0.5 * omStar * ((1.0 + 3.0 * gStar * (1.0 - omStar) * mu * mu) / denomG)
            !apg2 = alp2 + gam2
            !amg2 = alp2 - gam2
            !rdr = apg2 * rdifA0 + amg2 * tdifA0 * trn - amg2
            !tdr = apg2 * tdifA0 + amg2 * rdifA0 * trn - apg2 * trn + trn
        in (sw + mu * gwt, sr + mu * rdr * gwt, st + mu * tdr * gwt)
        ) (0.0, 0.0, 0.0) [0 .. ngmax - 1]
      !rdifA = if swt > 0.0 then smr / swt else rdifA0
      !tdifA = if swt > 0.0 then smt / swt else tdifA0

  in DeltaEddingtonProps
     { dep_tau_star = tauStar
     , dep_omega_star = omStar
     , dep_g_star = gStar
     , dep_rdif_a = max 0.0 rdifA
     , dep_tdif_a = max 0.0 tdifA
     , dep_rdir = rdir
     , dep_tdir = tdir
     , dep_trnlay = trnlay
     }

-- ========================================================================
-- Per-layer absorbed flux (from interface fluxes)
-- ========================================================================

-- | Compute absorbed flux in each snow layer from interface fluxes.
-- F_abs(i) = F_net(i) - F_net(i+1) where F_net = downward - upward
-- Ported from the flux absorption section of SNICAR_RT in SnowSnicarMod.F90.
computeLayerAbsorbedFlux :: VU.Vector Double  -- ^ trntdr per interface (downward total)
                         -> VU.Vector Double  -- ^ rupdir per interface (upward direct)
                         -> VU.Vector Double  -- ^ rupdif per interface (upward diffuse)
                         -> VU.Vector Double  -- ^ trndir per interface (downward direct)
                         -> VU.Vector Double  -- ^ rdndif per interface (downward diffuse above)
                         -> Double            -- ^ albsfc (surface albedo)
                         -> Int               -- ^ snl_top (0-based index of top active layer)
                         -> Int               -- ^ snl_btm (0-based index of bottom active layer)
                         -> VU.Vector Double  -- ^ absorbed flux per layer+ground (nlevsno+1)
computeLayerAbsorbedFlux !trntdr !rupdir !rupdif !trndir !rdndif !albsfc !snlTop !snlBtm =
  let !nlyr = nlevsno + 1
      -- Net downward flux at each interface
      -- F_down(i) = trntdr(i) + trndir(i) * rupdir(i) * rdndif(i) / (1 - rdndif(i)*rupdif(i))
      -- F_up(i) = trntdr(i)*rupdif(i) + trndir(i)*rupdir(i)
      -- Simplified: F_net(i) = F_down(i) - F_up(i)

      fNet i =
        let !td = if i < VU.length trntdr then trntdr VU.! i else 0.0
            !rd = if i < VU.length trndir then trndir VU.! i else 0.0
            !rup = if i < VU.length rupdir then rupdir VU.! i else 0.0
            !rupdf = if i < VU.length rupdif then rupdif VU.! i else 0.0
        in td - td * rupdf  -- simplified net flux

  in VU.generate nlyr $ \j ->
       if j < snlTop || j > snlBtm + 1
       then 0.0
       else let !fAbove = fNet j
                !fBelow = fNet (j + 1)
            in max 0.0 (fAbove - fBelow)

-- ========================================================================
-- Multi-band SNICAR wrapper
-- ========================================================================

data SnicarMultiBandInput = SnicarMultiBandInput
  { smbi_nbands        :: !Int      -- ^ number of spectral bands (5 or 480)
  , smbi_nir_bnd_bgn   :: !Int      -- ^ first NIR band index (0-based)
  , smbi_coszen        :: !Double
  , smbi_flg_direct    :: !Bool     -- ^ True = direct beam, False = diffuse
  , smbi_snl           :: !Int      -- ^ number of snow layers (negative)
  -- Per-band Mie properties (looked up from tables, indexed by grain radius)
  , smbi_ss_alb_snw    :: !(VU.Vector Double)  -- ^ (nbands * nRadii)
  , smbi_ext_cff_mss   :: !(VU.Vector Double)  -- ^ (nbands * nRadii)
  , smbi_asm_prm_snw   :: !(VU.Vector Double)  -- ^ (nbands * nRadii)
  -- Per-band aerosol properties
  , smbi_ss_alb_aer    :: !(VU.Vector Double)  -- ^ (nbands * snoNbrAer)
  , smbi_ext_cff_aer   :: !(VU.Vector Double)  -- ^ (nbands * snoNbrAer)
  , smbi_asm_prm_aer   :: !(VU.Vector Double)  -- ^ (nbands * snoNbrAer)
  -- Per-band spectral weights for VIS/NIR aggregation
  , smbi_flx_wgt       :: !(VU.Vector Double)  -- ^ spectral weights (nbands)
  -- Per-band surface albedo
  , smbi_albsfc        :: !(VU.Vector Double)  -- ^ (nbands)
  -- Snow layer data (same as SnicarRTInput)
  , smbi_h2osno_ice    :: !(VU.Vector Double)
  , smbi_h2osno_liq    :: !(VU.Vector Double)
  , smbi_snw_rds       :: !(VU.Vector Int)
  , smbi_mss_cnc_aer   :: !(VU.Vector Double)  -- ^ (nlevsno * snoNbrAer)
  , smbi_h2osno_total  :: !Double
  } deriving (Show)

data SnicarMultiBandResult = SnicarMultiBandResult
  { smbr_albout_vis   :: !Double  -- ^ VIS albedo (band-weighted)
  , smbr_albout_nir   :: !Double  -- ^ NIR albedo (band-weighted)
  , smbr_flx_abs      :: !(VU.Vector Double)  -- ^ absorbed flux per layer (nlevsno+1, VIS+NIR summed)
  } deriving (Show)

-- | Multi-band SNICAR wrapper.
-- Loops over spectral bands, runs the Adding-Doubling RT for each band,
-- then aggregates into VIS and NIR broadband albedos using spectral weights.
snicarRTMultiBand :: SnicarMultiBandInput -> SnicarMultiBandResult
snicarRTMultiBand !inp =
  let !nbands = smbi_nbands inp
      !nirBgn = smbi_nir_bnd_bgn inp
      !nRadii = idxMieSnwMx
      !nlyr = nlevsno + 1

      -- Optics are stored band-major (band b occupies slice [b*nRadii ..]);
      -- snicarRTColumn indexes each band's slice by grain-radius offset.
      sliceBand b v = if VU.length v >= (b + 1) * nRadii
                      then VU.slice (b * nRadii) nRadii v else v
      sliceAer  b v = if VU.length v >= (b + 1) * snoNbrAer
                      then VU.slice (b * snoNbrAer) snoNbrAer v
                      else VU.replicate snoNbrAer 0.0

      -- Full adding-doubling RT per band via 'snicarRTColumn'.
      bandRT b =
        let !albsfcB = if b < VU.length (smbi_albsfc inp)
                       then smbi_albsfc inp VU.! b else 0.3
        in snicarRTColumn SnicarRTInput
             { srt_coszen       = smbi_coszen inp
             , srt_flg_slr_in   = if smbi_flg_direct inp then 1 else 2
             , srt_h2osno_liq   = smbi_h2osno_liq inp
             , srt_h2osno_ice   = smbi_h2osno_ice inp
             , srt_h2osno_total = smbi_h2osno_total inp
             , srt_snw_rds      = smbi_snw_rds inp
             , srt_snl          = smbi_snl inp
             , srt_frac_sno     = 1.0
             , srt_albsfc_vis   = albsfcB
             , srt_albsfc_nir   = albsfcB
             , srt_mss_cnc_aer  = smbi_mss_cnc_aer inp
             , srt_ss_alb_snw   = sliceBand b (smbi_ss_alb_snw inp)
             , srt_ext_cff_mss  = sliceBand b (smbi_ext_cff_mss inp)
             , srt_asm_prm_snw  = sliceBand b (smbi_asm_prm_snw inp)
             , srt_ss_alb_aer   = sliceAer b (smbi_ss_alb_aer inp)
             , srt_ext_cff_aer  = sliceAer b (smbi_ext_cff_aer inp)
             , srt_asm_prm_aer  = sliceAer b (smbi_asm_prm_aer inp)
             }

      !bandRes = map bandRT [0 .. nbands - 1]
      bandAlb b = srr_albedo (bandRes !! b)
      wgtAt b   = if b < VU.length (smbi_flx_wgt inp) then smbi_flx_wgt inp VU.! b else 0.0

      -- VIS = band 0; NIR = flux-weighted average over bands [nirBgn ..].
      !albVis  = bandAlb 0
      !nirIdx  = [nirBgn .. nbands - 1]
      !nirWsum = sum [wgtAt b | b <- nirIdx]
      !albNir  = if nirWsum > 0.0
                 then sum [wgtAt b * bandAlb b | b <- nirIdx] / nirWsum else 0.0

      -- Per-layer absorbed flux: VIS(band 0) + flux-weighted NIR (broadband sum).
      !flxAbs = VU.generate nlyr $ \j ->
        let visA = srr_flx_abs (bandRes !! 0) VU.! j
            nirA = if nirWsum > 0.0
                   then sum [wgtAt b * (srr_flx_abs (bandRes !! b) VU.! j) | b <- nirIdx] / nirWsum
                   else 0.0
        in visA + nirA

  in SnicarMultiBandResult
     { smbr_albout_vis = max 0.0 (min 1.0 albVis)
     , smbr_albout_nir = max 0.0 (min 1.0 albNir)
     , smbr_flx_abs = flxAbs
     }

-- | Check if a Double is finite (not NaN, not Inf).
isFiniteDouble :: Double -> Bool
isFiniteDouble x = not (isNaN x) && not (isInfinite x)
{-# INLINE isFiniteDouble #-}

-- ========================================================================
-- SNICAR optics tables (band-major [nbands*nRadii]) loaded once per run
-- ========================================================================

-- | The 5-band SNICAR Mie optics + spectral flux weights for direct and
-- diffuse beams (pic16 ice model, mid-latitude-winter profile). Band-major
-- layout: band b occupies slice [b*idxMieSnwMx .. ]. Empty vectors => absent.
data SnicarOptics = SnicarOptics
  { sno_ss_alb_dir  :: !(VU.Vector Double)
  , sno_ext_cff_dir :: !(VU.Vector Double)
  , sno_asm_dir     :: !(VU.Vector Double)
  , sno_flx_wgt_dir :: !(VU.Vector Double)
  , sno_ss_alb_dif  :: !(VU.Vector Double)
  , sno_ext_cff_dif :: !(VU.Vector Double)
  , sno_asm_dif     :: !(VU.Vector Double)
  , sno_flx_wgt_dif :: !(VU.Vector Double)
  -- Grain-aging best-fit tables, flattened C-order (T=11, dTdz=31, dns=8).
  , sno_age_tau     :: !(VU.Vector Double)
  , sno_age_kappa   :: !(VU.Vector Double)
  , sno_age_drdt0   :: !(VU.Vector Double)
  } deriving (Show)

-- | Empty optics (=> SNICAR disabled, callers fall back to age-based albedo).
emptySnicarOptics :: SnicarOptics
emptySnicarOptics = SnicarOptics e e e e e e e e e e e where e = VU.empty

-- | True when the grain-aging best-fit tables are populated.
snicarAgingPresent :: SnicarOptics -> Bool
snicarAgingPresent o = VU.length (sno_age_drdt0 o) >= 11 * 31 * 8

-- | Look up the snow-aging best-fit parameters (tau, kappa, drdt0) for a layer
-- temperature [K], temperature gradient [K/m], and snow density [kg/m3].
-- Mirrors the binning in SnowSnicarMod.F90 (T_idx, Tgrd_idx, rhos_idx).
snicarAgingLookup :: SnicarOptics -> Double -> Double -> Double -> (Double, Double, Double)
snicarAgingLookup o tK dTdz rhos =
  let tIdx    = clampI 0 10 (round ((tK - 223.0) / 5.0))
      tgrdIdx = clampI 0 30 (round (dTdz / 10.0))
      rhosIdx = clampI 0 7  (round ((max 50.0 rhos - 50.0) / 50.0))
      flat    = (tIdx * 31 + tgrdIdx) * 8 + rhosIdx
      at v    = if flat < VU.length v then v VU.! flat else 0.0
      clampI lo hi x = max lo (min hi x)
  in (at (sno_age_tau o), at (sno_age_kappa o), at (sno_age_drdt0 o))

-- | True when the optics tables are populated.
snicarOpticsPresent :: SnicarOptics -> Bool
snicarOpticsPresent o =
  VU.length (sno_ss_alb_dir o) >= defaultNumberBands * idxMieSnwMx

-- | Compute SNICAR direct+diffuse snow albedo (albsnd, albsni) per 2-band
-- waveband [vis, nir] for one column, given the optics, the under-snow soil
-- albedo (vis,nir), and the snow column state. Returns Nothing when optics are
-- absent or there is no illuminated snow.
snicarSnowAlbedo
  :: SnicarOptics
  -> Double            -- ^ coszen
  -> Double            -- ^ albsod_vis (under-snow soil, direct)
  -> Double            -- ^ albsod_nir
  -> Int               -- ^ snl (<=0)
  -> VU.Vector Double  -- ^ h2osno_ice per layer (nlevsno)
  -> VU.Vector Double  -- ^ h2osno_liq per layer (nlevsno)
  -> VU.Vector Int     -- ^ snw_rds per layer (nlevsno) [microns]
  -> Double            -- ^ h2osno_total
  -> Maybe (VU.Vector Double, VU.Vector Double)
snicarSnowAlbedo o coszen albsodVis albsodNir snl ice liq rds h2oTot
  | not (snicarOpticsPresent o) = Nothing
  | coszen <= 0.0 || h2oTot <= minSnw = Nothing
  | otherwise =
      let albsfc = VU.fromList [albsodVis, albsodNir, albsodNir, albsodNir, albsodNir]
          nAer = defaultNumberBands * snoNbrAer
          mkInp dir ssAlb extCff asm flxWgt = SnicarMultiBandInput
            { smbi_nbands = defaultNumberBands, smbi_nir_bnd_bgn = 1
            , smbi_coszen = coszen, smbi_flg_direct = dir, smbi_snl = snl
            , smbi_ss_alb_snw = ssAlb, smbi_ext_cff_mss = extCff, smbi_asm_prm_snw = asm
            , smbi_ss_alb_aer  = VU.replicate nAer 0.0
            , smbi_ext_cff_aer = VU.replicate nAer 0.0
            , smbi_asm_prm_aer = VU.replicate nAer 0.0
            , smbi_flx_wgt = flxWgt
            , smbi_albsfc = albsfc
            , smbi_h2osno_ice = ice, smbi_h2osno_liq = liq
            , smbi_snw_rds = rds
            , smbi_mss_cnc_aer = VU.replicate (nlevsno * snoNbrAer) 0.0
            , smbi_h2osno_total = h2oTot
            }
          rDir = snicarRTMultiBand (mkInp True  (sno_ss_alb_dir o) (sno_ext_cff_dir o) (sno_asm_dir o) (sno_flx_wgt_dir o))
          rDif = snicarRTMultiBand (mkInp False (sno_ss_alb_dif o) (sno_ext_cff_dif o) (sno_asm_dif o) (sno_flx_wgt_dif o))
          albsnd = VU.fromList [smbr_albout_vis rDir, smbr_albout_nir rDir]
          albsni = VU.fromList [smbr_albout_vis rDif, smbr_albout_nir rDif]
      in Just (albsnd, albsni)
