-- | Land-to-atmosphere aggregation using subgrid averaging.
-- Ported from Julia: src/infrastructure/lnd2atm_mod.jl
-- Fortran: src/main/lnd2atmMod.F90
--
-- Pure functions that map land model output from patch/column/landunit
-- to gridcell level for coupling to the atmosphere.
module CLM.Infrastructure.Lnd2Atm
  ( -- * Land-to-atmosphere data
    Lnd2AtmData(..)
  , defaultLnd2AtmData
    -- * Land model fields (input)
  , LandOutputFields(..)
    -- * Aggregation functions
  , lnd2atm
  , lnd2atmMinimal
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!))

import CLM.Constants.PhysicalConstants (sb, numrad)
import CLM.Infrastructure.InitSubgrid
  ( BoundsType(..), LandunitData(..), SubgridColumnData(..), SubgridPatchData(..)
  , spval
  )
import CLM.Infrastructure.SubgridAverage
  ( p2g1d, c2g1d
  , ScaleType(..), C2LScaleType(..), L2GScaleType(..)
  )

-- ============================================================================
-- Land-to-atmosphere data (gridcell level output)
-- ============================================================================

-- | Gridcell-level data for coupling to the atmosphere.
data Lnd2AtmData = Lnd2AtmData
  { l2aTradGrc           :: !(Vector Double)  -- ^ Radiative temperature [K]
  , l2aTref2mGrc         :: !(Vector Double)  -- ^ 2m reference temperature [K]
  , l2aUref10mGrc        :: !(Vector Double)  -- ^ 10m wind speed [m/s]
  , l2aTauxGrc           :: !(Vector Double)  -- ^ Zonal surface stress [kg/m/s^2]
  , l2aTauyGrc           :: !(Vector Double)  -- ^ Meridional surface stress [kg/m/s^2]
  , l2aZ0mGrc            :: !(Vector Double)  -- ^ Roughness length [m]
  , l2aFvGrc             :: !(Vector Double)  -- ^ Friction velocity [m/s]
  , l2aRam1Grc           :: !(Vector Double)  -- ^ Aerodynamic resistance [s/m]
  , l2aEflxShTotGrc      :: !(Vector Double)  -- ^ Sensible heat flux [W/m^2]
  , l2aEflxLhTotGrc      :: !(Vector Double)  -- ^ Latent heat flux [W/m^2]
  , l2aEflxLwradOutGrc   :: !(Vector Double)  -- ^ Outgoing longwave [W/m^2]
  , l2aFsaGrc            :: !(Vector Double)  -- ^ Absorbed shortwave [W/m^2]
  , l2aAlbdGrc           :: !(Vector Double)  -- ^ Direct beam albedo (ngrc * numrad)
  , l2aAlbiGrc           :: !(Vector Double)  -- ^ Diffuse beam albedo (ngrc * numrad)
  , l2aNetCarbonGrc      :: !(Vector Double)  -- ^ Net carbon exchange [gC/m^2/s]
  } deriving (Show)

-- | Default (empty) land-to-atmosphere data.
defaultLnd2AtmData :: Int -> Lnd2AtmData
defaultLnd2AtmData ngrc = Lnd2AtmData
  { l2aTradGrc         = VU.replicate ngrc 0.0
  , l2aTref2mGrc       = VU.replicate ngrc 0.0
  , l2aUref10mGrc      = VU.replicate ngrc 0.0
  , l2aTauxGrc         = VU.replicate ngrc 0.0
  , l2aTauyGrc         = VU.replicate ngrc 0.0
  , l2aZ0mGrc          = VU.replicate ngrc 0.0
  , l2aFvGrc           = VU.replicate ngrc 0.0
  , l2aRam1Grc         = VU.replicate ngrc 0.0
  , l2aEflxShTotGrc    = VU.replicate ngrc 0.0
  , l2aEflxLhTotGrc    = VU.replicate ngrc 0.0
  , l2aEflxLwradOutGrc = VU.replicate ngrc 0.0
  , l2aFsaGrc          = VU.replicate ngrc 0.0
  , l2aAlbdGrc         = VU.replicate (ngrc * numrad) 0.0
  , l2aAlbiGrc         = VU.replicate (ngrc * numrad) 0.0
  , l2aNetCarbonGrc    = VU.replicate ngrc 0.0
  }

-- ============================================================================
-- Land model output fields (patch/column level)
-- ============================================================================

-- | Collection of land model output fields needed for l2a aggregation.
data LandOutputFields = LandOutputFields
  { lofEflxLwradOutPatch  :: !(Vector Double)  -- ^ Outgoing LW per patch
  , lofTref2mPatch        :: !(Vector Double)  -- ^ 2m temperature per patch
  , lofU10ClmPatch        :: !(Vector Double)  -- ^ 10m wind per patch
  , lofTauxPatch          :: !(Vector Double)  -- ^ Zonal stress per patch
  , lofTauyPatch          :: !(Vector Double)  -- ^ Meridional stress per patch
  , lofZ0mActualPatch     :: !(Vector Double)  -- ^ Roughness length per patch
  , lofFvPatch            :: !(Vector Double)  -- ^ Friction velocity per patch
  , lofRam1Patch          :: !(Vector Double)  -- ^ Aerodynamic resistance per patch
  , lofEflxShTotPatch     :: !(Vector Double)  -- ^ Sensible heat per patch
  , lofEflxLhTotPatch     :: !(Vector Double)  -- ^ Latent heat per patch
  , lofFsaPatch           :: !(Vector Double)  -- ^ Absorbed SW per patch
  , lofAlbdPatch          :: !(Vector Double)  -- ^ Direct albedo (np * numrad)
  , lofAlbiPatch          :: !(Vector Double)  -- ^ Diffuse albedo (np * numrad)
  , lofTgrndCol           :: !(Vector Double)  -- ^ Ground temperature per column
  , lofEflxShPrecipConvCol :: !(Vector Double) -- ^ SH from precip conversion per column
  } deriving (Show)

-- ============================================================================
-- Full land-to-atmosphere aggregation
-- ============================================================================

-- | Map land model output fields from patch/column/landunit to gridcell level.
-- Pure function.
lnd2atm
  :: BoundsType
  -> LandOutputFields
  -> SubgridPatchData
  -> SubgridColumnData
  -> LandunitData
  -> Lnd2AtmData
lnd2atm bounds lof pch col lun =
  let ng = endg bounds

      -- Radiative temperature from LW outgoing
      tRad = computeRadTemp bounds (lofEflxLwradOutPatch lof) (lofTgrndCol lof) pch

      -- p2g averages for scalar fields
      tRef2m = p2g1d (lofTref2mPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      u10    = p2g1d (lofU10ClmPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      taux   = p2g1d (lofTauxPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      tauy   = p2g1d (lofTauyPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      z0m    = p2g1d (lofZ0mActualPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      fv     = p2g1d (lofFvPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      ram1   = p2g1d (lofRam1Patch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      shTot  = p2g1d (lofEflxShTotPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      lhTot  = p2g1d (lofEflxLhTotPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      lwOut  = p2g1d (lofEflxLwradOutPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun
      fsa    = p2g1d (lofFsaPatch lof) bounds ScaleUnity C2LUnity L2GUnity pch col lun

      -- Add precipitation conversion sensible heat (c2g)
      shPrecip = if VU.length (lofEflxShPrecipConvCol lof) > 0
                 then c2g1d (lofEflxShPrecipConvCol lof) bounds C2LUnity L2GUnity col lun
                 else VU.replicate ng 0.0
      shTotFinal = VU.zipWith (\s sp -> if s /= spval && sp /= spval then s + sp else s)
                     shTot shPrecip

      -- Albedos (weighted average per band)
      (albd, albi) = computeAlbedos bounds (lofAlbdPatch lof) (lofAlbiPatch lof) pch

      -- Net carbon = 0 (no CN mode)
      netCarbon = VU.replicate ng 0.0

  in  Lnd2AtmData
        { l2aTradGrc         = tRad
        , l2aTref2mGrc       = tRef2m
        , l2aUref10mGrc      = u10
        , l2aTauxGrc         = taux
        , l2aTauyGrc         = tauy
        , l2aZ0mGrc          = z0m
        , l2aFvGrc           = fv
        , l2aRam1Grc         = ram1
        , l2aEflxShTotGrc    = shTotFinal
        , l2aEflxLhTotGrc    = lhTot
        , l2aEflxLwradOutGrc = lwOut
        , l2aFsaGrc          = fsa
        , l2aAlbdGrc         = albd
        , l2aAlbiGrc         = albi
        , l2aNetCarbonGrc    = netCarbon
        }

-- ============================================================================
-- Minimal land-to-atmosphere (albedo + radiative temperature only)
-- ============================================================================

-- | Minimal aggregation: albedo and radiative temperature only.
-- Used for the first timestep before full physics runs.
lnd2atmMinimal
  :: BoundsType
  -> Vector Double   -- ^ eflx_lwrad_out_patch
  -> Vector Double   -- ^ t_grnd_col (fallback)
  -> Vector Double   -- ^ albd_patch (np * numrad)
  -> SubgridPatchData
  -> Lnd2AtmData
lnd2atmMinimal bounds lwOutPatch tGrnd albdPatch pch =
  let ng = endg bounds
      tRad = computeRadTemp bounds lwOutPatch tGrnd pch
      -- Albedos (direct only for minimal)
      albd = computeAlbedoSingle bounds albdPatch pch
  in  (defaultLnd2AtmData ng)
        { l2aTradGrc = tRad
        , l2aAlbdGrc = albd
        }

-- ============================================================================
-- Internal helpers
-- ============================================================================

-- | Compute radiative temperature from outgoing LW (patch -> gridcell).
-- t_rad = (sum(lwout * wtgcell) / SB)^0.25
computeRadTemp :: BoundsType -> Vector Double -> Vector Double -> SubgridPatchData -> Vector Double
computeRadTemp bounds lwOutPatch tGrnd pch =
  let ng = endg bounds
  in  VU.generate ng $ \gix ->
        let g = gix + 1
            -- Sum weighted LW outgoing for this gridcell
            (lwSum, _cnt) = VU.ifoldl'
              (\(acc, n) ix isActive ->
                let p = ix + 1
                in  if p >= begp bounds && p <= endp bounds && isActive
                       && pchGridcell pch ! ix == g
                    then (acc + lwOutPatch ! ix * (pchWtgcell pch ! ix), n + 1)
                    else (acc, n)
              ) (0.0 :: Double, 0 :: Int) (pchActive pch)
        in  if lwSum > 0.0
            then (lwSum / sb) ** 0.25
            else if VU.length tGrnd > 0
                 then tGrnd ! 0  -- fallback
                 else 273.15

-- | Compute weighted-average albedo per band (direct).
computeAlbedoSingle :: BoundsType -> Vector Double -> SubgridPatchData -> Vector Double
computeAlbedoSingle bounds albPatch pch =
  let ng = endg bounds
  in  VU.generate (ng * numrad) $ \idx ->
        let (gix, b) = idx `divMod` numrad
            g = gix + 1
            (wsum, val) = VU.ifoldl'
              (\(ws, v) ix isActive ->
                let p = ix + 1
                in  if p >= begp bounds && p <= endp bounds && isActive
                       && pchGridcell pch ! ix == g
                    then let w = pchWtgcell pch ! ix
                         in  (ws + w, v + albPatch ! (ix * numrad + b) * w)
                    else (ws, v)
              ) (0.0 :: Double, 0.0 :: Double) (pchActive pch)
        in  if wsum > 0.0 then val / wsum else 0.0

-- | Compute weighted-average albedos for both direct and diffuse.
computeAlbedos
  :: BoundsType -> Vector Double -> Vector Double -> SubgridPatchData
  -> (Vector Double, Vector Double)
computeAlbedos bounds albdPatch albiPatch pch =
  let ng = endg bounds
      albd = VU.generate (ng * numrad) $ \idx ->
        let (gix, b) = idx `divMod` numrad
            g = gix + 1
            (wsum, val) = VU.ifoldl'
              (\(ws, v) ix isActive ->
                let p = ix + 1
                in  if p >= begp bounds && p <= endp bounds && isActive
                       && pchGridcell pch ! ix == g
                    then let w = pchWtgcell pch ! ix
                         in  (ws + w, v + albdPatch ! (ix * numrad + b) * w)
                    else (ws, v)
              ) (0.0 :: Double, 0.0 :: Double) (pchActive pch)
        in  if wsum > 0.0 then val / wsum else 0.0
      albi = VU.generate (ng * numrad) $ \idx ->
        let (gix, b) = idx `divMod` numrad
            g = gix + 1
            (wsum, val) = VU.ifoldl'
              (\(ws, v) ix isActive ->
                let p = ix + 1
                in  if p >= begp bounds && p <= endp bounds && isActive
                       && pchGridcell pch ! ix == g
                    then let w = pchWtgcell pch ! ix
                         in  (ws + w, v + albiPatch ! (ix * numrad + b) * w)
                    else (ws, v)
              ) (0.0 :: Double, 0.0 :: Double) (pchActive pch)
        in  if wsum > 0.0 then val / wsum else 0.0
  in  (albd, albi)
