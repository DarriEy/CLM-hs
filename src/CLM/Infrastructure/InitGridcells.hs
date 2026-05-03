-- | High-level grid construction: builds g/l/c/p hierarchy from surface data.
-- Ported from Julia: src/infrastructure/init_gridcells.jl
-- Fortran: src/main/initGridCellsMod.F90
--
-- All functions are pure. The master builder takes surface input data and
-- produces the complete subgrid hierarchy.
module CLM.Infrastructure.InitGridcells
  ( -- * Surface input for grid construction
    GridSurfaceInput(..)
  , defaultGridSurfaceInput
    -- * Grid construction configuration
  , GridConfig(..)
  , defaultGridConfig
    -- * Master builder
  , initGridCells
    -- * Per-landunit builders (exported for testing)
  , setLandunitVegCompete
  , setLandunitCropNoncompete
  , setLandunitUrban
  , setLandunitWetLake
  , setLandunitIce
    -- * Higher-order weight computation
  , computeHigherOrderWeights
  ) where

import qualified Data.Vector.Unboxed as VU
import Data.Vector.Unboxed (Vector, (!))

import CLM.Infrastructure.InitSubgrid

-- ============================================================================
-- Configuration
-- ============================================================================

-- | Configuration for grid construction.
data GridConfig = GridConfig
  { gcCreateCropLandunit :: !Bool   -- ^ Create crop landunit?
  , gcRunZeroWeightUrban :: !Bool   -- ^ Create urban even if zero weight?
  , gcNatpftLb           :: !Int    -- ^ Lower bound for natural PFT indices (typically 0)
  , gcCftLb              :: !Int    -- ^ Lower bound for crop functional type indices
  , gcMaxpatchUrb        :: !Int    -- ^ Maximum urban patch count (5)
  , gcMaxpatchGlc        :: !Int    -- ^ Maximum glacier elevation classes
  } deriving (Show, Eq)

defaultGridConfig :: GridConfig
defaultGridConfig = GridConfig
  { gcCreateCropLandunit = True
  , gcRunZeroWeightUrban = False
  , gcNatpftLb           = 0
  , gcCftLb              = 15
  , gcMaxpatchUrb        = 5
  , gcMaxpatchGlc        = 10
  }

-- ============================================================================
-- Surface input data for grid construction
-- ============================================================================

-- | Surface data needed for building the subgrid hierarchy.
-- Vectors are sized per gridcell; matrices are (ngrc, ntype).
data GridSurfaceInput = GridSurfaceInput
  { gsiWtLunit     :: !(Vector Double)  -- ^ Landunit weights (ngrc * maxLunit), row-major
  , gsiWtNatPatch  :: !(Vector Double)  -- ^ Natural PFT weights (ngrc * natpft_size), row-major
  , gsiNatpftSize  :: !Int              -- ^ Number of natural PFTs
  , gsiWtCft       :: !(Vector Double)  -- ^ CFT weights (ngrc * cft_size), row-major
  , gsiCftSize     :: !Int              -- ^ Number of crop functional types
  , gsiWtGlcMec    :: !(Vector Double)  -- ^ Glacier MEC weights (ngrc * glc_size), row-major
  , gsiGlcSize     :: !Int              -- ^ Number of glacier elevation classes
  } deriving (Show)

defaultGridSurfaceInput :: GridSurfaceInput
defaultGridSurfaceInput = GridSurfaceInput
  { gsiWtLunit    = VU.empty
  , gsiWtNatPatch = VU.empty
  , gsiNatpftSize = 0
  , gsiWtCft      = VU.empty
  , gsiCftSize    = 0
  , gsiWtGlcMec   = VU.empty
  , gsiGlcSize    = 0
  }

-- | Look up landunit weight for gridcell g (1-based), landunit type lt (1-based).
getWtLunit :: GridSurfaceInput -> Int -> Int -> Double
getWtLunit surf g lt = gsiWtLunit surf ! ((g - 1) * maxLunit + (lt - 1))

-- | Look up natural PFT weight for gridcell g (1-based), PFT m (1-based).
getWtNatPatch :: GridSurfaceInput -> Int -> Int -> Double
getWtNatPatch surf g m = gsiWtNatPatch surf ! ((g - 1) * gsiNatpftSize surf + (m - 1))

-- | Look up CFT weight for gridcell g (1-based), CFT m (1-based).
getWtCft :: GridSurfaceInput -> Int -> Int -> Double
getWtCft surf g m = gsiWtCft surf ! ((g - 1) * gsiCftSize surf + (m - 1))

-- | Look up glacier MEC weight for gridcell g (1-based), class m (1-based).
getWtGlcMec :: GridSurfaceInput -> Int -> Int -> Double
getWtGlcMec surf g m = gsiWtGlcMec surf ! ((g - 1) * gsiGlcSize surf + (m - 1))

-- ============================================================================
-- Builder state (threaded through construction)
-- ============================================================================

-- | Mutable-like state threaded through the pure construction functions.
data BuilderState = BuilderState
  { bsLi  :: !Int                  -- ^ Current landunit index
  , bsCi  :: !Int                  -- ^ Current column index
  , bsPi  :: !Int                  -- ^ Current patch index
  , bsLun :: !LandunitData         -- ^ Landunit data being built
  , bsCol :: !SubgridColumnData    -- ^ Column data being built
  , bsPch :: !SubgridPatchData     -- ^ Patch data being built
  } deriving (Show)

-- ============================================================================
-- Master builder
-- ============================================================================

-- | Build the complete subgrid hierarchy from surface data.
-- Returns (GridcellData, LandunitData, SubgridColumnData, SubgridPatchData).
-- Pure function.
initGridCells
  :: BoundsType
  -> GridConfig
  -> GridSurfaceInput
  -> Double            -- ^ lat [degrees]
  -> Double            -- ^ lon [degrees]
  -> Double            -- ^ area [km^2]
  -> (GridcellData, LandunitData, SubgridColumnData, SubgridPatchData)
initGridCells bounds cfg surf lat lon area =
  let rpi = 3.14159265358979323846
      nl = endl bounds - begl bounds + 1
      nc = endc bounds - begc bounds + 1
      np = endp bounds - begp bounds + 1

      initState = BuilderState
        { bsLi  = begl bounds - 1
        , bsCi  = begc bounds - 1
        , bsPi  = begp bounds - 1
        , bsLun = defaultLandunitData nl
        , bsCol = defaultSubgridColumnData nc
        , bsPch = defaultSubgridPatchData np
        }

      -- Natural vegetation
      s1 = foldl (\s g -> setLandunitVegCompete cfg surf g s) initState
           [begg bounds .. endg bounds]

      -- Crop
      s2 = foldl (\s g -> setLandunitCropNoncompete cfg surf g s) s1
           [begg bounds .. endg bounds]

      -- Urban TBD, HD, MD
      s3 = foldl (\s (lt, g) -> setLandunitUrban cfg surf lt g s) s2
           [(lt, g) | lt <- [isturb_tbd, isturb_hd, isturb_md]
                    , g <- [begg bounds .. endg bounds]]

      -- Lake
      s4 = foldl (\s g -> setLandunitWetLake surf istdlak g s) s3
           [begg bounds .. endg bounds]

      -- Wetland
      s5 = foldl (\s g -> setLandunitWetLake surf istwet g s) s4
           [begg bounds .. endg bounds]

      -- Glacier
      s6 = foldl (\s g -> setLandunitIce cfg surf g s) s5
           [begg bounds .. endg bounds]

      -- Set gridcell properties
      ngrc = endg bounds - begg bounds + 1
      grc0 = defaultGridcellData ngrc
      grc1 = grc0
        { grcArea   = VU.replicate ngrc area
        , grcLatdeg = VU.replicate ngrc lat
        , grcLondeg = VU.replicate ngrc lon
        , grcLat    = VU.replicate ngrc (lat * rpi / 180.0)
        , grcLon    = VU.replicate ngrc (lon * rpi / 180.0)
        }

      -- Fill down-pointers
      (grc2, lun2, col2) = clmPtrsCompdown bounds grc1 (bsLun s6) (bsCol s6) (bsPch s6)

      -- Compute higher-order weights
      (col3, pch3) = computeHigherOrderWeights bounds col2 lun2 (bsPch s6)

  in  (grc2, lun2, col3, pch3)

-- ============================================================================
-- Higher-order weight computation
-- ============================================================================

-- | Compute col.wtgcell, patch.wtlunit, patch.wtgcell from lower-order weights.
computeHigherOrderWeights
  :: BoundsType -> SubgridColumnData -> LandunitData -> SubgridPatchData
  -> (SubgridColumnData, SubgridPatchData)
computeHigherOrderWeights bounds col lun pch = (col', pch')
  where
    -- col.wtgcell = col.wtlunit * lun.wtgcell
    colWtg = VU.generate (endc bounds) $ \ix ->
      let c = ix + 1
      in  if c >= begc bounds && c <= endc bounds
          then let l = colLandunit col ! ix
               in  colWtlunit col ! ix * (lunWtgcell lun ! (l - 1))
          else 0.0
    col' = col { colWtgcell = colWtg }

    -- patch.wtlunit = patch.wtcol * col.wtlunit
    -- patch.wtgcell = patch.wtcol * col.wtgcell
    pchWtl = VU.generate (endp bounds) $ \ix ->
      let p = ix + 1
      in  if p >= begp bounds && p <= endp bounds
          then let c = pchColumn pch ! ix
               in  pchWtcol pch ! ix * (colWtlunit col ! (c - 1))
          else 0.0
    pchWtg = VU.generate (endp bounds) $ \ix ->
      let p = ix + 1
      in  if p >= begp bounds && p <= endp bounds
          then let c = pchColumn pch ! ix
               in  pchWtcol pch ! ix * (colWtgcell col' ! (c - 1))
          else 0.0
    pch' = pch { pchWtlunit = pchWtl, pchWtgcell = pchWtg }

-- ============================================================================
-- Per-landunit builders
-- ============================================================================

-- | Build natural vegetation landunit (ISTSOIL).
setLandunitVegCompete :: GridConfig -> GridSurfaceInput -> Int -> BuilderState -> BuilderState
setLandunitVegCompete cfg surf gi s0 =
  let wt = getWtLunit surf gi istsoil
      natpftSz = gsiNatpftSize surf
      (lun1, li1) = addLandunit (bsLun s0) (bsLi s0) gi istsoil wt
      (col1, ci1) = addColumn (bsCol s0) lun1 (bsCi s0) li1 1 1.0 False

      -- Add patches for each PFT with non-zero weight
      (pch1, pi1, nAdded) = foldl addPft (bsPch s0, bsPi s0, 0 :: Int)
                               [1 .. natpftSz]
      addPft (pchAcc, piAcc, n) m =
        let pWt = getWtNatPatch surf gi m
        in  if pWt > 0.0
            then let ptype = gcNatpftLb cfg + (m - 1)
                     (pchAcc', piAcc') = addPatch pchAcc col1 lun1 piAcc ci1
                                                  ptype pWt (gcNatpftLb cfg)
                 in  (pchAcc', piAcc', n + 1)
            else (pchAcc, piAcc, n)

      -- Must have at least one patch (bare ground)
      (pchF, piF) = if nAdded == 0
                     then addPatch pch1 col1 lun1 pi1 ci1 noveg 1.0 (gcNatpftLb cfg)
                     else (pch1, pi1)
  in  s0 { bsLi = li1, bsCi = ci1, bsPi = piF
          , bsLun = lun1, bsCol = col1, bsPch = pchF }

-- | Build crop landunit (ISTCROP).
setLandunitCropNoncompete :: GridConfig -> GridSurfaceInput -> Int -> BuilderState -> BuilderState
setLandunitCropNoncompete cfg surf gi s0
  | not (gcCreateCropLandunit cfg) = s0
  | getWtLunit surf gi istcrop <= 0.0 = s0
  | gsiCftSize surf <= 0 = s0
  | otherwise =
    let wt = getWtLunit surf gi istcrop
        (lun1, li1) = addLandunit (bsLun s0) (bsLi s0) gi istcrop wt
        cftSz = gsiCftSize surf

        (col1, pch1, ci1, pi1, nAdded) =
          foldl addCft (bsCol s0, bsPch s0, bsCi s0, bsPi s0, 0 :: Int)
                [1 .. cftSz]
        addCft (colAcc, pchAcc, ciAcc, piAcc, n) m =
          let cftWt = getWtCft surf gi m
          in  if cftWt > 0.0
              then let cftType = gcCftLb cfg + (m - 1)
                       ctype = istcrop * 100 + cftType
                       (colAcc', ciAcc') = addColumn colAcc lun1 ciAcc li1 ctype cftWt False
                       (pchAcc', piAcc') = addPatch pchAcc colAcc' lun1 piAcc ciAcc'
                                                    cftType 1.0 (gcNatpftLb cfg)
                   in  (colAcc', pchAcc', ciAcc', piAcc', n + 1)
              else (colAcc, pchAcc, ciAcc, piAcc, n)

        -- Fallback: at least one crop column
        (colF, pchF, ciF, piF) =
          if nAdded == 0
          then let cftType = gcCftLb cfg
                   ctype = istcrop * 100 + cftType
                   (col2, ci2) = addColumn col1 lun1 ci1 li1 ctype 1.0 False
                   (pch2, pi2) = addPatch pch1 col2 lun1 pi1 ci2
                                          cftType 1.0 (gcNatpftLb cfg)
               in  (col2, pch2, ci2, pi2)
          else (col1, pch1, ci1, pi1)
    in  s0 { bsLi = li1, bsCi = ciF, bsPi = piF
            , bsLun = lun1, bsCol = colF, bsPch = pchF }

-- | Build an urban landunit.
setLandunitUrban :: GridConfig -> GridSurfaceInput -> Int -> Int -> BuilderState -> BuilderState
setLandunitUrban cfg surf ltype gi s0
  | wt <= 0.0 && not (gcRunZeroWeightUrban cfg) = s0
  | otherwise =
    let (lun1, li1) = addLandunit (bsLun s0) (bsLi s0) gi ltype wt
        wtlunitRoof = 0.5
        wtRoadPerv  = 0.5

        -- Add 5 urban columns
        (col1, pch1, ci1, pi1) =
          foldl addUrbCol (bsCol s0, bsPch s0, bsCi s0, bsPi s0)
                [1 .. gcMaxpatchUrb cfg]
        addUrbCol (colAcc, pchAcc, ciAcc, piAcc) m =
          let (ctype, wtcol) = urbanColTypeWeight wtlunitRoof wtRoadPerv m
              (colAcc', ciAcc') = addColumn colAcc lun1 ciAcc li1 ctype wtcol False
              (pchAcc', piAcc') = addPatch pchAcc colAcc' lun1 piAcc ciAcc'
                                           noveg 1.0 (gcNatpftLb cfg)
          in  (colAcc', pchAcc', ciAcc', piAcc')
    in  s0 { bsLi = li1, bsCi = ci1, bsPi = pi1
            , bsLun = lun1, bsCol = col1, bsPch = pch1 }
  where
    wt = getWtLunit surf gi ltype

-- | Determine urban column type and weight.
urbanColTypeWeight :: Double -> Double -> Int -> (Int, Double)
urbanColTypeWeight wtRoof wtPerv m
  | m == 1    = (icolRoof,       wtRoof)
  | m == 2    = (icolSunwall,    (1.0 - wtRoof) / 3.0)
  | m == 3    = (icolShadewall,  (1.0 - wtRoof) / 3.0)
  | m == 4    = (icolRoadImperv, ((1.0 - wtRoof) / 3.0) * (1.0 - wtPerv))
  | otherwise = (icolRoadPerv,   ((1.0 - wtRoof) / 3.0) * wtPerv)

-- | Build a wetland or lake landunit.
setLandunitWetLake :: GridSurfaceInput -> Int -> Int -> BuilderState -> BuilderState
setLandunitWetLake surf ltype gi s0
  | ltype /= istdlak && wt <= 0.0 = s0  -- Always create lake, skip wetland if zero
  | otherwise =
    let (lun1, li1) = addLandunit (bsLun s0) (bsLi s0) gi ltype wt
        (col1, ci1) = addColumn (bsCol s0) lun1 (bsCi s0) li1 ltype 1.0 False
        (pch1, pi1) = addPatch (bsPch s0) col1 lun1 (bsPi s0) ci1 noveg 1.0 0
    in  s0 { bsLi = li1, bsCi = ci1, bsPi = pi1
            , bsLun = lun1, bsCol = col1, bsPch = pch1 }
  where
    wt = getWtLunit surf gi ltype

-- | Build a glacier (ice) landunit.
setLandunitIce :: GridConfig -> GridSurfaceInput -> Int -> BuilderState -> BuilderState
setLandunitIce cfg surf gi s0
  | wt <= 0.0 = s0
  | otherwise =
    let (lun1, li1) = addLandunit (bsLun s0) (bsLi s0) gi istice wt
        glcSz = gsiGlcSize surf
        maxGlc = gcMaxpatchGlc cfg

        (col1, pch1, ci1, pi1, nAdded) =
          if glcSz > 0
          then foldl addGlc (bsCol s0, bsPch s0, bsCi s0, bsPi s0, 0 :: Int)
                     [1 .. min maxGlc glcSz]
          else (bsCol s0, bsPch s0, bsCi s0, bsPi s0, 0)
        addGlc (colAcc, pchAcc, ciAcc, piAcc, n) m =
          let glcWt = getWtGlcMec surf gi m
          in  if glcWt > 0.0
              then let (colAcc', ciAcc') = addColumn colAcc lun1 ciAcc li1
                                                     (iceClassToColItype m) glcWt False
                       (pchAcc', piAcc') = addPatch pchAcc colAcc' lun1 piAcc ciAcc'
                                                    noveg 1.0 0
                   in  (colAcc', pchAcc', ciAcc', piAcc', n + 1)
              else (colAcc, pchAcc, ciAcc, piAcc, n)

        (colF, pchF, ciF, piF) =
          if nAdded == 0
          then let (col2, ci2) = addColumn col1 lun1 ci1 li1
                                           (iceClassToColItype 1) 1.0 False
                   (pch2, pi2) = addPatch pch1 col2 lun1 pi1 ci2 noveg 1.0 0
               in  (col2, pch2, ci2, pi2)
          else (col1, pch1, ci1, pi1)
    in  s0 { bsLi = li1, bsCi = ciF, bsPi = piF
            , bsLun = lun1, bsCol = colF, bsPch = pchF }
  where
    wt = getWtLunit surf gi istice
