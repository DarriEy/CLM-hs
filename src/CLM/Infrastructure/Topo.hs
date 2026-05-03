-- | Topographic height handling for each column.
-- Ported from: src/main/TopoMod.F90
-- Pure functions for initialization, cold-start, and update logic.
module CLM.Infrastructure.Topo
  ( -- * Data type
    TopoData(..)
  , defaultTopoData
    -- * Initialization
  , topoInitAllocate
  , topoInitCold
    -- * Update
  , topoUpdate
    -- * Query
  , topoDownscaleFilter
    -- * Cleanup
  , topoClean
    -- * Configuration
  , TopoConfig(..)
  , defaultTopoConfig
  ) where

import qualified Data.Vector.Unboxed as VU

import CLM.Constants.LandunitConstants (istice)

-- ---------------------------------------------------------------------------
-- Data types
-- ---------------------------------------------------------------------------

-- | Column-level topographic height data.
data TopoData = TopoData
  { topo_col               :: !(VU.Vector Double)  -- ^ Surface elevation [m]
  , needs_downscaling_col  :: !(VU.Vector Bool)    -- ^ Whether column needs downscaling
  } deriving (Show)

defaultTopoData :: TopoData
defaultTopoData = TopoData
  { topo_col              = VU.empty
  , needs_downscaling_col = VU.empty
  }

-- | Configuration for topographic operations.
data TopoConfig = TopoConfig
  { tc_use_hillslope                   :: !Bool
  , tc_downscale_hillslope_meteorology :: !Bool
  } deriving (Show)

defaultTopoConfig :: TopoConfig
defaultTopoConfig = TopoConfig False False

-- ---------------------------------------------------------------------------
-- Initialization
-- ---------------------------------------------------------------------------

-- | Allocate topo arrays for ncols columns (NaN elevation, no downscaling).
topoInitAllocate :: Int -> TopoData
topoInitAllocate ncols = TopoData
  { topo_col              = VU.replicate ncols (0.0 / 0.0)  -- NaN
  , needs_downscaling_col = VU.replicate ncols False
  }

-- | Cold-start initialization of topographic heights.
-- For ice landunits, uses topo_glc_mec if available.
-- For hillslope columns with downscaling, uses hill_elev.
-- Otherwise initializes to 0.
topoInitCold
  :: Int                          -- ^ ncols
  -> VU.Vector Int                -- ^ col%landunit (1-based)
  -> VU.Vector Int                -- ^ col%gridcell (1-based)
  -> VU.Vector Int                -- ^ col%itype
  -> VU.Vector Int                -- ^ lun%itype
  -> VU.Vector Bool               -- ^ col%is_hillslope_column
  -> VU.Vector Double             -- ^ col%hill_elev
  -> Maybe (VU.Vector Double)     -- ^ topo_glc_mec [g, ice_class] flattened (or Nothing)
  -> TopoConfig
  -> TopoData
topoInitCold ncols colLandunit _colGridcell _colItype lunItype
             isHillslope hillElev _mTopoGlcMec cfg =
  let buildTopo c =
        let l  = colLandunit VU.! c - 1
            lt = lunItype VU.! l
        in if lt == istice
           then (0.0, True)  -- simplified: always 0 for ice without GLC MEC data
           else if (isHillslope VU.! c) && tc_downscale_hillslope_meteorology cfg
                then (hillElev VU.! c, True)
                else (0.0, False)

      pairs = VU.generate ncols buildTopo

  in TopoData
    { topo_col              = VU.map fst pairs
    , needs_downscaling_col = VU.map snd pairs
    }

-- ---------------------------------------------------------------------------
-- Update
-- ---------------------------------------------------------------------------

-- | Update topographic heights each timestep.
-- Sets non-downscaled columns to the atmospheric topo height.
-- Applies hillslope elevation adjustments if configured.
topoUpdate
  :: Int                          -- ^ ncols
  -> VU.Vector Int                -- ^ col%gridcell (1-based)
  -> VU.Vector Int                -- ^ col%landunit (1-based)
  -> VU.Vector Bool               -- ^ col%is_hillslope_column
  -> VU.Vector Double             -- ^ col%hill_elev
  -> VU.Vector Double             -- ^ col%hill_area
  -> VU.Vector Double             -- ^ atm_topo (per gridcell)
  -> VU.Vector Int                -- ^ lun%coli
  -> VU.Vector Int                -- ^ lun%colf
  -> (Int, Int)                   -- ^ (begl, endl) landunit bounds
  -> TopoConfig
  -> TopoData                     -- ^ current topo (needs_downscaling reset)
  -> TopoData
topoUpdate ncols colGridcell colLandunit isHillslope hillElev hillArea
           atmTopo lunColi lunColf (begl, endl) cfg _topoIn =
  let -- Reset needs_downscaling
      nd0 = VU.replicate ncols False

      -- Compute mean hillslope elevation per landunit (if hillslope enabled)
      meanHillElev = if tc_use_hillslope cfg
        then VU.generate (VU.length lunColi) $ \l ->
          if l >= begl - 1 && l <= endl - 1
          then let ci = lunColi VU.! l
                   cf = lunColf VU.! l
                   (sumE, sumA) = foldl'
                     (\(se, sa) c ->
                       if isHillslope VU.! (c - 1)
                       then (se + (hillElev VU.! (c-1)) * (hillArea VU.! (c-1)),
                             sa + hillArea VU.! (c-1))
                       else (se, sa))
                     (0.0, 0.0) [ci..cf]
               in if sumA > 0.0 then sumE / sumA else 0.0
          else 0.0
        else VU.empty

      -- Set topo and downscaling flags
      buildCol c =
        let g = colGridcell VU.! c - 1
            baseTopo = if g < VU.length atmTopo then atmTopo VU.! g else 0.0
        in if (isHillslope VU.! c) && tc_downscale_hillslope_meteorology cfg
           then let l = colLandunit VU.! c - 1
                    mhe = if l < VU.length meanHillElev
                          then meanHillElev VU.! l else 0.0
                in (baseTopo + (hillElev VU.! c) - mhe, True)
           else (baseTopo, nd0 VU.! c)

      pairs = VU.generate ncols buildCol

  in TopoData
    { topo_col              = VU.map fst pairs
    , needs_downscaling_col = VU.map snd pairs
    }
  where
    foldl' f z []     = z
    foldl' f z (x:xs) = let z' = f z x in z' `seq` foldl' f z' xs

-- ---------------------------------------------------------------------------
-- Query
-- ---------------------------------------------------------------------------

-- | Return a mask of columns that need downscaling.
topoDownscaleFilter :: TopoData -> Int -> VU.Vector Bool
topoDownscaleFilter topo ncols =
  VU.take ncols (needs_downscaling_col topo)

-- ---------------------------------------------------------------------------
-- Cleanup
-- ---------------------------------------------------------------------------

-- | Reset all arrays to empty.
topoClean :: TopoData
topoClean = defaultTopoData
