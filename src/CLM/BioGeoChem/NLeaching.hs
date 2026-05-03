{-# LANGUAGE BangPatterns #-}
-- | Mineral nitrogen leaching: soluble mineral N loss via subsurface
-- drainage and (with nitrif_denitrif) NO3 loss via surface runoff.
-- Fortran: SoilBiogeochemNLeachingMod.F90
-- Julia:   src/biogeochem/n_leaching.jl
--
-- All functions are pure.
module CLM.BioGeoChem.NLeaching
  ( -- * Data types
    NLeachingParams(..)
  , defaultNLeachingParams
  , NLeachingInput(..)
  , NLeachingOutput(..)
    -- * Main computation
  , nLeaching
  ) where

import qualified Data.Vector.Unboxed as VU

-- | N-leaching parameters.
data NLeachingParams = NLeachingParams
  { nlp_sf     :: !Double  -- ^ soluble fraction of mineral N (unitless)
  , nlp_sf_no3 :: !Double  -- ^ soluble fraction of NO3 (unitless)
  } deriving (Show, Eq)

defaultNLeachingParams :: NLeachingParams
defaultNLeachingParams = NLeachingParams
  { nlp_sf     = 0.1
  , nlp_sf_no3 = 1.0
  }

-- | Input to N leaching calculation.
data NLeachingInput = NLeachingInput
  { nli_nc               :: !Int
  , nli_nlevdecomp       :: !Int
  , nli_nlevsoi          :: !Int
  , nli_mask             :: !(VU.Vector Bool)
  , nli_params           :: !NLeachingParams
  , nli_dt               :: !Double
  , nli_use_nitrif_denitrif :: !Bool
  -- Mineral N state (nc * nlev)
  , nli_sminn_vr         :: !(VU.Vector Double)    -- ^ total mineral N (non-nitrif mode)
  , nli_smin_no3_vr      :: !(VU.Vector Double)    -- ^ NO3 pool (nitrif mode)
  -- Water state (nc * nlevsoi)
  , nli_h2osoi_liq       :: !(VU.Vector Double)
  -- Water flux (nc)
  , nli_qflx_drain       :: !(VU.Vector Double)
  , nli_qflx_surf        :: !(VU.Vector Double)
  -- Column geometry (nc * nlev)
  , nli_col_dz           :: !(VU.Vector Double)
  -- Soil interface depths (nlevsoi+1), zisoi[0]=0 (surface)
  , nli_zisoi            :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of N leaching calculation.
data NLeachingOutput = NLeachingOutput
  { nlo_sminn_leached_vr     :: !(VU.Vector Double)  -- ^ leached sminn (nc * nlev) [non-nitrif mode]
  , nlo_smin_no3_leached_vr  :: !(VU.Vector Double)  -- ^ leached NO3 (nc * nlev) [nitrif mode]
  , nlo_smin_no3_runoff_vr   :: !(VU.Vector Double)  -- ^ NO3 lost to runoff (nc * nlev) [nitrif mode]
  } deriving (Show)

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Depth over which runoff mixes with soil water for N loss to runoff (m).
depthRunoffNloss :: Double
depthRunoffNloss = 0.05

-- | Calculate N leaching rates.
nLeaching :: NLeachingInput -> NLeachingOutput
nLeaching inp =
  let !nc   = nli_nc inp
      !nlev = nli_nlevdecomp inp
      !nlevsoi = nli_nlevsoi inp
      mask  = nli_mask inp
      params = nli_params inp
      dt    = nli_dt inp
      useND = nli_use_nitrif_denitrif inp
      zisoi = nli_zisoi inp

      -- Total soil water per column
      tot_water = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ nli_h2osoi_liq inp VU.! idx2 nc c j | j <- [0..nlevsoi-1] ]

      -- Surface water (within depthRunoffNloss depth)
      surface_water = VU.generate nc $ \c ->
        if not (mask VU.! c) then 0.0
        else sum [ let zi_bot = zisoi VU.! (j + 1)  -- bottom of layer j
                       zi_top = zisoi VU.! j         -- top of layer j
                       liq = nli_h2osoi_liq inp VU.! idx2 nc c j
                   in if zi_bot <= depthRunoffNloss
                      then liq
                      else if zi_top < depthRunoffNloss
                           then liq * ((depthRunoffNloss - zi_top) / (nli_col_dz inp VU.! idx2 nc c j))
                           else 0.0
                 | j <- [0..nlevsoi-1] ]

      -- Non-nitrif-denitrif pathway: sminn leaching
      sminn_leached = if not useND
        then VU.generate (nc * nlev) $ \ix ->
          let j = ix `div` nc; c = ix `mod` nc
              sf = nlp_sf params
          in if not (mask VU.! c) || tot_water VU.! c <= 0.0 then 0.0
             else let liq = nli_h2osoi_liq inp VU.! idx2 nc c j
                      disn_conc = if liq > 0.0
                                  then (sf * nli_sminn_vr inp VU.! idx2 nc c j * nli_col_dz inp VU.! idx2 nc c j) / liq
                                  else 0.0
                      drain = nli_qflx_drain inp VU.! c
                      dz = nli_col_dz inp VU.! idx2 nc c j
                      raw = disn_conc * drain * liq / (tot_water VU.! c * dz)
                      capped = min raw (sf * nli_sminn_vr inp VU.! idx2 nc c j / dt)
                  in max capped 0.0
        else VU.replicate (nc * nlev) 0.0

      -- Nitrif-denitrif pathway: NO3 leaching and runoff
      no3_leached = if useND
        then VU.generate (nc * nlev) $ \ix ->
          let j = ix `div` nc; c = ix `mod` nc
              sf_no3 = nlp_sf_no3 params
          in if not (mask VU.! c) || tot_water VU.! c <= 0.0 then 0.0
             else let liq = nli_h2osoi_liq inp VU.! idx2 nc c j
                      disn_conc = if liq > 0.0
                                  then (sf_no3 * nli_smin_no3_vr inp VU.! idx2 nc c j * nli_col_dz inp VU.! idx2 nc c j) / liq
                                  else 0.0
                      drain = nli_qflx_drain inp VU.! c
                      dz = nli_col_dz inp VU.! idx2 nc c j
                      raw = disn_conc * drain * liq / (tot_water VU.! c * dz)
                      capped1 = min raw (nli_smin_no3_vr inp VU.! idx2 nc c j / dt)
                      capped2 = min capped1 (sf_no3 * nli_smin_no3_vr inp VU.! idx2 nc c j / dt)
                  in max capped2 0.0
        else VU.replicate (nc * nlev) 0.0

      no3_runoff = if useND
        then VU.generate (nc * nlev) $ \ix ->
          let j = ix `div` nc; c = ix `mod` nc
              sf_no3 = nlp_sf_no3 params
          in if not (mask VU.! c) || surface_water VU.! c <= 0.0 then 0.0
             else let liq = nli_h2osoi_liq inp VU.! idx2 nc c j
                      disn_conc = if liq > 0.0
                                  then (sf_no3 * nli_smin_no3_vr inp VU.! idx2 nc c j * nli_col_dz inp VU.! idx2 nc c j) / liq
                                  else 0.0
                      zi_bot = zisoi VU.! (j + 1)
                      zi_top = zisoi VU.! j
                      qsurf = nli_qflx_surf inp VU.! c
                      dz = nli_col_dz inp VU.! idx2 nc c j
                      sw = surface_water VU.! c
                      leached_j = no3_leached VU.! idx2 nc c j

                      raw = if zi_bot <= depthRunoffNloss
                            then disn_conc * qsurf * liq / (sw * dz)
                            else if zi_top < depthRunoffNloss
                                 then disn_conc * qsurf * liq * ((depthRunoffNloss - zi_top) / dz) / (sw * (depthRunoffNloss - zi_top))
                                 else 0.0
                      capped = min raw (nli_smin_no3_vr inp VU.! idx2 nc c j / dt - leached_j)
                  in max capped 0.0
        else VU.replicate (nc * nlev) 0.0

  in NLeachingOutput
    { nlo_sminn_leached_vr    = sminn_leached
    , nlo_smin_no3_leached_vr = no3_leached
    , nlo_smin_no3_runoff_vr  = no3_runoff
    }
