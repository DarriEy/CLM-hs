{-# LANGUAGE BangPatterns #-}
-- | Precision control: truncate tiny C/N pool values for numerical stability.
-- Fortran: SoilBiogeochemPrecisionControlMod.F90
-- Julia:   src/biogeochem/decomp_precision_control.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompPrecisionControl
  ( -- * Constants
    ccritDefault
  , ncritDefault
    -- * Main computation
  , PrecisionControlInput(..)
  , PrecisionControlOutput(..)
  , soilBGCPrecisionControl
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Critical carbon threshold (gC/m3).
ccritDefault :: Double
ccritDefault = 1.0e-8

-- | Critical nitrogen threshold (gN/m3).
ncritDefault :: Double
ncritDefault = 1.0e-8

-- | Input to precision control.
data PrecisionControlInput = PrecisionControlInput
  { pci_nc               :: !Int
  , pci_nlevdecomp       :: !Int
  , pci_ndecomp_pools    :: !Int
  , pci_mask             :: !(VU.Vector Bool)
  , pci_decomp_cpools_vr :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , pci_decomp_npools_vr :: !(VU.Vector Double)
  , pci_ctrunc_vr        :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , pci_ntrunc_vr        :: !(VU.Vector Double)
  , pci_ccrit            :: !Double
  , pci_ncrit            :: !Double
  , pci_use_nitrif_denitrif :: !Bool
  , pci_smin_no3_vr      :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , pci_smin_nh4_vr      :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of precision control.
data PrecisionControlOutput = PrecisionControlOutput
  { pco_decomp_cpools_vr :: !(VU.Vector Double)
  , pco_decomp_npools_vr :: !(VU.Vector Double)
  , pco_ctrunc_vr        :: !(VU.Vector Double)
  , pco_ntrunc_vr        :: !(VU.Vector Double)
  , pco_smin_no3_vr      :: !(VU.Vector Double)
  , pco_smin_nh4_vr      :: !(VU.Vector Double)
  } deriving (Show)

-- | 3D index helper
idx3 :: Int -> Int -> Int -> Int -> Int -> Int
idx3 nc nlev c j k = c + nc * (j + nlev * k)
{-# INLINE idx3 #-}

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Apply precision control to decomposition pools.
soilBGCPrecisionControl :: PrecisionControlInput -> PrecisionControlOutput
soilBGCPrecisionControl inp =
  let !nc     = pci_nc inp
      !nlev   = pci_nlevdecomp inp
      !npools = pci_ndecomp_pools inp
      mask    = pci_mask inp
      ccrit   = pci_ccrit inp
      ncrit   = pci_ncrit inp

      -- Process each (c, j) pair: accumulate truncation, zero small pools
      -- Build list of updates
      updates = [ (c, j)
                | c <- [0..nc-1], mask VU.! c
                , j <- [0..nlev-1] ]

      -- For each (c,j), compute truncation sums
      ctruncDelta = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else sum [ let cv = pci_decomp_cpools_vr inp VU.! idx3 nc nlev c j k
                      in if abs cv < ccrit then cv else 0.0
                    | k <- [0..npools-1] ]

      ntruncDelta = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else sum [ let cv = pci_decomp_cpools_vr inp VU.! idx3 nc nlev c j k
                          nv = pci_decomp_npools_vr inp VU.! idx3 nc nlev c j k
                      in if abs cv < ccrit then nv else 0.0
                    | k <- [0..npools-1] ]

      -- Zero out small pools
      cpools_out = VU.generate (nc * nlev * npools) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            cv = pci_decomp_cpools_vr inp VU.! ix
        in if mask VU.! c && abs cv < ccrit then 0.0 else cv

      npools_out = VU.generate (nc * nlev * npools) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            cv = pci_decomp_cpools_vr inp VU.! ix
            nv = pci_decomp_npools_vr inp VU.! ix
        in if mask VU.! c && abs cv < ccrit then 0.0 else nv

      -- Update truncation sinks
      ctrunc_out = VU.generate (nc * nlev) $ \ix ->
        pci_ctrunc_vr inp VU.! ix + ctruncDelta VU.! ix

      ntrunc_out = VU.generate (nc * nlev) $ \ix ->
        pci_ntrunc_vr inp VU.! ix + ntruncDelta VU.! ix

      -- Mineral N stability
      ncritNit = ncrit / 1.0e4
      no3_out = if pci_use_nitrif_denitrif inp
        then VU.generate (nc * nlev) $ \ix ->
          let j = ix `div` nc; c = ix `mod` nc
              v = pci_smin_no3_vr inp VU.! ix
          in if mask VU.! c && abs v < ncritNit && v < 0.0 then 0.0 else v
        else pci_smin_no3_vr inp

      nh4_out = if pci_use_nitrif_denitrif inp
        then VU.generate (nc * nlev) $ \ix ->
          let j = ix `div` nc; c = ix `mod` nc
              v = pci_smin_nh4_vr inp VU.! ix
          in if mask VU.! c && abs v < ncritNit && v < 0.0 then 0.0 else v
        else pci_smin_nh4_vr inp

  in PrecisionControlOutput
    { pco_decomp_cpools_vr = cpools_out
    , pco_decomp_npools_vr = npools_out
    , pco_ctrunc_vr = ctrunc_out
    , pco_ntrunc_vr = ntrunc_out
    , pco_smin_no3_vr = no3_out
    , pco_smin_nh4_vr = nh4_out
    }
