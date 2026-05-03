{-# LANGUAGE BangPatterns #-}
-- | Potential decomposition rates and total immobilization demand.
-- Fortran: SoilBiogeochemPotentialMod.F90
-- Julia:   src/biogeochem/decomp_potential.jl
--
-- All functions are pure.
module CLM.BioGeoChem.DecompPotential
  ( -- * Constants
    iAtm
    -- * Data types
  , DecompPotentialParams(..)
  , defaultDecompPotentialParams
    -- * Main computation
  , PotentialInput(..)
  , PotentialOutput(..)
  , soilBGCPotential
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Atmosphere pool index (receiver_pool == 0 means "to atmosphere")
iAtm :: Int
iAtm = 0

-- | Potential decomposition parameters.
data DecompPotentialParams = DecompPotentialParams
  { dpp_dnp :: !Double  -- ^ denitrification proportion
  } deriving (Show, Eq)

defaultDecompPotentialParams :: DecompPotentialParams
defaultDecompPotentialParams = DecompPotentialParams { dpp_dnp = 0.01 }

-- | Input to potential decomposition.
data PotentialInput = PotentialInput
  { pi_nc               :: !Int
  , pi_nlevdecomp       :: !Int
  , pi_ndecomp_pools    :: !Int
  , pi_ndecomp_cascade_transitions :: !Int
  , pi_mask             :: !(VU.Vector Bool)
  , pi_cascade_donor_pool    :: !(VU.Vector Int)
  , pi_cascade_receiver_pool :: !(VU.Vector Int)
  , pi_floating_cn_ratio     :: !(VU.Vector Bool)
  , pi_initial_cn_ratio      :: !(VU.Vector Double)
  , pi_decomp_cpools_vr :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , pi_decomp_npools_vr :: !(VU.Vector Double)
  , pi_decomp_k         :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , pi_rf_decomp        :: !(VU.Vector Double)  -- ^ (nc * nlev * ntrans)
  , pi_pathfrac         :: !(VU.Vector Double)
  } deriving (Show)

-- | Output of potential decomposition.
data PotentialOutput = PotentialOutput
  { po_cn_decomp_pools     :: !(VU.Vector Double)  -- ^ (nc * nlev * npools)
  , po_p_decomp_cpool_loss :: !(VU.Vector Double)  -- ^ (nc * nlev * ntrans)
  , po_pmnf_decomp_cascade :: !(VU.Vector Double)  -- ^ (nc * nlev * ntrans)
  , po_potential_immob_vr  :: !(VU.Vector Double)  -- ^ (nc * nlev)
  , po_gross_nmin_vr       :: !(VU.Vector Double)
  , po_phr_vr              :: !(VU.Vector Double)
  } deriving (Show)

-- | 3D index helper
idx3 :: Int -> Int -> Int -> Int -> Int -> Int
idx3 nc nlev c j k = c + nc * (j + nlev * k)
{-# INLINE idx3 #-}

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Calculate potential decomposition rates.
soilBGCPotential :: PotentialInput -> PotentialOutput
soilBGCPotential inp =
  let !nc     = pi_nc inp
      !nlev   = pi_nlevdecomp inp
      !npools = pi_ndecomp_pools inp
      !ntrans = pi_ndecomp_cascade_transitions inp
      mask    = pi_mask inp
      donor   = pi_cascade_donor_pool inp
      recv    = pi_cascade_receiver_pool inp
      floating = pi_floating_cn_ratio inp
      initcn  = pi_initial_cn_ratio inp
      cpools  = pi_decomp_cpools_vr inp
      npoolsv = pi_decomp_npools_vr inp
      dk      = pi_decomp_k inp
      rf      = pi_rf_decomp inp
      pf      = pi_pathfrac inp

      -- C:N ratios
      cnPools = VU.generate (nc * nlev * npools) $ \ix ->
        let l = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
        in if not (mask VU.! c) then 0.0
           else if floating VU.! l
                then let nv = npoolsv VU.! idx3 nc nlev c j l
                     in if nv > 0.0
                        then cpools VU.! idx3 nc nlev c j l / nv
                        else 0.0
                else initcn VU.! l

      -- Potential C pool loss
      pCLoss = VU.generate (nc * nlev * ntrans) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            dp = donor VU.! k
            cpv = cpools VU.! idx3 nc nlev c j dp
            dkv = dk VU.! idx3 nc nlev c j dp
        in if not (mask VU.! c) then 0.0
           else if cpv > 0.0 && dkv > 0.0
                then cpv * dkv * (pf VU.! ix)
                else 0.0

      -- Potential mineral N flux
      pmnf = VU.generate (nc * nlev * ntrans) $ \ix ->
        let k = ix `div` (nc * nlev)
            rem1 = ix `mod` (nc * nlev)
            j = rem1 `div` nc; c = rem1 `mod` nc
            dp = donor VU.! k
            rp = recv VU.! k
        in if not (mask VU.! c) then 0.0
           else if cpools VU.! idx3 nc nlev c j dp <= 0.0 then 0.0
                else if dk VU.! idx3 nc nlev c j dp <= 0.0 then 0.0
                else if not (floating VU.! rp) || rp == iAtm
                     then if rp /= iAtm
                          then let ratio = if npoolsv VU.! idx3 nc nlev c j dp > 0.0
                                           then cnPools VU.! idx3 nc nlev c j rp /
                                                cnPools VU.! idx3 nc nlev c j dp
                                           else 0.0
                               in (pCLoss VU.! ix) *
                                  (1.0 - rf VU.! ix - ratio) /
                                  cnPools VU.! idx3 nc nlev c j rp
                          else negate (pCLoss VU.! ix) /
                               cnPools VU.! idx3 nc nlev c j dp
                     else 0.0  -- CWD or floating receiver

      -- Potential immobilization (sum of positive pmnf)
      potImmob = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else sum [ max 0.0 (pmnf VU.! idx3 nc nlev c j k) | k <- [0..ntrans-1] ]

      -- Gross mineralization (sum of negative pmnf, negated)
      grossNmin = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else sum [ negate (min 0.0 (pmnf VU.! idx3 nc nlev c j k)) | k <- [0..ntrans-1] ]

      -- Potential HR
      phr = VU.generate (nc * nlev) $ \ix ->
        let j = ix `div` nc; c = ix `mod` nc
        in if not (mask VU.! c) then 0.0
           else sum [ rf VU.! idx3 nc nlev c j k * pCLoss VU.! idx3 nc nlev c j k
                    | k <- [0..ntrans-1] ]

  in PotentialOutput
    { po_cn_decomp_pools     = cnPools
    , po_p_decomp_cpool_loss = pCLoss
    , po_pmnf_decomp_cascade = pmnf
    , po_potential_immob_vr  = potImmob
    , po_gross_nmin_vr       = grossNmin
    , po_phr_vr              = phr
    }
