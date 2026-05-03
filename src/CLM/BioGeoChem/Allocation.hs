{-# LANGUAGE BangPatterns #-}
-- | CN allocation: GPP, maintenance respiration, available C, crop allocation.
-- Fortran: CNAllocationMod.F90
-- Julia:   src/biogeochem/allocation.jl
--
-- All functions are pure.
module CLM.BioGeoChem.Allocation
  ( -- * Data types
    AllocationParams(..)
  , defaultAllocationParams
  , PftConAllocation(..)
    -- * Main computation
  , GPPMRInput(..)
  , GPPMROutput(..)
  , calcGppMrAvailC
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Allocation parameters.
data AllocationParams = AllocationParams
  { ap_dayscrecover :: !Double  -- ^ number of days to recover negative cpool
  } deriving (Show, Eq)

defaultAllocationParams :: AllocationParams
defaultAllocationParams = AllocationParams { ap_dayscrecover = 30.0 }

-- | PFT constants for allocation.
data PftConAllocation = PftConAllocation
  { pfa_woody      :: !(VU.Vector Double)
  , pfa_froot_leaf :: !(VU.Vector Double)
  , pfa_croot_stem :: !(VU.Vector Double)
  , pfa_stem_leaf  :: !(VU.Vector Double)
  , pfa_flivewd    :: !(VU.Vector Double)
  , pfa_leafcn     :: !(VU.Vector Double)
  , pfa_frootcn    :: !(VU.Vector Double)
  , pfa_livewdcn   :: !(VU.Vector Double)
  , pfa_deadwdcn   :: !(VU.Vector Double)
  , pfa_grperc     :: !(VU.Vector Double)
  } deriving (Show)

-- | Seconds per day.
secspday :: Double
secspday = 86400.0

-- | Input for GPP/MR/availC calculation.
data GPPMRInput = GPPMRInput
  { gmi_np            :: !Int
  , gmi_mask          :: !(VU.Vector Bool)
  , gmi_ivt           :: !(VU.Vector Int)      -- ^ PFT type per patch
  , gmi_psnsun        :: !(VU.Vector Double)   -- ^ sunlit photosynthesis (umol/m2/s)
  , gmi_psnsha        :: !(VU.Vector Double)   -- ^ shaded photosynthesis
  , gmi_laisun        :: !(VU.Vector Double)
  , gmi_laisha        :: !(VU.Vector Double)
  , gmi_leaf_mr       :: !(VU.Vector Double)
  , gmi_froot_mr      :: !(VU.Vector Double)
  , gmi_livestem_mr   :: !(VU.Vector Double)
  , gmi_livecroot_mr  :: !(VU.Vector Double)
  , gmi_xsmrpool      :: !(VU.Vector Double)
  , gmi_pftcon        :: !PftConAllocation
  , gmi_params        :: !AllocationParams
  } deriving (Show)

-- | Output of GPP/MR/availC calculation.
data GPPMROutput = GPPMROutput
  { gmo_gpp_before_downreg :: !(VU.Vector Double)
  , gmo_availc             :: !(VU.Vector Double)
  , gmo_leaf_curmr         :: !(VU.Vector Double)
  , gmo_leaf_xsmr          :: !(VU.Vector Double)
  , gmo_froot_curmr        :: !(VU.Vector Double)
  , gmo_froot_xsmr         :: !(VU.Vector Double)
  , gmo_xsmrpool_recover   :: !(VU.Vector Double)
  } deriving (Show)

-- | Calculate GPP, MR, and available C for allocation.
calcGppMrAvailC :: GPPMRInput -> GPPMROutput
calcGppMrAvailC inp =
  let !np   = gmi_np inp
      mask  = gmi_mask inp
      ivt   = gmi_ivt inp
      pft   = gmi_pftcon inp
      params = gmi_params inp
      dayscrecover = ap_dayscrecover params

      -- Helper: compute per-patch values (avoiding 7-tuples which lack Unbox)
      perPatch :: (Int -> Int -> Double) -> VU.Vector Double
      perPatch f = VU.generate np $ \p ->
        if not (mask VU.! p) then 0.0 else f p (ivt VU.! p + 1)

      computePatch :: Int -> Int
                   -> (Double, Double, Double, Double, Double, Double, Double)
      computePatch p iv =
        let psnsunC = gmi_psnsun inp VU.! p * gmi_laisun inp VU.! p * 12.011e-6
            pnshaC  = gmi_psnsha inp VU.! p * gmi_laisha inp VU.! p * 12.011e-6
            gpp     = psnsunC + pnshaC
            mr      = gmi_leaf_mr inp VU.! p + gmi_froot_mr inp VU.! p
                    + (if pfa_woody pft VU.! iv == 1.0
                       then gmi_livestem_mr inp VU.! p + gmi_livecroot_mr inp VU.! p
                       else 0.0)
            availc0 = gpp - mr
            curmr_ratio = if mr > 0.0 && availc0 < 0.0
                          then gpp / mr
                          else 1.0
            lcur = gmi_leaf_mr inp VU.! p * curmr_ratio
            lxs  = gmi_leaf_mr inp VU.! p - lcur
            fcur = gmi_froot_mr inp VU.! p * curmr_ratio
            fxs  = gmi_froot_mr inp VU.! p - fcur
            avail1 = max availc0 0.0
            xsmrpool = gmi_xsmrpool inp VU.! p
            (avail2, xsmr_rec) =
              if xsmrpool < 0.0
              then let xr = negate xsmrpool / (dayscrecover * secspday)
                   in if xr < avail1
                      then (avail1 - xr, xr)
                      else (0.0, avail1)
              else (avail1, 0.0)
        in (gpp, avail2, lcur, lxs, fcur, fxs, xsmr_rec)

      sel1 (a,_,_,_,_,_,_) = a
      sel2 (_,a,_,_,_,_,_) = a
      sel3 (_,_,a,_,_,_,_) = a
      sel4 (_,_,_,a,_,_,_) = a
      sel5 (_,_,_,_,a,_,_) = a
      sel6 (_,_,_,_,_,a,_) = a
      sel7 (_,_,_,_,_,_,a) = a

  in GPPMROutput
    { gmo_gpp_before_downreg = perPatch (\p iv -> sel1 (computePatch p iv))
    , gmo_availc             = perPatch (\p iv -> sel2 (computePatch p iv))
    , gmo_leaf_curmr         = perPatch (\p iv -> sel3 (computePatch p iv))
    , gmo_leaf_xsmr          = perPatch (\p iv -> sel4 (computePatch p iv))
    , gmo_froot_curmr        = perPatch (\p iv -> sel5 (computePatch p iv))
    , gmo_froot_xsmr         = perPatch (\p iv -> sel6 (computePatch p iv))
    , gmo_xsmrpool_recover   = perPatch (\p iv -> sel7 (computePatch p iv))
    }
