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
    -- * GPP/MR computation
  , GPPMRInput(..)
  , GPPMROutput(..)
  , calcGppMrAvailC
    -- * Allometric allocation
  , AllocInput(..)
  , AllocOutput(..)
  , calcAllocation
    -- * N demand
  , calcNDemand
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

-- =========================================================================
-- Allometric Allocation
-- =========================================================================

data AllocInput = AllocInput
  { ali_availc        :: !Double  -- ^ available C for allocation (gC/m2/s)
  , ali_ivt           :: !Int     -- ^ PFT index
  , ali_woody         :: !Double  -- ^ 1.0 for woody PFTs
  , ali_froot_leaf    :: !Double  -- ^ fine root to leaf ratio
  , ali_croot_stem    :: !Double  -- ^ coarse root to stem ratio
  , ali_stem_leaf     :: !Double  -- ^ stem to leaf ratio
  , ali_flivewd       :: !Double  -- ^ fraction of wood that is live
  , ali_leafcn        :: !Double  -- ^ leaf C:N ratio
  , ali_frootcn       :: !Double  -- ^ fine root C:N
  , ali_livewdcn      :: !Double  -- ^ live wood C:N
  , ali_deadwdcn      :: !Double  -- ^ dead wood C:N
  , ali_grperc        :: !Double  -- ^ growth respiration fraction
  , ali_downreg       :: !Double  -- ^ N downregulation factor [0,1]
  } deriving (Show)

data AllocOutput = AllocOutput
  { alo_cpool_to_leafc           :: !Double
  , alo_cpool_to_frootc          :: !Double
  , alo_cpool_to_livestemc       :: !Double
  , alo_cpool_to_deadstemc       :: !Double
  , alo_cpool_to_livecrootc      :: !Double
  , alo_cpool_to_deadcrootc      :: !Double
  , alo_cpool_leaf_gr            :: !Double  -- ^ growth resp for leaf
  , alo_cpool_froot_gr           :: !Double
  , alo_cpool_livestem_gr        :: !Double
  , alo_cpool_deadstem_gr        :: !Double
  , alo_cpool_livecroot_gr       :: !Double
  , alo_cpool_deadcroot_gr       :: !Double
  , alo_plant_ndemand            :: !Double  -- ^ total N demand (gN/m2/s)
  } deriving (Show)

-- | Calculate allometric allocation of available C to tissue pools.
-- Partitions C according to fixed allometric ratios (leaf:froot:stem:croot).
calcAllocation :: AllocInput -> AllocOutput
calcAllocation !inp =
  let !availc = ali_availc inp * ali_downreg inp
      !grperc = ali_grperc inp
      !isWoody = ali_woody inp > 0.5

      -- Allometric fractions
      !f_leaf = 1.0
      !f_froot = ali_froot_leaf inp
      !f_stem = if isWoody then ali_stem_leaf inp else 0.0
      !f_croot = if isWoody then ali_croot_stem inp * f_stem else 0.0
      !total_allom = f_leaf + f_froot + f_stem + f_croot

      -- Fraction of available C going to each pool (after growth resp)
      !c_for_growth = availc / (1.0 + grperc)

      !aleaf = if total_allom > 0.0 then c_for_growth * f_leaf / total_allom else 0.0
      !afroot = if total_allom > 0.0 then c_for_growth * f_froot / total_allom else 0.0
      !astem = if total_allom > 0.0 then c_for_growth * f_stem / total_allom else 0.0
      !acroot = if total_allom > 0.0 then c_for_growth * f_croot / total_allom else 0.0

      -- Live/dead wood partitioning
      !flive = ali_flivewd inp
      !livestem = astem * flive
      !deadstem = astem * (1.0 - flive)
      !livecroot = acroot * flive
      !deadcroot = acroot * (1.0 - flive)

      -- Growth respiration per pool
      !gr_leaf = aleaf * grperc
      !gr_froot = afroot * grperc
      !gr_livestem = livestem * grperc
      !gr_deadstem = deadstem * grperc
      !gr_livecroot = livecroot * grperc
      !gr_deadcroot = deadcroot * grperc

      -- N demand
      !ndem = aleaf / ali_leafcn inp
            + afroot / ali_frootcn inp
            + (if isWoody
               then livestem / ali_livewdcn inp + deadstem / ali_deadwdcn inp
                  + livecroot / ali_livewdcn inp + deadcroot / ali_deadwdcn inp
               else 0.0)

  in AllocOutput
     { alo_cpool_to_leafc = aleaf
     , alo_cpool_to_frootc = afroot
     , alo_cpool_to_livestemc = livestem
     , alo_cpool_to_deadstemc = deadstem
     , alo_cpool_to_livecrootc = livecroot
     , alo_cpool_to_deadcrootc = deadcroot
     , alo_cpool_leaf_gr = gr_leaf
     , alo_cpool_froot_gr = gr_froot
     , alo_cpool_livestem_gr = gr_livestem
     , alo_cpool_deadstem_gr = gr_deadstem
     , alo_cpool_livecroot_gr = gr_livecroot
     , alo_cpool_deadcroot_gr = gr_deadcroot
     , alo_plant_ndemand = ndem
     }

-- | Calculate plant N demand from allocation fluxes.
calcNDemand :: AllocOutput -> Double -> Double
calcNDemand !alloc !dt = alo_plant_ndemand alloc * dt
