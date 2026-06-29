{-# LANGUAGE BangPatterns #-}
-- | Vertical mixing of decomposing C and N pools via advection-diffusion.
-- Based on algorithm in Patankar (1980).
-- Fortran: SoilBiogeochemLittVertTranspMod.F90
-- Julia:   src/biogeochem/litter_vert_transp.jl
--
-- All functions are pure.
module CLM.BioGeoChem.LitterVertTransp
  ( -- * Data types
    LitterVertTranspParams(..)
  , defaultLitterVertTranspParams
    -- * Patankar A function
  , patankarA
    -- * Main computation
  , VertTranspInput(..)
  , VertTranspOutput(..)
  , litterVertTransp
    -- * Single-pool tridiagonal transport
  , TridiagTranspInput(..)
  , tridiagVertTransp
  ) where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector         as V

-- | Parameters for vertical transport.
data LitterVertTranspParams = LitterVertTranspParams
  { lvp_som_diffus                :: !Double  -- ^ SOM diffusion (m^2/s)
  , lvp_cryoturb_diffusion_k     :: !Double  -- ^ cryoturbation diffusive constant (m^2/s)
  , lvp_max_altdepth_cryoturbation :: !Double  -- ^ max active layer thickness for cryoturbation (m)
  } deriving (Show, Eq)

defaultLitterVertTranspParams :: LitterVertTranspParams
defaultLitterVertTranspParams = LitterVertTranspParams
  { lvp_som_diffus = 0.0
  , lvp_cryoturb_diffusion_k = 0.0
  , lvp_max_altdepth_cryoturbation = 0.0
  }

-- | Patankar's "A" function (Table 5.2, pg 95).
-- Returns max(0, (1 - 0.1*|pe|)^5).
patankarA :: Double -> Double
patankarA pe = max 0.0 ((1.0 - 0.1 * abs pe) ** 5)
{-# INLINE patankarA #-}

-- | Input to vertical transport computation.
data VertTranspInput = VertTranspInput
  { vti_nc               :: !Int
  , vti_nlevdecomp       :: !Int
  , vti_ndecomp_pools    :: !Int
  , vti_mask             :: !(VU.Vector Bool)
  , vti_params           :: !LitterVertTranspParams
  , vti_dtime            :: !Double
  -- Soil geometry
  , vti_zsoi             :: !(VU.Vector Double)  -- ^ soil node depths (nlevdecomp)
  , vti_dzsoi_decomp     :: !(VU.Vector Double)  -- ^ decomp layer thicknesses (nlevdecomp)
  , vti_zisoi            :: !(VU.Vector Double)  -- ^ soil interface depths (nlevdecomp+1), zisoi[0]=0
  -- Active layer
  , vti_altmax           :: !(VU.Vector Double)  -- ^ (nc) maximum active layer depth
  , vti_altmax_lastyear  :: !(VU.Vector Double)  -- ^ (nc) max active layer depth last year
  -- Bedrock
  , vti_nbedrock         :: !(VU.Vector Int)     -- ^ (nc) number of layers above bedrock
  -- Pool configuration
  , vti_is_cwd           :: !(VU.Vector Bool)    -- ^ (ndecomp_pools) whether pool is CWD
  , vti_spinup_factor    :: !(VU.Vector Double)  -- ^ (ndecomp_pools) spinup factor
  , vti_spinup_state     :: !Int
  -- Advection parameters
  , vti_som_adv_flux     :: !Double              -- ^ SOM advective flux (m/s)
  , vti_max_depth_cryoturb :: !Double            -- ^ max depth of cryoturbation (m)
  -- State arrays: (nc * nlev * npools) flat
  , vti_decomp_cpools_vr :: !(VU.Vector Double)
  , vti_decomp_npools_vr :: !(VU.Vector Double)
  -- Source/sink arrays: (nc * nlev * npools) flat
  , vti_decomp_cpools_sourcesink :: !(VU.Vector Double)
  , vti_decomp_npools_sourcesink :: !(VU.Vector Double)
  -- Latitude for spinup correction
  , vti_latdeg           :: !(VU.Vector Double)  -- ^ (nc) gridcell latitude
  } deriving (Show)

-- | Output of vertical transport computation.
data VertTranspOutput = VertTranspOutput
  { vto_decomp_cpools_vr :: !(VU.Vector Double)  -- ^ updated C pools (nc * nlev * npools)
  , vto_decomp_npools_vr :: !(VU.Vector Double)  -- ^ updated N pools
  , vto_c_transport_tendency :: !(VU.Vector Double)  -- ^ C transport tendency (nc * nlev * npools)
  , vto_n_transport_tendency :: !(VU.Vector Double)  -- ^ N transport tendency
  -- Diffusivity/advection coefficients (nc * (nlev+1))
  , vto_som_adv_coef    :: !(VU.Vector Double)
  , vto_som_diffus_coef :: !(VU.Vector Double)
  } deriving (Show)

-- | 3D index helper
idx3 :: Int -> Int -> Int -> Int -> Int -> Int
idx3 nc nlev c j k = c + nc * (j + nlev * k)
{-# INLINE idx3 #-}

-- | 2D index helper
idx2 :: Int -> Int -> Int -> Int
idx2 nc c j = c + nc * j
{-# INLINE idx2 #-}

-- | Spinup latitude term.
getSpinupLatitudeTerm :: Double -> Double
getSpinupLatitudeTerm lat
  | abs lat >= 75.0 = 0.25
  | abs lat >= 60.0 = 0.5
  | otherwise        = 1.0

-- | Solve a tridiagonal linear system with the Thomas algorithm.
--
--   a : sub-diagonal   (a!0 is unused)
--   b : main diagonal
--   c_: super-diagonal (c_!(n-1) is unused)
--   r : right-hand side
--
-- Returns the solution vector @u@ of the same length as @b@.
--
-- This is the standard, numerically faithful Thomas elimination used by the
-- CLM @Tridiagonal@ routine: a forward sweep building modified super-diagonal
-- (@cp@) and rhs (@dp@) coefficients, followed by a back substitution.
solveTridiag :: VU.Vector Double -> VU.Vector Double -> VU.Vector Double
             -> VU.Vector Double -> VU.Vector Double
solveTridiag a b c_ r =
  let !n = VU.length b
  in if n == 0
       then VU.empty
       else
         let -- Forward sweep: cp!i, dp!i
             !cp0 = (c_ VU.! 0) / (b VU.! 0)
             !dp0 = (r  VU.! 0) / (b VU.! 0)
             go !i !cpPrev !dpPrev cpAcc dpAcc
               | i >= n    = (reverse cpAcc, reverse dpAcc)
               | otherwise =
                   let !denom = (b VU.! i) - (a VU.! i) * cpPrev
                       !cpi   = (c_ VU.! i) / denom
                       !dpi   = ((r VU.! i) - (a VU.! i) * dpPrev) / denom
                   in go (i+1) cpi dpi (cpi : cpAcc) (dpi : dpAcc)
             (cpsL, dpsL) = go 1 cp0 dp0 [cp0] [dp0]
             !cp = VU.fromList cpsL
             !dp = VU.fromList dpsL
             -- Back substitution
             !xn = dp VU.! (n-1)
             back !i !xNext acc
               | i < 0     = acc
               | otherwise = let !xi = (dp VU.! i) - (cp VU.! i) * xNext
                             in back (i-1) xi (xi : acc)
         in VU.fromList (back (n-2) xn [xn])

-- | Calculate vertical mixing of all decomposing C and N pools.
--
-- Faithful port of @SoilBiogeochemLittVertTransp@
-- (SoilBiogeochemLittVertTranspMod.F90): an advection-diffusion (Patankar 1980,
-- power-law scheme) of every decomposing C and N pool over the column, plus
-- reconciliation of the source/sink terms computed in CStateUpdate1 /
-- NStateUpdate1.
--
-- For each (tracer, pool, active column):
--
--   * non-CWD pools are transported by assembling the Patankar tridiagonal
--     system (a_tri/b_tri/c_tri/r_tri, Fortran levels @0..nlevdecomp+1@ with
--     zero-gradient top and bottom boundary rows) and solving it with the
--     Thomas algorithm ('solveTridiag');
--   * CWD pools are not transported — only their source/sink is added;
--   * any tracer that leaks below bedrock is folded back into the bottom active
--     layer, so the column total is conserved.
--
-- The transport tendency @(conc_after - conc_before - source)/dtime@ is returned
-- for the non-CWD pools (zero for CWD, which has no transport).
litterVertTransp :: VertTranspInput -> VertTranspOutput
litterVertTransp inp =
  let !nc     = vti_nc inp
      !nlev   = vti_nlevdecomp inp
      !npools = vti_ndecomp_pools inp
      mask    = vti_mask inp
      params  = vti_params inp
      dtime   = vti_dtime inp
      zsoi    = vti_zsoi inp
      dzsoi   = vti_dzsoi_decomp inp
      zisoi   = vti_zisoi inp
      altmax  = vti_altmax inp
      altmax_ly = vti_altmax_lastyear inp
      nbedrock = vti_nbedrock inp
      is_cwd  = vti_is_cwd inp
      spf     = vti_spinup_factor inp
      spstate = vti_spinup_state inp
      latdeg  = vti_latdeg inp
      som_adv = vti_som_adv_flux inp
      max_cryo = vti_max_depth_cryoturb inp
      som_diffus_val = lvp_som_diffus params
      cryo_k  = lvp_cryoturb_diffusion_k params
      max_alt_cryo = lvp_max_altdepth_cryoturbation params

      !epsilon = 1.0e-30

      -- ----------------------------------------------------------------------
      -- Diffusivity / advection coefficients per column (nc * (nlev+1)).
      -- Fortran level j = 1..nlevdecomp+1 maps to Haskell jh = j-1 = 0..nlev.
      -- @zisoi@ is 0-based with zisoi[0]=0 (surface) and zisoi[k]=bottom of
      -- layer k, so Fortran zisoi(j) = zisoi[j] = zisoi[jh+1]; the bedrock
      -- interface Fortran zisoi(nbedrock+1) = zisoi[nbr+1].
      -- ----------------------------------------------------------------------
      computeCoefs = VU.generate (nc * (nlev + 1)) $ \ix ->
        let jh = ix `div` nc
            c  = ix `mod` nc
        in if not (mask VU.! c) then (0.0, 0.0)
           else
             let !alt = max (altmax VU.! c) (altmax_ly VU.! c)
                 !nbr = nbedrock VU.! c
             in if alt <= max_alt_cryo && alt > 0.0
                  then -- cryoturbation: constant in active layer, linear to 0
                    if jh <= nbr
                      then if (zisoi VU.! (jh + 1)) < alt
                             then (cryo_k, 0.0)
                             else ( max ( cryo_k *
                                          ( 1.0 - ((zisoi VU.! (jh + 1)) - alt)
                                            / (min max_cryo (zisoi VU.! (nbr + 1)) - alt) ) )
                                        0.0
                                  , 0.0 )
                      else (0.0, 0.0)
                  else if alt > 0.0
                         then -- bioturbation: constant advection + diffusion
                           if jh <= nbr then (som_diffus_val, som_adv) else (0.0, 0.0)
                         else (0.0, 0.0)  -- fully frozen: no mixing

      som_diffus_coef = VU.map fst computeCoefs
      som_adv_coef    = VU.map snd computeCoefs

      -- Node geometry (shared across columns).
      -- zsoi_ext: length nlev+1; [0..nlev-1] = zsoi, [nlev] = zisoi[nlev]
      -- (Fortran zsoi(nlevdecomp+1) := zisoi(nlevdecomp)).
      zsoi_ext = VU.generate (nlev + 1) $ \j ->
                   if j < nlev then zsoi VU.! j else zisoi VU.! nlev
      -- dz_node(1) = zsoi(1); dz_node(j) = zsoi(j) - zsoi(j-1).
      dz_node  = VU.generate (nlev + 1) $ \j ->
                   if j == 0 then zsoi_ext VU.! 0
                             else (zsoi_ext VU.! j) - (zsoi_ext VU.! (j - 1))

      -- Fortran "A" function shorthand.
      aaa = patankarA

      -- Bedrock-leak correction: any tracer in a layer below @nbr@ (Fortran
      -- 1-based bedrock count) is moved into the bottom active layer (index
      -- nbr-1), scaled by the thickness ratio, then zeroed -- conserving the
      -- column total.
      applyBedrockLeak :: Int -> VU.Vector Double -> VU.Vector Double
      applyBedrockLeak nbr raw
        | nbr < 1 || nbr > nlev = raw
        | otherwise =
            let !target = nbr - 1
                !leak = VU.sum $ VU.generate nlev $ \lj ->
                          if lj + 1 > nbr
                            then (raw VU.! lj) * ((dzsoi VU.! lj) / (dzsoi VU.! target))
                            else 0.0
            in VU.generate nlev $ \lj ->
                 if lj + 1 > nbr        then 0.0
                 else if lj == target   then (raw VU.! lj) + leak
                 else raw VU.! lj

      -- Transport one (column c, pool k) of a single tracer.
      -- Returns (updated concentration profile, transport tendency), each nlev.
      transportColumnPool :: VU.Vector Double -> VU.Vector Double
                          -> Int -> Int
                          -> (VU.Vector Double, VU.Vector Double)
      transportColumnPool conc_in source_in c k =
        let !nbr  = nbedrock VU.! c
            !is_c = is_cwd VU.! k
            -- per-layer (0-based) accessors
            concL lj = conc_in   VU.! idx3 nc nlev c lj k
            srcL  lj = source_in VU.! idx3 nc nlev c lj k
        in if is_c
             then -- CWD: no transport, just add source (+ bedrock leak)
               let raw = VU.generate nlev (\lj -> concL lj + srcL lj)
               in (applyBedrockLeak nbr raw, VU.replicate nlev 0.0)
             else
               let -- spinup acceleration of transport (advection + diffusion)
                   !sp0 = if spstate >= 1 then spf VU.! k else 1.0
                   !spinup_term =
                       if abs (sp0 - 1.0) > 1.0e-6
                         then sp0 * getSpinupLatitudeTerm (latdeg VU.! c)
                         else sp0
                   -- diffusivity / advective flux with spinup + epsilon floor
                   advV = VU.generate (nlev + 1) $ \jh ->
                            let base = som_adv_coef VU.! idx2 nc c jh
                            in if abs base * spinup_term < epsilon
                                 then epsilon else base * spinup_term
                   diffusV = VU.generate (nlev + 1) $ \jh ->
                            let base = som_diffus_coef VU.! idx2 nc c jh
                            in if abs base * spinup_term < epsilon
                                 then epsilon else base * spinup_term
                   -- Fortran 1-based accessors (j = 1..nlev+1)
                   advF j    = advV    VU.! (j - 1)
                   diffusF j = diffusV VU.! (j - 1)
                   zsoiF j   = zsoi_ext VU.! (j - 1)
                   zisoiF j  = zisoi   VU.! j
                   dzNodeF j = dz_node VU.! (j - 1)
                   dzsoiF j  = dzsoi   VU.! (j - 1)
                   -- conc_trcr: 0 at top/bottom ghost levels, conc inside.
                   concTr j
                     | j <= 0           = 0.0
                     | j <= nlev        = concL (j - 1)
                     | otherwise        = 0.0
                   -- D/dz, F (water flux), Pe (Peclet) terms for j = 1..nlev+1.
                   -- Tuple = (d_m1_zm1, d_p1_zp1, f_m1, f_p1, pe_m1, pe_p1).
                   dfpe :: V.Vector (Double, Double, Double, Double, Double, Double)
                   dfpe = V.generate (nlev + 1) $ \jh ->
                     let j = jh + 1
                     in if j == 1
                          then
                            let wp1 = (zsoiF (j + 1) - zisoiF j) / dzNodeF (j + 1)
                                dp1 = if diffusF (j + 1) > 0.0 && diffusF j > 0.0
                                        then 1.0 / ((1.0 - wp1) / diffusF j + wp1 / diffusF (j + 1))
                                        else 0.0
                                dp1zp1 = dp1 / dzNodeF (j + 1)
                                fm1 = advF j
                                fp1 = advF (j + 1)
                            in (0.0, dp1zp1, fm1, fp1, 0.0, fp1 / dp1zp1)
                        else if j >= nbr + 1
                          then
                            let wm1 = (zisoiF (j - 1) - zsoiF (j - 1)) / dzNodeF j
                                dm1 = if diffusF j > 0.0 && diffusF (j - 1) > 0.0
                                        then 1.0 / ((1.0 - wm1) / diffusF j + wm1 / diffusF (j - 1))
                                        else 0.0
                                dm1zm1 = dm1 / dzNodeF j
                                dp1zp1 = dm1zm1
                                fm1 = advF j
                                fp1 = 0.0
                            in (dm1zm1, dp1zp1, fm1, fp1, fm1 / dm1zm1, fp1 / dp1zp1)
                        else
                            let wm1 = (zisoiF (j - 1) - zsoiF (j - 1)) / dzNodeF j
                                dm1 = if diffusF (j - 1) > 0.0 && diffusF j > 0.0
                                        then 1.0 / ((1.0 - wm1) / diffusF j + wm1 / diffusF (j - 1))
                                        else 0.0
                                wp1 = (zsoiF (j + 1) - zisoiF j) / dzNodeF (j + 1)
                                dp1 = if diffusF (j + 1) > 0.0 && diffusF j > 0.0
                                        then 1.0 / ((1.0 - wp1) / diffusF j + wp1 / diffusF (j + 1))
                                        else (1.0 - wm1) * diffusF j + wp1 * diffusF (j + 1)
                                dm1zm1 = dm1 / dzNodeF j
                                dp1zp1 = dp1 / dzNodeF (j + 1)
                                fm1 = advF j
                                fp1 = advF (j + 1)
                            in (dm1zm1, dp1zp1, fm1, fp1, fm1 / dm1zm1, fp1 / dp1zp1)
                   dM1zm1 j = let (x,_,_,_,_,_) = dfpe V.! (j - 1) in x
                   dP1zp1 j = let (_,x,_,_,_,_) = dfpe V.! (j - 1) in x
                   fM1 j    = let (_,_,x,_,_,_) = dfpe V.! (j - 1) in x
                   fP1 j    = let (_,_,_,x,_,_) = dfpe V.! (j - 1) in x
                   peM1 j   = let (_,_,_,_,x,_) = dfpe V.! (j - 1) in x
                   peP1 j   = let (_,_,_,_,_,x) = dfpe V.! (j - 1) in x
                   -- Tridiagonal coefficients, Fortran levels j = 0..nlev+1.
                   coef j
                     | j == 0 = (0.0, 1.0, -1.0, 0.0)
                     | j == 1 =
                         let ap0 = dzsoiF j / dtime
                             a = negate (dM1zm1 j * aaa (peM1 j) + max (fM1 j) 0.0)
                             cc = negate (dP1zp1 j * aaa (peP1 j) + max (negate (fP1 j)) 0.0)
                             b = negate a - cc + ap0
                             r = srcL (j - 1) * dzsoiF j / dtime
                                 + (ap0 - advF j) * concTr j
                         in (a, b, cc, r)
                     | j < nlev + 1 =
                         let ap0 = dzsoiF j / dtime
                             a = negate (dM1zm1 j * aaa (peM1 j) + max (fM1 j) 0.0)
                             cc = negate (dP1zp1 j * aaa (peP1 j) + max (negate (fP1 j)) 0.0)
                             b = negate a - cc + ap0
                             r = srcL (j - 1) * dzsoiF j / dtime + ap0 * concTr j
                         in (a, b, cc, r)
                     | otherwise = (-1.0, 1.0, 0.0, 0.0)  -- j == nlev+1
                   rows = [ coef j | j <- [0 .. nlev + 1] ]
                   aV = VU.fromList [ a  | (a,_,_,_) <- rows ]
                   bV = VU.fromList [ b  | (_,b,_,_) <- rows ]
                   cV = VU.fromList [ cc | (_,_,cc,_) <- rows ]
                   rV = VU.fromList [ r  | (_,_,_,r) <- rows ]
                   -- Solve; sol indexed by Fortran level j = 0..nlev+1.
                   sol = solveTridiag aV bV cV rV
                   rawConc = VU.generate nlev $ \lj -> sol VU.! (lj + 1)
                   concOut = applyBedrockLeak nbr rawConc
                   -- tendency = (conc_after - conc_before - source)/dtime
                   tend = VU.generate nlev $ \lj ->
                            let j = lj + 1
                                pre = negate (concTr j + srcL lj)
                            in (pre + sol VU.! j) / dtime
               in (concOut, tend)

      -- Transport a whole tracer (all pools, all columns).
      solveAll :: VU.Vector Double -> VU.Vector Double
               -> (VU.Vector Double, VU.Vector Double)
      solveAll conc_in source_in =
        let solved = V.generate (npools * nc) $ \kc ->
              let k = kc `div` nc
                  c = kc `mod` nc
              in if not (mask VU.! c)
                   then ( VU.generate nlev (\lj -> conc_in VU.! idx3 nc nlev c lj k)
                        , VU.replicate nlev 0.0 )
                   else transportColumnPool conc_in source_in c k
            outAt pick = VU.generate (nc * nlev * npools) $ \ix ->
              let k    = ix `div` (nc * nlev)
                  rem1 = ix `mod` (nc * nlev)
                  lj   = rem1 `div` nc
                  c    = rem1 `mod` nc
              in pick (solved V.! (k * nc + c)) VU.! lj
        in (outAt fst, outAt snd)

      (cpools_out, c_tend) =
        solveAll (vti_decomp_cpools_vr inp) (vti_decomp_cpools_sourcesink inp)
      (npools_out, n_tend) =
        solveAll (vti_decomp_npools_vr inp) (vti_decomp_npools_sourcesink inp)

  in VertTranspOutput
    { vto_decomp_cpools_vr = cpools_out
    , vto_decomp_npools_vr = npools_out
    , vto_c_transport_tendency = c_tend
    , vto_n_transport_tendency = n_tend
    , vto_som_adv_coef = som_adv_coef
    , vto_som_diffus_coef = som_diffus_coef
    }

-- =========================================================================
-- Single-pool tridiagonal vertical transport (Patankar 1980)
-- =========================================================================

data TridiagTranspInput = TridiagTranspInput
  { tti_nlev           :: !Int
  , tti_conc_vr        :: !(VU.Vector Double)  -- ^ concentration per level (gC/m3 or gN/m3)
  , tti_source_vr      :: !(VU.Vector Double)  -- ^ source/sink per level (gC/m3/s or gN/m3/s)
  , tti_diffus         :: !(VU.Vector Double)  -- ^ diffusivity at interfaces (nlev+1) [m2/s]
  , tti_adv_flux       :: !(VU.Vector Double)  -- ^ advective flux at interfaces (nlev+1) [m/s]
  , tti_dzsoi          :: !(VU.Vector Double)  -- ^ layer thickness (nlev) [m]
  , tti_zisoi          :: !(VU.Vector Double)  -- ^ interface depths (nlev+1) [m]
  , tti_dt             :: !Double              -- ^ timestep [s]
  , tti_nbedrock       :: !Int                 -- ^ bedrock layer index
  } deriving (Show)

-- | Full Patankar tridiagonal advection-diffusion solver for a single
-- decomposition pool in a single column. Returns updated concentration profile.
--
-- The equation solved is:
--   d(conc)/dt = d/dz[D * d(conc)/dz] - d/dz[v * conc] + S
--
-- Using the power-law scheme (Patankar 1980, Table 5.2):
--   a_j * conc'_j = a_{j-1/2} * conc'_{j-1} + a_{j+1/2} * conc'_{j+1} + S_j * dz_j
tridiagVertTransp :: TridiagTranspInput -> VU.Vector Double
tridiagVertTransp !inp =
  let !nl     = tti_nlev inp
      !dt     = tti_dt inp
      !nbr    = tti_nbedrock inp
      !conc   = tti_conc_vr inp
      !src    = tti_source_vr inp
      !diffus = tti_diffus inp
      !adv    = tti_adv_flux inp
      !dz     = tti_dzsoi inp
      !zi     = tti_zisoi inp
      !tiny   = 1.0e-20

      -- cell-centre depth of layer j and the centre-to-centre spacing.
      node  j = 0.5 * ((zi VU.! j) + (zi VU.! (j + 1)))
      dnode j = node (j + 1) - node j

      -- West coupling for cell j (exchange with j-1 across interface j).
      -- Restricted to the active soil column (j <= nbedrock) so transport is
      -- zero-flux at the surface and below bedrock.
      aW j = if j > 0 && j <= nbr
               then let !d  = (diffus VU.! j) / max tiny (dnode (j - 1))
                        !f  = adv VU.! j
                        !pe = f / max tiny d
                    in d * patankarA pe + max 0.0 f
               else 0.0
      -- East coupling for cell j (exchange with j+1 across interface j+1).
      aE j = if j < nl - 1 && (j + 1) <= nbr
               then let !d  = (diffus VU.! (j + 1)) / max tiny (dnode j)
                        !f  = adv VU.! (j + 1)
                        !pe = f / max tiny d
                    in d * patankarA pe + max 0.0 (negate f)
               else 0.0

      aP0 j  = (dz VU.! j) / dt
      aSub   = VU.generate nl (\j -> negate (aW j))
      aSup   = VU.generate nl (\j -> negate (aE j))
      aDiag  = VU.generate nl (\j -> aW j + aE j + aP0 j)
      rhs    = VU.generate nl (\j -> (conc VU.! j) * aP0 j + (src VU.! j) * (dz VU.! j))

  in if nl <= 0 then VU.empty else solveTridiag aSub aDiag aSup rhs
