{-# LANGUAGE BangPatterns #-}
-- | Photosynthesis and stomatal conductance.
-- Fortran: PhotosynthesisMod
--
-- Implements:
--   * Farquhar model (C3) / Collatz model (C4) leaf photosynthesis
--   * Medlyn2011 and Ball-Berry 1987 stomatal conductance
--   * Brent's method root finder for ci (intercellular CO2)
--   * Hybrid Newton-secant / Brent solver
--   * Canopy integration of leaf-level fluxes
--
-- All functions are pure. Fortran variable names are preserved for traceability.
module CLM.BioGeoPhys.Photosynthesis
  ( -- * Constants
    LeafRespMethod(..)
  , bbboptC3
  , bbboptC4
  , medlynRhCanMax
  , medlynRhCanFact
  , maxCS
    -- * Parameter record
  , PhotoParams(..)
  , defaultPhotoParams
    -- * Temperature response functions
  , ftPhoto
  , fthPhoto
  , fth25Photo
    -- * Quadratic solver
  , quadraticSolve
    -- * Leaf-level gas exchange
  , CiFuncInput(..)
  , CiFuncResult(..)
  , ciFunc
    -- * Root finding
  , brentSolver
  , hybridSolver
    -- * Leaf photosynthesis (single patch, single layer)
  , LeafPhotoInput(..)
  , LeafPhotoResult(..)
  , leafPhotosynthesis
    -- * Canopy integration
  , CanopyPhotoInput(..)
  , CanopyPhotoResult(..)
  , canopyIntegrate
    -- * Patch-level photosynthesis
  , PatchPhotoInput(..)
  , PatchPhotoResult(..)
  , patchPhotosynthesis
  ) where

import CLM.Constants.PhysicalConstants (tfrz, rgas)
import CLM.Constants.ControlFlags (StomatalCondMethod(..))
import Data.List (foldl')

-- ---------------------------------------------------------------------------
-- Constants
-- ---------------------------------------------------------------------------

-- | Leaf respiration method
data LeafRespMethod
  = Ryan1991     -- ^ Ryan 1991 (default)
  | Atkin2015    -- ^ Atkin 2015
  deriving (Show, Eq)

-- | Ball-Berry intercept for C3 plants [umol/m2/s]
bbboptC3 :: Double
bbboptC3 = 10000.0

-- | Ball-Berry intercept for C4 plants [umol/m2/s]
bbboptC4 :: Double
bbboptC4 = 40000.0

-- | Maximum canopy RH for Medlyn [Pa]
medlynRhCanMax :: Double
medlynRhCanMax = 50.0

-- | Medlyn RH conversion factor [1/Pa]
medlynRhCanFact :: Double
medlynRhCanFact = 0.001

-- | Max CO2 partial pressure at leaf surface [mol/mol]
maxCS :: Double
maxCS = 1.0e-06

-- | Maximum stomatal resistance [s/m]
rsmax0 :: Double
rsmax0 = 2.0e4

-- ---------------------------------------------------------------------------
-- Photosynthesis parameters
-- ---------------------------------------------------------------------------

-- | Photosynthesis parameters record (from photo_params_type in Fortran).
-- All scalar parameters needed for the Farquhar/Collatz model.
data PhotoParams = PhotoParams
  { pp_act25         :: !Double  -- ^ Rubisco activity at 25 C (umol CO2/gRubisco/s)
  , pp_fnr           :: !Double  -- ^ Mass ratio total Rubisco to N in Rubisco
  , pp_cp25_yr2000   :: !Double  -- ^ CO2 compensation point at 25C (mol/mol)
  , pp_kc25_coef     :: !Double  -- ^ Michaelis-Menten for CO2 at 25C
  , pp_ko25_coef     :: !Double  -- ^ Michaelis-Menten for O2 at 25C
  , pp_fnps          :: !Double  -- ^ Fraction light absorbed by non-photosyn pigment
  , pp_theta_psii    :: !Double  -- ^ Curvature param for electron transport
  , pp_theta_ip      :: !Double  -- ^ Curvature param for ap co-limitation
  , pp_theta_cj      :: !Double  -- ^ Curvature param for ac/aj co-limitation
  , pp_vcmaxha       :: !Double  -- ^ Activation energy for vcmax (J/mol)
  , pp_jmaxha        :: !Double  -- ^ Activation energy for jmax (J/mol)
  , pp_tpuha         :: !Double  -- ^ Activation energy for tpu (J/mol)
  , pp_lmrha         :: !Double  -- ^ Activation energy for lmr (J/mol)
  , pp_kcha          :: !Double  -- ^ Activation energy for kc (J/mol)
  , pp_koha          :: !Double  -- ^ Activation energy for ko (J/mol)
  , pp_cpha          :: !Double  -- ^ Activation energy for cp (J/mol)
  , pp_vcmaxhd       :: !Double  -- ^ Deactivation energy for vcmax (J/mol)
  , pp_jmaxhd        :: !Double  -- ^ Deactivation energy for jmax (J/mol)
  , pp_tpuhd         :: !Double  -- ^ Deactivation energy for tpu (J/mol)
  , pp_lmrhd         :: !Double  -- ^ Deactivation energy for lmr (J/mol)
  , pp_lmrse         :: !Double  -- ^ Entropy term for lmr (J/mol/K)
  , pp_tpu25ratio    :: !Double  -- ^ Ratio tpu25top / vcmax25top
  , pp_kp25ratio     :: !Double  -- ^ Ratio kp25top / vcmax25top
  , pp_vcmaxse_sf    :: !Double  -- ^ Scale factor for vcmaxse
  , pp_jmaxse_sf     :: !Double  -- ^ Scale factor for jmaxse
  , pp_tpuse_sf      :: !Double  -- ^ Scale factor for tpuse
  , pp_jmax25top_sf  :: !Double  -- ^ Scale factor for jmax25top
  } deriving (Show, Eq)

-- | Default photo parameters (typical CLM5 values from clm5_params.nc)
defaultPhotoParams :: PhotoParams
defaultPhotoParams = PhotoParams
  { pp_act25        = 72.0
  , pp_fnr          = 7.16
  , pp_cp25_yr2000  = 4.275e-05
  , pp_kc25_coef    = 4.049e-04
  , pp_ko25_coef    = 0.2874
  , pp_fnps         = 0.15
  , pp_theta_psii   = 0.7
  , pp_theta_ip     = 0.999
  , pp_theta_cj     = 0.98
  , pp_vcmaxha      = 65330.0
  , pp_jmaxha       = 43540.0
  , pp_tpuha        = 53100.0
  , pp_lmrha        = 46390.0
  , pp_kcha         = 79430.0
  , pp_koha         = 36380.0
  , pp_cpha         = 37830.0
  , pp_vcmaxhd      = 149250.0
  , pp_jmaxhd       = 152040.0
  , pp_tpuhd        = 150650.0
  , pp_lmrhd        = 150650.0
  , pp_lmrse        = 490.0
  , pp_tpu25ratio   = 0.167
  , pp_kp25ratio    = 20000.0
  , pp_vcmaxse_sf   = 1.0
  , pp_jmaxse_sf    = 1.0
  , pp_tpuse_sf     = 1.0
  , pp_jmax25top_sf = 1.0
  }

-- ---------------------------------------------------------------------------
-- Temperature response functions
-- ---------------------------------------------------------------------------

-- | Photosynthesis temperature response (Arrhenius).
-- @ft_photo(tl, ha)@ in Fortran.
ftPhoto :: Double -> Double -> Double
ftPhoto tl ha =
  exp (ha / (rgas * (tfrz + 25.0)) * (1.0 - (tfrz + 25.0) / tl))

-- | Photosynthesis temperature inhibition.
-- @fth_photo(tl, hd, se, scaleFactor)@ in Fortran.
fthPhoto :: Double -> Double -> Double -> Double -> Double
fthPhoto tl hd se scaleFactor =
  scaleFactor / (1.0 + exp ((-hd + se * tl) / (rgas * tl)))

-- | Scaling factor for temperature inhibition at 25 C.
-- @fth25_photo(hd, se)@ in Fortran.
fth25Photo :: Double -> Double -> Double
fth25Photo hd se =
  1.0 + exp ((-hd + se * (tfrz + 25.0)) / (rgas * (tfrz + 25.0)))

-- ---------------------------------------------------------------------------
-- Quadratic solver
-- ---------------------------------------------------------------------------

-- | Solve @a*x^2 + b*x + c = 0@ for real roots.
-- Returns @(r1, r2)@ where @r1 >= r2@.
quadraticSolve :: Double -> Double -> Double -> (Double, Double)
quadraticSolve a b c
  | a == 0.0  = if b == 0.0
                then (0.0, 0.0)
                else let r = -c / b in (r, r)
  | otherwise =
      let disc  = max 0.0 (b * b - 4.0 * a * c)
          q     = -0.5 * (b + signum b * sqrt disc)
      in if q == 0.0
         then (0.0, 0.0)
         else let r1 = q / a
                  r2 = c / q
              in if r1 < r2 then (r2, r1) else (r1, r2)

-- ---------------------------------------------------------------------------
-- ci_func — evaluate f(ci) for standard (non-PHS) method
-- ---------------------------------------------------------------------------

-- | Input for ci function evaluation
data CiFuncInput = CiFuncInput
  { cfi_ci              :: !Double  -- ^ Intercellular CO2 (Pa)
  , cfi_forc_pbot       :: !Double  -- ^ Atmospheric pressure (Pa)
  , cfi_gb_mol          :: !Double  -- ^ Leaf boundary layer conductance (umol/m2/s)
  , cfi_je              :: !Double  -- ^ Electron transport rate (umol/m2/s)
  , cfi_cair            :: !Double  -- ^ Atmospheric CO2 (Pa)
  , cfi_oair            :: !Double  -- ^ Atmospheric O2 (Pa)
  , cfi_lmr_z           :: !Double  -- ^ Leaf maintenance respiration (umol/m2/s)
  , cfi_par_z           :: !Double  -- ^ PAR at canopy layer (W/m2)
  , cfi_rh_can          :: !Double  -- ^ Canopy relative humidity or VPD
  , cfi_c3flag          :: !Bool    -- ^ True for C3 plants
  , cfi_vcmax_z         :: !Double  -- ^ Max carboxylation rate (umol/m2/s)
  , cfi_cp              :: !Double  -- ^ CO2 compensation point (Pa)
  , cfi_kc              :: !Double  -- ^ Michaelis-Menten for CO2 (Pa)
  , cfi_ko              :: !Double  -- ^ Michaelis-Menten for O2 (Pa)
  , cfi_qe              :: !Double  -- ^ Quantum efficiency (C4)
  , cfi_tpu_z           :: !Double  -- ^ Triose phosphate utilization (umol/m2/s)
  , cfi_kp_z            :: !Double  -- ^ PEP carboxylase rate (C4, umol/m2/s)
  , cfi_bbb             :: !Double  -- ^ Ball-Berry intercept (umol/m2/s)
  , cfi_mbb             :: !Double  -- ^ Ball-Berry slope
  , cfi_stomatalcond_mtd :: !StomatalCondMethod
  , cfi_medlynslope     :: !Double  -- ^ Medlyn slope parameter
  , cfi_medlynintercept :: !Double  -- ^ Medlyn intercept (umol/m2/s)
  , cfi_theta_cj        :: !Double  -- ^ Curvature for ac/aj co-limitation
  , cfi_theta_ip        :: !Double  -- ^ Curvature for ap co-limitation
  } deriving (Show)

-- | Result of ci function evaluation
data CiFuncResult = CiFuncResult
  { cfr_fval    :: !Double  -- ^ f(ci) residual
  , cfr_gs_mol  :: !Double  -- ^ Stomatal conductance (umol/m2/s)
  , cfr_ac      :: !Double  -- ^ Rubisco-limited rate
  , cfr_aj      :: !Double  -- ^ RuBP-limited rate
  , cfr_ap      :: !Double  -- ^ Product-limited rate
  , cfr_ag      :: !Double  -- ^ Gross photosynthesis
  , cfr_an      :: !Double  -- ^ Net photosynthesis
  } deriving (Show)

-- | Evaluate f(ci) = ci - (ca - (1.37rb+1.65rs))*patm*an.
-- Pure function returning residual and stomatal conductance.
ciFunc :: CiFuncInput -> CiFuncResult
ciFunc inp =
  let ci  = cfi_ci inp
      c3  = cfi_c3flag inp
      vcmax_z = cfi_vcmax_z inp
      cp_p    = cfi_cp inp
      kc_p    = cfi_kc inp
      ko_p    = cfi_ko inp
      qe_p    = cfi_qe inp
      tpu_z   = cfi_tpu_z inp
      kp_z    = cfi_kp_z inp
      je      = cfi_je inp
      par_z   = cfi_par_z inp
      forc_pbot = cfi_forc_pbot inp
      gb_mol  = cfi_gb_mol inp
      cair    = cfi_cair inp
      oair    = cfi_oair inp
      lmr_z   = cfi_lmr_z inp
      rh_can  = cfi_rh_can inp
      bbb_p   = cfi_bbb inp
      mbb_p   = cfi_mbb inp
      theta_cj_v = cfi_theta_cj inp
      theta_ip_v = cfi_theta_ip inp
      medlynslope_v     = cfi_medlynslope inp
      medlynintercept_v = cfi_medlynintercept inp
      stomcond = cfi_stomatalcond_mtd inp

      -- Rubisco-limited (ac), RuBP-limited (aj), product-limited (ap)
      (ac, aj, ap)
        | c3 =
            let ac' = if ko_p > 0.0
                      then vcmax_z * max (ci - cp_p) 0.0 / (ci + kc_p * (1.0 + oair / ko_p))
                      else 0.0
                denomJ = 4.0 * ci + 8.0 * cp_p
                aj' = if denomJ > 0.0
                      then je * max (ci - cp_p) 0.0 / denomJ
                      else 0.0
                ap' = 3.0 * tpu_z
            in (ac', aj', ap')
        | otherwise =
            let ac' = vcmax_z
                aj' = qe_p * par_z * 4.6
                ap' = kp_z * max ci 0.0 / forc_pbot
            in (ac', aj', ap')

      -- Co-limit ac and aj
      (r1a, r2a) = quadraticSolve theta_cj_v (-(ac + aj)) (ac * aj)
      ai = min r1a r2a

      -- Co-limit with ap
      (r1b, r2b) = quadraticSolve theta_ip_v (-(ai + ap)) (ai * ap)
      ag = max 0.0 (min r1b r2b)

      an = ag - lmr_z
  in if an < 0.0
     then CiFuncResult 0.0 0.0 ac aj ap ag an
     else
       let cs0 = cair - 1.4 / gb_mol * an * forc_pbot
           cs  = max cs0 maxCS
           gs_mol = case stomcond of
             Medlyn2011 ->
               let term = 1.6 * an / (cs / forc_pbot * 1.0e06)
                   aq = 1.0
                   bq = -(2.0 * (medlynintercept_v * 1.0e-06 + term)
                          + (medlynslope_v * term) ** 2 / (gb_mol * 1.0e-06 * rh_can))
                   cq = medlynintercept_v ** 2 * 1.0e-12
                        + (2.0 * medlynintercept_v * 1.0e-06 + term
                           * (1.0 - medlynslope_v ** 2 / rh_can)) * term
                   (rr1, rr2) = quadraticSolve aq bq cq
               in max rr1 rr2 * 1.0e06
             BallBerry1987 ->
               let aq = cs
                   bq = cs * (gb_mol - bbb_p) - mbb_p * an * forc_pbot
                   cq = -gb_mol * (cs * bbb_p + mbb_p * an * forc_pbot * rh_can)
                   (rr1, rr2) = quadraticSolve aq bq cq
               in max rr1 rr2

           fval = ci - cair + an * forc_pbot * (1.4 * gs_mol + 1.6 * gb_mol)
                  / (gb_mol * gs_mol)
       in CiFuncResult fval gs_mol ac aj ap ag an

-- ---------------------------------------------------------------------------
-- Brent's method root finder
-- ---------------------------------------------------------------------------

-- | Brent's method for finding the root of ciFunc between two brackets.
-- Returns the root ci value.
brentSolver
  :: Double         -- ^ x1 (lower bracket)
  -> Double         -- ^ x2 (upper bracket)
  -> Double         -- ^ f(x1)
  -> Double         -- ^ f(x2)
  -> Double         -- ^ tolerance
  -> CiFuncInput    -- ^ base input (ci field will be overwritten)
  -> Double         -- ^ root ci
brentSolver x1_0 x2_0 f1_0 f2_0 tol baseInp =
  let epsVal = 1.0e-2
      itmax  = 20 :: Int
      -- Initial state: a=x1, b=x2, fa=f1, fb=f2, c=b, fc=fb, d=b-a, e=d
      go !iter !a !b !c_ !fa !fb !fc !d !e
        | iter > itmax = b
        | otherwise =
            let -- If fb and fc same sign, reset c to a
                (a', b', c_', fa', fb', fc', d', e') =
                  if (fb > 0.0 && fc > 0.0) || (fb < 0.0 && fc < 0.0)
                  then (a, b, a, fa, fb, fa, b - a, b - a)
                  else (a, b, c_, fa, fb, fc, d, e)

                -- If |fc| < |fb|, swap
                (a'', b'', c_'', fa'', fb'', fc'') =
                  if abs fc' < abs fb'
                  then (b', c_', b', fb', fc', fb')
                  else (a', b', c_', fa', fb', fc')

                tol1 = 2.0 * epsVal * abs b'' + 0.5 * tol
                xm   = 0.5 * (c_'' - b'')
            in if abs xm <= tol1 || fb'' == 0.0
               then b''
               else
                 let (d_new, e_new) =
                       if abs e' >= tol1 && abs fa'' > abs fb''
                       then
                         let s = fb'' / fa''
                             (p_v, q_v) =
                               if a'' == c_''
                               then (2.0 * xm * s, 1.0 - s)
                               else let q0 = fa'' / fc''
                                        r0 = fb'' / fc''
                                    in ( s * (2.0 * xm * q0 * (q0 - r0) - (b'' - a'') * (r0 - 1.0))
                                       , (q0 - 1.0) * (r0 - 1.0) * (s - 1.0) )
                             p_abs = abs p_v
                             q_neg = if p_v > 0.0 then -q_v else q_v
                         in if 2.0 * p_abs < min (3.0 * xm * q_neg - abs (tol1 * q_neg)) (abs (e' * q_neg))
                            then (p_abs / q_neg, d')  -- d_new=p/q, e_new=old d
                            else (xm, xm)             -- bisection fallback
                       else (xm, xm)

                     a_next  = b''
                     fa_next = fb''
                     b_next  = if abs d_new > tol1
                               then b'' + d_new
                               else b'' + signum xm * tol1

                     res = ciFunc (baseInp { cfi_ci = b_next })
                     fb_next = cfr_fval res
                 in if fb_next == 0.0
                    then b_next
                    else go (iter + 1) a_next b_next c_'' fa_next fb_next fc'' d_new e_new
  in go 1 x1_0 x2_0 x2_0 f1_0 f2_0 f2_0 (x2_0 - x1_0) (x2_0 - x1_0)

-- ---------------------------------------------------------------------------
-- Hybrid Newton-secant / Brent solver
-- ---------------------------------------------------------------------------

-- | Hybrid solver for ci. Returns (ci_solution, gs_mol).
hybridSolver
  :: Double       -- ^ Initial guess x0
  -> CiFuncInput  -- ^ Base input (ci field overwritten)
  -> (Double, Double)
hybridSolver x0_init baseInp =
  let epsVal = 1.0e-2
      eps1   = 1.0e-4
      itmax  = 40 :: Int

      res0   = ciFunc (baseInp { cfi_ci = x0_init })
      f0_0   = cfr_fval res0
      gs0    = cfr_gs_mol res0
  in if f0_0 == 0.0
     then (x0_init, gs0)
     else
       let x1_init = x0_init * 0.99
           res1    = ciFunc (baseInp { cfi_ci = x1_init })
           f1_init = cfr_fval res1
           gs1     = cfr_gs_mol res1
       in if f1_init == 0.0
          then (x1_init, gs1)
          else
            let minx0 = if abs f1_init < abs f0_0 then x1_init else x0_init
                minf0 = min (abs f0_0) (abs f1_init)
                gs_init = if abs f1_init < abs f0_0 then gs1 else gs0

                go !iter !x0 !f0 !x1 !f1 !minx !minf !gs_best
                  | iter > itmax =
                      let resM = ciFunc (baseInp { cfi_ci = minx })
                      in (minx, cfr_gs_mol resM)
                  | (f1 - f0) == 0.0 = (x1, gs_best)
                  | otherwise =
                      let dx  = -f1 * (x1 - x0) / (f1 - f0)
                          x   = x1 + dx
                          tolV = abs x * epsVal
                      in if abs dx < tolV
                         then
                           let resF = ciFunc (baseInp { cfi_ci = x })
                           in (x, cfr_gs_mol resF)
                         else
                           let resN = ciFunc (baseInp { cfi_ci = x })
                               f_new = cfr_fval resN
                               gs_new = cfr_gs_mol resN
                               (minx', minf') = if abs f_new < minf
                                                 then (x, abs f_new)
                                                 else (minx, minf)
                           in if abs f_new <= eps1
                              then (x, gs_new)
                              else if f_new * f1 < 0.0
                                   then
                                     let x_brent = brentSolver x1 x f1 f_new tolV baseInp
                                         resB = ciFunc (baseInp { cfi_ci = x_brent })
                                     in (x_brent, cfr_gs_mol resB)
                                   else go (iter + 1) x1 f1 x f_new minx' minf' gs_new

            in go 1 x0_init f0_0 x1_init f1_init minx0 minf0 gs_init

-- ---------------------------------------------------------------------------
-- Leaf-level photosynthesis (single patch, single canopy layer)
-- ---------------------------------------------------------------------------

-- | Input for single-leaf photosynthesis computation
data LeafPhotoInput = LeafPhotoInput
  { lpi_c3flag            :: !Bool
  , lpi_forc_pbot         :: !Double  -- ^ Atmospheric pressure (Pa)
  , lpi_t_veg             :: !Double  -- ^ Vegetation temperature (K)
  , lpi_t10               :: !Double  -- ^ 10-day running mean temperature (K)
  , lpi_tgcm              :: !Double  -- ^ Ground temperature for cf (K)
  , lpi_rb                :: !Double  -- ^ Leaf boundary layer resistance (s/m)
  , lpi_btran             :: !Double  -- ^ Soil water stress factor (0-1)
  , lpi_dayl_factor       :: !Double  -- ^ Day length factor (0-1)
  , lpi_oair              :: !Double  -- ^ O2 partial pressure (Pa)
  , lpi_cair              :: !Double  -- ^ CO2 partial pressure (Pa)
  , lpi_esat_tv           :: !Double  -- ^ Saturation vapour pressure at t_veg (Pa)
  , lpi_eair              :: !Double  -- ^ Actual vapour pressure (Pa)
  , lpi_par_z             :: !Double  -- ^ PAR at canopy layer (W/m2)
  , lpi_tlai_z            :: !Double  -- ^ Layer LAI
  , lpi_lai_z             :: !Double  -- ^ Layer LAI weight for integration
  , lpi_vcmaxcint         :: !Double  -- ^ Vcmax canopy integral
  , lpi_laican            :: !Double  -- ^ Cumulative LAI to layer midpoint
  , lpi_o3coefv           :: !Double  -- ^ Ozone damage coef for photosyn
  , lpi_o3coefg           :: !Double  -- ^ Ozone damage coef for conductance
  , lpi_leafcn            :: !Double  -- ^ Leaf C:N ratio
  , lpi_flnr              :: !Double  -- ^ Fraction leaf N in Rubisco
  , lpi_fnitr             :: !Double  -- ^ Nitrogen availability factor
  , lpi_slatop            :: !Double  -- ^ Specific leaf area at top of canopy
  , lpi_mbbopt            :: !Double  -- ^ Ball-Berry slope
  , lpi_medlynintercept   :: !Double  -- ^ Medlyn intercept (umol/m2/s)
  , lpi_medlynslope       :: !Double  -- ^ Medlyn slope
  , lpi_stomatalcond_mtd  :: !StomatalCondMethod
  , lpi_params            :: !PhotoParams
  , lpi_use_cn            :: !Bool
  , lpi_leaf_mr_vcm       :: !Double  -- ^ Ratio leaf maint resp to vcmax
  , lpi_light_inhibit     :: !Bool
  , lpi_nlevcan           :: !Int
  , lpi_nscaler           :: !Double  -- ^ Nitrogen scaler for this layer
  } deriving (Show)

-- | Result of single-leaf photosynthesis
data LeafPhotoResult = LeafPhotoResult
  { lpr_an        :: !Double  -- ^ Net photosynthesis (umol/m2/s)
  , lpr_ag        :: !Double  -- ^ Gross photosynthesis (umol/m2/s)
  , lpr_ac        :: !Double  -- ^ Rubisco-limited rate
  , lpr_aj        :: !Double  -- ^ RuBP-limited rate
  , lpr_ap        :: !Double  -- ^ Product-limited rate
  , lpr_gs_mol    :: !Double  -- ^ Stomatal conductance (umol/m2/s)
  , lpr_rs_z      :: !Double  -- ^ Stomatal resistance (s/m)
  , lpr_ci_z      :: !Double  -- ^ Intercellular CO2 (Pa)
  , lpr_lmr_z     :: !Double  -- ^ Leaf maintenance respiration (umol/m2/s)
  , lpr_vcmax_z   :: !Double  -- ^ Max carboxylation rate (umol/m2/s)
  , lpr_psn_z     :: !Double  -- ^ Leaf photosynthesis rate (umol/m2/s)
  , lpr_psn_wc_z  :: !Double  -- ^ Wc-limited photosynthesis
  , lpr_psn_wj_z  :: !Double  -- ^ Wj-limited photosynthesis
  , lpr_psn_wp_z  :: !Double  -- ^ Wp-limited photosynthesis
  , lpr_rh_leaf   :: !Double  -- ^ Relative humidity at leaf
  } deriving (Show)

-- | Compute leaf-level photosynthesis and stomatal conductance for a single
-- patch and canopy layer. Pure function.
leafPhotosynthesis :: LeafPhotoInput -> LeafPhotoResult
leafPhotosynthesis inp =
  let pp      = lpi_params inp
      c3      = lpi_c3flag inp
      t_veg   = lpi_t_veg inp
      t10     = lpi_t10 inp
      forc_pbot = lpi_forc_pbot inp
      btran   = lpi_btran inp
      dayl_factor = lpi_dayl_factor inp
      oair    = lpi_oair inp
      cair    = lpi_cair inp
      par_z   = lpi_par_z inp
      rb      = lpi_rb inp
      tgcm    = lpi_tgcm inp
      esat_tv = lpi_esat_tv inp
      eair    = lpi_eair inp
      stomcond = lpi_stomatalcond_mtd inp
      nscaler = lpi_nscaler inp

      -- Michaelis-Menten kinetics
      kc25 = pp_kc25_coef pp * forc_pbot
      ko25 = pp_ko25_coef pp * forc_pbot
      sco  = 0.5 * 0.209 / pp_cp25_yr2000 pp
      cp25 = 0.5 * oair / sco
      kc   = kc25 * ftPhoto t_veg (pp_kcha pp)
      ko   = ko25 * ftPhoto t_veg (pp_koha pp)
      cp   = cp25 * ftPhoto t_veg (pp_cpha pp)

      -- Leaf nitrogen content
      lnc = 1.0 / (lpi_slatop inp * lpi_leafcn inp)

      -- Vcmax25 at canopy top
      vcmax25top0 = lnc * lpi_flnr inp * pp_fnr pp * pp_act25 pp * dayl_factor
      vcmax25top  = if lpi_use_cn inp then vcmax25top0
                    else vcmax25top0 * lpi_fnitr inp

      -- Jmax, TPU, Kp at canopy top
      t10c = min (max (t10 - tfrz) 11.0) 35.0
      jmax25top = (2.59 - 0.035 * t10c) * vcmax25top * pp_jmax25top_sf pp
      tpu25top  = pp_tpu25ratio pp * vcmax25top
      kp25top   = pp_kp25ratio pp * vcmax25top

      -- Maintenance respiration
      lmrc = fth25Photo (pp_lmrhd pp) (pp_lmrse pp)
      lmr25top
        | lpi_use_cn inp = 2.525e-6 * (1.5 ** ((25.0 - 20.0) / 10.0)) * lnc / 12.0e-06
        | c3             = vcmax25top * lpi_leaf_mr_vcm inp
        | otherwise      = vcmax25top * 0.025

      lmr25 = lmr25top * nscaler
      lmr_z_raw
        | c3 = lmr25 * ftPhoto t_veg (pp_lmrha pp)
               * fthPhoto t_veg (pp_lmrhd pp) (pp_lmrse pp) lmrc
        | otherwise =
            let v = lmr25 * 2.0 ** ((t_veg - (tfrz + 25.0)) / 10.0)
            in v / (1.0 + exp (1.3 * (t_veg - (tfrz + 55.0))))

      -- Soil water stress and light inhibition
      lmr_z_1 = lmr_z_raw * btran
      lmr_z   = if lpi_light_inhibit inp && par_z > 0.0
                then lmr_z_1 * 0.67
                else lmr_z_1

      -- Conversion factor: mol/m3
      cf = forc_pbot / (rgas * tgcm) * 1.0e06
      gb = 1.0 / rb
      gb_mol = gb * cf

  in if par_z <= 0.0
     -- Night time
     then let gs_mol_night = case stomcond of
                BallBerry1987 -> max (bbboptC3 * btran) 1.0
                Medlyn2011    -> lpi_medlynintercept inp
              rs_z_night = case stomcond of
                BallBerry1987 -> min rsmax0 (1.0 / max (bbboptC3 * btran) 1.0 * cf)
                Medlyn2011    -> min rsmax0 (1.0 / lpi_medlynintercept inp * cf)
              an_night = 0.0 - lmr_z
          in LeafPhotoResult
               { lpr_an       = an_night
               , lpr_ag       = 0.0
               , lpr_ac       = 0.0
               , lpr_aj       = 0.0
               , lpr_ap       = 0.0
               , lpr_gs_mol   = gs_mol_night
               , lpr_rs_z     = rs_z_night
               , lpr_ci_z     = 0.0
               , lpr_lmr_z    = lmr_z
               , lpr_vcmax_z  = 0.0
               , lpr_psn_z    = 0.0
               , lpr_psn_wc_z = 0.0
               , lpr_psn_wj_z = 0.0
               , lpr_psn_wp_z = 0.0
               , lpr_rh_leaf  = 0.0
               }
     -- Day time
     else let
       -- Vcmax, Jmax, TPU temperature-adjusted
       vcmax25 = vcmax25top * nscaler
       jmax25  = jmax25top * nscaler
       tpu25   = tpu25top * nscaler
       kp25    = kp25top * nscaler

       vcmaxse = (668.39 - 1.07 * t10c) * pp_vcmaxse_sf pp
       jmaxse  = (659.70 - 0.75 * t10c) * pp_jmaxse_sf pp
       tpuse   = (668.39 - 1.07 * t10c) * pp_tpuse_sf pp
       vcmaxc  = fth25Photo (pp_vcmaxhd pp) vcmaxse
       jmaxc   = fth25Photo (pp_jmaxhd pp) jmaxse
       tpuc    = fth25Photo (pp_tpuhd pp) tpuse

       vcmax_z_raw
         | c3 = vcmax25 * ftPhoto t_veg (pp_vcmaxha pp)
                * fthPhoto t_veg (pp_vcmaxhd pp) vcmaxse vcmaxc
         | otherwise =
             let v = vcmax25 * 2.0 ** ((t_veg - (tfrz + 25.0)) / 10.0)
                 v' = v / (1.0 + exp (0.2 * ((tfrz + 15.0) - t_veg)))
             in v' / (1.0 + exp (0.3 * (t_veg - (tfrz + 40.0))))

       vcmax_z = vcmax_z_raw * btran

       jmax_z = jmax25 * ftPhoto t_veg (pp_jmaxha pp)
                * fthPhoto t_veg (pp_jmaxhd pp) jmaxse jmaxc

       tpu_z = tpu25 * ftPhoto t_veg (pp_tpuha pp)
               * fthPhoto t_veg (pp_tpuhd pp) tpuse tpuc

       kp_z = kp25 * 2.0 ** ((t_veg - (tfrz + 25.0)) / 10.0)

       -- Electron transport rate
       qabs  = 0.5 * (1.0 - pp_fnps pp) * par_z * 4.6
       (r1j, r2j) = quadraticSolve (pp_theta_psii pp) (-(qabs + jmax_z)) (qabs * jmax_z)
       je = min r1j r2j

       -- Canopy humidity
       ceair = min eair esat_tv
       rh_can = case stomcond of
         BallBerry1987 -> ceair / esat_tv
         Medlyn2011    -> max (esat_tv - ceair) medlynRhCanMax * medlynRhCanFact

       -- BB parameters
       bbb = case stomcond of
         BallBerry1987 -> max (bbboptC3 * btran) 1.0
         Medlyn2011    -> 0.0  -- not used

       -- Initial ci guess
       ci0 = if c3 then 0.7 * cair else 0.4 * cair

       -- Build CiFuncInput
       cfiBase = CiFuncInput
         { cfi_ci              = ci0
         , cfi_forc_pbot       = forc_pbot
         , cfi_gb_mol          = gb_mol
         , cfi_je              = je
         , cfi_cair            = cair
         , cfi_oair            = oair
         , cfi_lmr_z           = lmr_z
         , cfi_par_z           = par_z
         , cfi_rh_can          = rh_can
         , cfi_c3flag          = c3
         , cfi_vcmax_z         = vcmax_z
         , cfi_cp              = cp
         , cfi_kc              = kc
         , cfi_ko              = ko
         , cfi_qe              = if c3 then 0.0 else 0.05
         , cfi_tpu_z           = tpu_z
         , cfi_kp_z            = kp_z
         , cfi_bbb             = bbb
         , cfi_mbb             = lpi_mbbopt inp
         , cfi_stomatalcond_mtd = stomcond
         , cfi_medlynslope     = lpi_medlynslope inp
         , cfi_medlynintercept = lpi_medlynintercept inp
         , cfi_theta_cj        = pp_theta_cj pp
         , cfi_theta_ip        = pp_theta_ip pp
         }

       -- Solve for ci and gs
       (ci_sol, gs_mol_sol) = hybridSolver ci0 cfiBase

       -- Evaluate at solution to get final rates
       resFinal = ciFunc (cfiBase { cfi_ci = ci_sol })
       an_f = cfr_an resFinal
       ag_f = cfr_ag resFinal
       ac_f = cfr_ac resFinal
       aj_f = cfr_aj resFinal
       ap_f = cfr_ap resFinal

       -- Correct gs_mol if an < 0
       gs_mol_final
         | an_f < 0.0 = case stomcond of
             BallBerry1987 -> bbb
             Medlyn2011    -> lpi_medlynintercept inp
         | otherwise = gs_mol_sol

       -- Final ci
       ci_final0 = cair - an_f * forc_pbot
                   * (1.4 * gs_mol_final + 1.6 * gb_mol)
                   / (gb_mol * gs_mol_final)
       ci_final = max ci_final0 1.0e-06

       -- Convert to resistance
       gs_val = gs_mol_final / cf
       rs_z0  = min (1.0 / gs_val) rsmax0
       rs_z   = rs_z0 / lpi_o3coefg inp

       -- Photosynthesis with ozone
       psn_z = ag_f * lpi_o3coefv inp

       -- Limiting rate attribution
       (psn_wc, psn_wj, psn_wp)
         | ac_f <= aj_f && ac_f <= ap_f = (psn_z, 0.0, 0.0)
         | aj_f < ac_f  && aj_f <= ap_f = (0.0, psn_z, 0.0)
         | ap_f < ac_f  && ap_f < aj_f  = (0.0, 0.0, psn_z)
         | otherwise                    = (psn_z, 0.0, 0.0)

       -- Ball-Berry leaf RH
       rh_leaf_val = case stomcond of
         BallBerry1987 ->
           (gb_mol * ceair + gs_mol_final * esat_tv) /
           ((gb_mol + gs_mol_final) * esat_tv)
         Medlyn2011 -> rh_can

       in LeafPhotoResult
            { lpr_an       = an_f
            , lpr_ag       = ag_f
            , lpr_ac       = ac_f
            , lpr_aj       = aj_f
            , lpr_ap       = ap_f
            , lpr_gs_mol   = gs_mol_final
            , lpr_rs_z     = rs_z
            , lpr_ci_z     = ci_final
            , lpr_lmr_z    = lmr_z
            , lpr_vcmax_z  = vcmax_z
            , lpr_psn_z    = psn_z
            , lpr_psn_wc_z = psn_wc
            , lpr_psn_wj_z = psn_wj
            , lpr_psn_wp_z = psn_wp
            , lpr_rh_leaf  = rh_leaf_val
            }

-- ---------------------------------------------------------------------------
-- Canopy integration
-- ---------------------------------------------------------------------------

-- | Input for canopy integration across multiple layers.
data CanopyPhotoInput = CanopyPhotoInput
  { cpi_nrad     :: !Int          -- ^ Number of active radiation layers
  , cpi_rb       :: !Double       -- ^ Leaf boundary layer resistance (s/m)
  , cpi_layers   :: [(Double, LeafPhotoResult)]
      -- ^ List of (lai_z weight, leaf result) per canopy layer
  } deriving (Show)

-- | Result of canopy integration
data CanopyPhotoResult = CanopyPhotoResult
  { cpr_psn      :: !Double  -- ^ Canopy mean photosynthesis
  , cpr_psn_wc   :: !Double
  , cpr_psn_wj   :: !Double
  , cpr_psn_wp   :: !Double
  , cpr_lmr      :: !Double  -- ^ Canopy mean leaf maint resp
  , cpr_rs       :: !Double  -- ^ Canopy stomatal resistance (s/m)
  } deriving (Show)

-- | Integrate leaf-level fluxes over canopy layers.
-- Pure function corresponding to Pass 4 in Fortran PhotosynthesisMod.
canopyIntegrate :: CanopyPhotoInput -> CanopyPhotoResult
canopyIntegrate inp =
  let rb = cpi_rb inp
      _nrad = cpi_nrad inp  -- used by caller to slice layers
      layers = cpi_layers inp
      -- Accumulate
      (psncan, psncan_wc, psncan_wj, psncan_wp, lmrcan, gscan, laican) =
        foldl' accum (0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0) layers

      accum (!p, !wc, !wj, !wp, !lm, !gs, !la) (lai_z, lr) =
        ( p  + lpr_psn_z lr    * lai_z
        , wc + lpr_psn_wc_z lr * lai_z
        , wj + lpr_psn_wj_z lr * lai_z
        , wp + lpr_psn_wp_z lr * lai_z
        , lm + lpr_lmr_z lr    * lai_z
        , gs + lai_z / (rb + lpr_rs_z lr)
        , la + lai_z
        )
  in if laican > 0.0
     then CanopyPhotoResult
       { cpr_psn    = psncan / laican
       , cpr_psn_wc = psncan_wc / laican
       , cpr_psn_wj = psncan_wj / laican
       , cpr_psn_wp = psncan_wp / laican
       , cpr_lmr    = lmrcan / laican
       , cpr_rs     = laican / gscan - rb
       }
     else CanopyPhotoResult 0.0 0.0 0.0 0.0 0.0 0.0

-- ---------------------------------------------------------------------------
-- Nitrogen scaling of Vcmax across canopy layers
-- ---------------------------------------------------------------------------

-- | Compute Vcmax25 profile from leaf N distribution (exponential decay).
-- Follows the Bonan et al. canopy integration with N scaling.
vcmax25Profile :: Double  -- ^ vcmax25_top (top-of-canopy Vcmax at 25C)
               -> Double  -- ^ slatop (specific leaf area at top, m2/gC)
               -> Double  -- ^ nscaler_kn (nitrogen extinction coefficient)
               -> Int     -- ^ nrad (number of active layers)
               -> [Double] -- ^ cumulative LAI at each layer interface
               -> [Double] -- ^ vcmax25 per layer
vcmax25Profile !vcmax25_top !_slatop !kn !nrad !cumLAI =
  [ vcmax25_top * exp (-kn * lai_cum) | (i, lai_cum) <- zip [0..] cumLAI, i < nrad ]

-- | Compute Jmax25 from Vcmax25 (Wullschleger 1993 relationship).
jmax25FromVcmax25 :: Double -> Double
jmax25FromVcmax25 vcmax25 = 1.67 * vcmax25

-- ---------------------------------------------------------------------------
-- Sunlit/Shaded canopy partitioning
-- ---------------------------------------------------------------------------

data SunShadeFractions = SunShadeFractions
  { ssf_fsun   :: !Double  -- ^ sunlit LAI fraction
  , ssf_fsha   :: !Double  -- ^ shaded LAI fraction
  , ssf_laisun :: !Double  -- ^ sunlit LAI (m2/m2)
  , ssf_laisha :: !Double  -- ^ shaded LAI (m2/m2)
  } deriving (Show)

-- | Compute sunlit/shaded fractions from LAI and beam extinction.
sunShadeFractions :: Double  -- ^ total LAI
                  -> Double  -- ^ kb (beam extinction coefficient)
                  -> SunShadeFractions
sunShadeFractions !lai !kb =
  let !laisun = if kb > 0.0 then (1.0 - exp (-kb * lai)) / kb else lai * 0.5
      !laisha = max 0.0 (lai - laisun)
      !fsun = if lai > 0.0 then laisun / lai else 0.5
      !fsha = 1.0 - fsun
  in SunShadeFractions { ssf_fsun = fsun, ssf_fsha = fsha
                       , ssf_laisun = laisun, ssf_laisha = laisha }

-- ---------------------------------------------------------------------------
-- Patch-level photosynthesis driver
-- ---------------------------------------------------------------------------

data PatchPhotoInput = PatchPhotoInput
  { ppi_vcmax25_top    :: !Double   -- ^ Vcmax25 at top of canopy (umol/m2/s)
  , ppi_jmax25_top     :: !Double   -- ^ Jmax25 at top (or 0 to derive from Vcmax)
  , ppi_nrad           :: !Int      -- ^ number of active canopy layers
  , ppi_lai            :: !Double   -- ^ total LAI (m2/m2)
  , ppi_sai            :: !Double   -- ^ stem area index
  , ppi_kb             :: !Double   -- ^ direct beam extinction coefficient
  , ppi_kn             :: !Double   -- ^ nitrogen extinction coefficient
  , ppi_par_sun        :: ![Double] -- ^ PAR absorbed per layer (sunlit, W/m2)
  , ppi_par_sha        :: ![Double] -- ^ PAR absorbed per layer (shaded, W/m2)
  , ppi_cum_lai        :: ![Double] -- ^ cumulative LAI at layer midpoints
  -- Environment
  , ppi_forc_pbot      :: !Double   -- ^ atmospheric pressure (Pa)
  , ppi_co2_ppm        :: !Double   -- ^ CO2 mixing ratio (ppm)
  , ppi_o2_ppm         :: !Double   -- ^ O2 mixing ratio (ppm)
  , ppi_t_veg          :: !Double   -- ^ vegetation temperature (K)
  , ppi_rb             :: !Double   -- ^ leaf boundary layer resistance (s/m)
  , ppi_rh_can         :: !Double   -- ^ canopy air relative humidity
  , ppi_esat_tv        :: !Double   -- ^ saturation vapor pressure at T_veg (Pa)
  , ppi_ceair          :: !Double   -- ^ vapor pressure of canopy air (Pa)
  , ppi_gb_mol         :: !Double   -- ^ boundary layer conductance (mol/m2/s)
  -- Flags
  , ppi_c3flag         :: !Bool     -- ^ True for C3, False for C4
  , ppi_o3coefv        :: !Double   -- ^ ozone Vcmax scaling
  , ppi_o3coefg        :: !Double   -- ^ ozone conductance scaling
  } deriving (Show)

data PatchPhotoResult = PatchPhotoResult
  { ppr_psn_sun       :: !Double  -- ^ sunlit photosynthesis (umol CO2/m2/s)
  , ppr_psn_sha       :: !Double  -- ^ shaded photosynthesis
  , ppr_lmr_sun       :: !Double  -- ^ sunlit leaf maintenance resp
  , ppr_lmr_sha       :: !Double  -- ^ shaded leaf maintenance resp
  , ppr_gs_sun        :: !Double  -- ^ sunlit stomatal conductance (mol/m2/s)
  , ppr_gs_sha        :: !Double  -- ^ shaded stomatal conductance
  , ppr_an_sun        :: !Double  -- ^ sunlit net assimilation
  , ppr_an_sha        :: !Double  -- ^ shaded net assimilation
  , ppr_vcmax_sun_top :: !Double  -- ^ top-of-canopy sunlit Vcmax
  , ppr_rs_sun        :: !Double  -- ^ canopy sunlit stomatal resistance
  , ppr_rs_sha        :: !Double  -- ^ canopy shaded stomatal resistance
  } deriving (Show)

-- | Full patch-level photosynthesis with nitrogen scaling and sun/shade.
-- Calls leafPhotosynthesis for each layer, then integrates.
patchPhotosynthesis :: PhotoParams -> PatchPhotoInput -> PatchPhotoResult
patchPhotosynthesis !params !inp =
  let !nrad = ppi_nrad inp
      !lai = ppi_lai inp
      !kb = ppi_kb inp
      !kn = ppi_kn inp
      !vcmax25_top = ppi_vcmax25_top inp
      !rb = ppi_rb inp

      -- Sunlit/shaded fractions
      !ssf = sunShadeFractions lai kb

      -- Vcmax25 profile
      !vcmax25_layers = vcmax25Profile vcmax25_top 0.0 kn nrad (ppi_cum_lai inp)

      -- Layer weights (equal for now, could be dlai per layer)
      !dlai = if nrad > 0 then lai / fromIntegral nrad else 0.0

      -- Process sunlit layers
      sunResults = zipWith3 (\vcmax25 par_sun cum_lai ->
        let !jmax25 = jmax25FromVcmax25 vcmax25
            !lpi = buildLeafInput params inp vcmax25 jmax25 par_sun
        in leafPhotosynthesis lpi
        ) vcmax25_layers (ppi_par_sun inp) (ppi_cum_lai inp)

      -- Process shaded layers
      shaResults = zipWith3 (\vcmax25 par_sha cum_lai ->
        let !jmax25 = jmax25FromVcmax25 vcmax25
            !lpi = buildLeafInput params inp vcmax25 jmax25 par_sha
        in leafPhotosynthesis lpi
        ) vcmax25_layers (ppi_par_sha inp) (ppi_cum_lai inp)

      -- Integrate sunlit
      (!psnSun, !lmrSun, !gsSun, !anSun) = integrateResults sunResults dlai
      -- Integrate shaded
      (!psnSha, !lmrSha, !gsSha, !anSha) = integrateResults shaResults dlai

      -- Convert to canopy resistance
      !rsSun = if gsSun > 0.0 then 1.0 / gsSun else 1.0e6
      !rsSha = if gsSha > 0.0 then 1.0 / gsSha else 1.0e6

  in PatchPhotoResult
     { ppr_psn_sun = psnSun * ssf_laisun ssf / max 0.01 lai
     , ppr_psn_sha = psnSha * ssf_laisha ssf / max 0.01 lai
     , ppr_lmr_sun = lmrSun
     , ppr_lmr_sha = lmrSha
     , ppr_gs_sun = gsSun
     , ppr_gs_sha = gsSha
     , ppr_an_sun = anSun
     , ppr_an_sha = anSha
     , ppr_vcmax_sun_top = vcmax25_top
     , ppr_rs_sun = rsSun
     , ppr_rs_sha = rsSha
     }

-- | Integrate leaf results over layers.
integrateResults :: [LeafPhotoResult] -> Double -> (Double, Double, Double, Double)
integrateResults results dlai =
  foldl' (\(!p, !l, !g, !a) r ->
    ( p + lpr_psn_z r * dlai
    , l + lpr_lmr_z r * dlai
    , g + lpr_gs_mol r * dlai
    , a + lpr_an r * dlai
    )) (0.0, 0.0, 0.0, 0.0) results

-- | Build leaf-level input from patch-level data and per-layer values.
buildLeafInput :: PhotoParams -> PatchPhotoInput
               -> Double -> Double -> Double -> LeafPhotoInput
buildLeafInput params inp vcmax25 _jmax25 par =
  LeafPhotoInput
    { lpi_c3flag = ppi_c3flag inp
    , lpi_forc_pbot = ppi_forc_pbot inp
    , lpi_t_veg = ppi_t_veg inp
    , lpi_t10 = ppi_t_veg inp
    , lpi_tgcm = ppi_t_veg inp
    , lpi_rb = ppi_rb inp
    , lpi_btran = 1.0
    , lpi_dayl_factor = 1.0
    , lpi_oair = ppi_o2_ppm inp * ppi_forc_pbot inp * 1.0e-6
    , lpi_cair = ppi_co2_ppm inp * ppi_forc_pbot inp * 1.0e-6
    , lpi_esat_tv = ppi_esat_tv inp
    , lpi_eair = ppi_ceair inp
    , lpi_par_z = par
    , lpi_tlai_z = 1.0
    , lpi_lai_z = 1.0
    , lpi_vcmaxcint = vcmax25
    , lpi_laican = 0.0
    , lpi_o3coefv = ppi_o3coefv inp
    , lpi_o3coefg = ppi_o3coefg inp
    , lpi_leafcn = 25.0
    , lpi_flnr = 0.0
    , lpi_fnitr = 1.0
    , lpi_slatop = 0.01
    , lpi_mbbopt = if ppi_c3flag inp then 9.0 else 4.0
    , lpi_medlynintercept = 100.0
    , lpi_medlynslope = 6.0
    , lpi_stomatalcond_mtd = Medlyn2011
    , lpi_params = params
    , lpi_use_cn = False
    , lpi_leaf_mr_vcm = 0.015
    , lpi_light_inhibit = False
    , lpi_nlevcan = 1
    , lpi_nscaler = 1.0
    }
