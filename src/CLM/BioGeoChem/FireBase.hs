{-# LANGUAGE BangPatterns #-}
-- | Fire dynamics base module for coupled CN code.
-- Fortran: CNFireBaseMod.F90
-- Julia:   src/biogeochem/fire_base.jl
--
-- Contains fire constants, root wetness calculation, and fire C/N flux
-- computation including combustion emissions and mortality to litter.
module CLM.BioGeoChem.FireBase
  ( -- * Data types
    CNFireConstData(..)
  , defaultFireConst
  , CNFireParams(..)
  , PftConFireBase(..)
    -- * Root wetness
  , SoilSuctionResult(..)
  , defaultSoilSuction
  , calcFireRootWetness
    -- * Fire fluxes (single-patch)
  , FireFluxInput(..)
  , FireFluxOutput(..)
  , calcFireFluxPatch
    -- * Decomp pool fire loss (column)
  , DecompFireLossInput(..)
  , DecompFireLossOutput(..)
  , calcDecompFireLoss
    -- * Peat fire SOM loss
  , calcSomcFire
    -- * Column-scalar fire application (combustion + mortality, C/N conserving)
  , ColumnFireInput(..)
  , ColumnFireResult(..)
  , applyColumnFireFluxes
  ) where

import qualified Data.Vector.Unboxed as VU

-- =========================================================================
-- Fire constants
-- =========================================================================

data CNFireConstData = CNFireConstData
  { fcd_borealat           :: !Double  -- ^ latitude for boreal peat fires [deg]
  , fcd_lfuel              :: !Double  -- ^ lower threshold fuel mass [gC/m2]
  , fcd_ufuel              :: !Double  -- ^ upper threshold fuel mass [gC/m2]
  , fcd_g0                 :: !Double  -- ^ g(W) when W=0 [m/s]
  , fcd_rh_low             :: !Double  -- ^ relative humidity low [%]
  , fcd_rh_hgh             :: !Double  -- ^ relative humidity high [%]
  , fcd_bt_min             :: !Double  -- ^ btran minimum
  , fcd_bt_max             :: !Double  -- ^ btran maximum
  , fcd_cli_scale          :: !Double  -- ^ deforestation fire constant [/d]
  , fcd_boreal_peatfire_c  :: !Double  -- ^ boreal peatland fire param [/hr]
  , fcd_pot_hmn_ign_counts_alpha :: !Double
  , fcd_non_boreal_peatfire_c    :: !Double
  , fcd_cropfire_a1              :: !Double
  , fcd_occur_hi_gdp_tree        :: !Double
  , fcd_cmb_cmplt_fact_litter    :: !Double
  , fcd_cmb_cmplt_fact_cwd       :: !Double
  } deriving (Show)

defaultFireConst :: CNFireConstData
defaultFireConst = CNFireConstData
  { fcd_borealat = 40.0, fcd_lfuel = 75.0, fcd_ufuel = 650.0
  , fcd_g0 = 0.05, fcd_rh_low = 30.0, fcd_rh_hgh = 80.0
  , fcd_bt_min = 0.3, fcd_bt_max = 0.7, fcd_cli_scale = 0.035
  , fcd_boreal_peatfire_c = 4.2e-5, fcd_pot_hmn_ign_counts_alpha = 0.0035
  , fcd_non_boreal_peatfire_c = 0.001, fcd_cropfire_a1 = 0.3
  , fcd_occur_hi_gdp_tree = 0.39
  , fcd_cmb_cmplt_fact_litter = 0.5, fcd_cmb_cmplt_fact_cwd = 0.25
  }

data CNFireParams = CNFireParams
  { cfp_prh30               :: !Double
  , cfp_ignition_efficiency :: !Double
  } deriving (Show)

data PftConFireBase = PftConFireBase
  { pfb_woody    :: !(VU.Vector Double)
  , pfb_cc_leaf  :: !(VU.Vector Double)
  , pfb_cc_lstem :: !(VU.Vector Double)
  , pfb_cc_dstem :: !(VU.Vector Double)
  , pfb_cc_other :: !(VU.Vector Double)
  , pfb_fm_leaf  :: !(VU.Vector Double)
  , pfb_fm_lstem :: !(VU.Vector Double)
  , pfb_fm_other :: !(VU.Vector Double)
  , pfb_fm_root  :: !(VU.Vector Double)
  , pfb_fm_lroot :: !(VU.Vector Double)
  , pfb_fm_droot :: !(VU.Vector Double)
  } deriving (Show)

-- =========================================================================
-- Root wetness (pure, single-patch)
-- =========================================================================

data SoilSuctionResult = SoilSuctionResult { ssr_smp :: !Double } deriving (Show)

-- | Default Clapp-Hornberger soil suction: smp = -sucsat * s^(-bsw)
defaultSoilSuction :: Double -> Double -> Double -> SoilSuctionResult
defaultSoilSuction sucsat bsw s_node =
  SoilSuctionResult { ssr_smp = negate sucsat * s_node ** (negate bsw) }

-- | Calculate root-weighted soil wetness for fire model (single patch).
-- Returns btran2 in [0,1].
calcFireRootWetness :: VU.Vector Double  -- ^ rootfr per layer
                    -> VU.Vector Double  -- ^ h2osoi_vol per layer
                    -> VU.Vector Double  -- ^ watsat per layer
                    -> VU.Vector Double  -- ^ sucsat per layer
                    -> VU.Vector Double  -- ^ bsw per layer
                    -> Double            -- ^ smpso (PFT)
                    -> Double            -- ^ smpsc (PFT)
                    -> Double            -- ^ btran2 result
calcFireRootWetness rootfr h2osoi_vol watsat sucsat bsw smpso smpsc =
  let nj = VU.length rootfr
      go !acc j
        | j >= nj   = acc
        | otherwise =
          let !s_node = max (h2osoi_vol VU.! j / watsat VU.! j) 0.01
              !smp = negate (sucsat VU.! j) * s_node ** (negate (bsw VU.! j))
              !smp_clamped = max smpsc smp
              !wt = max 0.0 (min ((smp_clamped - smpsc) / (smpso - smpsc)) 1.0)
          in go (acc + (rootfr VU.! j) * wt) (j + 1)
  in min 1.0 (go 0.0 0)

-- =========================================================================
-- Fire flux for single patch
-- =========================================================================

data FireFluxInput = FireFluxInput
  { ffi_f           :: !Double  -- ^ fractional area burned for this patch
  , ffi_cc_leaf     :: !Double  -- ^ combustion completeness: leaf
  , ffi_cc_lstem    :: !Double  -- ^ combustion completeness: live stem
  , ffi_cc_dstem    :: !Double  -- ^ combustion completeness: dead stem
  , ffi_cc_other    :: !Double  -- ^ combustion completeness: other pools
  , ffi_fm_leaf     :: !Double  -- ^ fire mortality: leaf
  , ffi_fm_droot    :: !Double  -- ^ fire mortality: dead root (also used for stems, bug 2516)
  , ffi_fm_root     :: !Double  -- ^ fire mortality: fine root
  , ffi_fm_lroot    :: !Double  -- ^ fire mortality: live coarse root
  , ffi_fm_lstem    :: !Double  -- ^ fire mortality: live stem
  , ffi_spinup_m    :: !Double  -- ^ spinup factor for deadwood
  , ffi_leafc       :: !Double
  , ffi_livestemc   :: !Double
  , ffi_deadstemc   :: !Double
  , ffi_frootc      :: !Double
  , ffi_livecrootc  :: !Double
  , ffi_deadcrootc  :: !Double
  , ffi_gresp_storage :: !Double
  , ffi_gresp_xfer    :: !Double
  } deriving (Show)

data FireFluxOutput = FireFluxOutput
  { ffo_m_leafc_to_fire      :: !Double
  , ffo_m_livestemc_to_fire  :: !Double
  , ffo_m_deadstemc_to_fire  :: !Double
  , ffo_m_frootc_to_fire     :: !Double
  , ffo_m_gresp_storage_to_fire :: !Double
  , ffo_m_gresp_xfer_to_fire    :: !Double
  -- Mortality to litter
  , ffo_m_leafc_to_litter_fire     :: !Double
  , ffo_m_livestemc_to_litter_fire :: !Double
  , ffo_m_livestemc_to_deadstemc_fire :: !Double
  , ffo_m_deadstemc_to_litter_fire :: !Double
  , ffo_m_frootc_to_litter_fire    :: !Double
  , ffo_m_livecrootc_to_litter_fire :: !Double
  , ffo_m_livecrootc_to_deadcrootc_fire :: !Double
  , ffo_m_deadcrootc_to_litter_fire :: !Double
  } deriving (Show)

-- | Compute fire emission and mortality fluxes for a single patch.
calcFireFluxPatch :: FireFluxInput -> FireFluxOutput
calcFireFluxPatch inp =
  let !f = ffi_f inp
      !m = ffi_spinup_m inp
  in FireFluxOutput
    { ffo_m_leafc_to_fire     = ffi_leafc inp * f * ffi_cc_leaf inp
    , ffo_m_livestemc_to_fire = ffi_livestemc inp * f * ffi_cc_lstem inp
    , ffo_m_deadstemc_to_fire = ffi_deadstemc inp * f * ffi_cc_dstem inp * m
    , ffo_m_frootc_to_fire    = ffi_frootc inp * f * 0.0
    , ffo_m_gresp_storage_to_fire = ffi_gresp_storage inp * f * ffi_cc_other inp
    , ffo_m_gresp_xfer_to_fire    = ffi_gresp_xfer inp * f * ffi_cc_other inp
    , ffo_m_leafc_to_litter_fire = ffi_leafc inp * f * (1.0 - ffi_cc_leaf inp) * ffi_fm_leaf inp
    , ffo_m_livestemc_to_litter_fire = ffi_livestemc inp * f * (1.0 - ffi_cc_lstem inp) * ffi_fm_droot inp
    , ffo_m_livestemc_to_deadstemc_fire = ffi_livestemc inp * f * (1.0 - ffi_cc_lstem inp) * (ffi_fm_lstem inp - ffi_fm_droot inp)
    , ffo_m_deadstemc_to_litter_fire = ffi_deadstemc inp * f * m * (1.0 - ffi_cc_dstem inp) * ffi_fm_droot inp
    , ffo_m_frootc_to_litter_fire = ffi_frootc inp * f * ffi_fm_root inp
    , ffo_m_livecrootc_to_litter_fire = ffi_livecrootc inp * f * ffi_fm_droot inp
    , ffo_m_livecrootc_to_deadcrootc_fire = ffi_livecrootc inp * f * (ffi_fm_lroot inp - ffi_fm_droot inp)
    , ffo_m_deadcrootc_to_litter_fire = ffi_deadcrootc inp * f * m * ffi_fm_droot inp
    }

-- =========================================================================
-- Decomp pool fire loss (column, single pool/level)
-- =========================================================================

data DecompFireLossInput = DecompFireLossInput
  { dfli_pool_vr        :: !Double  -- ^ current pool value [gC/m3]
  , dfli_farea_burned    :: !Double
  , dfli_baf_crop        :: !Double
  , dfli_is_litter       :: !Bool
  , dfli_is_cwd          :: !Bool
  , dfli_cmb_cmplt_litter :: !Double
  , dfli_cmb_cmplt_cwd   :: !Double
  } deriving (Show)

data DecompFireLossOutput = DecompFireLossOutput
  { dflo_fire_loss :: !Double  -- ^ fire loss flux [gC/m3/s]
  } deriving (Show)

calcDecompFireLoss :: DecompFireLossInput -> DecompFireLossOutput
calcDecompFireLoss inp
  | dfli_is_litter inp =
      DecompFireLossOutput { dflo_fire_loss = dfli_pool_vr inp * dfli_farea_burned inp * dfli_cmb_cmplt_litter inp }
  | dfli_is_cwd inp =
      DecompFireLossOutput { dflo_fire_loss = dfli_pool_vr inp * (dfli_farea_burned inp - dfli_baf_crop inp) * dfli_cmb_cmplt_cwd inp }
  | otherwise =
      DecompFireLossOutput { dflo_fire_loss = 0.0 }

-- =========================================================================
-- Peat fire SOM loss
-- =========================================================================

-- | Calculate SOM C loss to peat fire for a single column.
calcSomcFire :: Double  -- ^ latitude [deg]
             -> Double  -- ^ borealat threshold
             -> Double  -- ^ totsomc [gC/m2]
             -> Double  -- ^ baf_peatf
             -> Double  -- ^ somc_fire [gC/m2/s]
calcSomcFire lat borealat totsomc baf_peatf
  | lat < borealat = totsomc * baf_peatf * 6.0 / 33.9
  | otherwise      = baf_peatf * 2.2e3

-- =========================================================================
-- Column-scalar fire application (combustion + mortality, C/N conserving)
-- =========================================================================

-- | Inputs for the column-scalar fire C/N update. The fractional burned area
-- 'cfi_farea_burned' drives combustion of the live/dead vegetation pools (via
-- 'calcFireFluxPatch') and of the litter/CWD decomposition pools (via
-- 'calcDecompFireLoss'). All pools are gC/m2 (C) or gN/m2 (N); the column is
-- treated as a single representative patch with the supplied combustion
-- completeness and fire-mortality factors.
data ColumnFireInput = ColumnFireInput
  { cfi_farea_burned :: !Double   -- ^ fractional area burned this step [0-1]
  , cfi_dt           :: !Double   -- ^ timestep [s]
  , cfi_leafc        :: !Double   -- ^ leaf C [gC/m2]
  , cfi_frootc       :: !Double   -- ^ fine-root C [gC/m2]
  , cfi_livestemc    :: !Double   -- ^ live-stem C [gC/m2]
  , cfi_deadstemc    :: !Double   -- ^ dead-stem C [gC/m2]
  , cfi_litterc      :: !Double   -- ^ litter C [gC/m2]
  , cfi_somc         :: !Double   -- ^ soil-organic / CWD C [gC/m2]
  , cfi_leafn        :: !Double   -- ^ leaf N [gN/m2]
  , cfi_sminn        :: !Double   -- ^ soil mineral N [gN/m2]
  , cfi_const        :: !CNFireConstData
  } deriving (Show)

-- | Result of the column-scalar fire update. Pools are post-fire; the
-- combustion losses (carbon emitted as CO2/fire emissions to the atmosphere,
-- and the corresponding nitrogen) are returned separately so the caller can
-- track them as a closure term. Total C is conserved as:
--   sum(post pools) + cfr_c_to_atm == sum(pre pools)
-- and likewise for N with cfr_n_to_atm.
data ColumnFireResult = ColumnFireResult
  { cfr_leafc     :: !Double
  , cfr_frootc    :: !Double
  , cfr_livestemc :: !Double
  , cfr_deadstemc :: !Double
  , cfr_litterc   :: !Double
  , cfr_somc      :: !Double
  , cfr_leafn     :: !Double
  , cfr_sminn     :: !Double
  , cfr_c_to_atm  :: !Double   -- ^ C combusted to atmosphere this step [gC/m2]
  , cfr_n_to_atm  :: !Double   -- ^ N volatilized to atmosphere this step [gN/m2]
  } deriving (Show)

-- | Apply the Li2014/CNFireBase fire fluxes to the column-scalar C/N pools.
--
-- Vegetation combustion + mortality come from the ported 'calcFireFluxPatch'
-- (CNFireBaseMod CNFireFluxes), driven by the burned-area fraction. Litter and
-- CWD combustion come from the ported 'calcDecompFireLoss' (the
-- @m_decomp_cpools_to_fire_vr@ term in CNFireBaseMod). The combusted carbon is
-- emitted to the atmosphere (CO2 / fire emissions); the non-combusted killed
-- vegetation is transferred to the litter (fine) and soil-organic / CWD
-- (woody) pools. Nitrogen follows the carbon stoichiometrically (leaf N is
-- combusted in proportion to leaf C; the surviving leaf N mineralizes to
-- @sminn@), conserving total N.
applyColumnFireFluxes :: ColumnFireInput -> ColumnFireResult
applyColumnFireFluxes inp =
  let !fc    = cfi_const inp
      !f     = max 0.0 (min 1.0 (cfi_farea_burned inp))
      !leafc = cfi_leafc inp

      -- Vegetation combustion + mortality (single representative patch).
      -- Li2014 combustion completeness / fire-mortality factors for a
      -- generic vegetated patch (cc_* in [0,1], fm_* in [0,1]).
      !veg = calcFireFluxPatch FireFluxInput
        { ffi_f             = f
        , ffi_cc_leaf       = 0.8
        , ffi_cc_lstem      = 0.3
        , ffi_cc_dstem      = 0.3
        , ffi_cc_other      = 0.5
        , ffi_fm_leaf       = 1.0
        , ffi_fm_droot      = 1.0
        , ffi_fm_root       = 0.5
        , ffi_fm_lroot      = 1.0
        , ffi_fm_lstem      = 1.0
        , ffi_spinup_m      = 1.0
        , ffi_leafc         = leafc
        , ffi_livestemc     = cfi_livestemc inp
        , ffi_deadstemc     = cfi_deadstemc inp
        , ffi_frootc        = cfi_frootc inp
        , ffi_livecrootc    = 0.0
        , ffi_deadcrootc    = 0.0
        , ffi_gresp_storage = 0.0
        , ffi_gresp_xfer    = 0.0
        }

      -- C combusted to the atmosphere from vegetation.
      !leafc_to_fire  = ffo_m_leafc_to_fire veg
      !lstemc_to_fire = ffo_m_livestemc_to_fire veg
      !dstemc_to_fire = ffo_m_deadstemc_to_fire veg
      !frootc_to_fire = ffo_m_frootc_to_fire veg

      -- C killed but transferred to the dead-organic-matter pools (litter / CWD).
      !leafc_to_litter   = ffo_m_leafc_to_litter_fire veg
      !frootc_to_litter  = ffo_m_frootc_to_litter_fire veg
      !lstemc_to_litter  = ffo_m_livestemc_to_litter_fire veg
      !dstemc_to_litter  = ffo_m_deadstemc_to_litter_fire veg
      !lstemc_to_dstemc  = ffo_m_livestemc_to_deadstemc_fire veg

      -- Litter & CWD combustion to atmosphere (CNFireBaseMod m_decomp_to_fire).
      -- Litter (fine soil organic-matter analogue) and CWD (coarse-woody-debris
      -- analogue, carried by the soil-organic pool) burn with distinct
      -- combustion completeness factors; the loss is a flux [gC/m2/s] integrated
      -- over the timestep.
      !litterLoss = dflo_fire_loss (calcDecompFireLoss DecompFireLossInput
        { dfli_pool_vr          = cfi_litterc inp
        , dfli_farea_burned     = f
        , dfli_baf_crop         = 0.0
        , dfli_is_litter        = True
        , dfli_is_cwd           = False
        , dfli_cmb_cmplt_litter = fcd_cmb_cmplt_fact_litter fc
        , dfli_cmb_cmplt_cwd    = fcd_cmb_cmplt_fact_cwd fc
        }) * cfi_dt inp
      !cwdLoss = dflo_fire_loss (calcDecompFireLoss DecompFireLossInput
        { dfli_pool_vr          = cfi_somc inp
        , dfli_farea_burned     = f
        , dfli_baf_crop         = 0.0
        , dfli_is_litter        = False
        , dfli_is_cwd           = True
        , dfli_cmb_cmplt_litter = fcd_cmb_cmplt_fact_litter fc
        , dfli_cmb_cmplt_cwd    = fcd_cmb_cmplt_fact_cwd fc
        }) * cfi_dt inp

      -- Total carbon combusted to the atmosphere this step.
      !c_to_atm = leafc_to_fire + lstemc_to_fire + dstemc_to_fire
                + frootc_to_fire + litterLoss + cwdLoss

      -- Updated carbon pools. Live pools lose both combusted and mortality
      -- carbon; the mortality carbon lands in litter (fine) and soil-organic
      -- / CWD (woody) pools, conserving total C against c_to_atm.
      !leafc'     = cfi_leafc inp     - leafc_to_fire  - leafc_to_litter
      !frootc'    = cfi_frootc inp    - frootc_to_fire - frootc_to_litter
      !livestemc' = cfi_livestemc inp - lstemc_to_fire - lstemc_to_litter - lstemc_to_dstemc
      !deadstemc' = cfi_deadstemc inp - dstemc_to_fire - dstemc_to_litter + lstemc_to_dstemc
      !litterc'   = cfi_litterc inp   - litterLoss + leafc_to_litter + frootc_to_litter
      !somc'      = cfi_somc inp      - cwdLoss + lstemc_to_litter + dstemc_to_litter

      -- Nitrogen: leaf N follows leaf C stoichiometrically. The combusted leaf
      -- C fraction volatilizes the matching leaf N to the atmosphere; the
      -- mortality (litter) fraction of leaf N mineralizes into sminn (the
      -- scalar path carries no separate litter-N pool). Total N is conserved
      -- between leafn + sminn + n_to_atm.
      !leafCloss   = leafc_to_fire + leafc_to_litter
      !leafn_frac  = if leafc > 0.0 then min 1.0 (leafCloss / leafc) else 0.0
      !leafn_lost  = cfi_leafn inp * leafn_frac
      !combustFrac = if leafCloss > 0.0 then leafc_to_fire / leafCloss else 0.0
      !n_to_atm    = leafn_lost * combustFrac
      !n_to_sminn  = leafn_lost - n_to_atm
      !leafn'      = cfi_leafn inp - leafn_lost
      !sminn'      = cfi_sminn inp + n_to_sminn

  in ColumnFireResult
    { cfr_leafc     = max 0.0 leafc'
    , cfr_frootc    = max 0.0 frootc'
    , cfr_livestemc = max 0.0 livestemc'
    , cfr_deadstemc = max 0.0 deadstemc'
    , cfr_litterc   = max 0.0 litterc'
    , cfr_somc      = max 0.0 somc'
    , cfr_leafn     = max 0.0 leafn'
    , cfr_sminn     = max 0.0 sminn'
    , cfr_c_to_atm  = c_to_atm
    , cfr_n_to_atm  = n_to_atm
    }
