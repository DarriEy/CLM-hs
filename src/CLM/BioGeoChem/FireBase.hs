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
