{-# LANGUAGE BangPatterns #-}
-- | Methane biogeochemistry module.
-- Fortran: ch4Mod.F90
-- Julia:   src/biogeochem/methane.jl
--
-- Contains methane control flags, parameters, production/oxidation/transport
-- constants, and pure helper functions for CH4 processes.
module CLM.BioGeoChem.Methane
  ( -- * Data types
    CH4VarCon(..)
  , defaultCH4VarCon
  , CH4Params(..)
  , defaultCH4Params
    -- * Constants
  , rgasLatm
    -- * Pure helpers
  , ch4ProdQ10Factor
  , ch4OxidRate
  , ch4EbullitionCheck
  ) where

-- | Non-tunable constant: L.atm/mol.K
rgasLatm :: Double
rgasLatm = 0.0821

-- =========================================================================
-- CH4VarCon — Methane control flags
-- =========================================================================

-- | Methane model control flags and configuration.
-- Ported from @ch4varcon@ module in CLM Fortran.
data CH4VarCon = CH4VarCon
  { ch4vc_allowlakeprod       :: !Bool    -- ^ allow CH4 production in lakes
  , ch4vc_replenishlakec      :: !Bool    -- ^ replenish lake soil C
  , ch4vc_anoxicmicrosites    :: !Bool    -- ^ CH4 production above WT in anoxic microsites
  , ch4vc_usephfact           :: !Bool    -- ^ use pH factor for CH4 production
  , ch4vc_ch4rmcnlim          :: !Bool    -- ^ remove CN N limitation for methanogenesis
  , ch4vc_ch4offline          :: !Bool    -- ^ use prescribed atmospheric CH4
  , ch4vc_transpirationloss   :: !Bool    -- ^ include transpiration CH4 loss
  , ch4vc_use_aereoxid_prog   :: !Bool    -- ^ prognostic aerenchyma oxidation via O2
  , ch4vc_ch4frzout           :: !Bool    -- ^ freeze-out effect on CH4 diffusion
  , ch4vc_finundation_mtd_h2osfc :: !Int  -- ^ finundation method h2osfc
  } deriving (Show)

defaultCH4VarCon :: CH4VarCon
defaultCH4VarCon = CH4VarCon
  { ch4vc_allowlakeprod = False
  , ch4vc_replenishlakec = True
  , ch4vc_anoxicmicrosites = True
  , ch4vc_usephfact = False
  , ch4vc_ch4rmcnlim = True
  , ch4vc_ch4offline = True
  , ch4vc_transpirationloss = True
  , ch4vc_use_aereoxid_prog = False
  , ch4vc_ch4frzout = False
  , ch4vc_finundation_mtd_h2osfc = 1
  }

-- =========================================================================
-- CH4Params — Methane model parameters
-- =========================================================================

-- | Methane model parameters read from parameter file.
-- Ported from @params_type@ in @ch4Mod.F90@.
data CH4Params = CH4Params
  { ch4p_q10ch4              :: !Double  -- ^ additional Q10 for CH4 production
  , ch4p_q10ch4base          :: !Double  -- ^ temperature at which f_ch4 = constant (K)
  , ch4p_f_ch4               :: !Double  -- ^ ratio of CH4 production to total C mineralization
  , ch4p_rootlitfrac         :: !Double  -- ^ fraction of SOM associated with roots
  , ch4p_cnscalefactor       :: !Double  -- ^ scale factor on CN decomp for CH4 flux
  , ch4p_redoxlag            :: !Double  -- ^ days to lag finundated_lag
  , ch4p_lake_decomp_fact    :: !Double  -- ^ base decomposition rate (1/s) at 25C
  , ch4p_redoxlag_vertical   :: !Double  -- ^ time lag (days) for newly unsaturated layers
  , ch4p_pHmax               :: !Double
  , ch4p_pHmin               :: !Double
  , ch4p_oxinhib             :: !Double  -- ^ inhibition by oxygen (m3/mol)
  , ch4p_vmax_ch4_oxid       :: !Double  -- ^ oxidation rate constant [mol/m3-w/s]
  , ch4p_k_m                 :: !Double  -- ^ Michaelis-Menten for CH4
  , ch4p_q10_ch4oxid         :: !Double
  , ch4p_smp_crit            :: !Double  -- ^ critical soil moisture potential (mm)
  , ch4p_k_m_o2              :: !Double  -- ^ Michaelis-Menten for O2
  , ch4p_k_m_unsat           :: !Double
  , ch4p_vmax_oxid_unsat     :: !Double
  , ch4p_aereoxid            :: !Double  -- ^ fraction of CH4 in aerenchyma oxidized
  , ch4p_scale_factor_aere   :: !Double
  , ch4p_nongrassporosratio  :: !Double
  , ch4p_unsat_aere_ratio    :: !Double
  , ch4p_porosmin            :: !Double
  , ch4p_vgc_max             :: !Double  -- ^ saturation pressure ratio for ebullition
  , ch4p_satpow              :: !Double
  , ch4p_scale_factor_gasdiff :: !Double
  , ch4p_scale_factor_liqdiff :: !Double
  , ch4p_capthick            :: !Double  -- ^ min thickness before h2osfc impermeable (mm)
  , ch4p_f_sat               :: !Double
  , ch4p_qflxlagd            :: !Double
  , ch4p_highlatfact         :: !Double
  , ch4p_q10lakebase         :: !Double
  , ch4p_atmch4              :: !Double  -- ^ atmospheric CH4 mixing ratio (mol/mol)
  , ch4p_rob                 :: !Double  -- ^ ratio of root length to vertical depth
  , ch4p_om_frac_sf          :: !Double
  } deriving (Show)

defaultCH4Params :: CH4Params
defaultCH4Params = CH4Params
  { ch4p_q10ch4 = 1.5, ch4p_q10ch4base = 295.0, ch4p_f_ch4 = 0.2
  , ch4p_rootlitfrac = 0.5, ch4p_cnscalefactor = 1.0, ch4p_redoxlag = 30.0
  , ch4p_lake_decomp_fact = 2.0e-8, ch4p_redoxlag_vertical = 30.0
  , ch4p_pHmax = 9.0, ch4p_pHmin = 2.2, ch4p_oxinhib = 10.0
  , ch4p_vmax_ch4_oxid = 0.0125, ch4p_k_m = 5.0e-3, ch4p_q10_ch4oxid = 1.9
  , ch4p_smp_crit = -2.4e5, ch4p_k_m_o2 = 2.0e-2, ch4p_k_m_unsat = 5.0e-3
  , ch4p_vmax_oxid_unsat = 1.25e-3
  , ch4p_aereoxid = 0.5, ch4p_scale_factor_aere = 1.0
  , ch4p_nongrassporosratio = 0.33, ch4p_unsat_aere_ratio = 0.167
  , ch4p_porosmin = 0.05, ch4p_vgc_max = 0.15
  , ch4p_satpow = 2.0, ch4p_scale_factor_gasdiff = 1.0
  , ch4p_scale_factor_liqdiff = 1.0, ch4p_capthick = 100.0
  , ch4p_f_sat = 0.95, ch4p_qflxlagd = 30.0, ch4p_highlatfact = 2.0
  , ch4p_q10lakebase = 298.0, ch4p_atmch4 = 1.7e-6, ch4p_rob = 3.0
  , ch4p_om_frac_sf = 1.0
  }

-- =========================================================================
-- Pure helper functions
-- =========================================================================

-- | Q10-based temperature factor for CH4 production.
ch4ProdQ10Factor :: Double  -- ^ q10ch4
                 -> Double  -- ^ q10ch4base [K]
                 -> Double  -- ^ temperature [K]
                 -> Double
ch4ProdQ10Factor q10 base t = q10 ** ((t - base) / 10.0)

-- | Michaelis-Menten CH4 oxidation rate.
ch4OxidRate :: Double  -- ^ vmax [mol/m3-w/s]
            -> Double  -- ^ conc_ch4 [mol/m3]
            -> Double  -- ^ k_m [mol/m3]
            -> Double  -- ^ conc_o2 [mol/m3]
            -> Double  -- ^ k_m_o2 [mol/m3]
            -> Double
ch4OxidRate vmax conc_ch4 k_m conc_o2 k_m_o2 =
  vmax * conc_ch4 / (k_m + conc_ch4) * conc_o2 / (k_m_o2 + conc_o2)

-- | Check if dissolved CH4 exceeds ebullition threshold.
ch4EbullitionCheck :: Double  -- ^ conc_ch4 [mol/m3]
                   -> Double  -- ^ vgc_max (saturation ratio threshold)
                   -> Double  -- ^ temperature [K]
                   -> Bool
ch4EbullitionCheck conc_ch4 vgc_max t =
  let !pCH4 = conc_ch4 * rgasLatm * t
  in pCH4 > vgc_max
