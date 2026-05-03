{-# LANGUAGE BangPatterns #-}
-- | Dust emission base module.
-- Fortran: DustEmisBase.F90
-- Julia:   src/biogeochem/dust_emission.jl
--
-- Simulates dust mobilization due to wind from the surface into the
-- lowest atmospheric layer. Contains dust size distribution parameters,
-- overlap factor computation, saltation factor, and Stokes correction.
module CLM.BioGeoChem.DustEmission
  ( -- * Constants
    dnsAer
  , ndst
  , dstSrcNbr
    -- * Data types
  , DustEmisBaseData(..)
    -- * Size distribution helpers
  , DustSizeParams(..)
  , defaultDustSizeParams
  , calcOverlapFactor
  , calcSaltationFactor
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Approximation of the error function (Abramowitz and Stegun 7.1.26).
erf :: Double -> Double
erf x
  | x < 0     = negate (erf (negate x))
  | otherwise  =
    let !t = 1.0 / (1.0 + 0.3275911 * x)
        !poly = t * (0.254829592
                + t * (-0.284496736
                + t * (1.421413741
                + t * (-1.453152027
                + t * 1.061405429))))
    in 1.0 - poly * exp (negate (x * x))

-- | Aerosol density [kg/m3]
dnsAer :: Double
dnsAer = 2.5e3

-- | Number of dust size bins
ndst :: Int
ndst = 4

-- | Number of source modes
dstSrcNbr :: Int
dstSrcNbr = 3

-- =========================================================================
-- DustEmisBaseData
-- =========================================================================

-- | Dust emission state and flux data structure.
-- Ported from @dust_emis_base_type@ in @DustEmisBase.F90@.
data DustEmisBaseData = DustEmisBaseData
  { deb_ovr_src_snk_mss :: !(VU.Vector Double)  -- ^ overlap factors [dst_src_nbr * ndst] (row-major)
  , deb_dmt_vwr         :: !(VU.Vector Double)  -- ^ mass-weighted mean diameter [ndst]
  , deb_stk_crc         :: !(VU.Vector Double)  -- ^ Stokes correction [ndst]
  , deb_saltation_factor :: !Double
  } deriving (Show)

-- =========================================================================
-- Size distribution parameters
-- =========================================================================

data DustSizeParams = DustSizeParams
  { dsp_dmt_vma_src :: !(VU.Vector Double)  -- ^ volume median diameter per source mode [m]
  , dsp_gsd_anl_src :: !(VU.Vector Double)  -- ^ geometric std dev per source mode
  , dsp_mss_frc_src :: !(VU.Vector Double)  -- ^ mass fraction per source mode
  , dsp_dmt_grd     :: !(VU.Vector Double)  -- ^ particle diameter grid boundaries [m] (ndst+1)
  } deriving (Show)

defaultDustSizeParams :: DustSizeParams
defaultDustSizeParams = DustSizeParams
  { dsp_dmt_vma_src = VU.fromList [0.832e-6, 4.82e-6, 19.38e-6]
  , dsp_gsd_anl_src = VU.fromList [2.10, 1.90, 1.60]
  , dsp_mss_frc_src = VU.fromList [0.036, 0.957, 0.007]
  , dsp_dmt_grd     = VU.fromList [0.1e-6, 1.0e-6, 2.5e-6, 5.0e-6, 10.0e-6]
  }

-- | Calculate overlap factor between source mode m and sink bin n.
-- Uses the error function (erf) from the lognormal distribution.
calcOverlapFactor :: Double  -- ^ dmt_vma_src [m]
                  -> Double  -- ^ gsd_anl_src
                  -> Double  -- ^ mss_frc_src
                  -> Double  -- ^ dmt_min [m] (lower bin boundary)
                  -> Double  -- ^ dmt_max [m] (upper bin boundary)
                  -> Double
calcOverlapFactor dmt_vma gsd mss_frc dmt_min dmt_max =
  let !sqrt2lngsdi = sqrt 2.0 * log gsd
      !lnmax = log (dmt_max / dmt_vma)
      !lnmin = log (dmt_min / dmt_vma)
      !ovr_frc = 0.5 * (erf (lnmax / sqrt2lngsdi) - erf (lnmin / sqrt2lngsdi))
  in ovr_frc * mss_frc

-- | Calculate saltation factor from optimal saltation diameter.
calcSaltationFactor :: Double  -- ^ dmt_slt_opt [m] (typically 75e-6)
                    -> Double  -- ^ dns_slt [kg/m3] (typically 2650)
                    -> Double  -- ^ gravity [m/s2]
                    -> Double  -- ^ air density [kg/m3]
                    -> Double  -- ^ kinematic viscosity [m2/s]
                    -> Double  -- ^ saltation factor
calcSaltationFactor dmt dns_slt grav rho_air nu =
  let !ryn = 0.38 + 1331.0 * (100.0 * dmt) ** 1.56
      !wnd_frc_thr_slt
            | ryn < 10.0 = 0.129 * (1.928 * ryn ** 0.092 - 1.0)
                          ** 0.5 / (1.0 - 0.0858 * exp (-0.0617 * (ryn - 10.0)))
                          * sqrt (dns_slt * grav * dmt / rho_air)
            | otherwise  = 0.129 * (1.0 + 6.0e-7 / (dns_slt * grav * dmt ** 2.5))
                          ** 0.5 * sqrt (dns_slt * grav * dmt / rho_air)
  in wnd_frc_thr_slt
