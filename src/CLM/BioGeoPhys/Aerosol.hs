-- | Aerosol mass tracking and deposition fluxes for SNICAR snow-impurity
-- radiative transfer.
-- Fortran: AerosolMod.F90
-- Julia:   src/biogeophys/aerosol.jl
--
-- Pure functions operating on immutable records.
-- Column-level aerosol mass arrays are indexed 1..nlevsno where
-- Fortran snow layer j in [-nlevsno+1, 0] maps to index j + nlevsno.
--
module CLM.BioGeoPhys.Aerosol
  ( -- * Data types
    AerosolLayerData(..)
  , defaultAerosolLayer
  , AerosolColumnData(..)
  , defaultAerosolColumn
  , AerosolDepFluxes(..)
  , defaultAerosolDepFluxes
    -- * Functions
  , aerosolMassConcentrations
  , aerosolFluxesFromForcing
  , zeroAerosolLayer
  ) where

import qualified Data.Vector.Unboxed as VU
import CLM.Constants.PhysicalConstants (nlevsno)

-- ========================================================================
-- Data types
-- ========================================================================

-- | Aerosol mass data for a single snow layer.
data AerosolLayerData = AerosolLayerData
  { al_mss_bcpho     :: !Double  -- ^ Hydrophobic BC mass [kg]
  , al_mss_bcphi     :: !Double  -- ^ Hydrophilic BC mass [kg]
  , al_mss_ocpho     :: !Double  -- ^ Hydrophobic OC mass [kg]
  , al_mss_ocphi     :: !Double  -- ^ Hydrophilic OC mass [kg]
  , al_mss_dst1      :: !Double  -- ^ Dust species 1 mass [kg]
  , al_mss_dst2      :: !Double  -- ^ Dust species 2 mass [kg]
  , al_mss_dst3      :: !Double  -- ^ Dust species 3 mass [kg]
  , al_mss_dst4      :: !Double  -- ^ Dust species 4 mass [kg]
  } deriving (Show, Eq)

defaultAerosolLayer :: AerosolLayerData
defaultAerosolLayer = AerosolLayerData 0 0 0 0 0 0 0 0

-- | Zero out all masses in a layer.
zeroAerosolLayer :: AerosolLayerData
zeroAerosolLayer = defaultAerosolLayer

-- | Column-integrated and top-layer aerosol diagnostics.
data AerosolColumnData = AerosolColumnData
  { ac_mss_bc_col  :: !Double  -- ^ Column-integrated BC [kg]
  , ac_mss_bc_top  :: !Double  -- ^ Top-layer BC [kg]
  , ac_mss_oc_col  :: !Double  -- ^ Column-integrated OC [kg]
  , ac_mss_oc_top  :: !Double  -- ^ Top-layer OC [kg]
  , ac_mss_dst_col :: !Double  -- ^ Column-integrated dust [kg]
  , ac_mss_dst_top :: !Double  -- ^ Top-layer dust [kg]
  } deriving (Show, Eq)

defaultAerosolColumn :: AerosolColumnData
defaultAerosolColumn = AerosolColumnData 0 0 0 0 0 0

-- | Aerosol deposition fluxes for a single column [kg/m2/s].
data AerosolDepFluxes = AerosolDepFluxes
  { ad_flx_bc_dep_dry  :: !Double
  , ad_flx_bc_dep_wet  :: !Double
  , ad_flx_bc_dep_phi  :: !Double
  , ad_flx_bc_dep_pho  :: !Double
  , ad_flx_bc_dep      :: !Double
  , ad_flx_oc_dep_dry  :: !Double
  , ad_flx_oc_dep_wet  :: !Double
  , ad_flx_oc_dep_phi  :: !Double
  , ad_flx_oc_dep_pho  :: !Double
  , ad_flx_oc_dep      :: !Double
  , ad_flx_dst_dep_dry1 :: !Double
  , ad_flx_dst_dep_wet1 :: !Double
  , ad_flx_dst_dep_dry2 :: !Double
  , ad_flx_dst_dep_wet2 :: !Double
  , ad_flx_dst_dep_dry3 :: !Double
  , ad_flx_dst_dep_wet3 :: !Double
  , ad_flx_dst_dep_dry4 :: !Double
  , ad_flx_dst_dep_wet4 :: !Double
  , ad_flx_dst_dep      :: !Double
  } deriving (Show, Eq)

defaultAerosolDepFluxes :: AerosolDepFluxes
defaultAerosolDepFluxes = AerosolDepFluxes
  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

-- ========================================================================
-- Mass concentration computation
-- ========================================================================

-- | Aerosol mass concentration result for a single active layer.
data AerosolMassCnc = AerosolMassCnc
  { amc_mss_bctot     :: !Double  -- ^ Total BC [kg]
  , amc_mss_octot     :: !Double  -- ^ Total OC [kg]
  , amc_mss_dsttot    :: !Double  -- ^ Total dust [kg]
  , amc_cnc_bcphi     :: !Double  -- ^ BC hydrophilic concentration [kg/kg]
  , amc_cnc_bcpho     :: !Double  -- ^ BC hydrophobic concentration [kg/kg]
  , amc_cnc_ocphi     :: !Double  -- ^ OC hydrophilic concentration [kg/kg]
  , amc_cnc_ocpho     :: !Double  -- ^ OC hydrophobic concentration [kg/kg]
  , amc_cnc_dst1      :: !Double  -- ^ Dust 1 concentration [kg/kg]
  , amc_cnc_dst2      :: !Double  -- ^ Dust 2 concentration [kg/kg]
  , amc_cnc_dst3      :: !Double  -- ^ Dust 3 concentration [kg/kg]
  , amc_cnc_dst4      :: !Double  -- ^ Dust 4 concentration [kg/kg]
  } deriving (Show)

-- | Compute mass concentrations for a single active snow layer.
--
-- @snowmass@ is the total snow (ice + liquid) in the layer [kg/m2].
aerosolMassConcentrations :: AerosolLayerData -> Double -> AerosolMassCnc
aerosolMassConcentrations lyr snowmass =
  let bctot = al_mss_bcpho lyr + al_mss_bcphi lyr
      octot = al_mss_ocpho lyr + al_mss_ocphi lyr
      dsttot = al_mss_dst1 lyr + al_mss_dst2 lyr
             + al_mss_dst3 lyr + al_mss_dst4 lyr
      inv = if snowmass > 0.0 then 1.0 / snowmass else 0.0
  in AerosolMassCnc
       { amc_mss_bctot  = bctot
       , amc_mss_octot  = octot
       , amc_mss_dsttot = dsttot
       , amc_cnc_bcphi  = al_mss_bcphi lyr * inv
       , amc_cnc_bcpho  = al_mss_bcpho lyr * inv
       , amc_cnc_ocphi  = al_mss_ocphi lyr * inv
       , amc_cnc_ocpho  = al_mss_ocpho lyr * inv
       , amc_cnc_dst1   = al_mss_dst1 lyr * inv
       , amc_cnc_dst2   = al_mss_dst2 lyr * inv
       , amc_cnc_dst3   = al_mss_dst3 lyr * inv
       , amc_cnc_dst4   = al_mss_dst4 lyr * inv
       }

-- ========================================================================
-- Deposition flux computation
-- ========================================================================

-- | Compute aerosol deposition fluxes from a 14-element forcing array.
--
-- The forcing array maps:
--   1 = BC dry phi, 2 = BC dry pho, 3 = BC wet,
--   4 = OC dry phi, 5 = OC dry pho, 6 = OC wet,
--   7 = dst1 wet, 8 = dst1 dry, 9 = dst2 wet, 10 = dst2 dry,
--  11 = dst3 wet, 12 = dst3 dry, 13 = dst4 wet, 14 = dst4 dry
--
-- Ported from @AerosolFluxes@ in @AerosolMod.F90@.
aerosolFluxesFromForcing :: VU.Vector Double  -- ^ 14-element forcing [kg/m2/s]
                         -> Bool              -- ^ snicar_use_aerosol
                         -> AerosolDepFluxes
aerosolFluxesFromForcing forc useAer =
  let f i = forc VU.! i
      raw = AerosolDepFluxes
        { ad_flx_bc_dep_dry  = f 0 + f 1
        , ad_flx_bc_dep_wet  = f 2
        , ad_flx_bc_dep_phi  = f 0 + f 2
        , ad_flx_bc_dep_pho  = f 1
        , ad_flx_bc_dep      = f 0 + f 1 + f 2
        , ad_flx_oc_dep_dry  = f 3 + f 4
        , ad_flx_oc_dep_wet  = f 5
        , ad_flx_oc_dep_phi  = f 3 + f 5
        , ad_flx_oc_dep_pho  = f 4
        , ad_flx_oc_dep      = f 3 + f 4 + f 5
        , ad_flx_dst_dep_wet1 = f 6
        , ad_flx_dst_dep_dry1 = f 7
        , ad_flx_dst_dep_wet2 = f 8
        , ad_flx_dst_dep_dry2 = f 9
        , ad_flx_dst_dep_wet3 = f 10
        , ad_flx_dst_dep_dry3 = f 11
        , ad_flx_dst_dep_wet4 = f 12
        , ad_flx_dst_dep_dry4 = f 13
        , ad_flx_dst_dep      = f 6 + f 7 + f 8 + f 9 + f 10 + f 11 + f 12 + f 13
        }
  in if useAer then raw else defaultAerosolDepFluxes
