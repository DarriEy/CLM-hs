{-# LANGUAGE BangPatterns #-}
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
  , AerosolMassCnc(..)
    -- * Core functions
  , aerosolMassConcentrations
  , aerosolFluxesFromForcing
  , zeroAerosolLayer
    -- * Aerosol mass tracking (from Fortran AerosolMasses)
  , AerosolMassesInput(..)
  , AerosolMassesOutput(..)
  , aerosolMassesColumn
    -- * Washout during melt percolation
  , AerosolWashoutInput(..)
  , AerosolWashoutOutput(..)
  , aerosolWashout
    -- * Deposition to top snow layer
  , aerosolDepositionToSnow
    -- * Inter-layer mixing during compaction
  , aerosolCompactionMix
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

-- ========================================================================
-- Column-integrated aerosol masses (Fortran: AerosolMasses)
-- ========================================================================

data AerosolMassesInput = AerosolMassesInput
  { ami_snl          :: !Int                       -- ^ number of snow layers (negative)
  , ami_layers       :: ![AerosolLayerData] -- ^ per-layer aerosol (nlevsno)
  , ami_h2osoi_ice   :: !(VU.Vector Double)        -- ^ ice in each snow layer (kg/m2)
  , ami_h2osoi_liq   :: !(VU.Vector Double)        -- ^ liquid in each snow layer (kg/m2)
  } deriving (Show)

data AerosolMassesOutput = AerosolMassesOutput
  { amo_mss_bc_col   :: !Double  -- ^ total BC in column (kg)
  , amo_mss_oc_col   :: !Double  -- ^ total OC in column (kg)
  , amo_mss_dst_col  :: !Double  -- ^ total dust in column (kg)
  , amo_mss_bc_top   :: !Double  -- ^ BC in top snow layer (kg)
  , amo_mss_oc_top   :: !Double  -- ^ OC in top snow layer
  , amo_mss_dst_top  :: !Double  -- ^ dust in top snow layer
  , amo_h2osno_top   :: !Double  -- ^ snow mass in top layer (kg/m2)
  , amo_cnc_layers   :: ![AerosolMassCnc]  -- ^ concentrations per active layer
  } deriving (Show)

-- | Compute column-integrated aerosol masses and per-layer concentrations.
-- Ported from AerosolMasses in AerosolMod.F90.
aerosolMassesColumn :: AerosolMassesInput -> AerosolMassesOutput
aerosolMassesColumn !inp =
  let !snl = ami_snl inp
      !topIdx = nlevsno + snl  -- 0-based index of top active snow layer
      !nactive = negate snl

      processLayer j =
        let !lyr = if j < length (ami_layers inp)
                   then ami_layers inp !! j else defaultAerosolLayer
            !snowmass = (ami_h2osoi_ice inp VU.! j) + (ami_h2osoi_liq inp VU.! j)
            !cnc = aerosolMassConcentrations lyr snowmass
            !bctot = al_mss_bcpho lyr + al_mss_bcphi lyr
            !octot = al_mss_ocpho lyr + al_mss_ocphi lyr
            !dsttot = al_mss_dst1 lyr + al_mss_dst2 lyr
                    + al_mss_dst3 lyr + al_mss_dst4 lyr
        in (bctot, octot, dsttot, cnc, snowmass)

      !activeResults = [ processLayer j | j <- [topIdx .. topIdx + nactive - 1]
                                        , j >= 0, j < nlevsno ]

      !bcCol = sum [ bc | (bc, _, _, _, _) <- activeResults ]
      !ocCol = sum [ oc | (_, oc, _, _, _) <- activeResults ]
      !dstCol = sum [ dst | (_, _, dst, _, _) <- activeResults ]

      -- Top layer values
      (!bcTop, !ocTop, !dstTop, _, !h2oTop) =
        if null activeResults then (0.0, 0.0, 0.0, AerosolMassCnc 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0, 0.0)
        else head activeResults

      !cncList = [ cnc | (_, _, _, cnc, _) <- activeResults ]

  in AerosolMassesOutput
     { amo_mss_bc_col = bcCol
     , amo_mss_oc_col = ocCol
     , amo_mss_dst_col = dstCol
     , amo_mss_bc_top = bcTop
     , amo_mss_oc_top = ocTop
     , amo_mss_dst_top = dstTop
     , amo_h2osno_top = h2oTop
     , amo_cnc_layers = cncList
     }

-- ========================================================================
-- Aerosol washout during melt percolation (Fortran: AerosolFluxes)
-- ========================================================================

data AerosolWashoutInput = AerosolWashoutInput
  { awi_layer          :: !AerosolLayerData
  , awi_qflx_percolation :: !Double  -- ^ meltwater percolation flux (kg/m2/s)
  , awi_h2osoi_liq     :: !Double    -- ^ liquid water in layer (kg/m2)
  , awi_h2osoi_ice     :: !Double    -- ^ ice in layer (kg/m2)
  , awi_dt             :: !Double    -- ^ timestep (s)
  , awi_scvng_fct_bc   :: !Double    -- ^ scavenging efficiency for BC [0,1]
  , awi_scvng_fct_oc   :: !Double    -- ^ scavenging efficiency for OC
  , awi_scvng_fct_dst  :: !Double    -- ^ scavenging efficiency for dust
  } deriving (Show)

data AerosolWashoutOutput = AerosolWashoutOutput
  { awo_layer_updated :: !AerosolLayerData
  , awo_flux_bc_out   :: !Double  -- ^ BC mass flux out of layer (kg/m2/s)
  , awo_flux_oc_out   :: !Double
  , awo_flux_dst_out  :: !Double
  } deriving (Show)

-- | Compute aerosol washout from a single snow layer during melt.
-- Scavenging removes a fraction of aerosol proportional to meltwater flux.
aerosolWashout :: AerosolWashoutInput -> AerosolWashoutOutput
aerosolWashout !inp =
  let !lyr = awi_layer inp
      !qperc = awi_qflx_percolation inp
      !dt = awi_dt inp
      !snowmass = awi_h2osoi_liq inp + awi_h2osoi_ice inp

      -- Fraction of layer mass removed by percolation
      !frac_removed = if snowmass > 0.0
                      then min 1.0 (qperc * dt / snowmass)
                      else 0.0

      -- BC washout (hydrophilic only; hydrophobic is not scavenged)
      !scvng_bc = awi_scvng_fct_bc inp
      !bc_phi_loss = al_mss_bcphi lyr * frac_removed * scvng_bc
      !bc_pho_loss = 0.0  -- hydrophobic BC not scavenged

      -- OC washout
      !scvng_oc = awi_scvng_fct_oc inp
      !oc_phi_loss = al_mss_ocphi lyr * frac_removed * scvng_oc
      !oc_pho_loss = 0.0

      -- Dust washout (all species equally scavenged)
      !scvng_dst = awi_scvng_fct_dst inp
      !dst1_loss = al_mss_dst1 lyr * frac_removed * scvng_dst
      !dst2_loss = al_mss_dst2 lyr * frac_removed * scvng_dst
      !dst3_loss = al_mss_dst3 lyr * frac_removed * scvng_dst
      !dst4_loss = al_mss_dst4 lyr * frac_removed * scvng_dst

      !lyr' = lyr
        { al_mss_bcphi = al_mss_bcphi lyr - bc_phi_loss
        , al_mss_ocphi = al_mss_ocphi lyr - oc_phi_loss
        , al_mss_dst1 = al_mss_dst1 lyr - dst1_loss
        , al_mss_dst2 = al_mss_dst2 lyr - dst2_loss
        , al_mss_dst3 = al_mss_dst3 lyr - dst3_loss
        , al_mss_dst4 = al_mss_dst4 lyr - dst4_loss
        }

      !totalBcOut = (bc_phi_loss + bc_pho_loss) / dt
      !totalOcOut = (oc_phi_loss + oc_pho_loss) / dt
      !totalDstOut = (dst1_loss + dst2_loss + dst3_loss + dst4_loss) / dt

  in AerosolWashoutOutput
     { awo_layer_updated = lyr'
     , awo_flux_bc_out = totalBcOut
     , awo_flux_oc_out = totalOcOut
     , awo_flux_dst_out = totalDstOut
     }

-- ========================================================================
-- Deposition to top snow layer
-- ========================================================================

-- | Add atmospheric deposition fluxes to the top snow layer.
-- Ported from AerosolFluxes in AerosolMod.F90 (deposition portion).
aerosolDepositionToSnow :: AerosolDepFluxes
                        -> AerosolLayerData
                        -> Double  -- ^ dt (s)
                        -> AerosolLayerData
aerosolDepositionToSnow !dep !lyr !dt = lyr
  { al_mss_bcphi = al_mss_bcphi lyr + ad_flx_bc_dep_phi dep * dt
  , al_mss_bcpho = al_mss_bcpho lyr + ad_flx_bc_dep_pho dep * dt
  , al_mss_ocphi = al_mss_ocphi lyr + ad_flx_oc_dep_phi dep * dt
  , al_mss_ocpho = al_mss_ocpho lyr + ad_flx_oc_dep_pho dep * dt
  , al_mss_dst1  = al_mss_dst1 lyr + (ad_flx_dst_dep_wet1 dep + ad_flx_dst_dep_dry1 dep) * dt
  , al_mss_dst2  = al_mss_dst2 lyr + (ad_flx_dst_dep_wet2 dep + ad_flx_dst_dep_dry2 dep) * dt
  , al_mss_dst3  = al_mss_dst3 lyr + (ad_flx_dst_dep_wet3 dep + ad_flx_dst_dep_dry3 dep) * dt
  , al_mss_dst4  = al_mss_dst4 lyr + (ad_flx_dst_dep_wet4 dep + ad_flx_dst_dep_dry4 dep) * dt
  }

-- ========================================================================
-- Inter-layer mixing during snow compaction
-- ========================================================================

-- | Mix aerosol masses between two adjacent snow layers during compaction.
-- When a layer is thinned, its aerosol mass is redistributed proportionally.
aerosolCompactionMix :: AerosolLayerData  -- ^ upper layer
                     -> AerosolLayerData  -- ^ lower layer
                     -> Double  -- ^ fraction of upper transferred to lower [0,1]
                     -> (AerosolLayerData, AerosolLayerData)
aerosolCompactionMix !upper !lower !frac =
  let transfer field = field upper * frac
      keep field = field upper * (1.0 - frac)
      add field = field lower + transfer field

      !upper' = upper
        { al_mss_bcphi = keep al_mss_bcphi, al_mss_bcpho = keep al_mss_bcpho
        , al_mss_ocphi = keep al_mss_ocphi, al_mss_ocpho = keep al_mss_ocpho
        , al_mss_dst1  = keep al_mss_dst1,  al_mss_dst2  = keep al_mss_dst2
        , al_mss_dst3  = keep al_mss_dst3,  al_mss_dst4  = keep al_mss_dst4
        }
      !lower' = lower
        { al_mss_bcphi = add al_mss_bcphi, al_mss_bcpho = add al_mss_bcpho
        , al_mss_ocphi = add al_mss_ocphi, al_mss_ocpho = add al_mss_ocpho
        , al_mss_dst1  = add al_mss_dst1,  al_mss_dst2  = add al_mss_dst2
        , al_mss_dst3  = add al_mss_dst3,  al_mss_dst4  = add al_mss_dst4
        }
  in (upper', lower')
