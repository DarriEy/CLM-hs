{-# LANGUAGE BangPatterns #-}
-- | Hydrology with drainage (subsurface runoff).
-- Fortran: HydrologyDrainageMod.F90
--
-- Provides:
--   * Wetland/ice hydrology flux assignment
--   * Urban drainage flux assignment
--   * Total runoff computation
--   * Water mass computation (stub)
--   * Glacier SMB runoff adjustment (stub)
--
-- All functions are pure. Fortran variable names preserved for traceability.
module CLM.BioGeoPhys.HydrologyDrainage
  ( -- * Types
    WetlandIceInput(..)
  , WetlandIceResult(..)
  , TotalRunoffInput(..)
  , TotalRunoffResult(..)
    -- * Constants
  , istwet
  , istice
  , istsoil
  , istcrop
  , icolRoadPerv
    -- * Science functions
  , computeWetlandIceHydrology
  , computeTotalRunoff
  ) where

import qualified Data.Vector.Unboxed as VU

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

-- | Landunit types
istwet :: Int
istwet = 5

istice :: Int
istice = 3

istsoil :: Int
istsoil = 1

istcrop :: Int
istcrop = 2

-- | Column type: pervious road
icolRoadPerv :: Int
icolRoadPerv = 74

-- | Special value
spval :: Double
spval = 1.0e36

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Input for wetland/ice hydrology for a single column
data WetlandIceInput = WetlandIceInput
  { wii_lun_itype          :: !Int     -- ^ landunit type
  , wii_urbpoi             :: !Bool    -- ^ is urban landunit
  , wii_col_itype          :: !Int     -- ^ column type
  , wii_qflx_evap_tot      :: !Double  -- ^ total evapotranspiration (mm/s)
  , wii_qflx_snwcp_ice     :: !Double  -- ^ snow capping ice (mm/s)
  , wii_qflx_snwcp_disc_ice :: !Double -- ^ discarded snow ice (mm/s)
  , wii_qflx_snwcp_disc_liq :: !Double -- ^ discarded snow liquid (mm/s)
  , wii_forc_rain           :: !Double  -- ^ rain rate (mm/s)
  , wii_forc_snow           :: !Double  -- ^ snow rate (mm/s)
  , wii_qflx_floodg         :: !Double  -- ^ flood water flux (mm/s)
  , wii_begwb               :: !Double  -- ^ beginning water balance (mm)
  , wii_endwb               :: !Double  -- ^ ending water balance (mm)
  , wii_dtime               :: !Double  -- ^ timestep (s)
  } deriving (Show, Eq)

-- | Result from wetland/ice hydrology for a single column
data WetlandIceResult = WetlandIceResult
  { wir_qflx_drain          :: !Double  -- ^ drainage (mm/s)
  , wir_qflx_drain_perched  :: !Double  -- ^ perched drainage (mm/s)
  , wir_qflx_surf           :: !Double  -- ^ surface runoff (mm/s)
  , wir_qflx_infl           :: !Double  -- ^ infiltration (mm/s)
  , wir_qflx_qrgwl          :: !Double  -- ^ glacier/wetland runoff (mm/s)
  , wir_qflx_latflow_out    :: !Double  -- ^ lateral outflow (mm/s)
  , wir_qflx_rsub_sat       :: !Double  -- ^ rsub sat (mm/s)
  , wir_qflx_ice_runoff_snwcp :: !Double -- ^ ice runoff from snow cap (mm/s)
  , wir_modified             :: !Bool    -- ^ whether wetland/ice fluxes were set
  } deriving (Show, Eq)

-- | Input for total runoff computation (single column)
data TotalRunoffInput = TotalRunoffInput
  { tri_qflx_drain         :: !Double
  , tri_qflx_surf          :: !Double
  , tri_qflx_qrgwl         :: !Double
  , tri_qflx_drain_perched :: !Double
  , tri_lun_itype          :: !Int
  , tri_urbpoi             :: !Bool
  } deriving (Show, Eq)

-- | Result of total runoff
data TotalRunoffResult = TotalRunoffResult
  { trr_qflx_runoff   :: !Double
  , trr_qflx_runoff_u :: !Double  -- ^ urban runoff
  , trr_qflx_runoff_r :: !Double  -- ^ rural runoff
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Science functions
--------------------------------------------------------------------------------

-- | Set hydrological fluxes for wetland and land ice columns.
-- For wetland/ice: zeroes drainage, infiltration, surface runoff;
-- qflx_qrgwl absorbs water balance residual.
-- For urban non-pervious: zeroes perched drainage and infiltration.
computeWetlandIceHydrology :: WetlandIceInput
                           -> Double  -- ^ existing qflx_drain
                           -> Double  -- ^ existing qflx_drain_perched
                           -> Double  -- ^ existing qflx_surf
                           -> Double  -- ^ existing qflx_infl
                           -> Double  -- ^ existing qflx_qrgwl
                           -> Double  -- ^ existing qflx_latflow_out
                           -> Double  -- ^ existing qflx_rsub_sat
                           -> WetlandIceResult
computeWetlandIceHydrology !inp !drain0 !drainp0 !surf0 !infl0 !qrgwl0 !latflow0 !rsub0 =
  let !lt = wii_lun_itype inp
      !ct = wii_col_itype inp
      !dtime = wii_dtime inp
      !ice_runoff = wii_qflx_snwcp_ice inp
  in if lt == istwet || lt == istice
     then WetlandIceResult
          { wir_qflx_drain = 0.0
          , wir_qflx_drain_perched = 0.0
          , wir_qflx_surf = 0.0
          , wir_qflx_infl = 0.0
          , wir_qflx_qrgwl = wii_forc_rain inp + wii_forc_snow inp + wii_qflx_floodg inp
                            - wii_qflx_evap_tot inp - wii_qflx_snwcp_ice inp
                            - wii_qflx_snwcp_disc_ice inp - wii_qflx_snwcp_disc_liq inp
                            - (wii_endwb inp - wii_begwb inp) / dtime
          , wir_qflx_latflow_out = 0.0
          , wir_qflx_rsub_sat = rsub0
          , wir_qflx_ice_runoff_snwcp = ice_runoff
          , wir_modified = True
          }
     else if wii_urbpoi inp && ct /= icolRoadPerv
     then WetlandIceResult
          { wir_qflx_drain = drain0
          , wir_qflx_drain_perched = 0.0
          , wir_qflx_surf = surf0
          , wir_qflx_infl = 0.0
          , wir_qflx_qrgwl = qrgwl0
          , wir_qflx_latflow_out = latflow0
          , wir_qflx_rsub_sat = spval
          , wir_qflx_ice_runoff_snwcp = ice_runoff
          , wir_modified = True
          }
     else WetlandIceResult
          { wir_qflx_drain = drain0
          , wir_qflx_drain_perched = drainp0
          , wir_qflx_surf = surf0
          , wir_qflx_infl = infl0
          , wir_qflx_qrgwl = qrgwl0
          , wir_qflx_latflow_out = latflow0
          , wir_qflx_rsub_sat = rsub0
          , wir_qflx_ice_runoff_snwcp = ice_runoff
          , wir_modified = False
          }

-- | Compute total runoff as sum of drainage, surface runoff, glacier/wetland
-- runoff, and perched drainage. Assigns urban vs rural runoff.
computeTotalRunoff :: TotalRunoffInput -> TotalRunoffResult
computeTotalRunoff !inp =
  let !qr = tri_qflx_drain inp + tri_qflx_surf inp +
            tri_qflx_qrgwl inp + tri_qflx_drain_perched inp
      !lt = tri_lun_itype inp
  in TotalRunoffResult
     { trr_qflx_runoff   = qr
     , trr_qflx_runoff_u = if tri_urbpoi inp then qr else 0.0
     , trr_qflx_runoff_r = if lt == istsoil || lt == istcrop then qr else 0.0
     }
