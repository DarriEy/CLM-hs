-- | Landunit-level data structures.
-- Fortran: LandunitType.F90 — g/l/c/p hierarchy, urban properties, hillslope variables.
module CLM.Types.LandunitData
  ( LandunitData(..)
  , defaultLandunitData
  ) where

import qualified Data.Vector.Unboxed as VU

-- | Landunit-level data (SoA layout).
-- Preserves Fortran variable names with @lun_@ prefix.
data LandunitData = LandunitData
  { -- g/l/c/p hierarchy
    lun_gridcell              :: !(VU.Vector Int)    -- ^ Index into gridcell level quantities
  , lun_wtgcell               :: !(VU.Vector Double) -- ^ Weight relative to gridcell
  , lun_coli                  :: !(VU.Vector Int)    -- ^ Beginning column index per landunit
  , lun_colf                  :: !(VU.Vector Int)    -- ^ Ending column index per landunit
  , lun_ncolumns              :: !(VU.Vector Int)    -- ^ Number of columns per landunit
  , lun_nhillslopes           :: !(VU.Vector Int)    -- ^ Number of hillslopes per landunit
  , lun_patchi                :: !(VU.Vector Int)    -- ^ Beginning patch index per landunit
  , lun_patchf                :: !(VU.Vector Int)    -- ^ Ending patch index per landunit
  , lun_npatches              :: !(VU.Vector Int)    -- ^ Number of patches per landunit
    -- topological mapping
  , lun_itype                 :: !(VU.Vector Int)    -- ^ Landunit type
    -- urban properties
  , lun_canyon_hwr            :: !(VU.Vector Double) -- ^ Canyon height to width ratio
  , lun_wtroad_perv           :: !(VU.Vector Double) -- ^ Weight of pervious road to total road
  , lun_wtlunit_roof          :: !(VU.Vector Double) -- ^ Weight of roof w.r.t. urban landunit
  , lun_ht_roof               :: !(VU.Vector Double) -- ^ Height of urban roof [m]
  , lun_z_0_town              :: !(VU.Vector Double) -- ^ Urban momentum roughness length [m]
  , lun_z_d_town              :: !(VU.Vector Double) -- ^ Urban displacement height [m]
    -- hillslope variables
  , lun_stream_channel_depth  :: !(VU.Vector Double) -- ^ Stream channel bankfull depth [m]
  , lun_stream_channel_width  :: !(VU.Vector Double) -- ^ Stream channel bankfull width [m]
  , lun_stream_channel_length :: !(VU.Vector Double) -- ^ Stream channel length [m]
  , lun_stream_channel_slope  :: !(VU.Vector Double) -- ^ Stream channel slope [m/m]
  , lun_stream_channel_number :: !(VU.Vector Double) -- ^ Number of channels in landunit
  } deriving (Show)

defaultLandunitData :: LandunitData
defaultLandunitData = LandunitData
  { lun_gridcell              = VU.empty
  , lun_wtgcell               = VU.empty
  , lun_coli                  = VU.empty
  , lun_colf                  = VU.empty
  , lun_ncolumns              = VU.empty
  , lun_nhillslopes           = VU.empty
  , lun_patchi                = VU.empty
  , lun_patchf                = VU.empty
  , lun_npatches              = VU.empty
  , lun_itype                 = VU.empty
  , lun_canyon_hwr            = VU.empty
  , lun_wtroad_perv           = VU.empty
  , lun_wtlunit_roof          = VU.empty
  , lun_ht_roof               = VU.empty
  , lun_z_0_town              = VU.empty
  , lun_z_d_town              = VU.empty
  , lun_stream_channel_depth  = VU.empty
  , lun_stream_channel_width  = VU.empty
  , lun_stream_channel_length = VU.empty
  , lun_stream_channel_slope  = VU.empty
  , lun_stream_channel_number = VU.empty
  }
