-- | Plant Functional Type (PFT) constants and data structure.
-- Ported from: src/main/pftconMod.F90
-- Preserves Fortran variable names for traceability.
--
-- NOTE: Fortran uses 0-based PFT indexing (allocate(0:mxpft)).
-- Haskell vectors are 0-based, so indices match Fortran directly.
module CLM.Constants.PFTConstants
  ( -- * PFT index constants (Fortran 0-based)
    noveg
  , ndllf_evr_tmp_tree
  , ndllf_evr_brl_tree
  , ndllf_dcd_brl_tree
  , nbrdlf_evr_trp_tree
  , nbrdlf_evr_tmp_tree
  , nbrdlf_dcd_trp_tree
  , nbrdlf_dcd_tmp_tree
  , nbrdlf_dcd_brl_tree
  , nbrdlf_evr_shrub
  , nbrdlf_dcd_tmp_shrub
  , nbrdlf_dcd_brl_shrub
  , nc3_arctic_grass
  , nc3_nonarctic_grass
  , nc4_grass
  , nc3crop
  , nc3irrig
  , ntmp_corn
  , nirrig_tmp_corn
  , nswheat
  , nirrig_swheat
  , nwwheat
  , nirrig_wwheat
  , ntmp_soybean
  , nirrig_tmp_soybean
  , nbarley
  , nirrig_barley
  , nwbarley
  , nirrig_wbarley
  , nrye
  , nirrig_rye
  , nwrye
  , nirrig_wrye
  , ncassava
  , nirrig_cassava
  , ncitrus
  , nirrig_citrus
  , ncocoa
  , nirrig_cocoa
  , ncoffee
  , nirrig_coffee
  , ncotton
  , nirrig_cotton
  , ndatepalm
  , nirrig_datepalm
  , nfoddergrass
  , nirrig_foddergrass
  , ngrapes
  , nirrig_grapes
  , ngroundnuts
  , nirrig_groundnuts
  , nmillet
  , nirrig_millet
  , noilpalm
  , nirrig_oilpalm
  , npotatoes
  , nirrig_potatoes
  , npulses
  , nirrig_pulses
  , nrapeseed
  , nirrig_rapeseed
  , nrice
  , nirrig_rice
  , nsorghum
  , nirrig_sorghum
  , nsugarbeet
  , nirrig_sugarbeet
  , nsugarcane
  , nirrig_sugarcane
  , nsunflower
  , nirrig_sunflower
  , nmiscanthus
  , nirrig_miscanthus
  , nswitchgrass
  , nirrig_switchgrass
  , ntrp_corn
  , nirrig_trp_corn
  , ntrp_soybean
  , nirrig_trp_soybean
    -- * Real-valued PFT parameters
  , reinickerp
  , dwood_param
  , allom1
  , allom2
  , allom3
  , allom1s
  , allom2s
  , root_density_param
  , root_radius_param
  , pftname_len
    -- * PFT names
  , pftNames
    -- * PFT constants record
  , PftconType(..)
  , pftconAllocate
  ) where

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

import CLM.Constants.VarPar (mxpft, numrad, nvariants)

-- ---------------------------------------------------------------------------
-- PFT index constants (Fortran 0-based)
-- ---------------------------------------------------------------------------

noveg :: Int
noveg = 0

ndllf_evr_tmp_tree :: Int
ndllf_evr_tmp_tree = 1

ndllf_evr_brl_tree :: Int
ndllf_evr_brl_tree = 2

ndllf_dcd_brl_tree :: Int
ndllf_dcd_brl_tree = 3

nbrdlf_evr_trp_tree :: Int
nbrdlf_evr_trp_tree = 4

nbrdlf_evr_tmp_tree :: Int
nbrdlf_evr_tmp_tree = 5

nbrdlf_dcd_trp_tree :: Int
nbrdlf_dcd_trp_tree = 6

nbrdlf_dcd_tmp_tree :: Int
nbrdlf_dcd_tmp_tree = 7

nbrdlf_dcd_brl_tree :: Int
nbrdlf_dcd_brl_tree = 8

nbrdlf_evr_shrub :: Int
nbrdlf_evr_shrub = 9

nbrdlf_dcd_tmp_shrub :: Int
nbrdlf_dcd_tmp_shrub = 10

nbrdlf_dcd_brl_shrub :: Int
nbrdlf_dcd_brl_shrub = 11

nc3_arctic_grass :: Int
nc3_arctic_grass = 12

nc3_nonarctic_grass :: Int
nc3_nonarctic_grass = 13

nc4_grass :: Int
nc4_grass = 14

nc3crop :: Int
nc3crop = 15

nc3irrig :: Int
nc3irrig = 16

ntmp_corn :: Int
ntmp_corn = 17

nirrig_tmp_corn :: Int
nirrig_tmp_corn = 18

nswheat :: Int
nswheat = 19

nirrig_swheat :: Int
nirrig_swheat = 20

nwwheat :: Int
nwwheat = 21

nirrig_wwheat :: Int
nirrig_wwheat = 22

ntmp_soybean :: Int
ntmp_soybean = 23

nirrig_tmp_soybean :: Int
nirrig_tmp_soybean = 24

nbarley :: Int
nbarley = 25

nirrig_barley :: Int
nirrig_barley = 26

nwbarley :: Int
nwbarley = 27

nirrig_wbarley :: Int
nirrig_wbarley = 28

nrye :: Int
nrye = 29

nirrig_rye :: Int
nirrig_rye = 30

nwrye :: Int
nwrye = 31

nirrig_wrye :: Int
nirrig_wrye = 32

ncassava :: Int
ncassava = 33

nirrig_cassava :: Int
nirrig_cassava = 34

ncitrus :: Int
ncitrus = 35

nirrig_citrus :: Int
nirrig_citrus = 36

ncocoa :: Int
ncocoa = 37

nirrig_cocoa :: Int
nirrig_cocoa = 38

ncoffee :: Int
ncoffee = 39

nirrig_coffee :: Int
nirrig_coffee = 40

ncotton :: Int
ncotton = 41

nirrig_cotton :: Int
nirrig_cotton = 42

ndatepalm :: Int
ndatepalm = 43

nirrig_datepalm :: Int
nirrig_datepalm = 44

nfoddergrass :: Int
nfoddergrass = 45

nirrig_foddergrass :: Int
nirrig_foddergrass = 46

ngrapes :: Int
ngrapes = 47

nirrig_grapes :: Int
nirrig_grapes = 48

ngroundnuts :: Int
ngroundnuts = 49

nirrig_groundnuts :: Int
nirrig_groundnuts = 50

nmillet :: Int
nmillet = 51

nirrig_millet :: Int
nirrig_millet = 52

noilpalm :: Int
noilpalm = 53

nirrig_oilpalm :: Int
nirrig_oilpalm = 54

npotatoes :: Int
npotatoes = 55

nirrig_potatoes :: Int
nirrig_potatoes = 56

npulses :: Int
npulses = 57

nirrig_pulses :: Int
nirrig_pulses = 58

nrapeseed :: Int
nrapeseed = 59

nirrig_rapeseed :: Int
nirrig_rapeseed = 60

nrice :: Int
nrice = 61

nirrig_rice :: Int
nirrig_rice = 62

nsorghum :: Int
nsorghum = 63

nirrig_sorghum :: Int
nirrig_sorghum = 64

nsugarbeet :: Int
nsugarbeet = 65

nirrig_sugarbeet :: Int
nirrig_sugarbeet = 66

nsugarcane :: Int
nsugarcane = 67

nirrig_sugarcane :: Int
nirrig_sugarcane = 68

nsunflower :: Int
nsunflower = 69

nirrig_sunflower :: Int
nirrig_sunflower = 70

nmiscanthus :: Int
nmiscanthus = 71

nirrig_miscanthus :: Int
nirrig_miscanthus = 72

nswitchgrass :: Int
nswitchgrass = 73

nirrig_switchgrass :: Int
nirrig_switchgrass = 74

ntrp_corn :: Int
ntrp_corn = 75

nirrig_trp_corn :: Int
nirrig_trp_corn = 76

ntrp_soybean :: Int
ntrp_soybean = 77

nirrig_trp_soybean :: Int
nirrig_trp_soybean = 78

-- ---------------------------------------------------------------------------
-- Real-valued PFT parameters
-- ---------------------------------------------------------------------------

-- | Parameter in allometric equation
reinickerp :: Double
reinickerp = 1.6

-- | CN wood density [gC/m3]; lpj:2.0e5
dwood_param :: Double
dwood_param = 2.5e5

-- | Parameters in allometric equations
allom1 :: Double
allom1 = 100.0

allom2 :: Double
allom2 = 40.0

allom3 :: Double
allom3 = 0.5

-- | Modified allometric params for shrubs
allom1s :: Double
allom1s = 250.0

allom2s :: Double
allom2s = 8.0

-- | Root density [g biomass / m3 root]
root_density_param :: Double
root_density_param = 0.31e6

-- | Root radius [m]
root_radius_param :: Double
root_radius_param = 0.29e-3

-- | Max length of pftname
pftname_len :: Int
pftname_len = 40

-- ---------------------------------------------------------------------------
-- PFT names (0-based index matches Fortran)
-- ---------------------------------------------------------------------------

-- | PFT names vector, index 0..78 matching Fortran 0:mxpft
pftNames :: V.Vector String
pftNames = V.fromList
  [ "not_vegetated"                        --  0
  , "needleleaf_evergreen_temperate_tree"  --  1
  , "needleleaf_evergreen_boreal_tree"     --  2
  , "needleleaf_deciduous_boreal_tree"     --  3
  , "broadleaf_evergreen_tropical_tree"    --  4
  , "broadleaf_evergreen_temperate_tree"   --  5
  , "broadleaf_deciduous_tropical_tree"    --  6
  , "broadleaf_deciduous_temperate_tree"   --  7
  , "broadleaf_deciduous_boreal_tree"      --  8
  , "broadleaf_evergreen_shrub"            --  9
  , "broadleaf_deciduous_temperate_shrub"  -- 10
  , "broadleaf_deciduous_boreal_shrub"     -- 11
  , "c3_arctic_grass"                      -- 12
  , "c3_non-arctic_grass"                  -- 13
  , "c4_grass"                             -- 14
  , "c3_crop"                              -- 15
  , "c3_irrigated"                         -- 16
  , "temperate_corn"                       -- 17
  , "irrigated_temperate_corn"             -- 18
  , "spring_wheat"                         -- 19
  , "irrigated_spring_wheat"               -- 20
  , "winter_wheat"                         -- 21
  , "irrigated_winter_wheat"               -- 22
  , "temperate_soybean"                    -- 23
  , "irrigated_temperate_soybean"          -- 24
  , "barley"                               -- 25
  , "irrigated_barley"                     -- 26
  , "winter_barley"                        -- 27
  , "irrigated_winter_barley"              -- 28
  , "rye"                                  -- 29
  , "irrigated_rye"                        -- 30
  , "winter_rye"                           -- 31
  , "irrigated_winter_rye"                 -- 32
  , "cassava"                              -- 33
  , "irrigated_cassava"                    -- 34
  , "citrus"                               -- 35
  , "irrigated_citrus"                     -- 36
  , "cocoa"                                -- 37
  , "irrigated_cocoa"                      -- 38
  , "coffee"                               -- 39
  , "irrigated_coffee"                     -- 40
  , "cotton"                               -- 41
  , "irrigated_cotton"                     -- 42
  , "datepalm"                             -- 43
  , "irrigated_datepalm"                   -- 44
  , "foddergrass"                          -- 45
  , "irrigated_foddergrass"                -- 46
  , "grapes"                               -- 47
  , "irrigated_grapes"                     -- 48
  , "groundnuts"                           -- 49
  , "irrigated_groundnuts"                 -- 50
  , "millet"                               -- 51
  , "irrigated_millet"                     -- 52
  , "oilpalm"                              -- 53
  , "irrigated_oilpalm"                    -- 54
  , "potatoes"                             -- 55
  , "irrigated_potatoes"                   -- 56
  , "pulses"                               -- 57
  , "irrigated_pulses"                     -- 58
  , "rapeseed"                             -- 59
  , "irrigated_rapeseed"                   -- 60
  , "rice"                                 -- 61
  , "irrigated_rice"                       -- 62
  , "sorghum"                              -- 63
  , "irrigated_sorghum"                    -- 64
  , "sugarbeet"                            -- 65
  , "irrigated_sugarbeet"                  -- 66
  , "sugarcane"                            -- 67
  , "irrigated_sugarcane"                  -- 68
  , "sunflower"                            -- 69
  , "irrigated_sunflower"                  -- 70
  , "miscanthus"                           -- 71
  , "irrigated_miscanthus"                 -- 72
  , "switchgrass"                          -- 73
  , "irrigated_switchgrass"                -- 74
  , "tropical_corn"                        -- 75
  , "irrigated_tropical_corn"              -- 76
  , "tropical_soybean"                     -- 77
  , "irrigated_tropical_soybean"           -- 78
  ]

-- ---------------------------------------------------------------------------
-- PFT constants data structure
-- ---------------------------------------------------------------------------

-- | PFT constants data. All 1D unboxed vectors are size (mxpft+1) for
-- Fortran indices 0:mxpft. 2D data uses boxed vectors of unboxed vectors.
-- Values are populated from parameter files at runtime.
data PftconType = PftconType
  { -- Optical/structural properties
    pft_dleaf          :: !(U.Vector Double)   -- ^ Characteristic leaf dimension [m]
  , pft_c3psn          :: !(U.Vector Double)   -- ^ Photosynthetic pathway: 0=c4, 1=c3
  , pft_xl             :: !(U.Vector Double)   -- ^ Leaf/stem orientation index
  , pft_rhol           :: !(V.Vector (U.Vector Double))  -- ^ Leaf reflectance (npft x numrad)
  , pft_rhos           :: !(V.Vector (U.Vector Double))  -- ^ Stem reflectance (npft x numrad)
  , pft_taul           :: !(V.Vector (U.Vector Double))  -- ^ Leaf transmittance (npft x numrad)
  , pft_taus           :: !(V.Vector (U.Vector Double))  -- ^ Stem transmittance (npft x numrad)
    -- Roughness parameters
  , pft_z0mr           :: !(U.Vector Double)   -- ^ Momentum roughness length ratio
  , pft_displar        :: !(U.Vector Double)   -- ^ Displacement height ratio
  , pft_roota_par      :: !(U.Vector Double)   -- ^ CLM rooting distribution param [1/m]
  , pft_rootb_par      :: !(U.Vector Double)   -- ^ CLM rooting distribution param [1/m]
    -- Crop/irrigation flags
  , pft_crop           :: !(U.Vector Double)   -- ^ Crop pft: 0=not crop, 1=crop
  , pft_irrigated      :: !(U.Vector Double)   -- ^ Irrigated: 0=not, 1=irrigated
    -- Soil water potential
  , pft_smpso          :: !(U.Vector Double)   -- ^ Soil water potential at full stomatal opening [mm]
  , pft_smpsc          :: !(U.Vector Double)   -- ^ Soil water potential at full stomatal closure [mm]
  , pft_fnitr          :: !(U.Vector Double)   -- ^ Foliage nitrogen limitation factor
    -- CN code
  , pft_slatop         :: !(U.Vector Double)   -- ^ SLA at top of canopy [m2/gC]
  , pft_dsladlai       :: !(U.Vector Double)   -- ^ dSLA/dLAI [m2/gC]
  , pft_leafcn         :: !(U.Vector Double)   -- ^ Leaf C:N [gC/gN]
  , pft_flnr           :: !(U.Vector Double)   -- ^ Fraction of leaf N in Rubisco
  , pft_woody          :: !(U.Vector Double)   -- ^ Woody lifeform flag (0 or 1)
  , pft_lflitcn        :: !(U.Vector Double)   -- ^ Leaf litter C:N [gC/gN]
  , pft_frootcn        :: !(U.Vector Double)   -- ^ Fine root C:N [gC/gN]
  , pft_livewdcn       :: !(U.Vector Double)   -- ^ Live wood C:N [gC/gN]
  , pft_deadwdcn       :: !(U.Vector Double)   -- ^ Dead wood C:N [gC/gN]
  , pft_grperc         :: !(U.Vector Double)   -- ^ Growth respiration parameter
  , pft_grpnow         :: !(U.Vector Double)   -- ^ Growth respiration parameter
    -- Stomatal conductance
  , pft_mbbopt         :: !(U.Vector Double)   -- ^ Ball-Berry slope
  , pft_medlynslope    :: !(U.Vector Double)   -- ^ Medlyn slope
  , pft_medlynintercept :: !(U.Vector Double)  -- ^ Medlyn intercept
    -- Allocation parameters
  , pft_froot_leaf     :: !(U.Vector Double)   -- ^ New fine root C per new leaf C [gC/gC]
  , pft_stem_leaf      :: !(U.Vector Double)   -- ^ New stem C per new leaf C [gC/gC]
  , pft_croot_stem     :: !(U.Vector Double)   -- ^ New coarse root C per new stem C [gC/gC]
  , pft_flivewd        :: !(U.Vector Double)   -- ^ Fraction of new wood that is live
  , pft_fcur           :: !(U.Vector Double)   -- ^ Fraction of allocation to current growth
    -- Litter fractions
  , pft_lf_flab        :: !(U.Vector Double)   -- ^ Leaf litter labile fraction
  , pft_lf_fcel        :: !(U.Vector Double)   -- ^ Leaf litter cellulose fraction
  , pft_lf_flig        :: !(U.Vector Double)   -- ^ Leaf litter lignin fraction
  , pft_fr_flab        :: !(U.Vector Double)   -- ^ Fine root litter labile fraction
  , pft_fr_fcel        :: !(U.Vector Double)   -- ^ Fine root litter cellulose fraction
  , pft_fr_flig        :: !(U.Vector Double)   -- ^ Fine root litter lignin fraction
    -- Phenology flags
  , pft_leaf_long      :: !(U.Vector Double)   -- ^ Leaf longevity [yrs]
  , pft_evergreen      :: !(U.Vector Double)   -- ^ Binary flag for evergreen (0 or 1)
  , pft_stress_decid   :: !(U.Vector Double)   -- ^ Binary flag for stress-deciduous
  , pft_season_decid   :: !(U.Vector Double)   -- ^ Binary flag for seasonal-deciduous
    -- Tree structure
  , pft_dwood          :: !(U.Vector Double)   -- ^ Wood density [gC/m3]
  , pft_root_density   :: !(U.Vector Double)   -- ^ Root density [gC/m3]
  , pft_root_radius    :: !(U.Vector Double)   -- ^ Root radius [m]
    -- Fire parameters
  , pft_cc_leaf        :: !(U.Vector Double)
  , pft_cc_lstem       :: !(U.Vector Double)
  , pft_cc_dstem       :: !(U.Vector Double)
  , pft_cc_other       :: !(U.Vector Double)
  , pft_fm_leaf        :: !(U.Vector Double)
  , pft_fm_lstem       :: !(U.Vector Double)
  , pft_fm_dstem       :: !(U.Vector Double)
  , pft_fm_other       :: !(U.Vector Double)
  , pft_fm_root        :: !(U.Vector Double)
  , pft_fm_lroot       :: !(U.Vector Double)
  , pft_fm_droot       :: !(U.Vector Double)
  } deriving (Show)

-- | Allocate a PftconType with all vectors of size (mxpft+1) filled with zeros.
-- Corresponds to Fortran @InitAllocate@.
pftconAllocate :: PftconType
pftconAllocate =
  let n = mxpft + 1
      z = U.replicate n 0.0
      zMat dim2 = V.replicate n (U.replicate dim2 0.0)
  in PftconType
       { pft_dleaf           = z
       , pft_c3psn           = z
       , pft_xl              = z
       , pft_rhol            = zMat numrad
       , pft_rhos            = zMat numrad
       , pft_taul            = zMat numrad
       , pft_taus            = zMat numrad
       , pft_z0mr            = z
       , pft_displar         = z
       , pft_roota_par       = z
       , pft_rootb_par       = z
       , pft_crop            = z
       , pft_irrigated       = z
       , pft_smpso           = z
       , pft_smpsc           = z
       , pft_fnitr           = z
       , pft_slatop          = z
       , pft_dsladlai        = z
       , pft_leafcn          = z
       , pft_flnr            = z
       , pft_woody           = z
       , pft_lflitcn         = z
       , pft_frootcn         = z
       , pft_livewdcn        = z
       , pft_deadwdcn        = z
       , pft_grperc          = z
       , pft_grpnow          = z
       , pft_mbbopt          = z
       , pft_medlynslope     = z
       , pft_medlynintercept = z
       , pft_froot_leaf      = z
       , pft_stem_leaf       = z
       , pft_croot_stem      = z
       , pft_flivewd         = z
       , pft_fcur            = z
       , pft_lf_flab         = z
       , pft_lf_fcel         = z
       , pft_lf_flig         = z
       , pft_fr_flab         = z
       , pft_fr_fcel         = z
       , pft_fr_flig         = z
       , pft_leaf_long       = z
       , pft_evergreen       = z
       , pft_stress_decid    = z
       , pft_season_decid    = z
       , pft_dwood           = z
       , pft_root_density    = z
       , pft_root_radius     = z
       , pft_cc_leaf         = z
       , pft_cc_lstem        = z
       , pft_cc_dstem        = z
       , pft_cc_other        = z
       , pft_fm_leaf         = z
       , pft_fm_lstem        = z
       , pft_fm_dstem        = z
       , pft_fm_other        = z
       , pft_fm_root         = z
       , pft_fm_lroot        = z
       , pft_fm_droot        = z
       }
