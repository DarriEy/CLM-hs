{-# LANGUAGE BangPatterns #-}
-- | FATES Patch Data Structures and Dynamics
module CLM.BioGeoChem.FATES.Patch
  ( FATESPatch(..)
  , FATERunningMean(..)
  , FATERunningMeanArr(..)
  , defaultFATESPatch
  , defaultFATERunningMean
  , defaultFATERunningMeanArr
  , patchCreate
  , updateTreeGrassArea
  , updateLiveGrass
  , isWoodyPFT
  ) where

import qualified Data.Vector.Unboxed as U
import CLM.BioGeoChem.FATES.Cohort
import CLM.BioGeoChem.FATES.Constants

-- | IEEE-754 NaN constant
nan :: Double
nan = 0.0 / 0.0

-- | Represents the running mean time-varying info.
-- Corresponds to rmean_type in FatesRunningMeanMod.F90.
data FATERunningMean = FATERunningMean
  { rmeanCMean  :: !Double
  , rmeanLMean  :: !Double
  , rmeanCIndex :: !Int
  } deriving (Show, Eq)

-- | Construct a default running mean.
defaultFATERunningMean :: FATERunningMean
defaultFATERunningMean = FATERunningMean
  { rmeanCMean  = nan
  , rmeanLMean  = nan
  , rmeanCIndex = 0
  }

-- | Represents a vector of running mean structures.
-- Corresponds to rmean_arr_type in FatesRunningMeanMod.F90.
data FATERunningMeanArr = FATERunningMeanArr
  { rmeanArrCMean  :: !(U.Vector Double)
  , rmeanArrLMean  :: !(U.Vector Double)
  , rmeanArrCIndex :: !(U.Vector Int)
  } deriving (Show, Eq)

-- | Construct a default running mean array of a given size.
defaultFATERunningMeanArr :: Int -> FATERunningMeanArr
defaultFATERunningMeanArr !size = FATERunningMeanArr
  { rmeanArrCMean  = U.replicate size nan
  , rmeanArrLMean  = U.replicate size nan
  , rmeanArrCIndex = U.replicate size 0
  }

-- | Represents a FATES patch.
-- Corresponds to fates_patch_type in FatesPatchMod.F90.
data FATESPatch = FATESPatch
  { patchNo                      :: !Int        -- ^ Unique tracking index for the patch
  , nocompPftLabel               :: !Int        -- ^ No-competition mode PFT label (0 for bareground)
  , age                          :: !Double     -- ^ Average patch age [years]
  , ageClass                     :: !Int        -- ^ Age class for history binning
  , area                         :: !Double     -- ^ Patch area [m2]
  , countCohorts                 :: !Int        -- ^ Number of cohorts currently in patch
  , nclP                         :: !Int        -- ^ Number of occupied canopy layers
  , landUseLabel                 :: !Int        -- ^ Land use category label
  , ageSinceAnthroDisturbance     :: !Double     -- ^ Average age since last anthropogenic disturbance [years]
  , changedLanduseThisTs         :: !Bool       -- ^ Flag for land use change in current timestep
  -- Leaf Organization
  , pftAgbProfile                :: !(U.Vector Double) -- ^ Flat maxpft * nDbhBins aboveground biomass profile
  , canopyLayerTlai              :: !(U.Vector Double) -- ^ Total leaf area index of each canopy layer
  , totalCanopyArea              :: !Double     -- ^ Area covered by vegetation [m2]
  , totalTreeArea                :: !Double     -- ^ Area covered by tree vegetation [m2]
  , totalGrassArea               :: !Double     -- ^ Area covered by grass vegetation [m2]
  , zstar                        :: !Double     -- ^ Height of smallest canopy tree [m]
  , elaiProfile                  :: !(U.Vector Double) -- ^ Exposed leaf area index profile (nclmax * maxpft * nlevleaf)
  , esaiProfile                  :: !(U.Vector Double) -- ^ Exposed stem area index profile
  , tlaiProfile                  :: !(U.Vector Double) -- ^ Total leaf area index profile
  , tsaiProfile                  :: !(U.Vector Double) -- ^ Total stem area index profile
  , canopyAreaProfile            :: !(U.Vector Double) -- ^ Canopy area profile
  , canopyMask                   :: !(U.Vector Int)    -- ^ Canopy mask (nclmax * maxpft)
  , nrad                         :: !(U.Vector Int)    -- ^ Exposed vegetation layers count
  , nleaf                        :: !(U.Vector Int)    -- ^ Total leaf layers count
  , cStomata                     :: !Double     -- ^ Mean stomatal conductance [umol/m2/s]
  , cLblayer                     :: !Double     -- ^ Mean boundary layer conductance [umol/m2/s]
  , nrmlzdParprofPftDirZ         :: !(U.Vector Double) -- ^ Normalized PAR direct-beam profile
  , nrmlzdParprofPftDifZ         :: !(U.Vector Double) -- ^ Normalized PAR diffuse-beam profile
  -- Radiation
  , radError                     :: !(U.Vector Double) -- ^ Radiation conservation error by band
  , fcansno                      :: !Double     -- ^ Fraction of canopy covered in snow
  , solarZenithFlag              :: !Bool       -- ^ Daylight flag based on zenith angle
  , solarZenithAngle             :: !Double     -- ^ Solar zenith angle [radians]
  , gndAlbDif                    :: !(U.Vector Double) -- ^ Ground albedo for diffuse radiation
  , gndAlbDir                    :: !(U.Vector Double) -- ^ Ground albedo for direct radiation
  , fabdSunZ                     :: !(U.Vector Double) -- ^ Direct light absorbed sun fraction
  , fabdShaZ                     :: !(U.Vector Double) -- ^ Direct light absorbed shade fraction
  , fabiSunZ                     :: !(U.Vector Double) -- ^ Indirect light absorbed sun fraction
  , fabiShaZ                     :: !(U.Vector Double) -- ^ Indirect light absorbed shade fraction
  , edParsunZ                    :: !(U.Vector Double) -- ^ PAR absorbed in sun
  , edParshaZ                    :: !(U.Vector Double) -- ^ PAR absorbed in shade
  , fSun                         :: !(U.Vector Double) -- ^ Leaves fraction in sun
  , edLaisunZ                    :: !(U.Vector Double) -- ^ Exposed sun LAI profile
  , edLaishaZ                    :: !(U.Vector Double) -- ^ Exposed shade LAI profile
  , parprofPftDirZ               :: !(U.Vector Double) -- ^ Direct-beam PAR profile
  , parprofPftDifZ               :: !(U.Vector Double) -- ^ Diffuse-beam PAR profile
  , trSoilDir                    :: !(U.Vector Double) -- ^ Direct radiation transmitted to soil
  , trSoilDif                    :: !(U.Vector Double) -- ^ Diffuse radiation transmitted to soil
  , trSoilDirDif                 :: !(U.Vector Double) -- ^ Direct radiation transmitted to soil as diffuse
  , fab                          :: !(U.Vector Double) -- ^ Fraction of radiation absorbed by canopy
  , fabd                         :: !(U.Vector Double) -- ^ Fraction of direct radiation absorbed by canopy
  , fabi                         :: !(U.Vector Double) -- ^ Fraction of diffuse radiation absorbed by canopy
  , sabsDir                      :: !(U.Vector Double) -- ^ Direct radiation absorbed by canopy fraction
  , sabsDif                      :: !(U.Vector Double) -- ^ Diffuse radiation absorbed by canopy fraction
  -- Roots
  , btranFt                      :: !(U.Vector Double) -- ^ PFT-level soil water stress factor [0-1]
  , bstressSalFt                 :: !(U.Vector Double) -- ^ PFT-level salinity stress factor [0-1]
  -- External Seed Rain
  , nitrReproStoich              :: !(U.Vector Double) -- ^ NC ratio of recruit PFTs
  , phosReproStoich              :: !(U.Vector Double) -- ^ PC ratio of recruit PFTs
  -- Disturbance
  , disturbanceRates             :: !(U.Vector Double) -- ^ Disturbance rates array
  , landuseTransitionRates       :: !(U.Vector Double) -- ^ Land use transition rates array
  , fractLdistNotHarvested       :: !Double     -- ^ Logged area fraction not harvested
  -- Litter
  , fragmentationScaler          :: !(U.Vector Double) -- ^ Soil layer litter fragmentation scale rate
  -- Fuels and Fire
  , livegrass                    :: !Double     -- ^ Total live grass biomass [kgC/m2]
  , rosFront                     :: !Double     -- ^ Fire forward rate of spread [m/min]
  , rosBack                      :: !Double     -- ^ Fire backward rate of spread [m/min]
  , tauL                         :: !Double     -- ^ Heating duration [min]
  , fi                           :: !Double     -- ^ Average fire intensity [kJ/m/s]
  , fire                         :: !Int        -- ^ Fire occurrence flag (1=yes, 0=no)
  , fd                           :: !Double     -- ^ Fire duration [min]
  , scorchHt                     :: !(U.Vector Double) -- ^ Scorch height [m]
  , tfcRos                       :: !Double     -- ^ Total fire-consumed fuel [kgC/m2 ground/day]
  , fracBurnt                    :: !Double     -- ^ Fraction of patch burned
  -- Running Means
  , tveg24                       :: !FATERunningMean
  , tvegLpa                     :: !FATERunningMean
  , tvegLongterm                 :: !FATERunningMean
  , seedlingLayerPar24           :: !FATERunningMean
  , sdlngEmergSmp                :: !FATERunningMeanArr
  , sdlngMortPar                 :: !FATERunningMean
  , sdlngMdd                     :: !FATERunningMeanArr
  , sdlng2sapPar                 :: !FATERunningMean
  } deriving (Show, Eq)

-- | Construct a default uninitialized FATESPatch.
defaultFATESPatch :: FATESPatch
defaultFATESPatch = FATESPatch
  { patchNo                      = fatesUnsetInt
  , nocompPftLabel               = fatesUnsetInt
  , age                          = nan
  , ageClass                     = fatesUnsetInt
  , area                         = nan
  , countCohorts                 = fatesUnsetInt
  , nclP                         = fatesUnsetInt
  , landUseLabel                 = fatesUnsetInt
  , ageSinceAnthroDisturbance     = nan
  , changedLanduseThisTs         = False
  , pftAgbProfile                = U.empty
  , canopyLayerTlai              = U.empty
  , totalCanopyArea              = nan
  , totalTreeArea                = nan
  , totalGrassArea               = nan
  , zstar                        = nan
  , elaiProfile                  = U.empty
  , esaiProfile                  = U.empty
  , tlaiProfile                  = U.empty
  , tsaiProfile                  = U.empty
  , canopyAreaProfile            = U.empty
  , canopyMask                   = U.empty
  , nrad                         = U.empty
  , nleaf                        = U.empty
  , cStomata                     = nan
  , cLblayer                     = nan
  , nrmlzdParprofPftDirZ         = U.empty
  , nrmlzdParprofPftDifZ         = U.empty
  , radError                     = U.empty
  , fcansno                      = nan
  , solarZenithFlag              = False
  , solarZenithAngle             = nan
  , gndAlbDif                    = U.empty
  , gndAlbDir                    = U.empty
  , fabdSunZ                     = U.empty
  , fabdShaZ                     = U.empty
  , fabiSunZ                     = U.empty
  , fabiShaZ                     = U.empty
  , edParsunZ                    = U.empty
  , edParshaZ                    = U.empty
  , fSun                         = U.empty
  , edLaisunZ                    = U.empty
  , edLaishaZ                    = U.empty
  , parprofPftDirZ               = U.empty
  , parprofPftDifZ               = U.empty
  , trSoilDir                    = U.empty
  , trSoilDif                    = U.empty
  , trSoilDirDif                 = U.empty
  , fab                          = U.empty
  , fabd                         = U.empty
  , fabi                         = U.empty
  , sabsDir                      = U.empty
  , sabsDif                      = U.empty
  , btranFt                      = U.empty
  , bstressSalFt                 = U.empty
  , nitrReproStoich              = U.empty
  , phosReproStoich              = U.empty
  , disturbanceRates             = U.empty
  , landuseTransitionRates       = U.empty
  , fractLdistNotHarvested       = nan
  , fragmentationScaler          = U.empty
  , livegrass                    = nan
  , rosFront                     = nan
  , rosBack                      = nan
  , tauL                         = nan
  , fi                           = nan
  , fire                         = fatesUnsetInt
  , fd                           = nan
  , scorchHt                     = U.empty
  , tfcRos                       = nan
  , fracBurnt                    = nan
  , tveg24                       = defaultFATERunningMean
  , tvegLpa                     = defaultFATERunningMean
  , tvegLongterm                 = defaultFATERunningMean
  , seedlingLayerPar24           = defaultFATERunningMean
  , sdlngEmergSmp                = defaultFATERunningMeanArr 0
  , sdlngMortPar                 = defaultFATERunningMean
  , sdlngMdd                     = defaultFATERunningMeanArr 0
  , sdlng2sapPar                 = defaultFATERunningMean
  }

-- | Create a new FATES patch initialized with default and input parameters.
-- Maps to the Create subroutine in FatesPatchMod.F90.
patchCreate
  :: Double  -- ^ age [years]
  -> Double  -- ^ area [m2]
  -> Int     -- ^ landUseLabel
  -> Int     -- ^ nocompPftLabel
  -> Int     -- ^ numSwb
  -> Int     -- ^ numPft
  -> Int     -- ^ numSoilLev
  -> Int     -- ^ currentTod
  -> Int     -- ^ regenerationModel
  -> FATESPatch
patchCreate !pAge !pArea !pLandUse !pNocompPft !numSwb !numPft !numSoilLev _currentTod _regModel =
  let -- Zeroed values according to ZeroValues in FatesPatchMod.F90
      zeroVecSwb = U.replicate numSwb 0.0
      zeroVecPft = U.replicate numPft 0.0
      zeroVecSoil = U.replicate numSoilLev 0.0
      
      -- Standard dimensions
      maxPftCount = 16
      nclmaxCount = 2
      nlevleafCount = 30
      
      -- AGB profile Flat maxPftCount * nDbhBins (16 * 6)
      pftAgb = U.replicate (maxPftCount * nDbhBins) nan
      
      -- 3D Profiles Flat nclmaxCount * maxPftCount * nlevleafCount
      profileSize3D = nclmaxCount * maxPftCount * nlevleafCount
      zeroVec3D = U.replicate profileSize3D 0.0
      
      -- 2D Mask / Count Flat nclmaxCount * maxPftCount
      maskSize2D = nclmaxCount * maxPftCount
      unsetVec2D = U.replicate maskSize2D fatesUnsetInt
      
      -- 4D normalized profiles Flat num_rad_stream_types * nclmax * maxpft * nlevleaf (typically stream types = 2)
      streamTypes = 2
      profileSize4D = streamTypes * nclmaxCount * maxPftCount * nlevleafCount
      zeroVec4D = U.replicate profileSize4D 0.0
      
      -- Secondary land disturbance age logic
      ageDist = if pLandUse == secondaryland
                  then pAge
                  else fatesUnsetR8
  in FATESPatch
       { patchNo                      = 1  -- Assigned a unique number during tracking
       , nocompPftLabel               = pNocompPft
       , age                          = pAge
       , ageClass                     = 1
       , area                         = pArea
       , countCohorts                 = 0
       , nclP                         = 1
       , landUseLabel                 = pLandUse
       , ageSinceAnthroDisturbance     = ageDist
       , changedLanduseThisTs         = False
       -- Leaf Organization
       , pftAgbProfile                = pftAgb
       , canopyLayerTlai              = U.replicate nclmaxCount 0.0
       , totalCanopyArea              = nan
       , totalTreeArea                = 0.0
       , totalGrassArea               = 0.0
       , zstar                        = 0.0
       , elaiProfile                  = zeroVec3D
       , esaiProfile                  = zeroVec3D
       , tlaiProfile                  = zeroVec3D
       , tsaiProfile                  = zeroVec3D
       , canopyAreaProfile            = zeroVec3D
       , canopyMask                   = unsetVec2D
       , nrad                         = unsetVec2D
       , nleaf                        = unsetVec2D
       , cStomata                     = 0.0
       , cLblayer                     = 0.0
       , nrmlzdParprofPftDirZ         = zeroVec4D
       , nrmlzdParprofPftDifZ         = zeroVec4D
       -- Radiation
       , radError                     = U.replicate numSwb 0.0
       , fcansno                      = nan
       , solarZenithFlag              = False
       , solarZenithAngle             = nan
       , gndAlbDif                    = U.replicate numSwb nan
       , gndAlbDir                    = U.replicate numSwb nan
       , fabdSunZ                     = zeroVec3D
       , fabdShaZ                     = zeroVec3D
       , fabiSunZ                     = zeroVec3D
       , fabiShaZ                     = zeroVec3D
       , edParsunZ                    = zeroVec3D
       , edParshaZ                    = zeroVec3D
       , fSun                         = zeroVec3D
       , edLaisunZ                    = zeroVec3D
       , edLaishaZ                    = zeroVec3D
       , parprofPftDirZ               = zeroVec3D
       , parprofPftDifZ               = zeroVec3D
       , trSoilDir                    = U.replicate numSwb 1.0  -- Set to 1.0 in Create
       , trSoilDif                    = U.replicate numSwb 1.0  -- Set to 1.0 in Create
       , trSoilDirDif                 = zeroVecSwb
       , fab                          = zeroVecSwb
       , fabd                         = zeroVecSwb
       , fabi                         = zeroVecSwb
       , sabsDir                      = zeroVecSwb
       , sabsDif                      = zeroVecSwb
       -- Roots
       , btranFt                      = zeroVecPft
       , bstressSalFt                 = U.replicate numPft nan
       -- Seed Rain
       , nitrReproStoich              = U.replicate numPft nan
       , phosReproStoich              = U.replicate numPft nan
       -- Disturbance
       , disturbanceRates             = U.replicate nDistTypes 0.0
       , landuseTransitionRates       = U.replicate nLanduseCats 0.0
       , fractLdistNotHarvested       = 0.0
       -- Litter
       , fragmentationScaler          = zeroVecSoil
       -- Fire
       , livegrass                    = 0.0
       , rosFront                     = 0.0
       , rosBack                      = 0.0
       , tauL                         = 0.0
       , fi                           = 0.0
       , fire                         = fatesUnsetInt
       , fd                           = 0.0
       , scorchHt                     = zeroVecPft
       , tfcRos                       = 0.0
       , fracBurnt                    = 0.0
       -- Running Means
       , tveg24                       = defaultFATERunningMean
       , tvegLpa                     = defaultFATERunningMean
       , tvegLongterm                 = defaultFATERunningMean
       , seedlingLayerPar24           = defaultFATERunningMean
       , sdlngEmergSmp                = defaultFATERunningMeanArr numSoilLev
       , sdlngMortPar                 = defaultFATERunningMean
       , sdlngMdd                     = defaultFATERunningMeanArr numSoilLev
       , sdlng2sapPar                 = defaultFATERunningMean
       }

-- | Update the total tree and grass canopy areas on a patch based on cohort data.
-- Maps to the UpdateTreeGrassArea subroutine in FatesPatchMod.F90.
updateTreeGrassArea :: FATESPatch -> [FATESCohort] -> FATESPatch
updateTreeGrassArea !patch cohorts
  | nocompPftLabel patch == nocompBareground = patch
  | otherwise =
      let (treeArea, grassArea) = foldl accumulate (0.0, 0.0) cohorts
          accumulate (!t, !g) !coh
            | isWoodyPFT (cohPft coh) = (t + cohC_area coh, g)
            | otherwise               = (t, g + cohC_area coh)
      in patch
           { totalTreeArea  = min treeArea (area patch)
           , totalGrassArea = min grassArea (area patch)
           }

-- | Calculates the sum of live grass biomass [kgC/m2] on a patch.
-- Maps to the UpdateLiveGrass subroutine in FatesPatchMod.F90.
updateLiveGrass :: FATESPatch -> [FATESCohort] -> FATESPatch
updateLiveGrass !patch cohorts =
  let !live_grass = foldl (\acc coh ->
                            if not (isWoodyPFT (cohPft coh))
                              then acc + (cohLeafC coh + cohSapwC coh + cohStructC coh) * cohN coh / area patch
                              else acc) 0.0 cohorts
  in patch { livegrass = live_grass }

-- | Helper to classify a PFT as woody vs grass (natural PFTs 1-11 are woody).
isWoodyPFT :: Int -> Bool
isWoodyPFT !ivt = ivt >= 1 && ivt <= 11
