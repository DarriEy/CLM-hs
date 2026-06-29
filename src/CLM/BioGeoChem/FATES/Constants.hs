{-# LANGUAGE BangPatterns #-}
-- | FATES constants ported from FatesConstantsMod.F90.
module CLM.BioGeoChem.FATES.Constants
  ( -- * String lengths
    fatesAvgFlagLength
  , fatesShortStringLength
  , fatesLongStringLength

    -- * Unset values
  , fatesUnsetInt
  , fatesUnsetR8
  , fatesCheckParamSet

    -- * Boolean/Integer equivalences
  , itrue
  , ifalse

    -- * DBH Bins
  , nDbhBins
  , patchfusionDbhbinLoweredges

    -- * Disturbance Types
  , nDistTypes
  , dtypeIfall
  , dtypeIfire
  , dtypeIlog
  , dtypeIlandusechange

    -- * Land Use Categories
  , nLanduseCats
  , primaryland
  , secondaryland
  , rangeland
  , pastureland
  , cropland
  , isCrop
  , nCropLuTypes

    -- * Bareground labels
  , nocompBaregroundLand
  , nocompBareground

    -- * Leaves flags
  , leavesOn
  , leavesOff
  , leavesShedding
  , ihardStressDecid
  , isemiStressDecid

    -- * Canopy layers
  , icanUpper
  , icanUstory

    -- * Nutrient uptake options
  , prescribedPUptake
  , coupledPUptake
  , prescribedNUptake
  , coupledNUptake
  , coupledNpCompScaling
  , trivialNpCompScaling

    -- * Tree regeneration
  , trsNoSeedlingDyn
  , trsRegeneration
  , defaultRegeneration
  , minMaxDbhForTrees

    -- * Age thresholds
  , secondaryAgeThreshold

    -- * Photosynthesis acclimation models
  , photosynthAcclimModelNone
  , photosynthAcclimModelKumarathungeEtal2019

    -- * Harvest units
  , hlmHarvestAreaFraction
  , hlmHarvestCarbon

    -- * Maintenance respiration models
  , lmrmodelRyan1991
  , lmrmodelAtkinEtal2017

    -- * Carbon starvation models
  , cstarvationModelLin
  , cstarvationModelExp

    -- * Error tolerances
  , callocAbsError
  , areaError1
  , areaError2
  , areaError3
  , areaError4
  , rsnblMathPrec

    -- * PFT thresholds
  , minNocompPftfracPerlanduse

    -- * Real precision limits
  , tinyr8
  , nearzero
  , fatesHuge
  , fatesTiny

    -- * Unit conversions
  , umolCToKgC
  , mgPerKg
  , gPerKg
  , kgPerG
  , mgPerG
  , kgPerMegag
  , umolPerMmmol
  , mmolPerMol
  , umolPerMol
  , molPerUmol
  , umolPerKmol
  , mPerMm
  , mmPerM
  , mmPerCm
  , mPerCm
  , m2PerHa
  , m2PerKm2
  , cm2PerM2
  , m3PerMm3
  , m3PerCm3
  , cm3PerM3
  , haPerM2
  , secPerMin
  , secPerDay
  , megajoulesPerJoule
  , daysPerSec
  , daysPerYear
  , ndaysPerYear
  , yearsPerDay
  , monthsPerYear
  , jPerKj

    -- * Physical constants
  , dewpointA
  , dewpointB
  , rgasJKKmol
  , rgasJKMol
  , tWaterFreezeK1atm
  , tWaterFreezeKTriple
  , densFreshLiquidWater
  , molarMassWater
  , molarMassRatioVapdry
  , gravEarth
  , paPerMpa
  , mpaPerPa
  , mpaPerMmSuction

    -- * Geodesy
  , earthRadiusEq
  , earthFlattening

    -- * Geometry
  , piConst
  , radPerDeg

    -- * Respiration parameters
  , lmrB
  , lmrC
  , lmrTrefC
  , lmrR1
  , lmrR2

    -- * Mortality types
  , nTermMortTypes
  , iTermMortTypeCstarv
  , iTermMortTypeCanlev
  , iTermMortTypeNumdens
  ) where

import qualified Data.Vector.Unboxed as VU

fatesAvgFlagLength :: Int
fatesAvgFlagLength = 3

fatesShortStringLength :: Int
fatesShortStringLength = 32

fatesLongStringLength :: Int
fatesLongStringLength = 199

fatesUnsetInt :: Int
fatesUnsetInt = -9999

fatesUnsetR8 :: Double
fatesUnsetR8 = -1.0e36

fatesCheckParamSet :: Double
fatesCheckParamSet = 9.9e32

itrue :: Int
itrue = 1

ifalse :: Int
ifalse = 0

nDbhBins :: Int
nDbhBins = 6

patchfusionDbhbinLoweredges :: VU.Vector Double
patchfusionDbhbinLoweredges = VU.fromList [0.0, 5.0, 20.0, 50.0, 100.0, 150.0]

nDistTypes :: Int
nDistTypes = 4

dtypeIfall :: Int
dtypeIfall = 1

dtypeIfire :: Int
dtypeIfire = 2

dtypeIlog :: Int
dtypeIlog = 3

dtypeIlandusechange :: Int
dtypeIlandusechange = 4

nLanduseCats :: Int
nLanduseCats = 5

primaryland :: Int
primaryland = 1

secondaryland :: Int
secondaryland = 2

rangeland :: Int
rangeland = 3

pastureland :: Int
pastureland = 4

cropland :: Int
cropland = 5

isCrop :: VU.Vector Bool
isCrop = VU.fromList [False, False, False, False, True]

nCropLuTypes :: Int
nCropLuTypes = 1

nocompBaregroundLand :: Int
nocompBaregroundLand = 0

nocompBareground :: Int
nocompBareground = 0

leavesOn :: Int
leavesOn = 2

leavesOff :: Int
leavesOff = 1

leavesShedding :: Int
leavesShedding = 3

ihardStressDecid :: Int
ihardStressDecid = 1

isemiStressDecid :: Int
isemiStressDecid = 2

icanUpper :: Int
icanUpper = 1

icanUstory :: Int
icanUstory = 2

prescribedPUptake :: Int
prescribedPUptake = 1

coupledPUptake :: Int
coupledPUptake = 2

prescribedNUptake :: Int
prescribedNUptake = 1

coupledNUptake :: Int
coupledNUptake = 2

coupledNpCompScaling :: Int
coupledNpCompScaling = 1

trivialNpCompScaling :: Int
trivialNpCompScaling = 2

trsNoSeedlingDyn :: Int
trsNoSeedlingDyn = 3

trsRegeneration :: Int
trsRegeneration = 2

defaultRegeneration :: Int
defaultRegeneration = 1

minMaxDbhForTrees :: Double
minMaxDbhForTrees = 15.0

secondaryAgeThreshold :: Double
secondaryAgeThreshold = 94.0

photosynthAcclimModelNone :: Int
photosynthAcclimModelNone = 1

photosynthAcclimModelKumarathungeEtal2019 :: Int
photosynthAcclimModelKumarathungeEtal2019 = 2

hlmHarvestAreaFraction :: Int
hlmHarvestAreaFraction = 1

hlmHarvestCarbon :: Int
hlmHarvestCarbon = 2

lmrmodelRyan1991 :: Int
lmrmodelRyan1991 = 1

lmrmodelAtkinEtal2017 :: Int
lmrmodelAtkinEtal2017 = 2

cstarvationModelLin :: Int
cstarvationModelLin = 1

cstarvationModelExp :: Int
cstarvationModelExp = 2

callocAbsError :: Double
callocAbsError = 1.0e-9

areaError1 :: Double
areaError1 = 1.0e-16

areaError2 :: Double
areaError2 = 1.0e-12

areaError3 :: Double
areaError3 = 10.0e-9

areaError4 :: Double
areaError4 = 1.0e-10

rsnblMathPrec :: Double
rsnblMathPrec = 1.0e-12

minNocompPftfracPerlanduse :: Double
minNocompPftfracPerlanduse = 0.01

tinyr8 :: Double
tinyr8 = 2.2250738585072014e-308

nearzero :: Double
nearzero = 1.0e-30

fatesHuge :: Double
fatesHuge = 1.7976931348623157e308

fatesTiny :: Double
fatesTiny = 2.2250738585072014e-308

umolCToKgC :: Double
umolCToKgC = 12.0e-9

mgPerKg :: Double
mgPerKg = 1.0e6

gPerKg :: Double
gPerKg = 1000.0

kgPerG :: Double
kgPerG = 0.001

mgPerG :: Double
mgPerG = 1000.0

kgPerMegag :: Double
kgPerMegag = 1000.0

umolPerMmmol :: Double
umolPerMmmol = 1000.0

mmolPerMol :: Double
mmolPerMol = 1000.0

umolPerMol :: Double
umolPerMol = 1.0e6

molPerUmol :: Double
molPerUmol = 1.0e-6

umolPerKmol :: Double
umolPerKmol = 1.0e9

mPerMm :: Double
mPerMm = 1.0e-3

mmPerM :: Double
mmPerM = 1.0e3

mmPerCm :: Double
mmPerCm = 10.0

mPerCm :: Double
mPerCm = 1.0e-2

m2PerHa :: Double
m2PerHa = 1.0e4

m2PerKm2 :: Double
m2PerKm2 = 1.0e6

cm2PerM2 :: Double
cm2PerM2 = 10000.0

m3PerMm3 :: Double
m3PerMm3 = 1.0e-9

m3PerCm3 :: Double
m3PerCm3 = 1.0e-6

cm3PerM3 :: Double
cm3PerM3 = 1.0e6

haPerM2 :: Double
haPerM2 = 1.0e-4

secPerMin :: Double
secPerMin = 60.0

secPerDay :: Double
secPerDay = 86400.0

megajoulesPerJoule :: Double
megajoulesPerJoule = 1.0e-6

daysPerSec :: Double
daysPerSec = 1.0 / 86400.0

daysPerYear :: Double
daysPerYear = 365.0

ndaysPerYear :: Int
ndaysPerYear = 365

yearsPerDay :: Double
yearsPerDay = 1.0 / 365.0

monthsPerYear :: Double
monthsPerYear = 12.0

jPerKj :: Double
jPerKj = 1000.0

dewpointA :: Double
dewpointA = 17.62

dewpointB :: Double
dewpointB = 243.12

rgasJKKmol :: Double
rgasJKKmol = 8314.4598

rgasJKMol :: Double
rgasJKMol = 8.3144598

tWaterFreezeK1atm :: Double
tWaterFreezeK1atm = 273.15

tWaterFreezeKTriple :: Double
tWaterFreezeKTriple = 273.16

densFreshLiquidWater :: Double
densFreshLiquidWater = 1.0e3

molarMassWater :: Double
molarMassWater = 18.0

molarMassRatioVapdry :: Double
molarMassRatioVapdry = 0.622

gravEarth :: Double
gravEarth = 9.8

paPerMpa :: Double
paPerMpa = 1.0e6

mpaPerPa :: Double
mpaPerPa = 1.0e-6

mpaPerMmSuction :: Double
mpaPerMmSuction = densFreshLiquidWater * gravEarth * 1.0e-9

earthRadiusEq :: Double
earthRadiusEq = 6378137.0

earthFlattening :: Double
earthFlattening = 1.0 / 298.257223563

piConst :: Double
piConst = 3.14159265359

radPerDeg :: Double
radPerDeg = piConst / 180.0

lmrB :: Double
lmrB = 0.1012

lmrC :: Double
lmrC = -0.0005

lmrTrefC :: Double
lmrTrefC = 25.0

lmrR1 :: Double
lmrR1 = 0.2061

lmrR2 :: Double
lmrR2 = -0.0402

nTermMortTypes :: Int
nTermMortTypes = 3

iTermMortTypeCstarv :: Int
iTermMortTypeCstarv = 1

iTermMortTypeCanlev :: Int
iTermMortTypeCanlev = 2

iTermMortTypeNumdens :: Int
iTermMortTypeNumdens = 3
