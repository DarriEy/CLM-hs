-- | Physical constants used throughout CLM.
-- Preserves Fortran variable names for traceability.
module CLM.Constants.PhysicalConstants
  ( -- * Fundamental constants
    tfrz
  , rgas
  , grav
  , sb
    -- * Water/ice properties
  , denh2o
  , denice
  , cpice
  , cpliq
  , hfus
  , hvap
  , hsub
    -- * Atmospheric
  , rair
  , cpair
    -- * Soil/snow
  , tkwat
  , tkice
  , tkair
    -- * Grid dimensions
  , nlevsoi
  , nlevgrnd
  , nlevsno
  , numrad
  ) where

-- | Freezing point of water [K]
tfrz :: Double
tfrz = 273.15

-- | Universal gas constant [J/K/kmol]
rgas :: Double
rgas = 8314.46

-- | Gravitational acceleration [m/s²]
grav :: Double
grav = 9.80616

-- | Stefan-Boltzmann constant [W/m²/K⁴]
sb :: Double
sb = 5.67e-8

-- | Density of liquid water [kg/m³]
denh2o :: Double
denh2o = 1000.0

-- | Density of ice [kg/m³]
denice :: Double
denice = 917.0

-- | Specific heat of ice [J/kg/K]
cpice :: Double
cpice = 2117.27

-- | Specific heat of liquid water [J/kg/K]
cpliq :: Double
cpliq = 4188.0

-- | Latent heat of fusion [J/kg]
hfus :: Double
hfus = 0.3337e6

-- | Latent heat of vaporization [J/kg]
hvap :: Double
hvap = 2.501e6

-- | Latent heat of sublimation [J/kg]
hsub :: Double
hsub = 2.501e6 + 0.3337e6

-- | Dry air gas constant [J/kg/K]
rair :: Double
rair = 287.0423

-- | Specific heat of dry air at constant pressure [J/kg/K]
cpair :: Double
cpair = 1004.64

-- | Thermal conductivity of water [W/m/K]
tkwat :: Double
tkwat = 0.57

-- | Thermal conductivity of ice [W/m/K]
tkice :: Double
tkice = 2.29

-- | Thermal conductivity of air [W/m/K]
tkair :: Double
tkair = 0.023

-- | Number of soil layers
nlevsoi :: Int
nlevsoi = 20

-- | Number of ground layers (soil + bedrock)
nlevgrnd :: Int
nlevgrnd = 25

-- | Maximum number of snow layers
nlevsno :: Int
nlevsno = 12

-- | Number of radiation bands (visible + NIR)
numrad :: Int
numrad = 2
