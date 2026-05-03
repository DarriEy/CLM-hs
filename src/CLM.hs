-- | CLM.hs — Top-level module for the Community Land Model (Haskell port)
--
-- This is a Haskell port of CLM/CTSM from Fortran 90.
-- Due to the large number of modules with overlapping names,
-- this module re-exports only the core constants, types, and solvers.
-- For physics modules, import them directly (e.g., import CLM.BioGeoPhys.SoilTemperature).
module CLM
  ( -- * Constants
    module CLM.Constants.PhysicalConstants
  , module CLM.Constants.ControlFlags
  , module CLM.Constants.VarPar
  , module CLM.Constants.PFTConstants
  , module CLM.Constants.LandunitConstants
  , module CLM.Constants.ColumnConstants
    -- * Core Types
  , module CLM.Types.ColumnData
  , module CLM.Types.PatchData
  , module CLM.Types.WaterStateData
  , module CLM.Types.TemperatureData
  , module CLM.Types.EnergyFluxData
  , module CLM.Types.WaterFluxData
  , module CLM.Types.CanopyStateData
  , module CLM.Types.SoilStateData
  , module CLM.Types.FrictionVelocityData
  , module CLM.Types.Atm2LndData
  , module CLM.Types.SolarAbsorbedData
  , module CLM.Types.WaterDiagnosticBulkData
  , module CLM.Types.WaterStateBulkData
  , module CLM.Types.WaterFluxBulkData
  , module CLM.Types.LakeStateData
    -- * Infrastructure (core solvers and utilities)
  , module CLM.Infrastructure.Tridiagonal
  , module CLM.Infrastructure.BandDiagonal
  , module CLM.Infrastructure.Filters
  , module CLM.Infrastructure.Decomp
  , module CLM.Infrastructure.InitVertical
  , module CLM.Infrastructure.TimeManager
  , module CLM.Infrastructure.Orbital
  , module CLM.Infrastructure.Accumulator
    -- * Core Biogeophysics
  , module CLM.BioGeoPhys.QSat
  , module CLM.BioGeoPhys.DayLength
  ) where

import CLM.Constants.PhysicalConstants
import CLM.Constants.ControlFlags
import CLM.Constants.VarPar hiding (numrad)
import CLM.Constants.PFTConstants
import CLM.Constants.LandunitConstants
import CLM.Constants.ColumnConstants
import CLM.Types.ColumnData
import CLM.Types.PatchData
import CLM.Types.WaterStateData
import CLM.Types.TemperatureData
import CLM.Types.EnergyFluxData
import CLM.Types.WaterFluxData
import CLM.Types.CanopyStateData
import CLM.Types.SoilStateData
import CLM.Types.FrictionVelocityData
import CLM.Types.Atm2LndData
import CLM.Types.SolarAbsorbedData
import CLM.Types.WaterDiagnosticBulkData
import CLM.Types.WaterStateBulkData
import CLM.Types.WaterFluxBulkData
import CLM.Types.LakeStateData
import CLM.Infrastructure.Tridiagonal
import CLM.Infrastructure.BandDiagonal
import CLM.Infrastructure.Filters
import CLM.Infrastructure.Decomp
import CLM.Infrastructure.InitVertical
import CLM.Infrastructure.TimeManager
import CLM.Infrastructure.Orbital
import CLM.Infrastructure.Accumulator
import CLM.BioGeoPhys.QSat
import CLM.BioGeoPhys.DayLength
