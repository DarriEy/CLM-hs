# FATES Porting Plan (100% Fortran Canon)

This plan details the modular breakdown for porting the Functionally Assembled Terrestrial Ecosystem Simulator (FATES) from the original Fortran source (`src/fates/`) to `src/CLM/BioGeoChem/FATES/` in Haskell.

## Strategy

1. **Shared Types & Constants**: Define core cohort, patch, and configuration data records.
2. **Deterministic Module Porting**: Port FATES biology and physical kernels in dependency order.
3. **Automated Ralph Harness Gating**: Creating the skeleton files with `TODO`/`placeholder` markers will allow `scripts/ralph_harness.py` to automatically register them as tasks and solve them iteratively via the `oracle` (`stack test`).

## Module Mapping (Fortran -> Haskell)

| Fortran Module | Haskell Target Module | Description | Status |
|----------------|-----------------------|-------------|--------|
| `FatesConstantsMod.F90` | `CLM.BioGeoChem.FATES.Constants` | Physical/biological constants, parameters | ❌ Pending |
| `FatesInterfaceTypesMod.F90` | `CLM.BioGeoChem.FATES.Types` | Cohort, Patch, Site structures and State | ❌ Pending |
| `FatesGlobals.F90` | `CLM.BioGeoChem.FATES.Globals` | Global configuration and gridcell tracking | ❌ Pending |
| `FatesUtilsMod.F90` | `CLM.BioGeoChem.FATES.Utils` | Numerical utilities, sorting, indexing | ❌ Pending |
| `FatesAllometryMod.F90` | `CLM.BioGeoChem.FATES.Allometry` | Biomass-to-dimension allocation models | ❌ Pending |
| `FatesCohortMod.F90` | `CLM.BioGeoChem.FATES.Cohort` | Cohort structural & metabolic dynamics | ❌ Pending |
| `FatesPatchMod.F90` | `CLM.BioGeoChem.FATES.Patch` | Patch age, light profile, and area dynamics | ❌ Pending |
| `FatesPlantRespPhotosynthMod.F90` | `CLM.BioGeoChem.FATES.Vegetation` | Stomatal resistance & photosynthesis | ❌ Pending |
| `FatesPlantHydraulicsMod.F90` | `CLM.BioGeoChem.FATES.Hydraulics` | Canopy-to-root flow, xylem pressure, bstress | ❌ Pending |
| `FatesRadiationDriveMod.F90` | `CLM.BioGeoChem.FATES.Radiation` | Norman radiation / Two-Stream light split | ❌ Pending |
| `FatesFuelMod.F90` | `CLM.BioGeoChem.FATES.Fire` | Fuel loading, fire spread & cohort combustion | ❌ Pending |
| `FatesIntegratorsMod.F90` | `CLM.BioGeoChem.FATES.Driver` | Cohort recruitment, mortality, daily integration | ❌ Pending |

## Integration Phase

1. **Wire FATES modes** in `ControlFlags` and `CLMDriverConfig`.
2. **Hook up** `FATESInterface.hs` back to the live pipeline in `PhysicsAdapters.hs` (instead of identity bypasses).
