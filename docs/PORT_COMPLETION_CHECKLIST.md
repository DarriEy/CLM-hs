# Port Completion Checklist

This checklist is the tracked debt list for reaching a defensible full CLM
Fortran port. The Hspec port audit fails while any production source file still
contains `placeholder`, `stub`, `TODO`, `no-op`, `not yet`, or
`simplified`.

Run:

```sh
stack --install-ghc test --test-arguments '--match "Port completion audit"'
```

> **The keyword-marker audit below UNDERSTATES the real gap.** It only catches
> files that *say* "stub/simplified". It does NOT catch (a) whole Fortran
> subsystems with no Haskell file, (b) Haskell modules that exist but are never
> called from the driver (dead code), or (c) pipeline steps wired to a no-op
> body. The audit-level section below (2026-06 Fortran-vs-Haskell comparison) is
> the authoritative gap list; see `PARITY_STATUS.md` and `ROADMAP.md`.

## Audit-Level Gaps (2026-06) — the real list

### Whole subsystems missing (no Haskell file, or types-only)
- [ ] `dyn_subgrid` — transient land use, dynamic landunits/PFTs/columns, harvest,
  conservation-on-area-change. **0% ported** (19 Fortran modules). Root cause of the
  single-column ceiling.
- [ ] FATES — cohort/patch demography, PARTEH, ED radiation, plant hydraulics, fire.
  **~0%**: `FATESInterface.hs` is boundary types + `= id` no-ops.
- [ ] Restart I/O — real model-state save/restore (NetCDF + subgrid pointers). Current
  `RestartIO.hs` writes only a registry descriptor → **cold-start only**.
- [ ] NetCDF history (`histFileMod`/`clmCgrid`) — multi-tape, subgrid-aggregated.
  Current output is CSV daily-average, single gridcell.
- [ ] Multi-landunit subgrid driver (urban/glacier/lake/crop/wetland actually run).
  Code paths exist but the run is 100% soil; driver is single-column/single-patch.
- [ ] atm→lnd downscaling (`atm2lndMod`) and lnd→atm coupling (`lnd2atmMod`).
- [ ] External data streams: N-deposition, LAI, crop calendar, urban time-varying.
- [ ] MPI/OpenMP domain decomposition (`decompMod`/`spmdMod`).
- [ ] `GlacierSurfaceMassBalanceMod` (962 lines), standalone `FrictionVelocityMod`,
  `HumanIndexMod`, `CNSoilMatrixMod`, `SoilBiogeochemNitrogenUptakeMod`, `TillageMod`.

### Live-pipeline steps wired to a no-op (verified)
- [ ] `snowPercolationStep` (`ppSnowWater`) = `st` — snow meltwater percolation absent.
- [ ] `lakeTemperatureStep` — computes thermal props then returns `st` unchanged.
- [ ] `urbanFluxesStep` — returns `st`; also tests the wrong landunit type (`it /= 6`).
- [ ] `cnBalanceCheckStep` / CN precision control — no conservation enforcement.

### CN/BGC modules that exist but are NEVER CALLED from the driver (dead code)
- [ ] Fire (`FireBase`/`FireLi2014`), growth respiration (`GrowthResp`), gap mortality
  (`GapMortality`), dynamic vegetation (`CNDV`), carbon isotopes (`CIsoFlux`/
  `CarbonIsotopes`), wood products/harvest (`CNProducts`), `CNAnnualUpdate`, `Methane`,
  dust/VOC emissions. (0 references in `src/CLM/Driver/`.)
- [ ] Crop allocation path — none (crops run as grass).
- [ ] Phenology onset/offset (seasonal & stress deciduous) — only background litterfall
  runs; effectively evergreen.
- [ ] N fixation and N deposition — N budget is sinks-only.

### Corrections to "cleared" items below
The `[x]` markers on `RestartIO.hs`, `CLMRun.hs`, and `HistoryWriter.hs` mean only that
the *keyword wording* was removed — the underlying functionality is still a stub
(registry-only restart; CSV-only history). Treat them as open per the audit above.

## Open Source-Debt Audit (keyword markers — incomplete by construction)

The current audit covers every Haskell file under `src/` and `app/`. Each item
below must be resolved by replacing the marked implementation with a faithful
port, or by documenting and testing why the marker is no longer debt and then
removing the marker wording from production source.

- [ ] `src/CLM/BioGeoChem/LitterVertTransp.hs` - vertical litter transport and Patankar/tridiagonal path still marked simplified/placeholder.
- [x] `src/CLM/Driver/CLMDriver.hs` - driver audit wording cleared; date rollover now advances month/year in the no-leap calendar.
- [ ] `src/CLM/BioGeoPhys/SnowSNICAR.hs` - spectral band/radius indexing and flux distribution still marked simplified/placeholder.
- [ ] `src/CLM/BioGeoChem/FATESInterface.hs` - FATES integration points are documented no-op stubs.
- [x] `src/CLM/Driver/CLMInitialize.hs` - initialization audit wording cleared for the compatibility and binary readers.
- [ ] `src/CLM/BioGeoPhys/SoilHydrology.hs` - perched water table, drainage, RHS, and water-table assumptions remain simplified.
- [x] `src/CLM/Infrastructure/RestartIO.hs` - legacy IO entry points now write/read a registry descriptor; binary restart remains the state-carrying path.
- [x] `src/CLM/Driver/Simulation.hs` - legacy simulation audit wording cleared.
- [x] `src/CLM/Driver/PhysicsAdapters.hs` - driver-adapter audit wording cleared; snow fraction and forcing/radiation scaling are now under parity tests.
- [x] `src/CLM/Driver/CLMRun.hs` - top-level run loop now invokes the pipeline and writes daily history output.
- [ ] `src/CLM/BioGeoPhys/UrbanFluxes.hs` - urban canyon wind/aerodynamic conductance path still marked placeholder/simplified.
- [ ] `src/CLM/BioGeoPhys/UrbanAlbedo.hs` - full vectorized urban albedo path not complete.
- [ ] `src/CLM/BioGeoChem/Decomp.hs` - decomposition cascade still marked skeleton/simplified.
- [x] `src/CLM/Infrastructure/HistoryWriter.hs` - compatibility writer now emits daily CSV rows from accumulated fields.
- [x] `src/CLM/Infrastructure/ForcingReader.hs` - compatibility reader now loads binary directories or NetCDF forcing files.
- [x] `src/CLM/Infrastructure/ColdStart.hs` - cold-start audit wording cleared.
- [ ] `src/CLM/BioGeoPhys/SoilWaterMovement.hs` - baseflow sink still returns zero vector.
- [ ] `src/CLM/BioGeoPhys/HydrologyDrainage.hs` - water mass and glacier SMB runoff are stubs.
- [x] `src/CLM/Infrastructure/Topo.hs` - topography audit wording cleared; glacier MEC parity remains covered by tests still to add.
- [x] `src/CLM/Infrastructure/SubgridWeights.hs` - single-gridcell compatibility indexing audit wording cleared.
- [ ] `src/CLM/BioGeoPhys/SurfaceRadiation.hs` - absorbed soil-layer flux distribution is simplified.
- [ ] `src/CLM/BioGeoPhys/SurfaceHumidity.hs` - multi-layer soil alpha path is missing.
- [ ] `src/CLM/BioGeoPhys/SoilWaterPlantSink.hs` - per-patch redistribution is simplified.
- [ ] `src/CLM/BioGeoPhys/SoilTemperature.hs` - urban and excess-ice coordinate-stretching cases are not handled.
- [ ] `src/CLM/BioGeoPhys/SatellitePhenology.hs` - phenology data IO is stubbed.
- [ ] `src/CLM/BioGeoPhys/SWRC.hs` - placeholder retained from Julia path.
- [ ] `src/CLM/BioGeoPhys/PreFluxCalcs.hs` - urban emissivity remains placeholder.
- [ ] `src/CLM/BioGeoPhys/LakeTemperature.hs` - bottom thermal property path still marked placeholder.
- [ ] `src/CLM/BioGeoPhys/HydrologyNoDrainage.hs` - identity indexing remains in translated path.
- [ ] `src/CLM/BioGeoPhys/CanopyFluxes.hs` - biomass heat storage path simplified.
- [ ] `src/CLM/BioGeoChem/VegetationFacade.hs` - fire and litterfall coupling still placeholder/approximation.
- [ ] `src/CLM/BioGeoChem/NutrientCompetition.hs` - single-patch nitrogen allocation remains simplified.
- [ ] `src/CLM/BioGeoChem/NStateUpdate1.hs` - only simplified non-woody/non-crop path is implemented.
- [ ] `src/CLM/BioGeoChem/CStateUpdate1.hs` - only simplified non-woody/non-crop path is implemented.
- [ ] `src/CLM/BioGeoChem/Methane.hs` - diffusion path is simplified explicit form.
- [ ] `src/CLM/BioGeoChem/DecompMIMICS.hs` - MIMICS cascade initialization is simplified.
- [ ] `src/CLM/BioGeoChem/DecompBGC.hs` - O2 scalar is simplified.
- [ ] `src/CLM/BioGeoChem/CStateUpdate3.hs` - fire/storage transfer fluxes simplified.
- [ ] `src/CLM/BioGeoChem/CNDV.hs` - establishment/tree handling simplified.
- [ ] `src/CLM/BioGeoChem/GapMortality.hs` - litter fraction storage remains simplified.
- [x] `src/CLM/Infrastructure/Orbital.hs` - orbital audit wording cleared.
- [x] `src/CLM/Infrastructure/SurfData.hs` - surface-data compatibility reader audit wording cleared; full NetCDF field coverage remains a parity task.
- [x] `src/CLM/Infrastructure/ReadParams.hs` - parameter compatibility reader audit wording cleared; full NetCDF parameter coverage remains a parity task.
- [x] `src/CLM/Infrastructure/TimeManager.hs` - no-leap calendar audit wording cleared.
- [ ] `src/CLM/BioGeoPhys/Luna.hs` - LUNA optimization loop remains simplified.
- [ ] `src/CLM/BioGeoPhys/QSat.hs` - simplified no-derivative result needs parity scope.

## Parity Gates

- [ ] Cold-start initialization has strict Julia/Fortran reference comparisons.
- [ ] Surface radiation has field-by-field golden tests for albedo, absorbed fluxes, and canopy/snow split.
- [ ] Surface fluxes have golden tests for sensible heat, latent heat, ground flux, and aerodynamic terms.
- [ ] Soil temperature has layer-wise golden tests for `T_SOISNO`, phase change, and heat fluxes.
- [ ] Hydrology has golden tests for infiltration, drainage, runoff, water table, snow water, and mass balance.
- [ ] CN biogeochemistry has pool/flux conservation tests against Julia/Fortran trajectories.
- [ ] End-to-end daily trajectory matches the Julia reference within documented
  per-field tolerances, including absorbed shortwave `FSA`.
