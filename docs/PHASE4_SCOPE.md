# Phase 4 Scope — Multi-landunit, restart, history (the architectural ceiling)

## TL;DR — the decisive constraint

The headline item is the **multi-landunit driver loop**. The architecture map says it
is *tractable* (loop the existing single-column kernel; no physics rewrite — the subgrid
machinery is already built). **But the current test gridcell is 100% soil**
(`wt_lunit = (1, 0, 0, …)`, `lun_wtgcell = (1, 0)`): the lake landunit/column exists in
the geometry but has **zero gridcell weight**. So a multi-landunit loop on the current
data produces **identical gridcell results** — the soil column is 100% of the gridcell.

Therefore most of Phase 4 (the multi-landunit work and everything it unlocks) is, on the
data we have, **unexercisable and unvalidatable** — building it now adds a large body of
code that does not run, the exact thing the 2026-06 audit set out to eliminate. The real
prerequisite is **bigger than the code**: a mixed-landunit gridcell dataset + a reference
to validate the aggregated output against. Both are absent.

## Architecture verdict (from the code map)

- **CLMState = one column + multi-patch.** Patch dimension is already vectorized
  (`cstate_*_patch` length np=4); the column dimension is scalar (`t_grnd_col`, `clmSnl`,
  `lakedepth`, `eflx_soil_grnd_col`). Per-layer fields are vectors for one column.
- **Subgrid machinery is COMPLETE but unused**: `InitSubgrid` (addLandunit/Column/Patch +
  down-pointers), `SubgridWeights` (g/l/c/p weights, active flags), `SubgridAverage`
  (p2c/c2l/c2g/l2g with scale types), `Filters` (soil/lake/urban masks). CLMState never
  instantiates the hierarchy.
- **Data loader reads column 0 only** (`extractCol1 … (j*nc + 0)`), silently dropping the
  lake column.
- **Recommended approach: Option A — loop the single-column kernel over N columns**, with
  per-column state extract/scatter and gridcell aggregation via the existing
  `SubgridAverage`. NOT full field vectorization (Option B, far more invasive).

## Items — effort, risk, validation path

| # | Item | Effort | Validatable on current data? |
|---|------|--------|------|
| 12 | Multi-landunit / multi-column driver loop | **L–XL** | **No** (100%-soil gridcell → identical output) |
| 13 | Lake + urban actually run — **LAKE DONE**: full lake-temperature solve wired + 3 latent index bugs fixed, validated to evolve a sane profile from a Fortran lake restart (urban still a stub) | M (lake) / M–L (urban) | lake: runs+sane (no tight parity); urban: No |
| 14 | Glacier surface mass balance (write `GlacierSurfaceMassBalanceMod`, not ported) | L | No |
| 15 | **Restart I/O** — **DONE**: write→read→resume **bit-identical** (validated); Fortran-NetCDF read pending | L | **Yes** ✅ |
| 16 | **NetCDF history** — **DONE** (single-tape): NetCDF writer + `writeDailyNetCDF`, round-trip validated; multi-tape/subgrid + Fortran-h0 compare pending | L | **Yes** ✅ |
| 17 | External streams (N-dep, LAI, crop calendar, urban-tv) | M | partial |
| 18 | atm→lnd downscaling + lnd→atm coupling | M | partial |

Note on #12 effort: the architecture agent's "~200–300 lines" covers the lake-loop
skeleton only. The honest surface is larger — per-column extract/scatter across **all**
state records (Temperature / Water / Soil / SoilHydro / EnergyFlux / CN — each carrying
column-scalars plus per-layer-per-column data), per-column CN/snow/hydrology state, and
gridcell aggregation of every diagnostic. Treat it as a substantial refactor.

Note on #13 urban: `urbanFluxesStep` is currently a passthrough no-op **and checks the
wrong landunit type** (`it /= 6`; 6 is wetland, urban is 7–9) — fix + wire the real
UrbanFluxes/Albedo/Radiation with column-type dispatch (roof/sunwall/shadewall/road).
`lakeTemperatureStep` computes thermal props then returns state unchanged — make it
solve+apply.

## The prerequisite that gates 12/13/14/17/18

To make the multi-landunit work *exercised and validated* you need, before the code:
1. **A mixed-landunit gridcell dataset** — surfdata with non-zero lake/urban/glacier/crop
   fractions + per-column initial state (cold-start or restart).
2. **A reference** (Fortran or Julia) for that gridcell to validate the aggregated output.

Both are absent, and regenerating instrumented Fortran references is itself blocked (the
pdump instrumentation is no longer in the installed Fortran source). Without (1)+(2),
Phase 4 multi-landunit is unverifiable scope expansion.

## Recommended sequencing

Given the validation gap, the two highest-value Phase 4 items are the ones that are
**independently useful AND validatable on existing data**:

- **#15 Restart I/O** — validatable (write→read→run must be bit-identical; reading a real
  Fortran restart and reproducing step 1 is a hard check), and it unlocks **warm-start from
  Fortran initial conditions** — which directly addresses the IC-confounding behind the
  long-standing ~2 K free-running residual.
- **#16 NetCDF history** — validatable against Fortran history; makes output
  machine-comparable to CTSM instead of CSV.

The multi-landunit loop (#12) and lake/urban/glacier (#13/#14) are large **and** blocked on
mixed-landunit data + reference — defer until that data exists, or treat as explicit
research scope rather than a validatable port step.

## Bottom line

Phase 4's headline (multi-landunit) is architecturally reachable via the column-loop
approach, but its payoff is gated on test data + references that don't exist — so doing it
now reproduces the dead-code problem at the gridcell level. The genuinely productive Phase 4
work *right now* is **restart + NetCDF history**: both validatable, both independently
useful, and restart additionally enables warm-start from Fortran ICs. The multi-landunit /
non-soil-landunit items should wait for (or be bundled with) creating a mixed-landunit
dataset and its reference.
