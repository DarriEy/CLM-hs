# CLM-hs → Fortran CLM5 Parity Plan

Goal: bring the Haskell port to numerical parity with the original Fortran
CLM/CTSM (`/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/installs/clm`),
following the methodology proven by the sister Julia port (`../CLM.jl`).

## Strategy (copied from CLM.jl)

CLM.jl reached Fortran parity not by re-running Fortran in a loop, but with a
**single-step boundary-injection harness**:

1. An instrumented `cesm.exe` (already built, already run) dumped per-boundary
   CLM5 restart-format NetCDF snapshots of the full model state.
2. The harness reads a `before_step` dump, **injects** that exact Fortran state
   into a fresh single-column model, runs **one timestep**, and **diffs** the
   evolved state field-by-field against the same-step `after_<boundary>` dump.
3. Because state is re-injected every step, this measures **per-step
   translation error**, not compounded drift — it pinpoints which module
   diverges.

We replicate this in Haskell. The Fortran reference data already exists on disk;
we do **not** need to rebuild or rerun Fortran for the core loop.

### Reference data (on disk, do not regenerate)

- Per-boundary dumps: `…/clm_bgc_spinup/bgc_ref_summer/pdump_<boundary>_n<nstep>.nc`
  - 28-step summer window, `n1757845 … n1757872`, **8 boundaries**, 416 vars each.
  - Boundaries (in driver order): `before_step`, `after_canopyfluxes`,
    `after_soiltemperature`, `after_soilfluxes`, `after_hydrologynodrainage`,
    `after_hydrologydrainage`, `after_ecosysdyn_predrain`, `after_competition`.
- SP (no-CN) per-step dumps: `…/clm_parity_run/pdump_*_n{13461,13470}.nc`
- Single-year history: `CLM.jl/test/reference_data/fortran_clm5_bow_2009.nc`
- Fallback (if fresh dumps ever needed): compiled `cesm.exe` at
  `…/installs/clm/cases/symfluence_build/bld/cesm.exe`, Bow-at-Banff case configured.

### Site / config of record

Bow at Banff, single column, 3 patches: bare ground, needleleaf-evergreen tree,
C3 grass. `use_cn`, PHS (`use_hydrstress`), LUNA (`use_luna`; vcmax25/jmax25
injected from dumps), `dtime=3600`, Jackson root profile, `forc_pco2 = 367e-6*pbot`.

### Parity metric & tolerances (from CLM.jl)

Metric: NaN-aware relative error `|F−J| / (1 + max(|F|,|J|))` plus absolute diff.

| Field | kind | tol (abs) |
|---|---|---|
| T_GRND | col1d | 0.20 K |
| T_SOISNO | col2d | 0.20 K |
| T_VEG | patch | 1.20 K |
| T_STEM | patch | 0.50 K |
| SABV_P | patch | 5.0 W/m² |
| SABG_P | patch | 5.0 W/m² |
| EFLX_GNET_P | patch | 6.0 W/m² |
| ZWT | col1d | 0.02 m |
| ZWT_PERCH | col1d | 0.05 m |
| H2OSOI_LIQ | col2d | 0.05 |
| H2OSOI_ICE | col2d | 0.05 |
| WA | col1d | 1.0 |
| H2OSFC | col1d | 1e-3 |
| SNOW_DEPTH | col1d | 1e-3 |
| frac_sno | col1d | 1e-3 |

CN pools (after_competition / after_ecosysdyn_predrain): leafc, frootc,
livestemc, deadstemc, soil1/2/3c_vr, litr1/2/3c_vr, cwdc_vr, sminn_vr,
smin_no3_vr, smin_nh4_vr. Acceptance: per-step rel-error < ~1%, bounded
free-running drift.

## What exists vs. must be built

| Component | Status |
|---|---|
| NetCDF read (`ncReadDouble1D/2D`, `ncDimLen`, `ncHasVar`) | ✅ reuse |
| `CLMState` record (all fields) | ✅ reuse |
| `clmDrv` single full timestep | ✅ reuse |
| `PhysicsPipeline` adapter slots (32) | ✅ reuse |
| pdump → `CLMState` injector | ❌ build (Phase 0) |
| Bow-at-Banff initializer | ❌ build (Phase 0) |
| Diff / field-registry / tolerance framework | ❌ build (Phase 0) |
| Per-boundary snapshot points in `clmDrv` | ❌ build (Phase 0b) |
| Gated hspec parity test | ❌ build (Phase 0) |

## Execution phases

### Phase 0 — Parity harness (SERIAL, foundational gate)
0a. `CLM.Calibration.FortranParity`: pdump NetCDF reader, field registry
    (Fortran name → `CLMState` lens), NaN-aware abs/rel diff, tolerance table.
0b. Bow injector: build a `CLMState` (+ `TimestepContext`) from a `before_step`
    dump; pull static geometry/params from dump + surfdata where the dump lacks them.
0c. Single-step runner: inject → `clmDrv` one step → diff vs `after_hydrologydrainage`.
0d. Boundary snapshots: expose intermediate states from `clmDrv`
    (after_canopyfluxes, after_soiltemperature, after_soilfluxes,
    after_hydrologynodrainage) so each module is diffed at its own boundary.
0e. Gated hspec test (`@test_skip` semantics) + emit a **baseline report**:
    per-field max abs/rel over the 28-step window → tells us which modules fail.

### Phase 1+ — Module parity (PARALLEL, fan out after Phase 0 baseline)
Each failing field/module is an independent fix: run harness → read its diff →
port the simplified code faithfully against Fortran + CLM.jl → re-run to tolerance.
Grouped by boundary so agents touch disjoint modules (worktree isolation):

- **Radiation** (after_canopyfluxes inputs): SurfaceRadiation absorbed-flux split,
  SnowSNICAR spectral indexing → SABV/SABG/FSA.
- **Canopy fluxes** (after_canopyfluxes): CanopyFluxes biomass heat storage,
  SurfaceHumidity multi-layer alpha → T_VEG/T_STEM/EFLX_GNET.
- **Soil temperature** (after_soiltemperature): SoilTemperature urban/excess-ice → T_SOISNO/T_GRND.
- **Hydrology** (after_hydrology{nodrainage,drainage}): SoilHydrology water table,
  SoilWaterMovement baseflow, HydrologyDrainage water mass/glacier SMB → H2OSOI/ZWT/runoff.
- **CN/BGC** (after_ecosysdyn_predrain / after_competition): CStateUpdate1/NStateUpdate1
  woody+crop, Decomp cascade, NutrientCompetition, Methane diffusion → pools.

Each item maps 1:1 to `docs/PORT_COMPLETION_CHECKLIST.md`.

### Phase 2 — Multi-step drift & end-to-end
Free-running 28-step (then full-year) drift bounded check; end-to-end daily
trajectory vs `julia_daily_avg.csv` / Fortran history within documented tolerances.

## Phase 0 baseline (recorded 2026-06-17)

Harness `CLM.Calibration.FortranParity` complete: real Bow forcing
(`clmforc.2003.nc` at the dump timestamp) + per-boundary snapshots + identity
check. **Identity check = 0.0 for every injected field** → injector is faithful,
so all errors below are physics. Single-column Bow, n1757845 + 28-step window:

| field | boundary | step-1 absΔ | 28-step max absΔ | tol | verdict |
|---|---|---|---|---|---|
| SABV_P | after_canopyfluxes | 35.6 (rel .76) | 113 (rel .97) | 5.0 | **radiation broken** |
| SABG_P | after_canopyfluxes | 61 (rel .84) | 242 (rel .98) | 5.0 | **radiation broken** |
| H2OSOI_LIQ | after_hydrologynodrainage | 52.7 (rel .98) | 52.7 | 0.05 | **hydrology broken** |
| ZWT | after_hydrologydrainage | 2.72 m (rel .45) | 2.72 | 0.02 | **hydrology broken** |
| ZWT_PERCH | after_hydrologydrainage | 0.57 | 0.57 | 0.05 | hydrology broken |
| T_GRND / T_SOISNO | after_soiltemperature | 4.48 K | 9.3 K | 0.20 | soil-temp (downstream of SABG) |
| T_VEG | after_canopyfluxes | 0.87 K | 8.6 K | 1.20 | step-1 PASS; drifts with radiation |
| H2OSOI_ICE, H2OSFC, SNOW_DEPTH, frac_sno | — | 0 | 0 | — | PASS (summer→0) |

**Attribution → Phase 1 waves (dependency-ordered):**
- Wave 1a **Radiation** (upstream, structural even with correct forcing):
  `SurfaceRadiation` absorbed-flux split, `SurfaceAlbedo`/`SnowSNICAR` → SABV_P/SABG_P.
- Wave 1b **Hydrology** (independent): `SoilWaterMovement` + `SoilHydrology` +
  `HydrologyDrainage` → H2OSOI_LIQ/ZWT/ZWT_PERCH.
- Wave 2 **Soil temperature**: `SoilTemperature` → T_SOISNO/T_GRND (inherits SABG,
  so do after radiation).

Build constraint: one Stack build-lock per repo, so parallel agents serialize on
build/verify unless run in separate git worktrees (which pay a full cold rebuild).

## Progress (updated 2026-06-18)

**Biogeophysical first-step parity COMPLETE** — all registry fields pass at step
n1757845. 28-step window status:

| group | fields | window-max | tol | status |
|---|---|---|---|---|
| Hydrology | H2OSOI_LIQ, ZWT, ZWT_PERCH, H2OSFC | pass | — | ✅ |
| Radiation | SABV_P, SABG_P | 1.5 / 2.5 | 5.0 | ✅ |
| Snow/ice (summer) | H2OSOI_ICE, SNOW_DEPTH, frac_sno | 0 | — | ✅ |
| Temperature | T_GRND, T_SOISNO | 0.59 | 0.20 | first ✅, window in progress |
| Canopy temp | T_VEG | 3.46 | 1.20 | first ✅, window in progress |

The temperature window residual is the single localized cause — the fabricated
photosynthesis/stomatal path (midday transpiration too weak) — being fixed in
wave 2b (wire real `canopySunShadeFracs` PAR + Vcmax25, remove the RSSUN floor).

**Bugs found so far were in the adapter glue + harness forcing, NOT the ported
physics modules** (hydrology hardcoded ZWT; radiation coszen/forcing-year/band
split/esai injection; soil-temp drag coeffs/roverg unit/nbedrock). The Fortran
two-stream, albedo, Zeng-Decker solver, and soil heat solve were already faithful.

Branch `parity-phase0`; worktrees per wave merged in. CN is the next phase
(`docs/CN_PARITY_PLAN.md`).

## Done criteria
All Phase-0 tolerance-table fields pass at every boundary across the 28-step
window; CN pools per-step < ~1%; drift bounded; checklist markers cleared with
tests, not by deleting wording.
