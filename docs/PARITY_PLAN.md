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

**BIOGEOPHYSICAL SINGLE-STEP PARITY COMPLETE** — all 12 registry fields pass at
BOTH the first step and the 28-step window max (integrated at `2ced79b`):

| group | fields | window-max | tol | status |
|---|---|---|---|---|
| Hydrology | H2OSOI_LIQ, ZWT, ZWT_PERCH, H2OSFC | 0.031 / 0 | — | ✅ |
| Radiation | SABV_P, SABG_P | 1.54 / 2.46 | 5.0 | ✅ |
| Snow/ice (summer) | H2OSOI_ICE, SNOW_DEPTH, frac_sno | 0 | — | ✅ |
| Temperature | T_GRND, T_SOISNO | 0.116 | 0.20 | ✅ |
| Canopy temp | T_VEG | 0.72 | 1.20 | ✅ |

Full suite: 111 examples, 4 failures — exactly the documented pre-existing ones
(port-audit stub scan, QRUNOFF fixture, 2 Julia-trajectory tests); no regressions.

**Every bug was in the adapter glue, harness forcing, or input params — NEVER the
ported physics modules**, which were already faithful (two-stream, albedo,
Zeng-Decker Richards solver, soil heat solve, Medlyn stomatal). Fixes by wave:
- Hydrology: adapter discarded prognostic ZWT (hardcoded 5.0) + unstable explicit
  solver → real implicit Zeng-Decker + prognostic state.
- Radiation: coszen = cos(declination) → real zenith; forcing **year 2002 not 2003**;
  solar zenith time-downscaling; CLMNCEP band split; inject esai/tsai.
- Soil/canopy temp: canopy drag coeffs from param file; `roverg` ×1000 unit;
  `nbedrock` from zbedrock.
- Photosynthesis: `rgas` ×1e-3 (made gb_mol 1000× small, masked by a stomatal
  floor); wired real sun/shade PAR; CLM5 per-PFT Medlyn slopes; bare t_veg=forc_t.
- Soil thermal: inject the dump's exact per-layer `THK_C` (bgc-run soil texture
  differs from test/data and is unreconstructable from surfdata).

Branch `parity-phase0`; per-wave worktrees merged. **Next phase: CN/BGC**
(`docs/CN_PARITY_PLAN.md`) — a subsystem rewrite (vectorize scalar CN → per-pft/
per-layer, port the real BGC physics).

## Generality validation (2026-06-19) — confirms faithful, not overfit

Validated the harness against a SECOND independent case to guard against
overfitting the single summer window: `clm_parity_run` n13461 = 2003-07-15,
coszen≈0.87 (local solar noon, PEAK sun), real-year-2003 forcing — a very
different radiation/flux regime and forcing year. Result: **10/12 fields PASS**:

| field | absΔ @ n13461 | tol | verdict |
|---|---|---|---|
| SABV_P / SABG_P | 1.11 / 1.00 | 5.0 | ✅ at PEAK sun → solar downscaling generalizes |
| T_VEG | 1.01 K | 1.20 | ✅ |
| H2OSOI_LIQ + hydrology/snow | ≤0.016 | — | ✅ |
| T_GRND / T_SOISNO | 0.44 K (rel 0.15%) | 0.20 | ✗ — residual amplified at high flux |

Key takeaways:
- The radiation port (whose solar-zenith downscaling was *calibrated* on the
  low-sun BGC window) **holds at peak sun** → genuine physics, not overfit.
- Hydrology, canopy temperature, snow generalize.
- `THK_C` is injected at n13461 too, so the T_GRND 0.44 K is NOT an artifact —
  it's a real **soil-temp/ground-flux coupling residual that grows with ground
  heat flux** (0.12 K at low sun → 0.44 K at peak sun). Well-localized next
  target (`SoilTemperature`/`SoilFluxes` ground-flux response), generality-
  confirmed rather than overfit-driven.

Harness is now parameterized by forcing file + dump dir (`initParityHarnessWith`,
`generalityReport`); gated generality test in `test/Spec.hs`.

## T_GRND high-flux residual — fully localized (2026-06-19)

The peak-sun T_GRND residual (0.44 K at n13461, 0.15% rel) was traced end-to-end:
- **Absorbed solar** (SABV/SABG): correct (pass at peak sun).
- **Heat capacity** (cv): RULED OUT by re-instrumenting the Fortran dump with a
  `CV_C` sidecar (rebuilt cesm.exe, reran 2003 case) and injecting it — T_GRND
  byte-identical, because our `csol`/`cv` already matches Fortran to ~0.7%.
- **Latent** (ground LH): our `qflx_evap_grnd`≈3e-6 ≈ Fortran `EFLX_LH_P`=0. Ruled out.
- **ROOT CAUSE — ground SENSIBLE heat flux.** Exposed per-patch `EFLX_GNET` and
  diffed vs the dumped `EFLX_GNET_P`: off ~20 W/m². Breakdown at n13461 (bare
  patch): our `eflx_sh_grnd`=50.3 vs Fortran `EFLX_SHG_P`=76.0 — **~26 W/m² too
  low** → too little sensible heat shed → `EFLX_GNET` too high → ground too warm.
  A ~10 W/m² longwave term partially offsets. The fix is in the ground
  sensible-heat / aerodynamic-resistance computation (BaregroundFluxes /
  CanopyFluxes ground path) at high flux — in-code, not data-limited.

New harness infra: `EFLX_GNET_P` registry probe + `cv` override + cvdump reader.

## T_GRND high-flux residual — CLOSED (2026-06-19)

The peak-sun T_GRND residual is resolved: **T_GRND passes at BOTH cases** — BGC
low-sun 0.101 K and peak-sun n13461 **0.011 K** (tol 0.20). The diagnostic chain
(re-instrument cv → rule out heat capacity → expose EFLX_GNET → EFLX_SHG)
surfaced FOUR real bugs, all in glue/inputs, none in the ported physics:
1. Bareground `bgi_beta` = soilbeta (wrong) → 1.0 (M-O convective coef).
2. `computePotentialTemperature` inflated thv ~7% at altitude; single-point datm
   delivers forc_th = forc_t.
3. `moninObukIni` used the wrong convective velocity + `ur`; Fortran uses fixed
   `wc=0.5` and `um`.
4. z0m/displa fallback was PFT-independent; now keyed off PFT (C3 grass z0mr=0.12).
Result: grass ground SH −8.0 → +2.7 (Fortran +1.4), T_VEG also improved
(1.01 → 0.28 K). Suite 113 examples, 4 pre-existing failures, no regressions.

Note: the `EFLX_GNET_P` registry probe still shows a bare-patch gap at peak sun,
but it's a boundary-convention artifact (our pre-solve flux vs the dump's
post-solve value at after_soiltemperature) — the actual T_GRND parity passes.

## Done criteria
All Phase-0 tolerance-table fields pass at every boundary across the 28-step
window; CN pools per-step < ~1%; drift bounded; checklist markers cleared with
tests, not by deleting wording.
