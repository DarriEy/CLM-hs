# CLM-hs Parity & Completeness Status

## SCOPE — read this first

CLM-hs is a **single-column, single-landunit (natural-vegetation soil), cold-start,
reduced-physics** research port. It integrates one soil column under prescribed forcing
and emits CSV. **It is NOT a drop-in CTSM replacement and cannot reproduce CTSM runs.**

A Fortran-vs-Haskell audit (2026-06) over `installs/clm/src` — biogeophys 87, biogeochem
70, soilbiogeochem 20, dyn_subgrid 19, main 48, utils 17, FATES ~62 modules — found the
port covers the core biophysical kernels of one soil column but is missing whole
subsystems, contains stubs in the live timestep path, and carries large bodies of
never-called (dead) code. Any earlier "complete / faithful / done" wording in this repo
applied only to a **narrow slice** (a few biophysical fields, one summer column, ~28
timesteps, plus ~2 K trajectory tracking vs the Julia port) and overstated the whole.

## What genuinely runs (single soil column, cold start)

Core biophysics kernels, ported and exercised on the one soil column:
Photosynthesis (Farquhar/Collatz, Ball-Berry/Medlyn), CanopyFluxes, BaregroundFluxes,
SoilTemperature (implicit, phase change), SoilWaterMovement (Richards), SoilHydrology
(infiltration/runoff/water table), SnowHydrology (accumulation/compaction/layer
combine-divide), SurfaceAlbedo + SurfaceRadiation (two-stream) + SNICAR (with grain
aging), SnowCoverFraction, QSat, DayLength, Aerosol, Irrigation-demand. Plus a partial
CN path (allocation + maintenance respiration + N-competition + a free-running CENTURY
decomposition cascade).

## What is validated (and only this)

Hard regression guards (gated on fixtures), all within the narrow slice above:
1. Soil-temp solver bit-exact vs Fortran single-step fixture (< 0.01 K).
2. Pipeline T_GRND within 2.5 K of the Julia 10-day daily trajectory; snow mass within 15%.
3. Matched-state Fortran biophysics at one peak-sun step (n13461): biophysical fields
   within tolerance (T_GRND 0.044 K); `EFLX_GNET_P` excluded (diagnostic LW convention).
4. CN free-running pool drift < 1% over 28 steps (xsmrpool + flux probes excluded);
   CENTURY cascade soil-C bounded over 30 days (stability, NOT correctness).
5. Restart round-trip (Phase 4 #15): writing the full prognostic state at day 3,
   reading it back onto a pristine cold-start base, and resuming yields a 6-day
   daily trajectory **bit-identical** to a continuous run — proving the restart is
   both lossless and complete for the prognostic + read-before-write state.

These do not validate: any non-soil landunit, multi-day winter, restart, multi-column,
fire/mortality/crop/dynamic-veg, or anything in the "missing" lists below.

## What is NOT implemented

### Whole subsystems absent or stubbed
| Subsystem | State | Note |
|-----------|-------|------|
| dyn_subgrid (transient land use, dynamic landunits, harvest, conservation) | **0%** | 19 modules, ~6,700 lines — structurally locks the port to single column |
| FATES (cohort/patch demography, PARTEH, ED radiation, hydraulics, fire) | **~0%** | 570-line type+no-op stub vs ~75,800 Fortran lines; `fatesDynamics` returns input |
| Restart I/O (read/write full model state) | **real (Phase 4 #15)** | Write→read→resume is **bit-identical** to a continuous run (validated). Native per-variable binary format. Also reads a real Fortran `clm2.r.*.nc` restart into sane state (`readFortranRestart`, validated) — warm-start-and-run *parity* still needs a matched config+reference; NetCDF *write* (history) is #16 |
| History output (NetCDF) | **real, single-tape (Phase 4 #16)** | `writeDailyNetCDF` emits a NetCDF history tape (time-dimensioned, CF-attributed vars), round-trip validated. Multi-tape / subgrid aggregation / Fortran-h0 comparison still out of scope |
| Multi-landunit subgrid (urban / glacier / lake / crop / wetland) | code exists, **never run** | The run is 100% soil (`wt_lunit = (1,0,0,…)`) |
| atm→lnd downscaling + lnd→atm coupling | **stub** | No topographic forcing downscaling; no coupling fluxes |
| External data streams (N-deposition, LAI, crop calendar, urban-tv) | **missing** | N budget has no atmospheric/biological inputs |
| MPI / OpenMP parallelism | **missing** | Single process, single column |

### Stubs in the LIVE timestep path
FIXED in Phase 1 (now real, runtime-gated on clmCNActive):
- `snowPercolationStep` — real meltwater percolation/refreeze + drainage to top soil.
- `cnBalanceCheckStep` — real CN precision control (round-off truncation +
  non-negativity guardrail) instead of a no-op.

FIXED in Phase 4 (#13, lake):
- `lakeTemperatureStep` — now chains the full lake-temperature solve (thermal
  props → density → eddy diffusivity → solar source → implicit tridiagonal solve
  → convective mixing → phase change) and applies it. Fixed three latent
  off-by-one indexing bugs in the never-exercised lake module. Validated to
  evolve a sane ice-covered profile warm-started from a Fortran lake restart.

Still a stub (not exercised by the single soil column):
- `urbanFluxesStep` — returns `st`; also checks the wrong landunit type (`it /= 6`).

### CN/BGC: modules exist but are NEVER CALLED (dead code; 0 driver references)
WIRED in Phase 1 (now run free-running): growth respiration (via `GResp.cnGrowthResp`),
gap (background) mortality (→ litter/CWD, conserving C/N), and phenology onset/offset
(seasonal & stress deciduous: classify, GDD/daylength/soil-water gating, storage→
transfer→display→litter transfers).
Also WIRED (Phase 2): N inputs — atmospheric deposition + free-living + NPP-driven
symbiotic fixation (via NDynamicsMod), so the N budget is no longer sinks-only (plant
uptake sink was already present via NutrientCompetition). Symbiotic fixation uses an
annualized-current-NPP proxy pending a true annual NPP accumulator.
WIRED in Phase 3 (now run free-running on the soil column): fire (CNFireLi2014/CNFireBase),
methane (ch4Mod — net surface flux on the lnd2atm diagnostic), carbon isotopes C13/C14
(CNCIsoFluxMod/CNC14DecayMod — ratio-diagnostic conservative with bulk pools, since CLMState
has no prognostic isotope field).
Still dead code (0 driver references): dynamic vegetation (CNDV), wood products / harvest,
annual updates, dust & VOC emissions. DEFERRED — crop allocation/phenology: the run has no
crop landunit (lun_itype [1,5]) or crop PFT (pch_itype [0,1,12,0]; crops are index 15+), so
wiring it would be dead code; deferred to Phase 4 (multi-landunit / crop PFT). (Per-layer
plant N uptake — SoilBiogeochemNitrogenUptakeMod — WIRED in Phase 2; the implicit matrix C/N
solver CNSoilMatrixMod is deferred by decision: explicit cascade is stable at this scope.)

### soilbiogeochem missing
`CNSoilMatrixMod` (implicit matrix C/N solver), `SoilBiogeochemNitrogenUptake` (plant N
uptake), `TillageMod`.

### biogeophys missing / partial
`GlacierSurfaceMassBalanceMod` (962 lines, missing), standalone `FrictionVelocityMod`
(u* is embedded in the flux modules, not independently validated), ozone stress on
photosynthesis, `HumanIndexMod`, detailed urban building-temperature (Oleson 2015).
Several "ported" modules are ~⅓ the Fortran line count (Photosynthesis 1,006 vs 5,209;
SoilTemperature 998 vs 2,974) — core algorithm without full option/edge-case coverage.

## Honest scorecard

| Area | Real status |
|------|-------------|
| Biogeophys core kernels (1 soil column) | mostly real (~40 modules) |
| CN/BGC | ~⅓ wired (alloc/MR/Ncomp/decomp); fire/mortality/CNDV/isotopes/products/crop/phenology-onset NOT wired |
| soilbiogeochem | ~60–70%; missing matrix solver + N uptake |
| dyn_subgrid | 0% |
| Restart | **real, bit-identical round-trip** (native binary) + **reads Fortran `clm2.r.*.nc`** into sane state; warm-start-run parity pending |
| History (NetCDF) | **real, single-tape, round-trip validated** (multi-tape/subgrid/Fortran-h0 compare pending) |
| Coupling / Streams | stub (~5–20%) |
| Multi-landunit (urban/glacier/lake/crop) | code present, not exercised |
| FATES | ~0% |

See `ROADMAP.md` for the prioritized path to closing these gaps, and
`PORT_COMPLETION_CHECKLIST.md` for the per-item debt list.
