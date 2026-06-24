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
| Multi-landunit subgrid (soil + lake gridcell) | **Option A done (Phase 4 #12)** | `runMixedGridcell` loops the column kernel (soil pipeline + lake path) and aggregates by area weight; weights read directly from NetCDF surfdata. Validated by self-consistency + column independence (no mixed Fortran ref exists). Option B (CLMState array-vectorization, filters/down-pointers) unported; urban/crop/wetland columns still not run |
| atm→lnd downscaling + lnd→atm coupling | **partial (Phase 4 #18)** | lnd→atm flux aggregation (`aggregateLnd2Atm`) into `l2a_*_grc`, conservation-validated; atm→lnd downscaling is identity at single-column scope. No coupling Fortran ref |
| External data streams (N-deposition, LAI, crop calendar, urban-tv) | **partial (Phase 4 #17)** | Time-interpolation core (`DataStream`) unit-validated + wired into N-deposition (constant default); LAI/crop/urban-tv readers pending |
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

WIRED in Phase 4 (#13 urban):
- `urbanFluxesStep` — now gated on urban landunit types 7/8/9 (was the wrong
  `it /= 6`, which is wetland) and runs the ported UrbanFluxes (canyon energy
  balance, per-facet sensible heat, HVAC waste heat) + UrbanRadiation
  (multiple-reflection canyon longwave). Validated for sanity/conservation only —
  no urban surfdata or urban Fortran reference exists, and the single-column port
  uses one t_grnd proxy for the five urban facet columns CLM splits out.

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
WIRED in Phase 4 (2026-06): dynamic vegetation (CNDV) — ppCNDV step runs every timestep
(faithful agdd/agddtw/t_a10/t_mo/prec365/annsum_npp accumulators + 20-yr means) and the
annual Light→Establishment→Mortality driver on the year boundary; per-PFT bioclimatic limits
from an LPJ/CLM-DGVM table; activated by use_cndv (seeds clmDGVS). See ROADMAP item 8 for the
documented limitations (no Fortran reference; LPJ pftpar stand-in; pre-established cold start).
ALSO WIRED in Phase 4 (2026-06): dust emission (ppDustEmission — Zender 2003, flux→l2a_flxdst,
real wind/moisture/bare-fraction dependence; mbl_bsn_fct=1.0 is CLM's default when the optional
soil-erodibility stream is off, DustEmisZender2003.F90:175), biogenic VOC (ppVOCEmission — MEGAN
isoprene, flux→l2a_flxvoc; representative EF, 24/240-hr acclimation means now carried as running
means on CLMState — no longer stubbed), wood/crop products (ppCNProducts — first-order decay of
the 1/10/100-yr pools, with gains from a prescribed annual wood harvest clmHarvestFrac routing
stem C to products + slash-to-litter; default 0 so static runs only decay), and CN annual update
(ppCNAnnualUpdate — annual mean 2m temperature via the ported annualCounter/annualPatch
functions; other annual sums have no ported consumer yet). Still dead code (0 driver references):
none of the previously listed CN modules remain unwired. DEFERRED — crop allocation/phenology: the run has no
crop landunit (lun_itype [1,5]) or crop PFT (pch_itype [0,1,12,0]; crops are index 15+), so
wiring it would be dead code; deferred to Phase 4 (multi-landunit / crop PFT). (Per-layer
plant N uptake — SoilBiogeochemNitrogenUptakeMod — WIRED in Phase 2; the implicit matrix C/N
solver CNSoilMatrixMod is deferred by decision: explicit cascade is stable at this scope.)

### soilbiogeochem missing
`CNSoilMatrixMod` (implicit matrix C/N solver), `SoilBiogeochemNitrogenUptake` (plant N
uptake), `TillageMod`.

### biogeophys missing / partial
`GlacierSurfaceMassBalanceMod` (Phase 4 #14: **ported** to
`CLM.BioGeoPhys.GlacierSurfaceMassBalance`, 7 deterministic unit tests; driver
integration pending a glacier column + stored snow-capping flux), standalone `FrictionVelocityMod`
(u* is embedded in the flux modules, not independently validated), ozone stress on
photosynthesis, `HumanIndexMod`, detailed urban building-temperature (Oleson 2015).
Several "ported" modules are ~⅓ the Fortran line count (Photosynthesis 1,006 vs 5,209;
SoilTemperature 998 vs 2,974) — core algorithm without full option/edge-case coverage.

## Honest scorecard

| Area | Real status |
|------|-------------|
| Biogeophys core kernels (1 soil column) | mostly real (~40 modules) |
| CN/BGC | ~⅔ wired (alloc/MR/Ncomp/decomp/fire/mortality/isotopes/phenology-onset/CNDV/products+harvest/annual-update/dust/VOC; real per-PFT MEGAN+pprod params); crop NOT wired; matrix C/N solver deferred |
| soilbiogeochem | ~60–70%; missing matrix solver + N uptake |
| dyn_subgrid | 0% |
| Restart | **real, bit-identical round-trip** (native binary) + **reads Fortran `clm2.r.*.nc`** into sane state; warm-start-run parity pending |
| History (NetCDF) | **real, single-tape, round-trip validated** (multi-tape/subgrid/Fortran-h0 compare pending) |
| Coupling / Streams | stub (~5–20%) |
| Multi-landunit (urban/glacier/lake/crop) | code present, not exercised |
| FATES | ~0% |

See `ROADMAP.md` for the prioritized path to closing these gaps, and
`PORT_COMPLETION_CHECKLIST.md` for the per-item debt list.
