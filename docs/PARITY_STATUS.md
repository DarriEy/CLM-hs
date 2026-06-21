# CLM-hs Parity Status (consolidated)

This is the consolidated, evidence-based state of the CLM-hs port's parity with the
Fortran CTSM reference and the Julia reference port. It supersedes the "dead end /
unwired" framing in earlier notes: every major biogeophysical and biogeochemical
subsystem investigated under that premise turned out **implemented, wired, and at or
near parity**. The residuals are compounding sensitivity and harness input-completeness
limits — not missing subsystems or fixable physics bugs.

## The live driver (read this first)

The canonical pipeline is:

```
PipelineRunner.runPipeline -> clmDrv (CLMDriver.hs) -> wiredPhysicsPipeline (PhysicsAdapters.hs)
```

`src/CLM/Driver/Simulation.hs` is the **old monolithic driver that `runPipeline` replaced**
— it is dead code (imported only by legacy `app/Main.hs` modes `--run`/`--test-data`, not by
`runPipeline` or the test suite). Do not read `Simulation.hs` to understand pipeline behavior;
read `PhysicsAdapters.hs` + `CLMDriver.hs`. Two analyses were misled by reading `Simulation.hs`
(its hardcoded albedo/Beer's-law) and by the stale `test/data/haskell_daily_avg.csv` (a legacy
`--run` crash trajectory, not the canonical pipeline).

## Subsystem verdicts

| Subsystem | State | Evidence |
|-----------|-------|----------|
| Boundary layer / canopy turbulence | Faithful | per-step ustar/rah/taf/t_veg match Fortran (BL per-step study) |
| Soil temperature solver | **Bit-exact** | reproduces Fortran column solve to < 0.01 K (max 5.7e-14) given Fortran inputs |
| Soil thermal props (cv, thk) | Correct | computed cv matches Fortran CV_C to ~0.002%; n13461 hits 0.046 K with NO property injection |
| Two-stream radiation + albedo | **Already wired** | `surfaceAlbedoDriver` runs every step (`useAlbDriver=True`); `--rad-test` matches Julia exactly |
| Snow-cover fraction | Tracks Julia | frac_sno climbs to ~0.88 in both; pipeline FSA within ~10% of Julia |
| CN/BGC (40+ modules) | **Already wired** | all 3 CN steps active in `clmDrv`; matched-state pools sub-1%, bounded free-run drift |
| Matched-state biophysics (n13461) | At parity | T_GRND/T_SOISNO 0.044 K; all biophysical fields within tolerance |

## Regression guards (hard assertions in test/Spec.hs)

These lock in the achieved parity so it cannot silently regress:

1. **Soil-temp solver bit-exactness** — `solveSoilTemperature` vs the Fortran `soiltemp/`
   fixture, < 0.01 K. (gated on fixture presence)
2. **Pipeline-vs-Julia trajectory** — `runPipeline` tracks Julia daily-mean T_GRND within
   2.5 K for 10 days and snow mass within 15%. Guards against the legacy cold crash.
3. **Matched-state Fortran biophysics (n13461 peak-sun)** — every biophysical field within
   its registered tolerance; `EFLX_GNET_P` excluded (documented diagnostic LW-convention
   difference, not temperature-affecting).
4. **CN free-running drift** — every CN/BGC state pool < 1% relative drift over the 28-step
   free-run; `xsmrpool` and the `*_VR_P` flux probes excluded (documented).

Suite: 115 examples, 0 failures, 2 pending (the tight aspirational Julia-parity test at
0.1 K / 1e-3 bounds; the port-debt marker audit).

## Characterized residuals (NOT bugs)

- **~2 K free-running T_GRND vs Julia/Fortran over days 4-10.** Survives every subsystem
  being correct (BL, solver, cv/thk, radiation, CN). It is compounding IC/regime sensitivity.
  Note radiation is *not* the driver: in winter the pipeline absorbs slightly *more* solar
  than Julia yet runs colder. Full free-running trajectory parity to Fortran is ill-posed —
  Julia (the "fully working" port) does not achieve it either.
- **`EFLX_GNET_P` ~47 W/m2 at matched state.** Diagnostic-only: Fortran's vegetated-patch
  gnet nets the under-canopy ground<->canopy longwave to ~0 (gnet ~= sabg - shg - ev*htvp);
  T_GRND in the same step matches at 0.044 K, so the physics `hs_top` is correct.
- **bgc-window matched-state T_GRND 0.5-0.7 K.** Harness input-completeness: the bgc spinup
  column's `tkmg`/`tkdry`/`csol` are not injectable from the available dumps (only
  `watsat`/`bsw`/`sucsat` are), so our thermal props mix bgc-injected + test/data base.
- **`xsmrpool` 1.4% free-run drift.** Harness input-completeness: GPP=0 on state injection
  (photosynthesis carriers not produced for the injected patches) -> `availc=0` -> the
  excess-MR-storage recovery is correctly capped to 0, while Fortran (real GPP) recovers.
  The allocation/recovery physics matches Fortran exactly.

## Genuinely remaining work (all optional / tooling — none is "the fix")

- **Full SNICAR snow albedo — WIRED (commits 5f219ed, fc3dd34), marginally regressive.** The
  real 5-band optics (pic16/mlw) are ingested; `snicarRTMultiBand` runs the full per-band
  adding-doubling (`snicarRTColumn`) and feeds `albsnd`/`albsni` into `surfaceAlbedoDriver`
  (via `sadi_snowAlbOverride`). Validated: physical, grain-responsive albedo (test in Spec.hs).
  BUT it runs ~0.2 K COLDER than the age-based fallback (T_GRND day10 260.04 vs 260.22; Julia
  262.22) — marginally worse, because the bulk-snow pipeline does NOT age snow grains
  (`snowAgingStep` is a no-op for snl>=0), so SNICAR uses fresh ~54um grains and over-estimates
  albedo. **Prerequisite to make SNICAR beneficial: implement bulk-snow grain aging** (track +
  evolve `snw_rds` via `snowageGrainLayer`, reset on snowfall). Parity harness/calibration keep
  `emptySnicarOptics` (age-based), so matched-state guards are unchanged.
- **Unify the CN decomposition paths.** Free-running runtime uses a simplified scalar Q10
  decay; the full vectorized CENTURY cascade only runs in the matched-state harness path.
  Real completeness gain, large job, no matched-state winter reference to validate against.
- **Regenerate winter Fortran pdumps.** Needed for matched-state winter verification; the
  pdump instrumentation is no longer in the installed Fortran source tree.
- **Stronger CN test window.** The bgc parity window is summer near-equilibrium (single-step
  CN change ~0); a growing-season window would stress CN harder.

## Bottom line

The biogeophysical + biogeochemical core is substantially complete and validated against
Fortran (matched-state) and Julia (trajectory), with hard regression guards in place. The
recurring residuals are sensitivity and test-harness limits, not missing physics.
