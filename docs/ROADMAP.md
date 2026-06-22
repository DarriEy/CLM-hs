# CLM-hs Roadmap to a Defensible Port

Derived from the 2026-06 Fortran-vs-Haskell audit (see `PARITY_STATUS.md`).
Ordered by value-per-effort and by dependency: fix what runs before adding what
doesn't, and earn the architectural lifts last. Each item notes **effort**,
**risk**, and **how it can be validated** — several can only be checked for
*stability/conservation*, not *correctness*, because no free-running or non-soil
reference exists here.

## Guiding principle

The single biggest constraint is that the driver is **single-column /
single-patch / single-landunit**. Phases 1–3 make that one column genuinely
complete and self-consistent (high value, bounded risk). Phase 4 removes the
single-column ceiling (large). Phase 5 is research-scale scope (FATES, transient
land use) that may be out of scope for this port's purpose.

---

## Phase 1 — Make the single soil column honest (fix live-path stubs + wire dead code) — DONE (2026-06)

Status: items 1–5 implemented and merged (commits up to 095226b); suite 118
examples, 0 failures. Snow percolation, growth respiration, gap mortality,
phenology onset/offset, and CN precision control now run on the single soil
column. Caveat: validated for build/stability/conservation-guardrail, NOT
bitwise correctness (no per-process Fortran reference). Items below kept for
the record with their original notes.

These are the highest-value, lowest-risk fixes: the code mostly exists; it's
either a no-op step or never called.

1. **Snow meltwater percolation** — replace `snowPercolationStep = st` with the real
   SnowHydrology percolation/refreeze. *Effort: M. Risk: M (affects snowmelt runoff).
   Validate: water mass balance closure + 30-day stability; ideally a SnowHydrology
   single-step fixture from Fortran.*
2. **Growth respiration** — call `GrowthResp` in the CN step (MR already runs; GR is
   computed in allocation but never subtracted into the live C budget). *Effort: S.
   Risk: L. Validate: NPP = GPP − MR − GR; C balance.*
3. **Gap mortality** — wire `GapMortality` (background mortality → litter/CWD).
   *Effort: S–M. Risk: M (changes pools). Validate: C/N conservation; stability.*
4. **CN balance + precision control** — make `cnBalanceCheckStep` / precision control
   actually enforce conservation (currently no-ops). *Effort: S. Risk: L. Validate:
   they should catch the errors introduced by 1–3 if those are wrong → use as a gate.*
5. **Phenology onset/offset** — wire seasonal & stress-deciduous onset/offset and
   leaf display↔storage transfers (only background litterfall runs today → evergreen
   behavior). *Effort: M. Risk: M. Validate: LAI seasonality vs Julia; C conservation.*

**Exit criterion for Phase 1:** the single column closes C, N, water, and energy
balances every step (enforced by the now-real balance checks), with snowmelt and a
seasonal canopy.

## Phase 2 — Close the budgets (inputs the column is missing)

6. **N inputs** — DONE (commit 8f0417e). N deposition (constant rate), free-living
   fixation (baseline), and NPP-driven symbiotic fixation wired via `NDynamicsMod` into
   the runtime sminn balance; budget no longer sinks-only. Symbiotic fixation uses an
   annualized-NPP proxy (true annual accumulator + ndep stream I/O deferred to Phase 4).
   Suite 118/0/2.
7. **soilbiogeochem completeness** — DONE (commit d207638). Ported
   `SoilBiogeochemNitrogenUptake` (the N-uptake vertical profile) and wired per-layer
   plant N uptake into `runVectorizedNCycle`: the column plant uptake is distributed by
   the available-N profile and debited from `sminn_vr`, conserving the column total
   (previously sminn_vr only gained mineralization). DECISION on `CNSoilMatrixMod`
   (implicit matrix C/N solver): keep the explicit cascade — it is stable over the
   30-day single-column run; the matrix solve is a long-spinup accuracy optimization not
   needed at this scope. Deferred. Suite 118/0/2.

## Phase 3 — Vegetation realism (still single column) — PARTIALLY DONE (2026-06)

Status: fire + methane + carbon isotopes WIRED via 3 parallel worktree agents +
hand-integration (commits 0a2b625, ca217d4, 8e3d0d2 + merges); suite 118/0/2.
Crop DEFERRED (not runnable on this column). All runtime-gated on clmCNActive;
matched-state harness untouched. Validated for build/stability/conservation, not
bitwise correctness.

8. **Crop path** — DEFERRED. The run has no crop landunit (lun_itype [1,5]) or crop PFT
   (pch_itype [0,1,12,0]; crops are index 15+), so crop allocation/phenology would be dead
   code. Moved to Phase 4 (multi-landunit / crop PFT) — wiring it now would only add
   unexercised code, the opposite of this effort's goal.
9. **Carbon isotopes (C13/C14)** — DONE (8e3d0d2). Wired CNCIsoFluxMod/CNC14DecayMod into
   cnBalanceCheckStep: GPP discrimination, respiration at source ratio, C14 decay. Honest
   limitation: ratio-diagnostic (conservative with bulk pools) rather than prognostic isotope
   pools, since CLMState has no isotope field.
10. **Fire** — DONE (0a2b625). Wired CNFireLi2014/CNFireBase into cnPreDrainageStep: a
    background burned-area fraction combusts veg+litter to atmosphere (folded into NEE) and
    litter/CWD, conserving C and N.
11. **Methane** — DONE (ca217d4). Wired ch4Mod into cnPostDrainageStep: CH4 production
    (anaerobic fraction of HR), oxidation, ebullition/aerenchyma transport; net surface flux
    on the lnd2atm diagnostic, carbon-conserving (re-routes already-respired HR carbon).
    **Dust, VOC** — still dead code; wire if those fluxes are needed. *Effort: M each.*

## Phase 4 — Remove the single-column ceiling (architectural)

These are large and unlock the landunit physics that already has code (urban/lake)
or needs writing (glacier).

12. **Multi-landunit / multi-column driver loop** — make `clmDrv` iterate landunits →
    columns → patches with proper weights and filters (the `decompMod`/`filterMod`/
    `subgridAve` machinery). *Effort: XL. Risk: H (touches all state). Validate:
    single-soil-column results must be unchanged (regression) before adding landunits.*
13. **Wire lake to actually run** — DONE (2026-06, lake). `lakeTemperatureStep`
    was a no-op (computed thermal props then returned state unchanged); it now
    chains the full CLM LakeTemperatureMod sequence — thermal props → lake
    density → eddy diffusivity → solar heat source → implicit tridiagonal solve
    (snow+lake+soil) → convective mixing → phase change — updating lake
    temperatures, lake ice fraction, and snow/soil temps + water.
    `lakeFluxesStep` now passes the surface coupling (ws/ks) through lake state;
    a `lake_t_lake_col` field was added (LakeStateData had no lake temperature),
    and `readFortranRestart` now loads T_LAKE/LAKE_ICEFRAC so a lake column can
    warm-start from a Fortran restart. **Fixed three latent indexing bugs** in
    the never-exercised lake module (uniform off-by-one: combined snow+soil
    arrays put Fortran layer L at index L+nlevsno-1, but `soilThermPropLake`,
    `lakeTridiagSolve`, and `phaseChangeLake` used `+nlevsno`, overrunning the
    bottom soil layer). **Validated**: warm-started from the Bow lake restart
    (column 1, ityplun=5) and run a day under cold forcing, the lake evolves a
    physically-sane ice-covered profile, bounded, ice fraction in [0,1] (test
    "lake column temperature physics runs and evolves a sane profile"). Tight
    Fortran parity isn't possible (single time-averaged h0 record). URBAN still a
    no-op stub (`urbanFluxesStep` returns input and checks the wrong landunit
    type `it /= 6`) — separate item, not addressed here.
14. **Glacier** — write `GlacierSurfaceMassBalanceMod` (not ported) + istice column
    handling. *Effort: L. Risk: M.*
15. **Restart I/O** — DONE (2026-06). Real full-prognostic-state save/restore
    (`writeRestartState`/`readRestartState` in `PipelineRunner`): native
    per-variable little-endian Float64 binary, overlaid onto a pristine cold-start
    base on read (static geometry/params re-derive). **Validated bit-identical**:
    an in-line round-trip hook (`pcRestartRoundtripDay`) writes at day 3, reads
    back onto a fresh base, and resumes to a 6-day trajectory equal to a continuous
    run, bit-for-bit (`test/Spec.hs` "restart round-trip is bit-identical"). Finding
    the complete read-before-write set was the real work — beyond the obvious
    prognostic fields (T/water/snow/geometry/CN), bit-identity required the canopy
    sun/shade state (recomputed only in daylight), the canopy-air reservoir +
    Monin-Obukhov seeds, soil SMP_L/HK_L, integrated snowfall, and the surface
    energy/water-flux carriers.
    **Fortran restart read also DONE**: `readFortranRestart` opens a real
    `clm2.r.*.nc` via the existing NetCDF reader and maps one column's
    biophysical state (T_SOISNO/H2OSOI_LIQ/ICE on the combined snow+soil grid,
    T_GRND/T_VEG/H2OSFC, DZSNO/ZSNO/ZISNO snow geometry, SNLSNO, ZWT/ZWT_PERCH,
    frac_sno, INT_SNOW) onto a base — the port's `nlevgrnd=25/nlevsno=12/levtot=37`
    match the Fortran file exactly, so column slices copy directly. Validated:
    a real Bow `clm2.r` restart parses into physically-sane state (test
    "reads a Fortran NetCDF restart into physically-sane state"). REMAINING:
    full warm-start-and-run *parity* (run from Fortran ICs and compare the
    trajectory to Fortran history) — needs a matched forcing/surfdata config +
    reference, the same data gap that blocks #12–14; and CN-pool / NetCDF-write
    (history #16) are still separate.
16. **NetCDF history** — DONE (2026-06, single-column scope). Added a NetCDF
    *writer* to `CLM.Infrastructure.NetCDF` (`ncWriteTimeseries`: nc_create /
    def_dim / def_var / put_att_text / enddef / put_var_double FFI) and
    `writeDailyNetCDF` in `PipelineRunner` — emits a history tape with a `time`
    dimension and one CF-attributed variable per daily field (T_GRND, FSA,
    EFLX_LH/SH, H2OSNO, SNOW_DEPTH, FRAC_SNO, GPP/NPP/NEE/HR/LEAFC/SOILORGC).
    **Validated by round-trip**: write the tape, read it back with the existing
    NetCDF reader, series match to < 1e-12 (test "NetCDF history round-trips").
    Output is now machine-comparable to CTSM. NOT done (out of single-column
    scope): multi-tape, subgrid aggregation, unlimited/append time, time-coordinate
    metadata, comparison against a real Fortran h0 tape (data-gap blocked).
17. **External data streams** — N-deposition, LAI, crop calendar, urban-tv readers +
    time interpolation. *Effort: M. Risk: L.*
18. **atm→lnd downscaling + lnd→atm coupling** — topographic forcing downscaling and
    coupling-flux aggregation (needed for any multi-column or coupled use). *Effort: M.*

## Phase 5 — Research-scale scope (likely out of scope)

19. **dyn_subgrid** — transient land use, dynamic landunit/PFT areas, harvest,
    conservation-on-area-change (19 modules, ~6,700 lines). Depends on Phase 4.
    *Effort: XL.*
20. **FATES** — cohort/patch demography, PARTEH, ED radiation, hydraulics, fire
    (~75,800 Fortran lines). Effectively a separate port. *Effort: XXL.*
21. **MPI/OpenMP** — only if multi-gridcell/global runs are a goal. *Effort: XL.*

---

## The validation gap (applies throughout)

The only references available here are: a few Fortran single-step *matched-state*
fixtures (summer soil column) and the Julia daily trajectory (also soil column,
~ill-posed for tight parity). There is **no** winter reference, no non-soil-landunit
reference, no free-running CN/pool reference, and no restart reference. So for most of
Phases 1–3 and all of Phase 5, "validated" can mean **conservation + stability +
plausibility**, not bitwise correctness. Closing that gap (regenerating instrumented
Fortran reference runs across regimes and landunits) is itself a prerequisite for
calling any of this "faithful," and is currently blocked (the pdump instrumentation is
no longer in the installed Fortran source).

## Recommended next concrete step

Phase 1, items 1–4: snow percolation + growth respiration + gap mortality, then make
the balance/precision checks real so they *gate* those changes. That makes the one
column we actually run close its budgets — the smallest unit of genuine, checkable
progress, and the honest foundation everything else needs.
