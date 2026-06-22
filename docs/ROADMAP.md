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

## Phase 3 — Vegetation realism (still single column)

8. **Crop path** — crop allocation fractions + crop phenology (crops currently run as
   grass). *Effort: M–L. Risk: M. Validate: no reference here → stability + plausibility.*
9. **Carbon isotopes (C13/C14)** — wire `CIsoFlux`/`CarbonIsotopes` if isotope output is
   wanted. *Effort: M. Risk: L. Validate: isotope mass tracks bulk C.*
10. **Fire** — wire `CNFireLi2014` (the modules exist, never called). *Effort: M. Risk:
    M. Validate: fire C/N emissions conserve; no reference → plausibility/stability.*
11. **Methane, dust, VOC** — wire if those fluxes are needed (currently dead code).
    *Effort: M each. Risk: L. Validate: plausibility.*

## Phase 4 — Remove the single-column ceiling (architectural)

These are large and unlock the landunit physics that already has code (urban/lake)
or needs writing (glacier).

12. **Multi-landunit / multi-column driver loop** — make `clmDrv` iterate landunits →
    columns → patches with proper weights and filters (the `decompMod`/`filterMod`/
    `subgridAve` machinery). *Effort: XL. Risk: H (touches all state). Validate:
    single-soil-column results must be unchanged (regression) before adding landunits.*
13. **Wire urban + lake to actually run** — replace `urbanFluxesStep`/`lakeTemperatureStep`
    no-ops with the real (already-ported) physics, exercised on urban/lake landunits.
    *Effort: M (depends on 12). Risk: M. Validate: lake temperature evolves; urban
    canyon energy balance closes.*
14. **Glacier** — write `GlacierSurfaceMassBalanceMod` (not ported) + istice column
    handling. *Effort: L. Risk: M.*
15. **Restart I/O** — real model-state save/restore (start with the binary path
    carrying full state; NetCDF + subgrid pointers later). Enables resume and
    bootstrapping from Fortran restarts. *Effort: L. Risk: M. Validate: write→read→run
    is bit-identical to no-restart; read a Fortran restart and reproduce step 1.*
16. **NetCDF history** — multi-tape, subgrid-aggregated output. *Effort: L. Risk: L.
    Validate: against Fortran history for the matched cases.*
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
