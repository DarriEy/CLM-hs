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
   **Status check (2026-06):** crop is also *unported* — only `CLM.Types.CropData`
   (a types stub) exists; there is no crop allocation/phenology module. Wiring crop
   would mean writing the physics from scratch AND synthesizing a crop-PFT surfdata.
   Not done.

   **CN dead-code audit (2026-06), re crop/CNDV/products on the static single column:**
   - **CNProducts** (wood/crop product pools) — its gain fluxes come from dynamic
     land-cover change (dyn_subgrid #19, unported) + wood/crop harvest, NONE of which
     occur in a static run, so a live step would carry only zero-flux dead pools.
     Instead the module MATH is now locked in by deterministic conservation unit
     tests (gain*dt growth, first-order decay loss = pool*k, Δpool = (gains−k·pool)·dt,
     distinct 1/10/100-yr lifespans) — test block "CNProducts (wood/crop product pools)".
   - **CNDV** (dynamic vegetation) — ported and *would* run on the natural-veg column
     (annual PFT competition/establishment/mortality), but no dynamic-vegetation Fortran
     reference exists, so it could only be sanity-validated. Not wired (offer pending).
   - **Crop** — unported (see above).
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

12. **Multi-landunit / multi-column driver loop** — DONE via Option A (2026-06,
    soil+lake gridcell). `runMixedGridcell` (PipelineRunner) loops the single-column
    kernel over the landunit columns — the soil column runs the full wired physics
    pipeline, the lake column runs the lake surface-flux + temperature path, both
    on the same forcing — and aggregates gridcell diagnostics by area weight.
    Columns are independent within a timestep (they interact only through the
    gridcell aggregate to the atmosphere), so the column-loop is exact for one
    gridcell without a CLMState multi-column refactor. Landunit weights are read
    **directly from NetCDF surfdata** (`readSurfdataLandunits`, no .bin export) —
    using CLM.jl's synthesized `surfdata_mixed.nc` (PCT_NATVEG=50/PCT_LAKE=50,
    LAKEDEPTH=10). **Validated** (test "aggregates soil + lake columns by area
    weight, columns independent"): the gridcell diagnostic equals the area-weighted
    column average and lies between the columns, and each column's trajectory is
    bit-identical (±1e-12) regardless of the weights — proving the loop doesn't let
    columns corrupt one another. No mixed Fortran reference run exists (the
    clm_lake_run is 100% lake), so this is self-consistency + per-column
    equivalence, not a mixed-gridcell Fortran parity.
    **Option B (full CLMState array-vectorization) feasibility (2026-06):** this is a
    ground-up rewrite — every `_col`/`_patch` scalar across ~25 state records becomes
    a column/patch-indexed array, and every physics adapter (thousands of lines) is
    reindexed + filter-driven. It cannot be done as a single incremental change
    without leaving the suite broken for an extended period. The *bounded, tractable
    core* is to instantiate + exercise the already-ported-but-0%-used subgrid
    machinery: `InitSubgrid` (addLandunit/Column/Patch + `clmPtrsCompdown` down-pointers
    + `clmPtrsCheck`), `Filters` (soil/lake/urban/snow masks), and `SubgridAverage`
    (p2c/c2l/c2g), then route the gridcell aggregation in `runMixedGridcell` through the
    real `c2g` instead of the ad-hoc weight. That instantiation (fiddly preallocation,
    no existing template — the machinery has zero tests today) is the recommended next
    focused step; the per-physics array rewrite is genuinely Phase-5-scale.
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
    "lake column temperature physics runs and evolves a sane profile").
    **Lake-vs-Fortran parity** (test "lake free-run cold-start tracks the Fortran
    h0 lake trajectory", mirroring CLM.jl's `fortran_parity_lake.jl`): cold-start a
    PCT_LAKE=100 / LAKEDEPTH=10 column (t_lake=277 K, ice-free) on the Bow site and
    free-run 48 hourly steps from 2003-01-01 (forcing index 26304 of the 2000-2004
    series), diffing against the Fortran `clm2.h0.2003-01-01` history. Result: the
    bulk lake thermodynamics track Fortran (deep TLAKE rel diff ~1e-14), while the
    surface ground temperature carries a ~3% residual (TG over-cools) — the SAME
    lake surface turbulent-flux / thermal coupling gap that is unresolved in CLM.jl.
    Reported as a pending parity diagnostic; the hard assertion is stability +
    physical bounds. So the lake solve is correct in the bulk; tight surface parity
    is an open problem shared with the Julia port.
    **URBAN now wired** (2026-06): `urbanFluxesStep` was a no-op that also checked
    the wrong landunit type (`it /= 6`; 6 is wetland). Fixed the gate to urban types
    7/8/9 (`isturb_min..isturb_max`) and wired the ported `UrbanFluxes`
    (`solveCanyonEnergyBalance` → canyon air temp, per-facet sensible heat, HVAC
    waste heat) + `UrbanRadiation` (`netLongwave` multiple-reflection canyon LW),
    writing results into the energy-flux fields and t_ref2m. Validated by 5 sanity/
    stability tests (urban types active + bounded; soil/wetland inert — explicit
    regression guard against the old `/= 6` bug; SH sign follows the surface–air
    gradient). HONEST LIMITS: no urban surfdata or urban Fortran reference exists,
    so NO Fortran parity — only sanity/conservation; and the single-column port uses
    one t_grnd proxy for all five urban facet columns CLM proper splits out.
14. **Glacier** — module DONE (2026-06). Ported `GlacierSurfaceMassBalanceMod` →
    `CLM.BioGeoPhys.GlacierSurfaceMassBalance` (HandleIceMelt + ComputeSurfaceMass-
    Balance + AdjustRunoffTerms as one pure single-column function), cross-checked
    against the Fortran and the CLM.jl port. **Validated by 7 precise deterministic
    unit tests**: meltwater→ice conversion + melt-flux accumulation, ice growth =
    snow-capping flux, net flux = frz−melt, standalone runoff routing (qrgwl+=melt,
    ice_runoff−=melt), glacial-inception threshold, dynamic ice-sheet routing, and
    do_smb-filter passthrough.
    **Driver wiring DONE** (`glacierSMBStep` in PhysicsAdapters): a gated
    PhysicsStep that, on istice columns, caps the snowpack at 10 m SWE, derives the
    snow-capping flux from the excess, runs `glacierSurfaceMassBalance`, and applies
    the updated water state (snow cap + meltwater→ice); non-glacier columns pass
    through unchanged (inert on the soil/lake runs). Validated by a wiring test
    ("glacierSMBStep caps snow and converts meltwater to ice on glacier columns"):
    a glacier column's snow is capped and top-soil meltwater converts to ice, a soil
    column is untouched. The glacier flux diagnostics (qflx_glcice*, adjusted runoff)
    are computed by the module but not persisted (CLMState has no glacier-flux
    field). A full multi-step glacier *gridcell* run still needs column-type dispatch
    in the driver (the soil pipeline isn't glacier-aware) + a glacier reference.
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
17. **External data streams** — time-interpolation core DONE (2026-06). New
    `CLM.Infrastructure.DataStream`: a sorted (time,value) knot series with
    `interpStream` (linear interpolation, clamps outside range, exact at knots),
    `mkDataStream`/`dataStreamFromVectors`/`constantStream`/`nDepRateAt`. Wired into
    the N-deposition path (`scalarVegPath`): the deposition rate is now
    `nDepRateAt defaultNDepStream defaultNDepRate (cnModelTime ctx)`, defaulting to a
    constant stream so existing CN behaviour is bit-unchanged; a multi-knot stream
    drives a time-varying rate. **Validated** by deterministic unit tests of the
    interpolation math (knots/midpoints/clamping/empty/fallback). NOT a Fortran
    `shr_stream` parity claim (no stream dataset/reference here). LAI / crop-calendar
    / urban-tv stream readers still pending (the utility is the reusable core).
18. **atm→lnd downscaling + lnd→atm coupling** — lnd→atm aggregation DONE (2026-06).
    `aggregateLnd2Atm` packs the patch-weighted column surface fluxes (SH/LH/LW-out/
    FSA + an SB-inverted radiative temperature) into the gridcell `l2a_*_grc` fields,
    wired into `energyBalanceStep`; validated by conservation/sanity (gridcell flux
    == column flux on this single-column port; SB temperature round-trip). atm→lnd
    topographic downscaling is the IDENTITY at single-column scope (no sub-grid
    topography) — stated explicitly, not implemented as a separate step. No coupling
    Fortran reference here, so not a parity claim.

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
