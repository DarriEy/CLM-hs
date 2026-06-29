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
   - **CNDV** (dynamic vegetation) — WIRED (2026-06). Added `clmDGVS` (DGVSData) +
     `clmCNDVYear` to CLMState, a `ppCNDV` pipeline slot, and `cndvStep` (PhysicsAdapters)
     running every step in `CLM.BioGeoChem.CNDVStep`. Faithful climate accumulators
     (agdd base-5, agddtw fed by a tracked 10-day t_a10, 30-day t_mo, 365-day prec365,
     annsum_npp, 20-yr tmomin20/agdd20 with the (19·old+new)/20 weighting) feed the annual
     Light→Establishment→Mortality driver gated on tcIsBegCurrYear. PFT bioclimatic limits
     (tcmin/tcmax/gddmin/twmax) resolved per-patch from a canonical LPJ/CLM-DGVM table
     (`dgvmPftBioclim`). Live activation: `use_cndv` (RunConfig) → `pcUseCndv` (PipelineConfig)
     seeds `clmDGVS` at cold start with a single natural-veg stand; unseeded ⇒ `cndvStep`
     is a no-op. Sapling establishment is ported (`estab_rate = 0.24·(1−e^(5·(fpc_tree−1)))/
     n_estab`, `nind += estab_rate·(1−fpc_tree)`), so woody PFTs passing the bioclimatic
     filter grow `nind`/`fpcgrid` into the open canopy. Grasses (nind=1, crownarea=1) fill the
     ground the tree canopy leaves, capped at fpc_grass_max = 1−min(fpc_tree,0.95); the tree
     canopy is re-capped at 0.95 after establishment — so combined cover stays ≤ 1. The full
     Light→survival→Mortality→Establishment (woody + grass) loop. 17 unit+integration tests.
     The real per-PFT bioclimatic constants (pftpar20/28-31) are now loaded from clm5_params.nc
     via `readDGVEcophysCon` (DGVEcophysCon on CLMState); `cndvStep` uses them when present and
     falls back to the LPJ table otherwise (scripts/extract_cndv_pftpar.py regenerates the
     pftcon_pftpar*.bin). REMAINING (can't close here): no dynamic-veg Fortran reference exists
     (sanity/conservation-validated only);
     the running means are the steady-state equivalent of CLM's boxcar runmean (no ring buffer);
     establishment is not coupled to prescribed sapling carbon pools (Fortran hardcodes
     leafcmax=1/deadstemc=0.1, so nind growth is self-contained); and the carbon-driven
     (slatop/LAI) grass FPC is simplified to "fill available space up to the grass cap".
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
    real `c2g` instead of the ad-hoc weight.
    **DONE (2026-06-29).** `runMixedGridcell` routes its aggregation through the real
    `c2g`, and the construction is now the reusable, data-driven
    `InitSubgrid.buildSingleColumnGridcell [(landunit itype, area weight)]` primitive
    (one column+patch per landunit, N landunits — generalizing the hand-inlined
    soil+lake cell). The machinery is now under test: a 3-landunit (soil+lake+ice)
    gridcell exercises clmPtrsCompdown down-pointers, clmPtrsCheck (valid + broken),
    the full SubgridAverage operator set (p2c/c2g/l2g/p2g/c2g2d), L2G scale-type
    selection, and `Filters.buildFilters` classification (11 tests; suite 217/0).
    The remaining per-physics CLMState array rewrite (running each column's physics off
    these pointers) is genuinely Phase-5-scale, but its **migration-safe foundation is
    now in place (2026-06-29):** `CLM.Driver.Vectorized` defines a Structure-of-Arrays
    `CLMStateV` (per-column `VU.Vector`s; per-(column,layer) flattened `c*nlev+j`, the
    same layout `SubgridAverage` assumes) that COEXISTS with the scalar `CLMState`,
    bridged by `gather`/`scatter`. That bridge is a free correctness oracle —
    `scatter sts (fV cfg ctx (gather sts)) == map (f cfg ctx) sts` must hold bit-for-bit
    — so vectorization changes only storage + iteration (`VU.zipWith`/`generate`) while
    reusing the existing per-column physics KERNELS, keeping the suite green throughout.
    Migrated + oracle-tested so far — **11 of ~37 adapters** (suite 233/0), incl.
    `waterTableStepV`, `surfaceHumidityStepV`, and `soilHydrologyStepV` (the Richards
    solve via scalar-kernel-reuse per column; handles the `snl→0` meltout):
    `waterBalanceStepV`, `hydrologyDrainageStepV`, `drvInitStepV` (introduces the
    per-(col,layer) `c*nlev+j` flatten), `soilEvapResistanceStepV` (multi-record +
    `calcBetaLeePielke1992` + soil-vs-grid striding), `fracH2oSfcStepV` (snow-layer
    sum), `preFluxCalcsStepV`, `snowCompactionStepV` (reuses the scalar kernel
    per-column for the sequential overburden loop; `colZi` on its own `vNlev+1`
    interface stride), and `snowPercolationStepV` (reuses `snowPercolationBottomPacked`
    with the per-column snow gate). Drafted across two parallel agent swarms (one
    adapter per agent) and integrated centrally — the swarm-able workflow is proven:
    each remaining adapter is an independent unit whose bit-identical equivalence test
    is its own gate (agents draft code; the build is integrated serially since stack
    locks `.stack-work`). The SoA layout is GPU/AD-ready.
    **The remaining adapters split into two classes.** (a) Column/layer-only adapters
    (`waterTableStep`/`soilHydrologyStep` done; `snowAging`, `snowLayerCombine/Divide`
    — the last two structural, change `snl`) keep migrating cleanly via scalar-reuse.
    (b) **Patch-vectorized adapters** (`soilFluxesStep`, `soilTemperatureFullStep`,
    `canopyFluxesStep`, `baregroundFluxesStep`) read/write **variable-length per-patch
    (`*_patch_vec`) arrays** whose length defines patchCount — these do NOT fit the flat
    `c*nlev+j` SoA layout and are DEFERRED pending a proper patch-indexed layout
    (per-column patch count/offset vectors, CSR-style). That layout is the next design
    step before class (b); it then unblocks ~6 adapters at once. `energyBalanceStep`
    stays deferred (its only live write is the gridcell `aggregateLnd2Atm` reduction —
    a `c2g`, not a per-column map). The architecture + oracle + multi-shape templates
    are done; this is multi-session mechanical cranking plus one patch-layout design.
    **PATCH-INDEXED LAYOUT DONE (2026-06-29):** `CLMStateV` now carries a CSR
    `vPatchOff` (length nCols+1) so ragged per-patch fields store flat with per-column
    variable patch counts; `patchOffsets`/`patchSliceD`/`patchSliceI` helpers + a
    `vp_wtgcell` first field, round-trip validated over columns of patch count 1/2/3
    (and empty) — suite 234/0. This unblocks class (b): `soilFluxesStep`,
    `soilTemperatureFullStep`, `canopyFluxesStep`, `baregroundFluxesStep` now migrate
    by storing their `*_patch_vec` fields flat via the CSR helpers + scalar-kernel-reuse.
    **CSR layout validated end-to-end with a real patch adapter (2026-06-29):**
    `soilFluxesStepV` migrated — its ~25 fields incl. the ragged `*_patch_vec` energy/
    water fluxes store flat via the CSR helpers; bit-identical oracle over 3 columns.
    **14 of ~37 adapters vectorized; suite 237/0 (incl. soilTemperature, baregroundFluxes, soilFluxes on the CSR patch layout). Remaining: canopyFluxesStep (594 lines) + column-only snowAging/snowLayerCombine/snowLayerDivide.** Remaining patch adapters
    (`soilTemperatureFullStep`, `canopyFluxesStep`, `baregroundFluxesStep`) follow the
    same CSR + scalar-reuse recipe; then the column-only `snowAging`/`snowLayerCombine`/
    `snowLayerDivide`. Mechanical from here.

    **COMPLETE PER-COLUMN SoA + NATIVE PIPELINE + NATIVE MULTI-PATCH — DONE (2026-06-29):**
    all 20 per-column physics adapters now have bit-identical `*StepV` SoA
    counterparts (soil temp/hydrology, soil/bareground/canopy fluxes, surface
    humidity, all snow steps, water table, radiation, albedo). `runVectorizedPipeline`
    chains all 20 through ONE `CLMStateV` (single gather/scatter) in canonical order
    and is proven bit-identical to the per-column scalar pipeline. **It now drives the
    REAL cold-start state** (np>1 PFT patches per column): the flat CSR is reconciled
    by giving the two patch fields whose per-column length does NOT equal the patch
    count their OWN traveling offsets — `vp_fsun_off` (`cstate_fsun_patch` is length-np
    out of surfaceRadiation but a length-1 representative singleton out of surfaceAlbedo)
    and `vp_ivt_off` (`clmPatchIvt` stores the single dominant PFT, length 1, not one
    per patch). All other patch fields are stably length-np or empty (null-branch) or
    have their own stride (`vp_optband_off` for optical np×numrad, `vp_rootfr_off` for
    per-(patch,layer)). Suite 245/0. The SoA fast path runs end-to-end on real
    multi-column gridcell data — GPU/AD-ready. Remaining non-vectorized steps
    (energyBalance c2g reduction; lake/urban/glacier dispatch; CN/dust/VOC/diagnostic)
    are by-design outside the per-column fast path.

    **VECTORIZED MULTI-LANDUNIT DRIVER + c2g REDUCTION — DONE (2026-06-29):**
    `gridDiagReduceV` aggregates a `CLMStateV` column->gridcell straight off the
    SoA column arrays (`vt_grnd`/`vh2osno`/`veflx_sh_tot`/`veflx_lh_tot`/`vfsa`)
    through the real area-weighted `SA.c2g1d` — the energyBalance-style c2g done as
    ONE vectorized reduction stage (no per-column scatter). `runGridcellColumnsV`
    is the end-to-end driver: a whole multi-landunit gridcell of soil columns
    advances through `runVectorizedPipeline` (SoA batch) each timestep and reduces
    to a `GridDiag` series via `gridDiagReduceV`. Proven (test) bit-identical to
    running the same 20-step pipeline scalar per column and aggregating with the
    same `c2g1d`, on the REAL np>1 cold-start state across 3 distinct soil columns.
    Lake/glacier/urban gating stays on the scalar `runGridcellColumns` dispatch
    (those kinds aren't in the 20-step per-column pipeline). Suite 246/0.
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

(Updated 2026-06.) Phases 1–4 are essentially done: the single soil column closes
its budgets, the CN/BGC path is ~⅔ wired (now incl. CNDV, dust, VOC, wood
products+harvest, CN annual update — all data-driven from `clm5_params.nc`/MEGAN
via the column's actual PFT), the mixed soil+lake gridcell loop routes its
aggregation through the real `c2g`, and the soil column now has a Fortran h0
parity test (TG tracks to ~2% with the known winter SH/snow residual). The
original "Phase 1 items 1–4" step below is complete.

The highest-leverage work is now **closing the validation gap** — converting
"sanity/conservation-validated" into "Fortran-parity-validated" across regimes:

1. **Soil-column h0 seasonal map — DONE (2026-06).** A 360-day free run vs
   `clm_parity_run` clm2.h0 bins the TG rel residual by quarter: Q1(JFM)
   0.080|0.023, Q2(AMJ) 0.064|0.016, Q3(JAS) 0.045|0.015, Q4(OND) 0.087|0.023
   (max|mean). Summer tracks tightly (1.5% mean); the cold quarters are ~2x worse
   and the residual dips in summer — so it is seasonal physics, not just drift.
2. **Winter residual root cause — FOUND (2026-06).** A per-day diff vs the h0
   localized it to SNOW ALBEDO: the port absorbs up to 2x less solar (FSA) under
   full snow, with frac_sno (FSNO) saturating to ~0.96 vs Fortran ~0.11 at the
   same SWE. Warm-starting from the Fortran snowpack (new `pcFortranRestart`) makes
   TG *worse*, proving it is a snow-physics bug, not an IC artifact. The bug:
   `snowWaterStep` hardcodes `n_melt = 1.0` instead of deriving the SCA shape
   parameter from topographic `std_elev` (`initNMelt`: n_melt_coef/max(10,std_elev);
   Bow std_elev=500 → n_melt≈0.4). This supersedes the prior canopy-turbulence
   theory as the *dominant* winter driver.
   **n_melt fix done + measured (commit c43a097):** threaded `std_elev` → `n_melt`
   (Bow → 0.4). Verified correctly wired but INERT — n_melt only shapes the SL2012
   *depletion* curve, and Bow winter is accumulation-dominated, so `frac_sno`
   saturates to ~0.96 via the *accumulation* curve regardless. No Julia-parity
   breakage (the tradeoff did not materialize) and no Fortran improvement; kept as
   a correctness fix.
   **Method investigation done (commit 3f40389) — NOT a method mismatch.** Fortran
   Bow's lnd_in sets `snow_cover_fraction_method='SwensonLawrence2012'`, the same as
   the port. The real bug: `snowWaterStep` calls `updateSnowDepthAndFracSL2012` with
   the **snowmelt arg hardcoded to 0.0**, so SL2012's depletion curve never fires and
   `frac_sno` is a one-way ratchet (→ ~0.96 vs Fortran ~0.11). The melt flux
   `qflx_snomelt` IS computed in `SoilTemperature.phaseChange` but
   `soilTemperatureFullStep` discards it. Confirmed by experiment: forcing the
   depletion path on drops `frac_sno` 0.96→0.52 and raises absorbed solar toward
   Fortran. **snowmelt fix done (commit 2a4fd45):** `clmQflxSnomelt` carries the
   phase-change melt flux from `soilTemperatureFullStep` into `snowWaterStep`'s
   depletion. Correct but does NOT close the winter gap — deep Bow winter has no melt
   (`qflx_snomelt=0`), so `frac_sno` still ratchets up. Julia diagnostic shifted 59→60
   field-days (honest-tolerances test still passes); kept as faithful.
   **REAL winter driver = `int_snow` spin-up.** Fortran's spun-up Jan-1 state has
   `int_snow`≈580 mm (season-long accumulation+sublimation; `smr=h2osno/int_snow`≈0.15 →
   `frac_sno`≈0.11), while the cold-started port has `int_snow`≈`h2osno` (`smr`≈1 →
   `frac_sno`≈1). This is a **spin-up / cold-start limitation**, not a physics bug — much
   of the "winter residual" is the port starting Jan-1 bare vs Fortran's mature snowpack.
   **SYNTHESIS (commit 3fd771e) — two coupled bugs that partially cancel.** A warm-start
   from the Fortran restart (`--pipeline-warm`) reproduces the CORRECT day-1 snow cover
   (frac_sno 0.115 ≈ Fortran 0.114, H2OSNO 75.7 ≈ 75.5) — yet TG **over-cools ~15 K**
   (warm day-1 244.4 vs cold-start 259.8). The wrong-high cold-start frac_sno (~0.96) was
   *masking* a bare-ground/canopy surface-energy-balance over-cooling (the SH/ustar
   canopy-turbulence residual). As frac_sno ratchets back up over days 8-18, TG recovers.
   So the cold-start's two bugs partially cancel, which is why every single fix was inert
   or worsened TG. **FIX requires BOTH together:** (a) the bare-ground/canopy SH
   over-cooling (needs a per-iteration canopy-solve dump to find where ustar/leaf-temp
   part), and (b) the frac_sno `int_snow` spin-up — then re-measure jointly. The
   warm-start test now encodes the coupling as a regression guard.
3. **Landunit gridcell runs**: column-type dispatch in the driver so glacier/
   urban/wetland columns run in `runMixedGridcell` (lake already does), each vs
   its Fortran reference (`clm_glacier_run`, etc.).
   **IN PROGRESS (2026-06-29).** The driver is now a general, data-driven
   N-column loop: `runGridcellColumns [(landunit itype, area weight, ColumnKind)]`
   dispatches each column's physics by `ColumnKind` (`SoilCol` → full `clmDrv`
   pipeline, `LakeCol` → lake flux+temperature path), threads per-column state
   independently, and aggregates through the real `c2g`. `runMixedGridcell` is now
   a thin wrapper over it (soil+lake, bit-identical). Validated by an N=3
   (soil+lake+soil) dispatch test + soil+lake equivalence to `runMixedGridcell`
   (suite 218/0). **`GlacierCol` now wired (2026-06-29):** a third dispatch kind
   runs the soil-thermal pipeline plus `glacierSMBStep` (snowpack capped at 10 m
   SWE, excess → ice) on an `istice`-marked column — the first time
   `glacierSMBStep` runs in a live multi-step driver (previously a standalone unit
   test only). Validated by a soil+lake+glacier 3-kind dispatch test (bounded,
   finite, area-weighted aggregate; suite 219/0). **`UrbanCol` now wired
   (2026-06-29):** a fourth dispatch kind runs the pipeline on an `isturb_md`-marked
   column so the gated `urbanFluxesStep` (canyon energy balance) fires. The gridcell
   driver also now aggregates a richer diagnostic bundle — `GridDiag`
   {T_GRND, H2OSNO, SH, LH, FSA} — with **every field averaged column→gridcell
   through the real `c2g`** (conservation-tested to 1e-9), so the multi-landunit cell
   emits coupler-grade gridcell means rather than just T_GRND/H2OSNO. Suite 220/0.
   **NetCDF gridcell output now wired (2026-06-29):** `writeGridcellNetCDF` emits
   the `GridDiag` series (T_GRND/H2OSNO/SH/LH/FSA) to a CF-attributed, time-
   dimensioned NetCDF history tape, round-trip validated to 1e-12 — so a
   mixed-landunit cell produces a real machine-comparable gridcell history file
   (suite 221/0). REMAINING: `WetlandCol` (≈ soil path), a per-landunit Fortran
   reference (glacier/urban stay sanity/conservation-validated only), and routing
   the `GridDiag` bundle into the `aggregateLnd2Atm` lnd→atm coupling carriers.
4. **Streams**: LAI / crop-calendar / urban-tv readers on the `DataStream` core.

Deferred (large / out of scope): crop (unported, needs a crop landunit), the
matrix C/N solver, and Phase 5 (dyn_subgrid, FATES, MPI). Regenerating
instrumented Fortran references across regimes/landunits remains the structural
blocker for tight parity everywhere, but the per-landunit h0 tapes that DO exist
(soil/lake/glacier) are now the practical parity targets.
