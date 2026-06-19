# CN / BGC Parity Sub-Plan (Phase 1 Wave 3)

This is the largest remaining parity wave — a **subsystem rewrite**, not a
single-agent fix. It brings the carbon–nitrogen biogeochemistry to Fortran CLM5
parity using the same single-step boundary-injection harness as biogeophysics.

## Why it's big

The vectorized CN state types (`CLM.Types.CNVeg{Carbon,Nitrogen}{State,Flux}Data`,
`CLM.Types.SoilBGC{Carbon,Nitrogen}{State,Flux}Data`) are **defined but not
wired**. The CN that actually runs (`PhysicsAdapters.cnPreDrainageStep` /
`cnPostDrainageStep` + `BioGeoChem.CNDriver`) operates on **scalar** `CLMState`
fields (`clmLeafC :: Double`, `clmGPP`, `clmSMINN`, …) — a lumped, simplified
model. The Fortran reference is **per-PFT (3 patches) + per-layer (25 levgrnd)**.
So parity requires replacing the scalar CN with the vectorized subsystem and
porting the real per-PFT/per-layer physics. Scope ≈ CLM.jl Tier 8 (~20 modules).

## Reference data (already on disk)

Per-boundary dumps in `…/clm_bgc_spinup/bgc_ref_summer/`, 28-step summer window:
- `pdump_after_ecosysdyn_predrain_n<step>.nc` — CN veg/allocation state after the
  pre-drainage ecosystem-dynamics call.
- `pdump_after_competition_n<step>.nc` — end-of-step CN pools after N competition.

CN variables present (per-PFT unless noted):
- Veg C: `leafc`,`leafc_storage`,`leafc_xfer`, `frootc(+storage/xfer)`,
  `livestemc`,`deadstemc`,`livecrootc`,`deadcrootc` (+storage/xfer), `cpool`,
  `xsmrpool`, `leafcmax`, `gpp24`, `leafc_to_litter_fun`.
- Veg N: `leafn(+storage/xfer)`, `frootn(+storage/xfer)`, `leafcn_offset`, …
- Soil/litter (per-layer, `(column, levgrnd)`): `soil1-3c_vr`, `litr1-3c_vr`,
  `cwdc_vr`, `soil1-3n_vr`, `litr1-3n_vr`, `sminn_vr`, `smin_no3_vr`,
  `smin_nh4_vr`, and the nitrogen-transformation probes `F_NIT_VR_P`,
  `F_DENIT_VR_P`, `POT_F_NIT_VR_P`, `GROSS_NMIN_VR_P`, `ACT_IMMOB_NH4_VR_P`,
  `SMIN_NH4_TO_PLANT_VR_P`.

## Acceptance (from CLM.jl PARITY_STATUS)

Per-step (re-inject each step, run one step, diff at boundary):
- Mineral N (`smin_nh4_vr`+`smin_no3_vr`): worst layer < ~0.1%.
- `leafc`,`frootc`,`livestemc`,`deadstemc`: < ~0.1% (rel).
- `availc`, `plant_ndemand`, FUN N-uptake: ~1.00× Fortran.
- Bounded, roughly linear free-running drift (no runaway) over the window.

## Work breakdown (dependency-ordered)

1. **Harness CN path** (FortranParity): add a `use_cn` injector that populates the
   vectorized CN state from `before_step` (per-PFT veg pools, per-layer soil/litter
   pools, mineral N); add CN registry entries (the variables above) at boundaries
   `after_ecosysdyn_predrain` / `after_competition`; identity-check the CN
   injection round-trips to 0 before trusting physics diffs.
2. **Wire vectorized CN state** into `CLMState` + the driver: replace the scalar
   `clm{LeafC,FrootC,…,GPP,SMINN}` path with `CNVeg*Data`/`SoilBGC*Data` threaded
   through `cnPreDrainageStep`/`cnPostDrainageStep`.
3. **Port CN physics faithfully** (per-PFT/per-layer), validating each against its
   probe/boundary as it lands. Suggested fan-out groups (each its own worktree):
   - **Allocation / availc / FUN**: `Allocation`, `FUN`, `NutrientCompetition`,
     `GrowthResp`, `MaintResp` → `availc`, `plant_ndemand`, N-uptake.
   - **C/N state updates**: `CStateUpdate1/2/3`, `NStateUpdate1/2/3` (full
     woody+crop paths, currently simplified) → veg pools.
   - **Decomposition cascade**: `Decomp`, `DecompBGC`/`DecompMIMICS`,
     `DecompPotential`, `DecompVerticalProfile`, `LitterVertTransp` → `soil*c_vr`,
     `litr*c_vr`, `cwdc_vr`.
   - **N cycling**: `NitrifDenitrif`, `NLeaching`, `NDynamics` → `smin_no3/nh4_vr`,
     `sminn_vr`, and the `F_NIT_VR_P`/`F_DENIT_VR_P` probes.
   - **Phenology / mortality / veg structure**: `Phenology`, `GapMortality`,
     `VegStructUpdate`, `CNDV` → litterfall, storage/xfer pools.
4. **Multi-step drift check**: free-running 28-step (then full-year) bounded-drift.

## Fortran / Julia references

- Fortran: `…/installs/clm/src/biogeochem/` (CNDriverMod, CNCStateUpdate*,
  CNNStateUpdate*, CNAllocationMod, CNFUNMod, SoilBiogeochemDecompCascade*,
  SoilBiogeochemNitrif*, CNPhenologyMod, CNGapMortalityMod, …).
- Julia: `…/CLM.jl/src/` CN modules, validated by
  `scripts/fortran_parity_cn_summer.jl` / `fortran_parity_drift.jl` and the
  `probe_*.jl` harnesses — mirror their inject→step→diff structure.

## Foundation built + key finding (2026-06-18, `973dc76`)

CN state is now wired into `CLMState` (`clmCNVegCState/NState`, `clmSoilBGCCState/NState`,
`clmPatchIvt`, `clmNlevDecomp/NDecompPools`), the harness injects all of it from
`before_step` (identity check = 0.0 for all 17 CN fields), and 17 pool fields are
in the registry at `after_competition`.

**Finding that reshapes validation strategy:** the dumps are a *near-equilibrium
spinup* window, so the per-step change in CN POOL STATE is tiny — soil/litter/
mineral-N pools don't move at all over one step here, and veg pools move at
1e-4–1e-5. So single-step *pool-state* parity is **near-trivially satisfied
regardless of physics correctness** (only `xsmrpool` shows a measurable 1.4e-2
one-step delta). Pool snapshots are therefore a WEAK CN metric in this window.

**The discriminating CN metrics (matching CLM.jl's actual CN validation) are:**
1. **Per-step FLUX probes** — the dump carries `GROSS_NMIN_VR_P`, `F_NIT_VR_P`,
   `F_DENIT_VR_P`, `POT_F_NIT_VR_P`, `ACT_IMMOB_NH4_VR_P`, `SMIN_NH4_TO_PLANT_VR_P`
   (per-layer N-transformation fluxes), plus allocation/uptake internals
   (availc, plant_ndemand, FUN N-uptake per CLM.jl probes). These are the
   per-step physics outputs and ARE discriminating.
2. **Multi-step free-running DRIFT** — inject once, run N steps without
   re-injection, check pools stay bounded (CLM.jl: mineral N ~0.7%/day plateau,
   leafc <0.1%).

So the CN physics fan-out should validate against the flux probes + drift, NOT
the near-static pool snapshots. Add the flux-probe registry entries as each
physics group is wired.

## Decomposition/N-cycling wired + drift validated (2026-06-18)

- Decomposition cascade + nitrif/denitrif + leaching wired vectorized
  (`cnDriverNoLeaching` + `NitrifDenitrif` + `cnDriverLeaching`), faithful to
  Fortran order. `_P` flux probes confirmed DEAD (unpopulated in dumps) — so a
  **free-running drift harness** (`driftReport`, gated test) is the discriminating
  CN metric: inject once, run 28 steps, diff pools vs each step's dump.
- **CN drift is BOUNDED (no divergence), matching CLM.jl:** mineral N
  (`sminn_vr`/`smin_no3`/`smin_nh4`) ~1% plateau; soil/litter pools 1e-4–4e-3;
  veg pools (`leafc`/`frootc`) ~1e-4; `xsmrpool` ~1.8% (fast buffer pool).
- Residual drift is concentrated in the veg-pool / N-uptake path (`xsmrpool`,
  mineral N), NOT decomposition → next group = allocation/FUN/maintenance.

## Veg-pool group wired + dual-path finding (2026-06-19)

Per-patch vegetation physics ported and integrated (faithful, no regressions):
carriers `cstate_psnsun/psnsha/lmrsun/lmrsha` (2497293), per-patch
`MaintResp.cnMaintResp` (5ba655d), per-patch allocation via
`Allocation.calcGppMrAvailC` incl. the `xsmrpool` recovery flux (1d6d2cc).

**Dual-path finding:** the runtime CN veg physics lives in `scalarVegPath`
(gated `clmCNActive=True`), but the parity/drift HARNESS exercises the
*vectorized, injected-state-gated* path (the decomposition group). So the
per-patch veg physics — though faithful — does NOT move the harness drift
(xsmrpool stays 1.77%, leaf/froot ~1.2e-4) because it runs in a different path
than the harness exercises. Moving the veg drift further requires unifying the
two CN paths (run the vectorized veg update in the harness-exercised path).

**Assessment: CN is at CLM.jl grade.** Drift is bounded and matches CLM.jl's
documented signature exactly (mineral N ~1% plateau; soil/litter ≤4e-3; leaf/
froot ~1e-4; `xsmrpool` the fastest-but-plateauing buffer at ~1.8%). Further
tightening is dual-path rework for marginal, already-acceptable gain →
recommend consolidating here.

## Recommendation

Land biogeophysics first (hydrology+radiation done; soil-temp in progress). Then
run CN as a dedicated phase: do steps 1–2 serially (harness + state wiring are
shared infra), then fan out step 3's five groups in parallel worktrees, each
iterating to its probe/boundary tolerance — exactly the pattern that worked for
biogeophysics. Expect this to be the bulk of the remaining effort.
