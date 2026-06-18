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

## Recommendation

Land biogeophysics first (hydrology+radiation done; soil-temp in progress). Then
run CN as a dedicated phase: do steps 1–2 serially (harness + state wiring are
shared infra), then fan out step 3's five groups in parallel worktrees, each
iterating to its probe/boundary tolerance — exactly the pattern that worked for
biogeophysics. Expect this to be the bulk of the remaining effort.
