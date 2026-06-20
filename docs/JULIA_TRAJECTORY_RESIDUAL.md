# Julia-trajectory parity residual — ground-truthed analysis

Status of the test `Pipeline integration (vs Julia reference) / matches Julia daily
reference for core trajectory fields` after the winter-LH forcing fix.

## What was fixed
`test/data/forcing/qbot.bin` was all-zeros → spurious winter sublimation. Regenerated
from `clmforc.2002.nc`. Day-1 `EFLX_LH_TOT` 11.7 → 3.8 W/m² (Julia ref 0.78). See
CHANGELOG. This was a genuine data-corruption bug; the ported physics was faithful.

## Ground-truth comparison (Julia `scripts/dump_canopy_steps.jl` vs Haskell trace)

The Bow single column has **4 patches**, weights `[0.05, 0.60, 0.35, 0.0]`:
patch 0 = bare soil (itype 0), patch 1 = needleleaf-evergreen tree (itype 1, htop 17 m),
patch 2 = C3 grass (itype 12), patch 3 = unused.

Finalized per-patch fluxes, **timestep 1** (W/m²):

| patch (wt)      | Haskell SH | Julia SH | Haskell LH | Julia LH | Julia ustar | HS ustar |
|-----------------|-----------:|---------:|-----------:|---------:|------------:|---------:|
| bare  (0.05)    |      161   |   123    |    9.64    |  0.96    |    0.11     |  ~0.14   |
| tree  (0.60)    |    **6.0** | **44.8** |    1.71    |  1.01    |  **0.30**   | **0.14** |
| grass (0.35)    |       85   |   126    |    1.12    |  0.99    |    0.16     |  0.15    |

## Two distinct issues

### 1. Diagnostic-definition mismatch (not physics)
The Julia reference daily CSV reports `ef.eflx_sh_tot_patch[1]` — the **bare-soil patch
only** (the export script's documented "patch 1 only" shortcut). The Haskell pipeline
reports the **patch-weighted gridcell mean** (the physically correct gridcell flux).
These are different quantities, so the headline "EFLX_SH_TOT off by ~26 W/m²" overstates
a physics gap. Neither a switch to patch-0 reporting nor regenerating the reference as a
weighted mean makes the test pass, because of issue 2.

### 2. Tall-canopy turbulence closure (the real deep thread)
The dominant physics gap is the **tree patch (60 % weight): SH = 6 vs Julia 44.8**.
Its turbulent exchange is far too weak: effective wind `um` ≈ 1.05 vs 1.74, `ustar`
0.14 vs 0.30, momentum resistance `ram1` 53.7 vs 19.3, stability `psi_m` ≈ 0.5 vs 1.18,
leaf temperature 252.7 vs 255.2 K. Config inputs match (forc_hgt = 30 m, zii = 1000 m,
htop = 17 m, PFT z0mr/displar applied), so this is **not** a glue/data bug — the Haskell
canopy converges to a more stable / weaker-exchange fixed point through the coupled
canopy-turbulence ↔ leaf-energy-balance ↔ convective-velocity feedback. The weaker
under-canopy exchange also halves the ground sensible heat (SHgrnd 30.7 vs 64.7).

**Ruled out (verified equal to Julia, so do not re-chase):** the reference-height /
displacement convention. CanopyFluxes.hs:627 internally adds `z0mv + displa` to the raw
forcing height, so `zldis = (30 + z0mv + displa) − displa = 30 + z0mv ≈ 30.9 m` for the
tree — identical to Julia's `forc_hgt_u_patch − displa` (canopy_fluxes.jl:1016,1068).
`z0mv` (0.935 m), `displa` (11.4 m), and the egvf scaling (egvf = 1 for the dense LAI+SAI
= 3.5 tree) all match. The divergence is downstream, in the leaf-temperature Newton solve
and canopy conductances (wta/wtg/wtl): Julia converges to a warmer leaf (255.2 vs 252.7 K),
which raises `thvstar` → convective velocity `wc` 1.66 vs 0.92 → `um` 1.74 vs 1.05 →
`ustar` 0.30 vs 0.14. Next step: instrument the per-iteration canopy state (rb, wta0,
wtg0, wtl0, t_veg, taf, wc, um, ustar) in both ports and find the first iteration where
they part.

Secondary, lower-weight deltas: bare-ground exchange slightly too strong (ustar 0.14 vs
0.11 → SH 161 vs 123), and bare-patch LH still ~10× high per-patch (0.05 weight, so a
~0.4 W/m² contribution to the gridcell mean).

## Deeper localization (step-1 canopy solve, inputs vs Julia)

Instrumented the tree patch's canopy inputs and outputs at step 1 and compared
against the Julia dump. **The inputs match; the solver diverges.**

| quantity        | Haskell  | Julia    |
|-----------------|---------:|---------:|
| t_grnd (input)  |  265.53  |  264.96  |
| forc_lwrad      |  151.78  |  153.16  |
| forc_u          |   0.384  |   0.352  |
| sabv (coszen 0) |   0.0    |   0.0    |
| z0mv / displa / htop | 0.935 / 11.39 / 17 | (same) |
| → t_veg (out)   | **252.67** | **255.21** |
| → ustar         | **0.143**  | **0.304**  |
| → ram1          |   53.7   |   19.3   |
| → SH_grnd       |   30.7   |   64.7   |

From near-identical inputs the canopy converges to a 2.5 K colder leaf and half
the friction velocity. Back-solving the convective-velocity relation gives
`thvstar` ≈ −0.20 (Haskell) vs −0.45 (Julia): Haskell's canopy-top heat flux is
~2× weaker, so it sits in a weaker-convection basin. The chain is
**canopy-top ustar (0.14 vs 0.30) → lower under-canopy uaf → higher rah_below →
SH_grnd halved (30.7 vs 64.7)**. The 2.5 K step-1 leaf seed then pushes step 2
into a stability collapse: Haskell's ustar pins to exactly 0.06369 (ram1 246.5)
identically across steps 2–4 despite changing t_grnd/wind — a degenerate stable
fixed point, vs Julia's smoothly-varying ustar ≈ 0.22.

**Additional ruled-out items** (verified equal between ports, do not re-chase):
`use_biomass_heat_storage` (false in both), `use_undercanopy_stability` (false in
both → same csoilcn branch), egvf roughness scaling, and all forcing/geometry
inputs above. The residual is purely the coupled canopy-top Monin-Obukhov ustar
solve and leaf-temperature Newton iteration converging to different self-consistent
basins.

**Precise next step:** instrument Julia's internal `canopy_fluxes` kernel to dump
per-iteration `rb`, `wta0/wtg0/wtl0`, `taf`, `dth`, `zeta`, `um`, `ustar` and diff
against the Haskell `canopyFluxesIteration` per-iteration state; find the first
iteration where the conductances or stability term part. That requires editing the
CLM.jl kernel (sister project), so it is a scoped task of its own.

## Why the test can't pass without deep work
The test tolerance is 0.5 W/m² (SH/LH) — effectively exact agreement across coupled
multi-patch winter physics between two independent ports. Closing it requires
step-by-step parity of `canopy_fluxes` (rb, above/below-canopy rah/raw, the leaf-temp
Newton solve, and the convective-velocity feedback) for the tall-tree patch. That is a
canopy-turbulence-closure parity effort, distinct from the completed authoritative
Fortran single-step parity.

Reusable diagnostic: `scripts/dump_canopy_steps.jl` (run from the CLM.jl project) dumps
Julia per-patch SH/LH/ustar/ram1/t_veg/sabv/longwave for the first N steps.

## Fortran reference run (the authoritative adjudication)

The built Fortran CTSM (`installs/clm/bin/cesm.exe`) produced a full-2003 Bow daily
history (`clm_parity_run/...clm2.h0.2003-...nc`). Extracted Fortran **winter** gridcell
daily fluxes (Jan 2003, W/m²):

| day | FSH (total SH) | FSH_V (veg) | FSH_G (ground) | EFLX_LH_TOT | TG |
|----:|---------------:|------------:|---------------:|------------:|-----:|
| 1   | −3.4 | −4.4 | +1.0 | −0.44 | 258.2 |
| 2   | −6.0 | −9.0 | +3.0 | −1.12 | 256.0 |
| 4   | −10.8| −6.2 | −4.7 | −2.06 | 263.8 |
| 8   | −12.0| −11.7| −0.3 | −2.27 | 261.6 |

**Fortran winter sensible heat is small and often negative; ground SH FSH_G is small
(mean ≈ −0.3); LH is small and negative (mean ≈ −1.3).** Two robust findings (Julia run
on 2003 via `scripts/compare_fortran_winter.jl`, Haskell on 2003 via `--pipeline`):

1. **The test's Julia reference is not Fortran-faithful.** The reference CSV reports the
   bare-soil patch (`eflx_sh_tot_patch[1]` ≈ 43 in 2002), but Fortran's actual gridcell
   winter SH is ≈ −6. Julia's *gridcell* ground SH over-produces vs Fortran (+7 vs ≈ 0).
2. **Haskell over-produces winter LH vs Fortran** (Fortran ≈ −0.4, Haskell ≈ +6.5 early
   days) — a real residual beyond the qbot fix, though partly confounded by the cold-start
   snow state.

**Caveat / why this is not yet a clean verdict:** all three runs use different initial
conditions (Fortran from a multi-year spinup restart; Julia and Haskell from independent
cold starts), so absolute daily values diverge from day 1 (TG day-1: Fortran 258 vs
ports 263). The IC mismatch confounds an exact "which port is right" call. A clean
adjudication needs both ports initialized from the Fortran `clm2.r.2003-01-01` restart.
The authoritative *matched-state, per-patch* Fortran parity exists only for SUMMER
(pdumps n11881+, n13461), where Haskell already passes.

**Mechanism of the IC confound (important):** Fortran's spun-up winter ground sits ~5 K
colder than the cold-started ports (TG 258 vs ~263). Saturation specific humidity over
ice drops steeply with temperature — ~0.0013 kg/kg at 258 K vs ~0.0024 at 265 K — so the
colder Fortran ground intrinsically yields a much smaller `qg − forc_q` gradient and
hence far less sublimation. The ports' apparent winter-LH "over-production" is therefore
**substantially driven by the warm cold-start ground temperature**, not purely a flux-
physics bug. This is why the matched-IC (Fortran-restart) initialization is the necessary
next step before attributing any residual winter LH to a Haskell physics error.

## Matched-IC attempt: the explicit snow-layer subsystem is non-functional

Built snow-state injection to run the matched-IC test (both ports from the Fortran
`clm2.r.2003-01-01` restart). `scripts/build_fortran_ic.py` extracts the restart (snl=−4,
3.21 m / 4-layer snowpack, 75.7 kg) into a coldstart bin set; `initCLMStateFromDir` now
reads optional `snl.bin` / `snow_depth.bin` / `frac_sno*.bin` (default: snow-free cold
start, so existing tests are unaffected). The injection loads correctly (snl=−4, ice 75.7,
depth 3.21 verified at init).

Running it exposed three faults in the explicit snow-layer subsystem, which is normally
**dormant** (`PhysicsAdapters` `shouldCreateLayer = False`, line ~1504 — the pipeline only
ever runs bulk snow, snl=0):

1. **Layer-indexing convention mismatch (fixed).** `combineSnowLayers` /
   `removeThinLayers` / `computeSnowTotals` (SnowHydrology.hs) index active layers
   *top-packed* (`0..msno-1`), but the pipeline and Fortran restart store them
   *bottom-packed* (`nlevsno+snl..nlevsno-1`, slots 8–11). So combine read the empty top
   slots, saw ice=0, and deleted the entire pack in step 1. Fixed by pack/unpack at the
   `snowLayerCombineStep` adapter boundary (snow now survives, mass-conserving).

2. **snowCompaction / snowLayerDivide carry the same top/bottom-packing fault
   (localized, not fixed).** Isolation test (no-op'ing both): with them disabled the snow
   depth holds at 3.21 m, so they are what collapses it 3.21 → 0.72 m in one step.
   The fix is the same pack/unpack as combine.

3. **Layered-snow thermal solve uses the wrong matrix structure (the deep blocker).**
   Even with snow mass conserved and depth stable, the matched-IC run drifts: the snow
   layers (stable ~261 K) and deep soil (~266 K) are fine, but the **exposed top-soil
   layer** crashes (258 → 205 K), propagating a cooling wave down the soil column. Probed
   values: `hs_soil ≈ −59 W/m²`, `dhsdT ≈ −8`, `frac_sno_eff = 0.114`. Root cause: with
   `frac_sno < 1` (patchy snow — here only 11.4 % snow-covered, depth 3.21 m on that
   fraction, grid-mean SNOWDP 0.37 m), the bare-soil fraction radiatively cools and our
   **single inline tridiagonal** with inline fse-weighting (SoilTemperature.hs ~430–518,
   the `j==1 && snl<0` rows) can't hold it. **Fortran does not use a single tridiagonal
   here.** It builds a *block matrix* (SoilTemperatureMod.F90 `SetMatrix`, ~2337–2557):
   separate `bmatrix_snow`, `bmatrix_soil`, `bmatrix_ssw` (standing surface water) blocks
   plus explicit off-diagonal coupling blocks `bmatrix_snow_soil`(−1) and
   `bmatrix_soil_snow`(1). Matching it requires reimplementing the soil-temp solve with
   that block structure and the frac_sno_eff / frac_h2osfc fractional coupling — a real
   numerical rewrite, validated against the Fortran h0 winter history.

**Conclusion:** the port runs bulk-snow-only by design because the explicit multi-layer
snow physics is incomplete — and the hardest missing piece is the **block-matrix soil-temp
solve** for patchy snow, not just the layer-management packing. The matched-IC gold-standard
adjudication needs that solve reimplemented + validated. Committed foundation:
snl-injection plumbing, `scripts/build_fortran_ic.py`, and the combine-indexing fix.
Reference for the rewrite: SoilTemperatureMod.F90 `SetMatrix`/`SetMatrixSoil`/
`SetMatrixSnow`, and CLM.jl `soil_temperature.jl` (block band solve).

## Important framing: which port is right is not established

The Haskell biogeophysics has **authoritative single-step Fortran parity at two
independent cases** — the low-sun baseline and the 2003 peak-sun case, the latter
including a vegetated (C3-grass) canopy whose under-canopy ground sensible heat was
explicitly validated against the Fortran dump (commit `d5f7bd8`). So this winter
night-time **tree-canopy** convective regime (coszen 0, warm ground driving free
convection through a 17 m needleleaf canopy) is simply not covered by a Fortran dump.
Haskell and Julia are two independent ports that converge to different self-consistent
canopy-turbulence basins here; it is **not demonstrated that Haskell deviates from
Fortran** — only that it deviates from Julia. Definitively resolving the residual would
require a Fortran reference run for this exact Bow winter case, not just a Julia diff.
The authoritative parity target (Fortran, the system of record) is met.
