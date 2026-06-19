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

Secondary, lower-weight deltas: bare-ground exchange slightly too strong (ustar 0.14 vs
0.11 → SH 161 vs 123), and bare-patch LH still ~10× high per-patch (0.05 weight, so a
~0.4 W/m² contribution to the gridcell mean).

## Why the test can't pass without deep work
The test tolerance is 0.5 W/m² (SH/LH) — effectively exact agreement across coupled
multi-patch winter physics between two independent ports. Closing it requires
step-by-step parity of `canopy_fluxes` (rb, above/below-canopy rah/raw, the leaf-temp
Newton solve, and the convective-velocity feedback) for the tall-tree patch. That is a
canopy-turbulence-closure parity effort, distinct from the completed authoritative
Fortran single-step parity.

Reusable diagnostic: `scripts/dump_canopy_steps.jl` (run from the CLM.jl project) dumps
Julia per-patch SH/LH/ustar/ram1/t_veg for the first N steps.
