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
