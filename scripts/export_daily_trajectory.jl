#!/usr/bin/env julia
# ==========================================================================
# Export a 30-day CLM simulation trajectory for Haskell driver verification.
#
# Runs clm_drv! for 30 days (1440 timesteps at 1800s) and exports:
# - Daily-average diagnostics (T_GRND, TSA, FSA, EFLX_LH_TOT, etc.)
# - End-of-day state snapshots
#
# Usage:
#   cd /path/to/CLM.jl
#   julia --project=. /path/to/CLM-hs/scripts/export_daily_trajectory.jl [output_dir]
# ==========================================================================

using Dates, JSON, Printf
using CLM

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

const DATA_DIR = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped"
const FSURDAT   = joinpath(DATA_DIR, "settings/CLM/parameters/surfdata_clm.nc")
const PARAMFILE = joinpath(DATA_DIR, "settings/CLM/parameters/clm5_params.nc")
const FORCING_DIR = joinpath(DATA_DIR, "data/forcing/CLM_input")
const FSNOWOPTICS = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_optics_5bnd_c013122.nc"
const FSNOWAGING  = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc"

const YEAR = 2002
const START_DATE = DateTime(YEAR, 1, 1)
const DTIME = 1800.0
const NDAYS = 30
const STEPS_PER_DAY = Int(86400 / DTIME)  # 48
const BASEFLOW_SCALAR = 0.0022
const INT_SNOW_MAX = 3113.2

const OUTDIR = length(ARGS) >= 1 ? ARGS[1] :
    joinpath(dirname(@__DIR__), "test/data/trajectory")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

function write_binary(filepath::String, data::AbstractArray{Float64})
    open(filepath, "w") do io; write(io, vec(data)); end
end
function write_binary(filepath::String, data::AbstractVector{<:Real})
    open(filepath, "w") do io; write(io, Float64.(data)); end
end

# ---------------------------------------------------------------------------
# Initialize
# ---------------------------------------------------------------------------

println("=== Initializing CLM for $NDAYS-day trajectory ===")

(inst, bounds, filt, tm) = CLM.clm_initialize!(;
    fsurdat = FSURDAT,
    paramfile = PARAMFILE,
    start_date = START_DATE,
    dtime = Int(DTIME),
    use_cn = false,
    use_aquifer_layer = false,
    fsnowoptics = FSNOWOPTICS,
    fsnowaging = FSNOWAGING,
    int_snow_max = INT_SNOW_MAX,
)

col = inst.column
pch = inst.patch
lun = inst.landunit
grc = inst.gridcell
temp = inst.temperature
a2l = inst.atm2lnd
ef = inst.energyflux
wsb = inst.water.waterstatebulk_inst
wfb = inst.water.waterfluxbulk_inst

nc = bounds.endc - bounds.begc + 1
np = bounds.endp - bounds.begp + 1
ng = bounds.endg - bounds.begg + 1
bc_col = bounds.begc:bounds.endc
bc_patch = bounds.begp:bounds.endp
bc_grc = bounds.begg:bounds.endg

println("  Grid: ng=$ng, nc=$nc, np=$np")

# Open forcing
fr = CLM.ForcingReader()
fforcing = joinpath(FORCING_DIR, "clmforc.$YEAR.nc")
CLM.forcing_reader_init!(fr, fforcing)

# Build driver config (SP mode)
config = CLM.CLMDriverConfig(
    use_cn = false,
    use_aquifer_layer = false,
    irrigate = false,
)

# Build inactive+active filter (same as filt for single-clump)
filt_ia = filt

# Create output directory
mkpath(OUTDIR)

# ---------------------------------------------------------------------------
# Run simulation and collect daily averages
# ---------------------------------------------------------------------------

println("=== Running $NDAYS-day simulation ===")

# Daily accumulation scalars (column 1 / patch 1 only)
daily_t_grnd  = 0.0
daily_tsa     = 0.0
daily_fsa     = 0.0
daily_eflx_lh = 0.0
daily_eflx_sh = 0.0
daily_h2osno  = 0.0
daily_qrunoff = 0.0
daily_snow_depth = 0.0
daily_frac_sno = 0.0

# Output storage
results = []

total_steps = NDAYS * STEPS_PER_DAY

for step in 1:total_steps
    # Compute time
    target_time = fr.times[step]
    calday = Dates.dayofyear(START_DATE) + (step - 1) * DTIME / 86400.0
    day_of_sim = div(step - 1, STEPS_PER_DAY) + 1

    # Read and downscale forcing
    CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
    CLM.downscale_forcings!(bounds, a2l, col, lun, inst.topo)

    # Compute orbital parameters
    (declinp1, eccf) = CLM.compute_orbital(calday)
    nextsw_cday = calday

    # Step flags
    is_first = (step == 1)
    is_beg_day = ((step - 1) % STEPS_PER_DAY == 0)
    is_end_day = (step % STEPS_PER_DAY == 0)

    # Time info
    cur_dt = START_DATE + Dates.Second(round(Int, (step - 1) * DTIME))
    mon = Dates.month(cur_dt)
    day_val = Dates.day(cur_dt)
    obliqr = 0.40910518  # ORB_OBLIQR_DEFAULT

    # Run the driver
    CLM.clm_drv!(config, inst, filt, filt_ia, bounds,
                 true,  # doalb
                 nextsw_cday, declinp1, declinp1, obliqr,
                 false, false, "", false;  # rstwr, nlend, rdate, rof_prognostic
                 nstep=step,
                 is_first_step=is_first,
                 is_beg_curr_day=is_beg_day,
                 is_end_curr_day=is_end_day,
                 is_beg_curr_year=(step == 1),
                 dtime=DTIME,
                 mon=mon,
                 day=day_val,
                 photosyns=inst.photosyns)

    # Accumulate daily averages
    global daily_t_grnd  += temp.t_grnd_col[1]
    global daily_tsa     += temp.t_ref2m_patch[1]
    global daily_fsa     += inst.solarabs.fsa_patch[1]
    global daily_eflx_lh += ef.eflx_lh_tot_patch[1]
    global daily_eflx_sh += ef.eflx_sh_tot_patch[1]
    # Compute total snow water (sum over active snow layers)
    c = 1  # column 1
    snl_c = col.snl[c]
    joff = CLM.varpar.nlevsno
    h2osno_val = 0.0
    for j in (snl_c+1):0
        h2osno_val += wsb.ws.h2osoi_liq_col[c, j + joff] + wsb.ws.h2osoi_ice_col[c, j + joff]
    end
    h2osno_val += wsb.ws.h2osno_no_layers_col[c]  # unresolved snow
    global daily_h2osno  += h2osno_val
    global daily_qrunoff += wfb.wf.qflx_runoff_col[1]
    global daily_snow_depth += inst.water.waterdiagnosticbulk_inst.snow_depth_col[1]
    global daily_frac_sno += inst.water.waterdiagnosticbulk_inst.frac_sno_col[1]

    # At end of day, write averages
    if is_end_day
        n = STEPS_PER_DAY
        global daily_t_grnd  /= n
        global daily_tsa     /= n
        global daily_fsa     /= n
        global daily_eflx_lh /= n
        global daily_eflx_sh /= n
        global daily_h2osno  /= n
        global daily_qrunoff /= n
        global daily_snow_depth /= n
        global daily_frac_sno /= n

        push!(results, Dict(
            "day" => day_of_sim,
            "T_GRND" => daily_t_grnd,
            "TSA" => daily_tsa,
            "FSA" => daily_fsa,
            "EFLX_LH_TOT" => daily_eflx_lh,
            "EFLX_SH_TOT" => daily_eflx_sh,
            "H2OSNO" => daily_h2osno,
            "QRUNOFF" => daily_qrunoff,
            "SNOW_DEPTH" => daily_snow_depth,
            "FRAC_SNO" => daily_frac_sno,
        ))

        println("  Day $day_of_sim: T_GRND=$(round(daily_t_grnd, digits=2)) K, H2OSNO=$(round(daily_h2osno, digits=4)) kg/m², SNOW_DEPTH=$(round(daily_snow_depth, digits=4)) m")

        # Reset accumulators
        global daily_t_grnd  = 0.0
        global daily_tsa     = 0.0
        global daily_fsa     = 0.0
        global daily_eflx_lh = 0.0
        global daily_eflx_sh = 0.0
        global daily_h2osno  = 0.0
        global daily_qrunoff = 0.0
        global daily_snow_depth = 0.0
        global daily_frac_sno = 0.0
    end

    # Print progress every 10 days
    if step % (10 * STEPS_PER_DAY) == 0
        println("  ... completed $step / $total_steps timesteps")
    end
end

CLM.forcing_reader_close!(fr)

# ---------------------------------------------------------------------------
# Write results
# ---------------------------------------------------------------------------

println("=== Writing trajectory data ===")

# Write daily averages as CSV
open(joinpath(OUTDIR, "daily_avg.csv"), "w") do io
    println(io, "day,T_GRND,TSA,FSA,EFLX_LH_TOT,EFLX_SH_TOT,H2OSNO,QRUNOFF,SNOW_DEPTH,FRAC_SNO")
    for r in results
        @printf(io, "%d,%.8e,%.8e,%.8e,%.8e,%.8e,%.8e,%.8e,%.8e,%.8e\n",
                r["day"], r["T_GRND"], r["TSA"], r["FSA"],
                r["EFLX_LH_TOT"], r["EFLX_SH_TOT"], r["H2OSNO"],
                r["QRUNOFF"], r["SNOW_DEPTH"], r["FRAC_SNO"])
    end
end

# Write end-of-simulation state for comparison
write_binary(joinpath(OUTDIR, "t_soisno_final.bin"), temp.t_soisno_col)
write_binary(joinpath(OUTDIR, "t_grnd_final.bin"), temp.t_grnd_col)
write_binary(joinpath(OUTDIR, "h2osoi_liq_final.bin"), wsb.ws.h2osoi_liq_col)
write_binary(joinpath(OUTDIR, "h2osoi_ice_final.bin"), wsb.ws.h2osoi_ice_col)
write_binary(joinpath(OUTDIR, "h2osno_no_layers_final.bin"), wsb.ws.h2osno_no_layers_col)
write_binary(joinpath(OUTDIR, "snow_depth_final.bin"), inst.water.waterdiagnosticbulk_inst.snow_depth_col)

# Metadata
meta = Dict(
    "ndays" => NDAYS,
    "steps_per_day" => STEPS_PER_DAY,
    "dtime" => DTIME,
    "nc" => nc,
    "np" => np,
    "ng" => ng,
    "year" => YEAR,
)
open(joinpath(OUTDIR, "metadata.json"), "w") do io
    JSON.print(io, meta, 2)
end

println("=== Done! Output in: $OUTDIR ===")
