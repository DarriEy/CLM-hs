#!/usr/bin/env julia
# Dump per-patch canopy flux internals for the first few steps, to compare
# directly against the Haskell pipeline trace. Run from CLM.jl project:
#   julia --project=. /path/to/CLM-hs/scripts/dump_canopy_steps.jl
using Dates, Printf
using CLM

const DATA_DIR = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped"
const FSURDAT   = joinpath(DATA_DIR, "settings/CLM/parameters/surfdata_clm.nc")
const PARAMFILE = joinpath(DATA_DIR, "settings/CLM/parameters/clm5_params.nc")
const FORCING_DIR = joinpath(DATA_DIR, "data/forcing/CLM_input")
const FSNOWOPTICS = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_optics_5bnd_c013122.nc"
const FSNOWAGING  = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc"
const YEAR = 2002
const START_DATE = DateTime(YEAR, 1, 1)
const DTIME = 1800.0
const STEPS_PER_DAY = Int(86400 / DTIME)
const INT_SNOW_MAX = 3113.2
const NSTEPS = 4

(inst, bounds, filt, tm) = CLM.clm_initialize!(;
    fsurdat = FSURDAT, paramfile = PARAMFILE, start_date = START_DATE,
    dtime = Int(DTIME), use_cn = false, use_aquifer_layer = false,
    fsnowoptics = FSNOWOPTICS, fsnowaging = FSNOWAGING, int_snow_max = INT_SNOW_MAX)

col = inst.column; pch = inst.patch; lun = inst.landunit; grc = inst.gridcell
temp = inst.temperature; a2l = inst.atm2lnd; ef = inst.energyflux
fv = inst.frictionvel
wsb = inst.water.waterstatebulk_inst; wfb = inst.water.waterfluxbulk_inst
nc = bounds.endc - bounds.begc + 1
np = bounds.endp - bounds.begp + 1
ng = bounds.endg - bounds.begg + 1

config = CLM.CLMDriverConfig(use_cn=false, use_aquifer_layer=false, irrigate=false)
filt_ia = filt
fr = CLM.ForcingReader()
CLM.forcing_reader_init!(fr, joinpath(FORCING_DIR, "clmforc.$YEAR.nc"))

println("np=$np  nc=$nc  ng=$ng")
println("patch wtgcell = ", [round(pch.wtgcell[p], digits=4) for p in bounds.begp:bounds.endp])
println("patch itype   = ", [pch.itype[p] for p in bounds.begp:bounds.endp])
println("patch active  = ", [pch.active[p] for p in bounds.begp:bounds.endp])

for step in 1:NSTEPS
    target_time = fr.times[step]
    calday = Dates.dayofyear(START_DATE) + (step - 1) * DTIME / 86400.0
    CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
    CLM.downscale_forcings!(bounds, a2l, col, lun, inst.topo)
    (declinp1, eccf) = CLM.compute_orbital(calday)
    cur_dt = START_DATE + Dates.Second(round(Int, (step - 1) * DTIME))
    CLM.clm_drv!(config, inst, filt, filt_ia, bounds, true,
                 calday, declinp1, declinp1, 0.40910518,
                 false, false, "", false;
                 nstep=step, is_first_step=(step==1),
                 is_beg_curr_day=((step-1) % STEPS_PER_DAY == 0),
                 is_end_curr_day=(step % STEPS_PER_DAY == 0),
                 is_beg_curr_year=(step==1), dtime=DTIME,
                 mon=Dates.month(cur_dt), day=Dates.day(cur_dt),
                 photosyns=inst.photosyns)

    sab = inst.solarabs
    @printf("\n=== step %d  t_grnd=%.4f forc_q=%.6g forc_t=%.4f forc_lw=%.3f coszen=%.4f forc_wind=%.4f ===\n",
            step, temp.t_grnd_col[1], a2l.forc_q_downscaled_col[1], a2l.forc_t_downscaled_col[1],
            a2l.forc_lwrad_downscaled_col[1], inst.surfalb.coszen_col[1], a2l.forc_u_grc[1])
    @printf("  REPORTED[1]: SH_tot=%.4f  LH_tot=%.4f\n",
            ef.eflx_sh_tot_patch[1], ef.eflx_lh_tot_patch[1])
    @printf("  z0mg_col=%.6g frac_sno=%.4f\n", fv.z0mg_col[1], inst.water.waterdiagnosticbulk_inst.frac_sno_eff_col[1])
    for p in bounds.begp:bounds.endp
        @printf("  p%d wt=%.3f SHveg=%.3f SHgrnd=%.3f tveg=%.3f taf=%.3f ustar=%.4f um=%.4f uaf=%.4f rah1=%.3f rah2=%.3f ram1=%.3f dlrad=%.3f\n",
            p, pch.wtgcell[p], ef.eflx_sh_veg_patch[p], ef.eflx_sh_grnd_patch[p],
            temp.t_veg_patch[p], fv.taf_patch[p], fv.ustar_patch[p], fv.um_patch[p],
            fv.uaf_patch[p], fv.rah1_patch[p], fv.rah2_patch[p], fv.ram1_patch[p], ef.dlrad_patch[p])
    end
end
CLM.forcing_reader_close!(fr)
