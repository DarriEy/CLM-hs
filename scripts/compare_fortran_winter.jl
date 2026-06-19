#!/usr/bin/env julia
# Run Julia CLM for 2003 January and emit BOTH the patch-1 value (what the test
# reference CSV uses) and the gridcell-weighted mean, to compare against the
# Fortran h0 daily history (clm_parity_run, 2003). Decides whether Julia matches
# Fortran in the winter tree-canopy regime.
#   julia --project=. /path/to/CLM-hs/scripts/compare_fortran_winter.jl
using Dates, Printf
using CLM

const DATA_DIR = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped"
const FSURDAT   = joinpath(DATA_DIR, "settings/CLM/parameters/surfdata_clm.nc")
const PARAMFILE = joinpath(DATA_DIR, "settings/CLM/parameters/clm5_params.nc")
const FORCING_DIR = joinpath(DATA_DIR, "data/forcing/CLM_input")
const FSNOWOPTICS = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_optics_5bnd_c013122.nc"
const FSNOWAGING  = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc"
const YEAR = 2003
const START_DATE = DateTime(YEAR, 1, 1)
const DTIME = 1800.0
const STEPS_PER_DAY = Int(86400 / DTIME)
const INT_SNOW_MAX = 3113.2
const NDAYS = 10

(inst, bounds, filt, tm) = CLM.clm_initialize!(;
    fsurdat = FSURDAT, paramfile = PARAMFILE, start_date = START_DATE,
    dtime = Int(DTIME), use_cn = false, use_aquifer_layer = false,
    fsnowoptics = FSNOWOPTICS, fsnowaging = FSNOWAGING, int_snow_max = INT_SNOW_MAX)

col = inst.column; pch = inst.patch; lun = inst.landunit; grc = inst.gridcell
temp = inst.temperature; a2l = inst.atm2lnd; ef = inst.energyflux
nc = bounds.endc - bounds.begc + 1
np = bounds.endp - bounds.begp + 1
ng = bounds.endg - bounds.begg + 1

config = CLM.CLMDriverConfig(use_cn=false, use_aquifer_layer=false, irrigate=false)
filt_ia = filt
fr = CLM.ForcingReader()
CLM.forcing_reader_init!(fr, joinpath(FORCING_DIR, "clmforc.$YEAR.nc"))

wts = [pch.wtgcell[p] for p in bounds.begp:bounds.endp]
@printf("Julia %d, np=%d, patch wts=%s\n", YEAR, np, wts)
println("day, SH_p1, SH_wmean, SHV_wmean, SHG_wmean, LH_wmean, TG, TV")

d_sh1=0.0; d_shw=0.0; d_shvw=0.0; d_shgw=0.0; d_lhw=0.0; d_tg=0.0; d_tv=0.0
for step in 1:(NDAYS*STEPS_PER_DAY)
    target_time = fr.times[step]
    calday = Dates.dayofyear(START_DATE) + (step - 1) * DTIME / 86400.0
    CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
    CLM.downscale_forcings!(bounds, a2l, col, lun, inst.topo)
    (declinp1, eccf) = CLM.compute_orbital(calday)
    cur_dt = START_DATE + Dates.Second(round(Int, (step - 1) * DTIME))
    CLM.clm_drv!(config, inst, filt, filt_ia, bounds, true,
                 calday, declinp1, declinp1, 0.40910518, false, false, "", false;
                 nstep=step, is_first_step=(step==1),
                 is_beg_curr_day=((step-1) % STEPS_PER_DAY == 0),
                 is_end_curr_day=(step % STEPS_PER_DAY == 0),
                 is_beg_curr_year=(step==1), dtime=DTIME,
                 mon=Dates.month(cur_dt), day=Dates.day(cur_dt),
                 photosyns=inst.photosyns)

    wsh  = sum(pch.wtgcell[p]*ef.eflx_sh_tot_patch[p] for p in bounds.begp:bounds.endp)
    wshv = sum(pch.wtgcell[p]*ef.eflx_sh_veg_patch[p] for p in bounds.begp:bounds.endp)
    wshg = sum(pch.wtgcell[p]*ef.eflx_sh_grnd_patch[p] for p in bounds.begp:bounds.endp)
    wlh  = sum(pch.wtgcell[p]*ef.eflx_lh_tot_patch[p] for p in bounds.begp:bounds.endp)
    global d_sh1  += ef.eflx_sh_tot_patch[1]
    global d_shw  += wsh; global d_shvw += wshv; global d_shgw += wshg; global d_lhw += wlh
    global d_tg += temp.t_grnd_col[1]; global d_tv += temp.t_veg_patch[1]
    if step % STEPS_PER_DAY == 0
        n = STEPS_PER_DAY; dy = div(step, STEPS_PER_DAY)
        @printf("%d, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f, %.3f\n",
            dy, d_sh1/n, d_shw/n, d_shvw/n, d_shgw/n, d_lhw/n, d_tg/n, d_tv/n)
        global d_sh1=0.0; global d_shw=0.0; global d_shvw=0.0; global d_shgw=0.0
        global d_lhw=0.0; global d_tg=0.0; global d_tv=0.0
    end
end
CLM.forcing_reader_close!(fr)
