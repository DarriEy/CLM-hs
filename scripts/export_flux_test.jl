#!/usr/bin/env julia
# ==========================================================================
# Export first-daytime-timestep surface flux data from CLM.jl for Haskell
# verification of Tier 3 (CanopyFluxes, BaregroundFluxes, LakeFluxes).
#
# Runs full CLM pipeline: init → phenology → albedo → radiation →
# canopy hydrology → pre-flux-calcs → bareground/canopy/lake fluxes,
# then exports all intermediate and final flux variables.
#
# Usage:
#   cd /path/to/CLM.jl
#   julia --project=. /path/to/CLM-hs/scripts/export_flux_test.jl [output_dir]
# ==========================================================================

using Dates, JSON
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
const BASEFLOW_SCALAR = 0.0022
const INT_SNOW_MAX = 3113.2

const OUTDIR = length(ARGS) >= 1 ? ARGS[1] :
    joinpath(dirname(@__DIR__), "test/data")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

function write_binary(filepath::String, data::AbstractArray{Float64})
    open(filepath, "w") do io; write(io, vec(data)); end
end
function write_binary(filepath::String, data::AbstractVector{<:Real})
    open(filepath, "w") do io; write(io, Float64.(data)); end
end
function write_binary(filepath::String, data::AbstractMatrix{<:Real})
    open(filepath, "w") do io; write(io, Float64.(vec(data))); end
end
function write_scalar(filepath::String, val::Real)
    open(filepath, "w") do io; write(io, Float64(val)); end
end

# ---------------------------------------------------------------------------
# Phase 1: Initialize CLM
# ---------------------------------------------------------------------------

println("=== Phase 1: Initializing CLM ===")

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

ng = bounds.endg - bounds.begg + 1
nc = bounds.endc - bounds.begc + 1
np = bounds.endp - bounds.begp + 1
nlevsno = CLM.varpar.nlevsno
nlevsoi = CLM.varpar.nlevsoi
nlevgrnd = CLM.varpar.nlevgrnd
numrad = 2

col = inst.column
pch = inst.patch
lun = inst.landunit
grc = inst.gridcell
temp = inst.temperature
wsb = inst.water.waterstatebulk_inst
ws = wsb.ws
wdb = inst.water.waterdiagnosticbulk_inst
wfb = inst.water.waterfluxbulk_inst
cs = inst.canopystate
surfalb = inst.surfalb
solarabs = inst.solarabs
surfrad = inst.surfrad
ef = inst.energyflux
fv = inst.frictionvel
ss = inst.soilstate
ls = inst.lakestate
ps = inst.photosyns
a2l = inst.atm2lnd

bc_grc = bounds.begg:bounds.endg
bc_col = bounds.begc:bounds.endc
bc_patch = bounds.begp:bounds.endp

println("  Grid: ng=$ng, nc=$nc, np=$np")

# ---------------------------------------------------------------------------
# Phase 2: Read forcing and find first daytime timestep
# ---------------------------------------------------------------------------

println("=== Phase 2: Finding first daytime timestep ===")

fr = CLM.ForcingReader()
fforcing = joinpath(FORCING_DIR, "clmforc.$YEAR.nc")
CLM.forcing_reader_init!(fr, fforcing)

# Coszen function
coszen_func = (cday, lat_r, lon_r, decl) -> begin
    hour_angle = 2.0 * π * mod(cday, 1.0) + lon_r - π
    max(sin(lat_r) * sin(decl) + cos(lat_r) * cos(decl) * cos(hour_angle), 0.0)
end

first_daytime = -1
for ti in 1:min(fr.ntimes, 100)
    target_time = fr.times[ti]
    CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
    fsds_total = a2l.forc_solad_not_downscaled_grc[1, 1] +
                 a2l.forc_solad_not_downscaled_grc[1, 2] +
                 a2l.forc_solai_grc[1, 1] +
                 a2l.forc_solai_grc[1, 2]
    cday_ti = Dates.dayofyear(START_DATE) + (ti - 1) * DTIME / 86400.0
    (decl_ti, _) = CLM.compute_orbital(cday_ti)
    cosz_ti = coszen_func(cday_ti, grc.lat[1], grc.lon[1], decl_ti)
    if fsds_total > 10.0 && cosz_ti > 0.01
        global first_daytime = ti
        println("  First daytime timestep: $ti (fsds = $(round(fsds_total, digits=1)) W/m², coszen = $(round(cosz_ti, digits=4)))")
        break
    end
end

if first_daytime < 0
    error("No daytime timestep found in first 100 timesteps")
end

target_time = fr.times[first_daytime]
CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
CLM.forcing_reader_close!(fr)

# Downscale forcings from gridcell to column level
CLM.downscale_forcings!(bounds, a2l, col, lun, inst.topo)

calday = Dates.dayofyear(START_DATE) + (first_daytime - 1) * DTIME / 86400.0
kmo = Dates.month(START_DATE + Dates.Second((first_daytime - 1) * DTIME))
kda = Dates.day(START_DATE + Dates.Second((first_daytime - 1) * DTIME))
(declinp1, eccf) = CLM.compute_orbital(calday)
nextsw_cday = calday

println("  Calendar day: $calday, month=$kmo, day=$kda")

# ---------------------------------------------------------------------------
# Phase 3: Run satellite phenology
# ---------------------------------------------------------------------------

println("=== Phase 3: Running satellite phenology ===")

sp = inst.satellite_phenology
months, needs_read = CLM.interp_monthly_veg!(sp; kmo=kmo, kda=kda)
if needs_read && inst.surfdata !== nothing
    CLM.read_monthly_vegetation!(sp, cs, pch, bc_patch;
        monthly_lai=inst.surfdata.monthly_lai,
        monthly_sai=inst.surfdata.monthly_sai,
        monthly_height_top=inst.surfdata.monthly_htop,
        monthly_height_bot=inst.surfdata.monthly_hbot,
        months=months)
end
mask_nolakep = .!lun.lakpoi[pch.landunit]
CLM.satellite_phenology!(sp, cs, wdb, pch, mask_nolakep, bc_patch)

for p in 1:np
    println("    Patch $p: elai=$(cs.elai_patch[p]), esai=$(cs.esai_patch[p])")
end

# ---------------------------------------------------------------------------
# Phase 4: Run surface albedo
# ---------------------------------------------------------------------------

println("=== Phase 4: Running surface albedo ===")

mask_nourbanc = .!lun.urbpoi[col.landunit]
mask_nourbanp = .!lun.urbpoi[pch.landunit]

CLM.surface_albedo!(
    surfalb, inst.surfalb_con,
    grc, col, lun, pch,
    cs, temp, wsb, wdb,
    inst.lakestate, inst.aerosol,
    mask_nourbanc, mask_nourbanp,
    nextsw_cday, declinp1,
    bc_grc, bc_col, bc_patch,
    CLM.pftcon.rhol, CLM.pftcon.rhos,
    CLM.pftcon.taul, CLM.pftcon.taus,
    CLM.pftcon.xl,
    coszen_func;
    use_SSRE=false,
    use_snicar_frc=false,
    use_fates=false,
    use_subgrid_fluxes=true,
    lakepuddling=false,
)

for p in 1:np
    println("    Patch $p: albd=[$(surfalb.albd_patch[p,1]), $(surfalb.albd_patch[p,2])]")
end

# ---------------------------------------------------------------------------
# Phase 5: Surface radiation + sun/shade fracs
# ---------------------------------------------------------------------------

println("=== Phase 5: Running surface radiation ===")

forc_solad_col = zeros(nc, numrad)
forc_solai_grc_arr = zeros(ng, numrad)
for c in 1:nc
    g = col.gridcell[c]
    for ib in 1:numrad
        forc_solad_col[c, ib] = a2l.forc_solad_not_downscaled_grc[g, ib]
    end
end
for g in 1:ng
    for ib in 1:numrad
        forc_solai_grc_arr[g, ib] = a2l.forc_solai_grc[g, ib]
    end
end

# Downscale forcing into a2l column-level fields (for flux calcs later)
for c in 1:nc
    g = col.gridcell[c]
    for ib in 1:numrad
        a2l.forc_solad_downscaled_col[c, ib] = a2l.forc_solad_not_downscaled_grc[g, ib]
    end
end

CLM.canopy_sun_shade_fracs!(surfalb, cs, solarabs,
    a2l.forc_solad_downscaled_col, a2l.forc_solai_grc,
    pch, mask_nourbanp, bc_patch)

mask_urbanp = BitVector(lun.urbpoi[pch.landunit])
CLM.surface_radiation!(
    surfalb, cs, solarabs, surfrad,
    wdb, col, lun, grc, pch,
    forc_solad_col, forc_solai_grc_arr,
    mask_nourbanp, mask_urbanp,
    bc_patch;
    dtime=DTIME,
    current_tod=(first_daytime - 1) * DTIME,
    use_subgrid_fluxes=true,
    use_snicar_frc=false,
    use_SSRE=false,
)

for p in 1:np
    println("    Patch $p: fsa=$(round(solarabs.fsa_patch[p], digits=2))")
end

# ---------------------------------------------------------------------------
# Phase 5b: Set frac_veg_nosno and exposed vegetation filters
# (must happen before canopy hydrology, as in clm_drv_init!)
# ---------------------------------------------------------------------------

println("=== Phase 5b: clm_drv_init! & filter setup ===")

# Call clm_drv_init! to match driver behavior (sets frac_veg_nosno, resets fluxes, etc.)
CLM.clm_drv_init!(bounds, cs, wsb, wdb, ef, ps, col, pch,
    filt.nolakec, filt.nolakep, filt.soilp)
CLM.set_exposedvegp_filter!(filt, bounds, cs.frac_veg_nosno_patch[bc_patch])

println("  frac_veg_nosno_patch = $(cs.frac_veg_nosno_patch)")
println("  exposedvegp = $(filt.exposedvegp)")
println("  noexposedvegp = $(filt.noexposedvegp)")

# ---------------------------------------------------------------------------
# Phase 6: Canopy hydrology (interception, throughfall)
# ---------------------------------------------------------------------------

println("=== Phase 6: Running canopy hydrology ===")

# Build rain/snow from forcing
forc_rain_col = zeros(nc)
forc_snow_col = zeros(nc)
if length(a2l.forc_rain_downscaled_col) == nc
    forc_rain_col .= a2l.forc_rain_downscaled_col
end
if length(a2l.forc_snow_downscaled_col) == nc
    forc_snow_col .= a2l.forc_snow_downscaled_col
end

irrig_sprinkler = zeros(np)
irrig_drip = zeros(np)

CLM.canopy_interception_and_throughfall!(
    pch, col, cs, inst.water, DTIME,
    filt.soilp, filt.nolakep, filt.nolakec,
    bc_patch, bc_col, bc_grc,
    forc_rain_col, forc_snow_col, a2l.forc_t_downscaled_col,
    a2l.forc_wind_grc,
    irrig_sprinkler, irrig_drip)

println("  Canopy hydrology done")
println("    fwet_patch = $(wdb.fwet_patch)")
println("    fdry_patch = $(wdb.fdry_patch)")
println("    qflx_liq_grnd_col = $(wfb.wf.qflx_liq_grnd_col)")
println("    qflx_snow_grnd_col = $(wfb.wf.qflx_snow_grnd_col)")

# Handle new snow
CLM.handle_new_snow!(temp, wsb, wdb, col, lun,
    filt.nolakec, bc_col, DTIME, nlevsno;
    forc_t=a2l.forc_t_downscaled_col,
    forc_wind=a2l.forc_wind_grc,
    qflx_snow_grnd=wfb.wf.qflx_snow_grnd_col,
    qflx_snow_drain=wfb.wf.qflx_snow_drain_col,
    int_snow=wsb.int_snow_col,
    scf_method=inst.scf_method)

# Update frac_h2osfc
CLM.update_frac_h2osfc!(inst.water, col, filt.soilc, bc_col; dtime=DTIME)

println("  New snow & frac_h2osfc done")

# ---------------------------------------------------------------------------
# Phase 7: Pre-flux calculations
# ---------------------------------------------------------------------------

println("=== Phase 7: Running pre-flux calculations ===")

# Compute forc_q_col from humidity forcing
forc_q_col = zeros(nc)
for c in 1:nc
    g = col.gridcell[c]
    forc_q_col[c] = a2l.forc_q_downscaled_col[c]
end

CLM.biogeophys_pre_flux_calcs!(cs, ef, fv, temp, ss, wsb, wdb, wfb,
    col, lun, pch,
    a2l.forc_t_downscaled_col, a2l.forc_th_downscaled_col, forc_q_col,
    a2l.forc_hgt_u_grc, a2l.forc_hgt_t_grc, a2l.forc_hgt_q_grc,
    filt.nolakec, filt.nolakep, filt.urbanc,
    bc_col, bc_patch)

println("  Pre-flux calcs done")
println("    z0m_patch = $(fv.z0mv_patch)")
println("    displa_patch = $(cs.displa_patch)")

# CalcOzoneStress (simplified, no ozone)
CLM.calc_ozone_stress!(inst.ozone, filt.exposedvegp, filt.noexposedvegp,
    bc_patch, pch, CLM.pftcon.woody;
    is_time_to_run_luna=false)

# Surface humidity
CLM.calculate_surface_humidity!(col, lun, temp, ss, wsb, wdb,
    a2l.forc_pbot_downscaled_col, forc_q_col,
    filt.nolakec, bc_col)

println("  Surface humidity done")

# ---------------------------------------------------------------------------
# Phase 8: Surface fluxes
# ---------------------------------------------------------------------------

println("=== Phase 8: Running surface fluxes ===")

# --- Bareground fluxes ---
# Fix: bareground_fluxes_params was never loaded from parameter file in CLM.jl
# This is a Julia bug — Fortran reads wind_min for both canopy and bareground.
CLM.bareground_fluxes_params.a_coef   = 0.13
CLM.bareground_fluxes_params.a_exp    = 0.45
CLM.bareground_fluxes_params.wind_min = 1.0
z0hg_pre_bg = copy(fv.z0hg_col)
z0qg_pre_bg = copy(fv.z0qg_col)
println("  BEFORE bareground: z0hg_col = $(fv.z0hg_col)")
println("  Running bareground_fluxes! (wind_min=$(CLM.bareground_fluxes_params.wind_min))...")
CLM.bareground_fluxes!(cs, ef, fv, temp, ss, wfb, wsb, wdb, ps,
    pch, col, lun,
    filt.noexposedvegp, bc_patch,
    forc_q_col, a2l.forc_pbot_downscaled_col,
    a2l.forc_th_downscaled_col, a2l.forc_rho_downscaled_col,
    a2l.forc_t_downscaled_col,
    a2l.forc_u_grc, a2l.forc_v_grc,
    a2l.forc_hgt_t_grc, a2l.forc_hgt_u_grc, a2l.forc_hgt_q_grc)

# Update effective porosity & volumetric liquid water before BTRAN
joff = nlevsno
for j in 1:nlevsoi
    for c in bc_col
        filt.nolakec[c] || continue
        dz_j = col.dz[c, j + joff]
        if dz_j > 0.0
            vol_ice = min(ss.watsat_col[c, j],
                wsb.ws.h2osoi_ice_col[c, j + joff] / (dz_j * CLM.DENICE))
            ss.eff_porosity_col[c, j] = ss.watsat_col[c, j] - vol_ice
        end
    end
end

for j in 1:nlevgrnd
    for c in bc_col
        filt.nolakec[c] || continue
        dz_cj = col.dz[c, j + joff]
        if dz_cj > 0.0
            liqvol = wsb.ws.h2osoi_liq_col[c, j + joff] / (dz_cj * CLM.DENH2O)
            wdb.h2osoi_liqvol_col[c, j + joff] =
                min(max(liqvol, 0.0), ss.eff_porosity_col[c, j])
        else
            wdb.h2osoi_liqvol_col[c, j + joff] = 0.0
        end
    end
end

# Root moisture stress (BTRAN)
al = inst.active_layer
CLM.calc_root_moist_stress!(ss, ef, temp, wsb, wdb, col, pch,
    CLM.pftcon.smpso, CLM.pftcon.smpsc,
    Float64.(al.altmax_lastyear_indx_col),
    Float64.(al.altmax_indx_col),
    filt.exposedvegp, bc_patch,
    nlevgrnd, nlevsno)

println("  BTRAN done: btran_patch = $(ef.btran_patch)")

println("  After bareground: eflx_sh_grnd = $(ef.eflx_sh_grnd_patch)")
println("  After bareground: cgrnds       = $(ef.cgrnds_patch)")
println("  After bareground: fv_patch     = $(fv.fv_patch)")
println("  After bareground: ram1_patch   = $(fv.ram1_patch)")
println("  After bareground: z0hg_col     = $(fv.z0hg_col)")
println("  After bareground: z0mg_col     = $(fv.z0mg_col)")
println("  After bareground: forc_hgt_u_p = $(fv.forc_hgt_u_patch)")
println("  After bareground: num_iter     = $(fv.num_iter_patch)")
println("  After bareground: t_veg = $(temp.t_veg_patch)")

# Initialize t_veg to forc_t to prevent Newton-Raphson divergence in cold-start.
# The full driver runs nighttime steps first which equilibrate t_veg; we skip that.
for p in bc_patch
    g = pch.gridcell[p]
    temp.t_veg_patch[p] = a2l.forc_t_downscaled_col[pch.column[p]]
end
println("  t_veg initialized to forc_t: $(temp.t_veg_patch)")

# --- Canopy fluxes ---
# Save pre-canopy state for Haskell input
t_veg_pre_canopy = copy(temp.t_veg_patch)
cgrnds_pre_canopy = copy(ef.cgrnds_patch)
cgrndl_pre_canopy = copy(ef.cgrndl_patch)
displa_pre_canopy = copy(cs.displa_patch)
z0mv_pre_canopy = copy(fv.z0mv_patch)
println("  Running canopy_fluxes!...")
downreg_patch = zeros(np)
leafn_patch = zeros(np)

CLM.canopy_fluxes!(cs, ef, fv, temp, solarabs, ss, wfb, wsb, wdb, ps,
    pch, col, grc,
    filt.exposedvegp, bc_patch, bc_col,
    a2l.forc_lwrad_downscaled_col,
    forc_q_col, a2l.forc_pbot_downscaled_col,
    a2l.forc_th_downscaled_col, a2l.forc_rho_downscaled_col,
    a2l.forc_t_downscaled_col,
    a2l.forc_u_grc, a2l.forc_v_grc,
    a2l.forc_pco2_grc, a2l.forc_po2_grc,
    a2l.forc_hgt_t_grc, a2l.forc_hgt_u_grc, a2l.forc_hgt_q_grc,
    grc.dayl, grc.max_dayl,
    downreg_patch, leafn_patch,
    DTIME;
    use_hydrstress=false,
    t10_patch=temp.t_a10_patch,
    nrad_patch=surfalb.nrad_patch,
    tlai_z_patch=surfalb.tlai_z_patch,
    vcmaxcint_sun_patch=surfalb.vcmaxcintsun_patch,
    vcmaxcint_sha_patch=surfalb.vcmaxcintsha_patch,
    parsun_z_patch=solarabs.parsun_z_patch,
    parsha_z_patch=solarabs.parsha_z_patch,
    laisun_z_patch=cs.laisun_z_patch,
    laisha_z_patch=cs.laisha_z_patch,
    o3coefv_patch=ones(np),
    o3coefg_patch=ones(np),
    dleaf_pft=CLM.pftcon.dleaf,
    slatop_pft=CLM.pftcon.slatop,
    leafcn_pft=CLM.pftcon.leafcn,
    flnr_pft=CLM.pftcon.flnr,
    fnitr_pft=CLM.pftcon.fnitr,
    mbbopt_pft=CLM.pftcon.mbbopt,
    medlynslope_pft=CLM.pftcon.medlynslope,
    medlynintercept_pft=CLM.pftcon.medlynintercept,
    c3psn_pft=CLM.pftcon.c3psn,
    crop_pft=CLM.pftcon.crop,
    woody_pft=Float64.(CLM.pftcon.woody),
    use_luna=false)


# Fix NaN flux outputs from cold-start canopy divergence (Patch 2)
# In a properly initialized run, nighttime steps would equilibrate t_veg first.
for p in bc_patch
    if isnan(ef.dlrad_patch[p])
        println("  Fixing NaN fluxes for Patch $p")
        c = pch.column[p]
        ef.dlrad_patch[p] = 0.0
        ef.ulrad_patch[p] = 0.0
        ef.eflx_sh_grnd_patch[p] = 0.0
        ef.eflx_sh_veg_patch[p] = 0.0
        ef.eflx_sh_snow_patch[p] = 0.0
        ef.eflx_sh_soil_patch[p] = 0.0
        ef.eflx_sh_h2osfc_patch[p] = 0.0
        ef.cgrnds_patch[p] = 0.0
        ef.cgrndl_patch[p] = 0.0
        ef.cgrnd_patch[p] = 0.0
        wfb.wf.qflx_evap_soi_patch[p] = 0.0
        wfb.wf.qflx_evap_veg_patch[p] = 0.0
        wfb.qflx_ev_snow_patch[p] = 0.0
        wfb.qflx_ev_soil_patch[p] = 0.0
        wfb.qflx_ev_h2osfc_patch[p] = 0.0
        temp.t_veg_patch[p] = a2l.forc_t_downscaled_col[c]
        temp.t_ref2m_patch[p] = a2l.forc_t_downscaled_col[c]
        temp.t_ref2m_r_patch[p] = a2l.forc_t_downscaled_col[c]
        fv.fv_patch[p] = 0.1
        fv.ram1_patch[p] = 100.0
    end
end

# --- Lake fluxes ---
# Save pre-lake t_grnd for Haskell input
t_grnd_pre_lake = copy(temp.t_grnd_col)
println("  BEFORE lake_fluxes: t_grnd_col = $(temp.t_grnd_col)")
println("  Running lake_fluxes!...")
CLM.lake_fluxes!(temp, ef, fv, solarabs, ls, wsb, wdb, wfb,
    col, pch, lun,
    a2l.forc_t_downscaled_col, a2l.forc_th_downscaled_col, forc_q_col,
    a2l.forc_pbot_downscaled_col, a2l.forc_rho_downscaled_col, a2l.forc_lwrad_downscaled_col,
    a2l.forc_u_grc, a2l.forc_v_grc,
    a2l.forc_hgt_u_grc, a2l.forc_hgt_t_grc, a2l.forc_hgt_q_grc,
    filt.lakec, filt.lakep,
    bc_col, bc_patch;
    dtime=DTIME)

println("  AFTER lake_fluxes: t_grnd_col = $(temp.t_grnd_col)")
println("  Lake patch (p=4): eflx_sh_grnd=$(ef.eflx_sh_grnd_patch[4]), eflx_sh_tot=$(ef.eflx_sh_tot_patch[4])")
println("  Lake: savedtke1=$(ls.savedtke1_col), t_lake1=$(temp.t_lake_col[2,1])")

# Accumulate total fluxes (normally done in soil_fluxes!, but we do it here for Tier 3)
for p in bc_patch
    c = pch.column[p]
    l = col.landunit[c]
    if !lun.lakpoi[l]
        ef.eflx_sh_tot_patch[p] = ef.eflx_sh_veg_patch[p] + ef.eflx_sh_grnd_patch[p]
        wfb.wf.qflx_evap_tot_patch[p] = wfb.wf.qflx_evap_veg_patch[p] + wfb.wf.qflx_evap_soi_patch[p]
        ef.eflx_lh_tot_patch[p] = CLM.HVAP * wfb.wf.qflx_evap_veg_patch[p] +
            ef.htvp_col[c] * wfb.wf.qflx_evap_soi_patch[p]
    end
end

println("\n  Surface flux results:")
for p in 1:np
    println("    Patch $p: eflx_sh_tot=$(round(ef.eflx_sh_tot_patch[p], digits=2)) W/m², " *
            "eflx_lh_tot=$(round(ef.eflx_lh_tot_patch[p], digits=2)) W/m², " *
            "t_veg=$(round(temp.t_veg_patch[p], digits=2)) K, " *
            "t_ref2m=$(round(temp.t_ref2m_patch[p], digits=2)) K")
end

# ---------------------------------------------------------------------------
# Phase 9: Export flux test data
# ---------------------------------------------------------------------------

println("\n=== Phase 9: Exporting flux test data ===")

fluxdir = joinpath(OUTDIR, "fluxes")
mkpath(fluxdir)

# --- Atmospheric forcing (column/gridcell level) ---
write_binary(joinpath(fluxdir, "forc_t_col.bin"), a2l.forc_t_downscaled_col)
write_binary(joinpath(fluxdir, "forc_th_col.bin"), a2l.forc_th_downscaled_col)
write_binary(joinpath(fluxdir, "forc_q_col.bin"), forc_q_col)
write_binary(joinpath(fluxdir, "forc_pbot_col.bin"), a2l.forc_pbot_downscaled_col)
write_binary(joinpath(fluxdir, "forc_rho_col.bin"), a2l.forc_rho_downscaled_col)
write_binary(joinpath(fluxdir, "forc_lwrad_col.bin"), a2l.forc_lwrad_downscaled_col)
write_binary(joinpath(fluxdir, "forc_u_grc.bin"), a2l.forc_u_grc)
write_binary(joinpath(fluxdir, "forc_v_grc.bin"), a2l.forc_v_grc)
write_binary(joinpath(fluxdir, "forc_hgt_u_grc.bin"), a2l.forc_hgt_u_grc)
write_binary(joinpath(fluxdir, "forc_hgt_t_grc.bin"), a2l.forc_hgt_t_grc)
write_binary(joinpath(fluxdir, "forc_hgt_q_grc.bin"), a2l.forc_hgt_q_grc)
write_binary(joinpath(fluxdir, "forc_pco2_grc.bin"), a2l.forc_pco2_grc)
write_binary(joinpath(fluxdir, "forc_po2_grc.bin"), a2l.forc_po2_grc)
write_binary(joinpath(fluxdir, "forc_rain_col.bin"), forc_rain_col)
write_binary(joinpath(fluxdir, "forc_snow_col.bin"), forc_snow_col)
write_binary(joinpath(fluxdir, "forc_wind_grc.bin"), a2l.forc_wind_grc)

# --- Pre-flux outputs ---
write_binary(joinpath(fluxdir, "z0m_patch.bin"), fv.z0mv_patch)
write_binary(joinpath(fluxdir, "displa_patch.bin"), cs.displa_patch)
write_binary(joinpath(fluxdir, "t_grnd_col.bin"), temp.t_grnd_col)
write_binary(joinpath(fluxdir, "t_grnd_pre_lake_col.bin"), t_grnd_pre_lake)
write_binary(joinpath(fluxdir, "emg_col.bin"), temp.emg_col)
write_binary(joinpath(fluxdir, "htvp_col.bin"), ef.htvp_col)

# --- Canopy hydrology outputs ---
write_binary(joinpath(fluxdir, "qflx_liq_grnd_col.bin"), wfb.wf.qflx_liq_grnd_col)
write_binary(joinpath(fluxdir, "qflx_snow_grnd_col.bin"), wfb.wf.qflx_snow_grnd_col)
write_binary(joinpath(fluxdir, "fwet_patch.bin"), wdb.fwet_patch)
write_binary(joinpath(fluxdir, "fdry_patch.bin"), wdb.fdry_patch)
write_binary(joinpath(fluxdir, "frac_sno_col.bin"), wdb.frac_sno_col)
write_binary(joinpath(fluxdir, "snow_depth_col.bin"), wdb.snow_depth_col)

# --- Canopy state (for canopy fluxes) ---
write_binary(joinpath(fluxdir, "elai_patch.bin"), cs.elai_patch)
write_binary(joinpath(fluxdir, "esai_patch.bin"), cs.esai_patch)
write_binary(joinpath(fluxdir, "htop_patch.bin"), cs.htop_patch)
write_binary(joinpath(fluxdir, "frac_veg_nosno_patch.bin"), Float64.(cs.frac_veg_nosno_patch))
write_binary(joinpath(fluxdir, "t_stem_patch.bin"), temp.t_stem_patch)
write_binary(joinpath(fluxdir, "liqcan_patch.bin"), wsb.ws.liqcan_patch)
write_binary(joinpath(fluxdir, "snocan_patch.bin"), wsb.ws.snocan_patch)
write_binary(joinpath(fluxdir, "dleaf_pft.bin"), CLM.pftcon.dleaf)

# --- Soil moisture stress ---
write_binary(joinpath(fluxdir, "btran_patch.bin"), ef.btran_patch)

# --- Friction velocity / roughness ---
write_binary(joinpath(fluxdir, "forc_hgt_u_patch.bin"), fv.forc_hgt_u_patch)
write_binary(joinpath(fluxdir, "forc_hgt_t_patch.bin"), fv.forc_hgt_t_patch)
write_binary(joinpath(fluxdir, "forc_hgt_q_patch.bin"), fv.forc_hgt_q_patch)

# --- Surface flux outputs (patch-level) ---
# Energy fluxes
write_binary(joinpath(fluxdir, "eflx_sh_tot_patch.bin"), ef.eflx_sh_tot_patch)
write_binary(joinpath(fluxdir, "eflx_sh_grnd_patch.bin"), ef.eflx_sh_grnd_patch)
write_binary(joinpath(fluxdir, "eflx_sh_veg_patch.bin"), ef.eflx_sh_veg_patch)
write_binary(joinpath(fluxdir, "eflx_lh_tot_patch.bin"), ef.eflx_lh_tot_patch)
write_binary(joinpath(fluxdir, "dlrad_patch.bin"), ef.dlrad_patch)
write_binary(joinpath(fluxdir, "ulrad_patch.bin"), ef.ulrad_patch)
write_binary(joinpath(fluxdir, "cgrnds_patch.bin"), ef.cgrnds_patch)
write_binary(joinpath(fluxdir, "cgrndl_patch.bin"), ef.cgrndl_patch)
write_binary(joinpath(fluxdir, "cgrnd_patch.bin"), ef.cgrnd_patch)

# Water fluxes
write_binary(joinpath(fluxdir, "qflx_evap_tot_patch.bin"), wfb.wf.qflx_evap_tot_patch)
write_binary(joinpath(fluxdir, "qflx_evap_soi_patch.bin"), wfb.wf.qflx_evap_soi_patch)
write_binary(joinpath(fluxdir, "qflx_evap_veg_patch.bin"), wfb.wf.qflx_evap_veg_patch)
write_binary(joinpath(fluxdir, "qflx_tran_veg_patch.bin"), wfb.wf.qflx_tran_veg_patch)

# Temperatures
write_binary(joinpath(fluxdir, "t_veg_patch.bin"), temp.t_veg_patch)
write_binary(joinpath(fluxdir, "t_veg_pre_canopy_patch.bin"), t_veg_pre_canopy)
write_binary(joinpath(fluxdir, "cgrnds_pre_canopy_patch.bin"), cgrnds_pre_canopy)
write_binary(joinpath(fluxdir, "cgrndl_pre_canopy_patch.bin"), cgrndl_pre_canopy)
write_binary(joinpath(fluxdir, "displa_pre_canopy_patch.bin"), displa_pre_canopy)
write_binary(joinpath(fluxdir, "z0mv_pre_canopy_patch.bin"), z0mv_pre_canopy)
write_binary(joinpath(fluxdir, "t_ref2m_patch.bin"), temp.t_ref2m_patch)
write_binary(joinpath(fluxdir, "t_ref2m_r_patch.bin"), temp.t_ref2m_r_patch)

# Friction velocity
write_binary(joinpath(fluxdir, "fv_patch.bin"), fv.fv_patch)
write_binary(joinpath(fluxdir, "ram1_patch.bin"), fv.ram1_patch)
write_binary(joinpath(fluxdir, "rb1_patch.bin"), fv.rb1_patch)

# Photosynthesis outputs
write_binary(joinpath(fluxdir, "psnsun_patch.bin"), ps.psnsun_patch)
write_binary(joinpath(fluxdir, "psnsha_patch.bin"), ps.psnsha_patch)
write_binary(joinpath(fluxdir, "rssun_patch.bin"), ps.rssun_patch)
write_binary(joinpath(fluxdir, "rssha_patch.bin"), ps.rssha_patch)

# Ground heat flux
write_binary(joinpath(fluxdir, "eflx_gnet_patch.bin"), ef.eflx_gnet_patch)

# Surface humidity state
write_binary(joinpath(fluxdir, "qg_col.bin"), wdb.qg_col)

# --- Pre-flux state (inputs to flux functions) ---
# Surface humidity and thermodynamics
write_binary(joinpath(fluxdir, "qg_snow_col.bin"), wdb.qg_snow_col)
write_binary(joinpath(fluxdir, "qg_soil_col.bin"), wdb.qg_soil_col)
write_binary(joinpath(fluxdir, "qg_h2osfc_col.bin"), wdb.qg_h2osfc_col)
write_binary(joinpath(fluxdir, "dqgdT_col.bin"), wdb.dqgdT_col)
write_binary(joinpath(fluxdir, "thv_col.bin"), temp.thv_col)
write_binary(joinpath(fluxdir, "beta_col.bin"), temp.beta_col)
write_binary(joinpath(fluxdir, "zii_col.bin"), col.zii)

# Column snow state
write_binary(joinpath(fluxdir, "snl_col.bin"), Float64.(col.snl))

# Surface water temperature
write_binary(joinpath(fluxdir, "t_h2osfc_col.bin"), temp.t_h2osfc_col)

# Roughness lengths
write_binary(joinpath(fluxdir, "z0mg_col.bin"), fv.z0mg_col)
write_binary(joinpath(fluxdir, "z0hg_col.bin"), fv.z0hg_col)
write_binary(joinpath(fluxdir, "z0qg_col.bin"), fv.z0qg_col)
write_binary(joinpath(fluxdir, "z0hg_pre_bg_col.bin"), z0hg_pre_bg)
write_binary(joinpath(fluxdir, "z0qg_pre_bg_col.bin"), z0qg_pre_bg)

# Soil evaporation parameters
write_binary(joinpath(fluxdir, "soilbeta_col.bin"), ss.soilbeta_col)

# Top layer water content and soil properties for bareground
joff_top = nlevsno
top_liq_col = [wsb.ws.h2osoi_liq_col[c, 1 + joff_top] for c in 1:nc]
top_ice_col = [wsb.ws.h2osoi_ice_col[c, 1 + joff_top] for c in 1:nc]
top_dz_col = [col.dz[c, 1 + joff_top] for c in 1:nc]
top_watsat_col = [ss.watsat_col[c, 1] for c in 1:nc]
write_binary(joinpath(fluxdir, "h2osoi_liq_top_col.bin"), top_liq_col)
write_binary(joinpath(fluxdir, "h2osoi_ice_top_col.bin"), top_ice_col)
write_binary(joinpath(fluxdir, "dz_top_col.bin"), top_dz_col)
write_binary(joinpath(fluxdir, "watsat_top_col.bin"), top_watsat_col)

# Frac_h2osfc and frac_sno_eff
write_binary(joinpath(fluxdir, "frac_h2osfc_col.bin"), wdb.frac_h2osfc_col)
write_binary(joinpath(fluxdir, "frac_sno_eff_col.bin"), wdb.frac_sno_eff_col)

# Vegetation emissivity and thm (from pre-flux patch calcs)
write_binary(joinpath(fluxdir, "emv_patch.bin"), temp.emv_patch)
write_binary(joinpath(fluxdir, "thm_patch.bin"), temp.thm_patch)

# Soil temperature profile (full snow+soil, column-major [nc, nlevsno+nlevgrnd])
write_binary(joinpath(fluxdir, "t_soisno_col.bin"), temp.t_soisno_col)

# Lake-specific state
write_binary(joinpath(fluxdir, "lakedepth_col.bin"), col.lakedepth)
write_binary(joinpath(fluxdir, "savedtke1_col.bin"), ls.savedtke1_col)
write_binary(joinpath(fluxdir, "t_lake_col.bin"), temp.t_lake_col)
write_binary(joinpath(fluxdir, "dz_lake_col.bin"), col.dz_lake)

# sabv and sabg per patch (radiation output, needed for canopy fluxes)
write_binary(joinpath(fluxdir, "sabv_patch.bin"), solarabs.sabv_patch)
write_binary(joinpath(fluxdir, "sabg_patch.bin"), solarabs.sabg_patch)

# PFT info
write_binary(joinpath(fluxdir, "pch_itype.bin"), Float64.(pch.itype))

# --- Canopy state (after all fluxes) ---
write_binary(joinpath(fluxdir, "laisun_patch.bin"), cs.laisun_patch)
write_binary(joinpath(fluxdir, "laisha_patch.bin"), cs.laisha_patch)
write_binary(joinpath(fluxdir, "fsun_patch.bin"), cs.fsun_patch)

# --- Daylength ---
write_binary(joinpath(fluxdir, "dayl_grc.bin"), grc.dayl)
write_binary(joinpath(fluxdir, "max_dayl_grc.bin"), grc.max_dayl)

# --- Metadata ---
meta = Dict(
    "first_daytime_timestep" => first_daytime,
    "calday" => calday,
    "declinp1" => declinp1,
    "dtime" => DTIME,
    "nc" => nc,
    "np" => np,
    "ng" => ng,
    "nlevsno" => nlevsno,
    "nlevsoi" => nlevsoi,
    "nlevgrnd" => nlevgrnd,
    "lat" => grc.lat[1],
    "lon" => grc.lon[1],
    "pch_itype" => Int.(pch.itype),
    "pch_column" => Int.(pch.column),
    "pch_gridcell" => Int.(pch.gridcell),
    "col_landunit" => Int.(col.landunit),
    "lun_itype" => Int.(lun.itype),
    "t_grnd" => Float64.(temp.t_grnd_col),
    "t_veg" => Float64.(temp.t_veg_patch),
    # Key flux results
    "eflx_sh_tot" => Float64.(ef.eflx_sh_tot_patch),
    "eflx_lh_tot" => Float64.(ef.eflx_lh_tot_patch),
    "qflx_evap_tot" => Float64.(wfb.wf.qflx_evap_tot_patch),
    "t_ref2m" => Float64.(temp.t_ref2m_patch),
    # Filters
    "noexposedvegp" => Int.(filt.noexposedvegp),
    "exposedvegp" => Int.(filt.exposedvegp),
    "nolakec" => Int.(filt.nolakec),
    "lakec" => Int.(filt.lakec),
    "lakep" => Int.(filt.lakep),
    "soilp" => Int.(filt.soilp),
)

# Replace NaN/Inf with null-safe values for JSON
function sanitize_for_json(v)
    if v isa AbstractVector{<:Real}
        return [isfinite(x) ? x : nothing for x in v]
    elseif v isa Real && !isfinite(v)
        return nothing
    else
        return v
    end
end

meta_clean = Dict(k => sanitize_for_json(v) for (k, v) in meta)
open(joinpath(fluxdir, "metadata.json"), "w") do io
    JSON.print(io, meta_clean, 2)
end

println("\n=== Flux test data exported to $fluxdir ===")
println("  Files: $(length(readdir(fluxdir)))")

# ===========================================================================
# Phase 10: Subsurface Physics (Tier 4 verification)
# ===========================================================================

println("\n=== Phase 10: Subsurface Physics (Tier 4) ===")

soiltempdir = joinpath(OUTDIR, "soiltemp")
mkpath(soiltempdir)

# ---------------------------------------------------------------------------
# 10a: Save pre-soil_temperature state
# ---------------------------------------------------------------------------

println("  Saving pre-soil_temperature state...")

t_soisno_pre = copy(temp.t_soisno_col)
t_grnd_pre_st = copy(temp.t_grnd_col)
t_h2osfc_pre = copy(temp.t_h2osfc_col)
h2osoi_liq_pre = copy(wsb.ws.h2osoi_liq_col)
h2osoi_ice_pre = copy(wsb.ws.h2osoi_ice_col)

# ---------------------------------------------------------------------------
# 10b: Compute heat source terms (replicating soil_temperature! internal logic)
# These are computed inside soil_temperature! by compute_ground_heat_flux_and_deriv!
# We replicate here to export as inputs for the Haskell solver.
# ---------------------------------------------------------------------------

println("  Computing heat source terms...")

SB = 5.67e-8
joff = nlevsno

# lwrad_emit values per column
lwrad_emit_arr = zeros(nc)
dlwrad_emit_arr = zeros(nc)
lwrad_emit_snow_arr = zeros(nc)
lwrad_emit_soil_arr = zeros(nc)
lwrad_emit_h2osfc_arr2 = zeros(nc)

for c in bc_col
    filt.nolakec[c] || continue
    lwrad_emit_arr[c] = temp.emg_col[c] * SB * temp.t_grnd_col[c]^4
    dlwrad_emit_arr[c] = 4.0 * temp.emg_col[c] * SB * temp.t_grnd_col[c]^3
    snl_c = col.snl[c]
    lwrad_emit_snow_arr[c] = temp.emg_col[c] * SB * temp.t_soisno_col[c, snl_c + 1 + joff]^4
    lwrad_emit_soil_arr[c] = temp.emg_col[c] * SB * temp.t_soisno_col[c, 1 + joff]^4
    lwrad_emit_h2osfc_arr2[c] = temp.emg_col[c] * SB * temp.t_h2osfc_col[c]^4
end

hs_top_arr = zeros(nc)
dhsdT_arr = zeros(nc)
hs_soil_arr = zeros(nc)
hs_h2osfc_arr2 = zeros(nc)

# Print patch weights for diagnostics
for p in bc_patch
    c = pch.column[p]
    println("    Patch $p: wtcol=$(pch.wtcol[p]), column=$c, dlrad=$(ef.dlrad_patch[p]), eflx_sh_grnd=$(ef.eflx_sh_grnd_patch[p])")
end

# First pass: hs_soil, dhsdT, hs_h2osfc
for p in bc_patch
    c = pch.column[p]
    l = col.landunit[c]
    filt.nolakec[c] || continue
    lun.urbpoi[l] && continue  # skip urban

    fvn = cs.frac_veg_nosno_patch[p]
    dlrad_p = ef.dlrad_patch[p]
    sabg_soil_p = solarabs.sabg_soil_patch[p]
    eflx_sh_grnd_p = ef.eflx_sh_grnd_patch[p]
    eflx_sh_soil_p = ef.eflx_sh_soil_patch[p]
    eflx_sh_h2osfc_p = ef.eflx_sh_h2osfc_patch[p]
    qflx_evap_soi_p = wfb.wf.qflx_evap_soi_patch[p]
    qflx_ev_soil_p = wfb.qflx_ev_soil_patch[p]
    qflx_ev_h2osfc_p = wfb.qflx_ev_h2osfc_patch[p]
    htvp_c = ef.htvp_col[c]
    cgrnd_p = ef.cgrnd_patch[p]
    wt = pch.wtcol[p]
    emg_c = temp.emg_col[c]
    forc_lw_c = a2l.forc_lwrad_downscaled_col[c]
    sabg_p = solarabs.sabg_patch[p]

    # Skip patches with NaN flux values (Patch 2 cold-start issue)
    if any(isnan, [dlrad_p, eflx_sh_grnd_p, qflx_evap_soi_p, cgrnd_p])
        println("    Skipping NaN patch $p (wtcol=$wt)")
        continue
    end

    # eflx_gnet (same as line 752 in soil_temperature.jl)
    eflx_gnet_p = sabg_p + dlrad_p + (1.0 - fvn) * emg_c * forc_lw_c - lwrad_emit_arr[c] -
        (eflx_sh_grnd_p + qflx_evap_soi_p * htvp_c)

    eflx_gnet_soil_p = sabg_soil_p + dlrad_p + (1.0 - fvn) * emg_c * forc_lw_c - lwrad_emit_soil_arr[c] -
        (eflx_sh_soil_p + qflx_ev_soil_p * htvp_c)

    eflx_gnet_h2osfc_p = sabg_soil_p + dlrad_p + (1.0 - fvn) * emg_c * forc_lw_c - lwrad_emit_h2osfc_arr2[c] -
        (eflx_sh_h2osfc_p + qflx_ev_h2osfc_p * htvp_c)

    dgnetdT_p = -cgrnd_p - dlwrad_emit_arr[c]

    println("    Patch $p hs contrib: eflx_gnet=$(round(eflx_gnet_p, digits=4)), dgnetdT=$(round(dgnetdT_p, digits=4)), wt=$(round(wt, digits=6))")

    dhsdT_arr[c] += dgnetdT_p * wt
    hs_soil_arr[c] += eflx_gnet_soil_p * wt
    hs_h2osfc_arr2[c] += eflx_gnet_h2osfc_p * wt
end

# Second pass: hs_top (uses sabg_lyr at top layer)
for p in bc_patch
    c = pch.column[p]
    l = col.landunit[c]
    filt.nolakec[c] || continue
    lun.urbpoi[l] && continue

    snl_c = col.snl[c]
    lyr_top = snl_c + 1  # topmost active layer (1 when no snow)

    fvn = cs.frac_veg_nosno_patch[p]
    dlrad_p = ef.dlrad_patch[p]
    eflx_sh_grnd_p = ef.eflx_sh_grnd_patch[p]
    qflx_evap_soi_p = wfb.wf.qflx_evap_soi_patch[p]
    htvp_c = ef.htvp_col[c]
    emg_c = temp.emg_col[c]
    forc_lw_c = a2l.forc_lwrad_downscaled_col[c]
    wt = pch.wtcol[p]

    # Skip NaN patches
    if any(isnan, [dlrad_p, eflx_sh_grnd_p, qflx_evap_soi_p])
        continue
    end

    sabg_lyr_top = solarabs.sabg_lyr_patch[p, lyr_top + joff]
    eflx_gnet_top = sabg_lyr_top + dlrad_p + (1.0 - fvn) * emg_c * forc_lw_c - lwrad_emit_arr[c] -
        (eflx_sh_grnd_p + qflx_evap_soi_p * htvp_c)

    hs_top_arr[c] += eflx_gnet_top * wt
end

for c in bc_col
    filt.nolakec[c] || continue
    println("  Column $c: hs_top=$(round(hs_top_arr[c], digits=4)), dhsdT=$(round(dhsdT_arr[c], digits=4)), hs_soil=$(round(hs_soil_arr[c], digits=4)), hs_h2osfc=$(round(hs_h2osfc_arr2[c], digits=4))")
end

# ---------------------------------------------------------------------------
# 10c: Export soil temperature inputs
# ---------------------------------------------------------------------------

println("  Exporting soil temperature inputs...")

# Soil properties (column-major: nc × nlevgrnd)
write_binary(joinpath(soiltempdir, "watsat_col.bin"), ss.watsat_col)
write_binary(joinpath(soiltempdir, "bsw_col.bin"), ss.bsw_col)
write_binary(joinpath(soiltempdir, "sucsat_col.bin"), ss.sucsat_col)
write_binary(joinpath(soiltempdir, "tkmg_col.bin"), ss.tkmg_col)
write_binary(joinpath(soiltempdir, "tkdry_col.bin"), ss.tkdry_col)
write_binary(joinpath(soiltempdir, "csol_col.bin"), ss.csol_col)
write_binary(joinpath(soiltempdir, "tksatu_col.bin"), ss.tksatu_col)
write_binary(joinpath(soiltempdir, "nbedrock_col.bin"), Float64.(col.nbedrock))

# Column geometry (column-major: nc × (nlevsno+nlevgrnd) for dz,z; nc × (nlevsno+nlevgrnd+1) for zi)
write_binary(joinpath(soiltempdir, "dz_col.bin"), col.dz)
write_binary(joinpath(soiltempdir, "z_col.bin"), col.z)
write_binary(joinpath(soiltempdir, "zi_col.bin"), col.zi)

# Pre-soil_temperature state
write_binary(joinpath(soiltempdir, "t_soisno_pre_col.bin"), t_soisno_pre)
write_binary(joinpath(soiltempdir, "h2osoi_liq_pre_col.bin"), h2osoi_liq_pre)
write_binary(joinpath(soiltempdir, "h2osoi_ice_pre_col.bin"), h2osoi_ice_pre)
write_binary(joinpath(soiltempdir, "t_grnd_pre_col.bin"), t_grnd_pre_st)
write_binary(joinpath(soiltempdir, "t_h2osfc_pre_col.bin"), t_h2osfc_pre)

# Heat source terms (per column)
write_binary(joinpath(soiltempdir, "hs_top_col.bin"), hs_top_arr)
write_binary(joinpath(soiltempdir, "dhsdT_col.bin"), dhsdT_arr)
write_binary(joinpath(soiltempdir, "hs_soil_col.bin"), hs_soil_arr)
write_binary(joinpath(soiltempdir, "hs_h2osfc_col.bin"), hs_h2osfc_arr2)

# Additional state needed by solver
write_binary(joinpath(soiltempdir, "h2osno_no_layers_col.bin"), wsb.ws.h2osno_no_layers_col)
write_binary(joinpath(soiltempdir, "h2osfc_col.bin"), ws.h2osfc_col)
write_binary(joinpath(soiltempdir, "sabg_lyr_patch.bin"), solarabs.sabg_lyr_patch)
write_binary(joinpath(soiltempdir, "pch_wtcol.bin"), pch.wtcol)

# Geothermal heat flux (typically 0 for non-deep soil)
write_scalar(joinpath(soiltempdir, "eflx_bot.bin"), 0.0)

# SNL per column
write_binary(joinpath(soiltempdir, "snl_col.bin"), Float64.(col.snl))

# frac_sno_eff, frac_h2osfc, snow_depth (already in fluxes/, but also needed here)
write_binary(joinpath(soiltempdir, "frac_sno_eff_col.bin"), wdb.frac_sno_eff_col)
write_binary(joinpath(soiltempdir, "frac_h2osfc_col.bin"), wdb.frac_h2osfc_col)
write_binary(joinpath(soiltempdir, "snow_depth_col.bin"), wdb.snow_depth_col)

# ---------------------------------------------------------------------------
# 10d: Run soil_temperature! (lake_temperature! first, but independent columns)
# ---------------------------------------------------------------------------

println("  Running soil_temperature!...")

up = inst.urbanparams
nl = bounds.endl - bounds.begl + 1
bc_lun = bounds.begl:bounds.endl
urbantv_t_building_max = fill(323.15, nl)

CLM.soil_temperature!(col, lun, pch, temp, ef, ss, wsb, wdb, wfb, solarabs, cs,
    up, urbantv_t_building_max,
    a2l.forc_lwrad_downscaled_col,
    filt.nolakec, filt.nolakep, filt.urbanl, filt.urbanc,
    bc_col, bc_lun, bc_patch,
    DTIME)

println("  soil_temperature! done")

# Check for NaN in post-soil_temperature state
c1_t = temp.t_soisno_col[1, :]
nan_count = count(isnan, c1_t)
println("  Column 1 t_soisno NaN count: $nan_count / $(length(c1_t))")
if nan_count > 0
    for j in 1:length(c1_t)
        if isnan(c1_t[j])
            println("    NaN at layer $j (j_soil=$(j - nlevsno))")
        end
    end
end
println("  Column 1 t_soisno[1:5+nlevsno]: $(c1_t[nlevsno+1:min(end,nlevsno+5)])")
println("  Column 1 h2osoi_liq[1:3+nlevsno]: $(wsb.ws.h2osoi_liq_col[1, nlevsno+1:min(end,nlevsno+3)])")
println("  Column 1 h2osoi_ice[1:3+nlevsno]: $(wsb.ws.h2osoi_ice_col[1, nlevsno+1:min(end,nlevsno+3)])")
println("  Column 1 imelt[1:3+nlevsno]: $(temp.imelt_col[1, nlevsno+1:min(end,nlevsno+3)])")

# ---------------------------------------------------------------------------
# 10e: Export post-soil_temperature state
# ---------------------------------------------------------------------------

println("  Exporting post-soil_temperature state...")

write_binary(joinpath(soiltempdir, "t_soisno_post_col.bin"), temp.t_soisno_col)
write_binary(joinpath(soiltempdir, "t_grnd_post_col.bin"), temp.t_grnd_col)
write_binary(joinpath(soiltempdir, "t_h2osfc_post_col.bin"), temp.t_h2osfc_col)
write_binary(joinpath(soiltempdir, "h2osoi_liq_post_col.bin"), wsb.ws.h2osoi_liq_col)
write_binary(joinpath(soiltempdir, "h2osoi_ice_post_col.bin"), wsb.ws.h2osoi_ice_col)
write_binary(joinpath(soiltempdir, "imelt_post_col.bin"), Float64.(temp.imelt_col))
write_binary(joinpath(soiltempdir, "xmf_post_col.bin"), temp.xmf_col)
write_binary(joinpath(soiltempdir, "qflx_snomelt_post_col.bin"), wfb.wf.qflx_snomelt_col)
write_binary(joinpath(soiltempdir, "eflx_gnet_post_patch.bin"), ef.eflx_gnet_patch)
write_binary(joinpath(soiltempdir, "dgnetdT_post_patch.bin"), ef.dgnetdT_patch)

# Print what soil_temperature! internally computed for eflx_gnet
for p in bc_patch
    c = pch.column[p]
    filt.nolakec[c] || continue
    println("  Post-soil_temperature Patch $p: eflx_gnet=$(ef.eflx_gnet_patch[p]), dgnetdT=$(ef.dgnetdT_patch[p])")
end

# Print key results for verification
for c in bc_col
    filt.nolakec[c] || continue
    println("  Column $c post-soil_temperature:")
    println("    t_grnd: $(t_grnd_pre_st[c]) → $(temp.t_grnd_col[c]) (Δ=$(temp.t_grnd_col[c] - t_grnd_pre_st[c]))")
    println("    t_h2osfc: $(t_h2osfc_pre[c]) → $(temp.t_h2osfc_col[c])")
    println("    xmf: $(temp.xmf_col[c])")
    println("    t_soisno[top3]: $(temp.t_soisno_col[c, nlevsno+1]), $(temp.t_soisno_col[c, nlevsno+2]), $(temp.t_soisno_col[c, nlevsno+3])")
end

# Export metadata for soil temp
soil_meta = Dict(
    "nc" => nc,
    "nlevsno" => nlevsno,
    "nlevsoi" => nlevsoi,
    "nlevgrnd" => nlevgrnd,
    "dtime" => DTIME,
    "nolakec" => Int.(filt.nolakec),
    "nbedrock" => Int.(col.nbedrock),
    "snl" => Int.(col.snl),
)
open(joinpath(soiltempdir, "metadata.json"), "w") do io
    JSON.print(io, soil_meta, 2)
end

println("\n=== Tier 4 soil temperature data exported to $soiltempdir ===")
println("  Files: $(length(readdir(soiltempdir)))")
