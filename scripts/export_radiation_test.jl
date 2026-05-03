#!/usr/bin/env julia
# ==========================================================================
# Export first-daytime-timestep radiation data from CLM.jl for Haskell
# verification of Tier 2 (SurfaceAlbedo + SurfaceRadiation).
#
# Runs CLM.jl initialization + satellite phenology + surface_albedo +
# surface_radiation for the first daytime half-hour, then dumps all
# intermediate and final variables as raw Float64 binary files.
#
# Usage:
#   cd /path/to/CLM.jl
#   julia --project=. /path/to/CLM-hs/scripts/export_radiation_test.jl [output_dir]
# ==========================================================================

using Dates, JSON
using CLM

# ---------------------------------------------------------------------------
# Configuration (same as main export script)
# ---------------------------------------------------------------------------

const DATA_DIR = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped"
const FSURDAT   = joinpath(DATA_DIR, "settings/CLM/parameters/surfdata_clm.nc")
const PARAMFILE = joinpath(DATA_DIR, "settings/CLM/parameters/clm5_params.nc")
const FORCING_DIR = joinpath(DATA_DIR, "data/forcing/CLM_input")
const FSNOWOPTICS = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_optics_5bnd_c013122.nc"
const FSNOWAGING  = "/Users/darri.eythorsson/projects/cesm-inputdata/lnd/clm2/snicardata/snicar_drdt_bst_fit_60_c070416.nc"

const YEAR = 2002
const START_DATE = DateTime(YEAR, 1, 1)
const DTIME = 1800
const BASEFLOW_SCALAR = 0.0022
const INT_SNOW_MAX = 3113.2

const OUTDIR = length(ARGS) >= 1 ? ARGS[1] :
    joinpath(dirname(@__DIR__), "test/data")

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

function write_binary(filepath::String, data::AbstractArray{Float64})
    open(filepath, "w") do io
        write(io, vec(data))
    end
end

function write_binary(filepath::String, data::AbstractVector{<:Real})
    open(filepath, "w") do io
        write(io, Float64.(data))
    end
end

function write_binary(filepath::String, data::AbstractMatrix{<:Real})
    open(filepath, "w") do io
        write(io, Float64.(vec(data)))
    end
end

function write_scalar(filepath::String, val::Real)
    open(filepath, "w") do io
        write(io, Float64(val))
    end
end

# ---------------------------------------------------------------------------
# Phase 1: Initialize CLM
# ---------------------------------------------------------------------------

println("=== Phase 1: Initializing CLM ===")

(inst, bounds, filt, tm) = CLM.clm_initialize!(;
    fsurdat = FSURDAT,
    paramfile = PARAMFILE,
    start_date = START_DATE,
    dtime = DTIME,
    use_cn = false,
    use_aquifer_layer = false,
    use_hydrstress = false,
    use_luna = false,
    use_cndv = false,
    fsnowoptics = FSNOWOPTICS,
    fsnowaging = FSNOWAGING,
    int_snow_max = INT_SNOW_MAX,
)

ng = bounds.endg - bounds.begg + 1
nc = bounds.endc - bounds.begc + 1
np = bounds.endp - bounds.begp + 1
nlevsno = CLM.varpar.nlevsno
numrad = 2

col = inst.column
pch = inst.patch
lun = inst.landunit
grc = inst.gridcell
temp = inst.temperature
wsb = inst.water.waterstatebulk_inst
ws = wsb.ws
cs = inst.canopystate
surfalb = inst.surfalb
solarabs = inst.solarabs
surfrad = inst.surfrad
waterdiag = inst.water.waterdiagnosticbulk_inst

println("  Grid: ng=$ng, nc=$nc, np=$np")

# ---------------------------------------------------------------------------
# Phase 2: Read forcing and find first daytime timestep
# ---------------------------------------------------------------------------

println("=== Phase 2: Finding first daytime timestep ===")

fr = CLM.ForcingReader()
fforcing = joinpath(FORCING_DIR, "clmforc.$YEAR.nc")
CLM.forcing_reader_init!(fr, fforcing)

a2l = inst.atm2lnd
lat = grc.lat[1]
lon = grc.lon[1]

# Scan timesteps for first one with positive coszen AND fsds > 0
coszen_check = (cday, lat_r, lon_r, decl) -> begin
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
    # Also check that coszen is positive at this timestep
    cday_ti = Dates.dayofyear(START_DATE) + (ti - 1) * DTIME / 86400.0
    (decl_ti, _) = CLM.compute_orbital(cday_ti)
    cosz_ti = coszen_check(cday_ti, grc.lat[1], grc.lon[1], decl_ti)
    if fsds_total > 10.0 && cosz_ti > 0.01
        global first_daytime = ti
        println("  First daytime timestep: $ti (fsds = $(round(fsds_total, digits=1)) W/m², coszen = $(round(cosz_ti, digits=4)))")
        break
    end
end

if first_daytime < 0
    error("No daytime timestep found in first 100 timesteps")
end

# Re-read that timestep to populate a2l
target_time = fr.times[first_daytime]
CLM.read_forcing_step!(fr, a2l, target_time, ng, nc)
CLM.forcing_reader_close!(fr)

# ---------------------------------------------------------------------------
# Phase 3: Run satellite phenology (cold-start state)
# ---------------------------------------------------------------------------

println("=== Phase 3: Running satellite phenology ===")

# Compute time for phenology interpolation
calday = Dates.dayofyear(START_DATE) + (first_daytime - 1) * DTIME / 86400.0
kmo = Dates.month(START_DATE + Dates.Second((first_daytime - 1) * DTIME))
kda = Dates.day(START_DATE + Dates.Second((first_daytime - 1) * DTIME))

println("  Calendar day: $calday, month=$kmo, day=$kda")

# Step 1: Interpolate monthly vegetation data
sp = inst.satellite_phenology
bc_patch = bounds.begp:bounds.endp

months, needs_read = CLM.interp_monthly_veg!(sp; kmo=kmo, kda=kda)
if needs_read && inst.surfdata !== nothing
    CLM.read_monthly_vegetation!(sp, cs, pch, bc_patch;
        monthly_lai=inst.surfdata.monthly_lai,
        monthly_sai=inst.surfdata.monthly_sai,
        monthly_height_top=inst.surfdata.monthly_htop,
        monthly_height_bot=inst.surfdata.monthly_hbot,
        months=months)
end

# Step 2: Run satellite phenology to update canopy state
# Build non-lake patch mask
mask_nolakep = .!lun.lakpoi[pch.landunit]

CLM.satellite_phenology!(sp, cs, waterdiag, pch, mask_nolakep, bc_patch)

println("  After phenology:")
for p in 1:np
    println("    Patch $p: elai=$(cs.elai_patch[p]), esai=$(cs.esai_patch[p]), " *
            "tlai=$(cs.tlai_patch[p]), tsai=$(cs.tsai_patch[p])")
end

# ---------------------------------------------------------------------------
# Phase 4: Run surface albedo
# ---------------------------------------------------------------------------

println("=== Phase 4: Running surface albedo ===")

# Compute solar declination and next-SW calendar day
(declinp1, eccf) = CLM.compute_orbital(calday)
nextsw_cday = calday

# Build non-urban masks
mask_nourbanc = .!lun.urbpoi[col.landunit]
mask_nourbanp = .!lun.urbpoi[pch.landunit]

# Coszen function (same lambda as in clm_drv!)
coszen_func = (cday, lat_r, lon_r, decl) -> begin
    hour_angle = 2.0 * π * mod(cday, 1.0) + lon_r - π
    max(sin(lat_r) * sin(decl) + cos(lat_r) * cos(decl) * cos(hour_angle), 0.0)
end

CLM.surface_albedo!(
    surfalb, inst.surfalb_con,
    grc, col, lun, pch,
    cs, temp, wsb, waterdiag,
    inst.lakestate, inst.aerosol,
    mask_nourbanc, mask_nourbanp,
    nextsw_cday, declinp1,
    bounds.begg:bounds.endg,
    bounds.begc:bounds.endc,
    bounds.begp:bounds.endp,
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

println("  After surface albedo:")
for p in 1:np
    println("    Patch $p: albd=[$(surfalb.albd_patch[p,1]), $(surfalb.albd_patch[p,2])], " *
            "albi=[$(surfalb.albi_patch[p,1]), $(surfalb.albi_patch[p,2])]")
end

# ---------------------------------------------------------------------------
# Phase 5: Run surface radiation
# ---------------------------------------------------------------------------

println("=== Phase 5: Running surface radiation ===")

# Downscale forcing to column level
forc_solad_col = zeros(nc, numrad)
forc_solai_grc = zeros(ng, numrad)
for c in 1:nc
    g = col.gridcell[c]
    for ib in 1:numrad
        forc_solad_col[c, ib] = a2l.forc_solad_not_downscaled_grc[g, ib]
    end
end
for g in 1:ng
    for ib in 1:numrad
        forc_solai_grc[g, ib] = a2l.forc_solai_grc[g, ib]
    end
end

mask_urbanp = BitVector(lun.urbpoi[pch.landunit])

CLM.surface_radiation!(
    surfalb, cs, solarabs, surfrad,
    waterdiag, col, lun, grc, pch,
    forc_solad_col, forc_solai_grc,
    mask_nourbanp, mask_urbanp,
    bounds.begp:bounds.endp;
    dtime=DTIME,
    current_tod=(first_daytime - 1) * DTIME,
    use_subgrid_fluxes=true,
    use_snicar_frc=false,
    use_SSRE=false,
)

println("  After surface radiation:")
for p in 1:np
    println("    Patch $p: fsa=$(solarabs.fsa_patch[p]), " *
            "sabg=$(solarabs.sabg_patch[p]), " *
            "sabv=$(solarabs.sabv_patch[p]), " *
            "fsr=$(solarabs.fsr_patch[p])")
end

# ---------------------------------------------------------------------------
# Phase 6: Export radiation test data
# ---------------------------------------------------------------------------

println("=== Phase 6: Exporting radiation test data ===")

raddir = joinpath(OUTDIR, "radiation")
mkpath(raddir)

# Cosine solar zenith angle
coszen_grc = zeros(ng)
for g in 1:ng
    coszen_grc[g] = coszen_func(nextsw_cday, grc.lat[g], grc.lon[g], declinp1)
end
write_binary(joinpath(raddir, "coszen_grc.bin"), coszen_grc)

# Forcing solar components
write_binary(joinpath(raddir, "forc_solad_col.bin"), forc_solad_col)
write_binary(joinpath(raddir, "forc_solai_grc.bin"), forc_solai_grc)

# Canopy state (after phenology)
write_binary(joinpath(raddir, "elai.bin"), Float64.(cs.elai_patch))
write_binary(joinpath(raddir, "esai.bin"), Float64.(cs.esai_patch))
write_binary(joinpath(raddir, "tlai.bin"), Float64.(cs.tlai_patch))
write_binary(joinpath(raddir, "tsai.bin"), Float64.(cs.tsai_patch))
write_binary(joinpath(raddir, "htop.bin"), Float64.(cs.htop_patch))
write_binary(joinpath(raddir, "hbot.bin"), Float64.(cs.hbot_patch))

# Surface albedo outputs — column level
write_binary(joinpath(raddir, "albsod_col.bin"), Float64.(surfalb.albsod_col))
write_binary(joinpath(raddir, "albsoi_col.bin"), Float64.(surfalb.albsoi_col))
write_binary(joinpath(raddir, "albsnd_hst_col.bin"), Float64.(surfalb.albsnd_hst_col))
write_binary(joinpath(raddir, "albsni_hst_col.bin"), Float64.(surfalb.albsni_hst_col))
write_binary(joinpath(raddir, "albgrd_col.bin"), Float64.(surfalb.albgrd_col))
write_binary(joinpath(raddir, "albgri_col.bin"), Float64.(surfalb.albgri_col))

# Surface albedo outputs — patch level
write_binary(joinpath(raddir, "albd_patch.bin"), Float64.(surfalb.albd_patch))
write_binary(joinpath(raddir, "albi_patch.bin"), Float64.(surfalb.albi_patch))
write_binary(joinpath(raddir, "fabd_patch.bin"), Float64.(surfalb.fabd_patch))
write_binary(joinpath(raddir, "fabi_patch.bin"), Float64.(surfalb.fabi_patch))
write_binary(joinpath(raddir, "ftdd_patch.bin"), Float64.(surfalb.ftdd_patch))
write_binary(joinpath(raddir, "ftid_patch.bin"), Float64.(surfalb.ftid_patch))
write_binary(joinpath(raddir, "ftii_patch.bin"), Float64.(surfalb.ftii_patch))

# SNICAR layer fluxes (column-level)
write_binary(joinpath(raddir, "flx_absdv_col.bin"), Float64.(surfalb.flx_absdv_col))
write_binary(joinpath(raddir, "flx_absdn_col.bin"), Float64.(surfalb.flx_absdn_col))
write_binary(joinpath(raddir, "flx_absiv_col.bin"), Float64.(surfalb.flx_absiv_col))
write_binary(joinpath(raddir, "flx_absin_col.bin"), Float64.(surfalb.flx_absin_col))

# Surface radiation outputs — patch level
write_binary(joinpath(raddir, "fsa.bin"), Float64.(solarabs.fsa_patch))
write_binary(joinpath(raddir, "sabg.bin"), Float64.(solarabs.sabg_patch))
write_binary(joinpath(raddir, "sabv.bin"), Float64.(solarabs.sabv_patch))
write_binary(joinpath(raddir, "fsr.bin"), Float64.(solarabs.fsr_patch))
write_binary(joinpath(raddir, "sabg_soil.bin"), Float64.(solarabs.sabg_soil_patch))
write_binary(joinpath(raddir, "sabg_snow.bin"), Float64.(solarabs.sabg_snow_patch))
write_binary(joinpath(raddir, "sabg_lyr.bin"), Float64.(solarabs.sabg_lyr_patch))
write_binary(joinpath(raddir, "parveg.bin"), Float64.(surfrad.parveg_ln_patch))

# Sun/shade fractions
write_binary(joinpath(raddir, "fsun.bin"), Float64.(cs.fsun_patch))
write_binary(joinpath(raddir, "laisun.bin"), Float64.(cs.laisun_patch))
write_binary(joinpath(raddir, "laisha.bin"), Float64.(cs.laisha_patch))
write_binary(joinpath(raddir, "vcmaxcintsun.bin"), Float64.(surfalb.vcmaxcintsun_patch))
write_binary(joinpath(raddir, "vcmaxcintsha.bin"), Float64.(surfalb.vcmaxcintsha_patch))

# fsun_z is on SurfaceAlbedoData (already correct below)

# Canopy layer structure
write_binary(joinpath(raddir, "nrad.bin"), Float64.(surfalb.nrad_patch))
write_binary(joinpath(raddir, "tlai_z.bin"), Float64.(surfalb.tlai_z_patch))
write_binary(joinpath(raddir, "tsai_z.bin"), Float64.(surfalb.tsai_z_patch))
write_binary(joinpath(raddir, "fsun_z.bin"), Float64.(surfalb.fsun_z_patch))

# Two-stream sun/shade layer absorption (for canopy_sun_shade_fracs)
write_binary(joinpath(raddir, "fabd_sun_z.bin"), Float64.(surfalb.fabd_sun_z_patch))
write_binary(joinpath(raddir, "fabi_sun_z.bin"), Float64.(surfalb.fabi_sun_z_patch))
write_binary(joinpath(raddir, "fabd_sha_z.bin"), Float64.(surfalb.fabd_sha_z_patch))
write_binary(joinpath(raddir, "fabi_sha_z.bin"), Float64.(surfalb.fabi_sha_z_patch))

# Metadata
meta = Dict(
    "first_daytime_timestep" => first_daytime,
    "calday" => calday,
    "declinp1" => declinp1,
    "coszen" => coszen_grc[1],
    "kmo" => kmo,
    "kda" => kda,
    "lat" => lat,
    "lon" => lon,
    "nc" => nc,
    "np" => np,
    "ng" => ng,
    "numrad" => numrad,
    "nlevsno" => nlevsno,
    "dtime" => DTIME,
    "forc_solad_vis" => a2l.forc_solad_not_downscaled_grc[1, 1],
    "forc_solad_nir" => a2l.forc_solad_not_downscaled_grc[1, 2],
    "forc_solai_vis" => a2l.forc_solai_grc[1, 1],
    "forc_solai_nir" => a2l.forc_solai_grc[1, 2],
    "pch_itype" => Int.(pch.itype),
    "pch_column" => Int.(pch.column),
    "pch_gridcell" => Int.(pch.gridcell),
    "lun_itype" => Int.(lun.itype[col.landunit]),
    "col_snl" => Int.(col.snl),
    "frac_sno" => Float64.(waterdiag.frac_sno_col),
    "snow_depth" => Float64.(waterdiag.snow_depth_col),
    "soil_color" => Int(inst.surfdata.soil_color[1]),
    "h2osoi_vol_top" => [Float64(ws.h2osoi_vol_col[c, 1]) for c in 1:nc],
    "t_grnd" => Float64.(temp.t_grnd_col),
    "t_veg" => Float64.(temp.t_veg_patch),
    "fwet" => Float64.(waterdiag.fwet_patch),
    "fcansno" => hasproperty(waterdiag, :fcansno_patch) ? Float64.(waterdiag.fcansno_patch) : zeros(np),
)

open(joinpath(raddir, "metadata.json"), "w") do io
    JSON.print(io, meta, 2)
end

println("\n=== Radiation test data exported to $raddir ===")
println("  Files: $(length(readdir(raddir)))")
