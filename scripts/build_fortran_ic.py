#!/usr/bin/env python3
"""Build a Haskell pipeline data dir initialized from the Fortran 2003-01-01
restart, for a matched-IC winter comparison against the Fortran h0 history.

Layout: snow lives in the top nlevsno=12 slots of the 37-layer column arrays;
active snow (snl=-4) occupies slots 8..11. Ground geometry (slots 12..36) and
all static column properties are taken from the existing 2002 coldstart; only
the state (t_soisno, h2osoi, snow geometry, t_grnd, t_veg, snl, snow diag) is
overwritten from the Fortran restart. Forcing is 2003.
"""
import os, shutil, numpy as np, netCDF4 as nc

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC  = os.path.join(ROOT, "test/data")
DST  = os.path.join(ROOT, "test/data_fortran_ic")
RST  = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/cvdump_run/Bow_at_Banff_lumped.clm2.r.2003-01-01-00000.nc"
FORC2003 = "/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/domain_Bow_at_Banff_lumped/data/forcing/CLM_input/clmforc.2003.nc"
NC, NLEVTOT, NLEVSNO = 2, 37, 12

def col1(name, nlev):           # column-0 slice of an interleaved 2-col bin
    b = np.fromfile(os.path.join(SRC, "coldstart", name), dtype="<f8")
    return b[0::NC][:nlev].copy()

def write2(name, vec):          # write a 1-col vector as interleaved 2-col bin
    out = np.empty(len(vec)*NC, dtype="<f8")
    for c in range(NC): out[c::NC] = vec
    out.tofile(os.path.join(DST, "coldstart", name))

def write1(name, vec):          # write a per-column / per-patch scalar bin as-is
    np.asarray(vec, dtype="<f8").tofile(os.path.join(DST, "coldstart", name))

# --- fresh dir: copy everything, then overwrite state bins + forcing ---
if os.path.exists(DST): shutil.rmtree(DST)
shutil.copytree(SRC, DST)
for f in os.listdir(os.path.join(DST, "forcing")):
    os.remove(os.path.join(DST, "forcing", f))
d = nc.Dataset(FORC2003)
for ncv, fn in [("TBOT","tbot"),("QBOT","qbot"),("WIND","wind"),("FSDS","fsds"),
                ("FLDS","flds"),("PSRF","psrf"),("PRECTmms","precip")]:
    np.asarray(d[ncv][:]).ravel().astype("<f8").tofile(os.path.join(DST,"forcing",fn+".bin"))

# --- Fortran restart state ---
r = nc.Dataset(RST)
def rv(k): return np.asarray(r[k][:]).ravel()
snl   = int(rv("SNLSNO")[0])                # -4
tsoi  = rv("T_SOISNO")                       # (37,) already snow@8-11, ground@12-36
hliq  = rv("H2OSOI_LIQ"); hice = rv("H2OSOI_ICE")
dzsno = rv("DZSNO"); zsno = rv("ZSNO"); zisno = rv("ZISNO")  # (12,)
tg    = rv("T_GRND")[0]; tveg = rv("T_VEG")
snowd = rv("SNOW_DEPTH")[0]; fsno = rv("frac_sno")[0]; fsnoeff = rv("frac_sno_eff")[0]
# With explicit snow layers (snl<0) the layer water lives in h2osoi_ice; the
# bulk h2osno_col holds only the "no layers" remainder (H2OSNO_NO_LAYERS, =0 here).
# Setting it to the total double-counts snow in the snow-water/melt steps.
h2osno = float(rv("H2OSNO_NO_LAYERS")[0])

# ground geometry from existing coldstart (static), snow geometry from restart
dz = col1("col_dz.bin", NLEVTOT); dz[0:NLEVSNO] = dzsno
z  = col1("col_z.bin",  NLEVTOT); z[0:NLEVSNO]  = zsno
zi = col1("col_zi.bin", NLEVTOT+1); zi[0:NLEVSNO] = zisno; zi[NLEVSNO] = 0.0

write2("t_soisno.bin", tsoi)
write2("h2osoi_liq.bin", hliq)
write2("h2osoi_ice.bin", hice)
write2("col_dz.bin", dz)
write2("col_z.bin", z)
write2("col_zi.bin", zi)
write1("t_grnd.bin", [tg])
write1("t_h2osfc.bin", [tg])
write1("h2osfc.bin", [0.0])
write1("h2osno.bin", [h2osno])
write1("snl.bin", [float(snl)])
write1("snow_depth.bin", [snowd])
write1("frac_sno.bin", [fsno])
write1("frac_sno_eff.bin", [fsnoeff])
# t_veg: Fortran has 3 active patches; Haskell has 4 (4th wt 0). Default the 4th.
tv4 = np.array(list(tveg[:3]) + [tg], dtype="<f8")
write1("t_veg.bin", tv4)

print(f"Built {DST}: snl={snl}, snow_depth={snowd:.3f}m, frac_sno={fsno:.4f}, "
      f"h2osno={h2osno:.1f}, t_grnd={tg:.2f}, t_veg={list(np.round(tveg,2))}")
print(f"  snow dz(8-11)={np.round(dzsno[8:12],3)}  ground dz(12-15)={np.round(dz[12:16],3)}")
print(f"  t_soisno snow(8-11)={np.round(tsoi[8:12],2)}  ground(12-15)={np.round(tsoi[12:16],2)}")
