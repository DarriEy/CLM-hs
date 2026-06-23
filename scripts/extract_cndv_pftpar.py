#!/usr/bin/env python3
"""Extract the CNDV (dynamic global vegetation) per-PFT ecophysiological
constants from a CLM5 parameter file into the port's per-variable binary
format (raw little-endian float64, one value per PFT, index 0 = bare ground).

These feed CLM.Infrastructure.ReadParams.readDGVEcophysCon ->
CLM.Types.DGVSData.DGVEcophysCon, which the CNDV step uses for the bioclimatic
establishment/survival limits (overriding the built-in LPJ fallback table).

Variables (see CNDVEstablishmentMod.F90 / clm5_params.nc):
  pftpar20 -> crownarea_max (m2)
  pftpar28 -> tcmin  (min coldest-month mean T, degC)
  pftpar29 -> tcmax  (max coldest-month mean T, degC)
  pftpar30 -> gddmin (min growing degree days >= 5C)
  pftpar31 -> twmax  (warmest-month T upper limit, degC; >=999 = no limit)

Fill values (9999.9 for tcmin, 1000 for tcmax/twmax) are written as-is; the
Haskell CNDV logic treats them correctly.

Usage:
    extract_cndv_pftpar.py <clm5_params.nc> <out_params_dir> [<out_params_dir> ...]
"""
import sys
import os
import numpy as np
import netCDF4 as nc

VARS = ["pftpar20", "pftpar28", "pftpar29", "pftpar30", "pftpar31"]


def main(argv):
    if len(argv) < 3:
        print(__doc__)
        return 1
    src, out_dirs = argv[1], argv[2:]
    ds = nc.Dataset(src)
    for v in VARS:
        a = np.ascontiguousarray(np.array(ds.variables[v][:], dtype="<f8"))
        for d in out_dirs:
            os.makedirs(d, exist_ok=True)
            with open(os.path.join(d, "pftcon_%s.bin" % v), "wb") as f:
                f.write(a.tobytes())
        print("wrote pftcon_%s.bin (%d PFTs) to %d dir(s)" % (v, len(a), len(out_dirs)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
