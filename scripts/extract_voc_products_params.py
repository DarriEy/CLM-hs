#!/usr/bin/env python3
"""Extract per-PFT VOC (MEGAN isoprene) and wood-product (pprod) parameters into
the port's per-variable binary format (raw little-endian float64, CLM-indexed
with bare ground = 0).

These feed CLM.Infrastructure.ReadParams:
  - readMeganIsopreneEF -> clmMeganEF  (VOC step's per-PFT isoprene EF)
  - readPprod10/readPprod100 -> clmPprod10/clmPprod100 (wood-harvest splits)

Sources:
  - pprod10/pprod100 come from clm5_params.nc (already CLM-indexed, 79 PFTs).
  - The isoprene emission factors come from the MEGAN2.1 factor file, variable
    Class_EF(PFT_Num=78, Class_Num), isoprene = class 0. That file has 78 PFTs
    (no bare ground), so it is shifted into CLM indexing with bare = 0:
    clm[0] = 0; clm[i] = Class_EF[i-1, 0] for i in 1..78.

Usage:
    extract_voc_products_params.py <clm5_params.nc> <megan_factors.nc> \
        <out_params_dir> [<out_params_dir> ...]
"""
import sys
import os
import numpy as np
import netCDF4 as nc


def main(argv):
    if len(argv) < 4:
        print(__doc__)
        return 1
    params_nc, megan_nc, out_dirs = argv[1], argv[2], argv[3:]

    def write(name, a):
        a = np.ascontiguousarray(np.array(a, dtype="<f8"))
        for d in out_dirs:
            os.makedirs(d, exist_ok=True)
            with open(os.path.join(d, name + ".bin"), "wb") as f:
                f.write(a.tobytes())
        print("wrote %s (len %d)" % (name, len(a)))

    p = nc.Dataset(params_nc)
    write("pftcon_pprod10", np.array(p.variables["pprod10"][:]))
    write("pftcon_pprod100", np.array(p.variables["pprod100"][:]))

    m = nc.Dataset(megan_nc)
    ef = np.array(m.variables["Class_EF"][:])[:, 0]  # isoprene = class 0
    clm = np.zeros(len(ef) + 1)
    clm[1:] = ef
    write("megan_isoprene_ef", clm)
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
