# Changelog for `CLM-hs`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

### Fixed
- **Winter latent-heat over-production (root cause: corrupt humidity forcing).**
  `test/data/forcing/qbot.bin` (2002 single-column forcing) was entirely zeros,
  so the bare-ground evaporation driver `qflx_evap_soi = -raiw·(forc_q − qg)` ran
  with `forc_q = 0`, i.e. zero atmospheric humidity opposing the ground saturation
  humidity. This produced spurious winter sublimation: day-1 `EFLX_LH_TOT` ≈ 11.7 W/m²
  against the Julia reference's ≈ 0.78 W/m². Regenerated `qbot.bin` from the source
  `clmforc.2002.nc` `QBOT` field (verified the other six forcing bins already matched
  the NetCDF exactly). Cuts day-1 winter LH over-production ~3× (11.7 → 3.8 W/m²).
  The ported bare-ground physics was faithful; the fault was solely the input data.
- **Bow at Banff calibration test:** added the missing `test/data_bow/params/`
  directory (70 global CLM parameter bins copied from the working `test/data/params/`,
  `numpft = 79`, same `clm5_params.nc`). The QRUNOFF smoke test now passes instead of
  crashing on a missing `phot_theta_ip.bin` fixture.

## 0.1.0.0 - YYYY-MM-DD
