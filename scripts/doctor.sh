#!/usr/bin/env sh
set -eu

fail=0

need_cmd() {
  if command -v "$1" >/dev/null 2>&1; then
    printf '%s: %s\n' "$1" "$(command -v "$1")"
  else
    printf '%s: missing\n' "$1" >&2
    fail=1
  fi
}

printf 'CLM-hs build doctor\n'
printf '===================\n'

need_cmd stack
need_cmd pkg-config

if command -v ghc >/dev/null 2>&1; then
  ghc --version
else
  printf 'ghc: missing on PATH; stack --install-ghc can provision it\n'
fi

if command -v cabal >/dev/null 2>&1; then
  cabal --numeric-version
else
  printf 'cabal: missing on PATH; stack is the canonical build path\n'
fi

if command -v pkg-config >/dev/null 2>&1; then
  if pkg-config --exists netcdf; then
    printf 'netcdf pkg-config: %s\n' "$(pkg-config --modversion netcdf)"
    pkg-config --cflags --libs netcdf
  else
    printf 'netcdf pkg-config: missing\n' >&2
    fail=1
  fi
fi

if [ "$fail" -ne 0 ]; then
  printf '\nOne or more required build inputs are missing.\n' >&2
  exit 1
fi

printf '\nBuild inputs look usable. Run: stack --install-ghc test\n'
