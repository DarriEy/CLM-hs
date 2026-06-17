# Reproducible Build

The canonical build path is Stack with the checked-in `stack.yaml.lock`.

```sh
sh scripts/doctor.sh
stack --install-ghc test
```

The project also has a pinned `cabal.project` for Cabal users with GHC 9.6.6:

```sh
cabal update
cabal test
```

## Native Dependency

`CLM-hs` links directly to `libnetcdf` through FFI. Cabal now discovers it via
`pkg-config` instead of hard-coded Homebrew paths.

macOS:

```sh
brew install haskell-stack pkg-config netcdf
```

Ubuntu:

```sh
sudo apt-get update
sudo apt-get install -y pkg-config libnetcdf-dev
```

## Current CI Contract

`stack test` is expected to fail until the port-completion audit is cleared.
Those failures are intentional: they track remaining placeholders/stubs and
loose Julia/Fortran parity gaps in executable form.
