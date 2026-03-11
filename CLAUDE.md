# CLM-hs — Haskell Port of CLM/CTSM

## Project Overview
This is a Haskell port of the Community Land Model (CLM/CTSM) from Fortran 90.
Sister project to `../CLM.jl` (Julia port). The goal is process fidelity, strong
static typing, and potential for automatic differentiation via AD libraries.

## Fortran Source Location
The original Fortran CLM source is at:
`/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/installs/clm/`

## Julia Reference
The Julia port (fully working) is at `../CLM.jl/` — use it as the primary
translation reference alongside the Fortran source.

## Architecture & Conventions

### Module Structure
```
src/CLM.hs                      -- Top-level re-export module
src/CLM/Constants/               -- Physical constants, control flags
src/CLM/Types/                   -- Data structures (records)
src/CLM/Infrastructure/          -- Solvers, filters, cold-start, I/O
src/CLM/BioGeoPhys/              -- Biogeophysics process modules
src/CLM/BioGeoChem/              -- Biogeochemistry process modules
src/CLM/Driver/                  -- Main timestep driver
```

### Translation Patterns

**Fortran types → Haskell records with strict fields:**
```haskell
data TemperatureData = TemperatureData
  { t_soisno_col  :: !(VU.Vector Double)
  , t_grnd_col    :: !Double
  } deriving (Show)
```

**Fortran filter loops → mask-based iteration:**
```haskell
VU.iforM_ mask $ \c active ->
  when active $ do
    -- physics for column c
```

**Fortran subroutines → pure functions or ST monad for mutation:**
```haskell
soilTemperature :: TemperatureData -> FilterSet -> TemperatureData
```

### Key Decisions
- **Preserve Fortran variable names** for traceability
- **Unboxed vectors** (`Data.Vector.Unboxed`) for numeric arrays — cache-friendly
- **Structure of Arrays** layout — matches Fortran, Julia, and GPU patterns
- **Bool vectors** replace integer filter arrays
- **Float64 (Double)** throughout initially — parametric types later
- **Discontinuities kept as-is** in Phase 1 — smoothing for AD in later phases

### Adding a New Module
1. Read the Fortran source completely
2. Reference the Julia port in `../CLM.jl/src/`
3. Create the Haskell file in the appropriate `src/CLM/` subdirectory
4. Add the module to `exposed-modules` in `CLM-hs.cabal`
5. Add the re-export to `src/CLM.hs`
6. Write tests in `test/Spec.hs`
7. Ensure ALL existing tests still pass: `stack test`

## Build & Test
```bash
stack build          # Compile
stack test           # Run test suite
stack run            # Run executable
stack ghci           # Interactive REPL
```

## Dependencies
- `vector` — Unboxed arrays (primary numeric container)
- `massiv` — Multi-dimensional arrays (for 2D soil grids)
- `hmatrix` — Linear algebra (band-diagonal solvers)
- `hspec` + `QuickCheck` — Testing
- `mtl` / `transformers` — Monad transformers for stateful computation
