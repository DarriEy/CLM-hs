# CLM-hs Agent Rules

## Ralph Loop Automation
When this agent is launched under the automated Ralph Loop, it must follow these steps:
1. Run `./scripts/ralph_harness.py next` to print the active task details.
2. Open the active Haskell file.
3. Look up the corresponding Fortran FATES reference source file under `/Users/darri.eythorsson/compHydro/SYMFLUENCE_data/installs/clm/src/fates/` using the `TODO` comment as a guide.
4. Port the Fortran logic to the Haskell module (using unboxed vectors, strict fields, and clean types).
5. Compile and verify your edits by running `./scripts/ralph_harness.py oracle`.
6. Exit once the oracle task compiles and passes.
