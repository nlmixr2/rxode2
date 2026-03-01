# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

**rxode2** is an R package for solving and simulating from ODE-based pharmacometric models. It parses a mini-language into C code, compiles it, and dynamically loads it into R. The package contains mixed R, C, C++, and Fortran code.

## Build and Development Commands

### Install/Build
```r
# Install development version (from within R)
devtools::install()

# Or from the shell
R CMD INSTALL .
```

### Document
```r
devtools::document()
```

### Run All Tests
```r
# From within R (sets up proper environment)
devtools::test()

# Or from shell
NOT_CRAN=true Rscript -e "devtools::test()"
```

### Run a Single Test File
```r
# Filter by test file name (without "test-" prefix and ".R" suffix)
devtools::test(filter="basic")
devtools::test(filter="ui")
devtools::test(filter="linCmt")
```

### Run Tests from Installed Package
```r
# Full test suite
rxValidate(TRUE)  # or rxTest(TRUE)

# CRAN-level tests only
rxValidate(FALSE)
```

### R CMD Check
```r
devtools::check()
# Or: R CMD check .
```

### Regenerate Grammar and Build Artifacts
When modifying `inst/tran.g` (the dparser grammar) or needing to regenerate generated files:
```r
# Regenerates: src/tran.g.d_parser.h, R/rxrandomui.R, inst/include/*.h
.rxodeBuildCode()
```

## Architecture

### ODE Solving Pipeline

1. **Model Definition**: Users define models as R functions with `ini({})` and `model({})` blocks, or as strings/files using the rxode2 mini-language.

2. **Parsing** (`src/tran.c`, `inst/tran.g`): The mini-language grammar is defined in `inst/tran.g` (a dparser grammar). `tran.c` uses this grammar (pre-compiled into `src/tran.g.d_parser.h`) to parse model equations into an AST.

3. **Code Generation** (`src/codegen.c`): The parsed AST is translated into C code with ODE functions, Jacobians, and sensitivity equations.

4. **Compilation**: The generated C code is compiled to a shared library (`inline` package) and dynamically loaded.

5. **Solving** (`src/par_solve.cpp`): `rxSolve()` drives the numerical ODE solving using the compiled model. Supports multiple backends:
   - LSODA (`src/lsoda.c`, `src/dlsoda.f`) — stiff/non-stiff adaptive
   - VODE (`src/call_dvode.c`) — stiff solver
   - DOP853 (`src/dop853.c`) — non-stiff Runge-Kutta
   - Parallel solving via OpenMP across subjects

### Key Subsystems

**UI/Model Interface** (`R/ui.R`, `R/rxUiGet.R`, `R/asRxui.R`):
- `rxUi` objects are R environments containing the fully parsed model
- `rxUiGet` is an S3 dispatch system: `ui$property` calls `rxUiGet.property(x)`
- Model piping (`R/piping*.R`) copies and modifies `rxUi` objects via `.copyUi()`

**Event Table** (`src/et.cpp`, `src/etTran.cpp`, `R/et.R`):
- `et()` constructs dosing/sampling event tables
- `etTran()` transforms event tables for the internal solver format
- Supports NONMEM-compatible event IDs (evid 0-4)

**Linear Compartment Models** (`src/linCmt.cpp`, `R/linCmt.R`):
- Analytical solutions for 1-3 compartment PK models
- Gradients computed via Stan math auto-differentiation (for FOCEi in nlmixr2)
- Can be mixed with ODEs in the same model

**Output Data Frame** (`src/rxode2_df.cpp`, `src/rxData.cpp`):
- The solved result is returned as a modified data frame with special classes
- Factor levels and units are preserved from the event table

**Testing** (`tests/testthat/`):
- Tests use testthat edition 3
- Most test files wrap tests in `rxTest({...})` which skips on CRAN (requires `NOT_CRAN=true`)
- Test files follow `test-*.R` naming convention

### Important Files

| File | Purpose |
|------|---------|
| `inst/tran.g` | dparser grammar for the rxode2 mini-language |
| `src/tran.c` | Grammar-based parser entry point |
| `src/codegen.c` | C code generation from parsed AST |
| `src/par_solve.cpp` | Main parallel ODE solver (OpenMP) |
| `src/rxData.cpp` | rx_solve data structure management |
| `src/linCmt.cpp` | Analytical PK compartment solutions |
| `src/et.cpp` | Event table construction |
| `R/rxode2.R` | `rxode2()` function entry point |
| `R/rxsolve.R` | `rxSolve()` function |
| `R/ui.R` | Model function parsing (ini/model blocks) |
| `R/rxUiGet.R` | S3 dispatch for `ui$property` access |
| `R/build.R` | Code generation helpers (run during development) |
| `inst/include/rxode2.h` | Main C header for generated model code |

## R Code Style

- **Exported functions**: `camelCase` (e.g., `rxSolve`, `rxControl`, `modelExtract`)
- **Internal/non-exported functions**: `.camelCase` with a leading dot (e.g., `.copyUi`, `.rxSolveFromUi`)
- **Local variables inside functions**: `.camelCase` with a leading dot (e.g., `.model`, `.iniDf`, `.ret`)
- **S3 methods**: follow `generic.class` R convention (e.g., `rxSolve.rxUi`, `modelExtract.function`)
- Avoid `snake_case` for new names; underscores only appear in S3 methods for snake_case generics from other packages (e.g., `drop_units.rxSolve`)

### C/C++ Conventions

- Use `(Rf_error)(...)` (wrapped in parens) rather than `Rf_error(...)` to prevent macro conflicts
- C files use `#define STRICT_R_HEADERS` and `#define USE_FC_LEN_T`
- C++ files use `#define R_NO_REMAP` to avoid R API name pollution
- OpenMP support is conditional via `rxomp.h`

### Generated Files (do not edit manually)

- `src/tran.g.d_parser.h` — generated from `inst/tran.g` via dparser
- `R/rxrandomui.R` — generated by `.generateRandomUiFuns()` in `R/build.R`
- `inst/include/rxode2parse_control.h` — generated by `genDefine()` in `R/build.R`
