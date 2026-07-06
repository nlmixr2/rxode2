# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when
working with code in this repository.

## Overview

**rxode2** is an R package for solving and simulating from ODE-based
pharmacometric models. It parses a mini-language into C code, compiles
it, and dynamically loads it into R. The package contains mixed R, C,
C++, and Fortran code.

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
```sh
# From shell
find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test()"
```

### Run a Single Test File
```sh
# Filter by test file name (without "test-" prefix and ".R" suffix)
find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test(filter='basic')"
find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test(filter='ui')"
find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test(filter='linCmt')"
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
invisible(lapply(list.files("src", "\\.s?o$", full.names = TRUE), unlink));devtools::check()
# Or: R CMD check .
```

### Get generated C code and C code errors from model

When trying to get the underlying generated code from a model that
compiled successfully, you can have it echo to the console with:

```r
summary(rxC(rxode2model))
```

With a model that did not compile successfully, you can get the code by:

```r
cat(suppressMessages(rxode2::rxLastCompile())$c)
```

To get the compile error you can use:

```r
cat(suppressMessages(rxode2::rxLastCompile())$stderr)
```

If you need stout too you can get that with

```r
cat(suppressMessages(rxode2::rxLastCompile())$stderr)
```

### Regenerate Grammar, Documentation and Build Artifacts

When modifying `inst/tran.g` (the dparser grammar) or needing to
regenerate generated files:

```r
# Regenerates: src/tran.g.d_parser.h, R/rxrandomui.R, inst/include/*.h
devtools::document()
```

## Architecture

### ODE Solving Pipeline

1. **Model Definition**: Users define models as R functions with
   `ini({})` and `model({})` blocks, or as strings/files using the
   rxode2 mini-language.

2. **Parsing** (`src/tran.c`, `inst/tran.g`): The mini-language
   grammar is defined in `inst/tran.g` (a dparser grammar). `tran.c`
   uses this grammar (pre-compiled into `src/tran.g.d_parser.h`) to
   parse model equations into an AST.

3. **Code Generation** (`src/codegen.c`): The parsed AST is translated
   into C code with ODE functions, Jacobians, and sensitivity
   equations.

4. **Compilation**: The generated C code is compiled to a shared
   library (`inline` package) and dynamically loaded.

5. **Solving** (`src/par_solve.cpp`): `rxSolve()` drives the numerical ODE solving using the compiled model. Supports multiple backends:
   - LSODA (`src/lsoda.c`, `src/dlsoda.f`) -- stiff/non-stiff adaptive
   - DOP853 (`src/dop853.c`) -- non-stiff Runge-Kutta
   - Parallel solving via OpenMP across subjects

### Key Subsystems

**UI/Model Interface** (`R/ui.R`, `R/rxUiGet.R`, `R/asRxui.R`):
- `rxUi` objects are R environments containing the fully parsed model
- `rxUiGet` is an S3 dispatch system: `ui$property` calls `rxUiGet.property(x)`
- Model piping (`R/piping*.R`) copies and modifies `rxUi` objects via `.copyUi()`

**Event Table** (`src/et.cpp`, `src/etTran.cpp`, `R/et.R`):

- `et()` constructs dosing/sampling event tables

- `etTran()` transforms event tables for the internal solver format

- Supports NONMEM-compatible event IDs (evid 0-4) as well as rxode2
  specific evids like 5 (replace event), 6 (multiply event) and 7
  (phantom/transit). Internally the EVIDs are translated to rxode2
  specific evids documented
  https://nlmixr2.github.io/rxode2/articles/rxode2-events-classic.html

**Linear Compartment Models** (`src/linCmt.cpp`, `R/linCmt.R`):
- Analytical solutions for 1-3 compartment PK models
- Gradients computed via Stan math auto-differentiation (for FOCEi in nlmixr2)
- Can be mixed with ODEs in the same model

**Sensitivity Analysis** (`R/rxJacobian.R`, `R/adjoint.R`, `src/expandGrid.cpp`, `src/adjoint.cpp`):
- Forward sensitivities: `.rxJacobian()` builds the analytic Jacobian
  (`symengine::D`), `.rxSens()` splices `d/dt(rx__sens_<state>_BY_<param>__)`
  variational-equation lines into the model (solved as ordinary extra
  compartments). This is symbolic, not autodiff.
- Adjoint (backward) sensitivities mirror the same `rx__sens_*` output but via a
  backward pass. `.rxAdjoint()` symbolically generates the costate
  (`-J^T lambda`) and quadrature (`-lambda^T df/dp`) equations reusing the
  `rx__df_*` derivatives. `.rxAdjointSolve()` = full per-timepoint `dy(t)/dp`;
  `.rxAdjointGrad()` = single-sweep `dG/dtheta` objective gradient (incl. a FOCEi
  `-2LL` `errModel=`), where adjoint beats forward sensitivity.
- `.rxAdjointGrad` is split into a symbolic **build** (`.rxAdjointGradBuild`,
  runs symengine once) and a numeric **eval** (`.rxAdjointGradEval` / the C++
  `.rxAdjointGradEvalC` via the `rxode2AdjointSweep` sweep) so an optimizer
  builds once and evaluates the gradient many times. Build the gradient object
  BEFORE any `rxSolve` (a `load_all`-only symengine dispatch quirk affects the
  one-time symbolic build, never the numeric eval).

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
- Use American English spelling for consistency and do not use unicode characters

### Working with symengine objects (`.rxToSE`/`rxFromSE`/`symengine::D`)

The symengine env (eg `rxUiGet.foceiEtaS`/`rxUiGet.nlmEnv`, `rxS()`) and its
`Basic` objects overload/register R functions, so once you touch symengine in
these contexts base calls start failing with `user function 'X' requires N
arguments`. When manipulating symengine objects programmatically:

- **`get()` is masked.** Capture Basics from `assign()`'s return value instead:
  `.b <- eval(parse(text = 'assign("x", with(.s, D(rx_pred_, y)), envir=.s)'))`
  returns the assigned Basic (this is how the FEta apply avoids `get`).
- **Do symbolic ops inside the env**: `with(.s, D(...))` (or the `assign(...,
  envir=.s)` string idiom), NOT `symengine::D(...)` directly -- the qualified
  `pkg::fun` form gets intercepted too.
- **`rxFromSE()` of a Basic containing `lag0()`/`llik*()` poisons the *next*
  `[[`/`get`/`$`-read** (but not `rxFromSE` itself). Convert the "clean" Basics
  to text first and the poisoning one LAST; after it, use only vectorized base
  ops.
- **`[[` on a list of Basics always dispatches to symengine's `[[`** (fails).
  Never build a list of Basics -- `rxFromSE()` each inline.
- **`assign()` and `$<-` (writes) are safe**; `paste0`/`gsub`/`lapply` (C-level
  element access) are safe.
- **Temp symbol names** must avoid a trailing `_` (treated like the internal
  `THETA_1_`) and symengine-registered substrings (eg `Dmean`).

### C/C++ Conventions

- Use `(Rf_error)(...)` (wrapped in parens) rather than
  `Rf_error(...)` to prevent macro conflicts
- C files use `#define STRICT_R_HEADERS` and `#define USE_FC_LEN_T`
- C++ files use `#define R_NO_REMAP` to avoid R API name pollution
- OpenMP support is conditional via `rxomp.h`
- Instead of using PROTECT/UNPROTECT:
  - In C:
    - begin functions that typically need PROTECT/UNPROTECT with `rxProtectGuard;`
    - Use PROTECT->rxP()/UNPROTECT->rxUP()
    - Use rxUPAll() to unprotect everything before returning since
      `rxP()`/`rxUP()` counts the number of protected expressions in the functions
  - in C++:
    - For functions where protection is needed:
    - Create a protection object `rxProtect rx_protect;`
    - Use `rx_protect.protect()` instead of `PROTECT()`; `UNPROTECT`
      will be handled when the object goes out of scope.

#### Exposing a C/C++ function to downstream packages (e.g. nlmixr2est)

CRAN has asked this package to share C entry points via a **function-pointer
table**, NOT `R_RegisterCCallable` and NOT Rcpp's
`// [[Rcpp::export]]`/`[[Rcpp::interfaces(r,cpp)]]` (both cause ABI coupling).
The table is the positional external-pointer list built by
`_rxode2_rxode2Ptr()` in `src/init.c` and installed into a downstream package's
globals by `iniRxodePtrs()`/`iniRxodePtrs0()` in `inst/include/rxode2ptr.h` when
that package loads. To add a new function `foo` (copy any existing entry, e.g.
`rxode2AdjointSweep` at index 66, as a template):

1. **`src/<file>.cpp`**: define `extern "C" <ret> foo(<plain C types>)` (no Rcpp;
   pass R matrices as column-major `double*` + sizes). If R itself must call it,
   add a manual `extern "C" SEXP _rxode2_foo(SEXP...)` `.Call` wrapper and
   register it in the `callMethods[]` table in `src/init.c`.
2. **`src/init.c` `_rxode2_rxode2Ptr()`**: bump `#define nVec`; create
   `R_MakeExternalPtrFn((DL_FUNC)&foo, ...)`; `SET_VECTOR_ELT(ret, N, ...)` and
   the parallel `SET_STRING_ELT(retN, N, Rf_mkChar("foo"))` at the new index `N`.
3. **`inst/include/rxode2ptr.h`**: add `typedef <ret> (*foo_t)(...)` +
   `extern foo_t foo;`; the `foo = (foo_t) R_ExternalPtrAddrFn(VECTOR_ELT(p, N));`
   line in `iniRxodePtrs0` (index `N` must match `init.c`); and
   `foo_t foo = NULL;` in the NULL-init macro at the bottom.
4. **`inst/include/rxode2.h`**: a plain declaration guarded
   `#ifndef __RXODE2PTR_H__` (so it does not collide with the downstream pointer
   variable -- same as `par_solve`/`getRxSolve_`).

Appending at a new index is backward compatible (older consumers read the lower
indices and ignore the new one). Changing any `inst/include/*.h` header requires
a full clean rebuild (`rm -f src/*.o && R CMD INSTALL .`).

### Generated Files (do not edit manually)

- `src/tran.g.d_parser.h` -- generated from `inst/tran.g` via dparser
- `R/rxrandomui.R` -- generated by `.generateRandomUiFuns()` in `R/build.R`
- `inst/include/rxode2parse_control.h` -- generated by `genDefine()` in `R/build.R`
- `R/parseFuns.R` -- the list of parser-recognized functions
  (`.parseEnv$.parseFuns`), generated by `.rxodeBuildCode()` in `R/build.R` from
  `.rxSupportedFuns()` (`R/symengine.R`). To ADD a syntax function it must be
  registered in the symengine tables in `R/symengine.R` (`.rxSEeq`/`.rxSEsingle`
  /`.rxSEdouble`/`.rxOnly`), NOT hand-added to `R/parseFuns.R`.
- `R/rxSyntaxFunctions.R` -- generated by `.rxodeBuildCode()` from
  `inst/syntax-functions.csv` (the documentation source for syntax functions).
- Regenerate both with `.rxodeBuildCode()` (or `devtools::document()`).

## Documentation and Comment Style

- Keep comments and documentation terse. Condense multi-line explanations to
  one-liners, and drop worked examples and numeric anecdotes (e.g. "matches to
  ~1e-6", "up to order 8"). State the fact, not the story behind it.
- Shorten multi-paragraph roxygen descriptions to a single compact paragraph,
  but keep every `@param`, `@return`, `@author`, `@export`, and `@keywords` tag.
- Compress `NEWS.md` entries to the terse house style: one bullet per change,
  a sentence or two, no nested walk-throughs or validation prose.
- Do not claim the adjoint method is cheaper/faster than (or "beats") forward
  sensitivity; those cost comparisons do not hold here. Describe what the code
  does, not how it compares.
- ASCII only. No Unicode anywhere in the repo (CRAN requirement): use `--` for
  em-dashes, `->` for arrows, straight quotes, `...` for ellipses, etc.

## Known Issues / Investigation Notes

### `lag()`/`diff()` and lag()-based AR(1) estimation -- status

FIXED (committed):
- Solve/codegen path for `lag(x,1)`/`diff(x,1)` (lhs via `_PL[_LHS_<ordinal>_]`,
  covariates via the true parameter index) -- `src/codegen.h`. Tests in
  `tests/testthat/test-lhs-lag.R`.
- `rxNorm` normalization: `assertCorrectDiffArgs` (`src/parseFunsDiff.h`) gated the
  lag-number extraction on `strlen(v2) > 2`, which dropped the no-space form `,1`
  (length 2) -> `lagNo=0` -> the sbt (normalized-text) emission was skipped and
  `rxNorm("b=lag(a,1)")` gave `b=;`. Gate is now `> 1`. `rxNorm`/`rxS` round-trip
  `lag()`/`diff()`.
- symengine load: `.rxToSELagOrLead` (`R/symengine.R`) wraps the lagged var in
  `symengine::S("var")` so the assignment `eval(with(envir,...))` does not inline
  an lhs definition into the `lag()` call.

The base AR(1) estimation model (`.rxArEstLlikLines` in `R/err-foceiBase.R`) now
builds through `rxNorm`/`rxS`.

FIXED since: (1) dotted names inside a llik() argument broke the symengine
derivative -> AR(1) intermediates now use dot-free names (rx_arE_<var> etc.);
(2) lag()-referenced variables were inlined/dead-code-eliminated by symengine ->
`.rxCollectLaggedVars` + `.rxToSEAssignOperators` now emit them as lhs and bind
them as symbols; (3) nlmixr2est model assemblies (`rxUiGet.nlmRxModel`,
`.rxFinalizePred`) now emit the lag()-referenced definitions ahead of rx_pred_.
The nlm AR(1) estimation model BUILDS and RUNS with correct symbolic gradients
(the sensitivity model has the AR covariance derivative via llikNormDsd and the
cor^dt derivative).

FIXED (first-record NaN): added `lag0()`/`diff0()`/`lead0()` (return 0 not NA on
the first record); AR(1) estimation uses lag0 + a NaN-safe `1-is.na(lag(...))`
first-record indicator, so phi=0 (marginal) on the first obs with no `0*NaN`. The
nlm AR(1) objective is finite and CORRECT (varies with params; truth is optimal)
and the symbolic gradients (sensitivity model) are all finite and non-zero,
including the AR covariance derivative (llikNormDsd) and the cor^dt derivative.

REMAINING (range transform for the AR correlation): the nlm fit does not converge
(params stay at init) because OUTSIDE [0,1) the objective is degenerate (cor>=1 ->
sqrt(1-phi^2) NaN -> spuriously low), so the UNBOUNDED nlm line search stalls when
it steps cor past 1. FIX (the author's earlier request): reparameterize the AR
correlation on an unbounded scale during estimation, cor = expit(theta) (theta =
logit(cor)), with the ini transformed and the report back-transformed -- a
parameter-transform feature spanning the ini/theta/report handling. AR(1)
SIMULATION, the lag/diff/lag0 fixes, opt-out asserts, and the symbolic-gradient
AR estimation model are complete.
