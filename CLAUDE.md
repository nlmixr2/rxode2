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
   - LSODA (`src/lsoda.c`, `src/dlsoda.f`) — stiff/non-stiff adaptive
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

- Supports NONMEM-compatible event IDs (evid 0-4) as well as rxode2
  specific evids like 5 (replace event), 6 (multiply event) and 7
  (phantom/transit). Internally the EVIDs are translated to rxode2
  specific evids documented
  https://nlmixr2.github.io/rxode2/articles/rxode2-events-classic.html

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
- Use American English spelling for consistency and do not use unicode characters

### C/C++ Conventions

- Use `(Rf_error)(...)` (wrapped in parens) rather than
  `Rf_error(...)` to prevent macro conflicts
- C files use `#define STRICT_R_HEADERS` and `#define USE_FC_LEN_T`
- C++ files use `#define R_NO_REMAP` to avoid R API name pollution
- OpenMP support is conditional via `rxomp.h`

### Generated Files (do not edit manually)

- `src/tran.g.d_parser.h` — generated from `inst/tran.g` via dparser
- `R/rxrandomui.R` — generated by `.generateRandomUiFuns()` in `R/build.R`
- `inst/include/rxode2parse_control.h` — generated by `genDefine()` in `R/build.R`

# context-mode — MANDATORY routing rules

You have context-mode MCP tools available. These rules are NOT optional — they protect your context window from flooding. A single unrouted command can dump 56 KB into context and waste the entire session.

## BLOCKED commands — do NOT attempt these

### curl / wget — BLOCKED
Any Bash command containing `curl` or `wget` is intercepted and replaced with an error message. Do NOT retry.
Instead use:
- `ctx_fetch_and_index(url, source)` to fetch and index web pages
- `ctx_execute(language: "javascript", code: "const r = await fetch(...)")` to run HTTP calls in sandbox

### Inline HTTP — BLOCKED
Any Bash command containing `fetch('http`, `requests.get(`, `requests.post(`, `http.get(`, or `http.request(` is intercepted and replaced with an error message. Do NOT retry with Bash.
Instead use:
- `ctx_execute(language, code)` to run HTTP calls in sandbox — only stdout enters context

### WebFetch — BLOCKED
WebFetch calls are denied entirely. The URL is extracted and you are told to use `ctx_fetch_and_index` instead.
Instead use:
- `ctx_fetch_and_index(url, source)` then `ctx_search(queries)` to query the indexed content

## REDIRECTED tools — use sandbox equivalents

### Bash (>20 lines output)
Bash is ONLY for: `git`, `mkdir`, `rm`, `mv`, `cd`, `ls`, `npm install`, `pip install`, and other short-output commands.
For everything else, use:
- `ctx_batch_execute(commands, queries)` — run multiple commands + search in ONE call
- `ctx_execute(language: "shell", code: "...")` — run in sandbox, only stdout enters context

### Read (for analysis)
If you are reading a file to **Edit** it → Read is correct (Edit needs content in context).
If you are reading to **analyze, explore, or summarize** → use `ctx_execute_file(path, language, code)` instead. Only your printed summary enters context. The raw file content stays in the sandbox.

### Grep (large results)
Grep results can flood context. Use `ctx_execute(language: "shell", code: "grep ...")` to run searches in sandbox. Only your printed summary enters context.

## Tool selection hierarchy

1. **GATHER**: `ctx_batch_execute(commands, queries)` — Primary tool. Runs all commands, auto-indexes output, returns search results. ONE call replaces 30+ individual calls.
2. **FOLLOW-UP**: `ctx_search(queries: ["q1", "q2", ...])` — Query indexed content. Pass ALL questions as array in ONE call.
3. **PROCESSING**: `ctx_execute(language, code)` | `ctx_execute_file(path, language, code)` — Sandbox execution. Only stdout enters context.
4. **WEB**: `ctx_fetch_and_index(url, source)` then `ctx_search(queries)` — Fetch, chunk, index, query. Raw HTML never enters context.
5. **INDEX**: `ctx_index(content, source)` — Store content in FTS5 knowledge base for later search.

## Subagent routing

When spawning subagents (Agent/Task tool), the routing block is automatically injected into their prompt. Bash-type subagents are upgraded to general-purpose so they have access to MCP tools. You do NOT need to manually instruct subagents about context-mode.

## Output constraints

- Keep responses under 500 words.
- Write artifacts (code, configs, PRDs) to FILES — never return them as inline text. Return only: file path + 1-line description.
- When indexing content, use descriptive source labels so others can `ctx_search(source: "label")` later.

## ctx commands

| Command | Action |
|---------|--------|
| `ctx stats` | Call the `ctx_stats` MCP tool and display the full output verbatim |
| `ctx doctor` | Call the `ctx_doctor` MCP tool, run the returned shell command, display as checklist |
| `ctx upgrade` | Call the `ctx_upgrade` MCP tool, run the returned shell command, display as checklist |
