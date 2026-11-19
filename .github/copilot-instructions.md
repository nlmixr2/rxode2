# Copilot Instructions for `rxode2`

## Overview

`rxode2` is an R package for solving and simulating ODE-based
pharmacometric models. It parses an rxode2 mini-language into C code,
compiles the generated code into a shared library, and dynamically
loads it into R. The repository contains R, C, C++, and Fortran code.

## Development workflow

- Install locally with `R CMD INSTALL .`.

- Regenerate documentation and generated development artifacts with `devtools::document()`.
- Run the full test suite with `find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test()"`.
- Run a single test file with `find src -name "*.so" -o -name "*.o" | xargs rm -f 2>/dev/null; NOT_CRAN=true Rscript -e "devtools::test(filter='basic')"` and swap the filter as needed.
- Run package checks with `devtools::check()` after removing compiled objects from `src/`.

## Architecture

### ODE solving pipeline

1. Users define models with `ini({})` and `model({})` blocks, or as strings/files in the rxode2 mini-language.
2. Parsing happens in `src/tran.c` using the grammar in `inst/tran.g`, which is precompiled into `src/tran.g.d_parser.h`.
3. `src/codegen.c` converts the parsed AST into generated C code.
4. The generated code is compiled to a shared library and loaded into R.
5. `rxSolve()` drives solving from `src/par_solve.cpp`, using LSODA, DOP853, and OpenMP-backed parallel solving where applicable.

### Key subsystems

- `R/ui.R`, `R/rxUiGet.R`, `R/asRxui.R`: model parsing and `rxUi` object access.
- `R/piping*.R`: piping helpers that copy and modify `rxUi` objects through `.copyUi()`.
- `R/et.R`, `src/et.cpp`, `src/etTran.cpp`: event table construction and translation.
- `R/linCmt.R`, `src/linCmt.cpp`: analytical linear compartment models and gradients.
- `src/rxode2_df.cpp`, `src/rxData.cpp`: solved output data-frame handling.
- `tests/testthat/`: testthat edition 3 tests, usually wrapped in `rxTest({...})`, which requires `NOT_CRAN=true`.

## File-level guidance

- `inst/tran.g`: dparser grammar for the rxode2 mini-language.
- `src/tran.c`: parser entry point.
- `src/codegen.c`: code generation from parsed AST.
- `src/par_solve.cpp`: main parallel ODE solver.
- `R/rxode2.R`: `rxode2()` entry point.
- `R/rxsolve.R`: `rxSolve()` implementation.
- `R/build.R`: code-generation helpers used during development.
- `inst/include/rxode2.h`: main C header for generated model code.

## Coding conventions

### R

- Use `camelCase` for exported functions.
- Use `.camelCase` for internal helpers and local variables.
- `camelCase` for function arguments
- Use standard S3 naming like `generic.class`.
- Avoid new `snake_case` names unless matching an existing snake_case generic.
- Use American English spelling and ASCII text.

### C and C++

- Use `(Rf_error)(...)` instead of `Rf_error(...)` to avoid macro conflicts.
- In C files, keep `#define STRICT_R_HEADERS` and `#define USE_FC_LEN_T` patterns intact when relevant.
- In C++ files, keep `#define R_NO_REMAP` patterns intact when relevant.
- Treat OpenMP support as conditional through `rxomp.h`.

## Generated files

Do not edit these manually:

- `src/tran.g.d_parser.h`
- `R/rxrandomui.R`
- `inst/include/rxode2parse_control.h`

Regenerate them through the existing development workflow, usually `devtools::document()`.

## When changing parser or codegen behavior

- If you change `inst/tran.g`, regenerate derived files instead of editing generated outputs directly.
- If you need to inspect generated model code after a compile, use ``cat(suppressMessages(rxode2::rxLastCompile())$c)` and compiler output with `cat(suppressMessages(rxode2::rxLastCompile())$stderr)`.


## vexp context tools <!-- vexp v2.0.12 -->

**MANDATORY: use `run_pipeline` -- do NOT grep, glob, or read files manually.**
vexp returns pre-indexed, graph-ranked context in a single call.

### Workflow
1. `run_pipeline` with your task description -- ALWAYS FIRST (replaces all other tools)
2. Make targeted changes based on the context returned
3. `run_pipeline` again only if you need more context

### Available MCP tools
- `run_pipeline` -- **PRIMARY TOOL**. Runs capsule + impact + memory in 1 call.
  Auto-detects intent. Includes file content. Example: `run_pipeline({ "task": "fix auth bug" })`
- `get_skeleton` -- compact file structure
- `index_status` -- indexing status
- `expand_vexp_ref` -- expand V-REF placeholders in v2 output

### Agentic search
- Do NOT use built-in file search, grep, or codebase indexing -- always call `run_pipeline` first
- If you spawn sub-agents or background tasks, pass them the context from `run_pipeline`
  rather than letting them search the codebase independently

### Smart Features
Intent auto-detection, hybrid ranking, session memory, auto-expanding budget.

### Multi-Repo
`run_pipeline` auto-queries all indexed repos. Use `repos: ["alias"]` to scope. Run `index_status` to see aliases.
<!-- /vexp -->
