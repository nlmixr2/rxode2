# rxode2 Performance Optimization Plan

Analyzed: 2026-05-15

This document captures concrete, incremental steps to make rxode2 the fastest
ODE pharmacometric solver available. Each item is scoped small enough to
implement, benchmark, and merge independently.

---

## Benchmark harness (do this first)

**`bench-harness`** — Create `bench/` with reproducible scripts:
- 1-cmt bolus (single subject)
- 3-cmt infusion (single subject)
- Large ODE system (10+ states)
- Population simulation (1000 subjects, liblsoda)
- Population simulation (1000 subjects, linCmt)

Use `bench::mark()` to capture wall time + memory allocations as baselines
before any change and after each optimization.

---

## A. Parallelism  *(highest impact)*

### A1. `par-indlin` — Parallelize `par_indLin()`
**File:** `src/par_solve.cpp`

`par_indLin()` has a `// FIXME parallel` comment and runs all subjects
**serially**. Every other solver (`par_liblsoda`, `par_linCmt`) uses OpenMP.
Add the same striped parallel loop used in `par_liblsodaR`:

```cpp
#pragma omp parallel for num_threads(cores)
for (int thread = 0; thread < cores; thread++) {
  for (int solveid = thread; solveid < nsolve; solveid += cores) {
    ...
    ind_indLin(rx, solveid, update_inis);
  }
}
```

Also needs per-thread seed management matching the other `par_` functions.
**Expected gain:** n-core speed-up on the inductive linearization path.

---

### A2. `omp-schedule-dynamic` — Dynamic scheduling in `par_liblsoda()`
**File:** `src/par_solve.cpp`

`par_liblsoda()` uses `#pragma omp parallel for` with default static chunking.
Subjects vary widely in solve cost (sparse vs. dense events, SS, etc.).
Switch to dynamic scheduling:

```cpp
#pragma omp parallel for schedule(dynamic, 1) num_threads(op->cores)
```

The existing `ordId` sort (heaviest subjects first) already helps, but dynamic
scheduling handles residual imbalance. Should also be applied to `par_liblsodaR`
which uses the manual striped loop — consider consolidating to one approach.
**Expected gain:** 10–30% better parallel efficiency for heterogeneous populations.

---

### A3. `df-parallel-fill` — Parallelize output data frame assembly
**File:** `src/rxode2_df.cpp`

The main fill loop over subjects/sims/times in `rxode2_df()` is single-threaded.
For large nsim × nsub this becomes the bottleneck *after* the parallel solve.

Strategy:
1. First pass (already fast): count per-subject row counts → compute exclusive
   prefix-sum to get each subject's write offset.
2. Second pass (parallelize): each thread fills its subjects' rows independently.

```cpp
#pragma omp parallel for schedule(dynamic, 4) num_threads(op->cores)
for (int csub = 0; csub < nsub * nsim; csub++) { ... }
```

**Expected gain:** near-linear scale-up of post-solve df construction.

---

## B. Memory allocation hot-path  *(second highest impact)*

### B1. `lsoda-ctx-pool` — Per-thread LSODA context pool
**Files:** `src/par_solve.cpp`, `src/lsoda.c`, `src/lsoda.h`

`ind_liblsoda0()` calls `lsoda_create_ctx()` and `lsoda_free()` for **every
subject** — one malloc/free pair per solve. For 1000 subjects that is 1000
heap allocations in the hot loop.

Pre-allocate one context per thread (matching the `__linCmtA`/`__linCmtB`
vector pattern in `src/linCmt.cpp`):

```cpp
std::vector<lsoda_context_t*> __lsodaCtx;

extern "C" void ensureLsodaCtx(int nCores) {
  if ((int)__lsodaCtx.size() < nCores) {
    __lsodaCtx.resize(nCores, nullptr);
    for (int i = 0; i < nCores; i++)
      if (!__lsodaCtx[i]) __lsodaCtx[i] = lsoda_create_ctx();
  }
}
```

Add a `lsoda_reset_ctx()` that zeroes the integration state without freeing,
and call it between subjects instead of destroy+create.
**Expected gain:** eliminates ~1000 malloc/free calls per population solve.

---

### B2. `rwork-pool` — Per-thread Fortran LSODA workspace
**File:** `src/par_solve.cpp`

`global_rworkp` and `global_iworkp` are single global arrays for the Fortran
LSODA path. Multi-threaded use of these causes false sharing and data races.

Create per-thread pools sized at setup time:

```cpp
std::vector<double*> thread_rwork;
std::vector<int*>    thread_iwork;
```

Initialize in `rxOptionsIniEnsure()` and index by `rx_get_thread(cores)`.
**Expected gain:** eliminates false sharing, enables safe Fortran LSODA parallel use.

---

### B3. `cbind-theta-prealloc` — Reuse theta/omega cbind buffer
**File:** `src/cbindThetaOmega.cpp`

The theta/omega parameter matrix is re-allocated on every `rxSolve()` call.
Cache the pointer and capacity; skip re-allocation when `nsub * nsim` and
parameter count match the previous call.
**Expected gain:** reduces setup overhead for repeated solve calls (e.g. MCMC).

---

## C. Compiler flags and generated-code hints

### C1. `codegen-opt-flags` — Optimize generated model compilation
**File:** `R/build.R` (and wherever `inline` compilation is invoked)

The user model's ODE RHS is the innermost hot loop but is compiled with R's
default flags. Explicitly pass higher optimization flags:

```r
# In the inline compilation call:
extra_flags <- "-O3 -fno-math-errno -funroll-loops"
```

`-fno-math-errno` is a safe subset of `-ffast-math` that avoids errno side
effects while enabling fused operations. `-funroll-loops` helps the typical
2–3 state PK model.
**Expected gain:** 15–40% faster ODE RHS evaluation.

---

### C2. `pkg-cflags-lto` — Evaluate LTO for package itself
**File:** `src/Makevars.in`

Link-time optimization allows the compiler to inline across `lsoda.c`,
`par_solve.cpp`, and the generated model code translation units:

```makefile
PKG_CFLAGS   = -D_isrxode2_ -O3
PKG_CXXFLAGS = $(SHLIB_OPENMP_CXXFLAGS) -D_isrxode2_ ... -O3 -flto
PKG_LIBS     = ... -flto
```

Gate behind a configure check; some toolchains (LLVM, older GCC) need `-flto=thin`.
**Expected gain:** 5–15% via cross-TU inlining of hot solver helpers.

---

### C3. `codegen-restrict` — `__restrict__` on generated ODE functions
**File:** `src/codegen.c`

The generated `dydt(int *neq, double t, double *y, double *ydot)` signature has
`y` and `ydot` that never alias. Add `__restrict__`:

```c
// Generated output becomes:
void _model_dydt(int * restrict neq, double t,
                 double * restrict __y, double * restrict __ydot) {
```

Guard with `#ifdef __GNUC__` / `#ifdef _MSC_VER` via a macro in a shared header.
**Expected gain:** enables auto-vectorization of element-wise RHS computations.

---

### C4. `codegen-hot-attr` — `__attribute__((hot))` on generated functions
**File:** `src/codegen.c`

Mark generated `dydt` and `calc_lhs` with `__attribute__((hot))` so the
compiler places them in the hot instruction path and prioritizes inlining:

```c
__attribute__((hot))
void _model_dydt(...) { ... }
```

Guard with `#ifdef __GNUC__`.
**Expected gain:** reduced branch misprediction and icache pressure.

---

## D. Algorithm improvements

### D1. `dop853-dense` — Dense output for DOP853
**File:** `src/dop853.c`, `src/par_solve.cpp`

DOP853 supports dense output: given an accepted step `[t_old, t]`, it can
evaluate the solution at any interior `t*` using a polynomial without extra
ODE function calls. Currently the solver is restarted (`istate=1`) at every
observation/event time.

Enable the `solout` callback to exploit dense output for observation-only
timepoints (EVID=0) between dose events, skipping the restart overhead.
**Expected gain:** fewer ODE function evaluations when sampling is denser than
the natural step size (common in PK/PD).

---

### D2. `liblsoda-continuation` — Audit unnecessary state resets
**File:** `src/par_solve.cpp`

Liblsoda continuation mode (`ctx->state = 2`) re-uses the cached Jacobian
and step-size history. The code currently resets `ctx->state = 1` in several
places. Audit each reset:
- Dose events (EVID ≠ 0): reset is **correct** — state is discontinuous.
- Observation events (EVID = 0): reset is **unnecessary** — remove it.
- The `preSolve()` path: check if a reset is forced anywhere it shouldn't be.

**Expected gain:** fewer Jacobian re-evaluations, especially for dense sampling.

---

## E. Output and data construction

### E1. `df-altrep` — ALTREP for repetitive output columns
**File:** `src/rxode2_df.cpp`

For population simulations, several columns are predictable sequences:
- `sim.id`: repeats `1..nsim` for each subject block
- `id`: repeats `1..nsub` for each sim block  
- `time`: same vector repeated nsim×nsub times when all subjects share a schedule

Replace these with ALTREP objects (compact integer/real sequences). R's
`seq_len` is already ALTREP; a custom ALTREP class for strided repetition is
~50 lines of C code.
**Expected gain:** O(1) construction and ~zero memory for these columns.

---

### E2. `et-batch-c` — C-level batch event table replication
**Files:** `src/et.cpp`, `R/et.R`

For population sims where all subjects share the same event schema, `et()`
builds one template then replicates it in R. The R-level replication is slow
for large subject counts. Implement:

```cpp
SEXP replicate_et(SEXP template_et, int n_subjects);
```

that writes directly into a pre-allocated `rxEt` structure, bypassing R list
manipulation.
**Expected gain:** faster dataset construction for population templates.

---

### E3. `ettran-cache` — Cache `etTran()` result
**Files:** `src/rxData.cpp`, `R/rxsolve.R`

`etTran()` is called on every `rxSolve()` even when the event table has not
changed. Add an xxHash or FNV fingerprint of the input event table (just the
raw column vectors, no copying), store the translated result in the `rxUi`
environment, and skip retranslation on hit.

Invalidate the cache when: event table data changes, model changes, or
`rxSolve()` is called with `resetCache = TRUE`.
**Expected gain:** near-zero etTran cost for repeated calls (MCMC/bootstrap).

---

## F. Covariate and parameter handling

### F1. `covariate-interp-cache` — Cache covariate interpolation per step
**Files:** `src/par_solve.cpp`, generated model code

`_update_par_ptr()` linearly scans the covariate time grid on every ODE
function call (including Jacobian sub-calls at the same time). Add a
per-individual two-entry cache:

```c
// In rx_solving_options_ind:
double cov_cache_t;   // last time covariates were updated
int    cov_cache_idx; // last bracket index found
```

On entry to `_update_par_ptr()`, if `t == cov_cache_t` return immediately.
**Expected gain:** eliminates redundant binary/linear searches during stiff
integration where Jacobian is evaluated multiple times per step.

---

### F2. `rnorm-batch` — Batch eta/epsilon generation
**Files:** `src/rxode2_df.cpp`, `src/cvPost.cpp`

Omega/sigma draws are currently generated per-subject or per-matrix call.
Pre-generate all normal variates for the entire simulation in one vectorized
call using the existing SIMD-friendly threefry engine:

```cpp
// Generate all nsub * n_eta variates at once
threefry_fill_normal(all_etas, nsub * n_eta, seed);
```

This improves PRNG throughput (better vectorization) and separates the random
generation from the solver loop for cleaner profiling.
**Expected gain:** faster random variate generation for large nsim/nsub.

---

## Execution order

| Step | ID                       | Area            | Est. Impact |
|------|--------------------------|-----------------|-------------|
| 0    | bench-harness            | Infrastructure  | prerequisite |
| 1    | par-indlin               | Parallelism     | 🔴 High     |
| 2    | omp-schedule-dynamic     | Parallelism     | 🔴 High     |
| 3    | lsoda-ctx-pool           | Memory          | 🔴 High     |
| 4    | df-parallel-fill         | Parallelism     | 🔴 High     |
| 5    | codegen-opt-flags        | Compiler        | 🟠 Medium   |
| 6    | rwork-pool               | Memory          | 🟠 Medium   |
| 7    | codegen-restrict         | Compiler        | 🟠 Medium   |
| 8    | codegen-hot-attr         | Compiler        | 🟡 Low-Med  |
| 9    | pkg-cflags-lto           | Compiler        | 🟡 Low-Med  |
| 10   | liblsoda-continuation    | Algorithm       | 🟠 Medium   |
| 11   | dop853-dense             | Algorithm       | 🟠 Medium   |
| 12   | df-altrep                | Output          | 🟠 Medium   |
| 13   | ettran-cache             | Data prep       | 🟠 Medium   |
| 14   | et-batch-c               | Data prep       | 🟡 Low-Med  |
| 15   | covariate-interp-cache   | Parameters      | 🟡 Low-Med  |
| 16   | cbind-theta-prealloc     | Memory          | 🟡 Low      |
| 17   | rnorm-batch              | Parameters      | 🟡 Low      |

---

## Notes

- Each item should be gated on a benchmark showing improvement before merge.
- Items in group A (parallelism) and B (memory) are likely to interact —
  measure them individually, then together.
- Compiler flag changes (group C) are platform-sensitive; use `configure`
  checks and fall back gracefully on Windows/CRAN toolchain.
- ALTREP (E1) requires R >= 3.5.0 which rxode2 already requires.
