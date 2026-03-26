# rxode2 Security & Memory Audit

Generated: 2026-03-26
Auditor: Claude Code (claude-sonnet-4-6)

---

## Summary

| Severity | Count |
|----------|-------|
| Critical | 2 |
| High     | 9 |
| Medium   | 4 |
| Low      | 3 |

---

## Critical Issues

### C-01 · Buffer overflow via off-by-one in `_strdup_printf`

**File:** `src/strdup_printf.c:23`
**Category:** Buffer overflow / data truncation

```c
int s = vsnprintf(zero, 0, fmt, va);   // s = chars needed, NOT counting '\0'
char * rt = malloc(s*sizeof(char));    // allocates s bytes — one short!
vsnprintf(rt, s, fmt, va);             // writes s-1 chars + '\0' — last char silently dropped
```

`vsnprintf` with a zero buffer size returns the number of bytes that *would* be written **excluding** the null terminator. The allocation must be `s + 1` bytes. As written, `vsnprintf` is handed a buffer of length `s`, so it writes only `s-1` real characters followed by `'\0'`, silently truncating the last character of every formatted string. Any downstream consumer reading the result as a full string will receive a corrupted (shortened) value.

**Fix:** `malloc((s + 1) * sizeof(char))` and `vsnprintf(rt, s + 1, fmt, va)`.

---

### C-02 · NULL pointer dereference after `malloc` in `lsoda_alloc`

**File:** `src/lsoda.c:310–324`
**Category:** NULL pointer dereference / crash

```c
_rxC(memory) = malloc(offset);           // line 310 — not checked before use

_rxC(yh) = (double **)((char *)_rxC(memory) + yhoff);   // line 312 — crash if NULL
_rxC(wm) =  (double **)((char *)_rxC(memory) + wmoff);  // line 313
// ...
```

The function returns `_rxC(memory) != NULL` at line 326, but the pointer arithmetic on lines 312–324 occurs **before** the NULL check. If `malloc` fails (e.g., under memory pressure with a large model), every pointer assignment will compute `NULL + offset`, yielding an invalid pointer.  The function is never called from a context that passes an extremely large `neq`, but under low-memory conditions this path is reachable.

**Fix:** Check `if (_rxC(memory) == NULL) return 0;` immediately after line 310.

---

## High Issues

### H-01 · Unchecked `malloc` for `gsigma` (sigma matrix buffer)

**File:** `src/rxData.cpp:2270, 2286`
**Category:** NULL pointer dereference

```c
// line 2270
_globals.gsigma = (double*)malloc((rx->neps * rx->neps * sigmaList.size() + 2 * rx->neps) * sizeof(double));
// immediately used at line 2273 via std::copy — no NULL check

// line 2286
_globals.gsigma = (double*)malloc((rx->neps * rx->neps + 2 * rx->neps) * sizeof(double));
// immediately used at line 2287–2288 via std::copy — no NULL check
```

---

### H-02 · Unchecked `malloc` for `gomega` (omega matrix buffer)

**File:** `src/rxData.cpp:2315, 2330`
**Category:** NULL pointer dereference

```c
// line 2315
_globals.gomega = (double*)malloc((2 * rx->neta + rx->neta * rx->neta * omegaList.size()) * sizeof(double));
// used at line 2318 via std::copy — no NULL check

// line 2330
_globals.gomega = (double*)malloc((2 * rx->neta + rx->neta * rx->neta) * sizeof(double));
// used at line 2331 via std::copy — no NULL check
```

---

### H-03 · Unchecked `malloc` for `ordId` (subject ordering array)

**File:** `src/rxData.cpp:2784`
**Category:** NULL pointer dereference

```c
rx->ordId = _globals.ordId = (int*)malloc(nall * sizeof(int));
std::iota(rx->ordId, rx->ordId + nall, 1);   // crashes if malloc returned NULL
```

---

### H-04 · Unchecked `malloc` for `gall_times` (time vector)

**File:** `src/rxData.cpp` (near line 3550)
**Category:** NULL pointer dereference

```c
_globals.gall_times = (double*)calloc(5 * time0.size(), sizeof(double));
// followed by pointer arithmetic with no NULL check
```

---

### H-05 · Unchecked `malloc` for `gcov` (covariate buffer)

**File:** `src/rxData.cpp` (near line 3628)
**Category:** NULL pointer dereference

```c
_globals.gcov = (double*)calloc(ncov * amt.size(), sizeof(double));
// used immediately without NULL check
```

---

### H-06 · Unchecked `malloc` for `gSampleCov`

**File:** `src/rxData.cpp` (near line 3987)
**Category:** NULL pointer dereference

```c
_globals.gSampleCov = (int*)calloc(op->ncov * rx->nsub * rx->nsim, sizeof(int));
// used immediately without NULL check
// NOTE: the multiplication itself can overflow — see M-01
```

---

### H-07 · `lsoda_create_ctx` / `lsoda_create_opt` return unvalidated `malloc`

**File:** `src/lsoda.c:895–904`
**Category:** NULL pointer dereference in callers

```c
struct lsoda_context_t * lsoda_create_ctx(void) {
    struct lsoda_context_t * mem = malloc(sizeof(struct lsoda_context_t));
    return mem;   // returned without checking; callers must check
}
struct lsoda_opt_t * lsoda_create_opt(void) {
    struct lsoda_opt_t * mem = malloc(sizeof(struct lsoda_opt_t));
    return mem;   // same
}
```

If callers do not check the returned pointer for NULL before use, this is a crash vector. At minimum the functions should document that callers must check, or check internally and call `Rf_error`.

---

### H-08 · Unchecked `malloc` for `_strdup_printf` empty-string path

**File:** `src/strdup_printf.c:10`
**Category:** NULL pointer dereference

```c
if (fmt == NULL) {
    char * rt = malloc(2 * sizeof(char));
    rt[0] = '\0';   // crash if malloc returned NULL
    return rt;
}
```

---

### H-09 · Integer overflow in `nall` multiplication (subject × simulation count)

**File:** `src/rxData.cpp:2779`
**Category:** Integer overflow → wrong allocation size / heap corruption

```c
int nall = rx->nsub * rx->nsim;   // both are int; can overflow for large runs
```

If `rx->nsub` and `rx->nsim` together exceed ~2 billion, `nall` wraps to a negative or small positive value. This poisoned value is then used for `malloc(nall*sizeof(int))` (line 2784) producing a tiny allocation followed by writes far beyond its end.

A recent fix (PR #999 / commit `b90bce454`) already added an int64_t overflow check for `nSize` elsewhere in the solve path; the same pattern is needed here.

---

## Medium Issues

### M-01 · Integer overflow in multi-dimensional size calculations

**File:** `src/rxData.cpp` (multiple lines ~3987, 5237, 5267, 5332)
**Category:** Integer overflow → wrong allocation / heap corruption

Several allocations multiply three or more `int`-typed fields without promoting to `int64_t` first:

```c
_globals.gSampleCov = (int*)calloc(op->ncov * rx->nsub * rx->nsim, sizeof(int));
int n0 = rx->nall * state.size() * rx->nsim;
int nlin = rx->linB * 7 * rx->nsub * rx->nsim;
calloc(n3a_c + n3 + 4 * rxSolveDat->nSize + 1 * rx->nall * rx->nsim, sizeof(int));
```

All of these should use `int64_t` (or `size_t`) for the intermediate product before passing to `calloc`/`malloc`.

---

### M-02 · `lsoda_free_opt` frees `atol`/`rtol` unconditionally

**File:** `src/lsoda.c:907–912`
**Category:** Double-free / free of uninitialized pointer

```c
void lsoda_free_opt(struct lsoda_opt_t * opt) {
    free(opt->atol);
    free(opt->rtol);
    free(opt);
}
```

If `lsoda_create_opt` was called but `opt->atol` or `opt->rtol` were never set (left as garbage values from the raw `malloc`), calling `free` on them is undefined behavior. The struct should be zero-initialized (`calloc` or `memset`) in `lsoda_create_opt`.

---

### M-03 · OpenMP data races on `_globals` mutable state

**File:** `src/par_solve.cpp` (solve loops), `src/rxData.cpp`
**Category:** Race condition / data corruption

`_globals` is a module-level mutable struct accessed and written during the solve setup phase. The parallel OpenMP loops read from `_globals.*` fields (e.g., `gsigma`, `gomega`, `gall_times`) while setup writes are still completing. If `rxSolve` is called concurrently from multiple R threads (e.g., via `parallel::mclapply`), there is no mutex protecting the shared `_globals` writes.

---

### M-04 · `codegen.c` uses `sprintf` into fixed-size stack buffers

**File:** `src/codegen.c` (multiple locations)
**Category:** Stack buffer overflow potential

Generated symbol names are constructed with `sprintf` into fixed-size `char buf[...]` arrays. If model variable names exceed the fixed size, this overflows the stack buffer. Consider using `_strdup_printf` (once its off-by-one is fixed) or snprintf with length checks.

---

## Low Issues

### L-01 · `strdup_printf` result never freed in error paths

**File:** `src/codegen.c`, `src/tran.c`
**Category:** Memory leak

`_strdup_printf` returns heap-allocated strings. In several call sites the pointer is passed directly into `Rf_error(...)` without being freed first (the Rf_error longjmps, skipping any subsequent `free`). This leaks a small allocation per error, which is acceptable for error paths but worth noting.

---

### L-02 · `lsoda_free_opt` does not null-check `opt`

**File:** `src/lsoda.c:907`
**Category:** NULL pointer dereference (defensive)

```c
void lsoda_free_opt(struct lsoda_opt_t * opt) {
    free(opt->atol);   // crashes if opt == NULL
```

`free(NULL)` is safe by the C standard, but `opt->atol` with a NULL `opt` is not.

---

### L-03 · Missing `free` on `_globals.gParPos` in some teardown paths

**File:** `src/rxData.cpp`
**Category:** Memory leak

Audit of `rxSolveFree` / teardown code shows that some `_globals` members allocated conditionally (e.g., `gParPos`) may not be freed when early-exit paths are taken due to input validation failures mid-setup. These are process-lifetime allocations (refreshed on each solve call), so the leak is bounded, but repeated calls with invalid inputs accumulate waste.

---

## Architectural Notes

### A-01 · Global mutable state (`_globals`) limits re-entrancy

`_globals` is a single module-level struct that holds all inter-phase buffers. This design means rxode2 cannot be safely called re-entrantly (e.g., a model callback that itself calls `rxSolve`). The existing code guards against some of this at the R level, but the C level has no protection.

### A-02 · No bounds checking on `rx->subjects[i]` indexing

**File:** `src/par_solve.cpp`, `src/rxData.cpp`
Throughout the codebase, `&(rx->subjects[i])` is accessed in loops bounded by `nall = rx->nsub * rx->nsim`. If `nall` overflows (see H-09), these accesses go out-of-bounds on the `subjects` array.

### A-03 · Grammar-generated C code (`codegen.c`) uses `strcat`/`strcpy` in some paths

User-supplied model variable names flow through the code generator into C source. While the grammar restricts valid identifiers, there is no explicit length cap on variable names. Very long names could overflow fixed-size intermediate buffers in the code generator.

---

## Recommendations (Priority Order)

1. **Fix C-01 immediately** — the `_strdup_printf` off-by-one silently corrupts every formatted string and is used pervasively.
2. **Add `NULL` checks after every `malloc`/`calloc`** in `rxData.cpp` — call `Rf_error` on failure.
3. **Fix the `lsoda_alloc` NULL deref (C-02)** — add the early-return check before pointer arithmetic.
4. **Promote `nall` and multi-dimensional products to `int64_t`** (H-09, M-01) — following the pattern already established by PR #999.
5. **Zero-initialize `lsoda_opt_t`** in `lsoda_create_opt` to prevent M-02 double-free.
6. **Add a mutex or re-entrancy guard** around `_globals` writes (M-03) if concurrent use is anticipated.
