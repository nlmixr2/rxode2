# Plan: Dynamic Dose Injection via `et_()` in rxode2 Models

**Branch:** `et-inline`  
**Goal:** Allow model equations to call `et_(time, amt, evid)` to push dosing events
dynamically during solving, reusing the existing `pushDosingEvent()` infrastructure.

---

## Design Summary

codegen.c already emits `rx_solving_options_ind *_ind = &(_solveData->subjects[_cSub]);`
at the top of **every** generated function (`dydt`, `calc_jac`, `inis`, `F`, `Lag`, `Rate`).
`_ind` is therefore already a local variable in scope wherever user model code runs.

**No thread-local storage is needed.** codegen.c simply emits the 4-arg C call
`et_((double)(time), (double)(amt), (int)(evid), _ind)` and `_ind` resolves to
the already-generated local. No header or solver changes are required to expose `_ind`.

In the mini-language, `et_(time, amt, evid)` is the user-facing 3-argument form.
The optional `ind` argument in the description (`et_(time, amt, evid, ind)`) is a
special keyword that codegen treats identically — it substitutes the already-in-scope
`_ind` either way.

---

## Files to Change

### 1. `inst/include/rxode2.h`
**Add** after the existing `t_dydt` typedef block:
```c
/* Dynamic dose injection from model equations. */
static inline void et_(double _time, double _amt, int _evid,
                       rx_solving_options_ind *_ind) {
  /* SS dose check — SS requires full time horizon, cannot be injected dynamically */
  {
    int _wh, _cmt2, _wh100, _whI, _wh0;
    getWh(_evid, &_wh, &_cmt2, &_wh100, &_whI, &_wh0);
    int _flag = _wh % 100;
    if (_flag == EVID0_SS   || _flag == EVID0_SS0  ||
        _flag == EVID0_SS2  || _flag == EVID0_SS20 ||
        _flag == EVID0_SSINF) {
      _ind->ssDoseN++;
      return;
    }
  }
  /* Past-time check — record violation, emit deferred warning in par_solve() */
  if (_time < _ind->curTime) {
    if (_ind->pastDoseN < ET_PAST_DOSE_MAX) {
      _ind->pastDoseTime[_ind->pastDoseN]       = _time;
      _ind->pastDoseSolverTime[_ind->pastDoseN] = _ind->curTime;
    }
    _ind->pastDoseN++;
    return;
  }
  pushDosingEvent(_time, _amt, _evid, _ind);
}
```

Note: `getWh`, `EVID0_*`, and `pushDosingEvent` are all defined in
`rxode2parseHandleEvid.h`. Verify that header is included before `rxode2.h`
in generated model compilation units; if not, move or forward-declare as needed.

### 2. `src/par_solve.cpp`
**Two changes only — no new dose-processing logic.**

The existing `handleExtraDose()` (in `par_solve.h`) is already called in every
solve loop iteration. When `et_()` calls `pushDosingEvent()`, it sets
`ind->extraSorted = 0` and updates `ind->extraDoseNewXout`. `handleExtraDose()`
picks this up on the very next loop iteration: it sorts lazily when
`extraSorted == 0` and respects `extraDoseNewXout` to stop the solver at the
injected dose time. No new checking code is needed in any solve loop.

The only additions to `par_solve.cpp` are:

#### 2a. Initialize tracking fields before each subject solve
Wherever `ind->extraDoseN[0] = 0` and related extra-dose fields are zeroed at
the start of each subject (search for that pattern), add:
```c
ind->pastDoseN = 0;
ind->ssDoseN   = 0;
```

#### 2b. Emit deferred warnings at the end of `par_solve()` (~line 4538)
See section 4c for the full warning loop — place it just before
`par_progress_0 = 0;` at the end of `par_solve()`.

### 3. `R/dfIni.R`, `R/symengine.R`, and `src/codegen.c`

#### 3a. Register `et_` in the function translation table

Function registration for codegen lives in `.parseEnv$.rxode2parseDf`, whose
initial value is the data frame hardcoded in `R/dfIni.R`. That file is generated
by running code in `R/symengine.R` (via `.rxodeBuildCode()` or equivalent).

Add a row to the data frame in `R/dfIni.R` for `et_`:

| column       | value              | notes                                              |
|--------------|--------------------|----------------------------------------------------|
| `rxFun`      | `"et_"`            | name as written in the model                       |
| `fun`        | `"et_"`            | C function name in `rxode2.h`                      |
| `type`       | `"rxode2_et_fun"`  | new type — see codegen.c below                     |
| `package`    | `"rxode2"`         |                                                    |
| `packageFun` | `"et_"`            |                                                    |
| `argMin`     | `3L`               | `time`, `amt`, `evid` — `_ind` is synthetic        |
| `argMax`     | `3L`               | user never writes the 4th arg                      |
| `threadSafe` | `1L`               | safe — uses only the already-in-scope local `_ind` |

After editing `R/symengine.R` to include the `et_` entry, re-run the generation
step so `R/dfIni.R` is regenerated and commit both files.

#### 3b. Add the `rxode2_et_fun` type to `src/codegen.c`

`_ind` is already declared as a local variable in every generated function, so
the emitted call simply uses it by name.

Search codegen.c for the dispatch on `type` strings (look for `"rxode2_fn"`,
`"rxode2_fn2"`, etc.). Add a new case for `"rxode2_et_fun"` that emits:

```c
et_(arg1, arg2, (int)(arg3), _ind);
```

as a **void statement** (no assignment, no return value). The explicit `(int)`
cast on the third argument is required because model expressions produce `double`
values but `et_()` takes `int` for the evid. codegen must emit this cast for the
`rxode2_et_fun` type — it is not applied automatically.

The `ind` keyword (4-arg form `et_(time, amt, evid, ind)`) should be treated as
a recognized synonym: codegen discards the 4th user argument and always
substitutes `_ind`.

No grammar (`inst/tran.g`) or UI (`R/ui.R`) changes are needed — once `et_` is
registered in `.rxode2parseDf` the parser and model-validation layer pick it up
automatically.

### 4. `inst/include/rxode2parseStruct.h` and `src/par_solve.cpp` — thread-safe past-dose tracking

`Rf_warning()` is **not thread-safe** and must never be called from inside the
OpenMP parallel region where `dydt` runs. Instead, record violations per-subject
into the subject's own `ind` struct (each thread owns its `ind` exclusively, so
no mutex is needed), then emit all warnings from the main thread after the
parallel solve completes.

#### 4a. Add tracking fields to `rx_solving_options_ind` (`inst/include/rxode2parseStruct.h`)

```c
#define ET_PAST_DOSE_MAX 10   /* cap on recorded violations per subject */
int    pastDoseN;             /* number of past-dose violations (may exceed cap) */
double pastDoseTime[ET_PAST_DOSE_MAX];        /* requested (bad) dose times */
double pastDoseSolverTime[ET_PAST_DOSE_MAX];  /* solver's curTime when rejected */
int    ssDoseN;               /* count of SS dose attempts dropped (SS unsupported) */
```

Initialize `pastDoseN = 0` and `ssDoseN = 0` wherever the rest of `ind` is
zeroed before each subject solve (search `par_solve.cpp` for where
`ind->extraDoseN[0] = 0` is set).

Because this modifies `rxode2parseStruct.h`, clean all `.o` files before
rebuilding: `rm -f src/*.o && R CMD INSTALL .`

#### 4b. Full `et_()` implementation is in `inst/include/rxode2.h` (see section 1)

Both rejection conditions (SS dose and past-time) are implemented directly inside
the `et_()` inline function defined in section 1.  No separate guard code is
needed elsewhere.

#### 4c. Emit warnings at the end of `par_solve()` (`src/par_solve.cpp` ~line 4538)

`par_solve()` (lines 4498–4540) is the single top-level function that dispatches
to every solver method (`par_linCmt`, `par_liblsoda`, `par_liblsodaR`,
`par_lsoda`, `par_dop`). Placing the warning loop here means it runs exactly
once, after all subjects are solved, regardless of which ODE method was used.

Add the following just before the existing `par_progress_0=0;` at the end of
`par_solve()`:

```c
  /* emit deferred past-dose warnings — Rf_warning() is not thread-safe so
     violations were recorded per-subject during the parallel solve above */
  for (unsigned int _si = 0; _si < rx->nSub; _si++) {
    rx_solving_options_ind *_ind2 = &(rx->subjects[_si]);
    if (_ind2->pastDoseN > 0) {
      int _nReport = (_ind2->pastDoseN < ET_PAST_DOSE_MAX)
                       ? _ind2->pastDoseN : ET_PAST_DOSE_MAX;
      for (int _pi = 0; _pi < _nReport; _pi++) {
        Rf_warning("et_(): subject %d: requested dose time (%.4g) is earlier than "
                   "current solver time (%.4g); dose dropped",
                   _ind2->id + 1,
                   _ind2->pastDoseTime[_pi],
                   _ind2->pastDoseSolverTime[_pi]);
      }
      if (_ind2->pastDoseN > ET_PAST_DOSE_MAX) {
        Rf_warning("et_(): subject %d: %d additional past-time dose(s) not shown",
                   _ind2->id + 1,
                   _ind2->pastDoseN - ET_PAST_DOSE_MAX);
      }
    }
    if (_ind2->ssDoseN > 0) {
      Rf_warning("et_(): subject %d: %d steady-state dose(s) cannot be dynamically "
                 "injected and were dropped (SS requires the full time horizon)",
                 _ind2->id + 1, _ind2->ssDoseN);
    }
  }

---

## Solver Correctness: Future vs Past Doses

The reference point for "past" is `ind->curTime` — the time at which the ODE
solver currently sits when `et_()` is called (i.e. the `t` visible inside the
model equations at that moment).  A requested injection time that is earlier
than `ind->curTime` is in the past relative to the solver state and cannot be
applied.

| Requested injection time         | Behavior                                                              |
|----------------------------------|-----------------------------------------------------------------------|
| `time > ind->curTime`            | Correct — `handleExtraDose()` picks it up on a future step           |
| `time == ind->curTime`           | Correct — same step, `handleExtraDose()` fires it immediately        |
| `time < ind->curTime`            | **Warn and skip** — earlier than current solver state; dose dropped  |

The guard in `et_()` records the violation into the per-subject `_ind` struct
(thread-safe — each subject's thread owns its `_ind` exclusively) and returns
without calling `Rf_warning()`. Warnings are emitted by the main thread after
the parallel solve completes. See section 4 for the full mechanism.

---

## Tests (`tests/testthat/test-et-inline.R`)

```r
rxTest({
  test_that("et_() bolus at state threshold", {
    m <- rxode2({
      if (depot < 0.5 * dose0) et_(t + 6, dose0, 101)  # cmt=1, bolus
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    })
    # ... solve and check central has a second peak ~6h after first dose
  })

  test_that("et_() infusion injection with explicit turn-off", {
    # Rate-based infusion into cmt 1:
    #   evid = EVIDF_INF_RATE*10000 + cmt*100 + EVID0_REGULAR
    #        = 1*10000 + 1*100 + 1 = 10101
    # A rate infusion requires TWO et_() calls:
    #   1. turn ON  at t_start  with +rate amt
    #   2. turn OFF at t_start+duration with -rate amt (same evid)
    # Without the negative-amt turn-off the infusion runs indefinitely.
    infRate <- 50   # mg/hr
    dur     <- 2    # hr
    m <- rxode2({
      # fire once when central drops below threshold
      if (isSameTime(t, 6)) {
        et_(t,       infRate,  10101)   # turn on
        et_(t + dur, -infRate, 10101)   # turn off after dur hours
      }
      d/dt(central) = -cl/v * central
    })
    et0 <- et(0, amt = 0, cmt = 1) %>% add.sampling(seq(0, 12, by = 0.5))
    params <- list(cl = 2, v = 10)
    s <- rxSolve(m, params, et0)
    # central should rise during [6, 8] then decline
    expect_true(s$central[s$time == 7]  > s$central[s$time == 6])   # rising
    expect_true(s$central[s$time == 12] < s$central[s$time == 8])   # declining after off
  })

  test_that("et_() dose earlier than current solver time is dropped", {
    m <- rxode2({
      et_(t - 10, 100, 101)   # t-10 is always < ind->curTime
      d/dt(depot) = -ka * depot
    })
    et0 <- et(0, amt = 100, cmt = 1) %>% add.sampling(seq(0, 24, by = 1))
    # solve without error; the always-past injection should produce no extra dose
    s <- rxSolve(m, list(ka = 0.5), et0)
    # depot should follow simple 1-cmt elimination with no second bump
    expect_true(all(diff(s$depot) <= 0))  # monotone decline after t=0
    # extraDoseN should remain 0 (or all doses were rejected)
    # If a warning is issued, it must not be an error
  })

  test_that("et_() warns when requested dose time is earlier than current solver time", {
    # Guard: if _time < ind->curTime the violation is recorded in ind->pastDose*
    # (thread-safe — each subject owns its ind exclusively under OpenMP).
    # Rf_warning() is called by the main thread after the parallel solve, so
    # expect_warning() still intercepts it before rxSolve() returns.
    # ind->curTime is the solver's current position in time, NOT the time of
    # any previously scheduled dose.
    # Here the model fires at solver time t=12 and requests a dose at t=2.
    m <- rxode2({
      if (isSameTime(t, 12)) et_(2.0, 50, 101)
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    })
    et0 <- et(0, amt = 100, cmt = 1) %>%
      add.sampling(c(0, 6, 12, 18, 24))
    params <- list(ka = 0.5, cl = 2, v = 10)
    # Warning names subject id, requested time, and current solver time
    expect_warning(
      s <- rxSolve(m, params, et0),
      regexp = "solver time"
    )
    # central should show NO second peak — the past dose was not applied
    central_vals <- s$central
    # After the initial peak the curve should be declining at t=18 and t=24
    expect_true(central_vals[s$time == 18] < central_vals[s$time == 12])
    expect_true(central_vals[s$time == 24] < central_vals[s$time == 18])
  })

  test_that("et_() does not affect subjects without the condition", {
    # two subjects; only subject 1 hits the threshold
    m <- rxode2({
      if (depot < 0.5 * dose0) et_(t + 6, dose0, 101)
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    })
    # subject 1: low dose0 so depot falls below threshold quickly
    # subject 2: dose0=0 so condition never fires (depot starts at 0)
    ev <- et(id = 1:2, amt = c(100, 0), cmt = 1) %>%
      add.sampling(seq(0, 24, by = 2))
    s <- rxSolve(m, list(ka = 0.5, cl = 2, v = 10, dose0 = 50), ev)
    s1 <- s[s$id == 1, ]
    s2 <- s[s$id == 2, ]
    # subject 1 should have a second peak (extra dose fired)
    expect_true(any(diff(s1$central) > 0))
    # subject 2 stays at zero throughout
    expect_true(all(s2$central == 0))
  })
})
```

---

## Commit Sequence

| # | Commit message                                                                        | Files                                                                          |
|---|---------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| 1 | `feat: add past-dose tracking fields to rx_solving_options_ind`                      | `inst/include/rxode2parseStruct.h`                                             |
| 2 | `feat: add et_() inline to rxode2.h with thread-safe past-dose flag recording`       | `inst/include/rxode2.h`                                                        |
| 3 | `feat: init pastDoseN/ssDoseN per subject and emit deferred warnings in par_solve()` | `src/par_solve.cpp`                                                            |
| 4 | `feat: register et_() in function translation table`                                 | `R/symengine.R`, `R/dfIni.R`                                                   |
| 5 | `feat: add rxode2_et_fun codegen type to emit et_() with in-scope _ind`              | `src/codegen.c`                                                                |
| 6 | `test: add tests for et_() dynamic dose injection`                                   | `tests/testthat/test-et-inline.R`                                              |
| 7 | `docs: news entry for et_() model-level dose injection`                              | `NEWS.md`                                                                      |

Each commit should be buildable and the package installable (`R CMD INSTALL .`)
before moving to the next. Clean `.o` files between steps when any header changes:
```bash
rm -f src/*.o && R CMD INSTALL .
```

---

## Open Questions / Risks

1. **`pushDosingEvent` include order** — `rxode2.h` is included in generated model
   `.c` files. Confirm that `rxode2parseHandleEvid.h` (which defines
   `pushDosingEvent`) is also included before `rxode2.h`, or move `pushDosingEvent`
   into `rxode2.h` directly.

2. **SS doses** — Steady-state doses (`EVID0_SS`, `EVID0_SS0`, `EVID0_SS2`,
   `EVID0_SS20`, `EVID0_SSINF`) require the full time horizon and cannot be
   dynamically injected. They are silently dropped inside `et_()` (incrementing
   `_ind->ssDoseN`) and a deferred warning is emitted per-subject at the end of
   `par_solve()`.

3. **Model cache** — No signature change to any generated function; `_ind` is
   already emitted by codegen. Existing compiled models remain valid without
   recompilation. However, `rxode2parseStruct.h` is modified (past-dose tracking
   fields), so run `rm -f src/*.o && R CMD INSTALL .` after commit 1.

4. **`pastDoseN` overflow** — If a model fires `et_()` in the past more than
   `ET_PAST_DOSE_MAX` times for one subject, only the first `ET_PAST_DOSE_MAX`
   violations are recorded in detail; a follow-up warning reports the remaining
   count. The `pastDoseN` counter always increments regardless.
