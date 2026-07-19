# rxode2 5.1.3

## Reverse-dependency note (pre-existing `nlmixr2est` segfault)

Reverse-dependency checks of `nlmixr2est` (6.0.1) and, through it,
`nlmixr2autoinit` (1.0.1) can segfault in examples that run a "cold"
`nls`/`nlm` fit as the very first model operation in a fresh session
(e.g. `nlmixr2autoinit::Fit_1cmpt_iv(..., est.method = "nls")`).

This crash is **not** caused by nor new to this `rxode2` submission.  It
is a pre-existing use-before-initialization bug in the *released*
`nlmixr2est` 6.0.1: in `nlmSetup()` (src/nlm.cpp) it calls the rxode2
accessor `getRxNsub(rx)` before `rx` has been assigned via
`rx = getRxSolve_()`, so on a cold first fit `rx` is `NULL` and is
dereferenced.

We confirmed this is version-independent by reproducing the identical
crash with the *current CRAN* stack -- `rxode2` 5.1.2 + `nlmixr2est`
6.0.1 + `nlmixr2autoinit` 1.0.1 -- giving the same backtrace
(`getRxNsub(rx = 0x0)` from `nlmSetup` at `nlm.cpp:146`).  Both the
current CRAN `rxode2` (5.1.2) and this submission (5.1.3) crash
identically, so no regression is introduced here.

The fix belongs in `nlmixr2est` (reorder so `rx = getRxSolve_()`
precedes the `getRxNsub(rx)` call) and is already in place in the
development version of `nlmixr2est`; an updated `nlmixr2est` will be
submitted to CRAN.

In addition, this `rxode2` release hardens the C accessors exposed
through the function-pointer API so that being handed an
uninitialized solve no longer crashes the R process: instead of
dereferencing a NULL/unset `rx_solve`, they now raise a normal,
catchable R error stating that the solving environment is not set up.
This turns the pre-existing `nlmixr2est` 6.0.1 crash into a graceful
error while the `nlmixr2est` fix propagates through CRAN.

## Reverse dependencies

We ran `revdepcheck` on all 26 CRAN reverse dependencies (the `nlmixr2`
suite, `babelmixr2`, `campsis`, `ggPMX`, `monolix2rx`, `nonmem2rx`,
`posologyr`, `ruminate`, `shinyMixR`, `ubiquity`, `xpose.xtras`, ...),
comparing CRAN `rxode2` 5.1.2 with this submission.  All 26 pass with
no new errors, warnings, or notes attributable to `rxode2`.

`revdepcheck` initially flagged one "new problem" in `PKbioanalysis`,
but it is a local false positive of parallel checking: its tests open a
shared per-user DuckDB file that lives outside the isolated check
library, so the concurrent "old" and "new" check jobs raced for the
file lock and one failed to acquire it.  Re-running `PKbioanalysis`
alone (a single worker) passes cleanly with no errors.

The `shinyMixR` vignette error (a missing figure inside its own
package) is present identically with both the CRAN and the submitted
`rxode2`, so it is not a new problem.
