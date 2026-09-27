# rxode2 5.1.8

This release exists mainly to fix the installation failure that makes 5.1.7
uninstallable on the two r-devel clang flavors.  The remaining changes are
features and bug fixes collected since 5.1.7; the full list is in NEWS.md.

## Incoming check failures fixed in this version

* `Installation failed` on `r-devel-linux-x86_64-debian-clang` and
  `r-devel-linux-x86_64-fedora-clang`.

      omp.h:546:39: error: expected 'match', 'adjust_args', or 'append_args'
                    clause on 'omp declare variant' directive
        #pragma omp begin declare variant match(device={kind(host)})
      Rinternals.h:996:17: note: expanded from macro 'match'
        #define match                   Rf_match

  `Rinternals.h` defines `match` as a macro for `Rf_match` unless
  `R_NO_REMAP` is set, and LLVM's `omp.h` spells a clause of its
  `declare variant` pragma `match(...)`.  Where R's headers were included
  first, the macro expanded inside the pragma and the compile failed.  Both
  places that include `omp.h` -- `src/rxomp.h` for the package and
  `inst/include/rxode2_model_shared.h` for the C code rxode2 generates and
  compiles on the user's machine -- now hide the macro across the include
  with `#pragma push_macro("match")` / `#undef match` /
  `#pragma pop_macro("match")`.  Nothing else is affected: the guard is
  confined to the `#include` line, and on a compiler whose `omp.h` has no
  such pragma it is a no-op.

  We do not have a clang 23 image to check against, so we reproduced the
  failure locally with clang 18, whose `omp.h` carries the same
  `declare variant match(...)` lines: without the guard clang gives exactly
  the diagnostic above, and with it the package and the generated model code
  both compile.  Neither side of the collision is version specific.

## Additional issues

* `gcc-UBSAN` reported `Status: OK` for 5.1.7.  The `runtime error` lines in
  that run's `00install.out` are all in the TBB sources bundled with
  RcppParallel, not in rxode2.  As of this version rxode2 no longer links to
  RcppParallel, StanHeaders or RcppEigen at all -- the Stan-based `linCmt()`
  kernels moved to the new package 'rxode2lincmt', which is already on CRAN --
  so those sources are no longer part of this package's build.

## R CMD check results

Status: OK under `NOT_CRAN=true` (the full test suite).

Status: 2 NOTEs under `--as-cran`, both described below.

* `Number of updates in past 6 months: 7`.  This upload is required for the
  package to remain on CRAN: 5.1.7 does not install on
  `r-devel-linux-x86_64-debian-clang` or `r-devel-linux-x86_64-fedora-clang`,
  and that has to be corrected rather than left to the next scheduled
  release.  The update frequency is a consequence of the fix being mandatory,
  not of a faster release cadence.

* `Compilation used the following non-portable flag(s):
  -mno-omit-leaf-frame-pointer`.  This flag comes from the Ubuntu
  distribution build of R itself (`R CMD config CFLAGS`), not from the
  package.  It is local to our check machine and has not appeared on CRAN's.

## Known NOTE on r-oldrel (not fixed, deliberately)

`Found non-API call to R: 'DATAPTR'`, on r-oldrel-macos-arm64,
r-oldrel-macos-x86_64 and r-oldrel-windows-x86_64.

This is the backport published in "Writing R Extensions", section "Some
backports":

    #if R_VERSION < R_Version(4, 6, 0)
    # define DATAPTR_RW(x) DATAPTR(x)

and the same manual's "Some API replacements for non-API entry points" names
this exact case as the exception: "One exception is that a writable pointer
may need to be returned by an ALTREP Dataptr method.  The function
`DATAPTR_RW` can use for this purpose."

rxode2 returns solved output columns as compact ALTREP vectors so large `id`
and `sim.id` columns are never materialized, which requires a `Dataptr`
method.  On R >= 4.6.0 it calls `DATAPTR_RW` directly; the `DATAPTR` fallback
is compiled only on older R, which is why the NOTE appears on r-oldrel and
nowhere else.  Replacing it would mean giving up the compact representation,
so we have kept the documented backport.

## Retained from the previous submission

The `configure` workaround that drops `-Wall` from the Fortran compile line
when the Fortran compiler identifies itself as `flang` and reports the flag
unusable is unchanged, and `r-devel-linux-x86_64-debian-gcc` is now OK.  We
remain happy to drop it if you would rather the configuration carried the fix.

## Test environments

* local: Ubuntu 24.04, R 4.6.1; `R CMD check --as-cran` and a full run with
  `NOT_CRAN=true`.

## revdepcheck results

We checked all 30 reverse dependencies against both the CRAN and the dev
version of this package, with `NOT_CRAN=true` so that each one ran its
complete test suite rather than the smaller CRAN-visible subset.

 * We saw 0 new problems
 * We failed to check 0 packages

Five packages report an ERROR against this version.  Each reports the same
ERROR against the CRAN version, so none of them is a new problem, and all
five come from tests or vignettes that CRAN's own checks do not run.  For
completeness:

* babelmixr2 -- one PopED assertion (`test-poped.R:302`), where
  `evaluate_design()` returns an OFV, FIM and RSE about 0.1 to 1 per cent away
  from the values the test expects (`[ FAIL 1 | PASS 382 ]`).  The numbers are
  identical under both versions of rxode2.  Reported upstream as
  nlmixr2/babelmixr2#223.

* monolix2rx -- four `vdiffr` plot-snapshot comparisons.  These compare
  rendered SVG against stored snapshots and so depend on the local graphics
  and font stack.

* nlmixr2scm -- its `workers="auto"` sizing asks for more threads than this
  machine has ("Requested 21 workers x 11 rxode2 threads = 231 threads, but
  only 22 cores are available") and stops.  This depends on the core count of
  the check machine.

* PKbioanalysis -- it keeps a duckdb database at a fixed per-user path outside
  the check directory, so checking both versions at once on one machine makes
  them contend for the same file ("Could not set lock on file
  .../PKbioanalysis/samples.db").  The check aborts in the package's own test
  helper before any test runs, so it reports nothing about the package's
  behavior with rxode2; checked on its own it passes.

* shinyMixR -- its vignette resolves an image through a relative path
  (`knitr::include_graphics("../man/figures/screen1.png")`) that this setup
  does not find.  CRAN's own checks of shinyMixR are OK on every flavor that
  can install rxode2, so this looks specific to our check setup rather than to
  the package.

nlmixr2est is the only package whose result differs between the two versions,
and it differs in this version's favour.  It exercises rxode2 the hardest and
skips most of its suite on CRAN; run in full it passes against this version
(`Status: OK`, `[ FAIL 0 | WARN 32 | SKIP 10 | PASS 16566 ]`), while against
the CRAN version one saem assertion fails.
