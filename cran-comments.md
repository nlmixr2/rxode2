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
  first the macro expanded inside the pragma and the compile failed.  Both
  places that include `omp.h` -- `src/rxomp.h` for the package and
  `inst/include/rxode2_model_shared.h` for the C code rxode2 generates and
  compiles on the user's machine -- now hide the macro across the include
  with `#pragma push_macro("match")` / `#undef match` /
  `#pragma pop_macro("match")`.  The guard is confined to the `#include`
  line, and on a compiler whose `omp.h` has no such pragma it does nothing.

  We have no clang 23 image, so we reproduced the failure locally with
  clang 18, whose `omp.h` carries the same `declare variant match(...)`
  lines: without the guard clang gives exactly the diagnostic above, and
  with it the package and the generated model code both compile.  Neither
  side of the collision is version specific.

## Sanitizers

* `gcc-UBSAN` reported `Status: OK` for 5.1.7.  The `runtime error` lines in
  that run's `00install.out` are all in the TBB sources bundled with
  RcppParallel, not in rxode2.  As of this version rxode2 no longer links to
  RcppParallel, StanHeaders or RcppEigen at all -- the Stan-based `linCmt()`
  kernels moved to the new package 'rxode2lincmt', already on CRAN -- so
  those sources are no longer part of this package's build.

* We ran this version through R-hub's `clang-ubsan` container: `Status: OK`,
  with the examples and the tests run under the sanitizer and no
  `runtime error:` or `SUMMARY: UndefinedBehaviorSanitizer` output.

* We also checked this version on R-hub's `m1-san` image (ASAN + UBSAN,
  macOS arm64, R-devel), since an M1-SAN problem was reported to us for
  5.1.7: `Status: OK`.  The examples, the examples under `--run-donttest`
  and the tests all pass, with no `runtime error:` and no
  `SUMMARY: AddressSanitizer` / `UndefinedBehaviorSanitizer` output.

  Two notes on that run.  It installs a reduced set of suggested packages:
  `Hmisc`, reached only through the suggested `xgxr`, cannot be built on that
  image because its Fortran uses the intrinsic module `iso_fortran_env` and
  the image's `flang` cannot resolve it -- with the full Suggests the check
  never got as far as compiling rxode2.  None of rxode2's own Fortran sources
  use that module.  And as on CRAN's own sanitizer runs, `NOT_CRAN` is unset,
  so the tests that ran are the CRAN-level subset.

## R CMD check results

Status: OK under `NOT_CRAN=true`, which runs the full test suite (41778
passing assertions, no failures, no skips in the event-translator gates).

Status: 2 NOTEs under `--as-cran`.

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
and `sim.id` columns are never materialised, which requires a `Dataptr`
method.  On R >= 4.6.0 it calls `DATAPTR_RW` directly; the `DATAPTR` fallback
is compiled only on older R, which is why the NOTE appears on r-oldrel and
nowhere else.  Using anything else there would mean giving up the compact
representation, so we have kept the documented backport.

## Retained from the previous submission

The `configure` workaround that drops `-Wall` from the Fortran compile line
when the Fortran compiler identifies itself as `flang` and reports the flag
unusable is unchanged, and `r-devel-linux-x86_64-debian-gcc` is now OK.  We
remain happy to drop it if you would rather the configuration carried the fix.

## Test environments

* local: Ubuntu 24.04, R 4.6.1; `R CMD check --as-cran` and a full run with
  `NOT_CRAN=true`.
* R-hub: `clang-ubsan`.

## revdepcheck results

We checked 30 reverse dependencies with `NOT_CRAN=true`, so each package's
full test suite ran rather than the subset it exposes to CRAN, against both
the CRAN and the development version of rxode2.

We saw no new problems.

Five packages fail identically on both versions, none of them for a reason in
rxode2:

* babelmixr2 -- `evaluate_design()` in its own PopED test no longer matches
  the expected OFV/FIM/RSE.  Identical against rxode2 5.1.7 and this version;
  reported upstream (nlmixr2/babelmixr2#223).
* monolix2rx -- `vdiffr` SVG plot snapshots differ from the stored ones, which
  is a property of the local graphics and font stack.
* nlmixr2scm -- its own `workers = "auto"` sizing refuses to run on this
  machine ("Requested 21 workers x 11 rxode2 threads ... but only 22 cores
  are available").
* PKbioanalysis -- keeps a duckdb database at a fixed path in the user's home
  directory, so checking two versions of it concurrently deadlocks on that one
  file.  Checked on its own it gives `checking tests ... OK`.
* shinyMixR -- a vignette includes an image by a path relative to the package
  source (`../man/figures/...`).  Our harness runs the extracted vignette code
  from a working directory where that path does not exist, which is not how
  the code runs from an installed package; the file itself is present in the
  tarball.  CRAN checks this package cleanly on every flavour.

Going the other way, nlmixr2est -- the package that exercises rxode2 the
hardest -- passes its complete suite against this version
(`[ FAIL 0 | WARN 32 | SKIP 10 | PASS 16566 ]`, `Status: OK`) while failing
one test against 5.1.7.
