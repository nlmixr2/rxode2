# rxode2 5.1.5

This is a bug-fix follow-up to 5.1.4.  It is submitted within a few days of the
last update because it also hardens the build against an external dependency:
the SUNDIALS public headers are now vendored and `LinkingTo: sundialr` has been
dropped, so the already-vendored SUNDIALS C sources always compile against
headers from the same SUNDIALS release instead of drifting when `sundialr`
updates its bundled copy (#1155).  The hard `qs2` (and `stringfish`) dependency
was also removed.

The remaining changes are bug fixes (compiled-model cache key stability,
event-table `ev$id` indexing, `delay()`/`past()` models with `if`/`else`, and a
batched multi-subject `linCmt()` cross-subject leak); see NEWS.md for the full
list.

## Test environments

* Local: Ubuntu 24.04, R 4.6.1

## R CMD check results

0 errors | 0 warnings | 2 notes

* One note is the standard "Days since last update" for this maintainer follow-up.
* One note reports a non-portable compilation flag
  (`-mno-omit-leaf-frame-pointer`) injected by the local R build's system flags,
  not by the package.

## Reverse dependencies

The downstream packages (nlmixr2est, babelmixr2, nonmem2rx, monolix2rx) rely on
the stable `$predDf`/`$iniDf` interface, which is unchanged in this release.
