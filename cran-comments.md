# rxode2 5.1.5

This is a bug-fix follow-up to 5.1.4.

This is submitted within a few days of the last update because without
it the next version of sundialr package cannot submitted; this package
broke their reverse dependencies.

This is also submitted because the windows version of RcppParallel (6.0.0) +
rxode2ll causes this package to crash without a revision.

Also online and in CRAN the rxode2 interaction with `stringfish`
causes rxode2 not to load.  Therefore the `qs2` dependency was also
removed since it seems to be broken.

The remaining changes are bug fixes (compiled-model cache key
stability, event-table `ev$id` indexing, `delay()`/`past()` models
with `if`/`else`, and a batched multi-subject `linCmt()` cross-subject
leak); see NEWS.md for the full list.

## R CMD check results

On all platforms I see the standard "Days since last update" for this
maintainer follow-up.

This is due to the CRAN reverse dependency check of sundialr as well
as the release of RcppParallel 6.0.0 which causes rxode2 to crash on
windows systems.

With this followup, sundialr is no longer blocked from their
submission, and rxode2 will not cause a segmentation fault in windows.

## Reverse dependencies

The downstream packages (nlmixr2est, babelmixr2, nonmem2rx,
monolix2rx) rely on the stable `$predDf`/`$iniDf` interface, which is
unchanged in this release. They also function correctly.
