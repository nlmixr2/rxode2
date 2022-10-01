# CRAN Comments

The following platforms were tested with a succesful check: MacOS release, Windows
  release, ubuntu devel, and ubuntu release.
  
Also the win-builder, rhub was checked and ran successfully.

* The clang errors are taken care of, as well as the latex errors.
 
* This shortens compilation time (as requested) by moving
  log-likelihood functions to `rxode2ll`.
  
* This also changes the `rxode2` model structure, which will change
  `nlmixr2est` and `nlmixr2extra` causing them to fail without an
  update.  The `nlmixr2est` is also ready and will be submitted as
  soon as `rxode2` is accepted.  Once `nlmixr2est` is released,
  `nlmixr2extra`, `nlmixr2plot` (to fix upcoming ggplot2 3.4 issues),
  and `nlmixr2` will be submitted (to fix a bug that doesn't capture
  the model name).  Once accepted, this will allow `ggPMX`, and
  `xpose.nlmixr2` to run correctly.
  
* Campsis will run with this new `rxode2` without any modifications
  (the tests were successful).
