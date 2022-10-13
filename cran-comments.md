# CRAN Comments

The following platforms were tested with a succesful check: MacOS release, Windows
  release, ubuntu devel, and ubuntu release.
  
Also the win-builder, rhub was checked and ran successfully.

* The clang errors are taken care of, as well as the latex errors.
 
* This shortens compilation time (as requested) by moving
  log-likelihood functions to `rxode2ll`, parsing to `rxode2parse`,
  random number generation to `rxode2random` and event table 
  creation to `rxode2et`
  
* Also removed some tests from CRAN (but still kept them for testing
  externally) since the testing was taking 274s on win builder.
  
* This also has pre-release fixes for `ggplot2` 3.4.0 which is slated
  to be released at the end of October.
  
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
