# rxode2 5.0.2

- Allow state-dependent `dur()`, `rate()`, `alag()`, `mtime()` now
  allow states to modify their behavior.  The state value at the time
  of the event is used to calculate any changes.

- Fix: all six ODE solve loops now use precomputed `timeThread` values for
  event times instead of recomputing via `getTime_()` with `ypNA`, preventing
  NA propagation for any state-dependent lag scenario.

- Export the internal `.rxGetSeed()` and `.rxSetSeed()` for use in the
  `nlmixr2save` package.

- Bug fix for `.copyUi()` with the new format (5.0+) of rxode2 ui models

- With new versions of R, `getOption()` is no longer a bottleneck, so
  syncing to local variables is no longer done internally

- Allow transforms to return `NA`.

- Drop `magrittr` and use `|>` instead of `%>%` in the examples
  (requires R 4.1)

- Change default model serialization to `bzip2` and move binary code
  generation inside of C.

- Fix where getting seed saves/modifies the RNG scope, as well as a bug fix
  for restoring the random seed state
