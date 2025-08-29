# rxode2 4.1.0

- Adds work-around for odd Rstudio bug that makes nlmixr2/rxode2 difficult to use

- Change rxui `$` evaluation when completing in rstudio, fixes strange
  calculations popping up in `rstudio` (#909)

- Add orphan `rxode2` model unloading when using `rxUnloadAll()`, and
  change the return type to always be a boolean.

- Add `assertRxUiIovNoCor` to assert IOVs have no correlations in them.

- Handle the levels for inter-occasion variability in the ui better (#614)

- Create a new function `mix()` that will allow mixture models to be
    simulated in preparation of mixture support in `nlmixr2`.  This
    allows mixture models to be specified as: `v = mix(v1, p1, v2, p2,
    v3)` where the probability of having `v=v1` is modeled by `p1`,
    `v=v2` is modeled by `p2`, and `v=v3` is modeled by probability
    `1-p1-p2`.

- Created new functions `mlogit()` and `mexpit()` to convert
  probabilities used in mixture models to log-scaled values.
  `mlogit()` converts the probabilities to log-scaled values (using
  root-finding) and `mexpit()` converts the log values into
  probabilities.  The equation for the conversion of log to
  probabilities is $p_i = \frac{exp(x_i)}{1+\sum_{j=1}^{N-1}exp(x_j)}$

- Added new assertion `assertRxUiNoMix` which throws an error when a
  mixture model is present (ie `mix()`)

- Fix for label processing when calling `rxode2(uiModel)`
