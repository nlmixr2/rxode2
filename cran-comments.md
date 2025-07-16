# rxode2 4.0.0

- Add more information errors about `NA`s during solving.

- Fix `rxDerived()` for mixed vector and non-vector input.

- Fix model variables for `alag(cmt)` when they are defined before
  `d/dt()` or `linCmt()`

- Just in time use of `state.ignore` in the model variables, fixes
  negative length error observed in #857.

- Fix steady state bug with time-varying covariates.  Now the
  covariates are inferred at the time of the steady state (instead of
  searching through the subject based on the projected time).

- Rework the linear solved systems to use the wnl solutions, and
  threaded linear systems solve (for non-gradient solutions). This new
  method closes a variety of linear compartment model bugs (#261,
  #272, #441, #504, #564, #717, #728, #827, and #855)

- Added new types of bounds for event tables:

  - 3 point bounds `et(list(c(low, mid, high)))` when specified this way,
    they will not change. Perfect for use with `babelmixr2`'s `PopED` (#862,
    #863, #854)

  - Intervals simulated by normal values instead of uniform.  In this
    case the first seen interval will be 3 elements with NA at the end
    `et(list(c(mean, sd, NA), c(mean, sd)))`, and the other elements
    can simply be 2 declaring the `c(mean, sd)`

  - Of course the uniform windows of `et(list(c(low, high)))` still work

  - Currently these different types of windows cannot be mixed.

- Add ability to pipe a list or named numeric as an eta with
  `%>% ini(~eta)`

- Added a fix for event tables where expanding IDs in non-sequential
  order.  In particular if the first ID is not the minimum ID when expanding
  the first event table, the smallest ID was not in the output table. Now
  the smallest ID is in the event table. (Fixes #878, #869, #870)

- Added ability to pipe `ini()` or `lotri()`, or any other expression
  that can be converted to an ini with `as.ini()`. Also allows `ini()`
  expressions to be converted to lotri with `as.lotri()`. Fixes #871

- Added new type of variability expression for simulation and
  estimation with focei and likelihood related methods: `+var()`. This
  changes standard deviation parameters to variance parameters.

- Added new type of endpoint expression for focei estimation
  `+dv()`. This only transforms the data and not the predictions. I
  can only see it being useful in model linearization.

- Bug fix for parameters that are in both input (`$params`) and output
  (`$lhs`) that respects the order of the `$lhs` declaration (Fixes
  #876)

- Add `rxFixRes` to literally fix the residual estimates in a model (#889)

- Now modeled duration of 0 is treated as a bolus dose (#892)
