# Changelog

## rxode2 5.0.1

- Change random number generation to always return doubles internally as
  well as no longer take a rxode2 individual structure, this is inferred
  by the thread number.

- Change string representation of model variables to internal binary C
  code (to avoid macOS M1 sanitizer issues with strings).

- Allow user to change the internal serialization type with
  `options("rxode2.serialize.type")`; Currently can be one of “qs2”,
  “qdata”, and “base”. This option must be set before `rxode2` is
  loaded, once loaded it keeps the option initially set.

- Removed lsoda `CDIR$ IVDEP` directive, as requested by CRAN.

## rxode2 5.0.0

CRAN release: 2025-11-28

- Better error for `tad(depot)` when `linCmt()` doesn’t include a depot
  compartment.

- Remove `qs` dependency; For rxode2 ui objects, use lists instead of
  serialized objects. The internal C++ code still generates `qs2`
  sterilization objects
  ([\#950](https://github.com/nlmixr2/rxode2/issues/950))

- Fixed translation for censoring/limit to account for a possible `CMT`
  variable before the `CENS` / `LIMIT` column
  ([\#951](https://github.com/nlmixr2/rxode2/issues/951),
  [\#952](https://github.com/nlmixr2/rxode2/issues/952))

- Added
  [`dmexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md)
  for getting the diagonal Jacobian.

- Added special handling of `mixest` and `mixunif`.

## rxode2 4.1.1

CRAN release: 2025-10-08

- Stacking for multiple-endpoint `ipredSim` now matches
  multiple-endpoint `sim`; Issue
  [\#929](https://github.com/nlmixr2/rxode2/issues/929)

- Fix occasional `$props` that threw an error with empty properties
  (when using properties like `tad0()`); Issue
  [\#924](https://github.com/nlmixr2/rxode2/issues/924)

- Allow mixture models
  [`mix()`](https://nlmixr2.github.io/rxode2/reference/mix.md) to be
  loaded with
  [`rxS()`](https://nlmixr2.github.io/rxode2/reference/rxS.md) as a step
  to support mixtures in nlmixr2’s focei; Issue
  [\#933](https://github.com/nlmixr2/rxode2/issues/933).

- Identify the correct transformation type for `iov` variables
  ([\#936](https://github.com/nlmixr2/rxode2/issues/936))

- Fix multiple compartment simulation edge cases where simulations were
  not being performed
  ([\#939](https://github.com/nlmixr2/rxode2/issues/939))

- When referencing `cmt` in models, the variable is forced to be `CMT`
  (related to [\#939](https://github.com/nlmixr2/rxode2/issues/939))

- Added ability to use `mixest` or `mixunif` to preserve the selected
  mixture estimates when performing a table step for a nlmixr2 mixture
  model ([\#942](https://github.com/nlmixr2/rxode2/issues/942))

## rxode2 4.1.0

CRAN release: 2025-08-29

- Change rxui `$` evaluation when completing in rstudio, fixes strange
  calculations popping up in `rstudio`
  ([\#909](https://github.com/nlmixr2/rxode2/issues/909))

- Add orphan `rxode2` model unloading when using
  [`rxUnloadAll()`](https://nlmixr2.github.io/rxode2/reference/rxUnloadAll.md),
  and change the return type to always be a boolean.

- Add `assertRxUiIovNoCor` to assert IOVs have no correlations in them.

- Handle the levels for inter-occasion variability in the ui better
  ([\#614](https://github.com/nlmixr2/rxode2/issues/614))

- Create a new function
  [`mix()`](https://nlmixr2.github.io/rxode2/reference/mix.md) that will
  allow mixture models to be simulated in preparation of mixture support
  in `nlmixr2`. This allows mixture models to be specified as:
  `v = mix(v1, p1, v2, p2, v3)` where the probability of having `v=v1`
  is modeled by `p1`, `v=v2` is modeled by `p2`, and `v=v3` is modeled
  by probability `1-p1-p2`.

- Created new functions
  [`mlogit()`](https://nlmixr2.github.io/rxode2/reference/mlogit.md) and
  [`mexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md) to
  convert probabilities used in mixture models to log-scaled values.
  [`mlogit()`](https://nlmixr2.github.io/rxode2/reference/mlogit.md)
  converts the probabilities to log-scaled values (using root-finding)
  and [`mexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md)
  converts the log values into probabilities. The equation for the
  conversion of log to probabilities is
  $p_{i} = \frac{exp\left( x_{i} \right)}{1 + \sum_{j = 1}^{N - 1}exp\left( x_{j} \right)}$

- Added new assertion `assertRxUiNoMix` which throws an error when a
  mixture model is present (ie
  [`mix()`](https://nlmixr2.github.io/rxode2/reference/mix.md))

- Fix for label processing when calling `rxode2(uiModel)`

## rxode2 4.0.3

CRAN release: 2025-07-24

- For CRAN’s m1 ASAN checks of nlmixr2est, loading and unloading the
  same dll or by deleting the dll and recreating the exact same code,
  and then loading the dll will cause the ASAN check to flag an odr
  violation. Because of this, a mechanism to not unload dlls has been
  added. This allows the next version of `nlmixr2est` to not have issues
  with Mac m1 san checks.

## rxode2 4.0.2

CRAN release: 2025-07-21

- At the request of CRAN, be a bit more careful so that names are not
  duplicated. Now include the md5 hash, a global counter and random 4
  digit and number combination. In addition add the name of the original
  function so it will be easier to debug in the future.

- Fall back to data.frame `rbind` when `rbind.rxSolve()` fails

## rxode2 4.0.1

CRAN release: 2025-07-17

- Add the ability to use `rbind` for solved `rxode2` frames.

- Fix `LTO` issue for `_rxode2_calcDerived`

## rxode2 4.0.0

CRAN release: 2025-07-16

- Add more information errors about `NA`s during solving.

- Fix
  [`rxDerived()`](https://nlmixr2.github.io/rxode2/reference/rxDerived.md)
  for mixed vector and non-vector input.

- Fix model variables for `alag(cmt)` when they are defined before
  `d/dt()` or `linCmt()`

- Just in time use of `state.ignore` in the model variables, fixes
  negative length error observed in
  [\#857](https://github.com/nlmixr2/rxode2/issues/857).

- Fix steady state bug with time-varying covariates. Now the covariates
  are inferred at the time of the steady state (instead of searching
  through the subject based on the projected time).

- Rework the linear solved systems to use the wnl solutions, and
  threaded linear systems solve (for non-gradient solutions). This new
  method closes a variety of linear compartment model bugs
  ([\#261](https://github.com/nlmixr2/rxode2/issues/261),
  [\#272](https://github.com/nlmixr2/rxode2/issues/272),
  [\#441](https://github.com/nlmixr2/rxode2/issues/441),
  [\#504](https://github.com/nlmixr2/rxode2/issues/504),
  [\#564](https://github.com/nlmixr2/rxode2/issues/564),
  [\#717](https://github.com/nlmixr2/rxode2/issues/717),
  [\#728](https://github.com/nlmixr2/rxode2/issues/728),
  [\#827](https://github.com/nlmixr2/rxode2/issues/827), and
  [\#855](https://github.com/nlmixr2/rxode2/issues/855))

- Added new types of bounds for event tables:

  - 3 point bounds `et(list(c(low, mid, high)))` when specified this
    way, they will not change. Perfect for use with `babelmixr2`’s
    `PopED` ([\#862](https://github.com/nlmixr2/rxode2/issues/862),
    [\#863](https://github.com/nlmixr2/rxode2/issues/863),
    [\#854](https://github.com/nlmixr2/rxode2/issues/854))

  - Intervals simulated by normal values instead of uniform. In this
    case the first seen interval will be 3 elements with NA at the end
    `et(list(c(mean, sd, NA), c(mean, sd)))`, and the other elements can
    simply be 2 declaring the `c(mean, sd)`

  - Of course the uniform windows of `et(list(c(low, high)))` still work

  - Currently these different types of windows cannot be mixed.

- Add ability to pipe a list or named numeric as an eta with
  `%>% ini(~eta)`

- Added a fix for event tables where expanding IDs in non-sequential
  order. In particular if the first ID is not the minimum ID when
  expanding the first event table, the smallest ID was not in the output
  table. Now the smallest ID is in the event table. (Fixes
  [\#878](https://github.com/nlmixr2/rxode2/issues/878),
  [\#869](https://github.com/nlmixr2/rxode2/issues/869),
  [\#870](https://github.com/nlmixr2/rxode2/issues/870))

- Added ability to pipe
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) or
  [`lotri()`](https://nlmixr2.github.io/lotri/reference/lotri.html), or
  any other expression that can be converted to an ini with
  [`as.ini()`](https://nlmixr2.github.io/rxode2/reference/as.ini.md).
  Also allows
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md)
  expressions to be converted to lotri with
  [`as.lotri()`](https://nlmixr2.github.io/lotri/reference/as.lotri.html).
  Fixes [\#871](https://github.com/nlmixr2/rxode2/issues/871)

- Added new type of variability expression for simulation and estimation
  with focei and likelihood related methods: `+var()`. This changes
  standard deviation parameters to variance parameters.

- Added new type of endpoint expression for focei estimation `+dv()`.
  This only transforms the data and not the predictions. I can only see
  it being useful in model linearization.

- Bug fix for parameters that are in both input (`$params`) and output
  (`$lhs`) that respects the order of the `$lhs` declaration (Fixes
  [\#876](https://github.com/nlmixr2/rxode2/issues/876))

- Add `rxFixRes` to literally fix the residual estimates in a model
  ([\#889](https://github.com/nlmixr2/rxode2/issues/889))

- Now modeled duration of 0 is treated as a bolus dose
  ([\#892](https://github.com/nlmixr2/rxode2/issues/892))

## rxode2 3.0.4

CRAN release: 2025-02-14

- Add stable hashes for rxUi objects
  ([\#838](https://github.com/nlmixr2/rxode2/issues/838),
  [\#689](https://github.com/nlmixr2/rxode2/issues/689))

- Fix for iov simulation
  ([\#842](https://github.com/nlmixr2/rxode2/issues/842))

- Fix for
  [`rxnbinom()`](https://nlmixr2.github.io/rxode2/reference/rxnbinom.md)
  called directly from R
  ([\#847](https://github.com/nlmixr2/rxode2/issues/847)) and expand it
  to match more close with R’s
  [`rnbinom()`](https://rdrr.io/r/stats/NegBinomial.html) including
  allowing named `mu=` calls. In rxode2 ui, these are also now allowed.

## rxode2 3.0.3

CRAN release: 2024-12-15

- Add `logit`/`expit` named expressions, that is `logit(x, high=20)`
  becomes `logit(x, 0, 20)` in ui models.

- Updated random ui models like `rxnorm(sd=10)` to accept complex
  numeric expressions like `rxnorm(sd=10+1)`.

- Updated random ui models to accept complex non-numeric expressions
  like `rxnorm(sd=a+b)`

- Rework the `tad()` and related functions so they use the same
  interface as compartments (this way they do not depend on the order of
  compartments); See
  [\#815](https://github.com/nlmixr2/rxode2/issues/815). For
  mu-referencing, Also allow dummy variables to ignore state
  requirements (ie `podo(depot)` in a single line will not error when
  parsing mu-referenced equations).

- Add `getRxNpars` to api. This allows the development version of
  `babelmixr2` to better check what model is loaded and unload/reload as
  necessary.

- Add
  [`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md)
  to rxode2 user function to get control information from something like
  `nlmixr2`

- Bug fix for tracking time after dose when dosing to 2 compartments
  occur at the exact same time
  ([\#804](https://github.com/nlmixr2/rxode2/issues/804),
  [\#819](https://github.com/nlmixr2/rxode2/issues/819))

- Change `transit()` model so that it uses `tad0()`, `podo0()` and
  related functions for a bit more stable simulation and estimation

- Fix compile flags to work with BH 1.87
  ([\#826](https://github.com/nlmixr2/rxode2/issues/826))

## rxode2 3.0.2

CRAN release: 2024-10-30

- Bug fix for `api`, the censoring function pointer has been updated
  ([\#801](https://github.com/nlmixr2/rxode2/issues/801)).

- Query `rxode2.verbose.pipe` at run time instead of requiring it to be
  set before loading `rxode2`.

- Have correct values at boundaries for `logit`, `expit`, `probit`, and
  `probitInv` (instead of `NA`). For most cases this does not break
  anything.

- Add a new style of user function that modifies the `ui` while parsing
  or just before using the function (in the presence of `data`).

- Used the new user function interface to allow all random functions in
  `rxode2` ui functions to be named. For example, you can use
  `rxnorm(sd=3)` instead of having to use `rxnorm(0, 3)`, although
  [`rxnorm()`](https://nlmixr2.github.io/rxode2/reference/rxnormV.md)
  still works.

## rxode2 3.0.1

CRAN release: 2024-09-22

- Explicitly initialize the order vector to stop valgrind warning
  (requested from CRAN)

## rxode2 3.0.0

CRAN release: 2024-09-18

### Breaking Changes

- The model properties was moved from `$params` to `$props` so it does
  not conflict with the low level `rxode2` model `$params`

- Error when specifying `wd` without `modName`

- With Linear and midpoint of a time between two points, how `rxode2`
  handles missing values has changed. When the missing value is lower
  than the requested time, it will look backward until it finds the
  first non-missing value (or if all are missing start looking forward).
  When the missing value is higher than the requested time, the
  algorithm will look forward until it finds the first non-missing value
  (or if all are missing, start looking backward).

- The order of ODEs is now only determined by the order of `cmt()` and
  `d/dt()`. Compartment properties, `tad()` and other compartment
  related variables no no longer affect compartment sorting. The option
  `rxode2.syntax.require.ode.first` no longer does anything.

- The handling of zeros “safely” has changed (see
  [\#775](https://github.com/nlmixr2/rxode2/issues/775))

  - when `safeZero=TRUE` and the denominator of a division expression is
    zero, use the Machine’s small number/`eps` (you can see this value
    with `.Machine$double.eps`)

  - when `saveLog=TRUE` and the x in the `log(x)` is less than or equal
    to zero, change this to `log(eps)`

  - when `safePow=TRUE` and the expression `x^y` has a zero for `x` and
    a negative number for `y` replace `x` with `eps`.

  Since the protection for divide by zero has changed, the results will
  also change. This is a more conservative protection mechanism than was
  applied previously.

- Random numbers from `rxode2` are different when using `dop853`,
  `lsoda` or `indLin` methods. These now seed the random numbers in the
  same way as `liblsoda`, so the random number provided will be the same
  with different solving methods.

- The arguments saved in the `rxSolve` for items like `thetaMat` will be
  the reduced matrices used in solving, not the full matrices (this will
  likely not break very many items)

### Possible breaking changes (though unlikely)

- `iCov` is no longer merged to the event dataset. This makes solving
  with `iCov` slightly faster
  ([\#743](https://github.com/nlmixr2/rxode2/issues/743))

### New features

- You can remove covariances for every omega by piping with
  `%>% ini(diag())` you can be a bit more granular by removing all
  covariances that have either `eta.ka` or `eta.cl` by:
  `%>% ini(diag(eta.ka, eta.cl))` or anything with correlations with
  `eta.cl` with `%>% ini(diag(eta.cl))`

- You can also remove individual covariances by `%>% ini(-cov(a, b))` or
  `%>% ini(-cor(a,b))`.

- You can specify the type of interpolation applied for added dosing
  records (or other added records) for columns that are kept with the
  `keep=` option in
  [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md).
  This new option is `keepInterpolation` and can be `locf` for last
  observation carried forward, `nocb` which is the next observation
  carried backward, as well as `NA` which puts a `NA` in all imputed
  data rows. See [\#756](https://github.com/nlmixr2/rxode2/issues/756).

  - Note: when interpolation is linear/midpoint for factors/characters
    it changes to locf with a warning
    ([\#759](https://github.com/nlmixr2/rxode2/issues/759))

  - Also note, that the default keep interpolation is `na`

- Now you can specify the interpolation method per covariate in the
  model:

  - `linear(var1, var2)` says both `var1` and `var2` would use linear
    interpolation when they are a time-varying covariate. You could also
    use `linear(var1)`

  - `locf()` declares variables using last observation carried forward

  - `nocb()` declares variables using next observation carried backward

  - `midpoint()` declares variables using midpoint interpolation

- `linear()`, `locf()`, `locb()`, `midpoint()`, `params()`, `cmt()` and
  `dvid()` declarations are now ignored when loading a `rxode2` model
  with [`rxS()`](https://nlmixr2.github.io/rxode2/reference/rxS.md)

- Strings can be assigned to variables in `rxode2`.

- Strings can now be enclosed with a single quote as well as a double
  quote. This limitation was only in the rxode2 using string since the
  R-parser changes single quotes to double quotes. (This has no impact
  with `rxode2({})` and ui/function form).

- More robust string encoding for symengine (adapted from
  [`utils::URLencode()`](https://rdrr.io/r/utils/URLencode.html) and
  [`utils::URLdecode()`](https://rdrr.io/r/utils/URLencode.html))

- Empty arguments to
  [`rxRename()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md)
  give a warning ([\#688](https://github.com/nlmixr2/rxode2/issues/688))

- Promoting from covariates to parameters with model piping (via
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md)) now
  allows setting bounds
  ([\#692](https://github.com/nlmixr2/rxode2/issues/692))

- Added
  [`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
  [`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
  [`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
  [`testCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
  [`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md)
  [`testVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
  [`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
  [`assertVariableName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
  and
  [`assertParameterValue()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md)
  to verify that a value is a valid nlmixr2 compartment name, nlmixr2
  compartment/variable exists in the model, variable name, or parameter
  value ([\#726](https://github.com/nlmixr2/rxode2/issues/726);
  [\#733](https://github.com/nlmixr2/rxode2/issues/733))

- Added
  [`assertRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md),
  [`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md),
  [`warnRxBounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)
  to allow `nlmixr2` warn about methods that ignore boundaries
  [\#760](https://github.com/nlmixr2/rxode2/issues/760)

- Added functions `tad0()`, `tafd0()`, `tlast0()` and `tfirst0()` that
  will give `0` instead of `NA` when the dose has not been administered
  yet. This is useful for use in ODEs since `NA`s will break the solving
  (so can be used a bit more robustly with models like Weibull
  absorption).

- `rxode2` is has no more binary link to `lotri`, which means that
  changes in the `lotri` package will not require `rxode2` to be
  recompiled (in most cases) and will not crash the system.

- `rxode2` also has no more binary linkage to `PreciseSums`

- The binary linkage for `dparser` is reduced to C structures only,
  making changes in dparser less likely to cause segmentation faults in
  `rxode2` if it wasn’t recompiled.

- A new model property has been added to `$props$cmtProp` and
  `$statePropDf`. Both are data-frames showing which compartment has
  properties (currently `ini`, `f`, `alag`, `rate` and `dur`) in the
  `rxode2` ui model. This comes from the lower level model variable
  `$stateProp` which has this information encoded in integers for each
  state.

- A new generic method `rxUiDeparse` can be used to deparse meta
  information into more readable expressions; This currently by default
  supports lower triangular matrices by lotri, but can be extended to
  support other types of objects like ’nlmixr2’s `foceiControl()` for
  instance.

### Bug fixes

- Fix `ui$props$endpoint` when the ui endpoint is defined in terms of
  the ode instead of lhs. See
  [\#754](https://github.com/nlmixr2/rxode2/issues/754)

- Fix `ui$props` when the ui is a linear compartment model without `ka`
  defined.

- Model extraction
  [`modelExtract()`](https://nlmixr2.github.io/rxode2/reference/modelExtract.md)
  will now extract model properties. Note that the model property of
  `alag(cmt)` and `lag(cmt)` will give the same value. See
  [\#745](https://github.com/nlmixr2/rxode2/issues/745)

- When assigning reserved variables, the parser will error. See
  [\#744](https://github.com/nlmixr2/rxode2/issues/744)

- Linear interpolation will now adjust the times as well as the values
  when `NA` values are observed.

- Fix when keeping data has `NA` values that it will not crash R; Also
  fixed some incorrect `NA` interpolations. See
  [\#756](https://github.com/nlmixr2/rxode2/issues/756)

- When using `cmt()` sometimes the next statement would be corrupted in
  the normalized syntax (like for instance `locf`); This bug was fixed
  ([\#763](https://github.com/nlmixr2/rxode2/issues/763))

- `keep` will now error when trying to keep items that are in the rxode2
  output data-frame and will be calculated
  ([\#764](https://github.com/nlmixr2/rxode2/issues/764))

### Big change

- At the request of CRAN, combine `rxode2parse`, `rxode2random`, and
  `rxode2et` into this package; The changes in each of the packages are
  now placed here:

#### rxode2et (no changes before merge)

##### rxode2et 2.0.13

- Fix import of data where there are NA times

##### rxode2et 2.0.12

- Fix formatting issues identified by m1mac, as requested by CRAN

##### rxode2et 2.0.11

- Make the stacking more flexible to help rxode2 have more types of
  plots

- Add `toTrialDuration` by Omar Elashkar to convert event data to trial
  duration data

- Fix Issue [\#23](https://github.com/nlmixr2/rxode2/issues/23) and
  prefer variable values over NSE values

##### rxode2et 2.0.10

- Fix dollar sign accessing of objects (like data frames), as pointed
  out by [@frbrz](https://github.com/frbrz) (issue
  [\#16](https://github.com/nlmixr2/rxode2/issues/16))

- Use `rxode2parse` functions for internal event table creation (where
  they were moved to).

- Dropped C++14 and let the system decide.

##### rxode2et 2.0.9

- Split off [`et()`](https://nlmixr2.github.io/rxode2/reference/et.md),
  [`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md)
  and related functions.

- Also split off
  [`rxStack()`](https://nlmixr2.github.io/rxode2/reference/rxStack.md)
  and
  [`rxCbindStudyIndividual()`](https://nlmixr2.github.io/rxode2/reference/rxCbindStudyIndividual.md)
  in this package.

- Added a `NEWS.md` file to track changes to the package.

#### rxode2random (before merge)

- Fix a bug when simulating nested variables
  ([\#25](https://github.com/nlmixr2/rxode2/issues/25))

##### rxode2random 2.1.0

- **Breaking Change** changed distributions from the standard C++
  `<random>` to `boost::random`. Since this is not dependent on the
  compiler, it makes the random numbers generated from Mac, Windows and
  Linux the same for every distribution. Unfortunately with a new random
  number transformation, the simulation results will likely be different
  than they were before. The exception to this is the uniform number,
  which was always the same between platforms.

##### rxode2random 2.0.13

- Fixed formatting issues (as requested by CRAN and identified on
  `m1mac`)

##### rxode2random 2.0.12

- Added function `dfWishart` which gives (by simulation) an
  approximation of the degrees of freedom of a Wishart to match a `rse`
  value.

- Added function `swapMatListWithCube` which swaps omegaList with
  omegaCube values

- Ensure that the outputs are integers (instead of long integers) as
  requested by CRAN for some checking functions.

##### rxode2random 2.0.11

- Fix qassert LTO

##### rxode2random 2.0.10

- Moved fast factor to `rxode2parse` to allow `etTrans` to be moved
  there

##### rxode2random 2.0.9

- Initial release of `rxode2random`, which separates the parallel safe,
  random number generation from ‘rxode2’ into a separate package to
  reduce ‘rxode2’ compilation time. This should make CRAN maintenance a
  bit easier.

- Added a `NEWS.md` file to track changes to the package.

#### rxode2parse (fixed before merging)

- As requested by CRAN remove the C code `SET_TYPEOF` which is no longer
  part of the C R API.

##### rxode2parse 2.0.19

- Added a evid suffix of 60 for cases where evid=2 adds an on event
  (fixes tad() calculation in certain edge cases)

- Initialize all variables to `NA`

##### rxode2parse 2.0.18

- Removed linear compartment solutions with gradients from rxode2parse
  (and rxode2) when compiled with intel c++ compiler (since it crashes
  while compiling).

- Fixed `m1mac` string issues as requested by CRAN

##### rxode2parse 2.0.17

- Added ability to query R user functions in a rxode2 model (will force
  single threaded solve)

- Moved core `rxFunParse` and `rxRmFunParse` here so that C and R user
  function clashes can be handled

- Model variables now tracks which compartments have a lag-time defined

- For compartment with steady state doses (NONMEM equivalent SS=1,
  SS=2), an additional tracking time-point is added at to track the time
  when the lagged dose is given. As an upshot, the lagged dose will
  start at the steady state concentration shifted by + ii - lag in
  `rxode2` (currently for ode systems only)

- This release calculates non bio-availability adjusted duration for all
  rates instead of trying to figure the rate duration during solving.

- Make double assignment an error, ie `a <- b <-`

- `NA` times are ignored (with warning)

- Steady state bolus doses with `addl` are treated as non steady state
  events (like what is observed in `NONMEM`)

- Timsort was upgraded; drop radix support in rxode2 structure

- `etTrans` now supports keeping logical vectors (with the appropriate
  version of `rxode2`).

- Security fixes were applied as requested by CRAN

##### rxode2parse 2.0.16

- Import `data.table` explicitly in the R code (before was imported only
  in C/C++ code)

##### rxode2parse 2.0.15

- Updates the make flags to support CXX17.

##### rxode2parse 2.0.14

- ‘linCmt()’ translations of ‘alpha’, ‘beta’, ‘gamma’, ‘k21’, ‘k31’,
  ‘vc’ now error instead of ignoring ‘gamma’ and ‘k31’ to give 2 cmt
  solution

- transit compartment internal code now changes dose to 0.0 when no dose
  has been administered to the depot compartment. This way dosing to the
  central compartment (without dosing to the transit compartment) will
  not give a `NA` for the depot compartment (and consequently for the
  central compartment)

- Moved `rxDerived` here and added tests for it here as well.

- Moved `etTransParse` here and added tests for it here as well (makes
  up most of `etTrans`). In addition the following changes were made to
  `etTransParse()`/[`etTrans()`](https://nlmixr2.github.io/rxode2/reference/etTrans.md):

  - The internal translation
    ([`etTrans()`](https://nlmixr2.github.io/rxode2/reference/etTrans.md))
    will not drop times when infusions stop. Before, if the infusion
    stopped after the last observation the time when the infusion
    stopped would be dropped. This interferes with `linCmt()` models.

  - Breaking change/bug fix `evid=2` are considered observations when
    translating data to internal `rxode2` event structure

  - Fix edge case to find infusion duration when it is the first item of
    the dosing record at time 0.

- Fixed a bug for certain infusions where the `rate`, `ii` and/or `ss`
  data items were dropped from the output when `addDosing=TRUE`

- Also have internal functions to convert between classic NONMEM events
  and rxode2 events

- Have an internal function that gives information on the linear
  compartmental model translation type, which could be useful for
  babelmixr2

- ‘time’ in model is now case insensitive

- Use function declaration in
  [`rxode2parseGetTranslation()`](https://nlmixr2.github.io/rxode2/reference/rxode2parseGetTranslation.md)
  to determine thread safety of functions available to rxode2

- Add check for correct number of function arguments to parser.

- Like R, known functions can be assigned as a variable and the function
  can still be called (while not changing the variable value). For
  example you can have a variable `gamma` as well as a function
  [`gamma()`](https://rdrr.io/r/base/Special.html).

- Fix garbled error messages that occur with certain messages.

- Fixed errors that occurred when using capitalized AMT variables in the
  model.

##### rxode2parse 2.0.13

- Version bump for dparser (so binaries will be built correctly)

##### rxode2parse 2.0.12

- Bug fix for strict prototypes

- Removed `sprintf` as noted by CRAN

- Made `rxode2parse` dll binary independent of
  [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)

##### rxode2parse 2.0.11

- Bug fix for strict aliasing as requested by CRAN

##### rxode2parse 2.0.10

- Use strict aliasing as requested by CRAN

##### rxode2parse 2.0.9

- Initial release to split of rxode2parse from rxode2 to reduce
  compilation time of ‘rxode2’

## rxode2 2.1.3

CRAN release: 2024-05-28

### Bug fixes

- Make sure that the object is a uncompressed rxode2 ui for solving with
  `rxSolve` (See [\#661](https://github.com/nlmixr2/rxode2/issues/661))

- Fix [\#670](https://github.com/nlmixr2/rxode2/issues/670) by using the
  last simulated observation residual when there are trailing doses.

### New features

- Create a function to see if a rxode2 solve is loaded in memory
  ([`rxode2::rxSolveSetup()`](https://nlmixr2.github.io/rxode2/reference/rxSolveSetup.md))

- Create a new function that fixes the rxode2 population values in the
  model (and drops them in the initial estimates);
  [`rxFixPop()`](https://nlmixr2.github.io/rxode2/reference/rxFixPop.md)

### Other changes

- Pendantic no-remap (as requested by CRAN)

- gcc USBAN fix (as requested by CRAN)

## rxode2 2.1.2

CRAN release: 2024-01-30

### Other changes

- `rxUi` compression now defaults to fast compression

- Fixes String literal formatting issues as identified by CRAN
  ([\#643](https://github.com/nlmixr2/rxode2/issues/643))

- Removes linear compartment solutions with gradients for intel c++
  compiler (since they crash the compiler).

## rxode2 2.1.0

CRAN release: 2023-12-11

### Breaking changes

- Steady state with lag times are no longer shifted by the lag time and
  then solved to steady state by default. In addition the steady state
  at the original time of dosing is also back-calculated. If you want
  the old behavior you can bring back the option with
  `ssAtDoseTime=FALSE`.

- “dop853” now uses the `hmax`/`h0` values from the
  [`rxControl()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md)
  or
  [`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md).
  This may change some ODE solving using “dop853”

- When not specified (and xgxr is available), the x axis is no longer
  assumed to be in hours

### New features

- User defined functions can now be R functions. For many of these R
  functions they can be converted to C with
  [`rxFun()`](https://nlmixr2.github.io/rxode2/reference/rxFun.md) (you
  can see the C code afterwards with `rxC("funName")`)

- Parallel solving of models that require sorting (like modeled lag
  times, modeled duration etc) now solve in parallel instead of
  downgrading to single threaded solving

- Steady state infusions with a duration of infusions greater than the
  inter-dose interval are now supported.

- Added `$symengineModelNoPrune` and `$symengineModelPrune` for loading
  models into rxode2 with
  [`rxS()`](https://nlmixr2.github.io/rxode2/reference/rxS.md)

- When plotting and creating confidence intervals for multiple endpoint
  models simulated from a rxode2 ui model, you can plot/summarize each
  endpoint with `sim`. (ie. `confint(model, "sim")` or
  `plot(model, sim)`).

  If you only want to summarize a subset of endpoints, you can focus on
  the endpoint by pre-pending the endpoint with `sim.` For example if
  you wanted to plot/summarize only the endpoint `eff` you would use
  `sim.eff`. (ie `confint(model, "sim.eff")` or `plot(model, sim.eff)`)

- Added `model$simulationIniModel` which prepend the initial conditions
  in the `ini({})` block to the classic `rxode2({})` model.

- Now `model$simulationModel` and `model$simulationIniModel` will save
  and use the initialization values from the compiled model, and will
  solve as if it was the original ui model.

- Allow `ini(model) <- NULL` to drop ini block and `as.ini(NULL)` gives
  `ini({})` (Issue
  [\#523](https://github.com/nlmixr2/rxode2/issues/523))

- Add a function
  [`modelExtract()`](https://nlmixr2.github.io/rxode2/reference/modelExtract.md)
  to extract model lines to allow modifying them and then changing the
  model by piping or simply assigning the modified lines with
  `model(ui) <- newModifiedLines`

- Add Algebraic mu-referencing detection (mu2) that allows you to
  express mu-referenced covariates as:

``` r
cl <- exp(tcl + eta.cl + wt_cl * log(WT/70.5))
```

Instead of the

``` r
cl <- exp(tcl + eta.cl + wt_cl * log.WT.div.70.5)
```

That was previously required (where `log.WT.div.70.5` was calculated in
the data) for mu expressions. The `ui` now has more information to allow
transformation of data internally and transformation to the old
mu-referencing style to run the optimization.

- Allow steady state infusions with a duration of infusion greater than
  the inter-dose interval to be solved.

- Solves will now possibly print more information when issuing a “could
  not solve the system” error

- The function
  [`rxSetPipingAuto()`](https://nlmixr2.github.io/rxode2/reference/rxSetPipingAuto.md)
  is now exported to change the way you affect piping in your individual
  setup

- Allow covariates to be specified in the model piping, that is
  `mod %>% model(a=var+3, cov="var")` will add `"var"` as a covariate.

- When calculating confidence intervals for `rxode2` simulated objects
  you can now use `by` to stratify the simulation summary. For example
  you can now stratify by gender and race by:
  `confint(sim, "sim", by=c("race", "gender"))`

- When calculating the intervals for `rxode2` simulated objects you can
  now use `ci=FALSE` so that it only calculates the default intervals
  without bands on each of the percentiles; You can also choose not to
  match the secondary bands limits with `levels` but use your own
  `ci=0.99` for instance

- A new function was introduced
  [`meanProbs()`](https://nlmixr2.github.io/rxode2/reference/meanProbs.md)
  which calculates the mean and expected confidence bands under either
  the normal or t distribution

- A related new function was introduced that calculates the mean and
  confidence bands under the Bernoulli/Binomial distribution
  ([`binomProbs()`](https://nlmixr2.github.io/rxode2/reference/binomProbs.md))

- When calculating the intervals for `rxode2` simulated objects you can
  also use `mean=TRUE` to use the mean for the first level of confidence
  using
  [`meanProbs()`](https://nlmixr2.github.io/rxode2/reference/meanProbs.md).
  For this confidence interval you can override the `n` used in the
  confidence interval by using `n=#`. You can also change this to a
  prediction interval instead using `pred=TRUE`.

- Also when calculating the intervals for `rxode2` simulated object you
  can also use `mean="binom"` to use the binomial distributional
  information (and ci) for the first level of confidence using
  [`binomProbs()`](https://nlmixr2.github.io/rxode2/reference/binomProbs.md).
  For this confidence interval you can override the `n` used in the
  confidence interval by using `n=#`. You can also change this to a
  prediction interval instead using `pred=TRUE`. With `pred=TRUE` you
  can override the number of predicted samples with `m=#`

- When plotting the `confint` derived intervals from an `rxode2`
  simulation, you can now subset based on a simulated value like
  `plot(ci, Cc)` which will only plot the variable `Cc` that you
  summarized even if you also summarized `eff` (for instance).

- When the rxode2 ui is a compressed ui object, you can modify the ini
  block with `$ini <-` or modify the model block with `$model <-`. These
  are equivalent to `ini(model) <-` and `model(model) <-`, respectively.
  Otherwise, the object is added to the user defined components in the
  function (ie `$meta`). When the object is uncompressed, it simply
  assigns it to the environment instead (just like before).

- When printing meta information that happens to be a `lotri` compatible
  matrix, use `lotri` to express it instead of the default R expression.

- Allow character vectors to be converted to expressions for piping
  ([\#552](https://github.com/nlmixr2/rxode2/issues/552))

- [`rxAppendModel()`](https://nlmixr2.github.io/rxode2/reference/rxAppendModel.md)
  will now take an arbitrary number of models and append them together;
  It also has better handling of models with duplicate parameters and
  models without
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) blocks
  ([\#617](https://github.com/nlmixr2/rxode2/issues/617) /
  [\#573](https://github.com/nlmixr2/rxode2/issues/573) /
  [\#575](https://github.com/nlmixr2/rxode2/issues/575)).

- `keep` will now also keep attributes of the input data (with special
  handling for `levels`); This means a broader variety of classes will
  be kept carrying more information with it (for example ordered
  factors, data frame columns with unit information, etc)

- Piping arguments `append` for
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) and
  [`model()`](https://nlmixr2.github.io/rxode2/reference/model.md) have
  been aligned to perform similarly. Therefore `ini(append=)` now can
  take expressions instead of simply strings and `model(append=)` can
  also take strings. Also model piping now can specify the integer line
  number to be modified just like the
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) could.
  Also `model(append=FALSE)` has been changed to `model(append=NULL)`.
  While the behavior is the same when you don’t specify the argument,
  the behavior has changed to align with
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) when
  piping. Hence `model(append=TRUE)` will append and
  `model(append=FALSE)` will now pre-pend to the model.
  `model(append=NULL)` will modify lines like the behavior of
  `ini(append=NULL)`. The default of `model(line)` modifying a line
  in-place still applies. While this is a breaking change, most code
  will perform the same.

- Labels can now be dropped by `ini(param=label(NULL))`. Also parameters
  can be dropped with the idiom `model(param=NULL)` or `ini(param=NULL)`
  changes the parameter to a covariate to align with this idiom of
  dropping parameters

- `rxRename` has been refactored to run faster

### Internal new features

- Add
  [`as.model()`](https://nlmixr2.github.io/rxode2/reference/as.model.md)
  for list expressions, which implies `model(ui) <- ui$lstExpr` will
  assign model components. It will also more robustly work with
  character vectors

- Simulated objects from `rxSolve` now can access the model variables
  with `$rxModelVars`

- Simulation models from the UI now use `rxerr.endpoint` instead of
  `err.endpoint` for the `sigma` residual error. This is to align with
  the convention that internally generated variables start with `rx` or
  `nlmixr`

- Sorting only uses timsort now, and was upgraded to the latest version
  from Morwenn

### Bug fixes

- Simulating/solving from functions/ui now prefers params over `omega`
  and `sigma` in the model
  ([\#632](https://github.com/nlmixr2/rxode2/issues/632))

- Piping does not add constants to the initial estimates

- When constants are specified in the `model({})` block (like `k <- 1`),
  they will not be to the `ini` block

- Bug fix for
  [`geom_amt()`](https://nlmixr2.github.io/rxode2/reference/stat_amt.md)
  when the `aes` transformation has `x`

- Bug fix for some covariate updates that may affect multiple
  compartment models (like issue
  [\#581](https://github.com/nlmixr2/rxode2/issues/581))

### Maintenance fixes

- Modify plot code to work with development `xgxr`

## rxode2 2.0.14

CRAN release: 2023-10-07

- CRAN requested that FORTRAN `kind` be changed as it was not portable;
  This was commented code, and simply removed the comment.

- Bug-fix for
  [`geom_amt()`](https://nlmixr2.github.io/rxode2/reference/stat_amt.md);
  also now uses `linewidth` and at least `ggplot2 3.4.0`

- Some documentation was cleaned up from `rxode2` 2.0.13

## rxode2 2.0.13

CRAN release: 2023-04-22

### Bug fixes

- A bug was fixed so that the
  [`zeroRe()`](https://nlmixr2.github.io/rxode2/reference/zeroRe.md)
  function works with correlated omega values.

- A bug was fixed so that the
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)
  function works with initial conditions for compartments (`cmt(0)`)

### New features

- A new function
  [`zeroRe()`](https://nlmixr2.github.io/rxode2/reference/zeroRe.md)
  allows simple setting of omega and/or sigma values to zero for a model
  ([\#456](https://github.com/nlmixr2/rxode2/issues/456))

- Diagonal zeros in the `omega` and `sigma` matrices are treated as
  zeros in the model. The corresponding `omega` and `sigma` matrices
  drop columns/rows where the diagonals are zero to create a new `omega`
  and `sigma` matrix for simulation. This is the same idiom that NONMEM
  uses for simulation from these matrices.

- Add the ability to pipe model estimates from another model by
  `parentModel %>% ini(modelWithNewEsts)`

- Add the ability to append model statements with piping using
  `%>% model(x=3, append=d/dt(depot))`, still supports appending with
  `append=TRUE` and pre-pending with `append=NA` (the default is to
  replace lines with `append=FALSE`)

- rxSolve’s keep argument will now maintain character and factor classes
  from input data with the same class
  ([\#190](https://github.com/nlmixr2/rxode2/issues/190))

- Parameter labels may now be modified via `ini(param = label("text"))`
  ([\#351](https://github.com/nlmixr2/rxode2/issues/351)).

- Parameter order may be modified via the `append` argument to
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) when
  piping a model. For example, `ini(param = 1, append = 0)` or
  `ini(param = label("text"), append = "param2")`
  ([\#352](https://github.com/nlmixr2/rxode2/issues/352)).

### Internal changes

- If lower/upper bounds are outside the required bounds, the adjustment
  is displayed.

- When initial values are piped that break the model’s boundary
  condition reset the boundary to unbounded and message which boundary
  was reset.

- Added
  [`as.rxUi()`](https://nlmixr2.github.io/rxode2/reference/as.rxUi.md)
  function to convert the following objects to `rxUi` objects: `rxode2`,
  `rxModelVars`, `function`. Converting nlmixr2 fits to `rxUi` will be
  placed in the `s3` method in the corresponding package.

- `assertRxUi(x)` now uses
  [`as.rxUi()`](https://nlmixr2.github.io/rxode2/reference/as.rxUi.md)
  so that it can be extended outside of `rxode2`/`nlmixr2`.

- `rxode2` now supports `addl` with `ss` doses

- Moved `rxDerived` to `rxode2parse` (and re-exported it here).

- Added test for transit compartment solving in absence of dosing to the
  transit compartment (fixed in `rxode2parse` but solving tested here)

- Using [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md)
  without any arguments on a `rxode2` type function will return the
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) block.
  Also added a method `ini(mod) <- iniBlock` to modify the `ini` block
  is you wish. `iniBlock` should be an expression.

- Using [`model()`](https://nlmixr2.github.io/rxode2/reference/model.md)
  without any arguments on a `rxode2` type function will return the
  [`model()`](https://nlmixr2.github.io/rxode2/reference/model.md)
  block. Also added a new method `model(mod) <- modelBlock`

- Added a new method `rxode2(mod) <- modFunction` which allows replacing
  the function with a new function while maintaining the meta
  information about the ui (like information that comes from `nonmem2rx`
  models). The `modFunction` should be the body of the new function, the
  new function, or a new `rxode2` ui.

- `rxode2` ui objects now have a `$sticky` item inside the internal
  (compressed) environment. This `$sticky` tells what variables to keep
  if there is a “significant” change in the ui during piping or other
  sort of model change. This is respected during model piping, or
  modifying the model with `ini(mod)<-`, `model(mod)<-`,
  `rxode2(mod)<-`. A significant change is a change in the model block,
  a change in the number of estimates, or a change to the value of the
  estimates. Estimate bounds, weather an estimate is fixed or estimate
  label changes are not considered significant.

- Added
  [`as.ini()`](https://nlmixr2.github.io/rxode2/reference/as.ini.md)
  method to convert various formats to an ini expression. It is used
  internally with `ini(mod)<-`. If you want to assign something new that
  you can convert to an ini expression, add a method for
  [`as.ini()`](https://nlmixr2.github.io/rxode2/reference/as.ini.md).

- Added
  [`as.model()`](https://nlmixr2.github.io/rxode2/reference/as.model.md)
  method to convert various formats to a model expression. It is used
  internally with `model(mod)<-`. If you want to assign something new
  that you can convert to a model expression, add a method for
  [`as.model()`](https://nlmixr2.github.io/rxode2/reference/as.model.md).

## rxode2 2.0.11

CRAN release: 2022-11-01

- Give a more meaningful error for ‘rxode2’ ui models with only error
  expressions

- Break the ABI requirement between `roxde2()` and
  [`rxode2parse()`](https://nlmixr2.github.io/rxode2/reference/rxode2parse.md)

- The new `rxode2parse` will fix the `sprintf` exclusion shown on CRAN.

## rxode2 2.0.10

CRAN release: 2022-10-20

- Time invariant covariates can now contain ‘NA’ values.

- When a column has ‘NA’ for the entire id, now ‘rxode2’ warns about
  both the id and column instead of just the id.

- To fix some CRAN issues in ‘nlmixr2est’, make the version dependency
  explicit.

## rxode2 2.0.9

CRAN release: 2022-10-19

- Remove log likelihoods from ‘rxode2’ to reduce compilation time and
  increase maintainability of ‘rxode2’. They were transferred to
  ‘rxode2ll’ (requested by CRAN).

- Remove the parsing from ‘rxode2’ and solved linear compartment code
  and move to ‘rxode2parse’ to reduce the compilation time (as requested
  by CRAN).

- Remove the random number generation from ‘rxode2’ and move to
  ‘rxode2random’ to reduce the compilation time (as requested by CRAN).

- Remove the event table translation and generation from ‘rxode2’ and
  move to ‘rxode2et’ to reduce the compilation time (as requested by
  CRAN).

- Change the `rxode2` ui object so it is a compressed, serialized object
  by default. This could reduce the `C stack size` problem that occurs
  with too many environments in R.

- Warn when ignoring items during simulations

- Export a method to change ‘rxode2’ solve methods into internal
  integers

- Bug fix for time invariant covariates identified as time variant
  covariate when the individual’s time starts after `0`.

## rxode2 2.0.8

CRAN release: 2022-09-23

### Breaking changes

- `rxgamma` now only allows a `rate` input. This aligns with the
  internal `rxode2` version of `rxgamma` and clarifies how this will be
  used. It is also aligned with the `llikGamma` function used for
  generalized likelihood estimation.

- ui `cauchy` simulations now follow the ui for `normal` and `t`
  distributions, which means you can combine with transformations. This
  is because the `cauchy` is a `t` distribution with one degree of
  freedom.

- ui [`dnorm()`](https://rdrr.io/r/stats/Normal.html) and
  [`norm()`](https://rdrr.io/r/base/norm.html) are no longer equivalent
  to `add()`. Now it allows you to use the loglik
  [`llikNorm()`](https://nlmixr2.github.io/rxode2/reference/llikNorm.md)
  instead of the standard `nlmixr2` style focei likelihood. This is done
  by adding [`dnorm()`](https://rdrr.io/r/stats/Normal.html) at the end
  of the line. It also means
  [`dnorm()`](https://rdrr.io/r/stats/Normal.html) now doesn’t take any
  arguments.

- Vandercorput normal removed (non-random number generator)

### New features

- Allow models in the `nlmixr2` form without an `ini({})` block

- Allow model piping of an omega matrix by `f %>% ini(omegaMatrix)`

- Standard models created with
  [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md) can
  no be piped into a model function

- Families of log-likelihood were added to `rxode2` so that mixed
  likelihood nonlinear mixed effects models may be specified and run.

- The memory footprint of a `rxode2` solving has been reduced

- Piping now allow named strings (issue
  [\#249](https://github.com/nlmixr2/rxode2/issues/249))

### Bug fixes

- `rxode2`’s symengine would convert `sqrt(2)` to `M_SQRT_2` when it
  should be `M_SQRT2`. This has been fixed; it was most noticeable in
  nlmixr2 log-likelihood estimation methods

- `rxode2` treats `DV` as a non-covariate with `etTran` (last time it
  would duplicate if it is in the model). This is most noticeable in the
  nlmixr2 log-likelihood estimation methods.

### New features

- A new flag (`rxFlag`) has been created to tell you where in the
  `rxode2` solving process you are. This is useful for debugging. If
  outputting this variable it will always be `11` or calculating the
  left handed equations. If you are using in conjunction with the
  `printf()` methods, it is a double variable and should be formatted
  with `"%f"`.

- An additional option of `fullPrint` has been added to
  [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
  which allows `rprintf()` to be used in almost all of
  [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
  steps (inductive linearization and matrix exponential are the
  exception here) instead of just the integration `ddt` step. It
  defaults to `FALSE`.

## rxode2 2.0.7

CRAN release: 2022-05-17

- Removed accidental `^S` from news as requested by CRAN.

- Bug fix for more complicated mu-referencing.

- Change rxode2 md5 to only depend on the C/C++/Fortran code and headers
  not the R files. That way if there is binary compatibility between
  `nlmixr2est` and `rxode2`, a new version of `nlmixr2est` will not need
  to be submitted to CRAN.

## rxode2 2.0.6

CRAN release: 2022-05-09

### Breaking changes

#### Solving controls

- The options for `rxControl` and `rxSolve` are more strict. `camelCase`
  is now always used. Old options like `add.cov` and `transit_abs` are
  no longer supported, only `addCov` is supported.

- A new option, `sigdig` has been added to
  [`rxControl()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md),
  which controls some of the more common significant figure options like
  `atol`, `rtol`, `ssAtol`, `ssRtol`, with a single option.

#### Simulations

- For simulations, `$simulationSigma` now assumes a diagonal matrix. The
  sigma values are assumed to be standard normal, and uncorrelated
  between endpoints. Simulation with uncertainty will still draw from
  this identity diagonal matrix

- Parallel solving now seeds each simulation per each individual based
  on the initial seed plus the simulation id. This makes the simulation
  reproducible regardless of the number of cores running the simulation.

#### Other breaking changes

- Solved objects now access the underlying rxode model with `$rxode2`
  instead of `$rxode`

- Since this change names, `rxode2`, `rxode` and `RxODE` all perform the
  same function.

- Options were changed from `RxODE.syntax` to `rxode2.syntax`.

- Assigning states with `rxode2.syntax.assign.state` (was
  `RxODE.syntax.assign.state`) is no longer supported.

- Enforcing “pure” assignment syntax with `=` syntax is no longer
  supported so `rxode2.syntax.assign` is no longer supported (was
  `RxODE.syntax.assign`).

- Since R supports `**` as an exponentiation operator, the pure syntax
  without `**` can no longer be enabled. Hence `rxode2.syntax.star.pow`
  (was `RxODE.syntax.star.pow`) no longer has any effect.

- The “pure” syntax that requires a semicolon can no longer be enabled.
  Therefore `rxode2.syntax.require.semicolon` (was
  `RxODE.syntax.require.semicolon`) no longer has any effect.

- The syntax `state(0)` can no longer be turned off.
  `rxode2.syntax.allow.ini0` (was `RxODE.syntax.allow.ini0`) has been
  removed.

- Variable with dots in variable and state names like `state.name` works
  in R. Therefore, “pure” syntax of excluding `.` values from variables
  cannot be enforced with `rxode2.syntax.allow.dots` (was
  `RxODE.syntax.allow.dots`).

- The mnemonic `et(rate=model)` and `et(dur=model)` mnemonics have been
  removed. `rate` needs to be set to `-1` and `-2` manually instead.

- The function `rxode2Test()` has been removed in favor of using
  testthat directly.

- Transit compartments need to use a new `evid`, `evid=7`. That being
  said, the `transitAbs` option is no longer supported.

- `ID` columns in input parameter data frames are not sorted or merged
  with original dataset any more; The underlying assumption of ID order
  should now be checked outside of
  [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md).
  Note that the event data frame is still sorted.

### Additional features

- The UI functions of `nlmixr` have been ported to work in `rxode2`
  directly.

- `rxModelVars({})` is now supported.

- You may now combine 2 models in `rxode2` with
  [`rxAppendModel()`](https://nlmixr2.github.io/rxode2/reference/rxAppendModel.md).
  In fact, as long as the first value is a rxode2 evaluated ui model,
  you can use `c`/`rbind` to bind 2 or more models together.

- You may now append model lines with piping using
  `%>% model(lines, append=TRUE)` you can also pre-pend lines by
  `%>% model(lines, append=NA)`

- You may now rename model variables, states and defined parameters with
  `%>% rxRename(new=old)` or if `dplyr` is loaded: `%>% rename(new=old)`

- You can fix parameters with `%>% ini(tcl=fix)` or `%>% ini(fix(tcl))`
  as well as unfix parameters with `%>% ini(tcl=unfix)` or
  `%>% ini(unfix(tcl))`

### Internal changes

- Strict R headers are enforced more places

- Since there are many changes that could be incompatible, this version
  has been renamed to `rxode2`

- [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
  printout no longer uses rules and centered headings to make it display
  better on a larger variety of systems.

### Bug fixes

- `tad()` and related time features only reset at the start of an
  infusion (as opposed to starting at the beginning and end of an
  infusion)
