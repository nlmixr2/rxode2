# Options, Solving & Simulation of an ODE/solved system

This uses rxode2 family of objects, file, or model specification to
solve a ODE system. There are many options for a solved rxode2 model,
the first are the required `object`, and `events` with the some-times
optional `params` and `inits`.

## Usage

``` r
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  scale = NULL,
  method = c("liblsoda", "lsoda", "dop853", "indLin"),
  sigdig = NULL,
  atol = 1e-08,
  rtol = 1e-06,
  maxsteps = 70000L,
  hmin = 0,
  hmax = NA_real_,
  hmaxSd = 0,
  hini = 0,
  maxordn = 12L,
  maxords = 5L,
  ...,
  cores,
  covsInterpolation = c("locf", "linear", "nocb", "midpoint"),
  naInterpolation = c("locf", "nocb"),
  keepInterpolation = c("na", "locf", "nocb"),
  addCov = TRUE,
  sigma = NULL,
  sigmaDf = NULL,
  sigmaLower = -Inf,
  sigmaUpper = Inf,
  nCoresRV = 1L,
  sigmaIsChol = FALSE,
  sigmaSeparation = c("auto", "lkj", "separation"),
  sigmaXform = c("identity", "variance", "log", "nlmixrSqrt", "nlmixrLog",
    "nlmixrIdentity"),
  nDisplayProgress = 10000L,
  amountUnits = NA_character_,
  timeUnits = "hours",
  theta = NULL,
  thetaLower = -Inf,
  thetaUpper = Inf,
  eta = NULL,
  addDosing = FALSE,
  stateTrim = Inf,
  updateObject = FALSE,
  omega = NULL,
  omegaDf = NULL,
  omegaIsChol = FALSE,
  omegaSeparation = c("auto", "lkj", "separation"),
  omegaXform = c("variance", "identity", "log", "nlmixrSqrt", "nlmixrLog",
    "nlmixrIdentity"),
  omegaLower = -Inf,
  omegaUpper = Inf,
  nSub = 1L,
  thetaMat = NULL,
  thetaDf = NULL,
  thetaIsChol = FALSE,
  nStud = 1L,
  dfSub = 0,
  dfObs = 0,
  returnType = c("rxSolve", "matrix", "data.frame", "data.frame.TBS", "data.table",
    "tbl", "tibble"),
  seed = NULL,
  nsim = NULL,
  minSS = 10L,
  maxSS = 10000L,
  infSSstep = 12,
  strictSS = TRUE,
  istateReset = TRUE,
  subsetNonmem = TRUE,
  maxAtolRtolFactor = 0.1,
  from = NULL,
  to = NULL,
  by = NULL,
  length.out = NULL,
  iCov = NULL,
  keep = NULL,
  indLinPhiTol = 1e-07,
  indLinPhiM = 0L,
  indLinMatExpType = c("expokit", "Al-Mohy", "arma"),
  indLinMatExpOrder = 6L,
  drop = NULL,
  idFactor = TRUE,
  mxhnil = 0,
  hmxi = 0,
  warnIdSort = TRUE,
  warnDrop = TRUE,
  ssAtol = 1e-08,
  ssRtol = 1e-06,
  safeZero = TRUE,
  safeLog = TRUE,
  safePow = TRUE,
  sumType = c("pairwise", "fsum", "kahan", "neumaier", "c"),
  prodType = c("long double", "double", "logify"),
  resample = NULL,
  resampleID = TRUE,
  maxwhile = 1e+05,
  atolSens = 1e-08,
  rtolSens = 1e-06,
  ssAtolSens = 1e-08,
  ssRtolSens = 1e-06,
  simVariability = NA,
  nLlikAlloc = NULL,
  useStdPow = FALSE,
  naTimeHandle = c("ignore", "warn", "error"),
  addlKeepsCov = FALSE,
  addlDropSs = TRUE,
  ssAtDoseTime = TRUE,
  ss2cancelAllPending = FALSE,
  ssSolved = TRUE,
  linCmtSensType = c("auto", "endpoint5", "endpoint5G", "forward3", "forward3G", "AD",
    "central", "forward", "forwardG", "forwardH", "centralH", "forward3H", "endpointH5",
    "forwardG"),
  linCmtSensH = 1e-04,
  linCmtGillFtol = 0,
  linCmtGillK = 20L,
  linCmtGillStep = 4,
  linCmtGillRtol = sqrt(.Machine$double.eps),
  linCmtShiErr = sqrt(.Machine$double.eps),
  linCmtShiMax = 20L,
  linCmtScale = FALSE,
  linCmtHcmt = NULL,
  linCmtHmeanI = c("geometric", "arithmetic", "harmonic"),
  linCmtHmeanO = c("geometric", "arithmetic", "harmonic"),
  linCmtSuspect = 1e-06,
  linCmtForwardMax = 2L,
  envir = parent.frame()
)

# S3 method for class '`function`'
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxUi'
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxode2tos'
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# S3 method for class 'nlmixr2FitData'
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# S3 method for class 'nlmixr2FitCore'
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# Default S3 method
rxSolve(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  theta = NULL,
  eta = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxSolve'
update(object, ...)

# S3 method for class 'rxode2'
predict(object, ...)

# S3 method for class '`function`'
predict(object, ...)

# S3 method for class 'rxUi'
predict(object, ...)

# S3 method for class 'rxSolve'
predict(object, ...)

# S3 method for class 'rxEt'
predict(object, ...)

# S3 method for class 'rxParams'
predict(object, ...)

# S3 method for class 'rxode2'
simulate(object, nsim = 1L, seed = NULL, ...)

# S3 method for class 'rxSolve'
simulate(object, nsim = 1L, seed = NULL, ...)

# S3 method for class 'rxParams'
simulate(object, nsim = 1L, seed = NULL, ...)

# S3 method for class 'rxSolve'
solve(a, b, ...)

# S3 method for class 'rxUi'
solve(a, b, ...)

# S3 method for class '`function`'
solve(a, b, ...)

# S3 method for class 'rxode2'
solve(a, b, ...)

# S3 method for class 'rxParams'
solve(a, b, ...)

# S3 method for class 'rxEt'
solve(a, b, ...)

rxControl(
  ...,
  params = NULL,
  events = NULL,
  inits = NULL,
  envir = parent.frame()
)
```

## Arguments

- object:

  is a either a rxode2 family of objects, or a file-name with a rxode2
  model specification, or a string with a rxode2 model specification.

- params:

  a numeric named vector with values for every parameter in the ODE
  system; the names must correspond to the parameter identifiers used in
  the ODE specification;

- events:

  an `eventTable` object describing the input (e.g., doses) to the
  dynamic system and observation sampling time points (see
  [`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md));

- inits:

  a vector of initial values of the state variables (e.g., amounts in
  each compartment), and the order in this vector must be the same as
  the state variables (e.g., PK/PD compartments);

- scale:

  a numeric named vector with scaling for ode parameters of the system.
  The names must correspond to the parameter identifiers in the ODE
  specification. Each of the ODE variables will be divided by the
  scaling factor. For example `scale=c(center=2)` will divide the center
  ODE variable by 2.

- method:

  The method for solving ODEs. Currently this supports:

  - `"liblsoda"` thread safe lsoda. This supports parallel thread-based
    solving, and ignores user Jacobian specification.

  - `"lsoda"` – LSODA solver. Does not support parallel thread-based
    solving, but allows user Jacobian specification.

  - `"dop853"` – DOP853 solver. Does not support parallel thread-based
    solving nor user Jacobian specification

  - `"indLin"` – Solving through inductive linearization. The rxode2 dll
    must be setup specially to use this solving routine.

- sigdig:

  Specifies the "significant digits" that the ode solving requests. When
  specified this controls the relative and absolute tolerances of the
  ODE solvers. By default the tolerance is `0.5*10^(-sigdig-2)` for
  regular ODEs. For the sensitivity equations the default is
  `0.5*10\^(-sigdig-1.5)` (sensitivity changes only applicable for
  liblsoda). This also controls the `atol`/`rtol` of the steady state
  solutions. The `ssAtol`/`ssRtol` is `0.5*10\^(-sigdig)` and for the
  sensitivities `0.5*10\^(-sigdig+0.625)`. By default this is
  unspecified (`NULL`) and uses the standard `atol`/`rtol`.

- atol:

  a numeric absolute tolerance (1e-8 by default) used by the ODE solver
  to determine if a good solution has been achieved; This is also used
  in the solved linear model to check if prior doses do not add anything
  to the solution.

- rtol:

  a numeric relative tolerance (`1e-6` by default) used by the ODE
  solver to determine if a good solution has been achieved. This is also
  used in the solved linear model to check if prior doses do not add
  anything to the solution.

- maxsteps:

  maximum number of (internally defined) steps allowed during one call
  to the solver. (5000 by default)

- hmin:

  The minimum absolute step size allowed. The default value is 0.

- hmax:

  The maximum absolute step size allowed. When `hmax=NA` (default), uses
  the average difference + hmaxSd\*sd in times and sampling events. The
  `hmaxSd` is a user specified parameter and which defaults to zero.
  When `hmax=NULL` rxode2 uses the maximum difference in times in your
  sampling and events. The value 0 is equivalent to infinite maximum
  absolute step size.

- hmaxSd:

  The number of standard deviations of the time difference to add to
  hmax. The default is 0

- hini:

  The step size to be attempted on the first step. The default value is
  determined by the solver (when `hini = 0`)

- maxordn:

  The maximum order to be allowed for the nonstiff (Adams) method. The
  default is 12. It can be between 1 and 12.

- maxords:

  The maximum order to be allowed for the stiff (BDF) method. The
  default value is 5. This can be between 1 and 5.

- ...:

  Other arguments including scaling factors for each compartment. This
  includes S# = numeric will scale a compartment \# by a dividing the
  compartment amount by the scale factor, like NONMEM.

- cores:

  Number of cores used in parallel ODE solving. This is equivalent to
  calling
  [`setRxThreads()`](https://nlmixr2.github.io/rxode2/reference/getRxThreads.md)

- covsInterpolation:

  specifies the interpolation method for time-varying covariates. When
  solving ODEs it often samples times outside the sampling time
  specified in `events`. When this happens, the time varying covariates
  are interpolated. Currently this can be:

  - `"linear"` interpolation, which interpolates the covariate by
    solving the line between the observed covariates and extrapolating
    the new covariate value.

  - `"locf"` – Last observation carried forward (the default).

  - `"nocb"` – Next Observation Carried Backward. This is the same
    method that NONMEM uses.

  - `"midpoint"` Last observation carried forward to midpoint; Next
    observation carried backward to midpoint.

    For time-varying covariates where a missing value is present, the
    interpolation method will use either "locf" or "nocb" throughout if
    they are the type of covariate interpolation that is selected.

    When using the linear or midpoint interpolation, the lower point in
    the interpolation will use locf to interpolate missing covariates
    and the upper point will use the nocb to interpolate missing
    covariates.

- naInterpolation:

  specifies the interpolation method for time-varying covariates when
  the instantaneous value is `NA` (not during an explicit interpolation)
  and the `covsInterpolation` is either `"midpoint"` or `"linear"`. This
  can be:

  - `"locf"` – last observation carried forward (default)

  - `"nocb"` – next observation carried backward.

  This will look for the prior value (backwards/locf) when
  instantaneously missing, or the next value when instantaneously
  missing. If all the covariates are missing and you find the
  end/beginning of the individual record, switch direction. If all are
  really missing, then return missing.

- keepInterpolation:

  specifies the interpolation method for variables in the `keep` column.
  When `nlmixr2` creates `mtime`, `addl` doses etc, these items were not
  originally in the dataset. The interpolation methods you can choose
  are:

  - `"locf"` – last observation carried forward (default)

  - `"nocb"` – next observation carried backward.

  - `"na"` – no interpolation, simply put `NA` for the interpolated
    `keep` covariates.

- addCov:

  A boolean indicating if covariates should be added to the output
  matrix or data frame. By default this is disabled.

- sigma:

  Named sigma covariance or Cholesky decomposition of a covariance
  matrix. The names of the columns indicate parameters that are
  simulated. These are simulated for every observation in the solved
  system. When `sigma` is `NA` and you are using it with a `rxode2` ui
  model, the unexplained variability described by the `sigma` matrix are
  set to zero.

- sigmaDf:

  Degrees of freedom of the sigma t-distribution. By default it is
  equivalent to `Inf`, or a normal distribution.

- sigmaLower:

  Lower bounds for simulated unexplained variability (by default -Inf)

- sigmaUpper:

  Upper bounds for simulated unexplained variability (by default Inf)

- nCoresRV:

  Number of cores used for the simulation of the sigma variables. By
  default this is 1. To reproduce the results you need to run on the
  same platform with the same number of cores. This is the reason this
  is set to be one, regardless of what the number of cores are used in
  threaded ODE solving.

- sigmaIsChol:

  Boolean indicating if the sigma is in the Cholesky decomposition
  instead of a symmetric covariance

- sigmaSeparation:

  separation strategy for sigma;

  Tells the type of separation strategy when simulating covariance with
  parameter uncertainty with standard deviations modeled in the
  `thetaMat` matrix.

  - `"lkj"` simulates the correlation matrix from the `rLKJ1` matrix
    with the distribution parameter `eta` equal to the degrees of
    freedom `nu` by `(nu-1)/2`

  - `"separation"` simulates from the identity inverse Wishart
    covariance matrix with `nu` degrees of freedom. This is then
    converted to a covariance matrix and augmented with the modeled
    standard deviations. While computationally more complex than the
    `"lkj"` prior, it performs better when the covariance matrix size is
    greater or equal to 10

  - `"auto"` chooses `"lkj"` when the dimension of the matrix is less
    than 10 and `"separation"` when greater than equal to 10.

- sigmaXform:

  When taking `sigma` values from the `thetaMat` simulations (using the
  separation strategy for covariance simulation), how should the
  `thetaMat` values be turned int standard deviation values:

  - `identity` This is when standard deviation values are directly
    modeled by the `params` and `thetaMat` matrix

  - `variance` This is when the `params` and `thetaMat` simulates the
    variance that are directly modeled by the `thetaMat` matrix

  - `log` This is when the `params` and `thetaMat` simulates `log(sd)`

  - `nlmixrSqrt` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `x\^2` modeled along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrLog` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `exp(x\^2)` along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrIdentity` This is when the `params` and `thetaMat` simulates
    the inverse cholesky decomposed matrix. This only works with a
    diagonal matrix.

- nDisplayProgress:

  An integer indicating the minimum number of c-based solves before a
  progress bar is shown. By default this is 10,000.

- amountUnits:

  This supplies the dose units of a data frame supplied instead of an
  event table. This is for importing the data as an rxode2 event table.

- timeUnits:

  This supplies the time units of a data frame supplied instead of an
  event table. This is for importing the data as an rxode2 event table.

- theta:

  A vector of parameters that will be named `THETA\[#\]` and added to
  parameters

- thetaLower:

  Lower bounds for simulated population parameter variability (by
  default `-Inf`)

- thetaUpper:

  Upper bounds for simulated population unexplained variability (by
  default `Inf`)

- eta:

  A vector of parameters that will be named `ETA\[#\]` and added to
  parameters

- addDosing:

  Boolean indicating if the solve should add rxode2 EVID and related
  columns. This will also include dosing information and estimates at
  the doses. Be default, rxode2 only includes estimates at the
  observations. (default `FALSE`). When `addDosing` is `NULL`, only
  include `EVID=0` on solve and exclude any model-times or `EVID=2`. If
  `addDosing` is `NA` the classic `rxode2` EVID events are returned.
  When `addDosing` is `TRUE` add the event information in NONMEM-style
  format; If `subsetNonmem=FALSE` rxode2 will also include extra event
  types (`EVID`) for ending infusion and modeled times:

  - `EVID=-1` when the modeled rate infusions are turned off (matches
    `rate=-1`)

  - `EVID=-2` When the modeled duration infusions are turned off
    (matches `rate=-2`)

  - `EVID=-10` When the specified `rate` infusions are turned off
    (matches `rate>0`)

  - `EVID=-20` When the specified `dur` infusions are turned off
    (matches `dur>0`)

  - `EVID=101,102,103,...` Modeled time where 101 is the first model
    time, 102 is the second etc.

- stateTrim:

  When amounts/concentrations in one of the states are above this value,
  trim them to be this value. By default Inf. Also trims to -stateTrim
  for large negative amounts/concentrations. If you want to trim between
  a range say `c(0, 2000000)` you may specify 2 values with a lower and
  upper range to make sure all state values are in the reasonable range.

- updateObject:

  This is an internally used flag to update the rxode2 solved object
  (when supplying an rxode2 solved object) as well as returning a new
  object. You probably should not modify it's `FALSE` default unless you
  are willing to have unexpected results.

- omega:

  Estimate of Covariance matrix. When omega is a list, assume it is a
  block matrix and convert it to a full matrix for simulations. When
  `omega` is `NA` and you are using it with a `rxode2` ui model, the
  between subject variability described by the `omega` matrix are set to
  zero.

- omegaDf:

  The degrees of freedom of a t-distribution for simulation. By default
  this is `NULL` which is equivalent to `Inf` degrees, or to simulate
  from a normal distribution instead of a t-distribution.

- omegaIsChol:

  Indicates if the `omega` supplied is a Cholesky decomposed matrix
  instead of the traditional symmetric matrix.

- omegaSeparation:

  Omega separation strategy

  Tells the type of separation strategy when simulating covariance with
  parameter uncertainty with standard deviations modeled in the
  `thetaMat` matrix.

  - `"lkj"` simulates the correlation matrix from the `rLKJ1` matrix
    with the distribution parameter `eta` equal to the degrees of
    freedom `nu` by `(nu-1)/2`

  - `"separation"` simulates from the identity inverse Wishart
    covariance matrix with `nu` degrees of freedom. This is then
    converted to a covariance matrix and augmented with the modeled
    standard deviations. While computationally more complex than the
    `"lkj"` prior, it performs better when the covariance matrix size is
    greater or equal to 10

  - `"auto"` chooses `"lkj"` when the dimension of the matrix is less
    than 10 and `"separation"` when greater than equal to 10.

- omegaXform:

  When taking `omega` values from the `thetaMat` simulations (using the
  separation strategy for covariance simulation), how should the
  `thetaMat` values be turned int standard deviation values:

  - `identity` This is when standard deviation values are directly
    modeled by the `params` and `thetaMat` matrix

  - `variance` This is when the `params` and `thetaMat` simulates the
    variance that are directly modeled by the `thetaMat` matrix

  - `log` This is when the `params` and `thetaMat` simulates `log(sd)`

  - `nlmixrSqrt` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `x\^2` modeled along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrLog` This is when the `params` and `thetaMat` simulates the
    inverse cholesky decomposed matrix with the `exp(x\^2)` along the
    diagonal. This only works with a diagonal matrix.

  - `nlmixrIdentity` This is when the `params` and `thetaMat` simulates
    the inverse cholesky decomposed matrix. This only works with a
    diagonal matrix.

- omegaLower:

  Lower bounds for simulated ETAs (by default -Inf)

- omegaUpper:

  Upper bounds for simulated ETAs (by default Inf)

- nSub:

  Number between subject variabilities (`ETAs`) simulated for every
  realization of the parameters.

- thetaMat:

  Named theta matrix.

- thetaDf:

  The degrees of freedom of a t-distribution for simulation. By default
  this is `NULL` which is equivalent to `Inf` degrees, or to simulate
  from a normal distribution instead of a `t`-distribution.

- thetaIsChol:

  Indicates if the `theta` supplied is a Cholesky decomposed matrix
  instead of the traditional symmetric matrix.

- nStud:

  Number virtual studies to characterize uncertainty in estimated
  parameters.

- dfSub:

  Degrees of freedom to sample the between subject variability matrix
  from the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- dfObs:

  Degrees of freedom to sample the unexplained variability matrix from
  the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- returnType:

  This tells what type of object is returned. The currently supported
  types are:

  - `"rxSolve"` (default) will return a reactive data frame that can
    change easily change different pieces of the solve and update the
    data frame. This is the currently standard solving method in rxode2,
    is used for `rxSolve(object, ...)`, `solve(object,...)`,

  - `"data.frame"` – returns a plain, non-reactive data frame; Currently
    very slightly faster than `returnType="matrix"`

  - `"matrix"` – returns a plain matrix with column names attached to
    the solved object. This is what is used `object$run` as well as
    `object$solve`

  - `"data.table"` – returns a `data.table`; The `data.table` is created
    by reference (ie `setDt()`), which should be fast.

  - `"tbl"` or `"tibble"` returns a tibble format.

- seed:

  an object specifying if and how the random number generator should be
  initialized

- nsim:

  represents the number of simulations. For rxode2, if you supply single
  subject event tables (created with `[eventTable()]`)

- minSS:

  Minimum number of iterations for a steady-state dose

- maxSS:

  Maximum number of iterations for a steady-state dose

- infSSstep:

  Step size for determining if a constant infusion has reached steady
  state. By default this is large value, 12.

- strictSS:

  Boolean indicating if a strict steady-state is required. If a strict
  steady-state is (`TRUE`) required then at least `minSS` doses are
  administered and the total number of steady states doses will continue
  until `maxSS` is reached, or `atol` and `rtol` for every compartment
  have been reached. However, if ODE solving problems occur after the
  `minSS` has been reached the whole subject is considered an invalid
  solve. If `strictSS` is `FALSE` then as long as `minSS` has been
  reached the last good solve before ODE solving problems occur is
  considered the steady state, even though either `atol`, `rtol` or
  `maxSS` have not been achieved.

- istateReset:

  When `TRUE`, reset the `ISTATE` variable to 1 for lsoda and liblsoda
  with doses, like `deSolve`; When `FALSE`, do not reset the `ISTATE`
  variable with doses.

- subsetNonmem:

  subset to NONMEM compatible EVIDs only. By default `TRUE`.

- maxAtolRtolFactor:

  The maximum `atol`/`rtol` that FOCEi and other routines may adjust to.
  By default 0.1

- from:

  When there is no observations in the event table, start observations
  at this value. By default this is zero.

- to:

  When there is no observations in the event table, end observations at
  this value. By default this is 24 + maximum dose time.

- by:

  When there are no observations in the event table, this is the amount
  to increment for the observations between `from` and `to`.

- length.out:

  The number of observations to create if there isn't any observations
  in the event table. By default this is 200.

- iCov:

  A data frame of individual non-time varying covariates to combine with
  the `events` dataset. The `iCov` dataset has one covariate per ID and
  should match the event table

- keep:

  Columns to keep from either the input dataset or the `iCov` dataset.
  With the `iCov` dataset, the column is kept once per line. For the
  input dataset, if any records are added to the data LOCF (Last
  Observation Carried forward) imputation is performed.

- indLinPhiTol:

  the requested accuracy tolerance on exponential matrix.

- indLinPhiM:

  the maximum size for the Krylov basis

- indLinMatExpType:

  This is them matrix exponential type that is use for rxode2. Currently
  the following are supported:

  - `Al-Mohy` Uses the exponential matrix method of Al-Mohy Higham
    (2009)

  - `arma` Use the exponential matrix from RcppArmadillo

  - `expokit` Use the exponential matrix from Roger B. Sidje (1998)

- indLinMatExpOrder:

  an integer, the order of approximation to be used, for the `Al-Mohy`
  and `expokit` values. The best value for this depends on machine
  precision (and slightly on the matrix). We use `6` as a default.

- drop:

  Columns to drop from the output

- idFactor:

  This boolean indicates if original ID values should be maintained.
  This changes the default sequentially ordered ID to a factor with the
  original ID values in the original dataset. By default this is
  enabled.

- mxhnil:

  maximum number of messages printed (per problem) warning that
  `T + H = T` on a step (`H` = step size). This must be positive to
  result in a non-default value. The default value is 0 (or infinite).

- hmxi:

  inverse of the maximum absolute value of `H` to are used. hmxi = 0.0
  is allowed and corresponds to an infinite
  `hmax1 (default). `hmin`and`hmxi`may be changed at any time, but will not take effect until the next change of`H`is considered. This option is only considered with`method="liblsoda"\`.

- warnIdSort:

  Warn if the ID is not present and rxode2 assumes the order of the
  parameters/iCov are the same as the order of the parameters in the
  input dataset.

- warnDrop:

  Warn if column(s) were supposed to be dropped, but were not present.

- ssAtol:

  Steady state atol convergence factor. Can be a vector based on each
  state.

- ssRtol:

  Steady state rtol convergence factor. Can be a vector based on each
  state.

- safeZero:

  Use safe zero divide. By default this is turned on but you may turn it
  off if you wish.

- safeLog:

  Use safe log. When enabled if your value that you are taking log() of
  is negative or zero, this will return `log(machine epsilon)`. By
  default this is turned on.

- safePow:

  Use safe powers. When enabled if your power is negative and your base
  is zero, this will return the `machine epsilon^(negative power)`. By
  default this is turned on.

- sumType:

  Sum type to use for [`sum()`](https://rdrr.io/r/base/sum.html) in
  rxode2 code blocks.

  `pairwise` uses the pairwise sum (fast, default)

  `fsum` uses the PreciseSum package's fsum function (most accurate)

  `kahan` uses Kahan correction

  `neumaier` uses Neumaier correction

  `c` uses no correction: default/native summing

- prodType:

  Product to use for [`prod()`](https://rdrr.io/r/base/prod.html) in
  rxode2 blocks

  `long double` converts to long double, performs the multiplication and
  then converts back.

  `double` uses the standard double scale for multiplication.

- resample:

  A character vector of model variables to resample from the input
  dataset; This sampling is done with replacement. When `NULL` or
  `FALSE` no resampling is done. When `TRUE` resampling is done on all
  covariates in the input dataset

- resampleID:

  boolean representing if the resampling should be done on an individual
  basis `TRUE` (ie. a whole patient is selected) or each covariate is
  resampled independent of the subject identifier `FALSE`. When
  `resampleID=TRUE` correlations of parameters are retained, where as
  when `resampleID=FALSE` ignores patient covariate correaltions. Hence
  the default is `resampleID=TRUE`.

- maxwhile:

  represents the maximum times a while loop is evaluated before exiting.
  By default this is 100000

- atolSens:

  Sensitivity atol, can be different than atol with liblsoda. This
  allows a less accurate solve for gradients (if desired)

- rtolSens:

  Sensitivity rtol, can be different than rtol with liblsoda. This
  allows a less accurate solve for gradients (if desired)

- ssAtolSens:

  Sensitivity absolute tolerance (atol) for calculating if steady state
  has been achieved for sensitivity compartments.

- ssRtolSens:

  Sensitivity relative tolerance (rtol) for calculating if steady state
  has been achieved for sensitivity compartments.

- simVariability:

  determines if the variability is simulated. When `NA` (default) this
  is determined by the solver.

- nLlikAlloc:

  The number of log likelihood endpoints that are used in the model.
  This allows independent log likelihood per endpoint in focei for
  nlmixr2. It likely shouldn't be set, though it won't hurt anything if
  you do (just may take up more memory for larger allocations).

- useStdPow:

  This uses C's `pow` for exponentiation instead of R's `R_pow` or
  `R_pow_di`. By default this is `FALSE`

- naTimeHandle:

  Determines what time of handling happens when the time becomes `NA`:
  current options are:

  - `ignore` this ignores the `NA` time input and passes it through.

  - `warn` (default) this will produce a warning at the end of the
    solve, but continues solving passing through the `NA` time

  - `error` this will stop this solve if this is not a parallel solved
    ODE (otherwise stopping can crash R)

- addlKeepsCov:

  This determines if the additional dosing items repeats the dose only
  (`FALSE`) or keeps the covariates at the record of the dose (`TRUE`)

- addlDropSs:

  When there are steady state doses with an `addl` specification the
  steady state flag is dropped with repeated doses (when `TRUE`) or
  retained (when `FALSE`)

- ssAtDoseTime:

  Boolean that when `TRUE` back calculates the steady concentration at
  the actual time of dose, otherwise when `FALSE` the doses are shifted

- ss2cancelAllPending:

  When `TRUE` the `SS=2` event type cancels all pending doses like
  `SS=1`. When `FALSE` the pending doses not canceled with `SS=2` (the
  infusions started before `SS=2` occurred are canceled, though).

- ssSolved:

  When `TRUE` this will return the solved steady state solutions for the
  linear compartment model. When `FALSE` this will solve to steady state
  using the linear solutions instead. This is only used when the method
  only has `linCmt()` and does not mix ODEs with the solution. The
  default is `TRUE`.

- linCmtSensType:

  The type of linear compartment sensitivity/gradients to use. The
  current options are:

  - `auto` – for one compartment models this will use the `AD` method,
    for 2 and 3 compartment model this will use `forwardG`.

  - `AD` – automatic differentiation (using stan math)

  - `forward` – forward sensitivity where the step size is determined by
    shi 2021 optimization (only once per problem)

  - `forwardG` – forward sensitivity where the step size is determined
    by the Gill 1983 optimization for forward differences (only once per
    problem).

  - `central` – central sensitivity where the step size is determined by
    shi 2021 optimization (only once per problem)

  - `forward3` – three point central difference where step size is
    determined by shi 2021 optimization for central differences (only
    once per problem)

  - `endpoint5` – five point endpoint difference where step size is
    determined by the shi 2021 optimization for central differences
    (only once per problem)

  - `fowardH` – forward sensitivity where the step size is fixed

  - `centralH` – central sensitivity where the step size is fixed

  - `forward3H` – three point central difference where step size is
    fixed

  - `endpoint5H` – five point endpoint difference where step size is
    fixed

- linCmtSensH:

  The step size for the forward and central differences when using the
  option `centralH`, `forwardH`, `foward3H` or `endpoint5H` options. \#'

- linCmtGillFtol:

  The gillFtol is the gradient error tolerance that is acceptable before
  issuing a warning/error about the gradient estimates.

- linCmtGillK:

  The total number of possible steps to determine the optimal
  forward/central difference step size per parameter (by the Gill 1983
  method). If 0, no optimal step size is determined. Otherwise this is
  the optimal step size determined.

- linCmtGillStep:

  When looking for the optimal forward difference step size, this is
  This is the step size to increase the initial estimate by. So each
  iteration the new step size = (prior step size)\*gillStep

- linCmtGillRtol:

  The relative tolerance used for Gill 1983 optimal step size
  determination.

- linCmtShiErr:

  Shi difference error

- linCmtShiMax:

  The maximum number of steps for the optimization of the
  forward-difference step size in linear compartment numeric difference.

- linCmtScale:

  The scale of the linear compartment model. This is applied to
  sensitivity approximation using numeric differences. When `TRUE` or
  `NULL` use default scaling, when `FALSE` use no scaling. If it is one
  elment numeric, the value is duplicated 7 times and applies to all the
  parameters. Otherwise this is a seven element numeric vector implying
  the scaling for each of the linear compartmental model parameters.

- linCmtHcmt:

  This represents the compartments considered when optimizing the
  forward difference step size. When a character vector it can be any of
  the following (multiple allowed):

  - `"depot"` – depot compartment

  - `"central"` – central compartment

  - `"peripheral1"` – peripheral compartment

  - `"peripheral2"` – second peripheral compartment

  - `"concentration"` – concentration value (i.e. central
    compartment/volume)

- linCmtHmeanI:

  This represents the type of sum done for each time-point of the linear
  solved systems (as defined by `"linCmtHcmt"`).

  - `"arithmetic"` – gives the arithmetic mean

  - `"geometric"` – gives the geometric mean

  - `"harmonic"` – gives the harmonic mean

- linCmtHmeanO:

  This represents the type of sum done for the overall problem of the
  linear solved systems (first each time point mean is calculated with
  `linCmtHmeanI`).

  - `"arithmetic"` – gives the arithmetic mean

  - `"geometric"` – gives the geometric mean

  - `"harmonic"` – gives the harmonic mean

- linCmtSuspect:

  The tolerance for gradients in linear compartment solutions to
  re-compute when gradients seem to be zero.

- linCmtForwardMax:

  The maximum number of points in a forward difference to take while
  calculating the gradients. This is an integer from 1 to 3. There is at
  least 1 extra point taken for gradient calculation, if the gradient is
  suspect another is taken (if this value is 2), and finally a third is
  calculated if the gradient is still suspect.

- envir:

  is the environment to look for R user functions (defaults to parent
  environment)

- a:

  when using [`solve()`](https://rdrr.io/pkg/symengine/man/solve.html),
  this is equivalent to the `object` argument. If you specify `object`
  later in the argument list it overwrites this parameter.

- b:

  when using [`solve()`](https://rdrr.io/pkg/symengine/man/solve.html),
  this is equivalent to the `params` argument. If you specify `params`
  as a named argument, this overwrites the output

## Value

An “rxSolve” solve object that stores the solved value in a special
data.frame or other type as determined by `returnType`. By default this
has as many rows as there are sampled time points and as many columns as
system variables (as defined by the ODEs and additional assignments in
the rxode2 model code). It also stores information about the call to
allow dynamic updating of the solved object.

The operations for the object are similar to a data-frame, but expand
the `$` and `[[""]]` access operators and assignment operators to
resolve based on different parameter values, initial conditions, solver
parameters, or events (by updating the `time` variable).

You can call the
[`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md)
methods on the solved object to update the event table and resolve the
system of equations.

## Details

The rest of the document focus on the different ODE solving methods,
followed by the core solving method's options, rxode2 event handling
options, rxode2's numerical stability options, rxode2's output options,
and finally internal rxode2 options or compatibility options.

## References

"New Scaling and Squaring Algorithm for the Matrix Exponential", by Awad
H. Al-Mohy and Nicholas J. Higham, August 2009

Roger B. Sidje (1998). EXPOKIT: Software package for computing matrix
exponentials. ACM - Transactions on Mathematical Software *24*(1),
130-156.

Hindmarsh, A. C. *ODEPACK, A Systematized Collection of ODE Solvers*.
Scientific Computing, R. S. Stepleman et al. (Eds.), North-Holland,
Amsterdam, 1983, pp. 55-64.

Petzold, L. R. *Automatic Selection of Methods for Solving Stiff and
Nonstiff Systems of Ordinary Differential Equations*. Siam J. Sci. Stat.
Comput. 4 (1983), pp. 136-148.

Hairer, E., Norsett, S. P., and Wanner, G. *Solving ordinary
differential equations I, nonstiff problems*. 2nd edition, Springer
Series in Computational Mathematics, Springer-Verlag (1993).

## See also

[`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)

## Author

Matthew Fidler, Melissa Hallow and Wenping Wang
