# Parameters specified by the model

This returns the model's parameters that are required to solve the ODE
system, and can be used to pipe parameters into an rxode2 solve

## Usage

``` r
rxParams(obj, ...)

# S3 method for class 'rxode2'
rxParams(
  obj,
  constants = TRUE,
  ...,
  params = NULL,
  inits = NULL,
  iCov = NULL,
  keep = NULL,
  thetaMat = NULL,
  omega = NULL,
  dfSub = NULL,
  sigma = NULL,
  dfObs = NULL,
  nSub = NULL,
  nStud = NULL
)

# S3 method for class 'rxSolve'
rxParams(
  obj,
  constants = TRUE,
  ...,
  params = NULL,
  inits = NULL,
  iCov = NULL,
  keep = NULL,
  thetaMat = NULL,
  omega = NULL,
  dfSub = NULL,
  sigma = NULL,
  dfObs = NULL,
  nSub = NULL,
  nStud = NULL
)

# S3 method for class 'rxEt'
rxParams(
  obj,
  ...,
  params = NULL,
  inits = NULL,
  iCov = NULL,
  keep = NULL,
  thetaMat = NULL,
  omega = NULL,
  dfSub = NULL,
  sigma = NULL,
  dfObs = NULL,
  nSub = NULL,
  nStud = NULL
)

rxParam(obj, ...)
```

## Arguments

- obj:

  rxode2 family of objects

- ...:

  Other arguments including scaling factors for each compartment. This
  includes S# = numeric will scale a compartment \# by a dividing the
  compartment amount by the scale factor, like NONMEM.

- constants:

  is a boolean indicting if constants should be included in the list of
  parameters. Currently rxode2 parses constants into variables in case
  you wish to change them without recompiling the rxode2 model.

- params:

  a numeric named vector with values for every parameter in the ODE
  system; the names must correspond to the parameter identifiers used in
  the ODE specification;

- inits:

  a vector of initial values of the state variables (e.g., amounts in
  each compartment), and the order in this vector must be the same as
  the state variables (e.g., PK/PD compartments);

- iCov:

  A data frame of individual non-time varying covariates to combine with
  the `events` dataset. The `iCov` dataset has one covariate per ID and
  should match the event table

- keep:

  Columns to keep from either the input dataset or the `iCov` dataset.
  With the `iCov` dataset, the column is kept once per line. For the
  input dataset, if any records are added to the data LOCF (Last
  Observation Carried forward) imputation is performed.

- thetaMat:

  Named theta matrix.

- omega:

  Estimate of Covariance matrix. When omega is a list, assume it is a
  block matrix and convert it to a full matrix for simulations. When
  `omega` is `NA` and you are using it with a `rxode2` ui model, the
  between subject variability described by the `omega` matrix are set to
  zero.

- dfSub:

  Degrees of freedom to sample the between subject variability matrix
  from the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- sigma:

  Named sigma covariance or Cholesky decomposition of a covariance
  matrix. The names of the columns indicate parameters that are
  simulated. These are simulated for every observation in the solved
  system. When `sigma` is `NA` and you are using it with a `rxode2` ui
  model, the unexplained variability described by the `sigma` matrix are
  set to zero.

- dfObs:

  Degrees of freedom to sample the unexplained variability matrix from
  the inverse Wishart distribution (scaled) or scaled inverse chi
  squared distribution.

- nSub:

  Number between subject variabilities (`ETAs`) simulated for every
  realization of the parameters.

- nStud:

  Number virtual studies to characterize uncertainty in estimated
  parameters.

## Value

When extracting the parameters from an rxode2 model, a character vector
listing the parameters in the model.

## See also

Other Query model information:
[`rxDfdy()`](https://nlmixr2.github.io/rxode2/reference/rxDfdy.md),
[`rxInits()`](https://nlmixr2.github.io/rxode2/reference/rxInits.md),
[`rxLhs()`](https://nlmixr2.github.io/rxode2/reference/rxLhs.md),
[`rxModelVars()`](https://nlmixr2.github.io/rxode2/reference/rxModelVars.md),
[`rxState()`](https://nlmixr2.github.io/rxode2/reference/rxState.md)

## Author

Matthew L.Fidler
