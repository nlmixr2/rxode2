# Model block for rxode2/nlmixr models

Model block for rxode2/nlmixr models

## Usage

``` r
# S3 method for class '`function`'
model(
  x,
  ...,
  append = NULL,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxUi'
model(
  x,
  ...,
  append = NULL,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxode2'
model(
  x,
  ...,
  append = NULL,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir = parent.frame()
)

# S3 method for class 'rxModelVars'
model(
  x,
  ...,
  append = NULL,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir = parent.frame()
)

model(
  x,
  ...,
  append = FALSE,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir = parent.frame()
)

# Default S3 method
model(x, ..., append = FALSE, cov = NULL, envir = parent.frame())
```

## Arguments

- x:

  model expression

- ...:

  Other arguments

- append:

  This is a boolean to determine if the lines are appended in piping.
  The possible values for this is:

  - `TRUE` which is when the lines are appended to the model instead of
    replaced

  - `FALSE` when the lines are replaced in the model (default)

  - `NA` is when the lines are pre-pended to the model instead of
    replaced

  - `lhs expression`, which will append the lines after the last
    observed line of the expression `lhs`

- auto:

  This boolean tells if piping automatically selects the parameters
  should be characterized as a population parameter, between subject
  variability, or a covariate. When `TRUE` this automatic selection
  occurs. When `FALSE` this automatic selection is turned off and
  everything is added as a covariate (which can be promoted to a
  parameter with the `ini` statement). By default this is `TRUE`, but it
  can be changed by `options(rxode2.autoVarPiping=FALSE)`.

- cov:

  is a character vector of variables that should be assumed to be
  covariates. This will override automatic promotion to a population
  parameter estimate (or an eta)

- envir:

  the `environment` in which unevaluated model expressions is to be
  evaluated. May also be `NULL`, a list, a data frame, a pairlist or an
  integer as specified to `sys.call`.

## Value

Model block with ini information included. `ini` must be called before
`model` block

## Author

Matthew Fidler
