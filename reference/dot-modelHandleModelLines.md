# Handle model lines

Handle model lines

## Usage

``` r
.modelHandleModelLines(
  modelLines,
  rxui,
  modifyIni = FALSE,
  append = NULL,
  auto = getOption("rxode2.autoVarPiping", TRUE),
  cov = NULL,
  envir
)
```

## Arguments

- modelLines:

  The model lines that are being considered

- rxui:

  The rxode2 UI object

- modifyIni:

  Should the ini() be considered

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

  Environment for evaluation

## Value

New UI

## Author

Matthew L. Fidler
