# rxode2 to symengine environment

rxode2 to symengine environment

## Usage

``` r
rxToSE(
  x,
  envir = NULL,
  progress = FALSE,
  promoteLinSens = TRUE,
  parent = parent.frame()
)

.rxToSE(x, envir = NULL, progress = FALSE)

rxFromSE(
  x,
  unknownDerivatives = c("forward", "central", "error"),
  parent = parent.frame()
)

.rxFromSE(x)
```

## Arguments

- x:

  expression

- envir:

  default is `NULL`; Environment to put symengine variables in.

- progress:

  shows progress bar if true.

- promoteLinSens:

  Promote solved linear compartment systems to sensitivity-based
  solutions.

- parent:

  is the parent environment to look for R-based user functions

- unknownDerivatives:

  When handling derivatives from unknown functions, the translator will
  translate into different types of numeric derivatives. The currently
  supported methods are:

      - `forward` for forward differences
      - `central` for central differences
      - `error` for throwing an error for unknown derivatives

## Value

An rxode2 symengine environment

## Author

Matthew L. Fidler
