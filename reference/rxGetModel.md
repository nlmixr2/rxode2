# Get model properties without compiling it.

Get model properties without compiling it.

## Usage

``` r
rxGetModel(
  model,
  calcSens = NULL,
  calcJac = NULL,
  collapseModel = NULL,
  indLin = FALSE
)
```

## Arguments

- model:

  rxode2 specification

- calcSens:

  boolean indicating if rxode2 will calculate the sensitivities
  according to the specified ODEs.

- calcJac:

  boolean indicating if rxode2 will calculate the Jacobain according to
  the specified ODEs.

- collapseModel:

  boolean indicating if rxode2 will remove all LHS variables when
  calculating sensitivities.

- indLin:

  Calculate inductive linearization matrices and compile with inductive
  linearization support.

## Value

rxode2 trans list

## Author

Matthew L. Fidler
