# Load a model into a symengine environment

Load a model into a symengine environment

## Usage

``` r
rxS(x, doConst = TRUE, promoteLinSens = FALSE, envir = parent.frame())
```

## Arguments

- x:

  rxode2 object

- doConst:

  Load constants into the environment as well.

- promoteLinSens:

  Promote solved linear compartment systems to sensitivity-based
  solutions.

- envir:

  default is `NULL`; Environment to put symengine variables in.

## Value

rxode2/symengine environment

## Author

Matthew Fidler
