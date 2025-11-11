# Internal translation to get model variables list

Internal translation to get model variables list

## Usage

``` r
rxode2parse(
  model,
  linear = FALSE,
  linCmtSens = c("linCmtA", "linCmtB"),
  verbose = FALSE,
  code = NULL,
  envir = parent.frame()
)
```

## Arguments

- model:

  Model (either file name or string)

- linear:

  boolean indicating if linear compartment model should be generated
  from `linCmt()` (default FALSE)

- linCmtSens:

  Linear compartment model sensitivity type

- verbose:

  is a boolean indicating the type of model detected with `linCmt()`
  parsing

- code:

  is a file name where the c code is written to (for testing purposes
  mostly, it needs `rxode2` to do anything fancy)

- envir:

  is the environment to look for R user functions (defaults to parent
  environment)

## Value

A rxModelVars object that has the model variables of a rxode2 syntax
expression

## Examples

``` r
rxode2parse("a=3")
#>  
#> rxode2 model variables (see str to see all variables)
#> value$params: a
```
