# Test if the rxode2 model has any parameters with user defined boundaries

Test if the rxode2 model has any parameters with user defined boundaries

## Usage

``` r
testRxUnbounded(ui)

assertRxUnbounded(ui, extra = "", .var.name = .vname(ui))

warnRxBounded(ui, extra = "", .var.name = .vname(ui))
```

## Arguments

- ui:

  rxode2 ui

- extra:

  extra information to append to the error message

- .var.name:

  variable name

## Value

boolean indicating if any parameters have user defined boundaries

## Functions

- `assertRxUnbounded()`: Assert that the rxode2 model has any parameters
  with user defined boundaries

- `warnRxBounded()`: Warn that the rxode2 model has any parameters with
  user defined boundaries

## See also

Other Assertions:
[`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
[`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md),
[`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md)

## Author

Matthew L. Fidler

## Examples

``` r
one.cmt <- function() {
  ini({
    tka <- 0.45; label("Ka")
    tcl <- log(c(0, 2.7, 100)); label("Cl")
    tv <- 3.45; label("V")
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd)
  })
}

testRxUnbounded(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> [1] FALSE

try(assertRxUnbounded(one.cmt))
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> Error : 'one.cmt' can not have user defined boundaries

warnRxBounded(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> Warning: 'one.cmt' has the following user-defined boundaries: tcl
```
