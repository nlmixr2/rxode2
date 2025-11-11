# This function tests if this object is a iniDf as needed by the UI

This function tests if this object is a iniDf as needed by the UI

## Usage

``` r
testIniDf(iniDf)

assertIniDf(iniDf, extra = "", .var.name = .vname(iniDf), null.ok = FALSE)
```

## Arguments

- iniDf:

  the object to test if it is a rxode2 ui `iniDf` data.frame

- extra:

  information to append to the error message

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

- null.ok:

  \[`logical(1)`\]  
  If set to `TRUE`, `x` may also be `NULL`. In this case only a type
  check of `x` is performed, all additional checks are disabled.

## Value

boolean, indicating if the object is a valid initialization data frame

## Functions

- `assertIniDf()`: Assert that the object is a valid rxode2 ui
  initialization data frame

## See also

Other Assertions:
[`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
[`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md),
[`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)

## Author

Matthew L. Fidler

## Examples

``` r
testIniDf(TRUE)
#> [1] FALSE
```
