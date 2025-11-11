# Verify that the compartment exists in a model

Verify that the compartment exists in a model

## Usage

``` r
assertCompartmentExists(ui, x)

testCompartmentExists(ui, x)
```

## Arguments

- ui:

  is the model to test

- x:

  The value to test (can be a vector of strings)

## Value

the value of the compartment that exists; if it is a vector returns the
first item that matches

## Functions

- `testCompartmentExists()`: Test if compartment exists

## See also

Other Assertions:
[`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md),
[`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md),
[`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)

## Author

Matthew Fidler & Bill Denney
