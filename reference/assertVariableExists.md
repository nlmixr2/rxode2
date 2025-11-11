# Assert a variable exists in the model

Assert a variable exists in the model

## Usage

``` r
assertVariableExists(ui, x)

testVariableExists(ui, x)
```

## Arguments

- ui:

  rxode2 ui model

- x:

  does the `x` variable exist in the model. If it is a vector of
  variable check to see if any exists, but all must be valid nlmixr2
  variable names

## Value

variable that matches, in the case of multiple variables, the first that
matches. If nothing matches return error

## Functions

- `testVariableExists()`: Test if variable exists

## See also

Other Assertions:
[`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
[`assertCompartmentName()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentName.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md),
[`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)

## Author

Matthew L. Fidler
