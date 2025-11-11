# Verify that a value is a valid nlmixr2 compartment name

Verify that a value is a valid nlmixr2 compartment name

## Usage

``` r
assertCompartmentName(x)

assertVariableName(x)

assertParameterValue(x)

assertExists(ui, x)

testExists(ui, x)
```

## Arguments

- x:

  The value to test

- ui:

  when needed, this is the rxode2/nlmixr2 model

## Value

The value or an error

## Functions

- `assertVariableName()`: Verify that a value is a valid nlmixr2
  variable name

- `assertParameterValue()`: Verify that a value is a valid nlmixr2
  parameter value

- `assertExists()`: Assert compartment/variable exists

- `testExists()`: Test compartment/variable exists

## See also

Other Assertions:
[`assertCompartmentExists()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentExists.md),
[`assertCompartmentNew()`](https://nlmixr2.github.io/rxode2/reference/assertCompartmentNew.md),
[`assertRxUi()`](https://nlmixr2.github.io/rxode2/reference/assertRxUi.md),
[`assertVariableExists()`](https://nlmixr2.github.io/rxode2/reference/assertVariableExists.md),
[`assertVariableNew()`](https://nlmixr2.github.io/rxode2/reference/assertVariableNew.md),
[`testIniDf()`](https://nlmixr2.github.io/rxode2/reference/testIniDf.md),
[`testRxUnbounded()`](https://nlmixr2.github.io/rxode2/reference/testRxUnbounded.md)

## Author

Bill Denney
