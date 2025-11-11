# Initial Values and State values for a rxode2 object

Returns the initial values of the rxDll object

## Usage

``` r
rxInits(
  obj,
  vec = NULL,
  req = NULL,
  defaultValue = 0,
  noerror = FALSE,
  noini = FALSE,
  rxLines = FALSE
)

rxInit(
  obj,
  vec = NULL,
  req = NULL,
  defaultValue = 0,
  noerror = FALSE,
  noini = FALSE,
  rxLines = FALSE
)
```

## Arguments

- obj:

  rxDll, rxode2, or named vector representing default initial arguments

- vec:

  If supplied, named vector for the model.

- req:

  Required names, and the required order for the ODE solver

- defaultValue:

  a number or NA representing the default value for parameters missing
  in `vec`, but required in `req`.

- noerror:

  is a boolean specifying if an error should be thrown for missing
  parameter values when `default` = `NA`

## Value

Initial values of the rxDll object

## See also

Other Query model information:
[`rxDfdy()`](https://nlmixr2.github.io/rxode2/reference/rxDfdy.md),
[`rxLhs()`](https://nlmixr2.github.io/rxode2/reference/rxLhs.md),
[`rxModelVars()`](https://nlmixr2.github.io/rxode2/reference/rxModelVars.md),
[`rxParams()`](https://nlmixr2.github.io/rxode2/reference/rxParams.md),
[`rxState()`](https://nlmixr2.github.io/rxode2/reference/rxState.md)

## Author

Matthew L.Fidler
