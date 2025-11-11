# State variables

This returns the model's compartments or states.

## Usage

``` r
rxState(obj = NULL, state = NULL)
```

## Arguments

- obj:

  rxode2 family of objects

- state:

  is a string indicating the state or compartment that you would like to
  lookup.

## Value

If state is missing, return a character vector of all the states.

If state is a string, return the compartment number of the named state.

## See also

[`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)

Other Query model information:
[`rxDfdy()`](https://nlmixr2.github.io/rxode2/reference/rxDfdy.md),
[`rxInits()`](https://nlmixr2.github.io/rxode2/reference/rxInits.md),
[`rxLhs()`](https://nlmixr2.github.io/rxode2/reference/rxLhs.md),
[`rxModelVars()`](https://nlmixr2.github.io/rxode2/reference/rxModelVars.md),
[`rxParams()`](https://nlmixr2.github.io/rxode2/reference/rxParams.md)

## Author

Matthew L.Fidler
