# Return the model variables that is being processed or setup model variables for processing

Return the model variables that is being processed or setup model
variables for processing

## Usage

``` r
rxUdfUiMv(value)
```

## Arguments

- value:

  when specified, this assigns the model variables to be processed, or
  resets it by assigning it to be `NULL`.

## Value

value of the `modelVariables` being processed or `NULL`.

## See also

Other User functions:
[`linMod()`](https://nlmixr2.github.io/rxode2/reference/linMod.md),
[`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md),
[`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md),
[`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md),
[`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md),
[`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md),
[`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)

## Author

Matthew L. Fidler

## Examples

``` r
rxUdfUiMv()
#> NULL
```
