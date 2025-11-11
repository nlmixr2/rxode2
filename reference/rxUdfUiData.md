# Return the data.frame that is being processed or setup data.frame for processing

Return the data.frame that is being processed or setup data.frame for
processing

## Usage

``` r
rxUdfUiData(value)
```

## Arguments

- value:

  when specified, this assigns the data.frame to be processed, or resets
  it by assigning it to be `NULL`.

## Value

value of the `data.frame` being processed or `NULL`.

## See also

Other User functions:
[`linMod()`](https://nlmixr2.github.io/rxode2/reference/linMod.md),
[`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md),
[`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md),
[`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md),
[`rxUdfUiMv()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiMv.md),
[`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md),
[`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)

## Author

Matthew L. Fidler

## Examples

``` r
rxUdfUiData()
#> NULL
```
