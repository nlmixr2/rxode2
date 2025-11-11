# Return the control that is being processed or setup control for processing

Return the control that is being processed or setup control for
processing

## Usage

``` r
rxUdfUiControl(value)
```

## Arguments

- value:

  when specified, this assigns the control to be processed, or resets it
  by assigning it to be `NULL`.

## Value

value of the `data.frame` being processed or `NULL`.

## See also

Other User functions:
[`linMod()`](https://nlmixr2.github.io/rxode2/reference/linMod.md),
[`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md),
[`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md),
[`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md),
[`rxUdfUiMv()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiMv.md),
[`rxUdfUiNum()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiNum.md),
[`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)

## Author

Matthew L. Fidler

## Examples

``` r
rxUdfUiControl()
```
