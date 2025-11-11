# This gives the current number in the ui of the particular function being called.

If this is called outside of function parsing or the input is unexpected
this returns 1L. This is useful when writing replacement UI functions

## Usage

``` r
rxUdfUiNum()
```

## Value

integer greater than 1L

## See also

Other User functions:
[`linMod()`](https://nlmixr2.github.io/rxode2/reference/linMod.md),
[`rxUdfUiControl()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiControl.md),
[`rxUdfUiData()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiData.md),
[`rxUdfUiEst()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiEst.md),
[`rxUdfUiIniLhs()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiIniLhs.md),
[`rxUdfUiMv()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiMv.md),
[`rxUdfUiParsing()`](https://nlmixr2.github.io/rxode2/reference/rxUdfUiParsing.md)

## Author

Matthew L. Fidler

## Examples

``` r
rxUdfUiNum()
#> [1] 1
```
