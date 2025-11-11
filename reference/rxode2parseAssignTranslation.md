# This assigns the c level linkages for a roxde2 model

This assigns the c level linkages for a roxde2 model

## Usage

``` r
rxode2parseAssignTranslation(df)
```

## Arguments

- df:

  data frame containing the character column names rxFun, fun, type,
  package, packageFun and the integer column names argMin and argMax

## Value

Nothing called for side effects

## Author

Matthew L. Fidler

## Examples

``` r
rxode2parseAssignTranslation(rxode2parseGetTranslation())
```
