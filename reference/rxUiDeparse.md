# This is a generic function for deparsing certain objects when printing out a rxode2 object. Currently it is used for any meta-information

This is a generic function for deparsing certain objects when printing
out a rxode2 object. Currently it is used for any meta-information

rxUiDeparse.rxControl(rxControl(covsInterpolation="linear",
method="dop853", naInterpolation="nocb", keepInterpolation="nocb",
sigmaXform="variance", omegaXform="variance", returnType="data.frame",
sumType="fsum", prodType="logify"), "ctl")

## Usage

``` r
rxUiDeparse(object, var)

# S3 method for class 'lotriFix'
rxUiDeparse(object, var)

# Default S3 method
rxUiDeparse(object, var)

# S3 method for class 'rxControl'
rxUiDeparse(object, var)
```

## Arguments

- object:

  object to be deparsed

- var:

  variable name to be assigned

## Value

parsed R expression that can be used for printing and
[`as.function()`](https://rdrr.io/r/base/as.function.html) calls.

## Author

Matthew L. Fidler

## Examples

``` r
mat <- matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames=list(c("a", "b"), c("a", "b")))

rxUiDeparse(matrix(c(1, 0.1, 0.1, 1), 2, 2, dimnames=list(c("a", "b"), c("a", "b"))), "x")
#> x <- lotri({
#>     a ~ 1
#>     b ~ c(0.1, 1)
#> })
```
