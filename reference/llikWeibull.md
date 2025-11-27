# log likelihood and derivatives for Weibull distribution

log likelihood and derivatives for Weibull distribution

## Usage

``` r
llikWeibull(x, shape, scale, full = FALSE)
```

## Arguments

- x:

  variable distributed by a Weibull distribution

- shape, scale:

  shape and scale parameters, the latter defaulting to 1.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dProb` that has the
derivatives with respect to the `prob` parameters at the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikWeibull()` but you have to use the x and rate
arguments. You can also get the derivative of `shape` or `scale` with
`llikWeibullDshape()` and `llikWeibullDscale()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikWeibull(1, 1, 10)
#>          fx    dShape dScale
#> 1 -2.402585 -1.072327  -0.09

# rxode2 can use this too:

et  <- et(seq(0.001, 1, length.out=10))
et$shape <- 1
et$scale <- 10
 
model <- function() {
  model({
    fx <- llikWeibull(time, shape, scale)
    dShape<- llikWeibullDshape(time, shape, scale)
    dScale <- llikWeibullDscale(time, shape, scale)
  })
}

rxSolve(model, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 6
#>    time    fx dShape  dScale shape scale
#>   <dbl> <dbl>  <dbl>   <dbl> <dbl> <dbl>
#> 1 0.001 -2.30  -8.21 -0.1000     1    10
#> 2 0.112 -2.31  -3.44 -0.0989     1    10
#> 3 0.223 -2.32  -2.72 -0.0978     1    10
#> 4 0.334 -2.34  -2.29 -0.0967     1    10
#> 5 0.445 -2.35  -1.97 -0.0956     1    10
#> 6 0.556 -2.36  -1.73 -0.0944     1    10
#> # ℹ 4 more rows
# }
```
