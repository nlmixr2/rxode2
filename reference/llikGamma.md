# log likelihood and derivatives for Gamma distribution

log likelihood and derivatives for Gamma distribution

## Usage

``` r
llikGamma(x, shape, rate, full = FALSE)
```

## Arguments

- x:

  variable that is distributed by gamma distribution

- shape:

  this is the distribution's shape parameter. Must be positive.

- rate:

  this is the distribution's rate parameters. Must be positive.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dProb` that has the
derivatives with respect to the `prob` parameters at the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikGamma()` but you have to use the x and rate
arguments. You can also get the derivative of `shape` or `rate` with
`llikGammaDshape()` and `llikGammaDrate()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

llikGamma(1, 1, 10)
#>          fx   dShape dRate
#> 1 -7.697415 2.879801  -0.9

# You can use this in `rxode2` too:

et  <- et(seq(0.001, 1, length.out=10))
et$shape <- 1
et$rate <- 10
 
model <- function() {
  model({
    fx <- llikGamma(time, shape, rate)
    dShape<- llikGammaDshape(time, shape, rate)
    dRate <- llikGammaDrate(time, shape, rate)
  })
}

rxSolve(model, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 6
#>    time      fx dShape  dRate  rate shape
#>   <dbl>   <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 0.001  2.29   -4.03   0.099    10     1
#> 2 0.112  1.18    0.691 -0.012    10     1
#> 3 0.223  0.0726  1.38  -0.123    10     1
#> 4 0.334 -1.04    1.78  -0.234    10     1
#> 5 0.445 -2.15    2.07  -0.345    10     1
#> 6 0.556 -3.26    2.29  -0.456    10     1
#> # ℹ 4 more rows
# }
```
