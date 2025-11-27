# log likelihood and derivatives for Geom distribution

log likelihood and derivatives for Geom distribution

## Usage

``` r
llikGeom(x, prob, full = FALSE)
```

## Arguments

- x:

  variable distributed by a geom distribution

- prob:

  probability of success in each trial. `0 < prob <= 1`.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dProb` that has the
derivatives with respect to the `prob` parameters at the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikGeom()` but you have to use the x and rate
arguments. You can also get the derivative of `prob` with
`llikGeomDprob()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

llikGeom(1:10, 0.2)
#>           fx dProb
#> 1  -1.832581  3.75
#> 2  -2.055725  2.50
#> 3  -2.278869  1.25
#> 4  -2.502012  0.00
#> 5  -2.725156 -1.25
#> 6  -2.948299 -2.50
#> 7  -3.171443 -3.75
#> 8  -3.394586 -5.00
#> 9  -3.617730 -6.25
#> 10 -3.840873 -7.50

et  <- et(1:10)
et$prob <- 0.2
 
model <- function() {
  model({
    fx <- llikGeom(time, prob)
    dProb <- llikGeomDprob(time, prob)
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
#> # A tibble: 10 × 4
#>    time    fx dProb  prob
#>   <dbl> <dbl> <dbl> <dbl>
#> 1     1 -1.83  3.75   0.2
#> 2     2 -2.06  2.5    0.2
#> 3     3 -2.28  1.25   0.2
#> 4     4 -2.50  0      0.2
#> 5     5 -2.73 -1.25   0.2
#> 6     6 -2.95 -2.5    0.2
#> # ℹ 4 more rows
# }
```
