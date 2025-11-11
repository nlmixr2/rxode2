# log likelihood and derivatives for exponential distribution

log likelihood and derivatives for exponential distribution

## Usage

``` r
llikExp(x, rate, full = FALSE)
```

## Arguments

- x:

  variable that is distributed by exponential distribution

- rate:

  vector of rates.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dRate` that has the
derivatives with respect to the `rate` parameter the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikExp()` but you have to use the x and rate
arguments. You can also get the derivative of `rate` with
`llikExpDrate()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikExp(1, 1:3)
#>          fx      dRate
#> 1 -1.000000  0.0000000
#> 2 -1.306853 -0.5000000
#> 3 -1.901388 -0.6666667

llikExp(1, 1:3, full=TRUE)
#>   x rate        fx      dRate
#> 1 1    1 -1.000000  0.0000000
#> 2 1    2 -1.306853 -0.5000000
#> 3 1    3 -1.901388 -0.6666667

# You can use rxode2 for these too:

et <- et(1:3)
et$x <- 1

model <- function() {
  model({
    fx <- llikExp(x, time)
    dRate <- llikExpDrate(x, time)
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
#> # A tibble: 3 × 4
#>    time    fx  dRate     x
#>   <dbl> <dbl>  <dbl> <dbl>
#> 1     1 -1     0         1
#> 2     2 -1.31 -0.5       1
#> 3     3 -1.90 -0.667     1
# }
```
