# log likelihood and derivatives for chi-squared distribution

log likelihood and derivatives for chi-squared distribution

## Usage

``` r
llikChisq(x, df, full = FALSE)
```

## Arguments

- x:

  variable that is distributed by chi-squared distribution

- df:

  degrees of freedom (non-negative, but can be non-integer).

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dDf` that has the
derivatives with respect to the `df` parameter the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikChisq()` but you have to use the x and df
arguments. You can also get the derivative of `df` with
`llikChisqDdf()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikChisq(1, df = 1:3, full=TRUE)
#>   x df        fx         dDf
#> 1 1  1 -1.418939  0.63518142
#> 2 1  2 -1.193147 -0.05796576
#> 3 1  3 -1.418939 -0.36481858

llikChisq(1, df = 6:9)
#>          fx        dDf
#> 1 -3.272589 -0.8079658
#> 2 -4.126989 -0.8981519
#> 3 -5.064348 -0.9746324
#> 4 -6.072899 -1.0410091

et <- et(1:3)
et$x <- 1

model <- function() {
  model({
   fx <- llikChisq(x, time)
   dDf <- llikChisqDdf(x, time)
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
#> # A tibble: 3 × 4
#>    time    fx     dDf     x
#>   <dbl> <dbl>   <dbl> <dbl>
#> 1     1 -1.42  0.635      1
#> 2     2 -1.19 -0.0580     1
#> 3     3 -1.42 -0.365      1
# }
```
