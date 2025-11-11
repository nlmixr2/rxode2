# log-likelihood for the Poisson distribution

log-likelihood for the Poisson distribution

## Usage

``` r
llikPois(x, lambda, full = FALSE)
```

## Arguments

- x:

  non negative integers

- lambda:

  non-negative means

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the pdf value of with `dLambda` that has the
derivatives with respect to the parameters at the observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikPois()` but you have to use all arguments. You
can also get the derivatives with `llikPoisDlambda()`

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikPois(0:7, lambda = 1)
#>          fx dLambda
#> 1 -1.000000      -1
#> 2 -1.000000       0
#> 3 -1.693147       1
#> 4 -2.791759       2
#> 5 -4.178054       3
#> 6 -5.787492       4
#> 7 -7.579251       5
#> 8 -9.525161       6

llikPois(0:7, lambda = 4, full=TRUE)
#>   x lambda        fx dLambda
#> 1 0      4 -4.000000   -1.00
#> 2 1      4 -2.613706   -0.75
#> 3 2      4 -1.920558   -0.50
#> 4 3      4 -1.632876   -0.25
#> 5 4      4 -1.632876    0.00
#> 6 5      4 -1.856020    0.25
#> 7 6      4 -2.261485    0.50
#> 8 7      4 -2.821101    0.75

# In rxode2 you can use:

et <- et(0:10)
et$lambda <- 0.5

model <- function() {
  model({
    fx <- llikPois(time, lambda)
    dLambda <- llikPoisDlambda(time, lambda)
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
#> # A tibble: 11 × 4
#>    time    fx dLambda lambda
#>   <dbl> <dbl>   <dbl>  <dbl>
#> 1     0 -0.5       -1    0.5
#> 2     1 -1.19       1    0.5
#> 3     2 -2.58       3    0.5
#> 4     3 -4.37       5    0.5
#> 5     4 -6.45       7    0.5
#> 6     5 -8.75       9    0.5
#> # ℹ 5 more rows
# }
```
