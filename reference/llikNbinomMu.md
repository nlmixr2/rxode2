# Calculate the log likelihood of the negative binomial function (and its derivatives)

Calculate the log likelihood of the negative binomial function (and its
derivatives)

## Usage

``` r
llikNbinomMu(x, size, mu, full = FALSE)
```

## Arguments

- x:

  Number of successes

- size:

  Size of trial

- mu:

  mu parameter for negative binomial

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the pdf value of with `dProb` that has the
derivatives with respect to the parameters at the observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikNbinomMu()` but you have to use all arguments.
You can also get the derivative of `mu` with `llikNbinomMuDmu()`

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikNbinomMu(46:54, 100, 40)
#>          fx       dMu
#> 1 -3.326828 0.1071429
#> 2 -3.446132 0.1250000
#> 3 -3.579663 0.1428571
#> 4 -3.727034 0.1607143
#> 5 -3.887874 0.1785714
#> 6 -4.061827 0.1964286
#> 7 -4.248554 0.2142857
#> 8 -4.447728 0.2321429
#> 9 -4.659037 0.2500000

llikNbinomMu(46:54, 100, 40, TRUE)
#>    x size mu        fx       dMu
#> 1 46  100 40 -3.326828 0.1071429
#> 2 47  100 40 -3.446132 0.1250000
#> 3 48  100 40 -3.579663 0.1428571
#> 4 49  100 40 -3.727034 0.1607143
#> 5 50  100 40 -3.887874 0.1785714
#> 6 51  100 40 -4.061827 0.1964286
#> 7 52  100 40 -4.248554 0.2142857
#> 8 53  100 40 -4.447728 0.2321429
#> 9 54  100 40 -4.659037 0.2500000

et <- et(46:54)
et$size <- 100
et$mu <- 40

model <- function() {
  model({
    fx <- llikNbinomMu(time, size, mu)
    dProb <- llikNbinomMuDmu(time, size, mu)
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
#> # A tibble: 9 × 5
#>    time    fx dProb  size    mu
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    46 -3.33 0.107   100    40
#> 2    47 -3.45 0.125   100    40
#> 3    48 -3.58 0.143   100    40
#> 4    49 -3.73 0.161   100    40
#> 5    50 -3.89 0.179   100    40
#> 6    51 -4.06 0.196   100    40
#> # ℹ 3 more rows
# }
```
