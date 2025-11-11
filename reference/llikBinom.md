# Calculate the log likelihood of the binomial function (and its derivatives)

Calculate the log likelihood of the binomial function (and its
derivatives)

## Usage

``` r
llikBinom(x, size, prob, full = FALSE)
```

## Arguments

- x:

  Number of successes

- size:

  Size of trial

- prob:

  probability of success

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the pdf value of with `dProb` that has the
derivatives with respect to the parameters at the observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikBinom()` but you have to use all arguments. You
can also get the derivative of `prob` with `llikBinomDprob()`

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikBinom(46:54, 100, 0.5)
#>          fx dProb
#> 1 -2.848030   -16
#> 2 -2.709193   -12
#> 3 -2.610102    -8
#> 4 -2.550679    -4
#> 5 -2.530876     0
#> 6 -2.550679     4
#> 7 -2.610102     8
#> 8 -2.709193    12
#> 9 -2.848030    16

llikBinom(46:54, 100, 0.5, TRUE)
#>    x size prob        fx dProb
#> 1 46  100  0.5 -2.848030   -16
#> 2 47  100  0.5 -2.709193   -12
#> 3 48  100  0.5 -2.610102    -8
#> 4 49  100  0.5 -2.550679    -4
#> 5 50  100  0.5 -2.530876     0
#> 6 51  100  0.5 -2.550679     4
#> 7 52  100  0.5 -2.610102     8
#> 8 53  100  0.5 -2.709193    12
#> 9 54  100  0.5 -2.848030    16

# In rxode2 you can use:

et <- et(46:54)
et$size <- 100
et$prob <-0.5

model <- function() {
  model({
    fx <- llikBinom(time, size, prob)
    dProb <- llikBinomDprob(time, size, prob)
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
#>    time    fx dProb  size  prob
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1    46 -2.85   -16   100   0.5
#> 2    47 -2.71   -12   100   0.5
#> 3    48 -2.61    -8   100   0.5
#> 4    49 -2.55    -4   100   0.5
#> 5    50 -2.53     0   100   0.5
#> 6    51 -2.55     4   100   0.5
#> # ℹ 3 more rows
# }
```
