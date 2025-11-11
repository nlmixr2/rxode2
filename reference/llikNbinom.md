# Calculate the log likelihood of the negative binomial function (and its derivatives)

Calculate the log likelihood of the negative binomial function (and its
derivatives)

## Usage

``` r
llikNbinom(x, size, prob, full = FALSE)
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
model, you can use `llikNbinom()` but you have to use all arguments. You
can also get the derivative of `prob` with `llikNbinomDprob()`

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
llikNbinom(46:54, 100, 0.5)
#>          fx dProb
#> 1 -13.25200   108
#> 2 -12.81168   106
#> 3 -12.38560   104
#> 4 -11.97335   102
#> 5 -11.57458   100
#> 6 -11.18892    98
#> 7 -10.81603    96
#> 8 -10.45559    94
#> 9 -10.10728    92

llikNbinom(46:54, 100, 0.5, TRUE)
#>    x size prob        fx dProb
#> 1 46  100  0.5 -13.25200   108
#> 2 47  100  0.5 -12.81168   106
#> 3 48  100  0.5 -12.38560   104
#> 4 49  100  0.5 -11.97335   102
#> 5 50  100  0.5 -11.57458   100
#> 6 51  100  0.5 -11.18892    98
#> 7 52  100  0.5 -10.81603    96
#> 8 53  100  0.5 -10.45559    94
#> 9 54  100  0.5 -10.10728    92

# In rxode2 you can use:

et <- et(46:54)
et$size <- 100
et$prob <-0.5

model <- function() {
  model({
    fx <- llikNbinom(time, size, prob)
    dProb <- llikNbinomDprob(time, size, prob)
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
#> 1    46 -13.3   108   100   0.5
#> 2    47 -12.8   106   100   0.5
#> 3    48 -12.4   104   100   0.5
#> 4    49 -12.0   102   100   0.5
#> 5    50 -11.6   100   100   0.5
#> 6    51 -11.2    98   100   0.5
#> # ℹ 3 more rows
# }
```
