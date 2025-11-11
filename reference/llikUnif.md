# log likelihood and derivatives for Unif distribution

log likelihood and derivatives for Unif distribution

## Usage

``` r
llikUnif(x, alpha, beta, full = FALSE)
```

## Arguments

- x:

  variable distributed by a uniform distribution

- alpha:

  is the lower limit of the uniform distribution

- beta:

  is the upper limit of the distribution

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dProb` that has the
derivatives with respect to the `prob` parameters at the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikUnif()` but you have to use the x and rate
arguments. You can also get the derivative of `alpha` or `beta` with
`llikUnifDalpha()` and `llikUnifDbeta()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

llikUnif(1, -2, 2)
#>          fx dAlpha dBeta
#> 1 -1.386294   0.25 -0.25

et  <- et(seq(1,1, length.out=4))
et$alpha <- -2
et$beta <- 2
 
model <- function() {
  model({
    fx <- llikUnif(time, alpha, beta)
    dAlpha<- llikUnifDalpha(time, alpha, beta)
    dBeta <- llikUnifDbeta(time, alpha, beta)
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
#> # A tibble: 4 × 6
#>    time    fx dAlpha dBeta alpha  beta
#>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1     1 -1.39   0.25 -0.25    -2     2
#> 2     1 -1.39   0.25 -0.25    -2     2
#> 3     1 -1.39   0.25 -0.25    -2     2
#> 4     1 -1.39   0.25 -0.25    -2     2
# }
```
