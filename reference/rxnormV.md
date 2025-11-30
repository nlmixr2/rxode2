# Simulate random normal variable from threefry generator

Simulate random normal variable from threefry generator

## Usage

``` r
rxnormV(mean = 0, sd = 1, n = 1L, ncores = 1L)

rxnorm(mean = 0, sd = 1, n = 1L, ncores = 1L)
```

## Arguments

- mean:

  vector of means.

- sd:

  vector of standard deviations.

- n:

  number of observations

- ncores:

  Number of cores for the simulation

  `rxnorm` simulates using the threefry sitmo generator.

  `rxnormV` used to simulate with the vandercorput simulator, but since
  it didn't satisfy the normal properties it was changed to simple be an
  alias of `rxnorm`. It is no longer supported in `rxode2({})` blocks

## Value

normal random number deviates

## Examples

``` r
# \donttest{
## Use threefry engine

rxnorm(n = 10) # with rxnorm you have to explicitly state n
#>  [1] -0.35522516  0.10919603 -0.56429872  0.02535414 -1.61584065 -0.73493239
#>  [7] -1.76977837 -1.31534178 -0.09703258  1.29738697
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] -0.1716109  0.6504483 -0.1001441 -1.8656371  1.5501107  0.9400028
#>  [7]  0.2631606 -1.0612671 -0.1266268 -0.9755515

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 2.518666


## This example uses `rxnorm` directly in the model

rx <- function() {
  model({
    a <- rxnorm()
  })
}

et <- et(1, id = 1:2)

s <- rxSolve(rx, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  

# }
```
