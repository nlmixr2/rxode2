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
#>  [1] -0.40190917 -1.32333022  0.96333714 -1.52839559 -0.53735594 -0.35484226
#>  [7] -1.26935036  2.41024850  0.05656893 -0.51047928
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  0.686899534 -0.167044769 -0.001168232 -1.112597790 -0.321701856
#>  [6] -0.597351624 -0.494907399 -0.283706762  1.950512537 -1.258012025

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 6.370036


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
