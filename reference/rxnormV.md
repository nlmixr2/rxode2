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
#>  [1] -0.17161094 -0.10014410  1.55011069  0.26316056 -0.12662675 -0.09064677
#>  [7] -0.03711680  0.50534759  1.46995321 -0.78737765
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  0.172888709 -0.444168200  0.284784040 -0.671484483 -0.005692539
#>  [6]  1.418115665 -0.421214620 -1.560920799  0.452990248  0.926481823

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 2.203608


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
