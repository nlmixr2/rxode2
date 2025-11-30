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
#>  [1]  1.4566788  0.1270408 -0.5903779 -0.1518110 -0.7194818 -0.2137435
#>  [7]  0.7710692 -1.1108157 -0.8684514 -1.6103059
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  1.0107943  0.6058134 -0.1480862 -0.2087799 -1.0682647  1.0362662
#>  [7]  0.4104250 -1.6970379  0.0199034 -1.1134117

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 0.9343245


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
