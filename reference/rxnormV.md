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
#>  [1]  1.0107943 -0.1480862 -1.0682647  0.4104250  0.0199034 -0.9238654
#>  [7] -0.0665825 -0.3854215  0.9946806 -0.4508997
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] -0.35522516  1.27963727  0.10919603  1.00295949 -0.56429872  1.04243749
#>  [7]  0.02535414  1.38325440 -1.61584065 -0.92661395

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 1.485167


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
