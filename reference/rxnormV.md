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
#>  [1]  0.172888709  0.284784040 -0.005692539 -0.421214620  0.452990248
#>  [6]  0.012650216 -0.934263347 -1.208389993  0.298874284 -0.397359774
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  0.06786949 -0.28052929 -0.66690901  0.33965344 -1.16260363  0.25713640
#>  [7] -1.12190228  0.79853569 -0.53656934  0.07836036

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] -0.7994892


## This example uses `rxnorm` directly in the model

rx <- function() {
  model({
    a <- rxnorm()
  })
}

et <- et(1, id = 1:2)

if (Sys.info()["sysname"] != "Darwin") { # skip on macOS for CRAN
  s <- rxSolve(rx, et)
}
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  

# }
```
