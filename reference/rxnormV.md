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
#>  [1]  0.686899534 -0.001168232 -0.321701856 -0.494907399  1.950512537
#>  [6]  1.973538177  0.230160365 -1.721883041  0.919816806  1.735194994
rxnorm(n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  1.45667876  0.07317850  0.12704082 -0.76873491 -0.59037792 -0.51906444
#>  [7] -0.15181101 -0.87724909 -0.71948178 -0.07161635

rxnorm(2, 3) ## The first 2 arguments are the mean and standard deviation
#> [1] 5.032383


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
