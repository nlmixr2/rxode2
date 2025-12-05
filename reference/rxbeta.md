# Simulate beta variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxbeta(shape1, shape2, n = 1L, ncores = 1L)
```

## Arguments

- shape1, shape2:

  non-negative parameters of the Beta distribution.

- n:

  number of observations. If `length(n) > 1`, the length is taken to be
  the number required.

- ncores:

  Number of cores for the simulation

  `rxnorm` simulates using the threefry sitmo generator.

  `rxnormV` used to simulate with the vandercorput simulator, but since
  it didn't satisfy the normal properties it was changed to simple be an
  alias of `rxnorm`. It is no longer supported in `rxode2({})` blocks

## Value

beta random deviates

## Details

Therefore, a simple call to the random number generated followed by a
second call to random number generated may have identical seeds. As the
number of random number generator calls are increased the probability
that the birthday problem will increase.

The key to avoid this problem is to either run all simulations in the
`rxode2` environment once (therefore one seed or series of seeds for the
whole simulation), pre-generate all random variables used for the
simulation, or seed the rxode2 engine with
[`rxSetSeed()`](https://nlmixr2.github.io/rxode2/reference/rxSetSeed.md)

Internally each ID is seeded with a unique number so that the results do
not depend on the number of cores used.

## Examples

``` r
# \donttest{

## Use threefry engine

rxbeta(0.5, 0.5, n = 10) # with rxbeta you have to explicitly state n
#>  [1] 0.20010253 0.57312552 0.37768796 0.86754361 0.85391446 0.99893102
#>  [7] 0.99309318 0.02719286 0.00314745 0.42356861
rxbeta(5, 1, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 0.9998949 0.9416763 0.8655410 0.8965935 0.9685887 0.9084803 0.8582096
#>  [8] 0.7147322 0.9299513 0.9665686

rxbeta(1, 3)
#> [1] 0.1473567


## This example uses `rxbeta` directly in the model

rx <- function() {
  model({
    a <- rxbeta(2, 2)
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
