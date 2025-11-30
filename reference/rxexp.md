# Simulate exponential variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxexp(rate, n = 1L, ncores = 1L)
```

## Arguments

- rate:

  vector of rates.

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

exponential random deviates

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

rxexp(0.5, n = 10) # with rxexp you have to explicitly state n
#>  [1] 3.92982367 2.30571394 0.13060562 0.36272055 2.55179781 0.02616402
#>  [7] 1.77511025 0.27215501 1.45345123 0.43166959
rxexp(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 0.287795777 0.019977350 0.480766928 0.288353717 0.863187630 0.216799495
#>  [7] 0.003140799 0.026771009 0.067026635 0.038103789

rxexp(1)
#> [1] 0.02705202


## This example uses `rxexp` directly in the model

rx <- function() {
  model({
    a <- rxexp(2)
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
