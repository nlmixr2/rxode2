# Simulate Cauchy variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxcauchy(location = 0, scale = 1, n = 1L, ncores = 1L)
```

## Arguments

- location, scale:

  location and scale parameters.

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

Cauchy random deviates

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

rxcauchy(0, 1, n = 10) # with rxcauchy you have to explicitly state n
#>  [1]  1.8081782 -2.0814166 -2.2317520  0.2836702  5.6635863  2.1914016
#>  [7]  0.9479879  1.0607785 -0.4471394 -5.1158296
rxcauchy(0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  0.6741904  1.5930772  1.4168419  0.5513095  4.4858583 -2.0193630
#>  [7]  3.2429941  0.5643090 -0.0738962  4.7696909

rxcauchy(3)
#> [1] 2.683804


## This example uses `rxcauchy` directly in the model

rx <- function() {
  model({
    a <- rxcauchy(2)
  })
}

et <- et(1, id = 1:2)

# s <- rxSolve(rx, et)
# }
```
