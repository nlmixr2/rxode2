# Simulate random Poisson variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxpois(lambda, n = 1L, ncores = 1L)
```

## Arguments

- lambda:

  vector of (non-negative) means.

- n:

  number of random values to return.

- ncores:

  Number of cores for the simulation

  `rxnorm` simulates using the threefry sitmo generator.

  `rxnormV` used to simulate with the vandercorput simulator, but since
  it didn't satisfy the normal properties it was changed to simple be an
  alias of `rxnorm`. It is no longer supported in `rxode2({})` blocks

## Value

poission random number deviates

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

rxpois(lambda = 3, n = 10) # with rxpois you have to explicitly state n
#>  [1] 0 1 4 1 1 4 3 4 2 2
rxpois(lambda = 3, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 2 0 2 2 7 1 4 2 3 3

rxpois(4) ## The first arguments are the lambda parameter
#> [1] 4


## This example uses `rxpois` directly in the model

rx <- function() {
  model({
    a <- rxpois(3)
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
