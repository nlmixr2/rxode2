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
#>  [1] 1.5460877 0.2760776 3.7481604 1.3232471 0.2074676 0.6560125 4.2447157
#>  [8] 1.5100429 9.2924856 0.5102384
rxexp(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 0.2576992182 0.2275406709 0.5433536972 0.1155038138 0.2487963524
#>  [6] 0.2060425594 0.0338518576 0.0875755740 0.0067600281 0.0008986536

rxexp(1)
#> [1] 1.964912


## This example uses `rxexp` directly in the model

rx <- function() {
  model({
    a <- rxexp(2)
  })
}

et <- et(1, id = 1:2)

# s <- rxSolve(rx, et)
# }
```
