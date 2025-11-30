# Simulate gamma variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxgamma(shape, rate = 1, n = 1L, ncores = 1L)
```

## Arguments

- shape:

  The shape of the gamma random variable

- rate:

  an alternative way to specify the scale.

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

gamma random deviates

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

rxgamma(0.5, n = 10) # with rxgamma you have to explicitly state n
#>  [1] 0.440016436 2.113270720 0.402486712 0.021449329 0.035048746 0.057612625
#>  [7] 0.013696571 0.105833474 0.014407980 0.001445439
rxgamma(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 6.469249 7.153268 3.206360 3.916926 6.404371 6.522418 5.211172 2.765756
#>  [9] 5.363930 1.570771

rxgamma(1)
#> [1] 0.2594581


## This example uses `rxgamma` directly in the model

rx <- function() {
  model({
    a <- rxgamma(2)
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
