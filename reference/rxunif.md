# Simulate uniform variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxunif(min = 0, max = 1, n = 1L, ncores = 1L)
```

## Arguments

- min, max:

  lower and upper limits of the distribution. Must be finite.

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

uniform random numbers

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

rxunif(min = 0, max = 4, n = 10) # with rxunif you have to explicitly state n
#>  [1] 2.4347497 0.5972942 2.3544063 2.5451407 0.6596378 1.8485363 2.0868895
#>  [8] 3.4339510 1.1536325 2.9037937
rxunif(min = 0, max = 4, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 2.540626 2.500096 3.719580 3.872736 3.478180 2.718452 3.179323 1.104371
#>  [9] 1.647580 1.231510

rxunif()
#> [1] 0.3338758


## This example uses `rxunif` directly in the model

rx <- function() {
  model({
    a <- rxunif(0, 3)
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
