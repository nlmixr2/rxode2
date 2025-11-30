# Simulate Weibull variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxweibull(shape, scale = 1, n = 1L, ncores = 1L)
```

## Arguments

- shape, scale:

  shape and scale parameters, the latter defaulting to 1.

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

Weibull random deviates

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

# with rxweibull you have to explicitly state n
rxweibull(shape = 1, scale = 4, n = 10)
#>  [1]  0.3244823  3.1480192  3.8354793  0.3132166  1.7447368 10.0631026
#>  [7]  7.7223081  0.7388451 14.6273546  2.2898651

# You can parallelize the simulation using openMP
rxweibull(shape = 1, scale = 4, n = 10, ncores = 2)
#>  [1] 2.1951564 5.9991509 1.1143366 1.5860025 1.2975847 2.9855209 1.6129127
#>  [8] 0.7326378 1.7116034 6.9761069

rxweibull(3)
#> [1] 0.7875015


## This example uses `rxweibull` directly in the model

rx <- function() {
  model({
    a <- rxweibull(1, 3)
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
