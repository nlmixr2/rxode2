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
#>  [1] 0.090580812 0.633830450 0.005101089 0.013031173 1.323613514 0.030093762
#>  [7] 0.273188314 0.824661465 0.001406051 0.182786650
rxgamma(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 3.736027 2.057629 5.219942 3.083461 3.954426 2.495389 2.344963 3.924449
#>  [9] 7.588346 7.135461

rxgamma(1)
#> [1] 1.418363


## This example uses `rxbeta` directly in the model

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
