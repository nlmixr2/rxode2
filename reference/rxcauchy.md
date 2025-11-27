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
#>  [1] -0.3161956 -9.8293808  0.2817785  0.5647767 -0.8987297 -0.2183899
#>  [7] 18.6560233  2.9999284 -0.5308393 -1.9472665
rxcauchy(0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]   1.55072313   1.22780297 -49.40914679  32.47667344   2.33699668
#>  [6]   4.99563885   2.63606252  -1.93851598  -0.05421445 -11.45911180

rxcauchy(3)
#> [1] 2.112529


## This example uses `rxcauchy` directly in the model

rx <- function() {
  model({
    a <- rxcauchy(2)
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
