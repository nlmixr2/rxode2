# Simulate F variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxf(df1, df2, n = 1L, ncores = 1L)
```

## Arguments

- df1, df2:

  degrees of freedom. `Inf` is allowed.

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

f random deviates

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

rxf(0.5, 0.5, n = 10) # with rxf you have to explicitly state n
#>  [1] 2.268848e-06 1.315195e+00 1.918315e+02 5.195747e-05 1.602946e+00
#>  [6] 2.437402e+00 5.475269e+02 2.882414e+04 2.329001e-07 5.059613e+00
rxf(5, 1, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 23.7578920 28.9085893  0.5882514  0.3202233 20.7032261  0.2102623
#>  [7]  6.0936919  0.7095009  0.6808133  1.2529923

rxf(1, 3)
#> [1] 0.8087568


## This example uses `rxf` directly in the model

rx <- function() {
  model({
    a <- rxf(2, 2)
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
