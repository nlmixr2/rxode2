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
#>  [1] 1.926531e+00 2.000853e+00 9.206099e-03 8.889364e-04 3.872734e-01
#>  [6] 4.760471e-02 1.301191e+01 2.871642e+02 3.244250e-08 4.014507e+02
rxf(5, 1, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]    0.6350873    3.3053210    2.4290059    0.1428285 4782.1776033
#>  [6]    3.0144850    0.9910547   11.2321214    2.2102048    1.7826063

rxf(1, 3)
#> [1] 24.01135


## This example uses `rxf` directly in the model

rx <- function() {
  model({
    a <- rxf(2, 2)
  })
}

et <- et(1, id = 1:2)

# s <- rxSolve(rx, et)
# }
```
