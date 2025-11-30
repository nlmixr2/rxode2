# Simulate chi-squared variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxchisq(df, n = 1L, ncores = 1L)
```

## Arguments

- df:

  degrees of freedom (non-negative, but can be non-integer).

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

chi squared random deviates

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

rxchisq(0.5, n = 10) # with rxchisq you have to explicitly state n
#>  [1] 3.30573119 0.01474175 0.69487370 0.95370212 0.01775513 4.98306875
#>  [7] 0.25408197 0.20094271 0.40818131 0.03259946
rxchisq(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  1.145127  4.797569  2.427267  3.633049  4.528524  2.622606  4.819438
#>  [8]  7.069062 10.926754  1.924830

rxchisq(1)
#> [1] 1.951945


## This example uses `rxchisq` directly in the model

rx <- function() {
  model({
    a <- rxchisq(2)
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
