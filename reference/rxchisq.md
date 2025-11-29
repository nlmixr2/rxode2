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
#>  [1] 1.399566e-03 4.957758e-03 2.501368e+00 1.722130e+00 6.431632e-01
#>  [6] 2.101400e-02 2.494125e-06 1.971534e-01 6.847884e-01 2.126272e-01
rxchisq(5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1]  4.040672  3.568613  3.058863  5.186983  7.802258  2.226861 22.525644
#>  [8]  2.061224  1.905642  6.718301

rxchisq(1)
#> [1] 0.0471221


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
