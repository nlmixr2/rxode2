# Simulate Binomial variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxbinom(size, prob, n = 1L, ncores = 1L)
```

## Arguments

- size:

  number of trials (zero or more).

- prob:

  probability of success on each trial.

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

binomial random deviates

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

rxbinom(10, 0.9, n = 10) # with rxbinom you have to explicitly state n
#>  [1] 10  8  9  7 10  9  9  8 10  8
rxbinom(3, 0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 3 3 2 3 2 3 2 1 3 1

rxbinom(4, 0.7)
#> [1] 3


## This example uses `rxbinom` directly in the model

rx <- function() {
  model({
    a <- rxbinom(1, 0.5)
  })
}

et <- et(1, id = 1:2)

if (Sys.info()["sysname"] != "Darwin") { # skip on macOS for CRAN
  s <- rxSolve(rx, et)
}
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
# }
```
