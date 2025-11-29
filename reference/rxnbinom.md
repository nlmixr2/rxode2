# Simulate Binomial variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxnbinom(size, prob, mu, n = 1L, ncores = 1L)

rxnbinomMu(size, mu, n = 1L, ncores = 1L)
```

## Arguments

- size:

  target for number of successful trials, or dispersion parameter (the
  shape parameter of the gamma mixing distribution). Must be strictly
  positive, need not be integer.

- prob:

  probability of success in each trial. `0 < prob <= 1`.

- mu:

  alternative parametrization via mean: see ‘Details’.

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

negative binomial random deviates. Note that `rxbinom2` uses the `mu`
parameterization an the `rxbinom` uses the `prob` parameterization
(`mu=size/(prob+size)`)

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

rxnbinom(10, 0.9, n = 10) # with rxbinom you have to explicitly state n
#>  [1] 2 3 1 2 0 0 1 0 0 1
rxnbinom(3, 0.5, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] 9 1 5 4 6 0 0 1 7 4

rxnbinom(4, 0.7)
#> [1] 1

# use mu parameter
rxnbinomMu(40, 40, n=10)
#>  [1] 37 49 29 45 42 33 26 55 42 20

## This example uses `rxbinom` directly in the model

rx <- function() {
  model({
    a <- rxnbinom(10, 0.5)
  })
}

et <- et(1, id = 1:100)

s <- rxSolve(rx, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  

rx <- function() {
  model({
    a <- rxnbinomMu(10, 40)
  })
}

s <- rxSolve(rx, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
# }
```
