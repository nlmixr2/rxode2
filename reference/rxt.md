# Simulate student t variable from threefry generator

Care should be taken with this method not to encounter the birthday
problem, described
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>.
Since the `sitmo` `threefry`, this currently generates one random
deviate from the uniform distribution to seed the engine `threefry` and
then run the code.

## Usage

``` r
rxt(df, n = 1L, ncores = 1L)
```

## Arguments

- df:

  degrees of freedom (\\\> 0\\, maybe non-integer). `df = Inf` is
  allowed.

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

t-distribution random numbers

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
rxt(df = 3, n = 10) # with rxt you have to explicitly state n
#>  [1]  0.8220556  0.6661569 -0.8835700  0.1632563 -0.4805511 -1.6321660
#>  [7] -0.6055073 -0.6486948  0.1985553  1.3762934
rxt(df = 3, n = 10, ncores = 2) # You can parallelize the simulation using openMP
#>  [1] -1.7636096 -0.5340158 -0.5320123 -0.8796961  0.2705535 -0.4798122
#>  [7] -0.5863283 -0.0247823 -0.3826859  2.0279956

rxt(4) ## The first argument is the df parameter
#> [1] -1.699156


## This example uses `rxt` directly in the model

rx <- function() {
   model({
    a <- rxt(3)
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
