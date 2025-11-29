# Set the parallel seed for rxode2 random number generation

This sets the seed for the rxode2 parallel random number generation. If
set, then whenever a seed is set for the threefry or vandercorput
simulation engine, it will use this seed, increment for the number of
seeds and continue with the sequence the next time the random number
generator is called.

## Usage

``` r
rxSetSeed(seed)
```

## Arguments

- seed:

  An integer that represents the rxode2 parallel and internal random
  number generator seed. When positive, use this seed for random number
  generation and increment and reseed any parallel or new engines that
  are being called. When negative, turn off the rxode2 seed and generate
  a seed from the R's uniform random number generator. Best practice is
  to set this seed.

## Value

Nothing, called for its side effects

## Details

In contrast, when this is not called, the time that the vandercorput or
threefry simulation engines are seeded it comes from a uniform random
number generated from the standard R random seed. This may cause a
duplicate seed based on the R seed state. This means that there could be
correlations between simulations that do not exist This will avoid the
birthday problem picking exactly the same seed using the seed state of
the R random number generator. The more times the seed is called, the
more likely this becomes.

## References

JD Cook. (2016). Random number generator seed mistakes.
<https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/>

## See also

rxGetSeed, rxWithSeed, rxWithPreserveSeed

## Author

Matthew Fidler

## Examples

``` r
rxSetSeed(42)

# seed with generator 42
rxnorm()
#> [1] -0.640542

# Use R's random number generator
rnorm(1)
#> [1] -1.388039

rxSetSeed(42)

# reproduces the same number
rxnorm()
#> [1] -0.640542

# But R's random number is not the same

rnorm(1)
#> [1] 1.630818

# If we reset this to use the R's seed
# (internally rxode2 uses a uniform random number to span seeds)
# This can lead to duplicate sequences and seeds

rxSetSeed(-1)

# Now set seed works for both.

# This is not recommended, but illustrates the different types of
# seeds that can be generated.

set.seed(42)

rxnorm()
#> [1] 0.235007

rnorm(1)
#> [1] -0.5646982

set.seed(42)

rxnorm()
#> [1] 0.235007

rnorm(1)
#> [1] -0.5646982
```
