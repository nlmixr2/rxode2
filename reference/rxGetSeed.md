# Get the rxode2 seed

Get the rxode2 seed

## Usage

``` r
rxGetSeed()
```

## Value

rxode2 seed state or -1 when the seed isn't set

## See also

rxSetSeed, rxWithSeed, rxWithPreserveSeed

## Examples

``` r
# without setting seed

rxGetSeed()
#> [1] -1
# Now set the seed
rxSetSeed(42)

rxGetSeed()
#> [1] 42

rxnorm()
#> [1] -0.640542

rxGetSeed()
#> [1] 44

# don't use the rxode2 seed again

rxSetSeed(-1)

rxGetSeed()
#> [1] -1

rxnorm()
#> [1] -0.4821452

rxGetSeed()
#> [1] -1
```
