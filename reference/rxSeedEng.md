# This seeds the engine based on the number of cores used in random number generation

This seeds the engine based on the number of cores used in random number
generation

## Usage

``` r
rxSeedEng(ncores = 1L)
```

## Arguments

- ncores:

  is the number of cores to use.

## Value

Nothing, called for side effects

## Examples

``` r
rxSeedEng()
#> NULL
```
