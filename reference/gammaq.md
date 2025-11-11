# Gammaq: normalized upper incomplete gamma function

This is the gamma_q from the boost library

## Usage

``` r
gammaq(a, z)
```

## Arguments

- a:

  The numeric 'a' parameter in the normalized upper incomplete gamma

- z:

  The numeric 'z' parameter in the normalized upper incomplete gamma

## Value

gammaq results

## Details

The gamma q function is given by:

gammaq = uppergamma(a, z)/gamma(a)

## Author

Matthew L. Fidler

## Examples

``` r
gammaq(1, 3)
#> [1] 0.04978707
gammaq(1:3, 3)
#> [1] 0.04978707 0.19914827 0.42319008
gammaq(1, 1:3)
#> [1] 0.36787944 0.13533528 0.04978707
```
