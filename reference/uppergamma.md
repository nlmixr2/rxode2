# uppergamma: upper incomplete gamma function

This is the tgamma from the boost library

## Usage

``` r
uppergamma(a, z)
```

## Arguments

- a:

  The numeric 'a' parameter in the upper incomplete gamma

- z:

  The numeric 'z' parameter in the upper incomplete gamma

## Value

uppergamma results

## Details

The uppergamma function is given by:

\\uppergamma(a, z) = \int\_{z}^{\infty}t^{a-1}\cdot e^{-t} dt\\

## Author

Matthew L. Fidler

## Examples

``` r
uppergamma(1, 3)
#> [1] 0.04978707

uppergamma(1:3, 3)
#> [1] 0.04978707 0.19914827 0.84638016

uppergamma(1, 1:3)
#> [1] 0.36787944 0.13533528 0.04978707
```
