# lowergamma: upper incomplete gamma function

This is the tgamma_lower from the boost library

## Usage

``` r
lowergamma(a, z)
```

## Arguments

- a:

  The numeric 'a' parameter in the upper incomplete gamma

- z:

  The numeric 'z' parameter in the upper incomplete gamma

## Value

lowergamma results

## Details

The lowergamma function is given by:

\$\$lowergamma(a, z) = \int\_{0}^{z}t^{a-1}\cdot e^{-t} dt\$\$

## Author

Matthew L. Fidler

## Examples

``` r
lowergamma(1, 3)
#> [1] 0.9502129

lowergamma(1:3, 3)
#> [1] 0.9502129 0.8008517 1.1536198

lowergamma(1, 1:3)
#> [1] 0.6321206 0.8646647 0.9502129
```
