# Gammap: normalized lower incomplete gamma function

This is the gamma_p from the boost library

## Usage

``` r
gammap(a, z)
```

## Arguments

- a:

  The numeric 'a' parameter in the normalized lower incomplete gamma

- z:

  The numeric 'z' parameter in the normalized lower incomplete gamma

## Value

gammap results

## Details

The gamma p function is given by:

gammap = lowergamma(a, z)/gamma(a)

## Author

Matthew L. Fidler

## Examples

``` r
gammap(1, 3)
#> [1] 0.9502129
gammap(1:3, 3)
#> [1] 0.9502129 0.8008517 0.5768099
gammap(1, 1:3)
#> [1] 0.6321206 0.8646647 0.9502129
```
