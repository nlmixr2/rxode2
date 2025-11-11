# gammapDer: derivative of gammap

This is the gamma_p_derivative from the boost library

## Usage

``` r
gammapDer(a, z)
```

## Arguments

- a:

  The numeric 'a' parameter in the upper incomplete gamma

- z:

  The numeric 'z' parameter in the upper incomplete gamma

## Value

lowergamma results

## Author

Matthew L. Fidler

## Examples

``` r
gammapDer(1:3, 3)
#> [1] 0.04978707 0.14936121 0.22404181

gammapDer(1, 1:3)
#> [1] 0.36787944 0.13533528 0.04978707
```
