# Scaled Inverse Chi Squared distribution

Scaled Inverse Chi Squared distribution

## Usage

``` r
rinvchisq(n = 1L, nu = 1, scale = 1)
```

## Arguments

- n:

  Number of random samples

- nu:

  degrees of freedom of inverse chi square

- scale:

  Scale of inverse chi squared distribution (default is 1).

## Value

a vector of inverse chi squared deviates.

## Examples

``` r
rinvchisq(3, 4, 1) ## Scale = 1, degrees of freedom = 4
#> [1] 1.507912 2.595590 1.620441
rinvchisq(2, 4, 2) ## Scale = 2, degrees of freedom = 4
#> [1] 3.309325 2.550773
```
