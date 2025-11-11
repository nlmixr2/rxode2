# Cumulative distribution of standard normal

Cumulative distribution of standard normal

## Usage

``` r
phi(q)
```

## Arguments

- q:

  vector of quantiles

## Value

cumulative distribution of standard normal distribution

## Author

Matthew Fidler

## Examples

``` r
# phi is equivalent to pnorm(x)
phi(3)
#> [1] 0.9986501

# See
pnorm(3)
#> [1] 0.9986501

# This is provided for NONMEM-like compatibility in rxode2 models
```
