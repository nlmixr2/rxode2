# One correlation sample from the LKJ distribution

One correlation sample from the LKJ distribution

## Usage

``` r
rLKJ1(d, eta = 1, cholesky = FALSE)
```

## Arguments

- d:

  The dimension of the correlation matrix

- eta:

  The scaling parameter of the LKJ distribution. Must be \> 1. Also
  related to the degrees of freedom nu. eta = (nu-1)/2.

- cholesky:

  boolean; If `TRUE` return the cholesky decomposition.

## Value

A correlation sample from the LKJ distribution

## Author

Matthew Fidler (translated to RcppArmadillo) and Emma Schwager
