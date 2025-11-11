# One correlation sample from the Inverse Wishart distribution

This correlation is constructed by transformation of the Inverse Wishart
random covariate to a correlation.

## Usage

``` r
invWR1d(d, nu, omegaIsChol = FALSE)
```

## Arguments

- d:

  The dimension of the correlation matrix

- nu:

  Degrees of freedom of the Wishart distribution

- omegaIsChol:

  is an indicator of if the omega matrix is in the Cholesky
  decomposition. This is only used when `type="invWishart"`

## Value

One correlation sample from the inverse wishart

## Author

Matthew Fidler
