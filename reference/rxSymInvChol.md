# Get Omega^-1 and derivatives

Get Omega^-1 and derivatives

## Usage

``` r
rxSymInvChol(
  invObjOrMatrix,
  theta = NULL,
  type = "cholOmegaInv",
  thetaNumber = 0L
)
```

## Arguments

- invObjOrMatrix:

  Object for inverse-type calculations. If this is a matrix, setup the
  object for inversion
  [`rxSymInvCholCreate()`](https://nlmixr2.github.io/rxode2/reference/rxSymInvCholCreate.md)
  with the default arguments and return a reactive s3 object. Otherwise,
  use the inversion object to calculate the requested
  derivative/inverse.

- theta:

  Thetas to be used for calculation. If missing (`NULL`), a special s3
  class is created and returned to access `Omega^1` objects as needed
  and cache them based on the theta that is used.

- type:

  The type of object. Currently the following types are supported:

  - `cholOmegaInv` gives the Cholesky decomposition of the Omega Inverse
    matrix.

  - `omegaInv` gives the Omega Inverse matrix.

  - `d(omegaInv)` gives the `d(Omega^-1)` withe respect to the theta
    parameter specified in `thetaNumber`.

  - `d(D)` gives the `d(diagonal(Omega^-1))` with respect to the theta
    parameter specified in the `thetaNumber` parameter

- thetaNumber:

  For types `d(omegaInv)` and `d(D)`, the theta number that the
  derivative is taken against. This must be positive from 1 to the
  number of thetas defining the Omega matrix.

## Value

Matrix based on parameters or environment with all the matrixes
calculated in variables `omega`, `omegaInv`, `dOmega`, `dOmegaInv`.

## Author

Matthew L. Fidler
