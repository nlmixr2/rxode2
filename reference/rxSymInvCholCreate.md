# Creates an object for calculating Omega/Omega^-1 and derivatives

Creates an object for calculating Omega/Omega^-1 and derivatives

## Usage

``` r
rxSymInvCholCreate(
  mat,
  diag.xform = c("sqrt", "log", "identity"),
  create.env = TRUE,
  envir = parent.frame()
)
```

## Arguments

- mat:

  Initial Omega matrix

- diag.xform:

  transformation to diagonal elements of OMEGA. or `chol(Omega^-1)`

- create.env:

  – Create an environment to calculate the inverses. (By default TRUE)

- envir:

  – Environment to evaluate function, bu default it is the parent frame.

## Value

A rxSymInv object OR a rxSymInv environment

## Author

Matthew L. Fidler
