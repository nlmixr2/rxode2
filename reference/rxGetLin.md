# Get the linear compartment model true function

Get the linear compartment model true function

## Usage

``` r
rxGetLin(model, linCmtSens = c("linCmtA", "linCmtB"), verbose = FALSE)
```

## Arguments

- model:

  This is the ODE model specification. It can be:

  - a string containing the set of ordinary differential equations (ODE)
    and other expressions defining the changes in the dynamic system.

  - a file name where the ODE system equation is contained

  An ODE expression enclosed in `\{\}`

  (see also the `filename` argument). For details, see the sections
  “Details” and `rxode2 Syntax` below.

- linCmtSens:

  The method to calculate the linCmt() solutions

- verbose:

  When `TRUE` be verbose with the linear compartmental model

## Value

model with linCmt() replaced with linCmtA()

## Author

Matthew Fidler
