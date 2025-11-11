# Conversion between character and integer ODE integration methods for rxode2

If `NULL` is given as the method, all choices are returned as a named
vector.

## Usage

``` r
odeMethodToInt(method = c("liblsoda", "lsoda", "dop853", "indLin"))
```

## Arguments

- method:

  The method for solving ODEs. Currently this supports:

  - `"liblsoda"` thread safe lsoda. This supports parallel thread-based
    solving, and ignores user Jacobian specification.

  - `"lsoda"` – LSODA solver. Does not support parallel thread-based
    solving, but allows user Jacobian specification.

  - `"dop853"` – DOP853 solver. Does not support parallel thread-based
    solving nor user Jacobian specification

  - `"indLin"` – Solving through inductive linearization. The rxode2 dll
    must be setup specially to use this solving routine.

## Value

An integer for the method (unless the input is NULL, in which case, see
the details)
