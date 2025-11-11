# This updates the tolerances based on the sensitivity equations

This assumes the normal ODE equations are the first equations and the
ODE is expanded by the forward sensitivities or other type of
sensitivity (like adjoint)

## Usage

``` r
rxControlUpdateSens(rxControl, sensCmt = NULL, ncmt = NULL)
```

## Arguments

- rxControl:

  Input list or rxControl type of list

- sensCmt:

  Number of sensitivity compartments

- ncmt:

  Number of compartments

## Value

Updated rxControl where `$atol`, `$rtol`, `$ssAtol` `$ssRtol` are
updated with different sensitivities for the normal ODEs (first) and a
different sensitivity for the larger compartments (sensitivities).

## Author

Matthew L. Fidler

## Examples

``` r
tmp <- rxControl()

tmp2 <- rxControlUpdateSens(tmp, 3, 6)

tmp2$atol
#> [1] 1e-08 1e-08 1e-08 1e-08 1e-08 1e-08
tmp2$rtol
#> [1] 1e-06 1e-06 1e-06 1e-06 1e-06 1e-06
tmp2$ssAtol
#> [1] 1e-08 1e-08 1e-08 1e-08 1e-08 1e-08
tmp2$ssRtol
#> [1] 1e-06 1e-06 1e-06 1e-06 1e-06 1e-06
```
