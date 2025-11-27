# Derivatives Parametric ReLU Activation Function

Derivatives Parametric ReLU Activation Function

## Usage

``` r
dPReLU(x, alpha = 1)

dPReLUa(x, alpha = 1)

dPReLUa1(x, alpha = 1)
```

## Arguments

- x:

  A numeric vector. All elements must be finite and non-missing.

- alpha:

  A numeric scalar. All elements must be finite and non-missing.

## Value

A numeric vector where the derivative(s) of the ELU function has been
applied to each element of `x`.

## See also

Other Activation Functions:
[`ELU()`](https://nlmixr2.github.io/rxode2/reference/ELU.md),
[`GELU()`](https://nlmixr2.github.io/rxode2/reference/GELU.md),
[`PReLU()`](https://nlmixr2.github.io/rxode2/reference/PReLU.md),
[`ReLU()`](https://nlmixr2.github.io/rxode2/reference/ReLU.md),
[`SELU()`](https://nlmixr2.github.io/rxode2/reference/SELU.md),
[`Swish()`](https://nlmixr2.github.io/rxode2/reference/Swish.md),
[`dELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md),
[`dGELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md),
[`dReLU()`](https://nlmixr2.github.io/rxode2/reference/dReLU.md),
[`dSELU()`](https://nlmixr2.github.io/rxode2/reference/dSELU.md),
[`dSwish()`](https://nlmixr2.github.io/rxode2/reference/dSwish.md),
[`dlReLU()`](https://nlmixr2.github.io/rxode2/reference/dlReLU.md),
[`dsoftplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md),
[`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md),
[`softplus()`](https://nlmixr2.github.io/rxode2/reference/softplus.md)

## Author

Matthew L. Fidler

## Examples

``` r
dPReLU(c(-1, 0, 1, 2), 2)
#> [1] 2 2 1 1
dPReLUa(c(-1, 0, 1, 2), 2)
#> [1] -1  0  0  0
dPReLUa1(c(-1, 0, 1, 2), 2)
#> [1] 1 1 0 0


# Can also be used in rxode2:
r <- rxode2({
  r1=dPReLU(time, 2)
  r2a=dPReLUa(time, 2)
  ra=dPReLUa1(time, 2)
})
#>  
#>  

e <- et(c(-1, 0, 1, 2))
rxSolve(r, e)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 4 × 4
#>    time    r1   r2a    ra
#>   <dbl> <dbl> <dbl> <dbl>
#> 1    -1     2    -1     1
#> 2     0     2     0     1
#> 3     1     1     0     0
#> 4     2     1     0     0
```
