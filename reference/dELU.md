# Derivatives of the Exponential Linear Unit (ELU) Activation Function

Derivatives of the Exponential Linear Unit (ELU) Activation Function

## Usage

``` r
dELU(x, alpha = 1)

d2ELU(x, alpha = 1)

d2aELU(x, alpha = 1)

dELUa(x, alpha = 1)

d2ELUa(x, alpha = 1)
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
[`dGELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md),
[`dPReLU()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md),
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
dELU(c(-1, 0, 1, 2), 2)
#> [1] 0.7357589 2.0000000 1.0000000 1.0000000
d2ELU(c(-1, 0, 1, 2), 2)
#> [1] 0.7357589 2.0000000 0.0000000 0.0000000
d2aELU(c(-1, 0, 1, 2), 2)
#> [1] 0.3678794 1.0000000 0.0000000 0.0000000
dELUa(c(-1, 0, 1, 2), 2)
#> [1] -0.6321206  0.0000000  0.0000000  0.0000000
d2ELUa(c(-1, 0, 1, 2), 2)
#> [1] 0.3678794 1.0000000 0.0000000 0.0000000

# Can also be used in rxode2:
r <- rxode2({
  r1=dELU(time, 2)
  r2=d2ELU(time, 2)
  r2a=d2aELU(time, 2)
  ra=dELUa(time, 2)
  r2a=d2ELUa(time, 2)
})
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

e <- et(c(-1, 0, 1, 2))
rxSolve(r, e)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 4 × 5
#>    time    r1    r2   r2a     ra
#>   <dbl> <dbl> <dbl> <dbl>  <dbl>
#> 1    -1 0.736 0.736 0.368 -0.632
#> 2     0 2     2     1      0    
#> 3     1 1     0     0      0    
#> 4     2 1     0     0      0    
```
