# Derivative of the Scaled Exponential Linear Unit (SELU) Activation Function

Derivative of the Scaled Exponential Linear Unit (SELU) Activation
Function

## Usage

``` r
dSELU(x)
```

## Arguments

- x:

  A numeric vector. All elements must be finite and non-missing.

## Value

A numeric vector where the derivative of the SELU function has been
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
[`dPReLU()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md),
[`dReLU()`](https://nlmixr2.github.io/rxode2/reference/dReLU.md),
[`dSwish()`](https://nlmixr2.github.io/rxode2/reference/dSwish.md),
[`dlReLU()`](https://nlmixr2.github.io/rxode2/reference/dlReLU.md),
[`dsoftplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md),
[`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md),
[`softplus()`](https://nlmixr2.github.io/rxode2/reference/softplus.md)

## Author

Matthew Fidler

## Examples

``` r
dSELU(c(-1, 0, 1, 2))
#> [1] 0.6467686 1.7580993 1.0507010 1.0507010
# Can also be used in rxode2:
x <- rxode2({
  r=dSELU(time)
})
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
e <- et(c(-1, 0, 1, 2))
rxSolve(x, e)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 4 × 2
#>    time     r
#>   <dbl> <dbl>
#> 1    -1 0.647
#> 2     0 1.76 
#> 3     1 1.05 
#> 4     2 1.05 
```
