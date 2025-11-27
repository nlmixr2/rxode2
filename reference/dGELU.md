# Derivatives of GELU

Derivatives of GELU

## Usage

``` r
dGELU(x)

d2GELU(x)

d3GELU(x)

d4GELU(x)
```

## Arguments

- x:

  numeric vector

## Value

numeric vector

## See also

Other Activation Functions:
[`ELU()`](https://nlmixr2.github.io/rxode2/reference/ELU.md),
[`GELU()`](https://nlmixr2.github.io/rxode2/reference/GELU.md),
[`PReLU()`](https://nlmixr2.github.io/rxode2/reference/PReLU.md),
[`ReLU()`](https://nlmixr2.github.io/rxode2/reference/ReLU.md),
[`SELU()`](https://nlmixr2.github.io/rxode2/reference/SELU.md),
[`Swish()`](https://nlmixr2.github.io/rxode2/reference/Swish.md),
[`dELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md),
[`dPReLU()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md),
[`dReLU()`](https://nlmixr2.github.io/rxode2/reference/dReLU.md),
[`dSELU()`](https://nlmixr2.github.io/rxode2/reference/dSELU.md),
[`dSwish()`](https://nlmixr2.github.io/rxode2/reference/dSwish.md),
[`dlReLU()`](https://nlmixr2.github.io/rxode2/reference/dlReLU.md),
[`dsoftplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md),
[`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md),
[`softplus()`](https://nlmixr2.github.io/rxode2/reference/softplus.md)

## Examples

``` r
dGELU(c(-2, -1, 0, 1, 2))
#> [1] -0.08523180 -0.08331547  0.50000000  1.08331547  1.08523180
d2GELU(c(-2, -1, 0, 1, 2))
#> [1] -0.1079819  0.2419707  0.7978846  0.2419707 -0.1079819
d3GELU(c(-2, -1, 0, 1, 2))
#> [1]  0.0000000  0.7259122  0.0000000 -0.7259122  0.0000000
d4GELU(c(-2, -1, 0, 1, 2))
#> [1]  0.4319277  0.4839414 -1.5957691  0.4839414  0.4319277
# you can use rxode2 as well
r <- rxode2({
   r1 <- dGELU(time)
   r2 <- d2GELU(time)
   r3 <- d3GELU(time)
   r4 <- d4GELU(time)
})
#>  
#>  
et <- et(c(-2, -1, 0, 1, 2))
rxSolve(r, et)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 5 × 5
#>    time      r1     r2     r3     r4
#>   <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#> 1    -2 -0.0852 -0.108  0      0.432
#> 2    -1 -0.0833  0.242  0.726  0.484
#> 3     0  0.5     0.798  0     -1.60 
#> 4     1  1.08    0.242 -0.726  0.484
#> 5     2  1.09   -0.108  0      0.432
```
