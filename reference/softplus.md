# Softplus Activation Function

Softplus Activation Function

## Usage

``` r
softplus(x)
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
[`dGELU()`](https://nlmixr2.github.io/rxode2/reference/dGELU.md),
[`dPReLU()`](https://nlmixr2.github.io/rxode2/reference/dPReLU.md),
[`dReLU()`](https://nlmixr2.github.io/rxode2/reference/dReLU.md),
[`dSELU()`](https://nlmixr2.github.io/rxode2/reference/dSELU.md),
[`dSwish()`](https://nlmixr2.github.io/rxode2/reference/dSwish.md),
[`dlReLU()`](https://nlmixr2.github.io/rxode2/reference/dlReLU.md),
[`dsoftplus()`](https://nlmixr2.github.io/rxode2/reference/dsoftplus.md),
[`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md)

## Author

Matthew L. Fidler

## Examples

``` r
softplus(c(-1, 0, 1, 2))
#> [1] 0.3132617 0.6931472 1.3132617 2.1269280

# You can use rxode2 too:

r <- rxode2({
 s <- softplus(time)
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
#> # A tibble: 4 × 2
#>    time     s
#>   <dbl> <dbl>
#> 1    -1 0.313
#> 2     0 0.693
#> 3     1 1.31 
#> 4     2 2.13 
```
