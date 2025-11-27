# Default Softplus Activation Function

Default Softplus Activation Function

## Usage

``` r
dsoftplus(x)

d2softplus(x)

d3softplus(x)

d4softplus(x)
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
[`lReLU()`](https://nlmixr2.github.io/rxode2/reference/lReLU.md),
[`softplus()`](https://nlmixr2.github.io/rxode2/reference/softplus.md)

## Author

Matthew L. Fidler

## Examples

``` r
dsoftplus(c(-1, 0, 1, 2))
#> [1] 0.2689414 0.5000000 0.7310586 0.8807971

# You can use rxode2 too:

r <- rxode2({
 s1 <- dsoftplus(time)
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
#>    time    s1
#>   <dbl> <dbl>
#> 1    -1 0.269
#> 2     0 0.5  
#> 3     1 0.731
#> 4     2 0.881
```
