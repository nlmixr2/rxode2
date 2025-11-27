# Switch Activation Function

The switch activation function is defined as:

## Usage

``` r
Swish(x)
```

## Arguments

- x:

  A numeric vector. All elements must be finite and non-missing.

## Value

A numeric vector where the ReLU function has been applied to each
element of `x`.

## Details

\$\$f(x) = x \cdot \text{sigmoid}(x)\$\$

## See also

Other Activation Functions:
[`ELU()`](https://nlmixr2.github.io/rxode2/reference/ELU.md),
[`GELU()`](https://nlmixr2.github.io/rxode2/reference/GELU.md),
[`PReLU()`](https://nlmixr2.github.io/rxode2/reference/PReLU.md),
[`ReLU()`](https://nlmixr2.github.io/rxode2/reference/ReLU.md),
[`SELU()`](https://nlmixr2.github.io/rxode2/reference/SELU.md),
[`dELU()`](https://nlmixr2.github.io/rxode2/reference/dELU.md),
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

Matthew Fidler

## Examples

``` r
Swish(c(-1, 0, 1, 2))
#> [1] -0.2689414  0.0000000  0.7310586  1.7615942

# Can also be used in rxode2:
x <- rxode2({
   r<- Swish(time)
})
#>  
#>  

e <- et(c(-1, 0, 1, 2))

rxSolve(x, e)
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 4 × 2
#>    time      r
#>   <dbl>  <dbl>
#> 1    -1 -0.269
#> 2     0  0    
#> 3     1  0.731
#> 4     2  1.76 
```
