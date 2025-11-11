# mexpit – Convert log-scale numbers to probabilities

This function converts log-scale numbers to probabilities.

## Usage

``` r
mexpit(...)

dmexpit(...)
```

## Arguments

- ...:

  numeric log-scale numbers to convert to probabilities.

## Value

Probabilities that add up to a number less than 1.

## Details

The probabilities are calculated using the following equation:

\$\$p_i = \frac{e^{x_i}}{1+\sum\_{j=1}^{N-1} e^{x_j}}\$\$

This ensures one remaining probability will add to one, that is

\$\$p_N = \frac{1}{1+\sum\_{j=1}^{N-1} e^{x_j}}\$\$

For the function `dmexpit()`, the element-wise derivatives are
calculated; that is, it returns the diagonal of the Jacobian matrix, \\d
p_i / d x_i\\, not the full Jacobian with off-diagonal terms.

## Author

Matthew L. Fidler

## Examples

``` r
m <- mlogit(0.1, 0.2, 0.3)
mexpit(m)
#> [1] 0.1 0.2 0.3

# derivatives
dmexpit(m)
#> [1] 0.09 0.16 0.21

p <- mexpit(-3, 0.5, 3)
mlogit(p)
#> [1] -3.0  0.5  3.0

```
