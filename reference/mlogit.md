# mlogit – Convert multiple probabilities to log-scale numbers

These multiple probabilities need to add up to be less than 1.

## Usage

``` r
mlogit(
  ...,
  maxiter = 10000,
  rtol = 1e-10,
  atol = 1e-12,
  ctol = 1e-12,
  returnRoot = FALSE
)
```

## Arguments

- ...:

  numeric probabilities to convert to log-scale numbers. These
  probabilities must add to a number less than 1 and are used in the
  mix() estimation algorithm.

- maxiter:

  maximal number of iterations allowed.

- rtol:

  relative error tolerance, either a scalar or a vector, one value for
  each element in the unknown x.

- atol:

  absolute error tolerance, either a scalar or a vector, one value for
  each element in x.

- ctol:

  a scalar. If between two iterations, the maximal change in the
  variable values is less than this amount, then it is assumed that the
  root is found.

- returnRoot:

  logical; If TRUE, return the root object, otherwise return the root
  itself.

## Value

A numeric vector of the log-scale numbers for use in regressions where
the sum of a set of probabilities must add to be one.

## Details

Once converted to log-scale numbers, they can be used in the in the
[`mexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md) to
get the probabilities back with the following equation

\$\$p_i = \frac{e^{x_i}}{1+\sum\_{j=1}^{N-1} e^{x_j}}\$\$

This ensures one remaining probability will add to one, that is

\$\$p_N = \frac{1}{1+\sum\_{j=1}^{N-1} e^{x_j}}\$\$

Unfortunately, the log-scale inverse cannot be solved analytically, so
it is solved with the
[`rootSolve::multiroot()`](https://rdrr.io/pkg/rootSolve/man/multiroot.html)
function.

You may adjust some of the root finding options when using this
function.

When running `nlmixr2` with mixture models (ie.
[`mix()`](https://nlmixr2.github.io/rxode2/reference/mix.md) models),
the `mlogit()` function is called in the probabilities and the log-based
values are used in the optimization problem. The probabilities are
determined by the
[`mexpit()`](https://nlmixr2.github.io/rxode2/reference/mexpit.md)
function.

## Author

Matthew L. Fidler

## Examples

``` r
mlogit(0.1, 0.2, 0.3)
#> [1] -1.3862944 -0.6931472 -0.2876821

mlogit(0.1, 0.2, 0.3, returnRoot = TRUE)
#> $root
#> [1] -1.3862944 -0.6931472 -0.2876821
#> 
#> $f.root
#> [1]  0.000000e+00 -2.775558e-17 -5.551115e-17
#> 
#> $iter
#> [1] 6
#> 
#> $estim.precis
#> [1] 2.775558e-17
#> 
```
