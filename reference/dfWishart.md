# This uses simulations to match the rse

This uses simulations to match the rse

## Usage

``` r
dfWishart(omega, n, rse, upper, totN = 1000, diag = TRUE, seed = 1234)
```

## Arguments

- omega:

  represents the matrix for simulation

- n:

  This represents the number of subjects/samples this comes from (used
  to calculate rse). When present it assumes the rse= sqrt(2)/sqrt(n)

- rse:

  This is the rse that we try to match, if not specified, it is derived
  from `n`

- upper:

  The upper boundary for root finding in terms of degrees of freedom. If
  not specified, it is n\*200

- totN:

  This represents the total number of simulated inverse wishart deviates

- diag:

  When `TRUE`, represents the rse to match is the diagonals, otherwise
  it is the total matrix.

- seed:

  to make the simulation reproducible, this represents the seed that is
  used for simulating the inverse Wishart distribution

## Value

output from [`uniroot()`](https://rdrr.io/r/stats/uniroot.html) to find
the right estimate

## Author

Matthew L. Fidler

## Examples

``` r
dfWishart(lotri::lotri(a+b~c(1, 0.5, 1)), 100)
#> $root
#> [1] 164.9818
#> 
#> $f.root
#> [1] 0.0005180792
#> 
#> $iter
#> [1] 28
#> 
#> $init.it
#> [1] NA
#> 
#> $estim.prec
#> [1] 7.882673e-05
#> 
```
