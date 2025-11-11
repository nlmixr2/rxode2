# Calculate the lambdas and coefficients of the two compartment model

Calculate the lambdas and coefficients of the two compartment model

## Usage

``` r
.solComp2(k10, k12, k21)
```

## Arguments

- k10:

  elimination rate

- k12:

  rate from central to peripheral compartment

- k21:

  rate from peripheral to central compartment

## Value

List with `L` vector and matrices `C1` and `C2`

## Author

Matthew L. Fidler based on `wnl` package/paper, implemented in C/C++

## Examples

``` r
.solComp2(k10=0.1, k12=3, k21=1)
#> $L
#> [1] 4.07546291 0.02453709
#> 
#> $C1
#>            [,1]      [,2]
#> [1,]  0.7592000 0.2408000
#> [2,] -0.7405714 0.7405714
#> 
#> $C2
#>            [,1]      [,2]
#> [1,] -0.2468571 0.2468571
#> [2,]  0.2408000 0.7592000
#> 
```
