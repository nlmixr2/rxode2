# Calculate the lambdas and coefficients of the three compartment model

Calculate the lambdas and coefficients of the three compartment model

## Usage

``` r
.solComp3(k10, k12, k21, k13, k31)
```

## Arguments

- k10:

  elimination rate

- k12:

  rate from central to peripheral compartment

- k21:

  rate from peripheral to central compartment

- k13:

  rate from central to peripheral compartment \#2

- k31:

  rate from peripheral compartment \#2 to central

## Value

List with `L` vector and matrices `C1`, `C2` and `C3`

## Author

Matthew L. Fidler

## Examples

``` r
.solComp3(k10=0.1, k12=3, k21=1, k13=2, k31=0.5)
#> $L
#> [1] 5.89778324 0.01228788 0.68992888
#> 
#> $C1
#>            [,1]      [,2]       [,3]
#> [1,]  0.8625279 0.1207845  0.0166876
#> [2,] -0.5283173 0.3668615  0.1614558
#> [3,] -0.3195860 0.4953107 -0.1757247
#> 
#> $C2
#>             [,1]      [,2]        [,3]
#> [1,] -0.17610577 0.1222872  0.05381861
#> [2,]  0.10786866 0.3714255  0.52070584
#> [3,]  0.06525115 0.5014727 -0.56672385
#> 
#> $C3
#>             [,1]      [,2]        [,3]
#> [1,] -0.07989649 0.1238277 -0.04393117
#> [2,]  0.04893836 0.3761045 -0.42504289
#> [3,]  0.02960345 0.5077900  0.46260657
#> 
```
