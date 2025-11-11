# probit and inverse probit functions

probit and inverse probit functions

## Usage

``` r
probit(x, low = 0, high = 1)

probitInv(x, low = 0, high = 1)
```

## Arguments

- x:

  Input value(s) in range \[low,high\] to translate -Inf to Inf

- low:

  Lowest value in the range

- high:

  Highest value in the range

## Value

values from probit, probitInv and probitNormInfo

## Examples

``` r
probit(0.25)
#> [1] -0.6744898

probitInv(-0.674)
#> [1] 0.2501557

probitNormInfo(probit(0.25), sd = 0.1)
#>       mean        var         cv 
#> 0.25106491 0.00100658 0.12636827 

probitNormInfo(probit(1, 0, 10), sd = 1, low = 0, high = 10)
#>     mean      var       cv 
#> 1.824166 4.307824 1.137796 
```
