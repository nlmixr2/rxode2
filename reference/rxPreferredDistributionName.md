# Change distribution name to the preferred distribution name term

This is determined by the internal preferred condition name list
`.errIdenticalDists`

## Usage

``` r
rxPreferredDistributionName(dist)
```

## Arguments

- dist:

  This is the input distribution

## Value

Preferred distribution term

## Author

Matthew Fidler

## Examples

``` r
rxPreferredDistributionName("dt")
#> [1] "t"

rxPreferredDistributionName("add")
#> [1] "add"

# can be vectorized

rxPreferredDistributionName(c("add","dt"))
#>   add    dt 
#> "add"   "t" 
```
