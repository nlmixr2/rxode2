# Get the internal breakdown of an evid

Get the internal breakdown of an evid

## Usage

``` r
.getWh(i)
```

## Arguments

- i:

  evid to breakdown

## Value

named evid integer vector

## Author

Matthew L. Fidler

## Examples

``` r
.getWh(1001)
#>    wh   cmt wh100   whI   wh0 
#> 11001     9     0     0     1 
.getWh(10401)
#>    wh   cmt wh100   whI   wh0 
#> 10401     3     0     1     1 
```
