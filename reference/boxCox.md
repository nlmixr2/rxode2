# boxCox/yeoJohnson and inverse boxCox/yeoJohnson functions

boxCox/yeoJohnson and inverse boxCox/yeoJohnson functions

## Usage

``` r
boxCox(x, lambda = 1)

boxCoxInv(x, lambda = 1)

yeoJohnson(x, lambda = 1)

yeoJohnsonInv(x, lambda = 1)
```

## Arguments

- x:

  input value(s) to transform

- lambda:

  lambda value for the transformation

## Value

values from boxCox and boxCoxInv

## Examples

``` r
boxCox(10, 0.5)
#> [1] 4.324555

boxCoxInv(4.32, 0.5)
#> [1] 9.9856

yeoJohnson(10, 0.5)
#> [1] 4.63325

yeoJohnsonInv(4.32, 0.5)
#> [1] 8.9856
```
