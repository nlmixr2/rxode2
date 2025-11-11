# gammaqInv and gammaqInva: Inverses of normalized gammaq function

gammaqInv and gammaqInva: Inverses of normalized gammaq function

## Usage

``` r
gammaqInv(a, q)

gammaqInva(x, q)
```

## Arguments

- a:

  The numeric 'a' parameter in the upper incomplete gamma

- q:

  The numeric 'q' parameter in the upper incomplete gamma

- x:

  The numeric 'x' parameter in the upper incomplete gamma

## Value

inverse gammaq results

## Details

With the equation:

q = gammaq(a, x)

The 'gammaqInv' function returns a value 'x' that satisfies the equation
above

The 'gammaqInva' function returns a value 'a' that satisfies the
equation above

NOTE: gammaqInva is slow

## Author

Matthew L. Fidler

## Examples

``` r
gammaqInv(1:3, 0.5)
#> [1] 0.6931472 1.6783470 2.6740603

gammaqInv(1, 1:3 / 3)
#> [1] 1.0986123 0.4054651 0.0000000

gammaqInv(1:3, 1:3 / 3.1)
#> [1] 1.131402 1.248533 0.684289

gammaqInva(1:3, 1:3 / 3.1)
#> [1] 1.131402 1.248533 0.684289
```
