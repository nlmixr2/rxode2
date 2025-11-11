# gammapInv and gammapInva: Inverses of normalized gammap function

gammapInv and gammapInva: Inverses of normalized gammap function

## Usage

``` r
gammapInv(a, p)

gammapInva(x, p)
```

## Arguments

- a:

  The numeric 'a' parameter in the upper incomplete gamma

- p:

  The numeric 'p' parameter in the upper incomplete gamma

- x:

  The numeric 'x' parameter in the upper incomplete gamma

## Value

inverse gammap results

## Details

With the equation:

p = gammap(a, x)

The 'gammapInv' function returns a value 'x' that satisfies the equation
above

The 'gammapInva' function returns a value 'q' that satisfies the
equation above

NOTE: gammapInva is slow

## Author

Matthew L. Fidler

## Examples

``` r
gammapInv(1:3, 0.5)
#> [1] 0.6931472 1.6783470 2.6740603

gammapInv(1, 1:3 / 3.1)
#> [1] 0.3894648 1.0360919 3.4339872

gammapInv(1:3, 1:3 / 3.1)
#> [1] 0.3894648 2.1988984 6.8872929

gammapInva(1:3, 1:3 / 3.1)
#> [1] 0.3894648 2.1988984 6.8872929
```
