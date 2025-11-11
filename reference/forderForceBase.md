# Force using base order for rxode2 radix sorting

Force using base order for rxode2 radix sorting

## Usage

``` r
forderForceBase(forceBase = FALSE)
```

## Arguments

- forceBase:

  boolean indicating if rxode2 should use R's
  [`order()`](https://rdrr.io/r/base/order.html) for radix sorting
  instead of `data.table`'s parallel radix sorting.

## Value

value of `forceBase` (can change if `data.table` is not available)

## Examples

``` r
# \donttest{
forderForceBase(TRUE) # Use base `order` for rxode2 sorts
forderForceBase(FALSE) # Use `data.table` for rxode2 sorts
# }
```
