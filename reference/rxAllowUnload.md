# Allow unloading of dlls

Allow unloading of dlls

## Usage

``` r
rxAllowUnload(allow)
```

## Arguments

- allow:

  boolean indicating if garbage collection will unload of rxode2 dlls.

## Value

Boolean allow; called for side effects

## Author

Matthew Fidler

## Examples

``` r
# Garbage collection will not unload un-used rxode2 dlls
rxAllowUnload(FALSE);
#> [1] FALSE

# Garbage collection will unload unused rxode2 dlls
rxAllowUnload(TRUE);
#> [1] TRUE
```
