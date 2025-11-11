# Unloads all rxode2 compiled DLLs

Unloads all rxode2 compiled DLLs

## Usage

``` r
rxUnloadAll(set = TRUE)
```

## Arguments

- set:

  If specified and `TRUE`, unloads all rxode2 dlls. If specified and
  `FALSE`, block a simple `rxUnloadAll()` will not actually unload all
  dlls (helps with CRAN ASAN check)

## Value

boolean telling if `rxUnloadAll()` completed the unloading procedure

## Examples

``` r
print(rxUnloadAll())
#> [1] TRUE
```
