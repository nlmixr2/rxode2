# Control the packages that are loaded when a `rxode2` model dll is loaded

Control the packages that are loaded when a `rxode2` model dll is loaded

## Usage

``` r
rxode2parseGetPackagesToLoad()

rxode2parseAssignPackagesToLoad(pkgs = rxode2parseGetPackagesToLoad())
```

## Arguments

- pkgs:

  The packages to make sure are loaded every time you load an rxode2
  model.

## Value

List of packages to load

## Author

Matthew Fidler

## Examples

``` r
rxode2parseGetPackagesToLoad()
#> [1] "rxode2ll" "lotri"   

rxode2parseAssignPackagesToLoad(rxode2parseGetPackagesToLoad())
#> [1] "rxode2ll" "lotri"   
```
