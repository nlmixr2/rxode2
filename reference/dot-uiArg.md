# Handle arguments for ui functions

Note this is an internal function but it is exported in case it is
useful.

## Usage

``` r
.uiArg(char, f, dp)
```

## Arguments

- char:

  This is the character equivalent of the argument

- f:

  This is the forced equivalent of the argument

- dp:

  This is deparsed expression

## Value

character representing the underlying rxode2 code for the argument

## Author

Matthew L. Fidler

## Examples

``` r
.uiArg("1.0", 1.0, "1.0")
#> [1] "1"
```
