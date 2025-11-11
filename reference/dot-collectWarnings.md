# Collect warnings and just warn once.

Collect warnings and just warn once.

## Usage

``` r
.collectWarnings(expr, lst = FALSE)
```

## Arguments

- expr:

  R expression

- lst:

  When `TRUE` return a list with list(object,warnings) instead of
  issuing the warnings. Otherwise, when `FALSE` issue the warnings and
  return the object.

## Value

The value of the expression or a list with the value of the expression
and a list of warning messages

## Author

Matthew L. Fidler
