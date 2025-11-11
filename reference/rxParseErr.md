# Prepare Error function for inclusion in rxode2

Prepare Error function for inclusion in rxode2

## Usage

``` r
rxParseErr(
  x,
  baseTheta,
  ret = "rx_r_",
  init = NULL,
  addProp = c("combined2", "combined1")
)
```

## Arguments

- x:

  error function

- baseTheta:

  Base theta to start numbering add(.) and prop(.) from.

- ret:

  Internal return type. Should not be changed by the user...

- init:

  Initialization vector

## Value

rxode2 transformed text

## Author

Matthew L. Fidler
