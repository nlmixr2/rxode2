# Validate rxode2 This allows easy validation/qualification of nlmixr by running the testing suite on your system.

Validate rxode2 This allows easy validation/qualification of nlmixr by
running the testing suite on your system.

## Usage

``` r
rxValidate(type = NULL, skipOnCran = TRUE)

rxTest(type = NULL, skipOnCran = TRUE)
```

## Arguments

- type:

  Type of test or filter of test type, When this is an expression,
  evaluate the contents, respecting `skipOnCran`

- skipOnCran:

  when `TRUE` skip the test on CRAN.

## Value

nothing

## Author

Matthew L. Fidler
