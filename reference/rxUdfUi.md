# This function is called when processing rxode2 user functions from the models

This function is called when processing rxode2 user functions from the
models

## Usage

``` r
rxUdfUi(fun)

# S3 method for class 'rinbinom'
rxUdfUi(fun)
```

## Arguments

- fun:

  this is the function that needs to be parsed and changed. This is a R
  language expression

## Value

This needs to return a list with the following elements:

- `iniDf` – the modified initial estimate data.frame

- `before` – any model code that needs to be added before the current
  line

- `after` – any model code that needs to be added after the current line

- `replace` – replacement code for this user function

## Author

Matthew L. Fidler
