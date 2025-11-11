# Update the iniDf of a model

Update the iniDf of a model

## Usage

``` r
.iniHandleLine(expr, rxui, envir = parent.frame(), append = NULL)

.iniHandleFixOrUnfix(expr, rxui, envir = parent.frame(), append = NULL)
```

## Arguments

- expr:

  Expression for parsing

- rxui:

  User interface function

- envir:

  Environment for parsing

- append:

  Reorder theta parameters. `NULL` means no change to parameter order. A
  parameter name (as a character string) means to put the new parameter
  after the named parameter. A number less than or equal to zero means
  to put the parameter at the beginning of the list. A number greater
  than the last parameter number means to put the parameter at the end
  of the list.

## Value

Nothing, called for side effects

## Author

Matthew L. Fidler
