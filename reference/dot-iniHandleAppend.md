# Reorder rows in iniDf

Reorder rows in iniDf

## Usage

``` r
.iniHandleAppend(expr, rxui, envir, append)
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
