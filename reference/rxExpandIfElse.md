# Expand if/else clauses into multiple different types of lines.

Expand if/else clauses into multiple different types of lines.

## Usage

``` r
rxExpandIfElse(model, removeInis = TRUE, removePrint = TRUE)
```

## Arguments

- model:

  Model can be a character, or a rxode2 model. It needs to have
  normalized syntax, that is `if (...){}` has to be on the same line.
  The `else` statement must be on its own line with the closing bracket
  of the `if` statement on the previous line. This `else` statment must
  also contain the opening bracket, like the code `else {}`

- removeInis:

  A boolean indicating if parameter initializations should be removed
  from the model.

- removePrint:

  A boolean indicating if printing statements should be removed from the
  model.

## Value

A named character vector. The names of the vector are the logical
conditions, the values are the lines that satisfy the logical
conditions.

## Author

Matthew L. Fidler
