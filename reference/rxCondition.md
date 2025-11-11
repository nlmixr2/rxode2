# Current Condition for rxode2 object

Current Condition for rxode2 object

## Usage

``` r
rxCondition(obj, condition = NULL)
```

## Arguments

- obj:

  rxode2 object

- condition:

  If specified and is one of the conditions in the rxode2 object (as
  determined by
  [`rxExpandIfElse()`](https://nlmixr2.github.io/rxode2/reference/rxExpandIfElse.md)),
  assign the rxode2 current condition to this parameter. If the
  condition is not one of the known condition, the condition is set to
  `NULL`, implying no conditioning currently used.

## Value

Current condition for rxode2 object

## Author

Matthew L. Fidler
