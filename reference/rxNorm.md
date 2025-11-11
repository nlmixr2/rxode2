# Get the normalized model

This get the syntax preferred model for processing

## Usage

``` r
rxNorm(obj, condition = NULL, removeInis, removeJac, removeSens)
```

## Arguments

- obj:

  rxode2 family of objects

- condition:

  Character string of a logical condition to use for subsetting the
  normalized model. When missing, and a condition is not set via
  `rxCondition`, return the whole code with all the conditional settings
  intact. When a condition is set with `rxCondition`, use that
  condition.

- removeInis:

  A boolean indicating if parameter initialization will be removed from
  the model

- removeJac:

  A boolean indicating if the Jacobians will be removed.

- removeSens:

  A boolean indicating if the sensitivities will be removed.

## Value

Normalized Normal syntax (no comments)

## Author

Matthew L. Fidler
