# Recast model in terms of sum/prod

Recast model in terms of sum/prod

## Usage

``` r
rxSumProdModel(model, expand = FALSE, sum = TRUE, prod = TRUE)
```

## Arguments

- model:

  rxode2 model

- expand:

  Boolean indicating if the expression is expanded.

- sum:

  Use sum(...)

- prod:

  Use prod(...)

## Value

model string with prod(.) and sum(.) for all these operations.

## Author

Matthew L. Fidler
