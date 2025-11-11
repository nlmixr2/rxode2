# This sets the inductive linearization strategy for matrix building

When there is more than one state in a ODE that cannot be separated this
specifies how it is incorporated into the matrix exponential.

## Usage

``` r
rxIndLinStrategy(strategy = c("curState", "split"))
```

## Arguments

- strategy:

  The strategy for inductive linearization matrix building

  - `curState` Prefer parameterizing in terms of the current state,
    followed by the first state observed in the term.

  - `split` Split the parameterization between all states in the term by
    dividing each by the number of states in the term and then adding a
    matrix term for each state.

## Value

Nothing

## Author

Matthew L. Fidler
