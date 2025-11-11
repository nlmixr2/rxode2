# Stack a solved object for things like default ggplot2 plot

Stack a solved object for things like default ggplot2 plot

## Usage

``` r
rxStack(data, vars = NULL, doSim = TRUE, doIpredSim = TRUE)
```

## Arguments

- data:

  is a rxode2 object to be stacked.

- vars:

  Variables to include in stacked data; By default this is all the
  variables when vars is NULL.

  When vars is `sim` and comes from a `rxode2` ui simulation with
  multiple endpoints (ie it has a `CMT` in the simulation), it will
  rework the data as if it was stacked based the value based on the
  compartments in the multiple endpoint model.

  When the vars is `sim.endpoint1` it will subset the stack to
  endpoint1, you can also have \`c("sim.endpoint1", "sim.endpoint2") and
  the "stack" will subset to endpoint1 and endpoint2.

  When you specify the `sim` type variables they have to be all prefixed
  with `sim` otherwise, the stack will not treat them differently.

- doSim:

  boolean that determines if the "sim" variable in a `rxSolve` dataset
  is actually "stacking" based on the endpoint (`TRUE`) or simply
  treating `sim` as a variable.

- doIpredSim:

  boolean that determines if the "ipredSim" variable in a `rxSolve`
  dataset is actually "stacking" based on the endpoint (`TRUE`) or
  simply treating `ipredSim` as a variable.

## Value

Stacked data with `value` and `trt`, where value is the values and `trt`
is the state and `lhs` variables.

## Author

Matthew Fidler
