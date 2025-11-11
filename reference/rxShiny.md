# Use Shiny to help develop an rxode2 model

Use Shiny to help develop an rxode2 model

## Usage

``` r
rxShiny(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  data = data.frame()
)

# S3 method for class 'rxSolve'
rxShiny(
  object,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  data = data.frame()
)

# Default S3 method
rxShiny(
  object = NULL,
  params = NULL,
  events = NULL,
  inits = NULL,
  ...,
  data = data.frame()
)
```

## Arguments

- object:

  A rxode2 family of objects. If not supplied a 2-compartment indirect
  effect model is used. If it is supplied, use the model associated with
  the rxode2 object for the model exploration.

- params:

  Initial parameters for model

- events:

  Event information (currently ignored)

- inits:

  Initial estimates for model

- ...:

  Other arguments passed to rxShiny. Currently doesn't do anything.

- data:

  Any data that you would like to plot. If the data has a `time`
  variable as well as a compartment or calculated variable that matches
  the rxode2 model, the data will be added to the plot of a specific
  compartment or calculated variable.

## Value

Nothing; Starts a shiny server

## Author

Zufar Mulyukov and Matthew L. Fidler
