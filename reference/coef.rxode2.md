# Return the rxode2 coefficients

This returns the parameters , state variables

## Usage

``` r
# S3 method for class 'rxode2'
coef(object, ...)
```

## Arguments

- object:

  is an rxode2 object

- ...:

  ignored arguments

## Value

a rxCoef object with the following

- `params` is a list of strings for parameters for the rxode2 object

- `state` is a list of strings for the names of each state in the rxode2
  object.

- `ini` is the model specified default values for the parameters.

- `rxode2` is the referring rxode2 object

## Author

Matthew L.Fidler
