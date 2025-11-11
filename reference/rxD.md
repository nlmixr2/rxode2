# Add to rxode2's derivative tables

Add to rxode2's derivative tables

## Usage

``` r
rxD(name, derivatives)
```

## Arguments

- name:

  Function Name

- derivatives:

  A list of functions. Each function takes the same number of arguments
  as the original function. The first function will construct the
  derivative with respect to the first argument; The second function
  will construct the derivitive with respect to the second argument, and
  so on.

## Value

nothing

## Author

Matthew Fidler

## Examples

``` r
## Add an arbitrary list of derivative functions
## In this case the fun(x,y) is assumed to be 0.5*x^2+0.5*y^2

rxD("fun", list(
  function(x, y) {
    return(x)
  },
  function(x, y) {
    return(y)
  }
))
```
