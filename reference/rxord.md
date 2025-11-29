# Simulate ordinal value

Simulate ordinal value

## Usage

``` r
rxord(...)
```

## Arguments

- ...:

  the probabilities to be simulated. These should sum up to a number
  below one.

## Value

A number from 1 to the (number of probabilities specified + 1)

## Details

The values entered into the 'rxord' simulation will simulate the
probability of falling each group. If it falls outside of the specified
probabilities, it will simulate the group (number of probabilities
specified + 1)

## Author

Matthew L. Fidler

## Examples

``` r
# This will give values 1, and 2
rxord(0.5)
#> [1] 2
rxord(0.5)
#> [1] 2
rxord(0.5)
#> [1] 1
rxord(0.5)
#> [1] 2

# This will give values 1, 2 and 3
rxord(0.3, 0.3)
#> [1] 2
rxord(0.3, 0.3)
#> [1] 1
rxord(0.3, 0.3)
#> [1] 1
```
