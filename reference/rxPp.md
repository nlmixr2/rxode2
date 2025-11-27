# Simulate a from a Poisson process

Simulate a from a Poisson process

## Usage

``` r
rxPp(
  n,
  lambda,
  gamma = 1,
  prob = NULL,
  t0 = 0,
  tmax = Inf,
  randomOrder = FALSE
)
```

## Arguments

- n:

  Number of time points to simulate in the Poisson process

- lambda:

  Rate of Poisson process

- gamma:

  Asymmetry rate of Poisson process. When gamma=1.0, this simulates a
  homogenous Poisson process. When gamma\<1.0, the Poisson process has
  more events early, when gamma \> 1.0, the Poisson process has more
  events late in the process.

  When gamma is non-zero, the tmax should not be infinite but indicate
  the end of the Poisson process to be simulated. In most pharamcometric
  cases, this will be the end of the study. Internally this uses a rate
  of:

  l(t) = lambda*gamma*(t/tmax)^(gamma-1)

- prob:

  When specified, this is a probability function with one argument,
  time, that gives the probability that a Poisson time t is accepted as
  a rejection time.

- t0:

  the starting time of the Poisson process

- tmax:

  the maximum time of the Poisson process

- randomOrder:

  when `TRUE` randomize the order of the Poisson events. By default
  (`FALSE`) it returns the Poisson process is in order of how the events
  occurred.

## Value

This returns a vector of the Poisson process times; If the dropout is
\>= tmax, then all the rest of the times are = tmax to indicate the
dropout is equal to or after tmax.

## Author

Matthew Fidler

## Examples

``` r
## Sample homogenous Poisson process of rate 1/10
rxPp(10, 1 / 10)
#>  [1]  27.03681  28.65754  40.59398  45.28264  51.68730  64.59838  70.59086
#>  [8]  83.88122 108.04053 113.30850

## Sample inhomogenous Poisson rate of 1/10

rxPp(10, 1 / 10, gamma = 2, tmax = 100)
#>  [1]  14.83163  40.54204  43.57573  53.66111  59.52112  68.81553  69.99779
#>  [8]  74.17765  94.77493 100.00000

## Typically the Poisson process times are in a sequential order,
## using randomOrder gives the Poisson process in random order

rxPp(10, 1 / 10, gamma = 2, tmax = 10, randomOrder = TRUE)
#>  [1] 10.000000 10.000000 10.000000  5.323784 10.000000 10.000000 10.000000
#>  [8] 10.000000 10.000000 10.000000

## This uses an arbitrary function to sample a non-homogenous Poisson process

rxPp(10, 1 / 10, prob = function(x) {
  1/(1+abs(x))
})
#>  [1]   20.06787  107.15283  171.50404  328.22688  591.52699  661.97794
#>  [7]  916.97768 1007.12114 1230.03163 1444.61285
```
