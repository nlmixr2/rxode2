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
#>  [1]  0.1418592  3.0551066 17.5266217 18.9209061 23.5764622 32.0313849
#>  [7] 34.5432774 39.6298088 58.8308591 58.9267474

## Sample inhomogenous Poisson rate of 1/10

rxPp(10, 1 / 10, gamma = 2, tmax = 100)
#>  [1]  55.09252  90.40874  96.89581  99.67418 100.00000 100.00000 100.00000
#>  [8] 100.00000 100.00000 100.00000

## Typically the Poisson process times are in a sequential order,
## using randomOrder gives the Poisson process in random order

rxPp(10, 1 / 10, gamma = 2, tmax = 10, randomOrder = TRUE)
#>  [1] 10 10 10 10 10 10 10 10 10 10

## This uses an arbitrary function to sample a non-homogenous Poisson process

rxPp(10, 1 / 10, prob = function(x) {
  1/(1+abs(x))
})
#>  [1]  11.89423 114.97292 279.79528 331.31831 370.20997 480.07313 604.24600
#>  [8] 633.01980 797.29489 799.28924
```
