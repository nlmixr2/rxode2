# logit and inverse logit (expit) functions

logit and inverse logit (expit) functions

## Usage

``` r
logit(x, low = 0, high = 1)

expit(alpha, low = 0, high = 1)

logitNormInfo(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-06, ...)

probitNormInfo(mean = 0, sd = 1, low = 0, high = 1, abs.tol = 1e-06, ...)
```

## Arguments

- x:

  Input value(s) in range \[low,high\] to translate -Inf to Inf

- low:

  Lowest value in the range

- high:

  Highest value in the range

- alpha:

  Infinite value(s) to translate to range of \[low, high\]

- mean:

  logit-scale mean

- sd:

  logit-scale standard deviation

- abs.tol:

  absolute accuracy requested.

- ...:

  other parameters passed to
  [`integrate()`](https://rdrr.io/r/stats/integrate.html)

## Value

values from logit and expit

## Details

logit is given by:

logit(p) = -log(1/p-1)

where:

p = x-low/high-low

expit is given by:

expit(p, low, high) = (high-low)/(1+exp(-alpha)) + low

The `logitNormInfo()` gives the mean, variance and coefficient of
variability on the untransformed scale.

## Examples

``` r
logit(0.25)
#> [1] -1.098612

expit(-1.09)
#> [1] 0.2516183

logitNormInfo(logit(0.25), sd = 0.1)
#>         mean          var           cv 
#> 0.2504672899 0.0003515538 0.0748591440 

logitNormInfo(logit(1, 0, 10), sd = 1, low = 0, high = 10)
#>      mean       var        cv 
#> 1.3386966 1.2662276 0.8405697 
```
