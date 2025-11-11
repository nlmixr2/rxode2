# Calculate expected confidence bands with binomial sampling distribution

This is meant to perform in the same way as
[`quantile()`](https://rdrr.io/r/stats/quantile.html) so it can be a
drop in replacement for code using
[`quantile()`](https://rdrr.io/r/stats/quantile.html) but using
distributional assumptions.

## Usage

``` r
binomProbs(x, ...)

# Default S3 method
binomProbs(
  x,
  probs = c(0.025, 0.05, 0.5, 0.95, 0.975),
  na.rm = FALSE,
  names = TRUE,
  onlyProbs = TRUE,
  n = 0L,
  m = 0L,
  pred = FALSE,
  piMethod = c("lim"),
  M = 5e+05,
  tol = .Machine$double.eps^0.25,
  ciMethod = c("wilson", "wilsonCorrect", "agrestiCoull", "wald", "wc", "ac"),
  ...
)
```

## Arguments

- x:

  numeric vector whose mean and probability based confidence values are
  wanted, NA and NaN values are not allowed in numeric vectors unless
  `na.rm` is `TRUE`.

- ...:

  Arguments passed to default method, allows many different methods to
  be applied.

- probs:

  numeric vector of probabilities with values in the interval 0 to 1,
  inclusive. When 0, it represents the maximum observed, when 1, it
  represents the maximum observed. When 0.5 it represents the expected
  probability (mean).

- na.rm:

  logical; if true, any NA and NaN's are removed from `x` before the
  quantiles are computed.

- names:

  logical; if true, the result has a names attribute.

- onlyProbs:

  logical; if true, only return the probability based confidence
  interval/prediction interval estimates, otherwise return extra
  statistics.

- n:

  integer/integerish; this is the n used to calculate the prediction or
  confidence interval. When `n=0` (default) use the number of non-`NA`
  observations. When calculating the prediction interval, this
  represents the number of observations used in the input ("true")
  distribution.

- m:

  integer. When using the prediction interval this represents the number
  of samples that will be observed in the future for the prediction
  interval.

- pred:

  Use a prediction interval instead of a confidence interval. By default
  this is `FALSE`.

- piMethod:

  gives the prediction interval method (currently only lim) from Lu 2020

- M:

  number of simulations to run for the LIM PI.

- tol:

  tolerance of root finding in the LIM prediction interval

- ciMethod:

  gives the method for calculating the confidence interval.

  Can be:

  - "argestiCoull" or "ac" – Agresti-Coull method. For a 95\\ interval,
    this method does not use the concept of "adding 2 successes and 2
    failures," but rather uses the formulas explicitly described in the
    following link:

  https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Agresti-Coull_Interval.

  - "wilson" – Wilson Method

  - "wilsonCorrect" or "wc" – Wilson method with continuity correction

  - "wald" – Wald confidence interval or standard z approximation.

## Value

By default the return has the probabilities as names (if named) with the
points where the expected distribution are located given the sampling
mean and standard deviation. If `onlyProbs=FALSE` then it would prepend
mean, variance, standard deviation, minimum, maximum and number of
non-NA observations.

## Details

It is used for confidence intervals with rxode2 solved objects using
`confint(mean="binom")`

## References

- Newcombe, R. G. (1998). "Two-sided confidence intervals for the single
  proportion: comparison of seven methods". Statistics in Medicine. 17
  (8): 857–872.
  doi:10.1002/(SICI)1097-0258(19980430)17:8\<857::AID-SIM777\>3.0.CO;2-E.
  PMID 9595616.

- Hezhi Lu, Hua Jin, A new prediction interval for binomial random
  variable based on inferential models, Journal of Statistical Planning
  and Inference, Volume 205, 2020, Pages 156-174, ISSN 0378-3758,
  https://doi.org/10.1016/j.jspi.2019.07.001.

## Author

Matthew L. Fidler

## Examples

``` r
x<- rbinom(7001, p=0.375, size=1)
binomProbs(x)
#>      2.5%        5%       50%       95%     97.5% 
#> 0.3721935 0.3740047 0.3835166 0.3931186 0.3949675 

# you can also use the prediction interval
# \donttest{
binomProbs(x, pred=TRUE)
#>      2.5%        5%       50%       95%     97.5% 
#> 0.3675189 0.3700900 0.3835166 0.3970861 0.3996572 
# }

# Can get some extra statistics if you request onlyProbs=FALSE
binomProbs(x, onlyProbs=FALSE)
#>         mean          var           sd            n         2.5%           5% 
#>    0.3835166    0.2364316    0.4862424 7001.0000000    0.3721935    0.3740047 
#>          50%          95%        97.5% 
#>    0.3835166    0.3931186    0.3949675 

x[2] <- NA_real_

binomProbs(x, onlyProbs=FALSE)
#>  mean   var    sd     n  2.5%    5%   50%   95% 97.5% 
#>    NA    NA    NA    NA    NA    NA    NA    NA    NA 

binomProbs(x, na.rm=TRUE)
#>      2.5%        5%       50%       95%     97.5% 
#> 0.3722472 0.3740585 0.3835714 0.3931743 0.3950234 
```
