# Calculate expected confidence bands or prediction intreval with normal or t sampling distribution

The generic function `meanProbs` produces expected confidence bands
under either the t distribution or the normal sampling distribution.
This uses [`qnorm()`](https://rdrr.io/r/stats/Normal.html) or
[`qt()`](https://rdrr.io/r/stats/TDist.html) with the mean and standard
deviation.

## Usage

``` r
meanProbs(x, ...)

# Default S3 method
meanProbs(
  x,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  names = TRUE,
  useT = TRUE,
  onlyProbs = TRUE,
  pred = FALSE,
  n = 0L,
  ...
)
```

## Arguments

- x:

  numeric vector whose mean and probability based confidence values are
  wanted, NA and NaN values are not allowed in numeric vectors unless
  ‘na.rm’ is ‘TRUE’.

- ...:

  Arguments passed to default method, allows many different methods to
  be applied.

- probs:

  numeric vector of probabilities with values in the interval from 0 to
  1 .

- na.rm:

  logical; if true, any NA and NaN's are removed from `x` before the
  quantiles are computed.

- names:

  logical; if true, the result has a names attribute.

- useT:

  logical; if true, use the t-distribution to calculate the
  confidence-based estimates. If false use the normal distribution to
  calculate the confidence based estimates.

- onlyProbs:

  logical; if true, only return the probability based confidence
  interval estimates, otherwise return

- pred:

  logical; if true use the prediction interval instead of the confidence
  interval

- n:

  integer/integerish; this is the n used to calculate the prediction or
  confidence interval. When `n=0` (default) use the number of non-`NA`
  observations.

## Value

By default the return has the probabilities as names (if named) with the
points where the expected distribution are located given the sampling
mean and standard deviation. If `onlyProbs=FALSE` then it would prepend
mean, variance, standard deviation, minimum, maximum and number of
non-NA observations.

## Details

For a single probability, p, it uses either:

mean + qt(p, df=n)\*sd/sqrt(n)

or

mean + qnorm(p)\*sd/sqrt(n)

The smallest observation corresponds to a probability of 0 and the
largest to a probability of 1 and the mean corresponds to 0.5.

The mean and standard deviation of the sample is calculated based on
Welford's method for a single pass.

This is meant to perform in the same way as
[`quantile()`](https://rdrr.io/r/stats/quantile.html) so it can be a
drop in replacement for code using
[`quantile()`](https://rdrr.io/r/stats/quantile.html) but using
distributional assumptions.

## Author

Matthew L. Fidler

## Examples

``` r
quantile(x<- rnorm(1001))
#>           0%          25%          50%          75%         100% 
#> -3.431442969 -0.682099739  0.005737979  0.740822144  3.218127184 
meanProbs(x)
#>           0%          25%          50%          75%         100% 
#> -3.431442969 -0.002696175  0.019073807  0.040843790  3.218127184 

# Can get some extra statistics if you request onlyProbs=FALSE
meanProbs(x, onlyProbs=FALSE)
#>          mean           var            sd           min           max 
#>  1.907381e-02  1.042038e+00  1.020803e+00 -3.431443e+00  3.218127e+00 
#>             n            0%           25%           50%           75% 
#>  1.001000e+03 -3.431443e+00 -2.696175e-03  1.907381e-02  4.084379e-02 
#>          100% 
#>  3.218127e+00 

x[2] <- NA_real_

meanProbs(x, onlyProbs=FALSE)
#> mean  var   sd  min  max    n   0%  25%  50%  75% 100% 
#>   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA   NA 

quantile(x<- rnorm(42))
#>          0%         25%         50%         75%        100% 
#> -2.68522697 -0.44967437 -0.05137773  0.92975485  2.47176507 

meanProbs(x)
#>         0%        25%        50%        75%       100% 
#> -2.6852270  0.0681077  0.1996618  0.3312159  2.4717651 

meanProbs(x, useT=FALSE)
#>          0%         25%         50%         75%        100% 
#> -2.68522697  0.06927358  0.19966182  0.33005006  2.47176507 
```
