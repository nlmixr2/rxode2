# log likelihood of Cauchy distribution and it's derivatives (from stan)

log likelihood of Cauchy distribution and it's derivatives (from stan)

## Usage

``` r
llikCauchy(x, location = 0, scale = 1, full = FALSE)
```

## Arguments

- x:

  Observation

- location, scale:

  location and scale parameters.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dLocation` and
`dScale` that has the derivatives with respect to the parameters at the
observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikCauchy()` but you have to use all arguments. You
can also get the derivative of `location` and `scale` with
`llikCauchyDlocation()` and `llikCauchyDscale()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
x <- seq(-3, 3, length.out = 21)

llikCauchy(x, 0, 1)
#>           fx  dLocation     dScale
#> 1  -3.447315 -0.6000000  0.8000000
#> 2  -3.259780 -0.6513872  0.7587455
#> 3  -3.055753 -0.7100592  0.7041420
#> 4  -2.832979 -0.7763401  0.6303142
#> 5  -2.589293 -0.8490566  0.5283019
#> 6  -2.323385 -0.9230769  0.3846154
#> 7  -2.036728 -0.9836066  0.1803279
#> 8  -1.738057 -0.9944751 -0.1049724
#> 9  -1.452215 -0.8823529 -0.4705882
#> 10 -1.230908 -0.5504587 -0.8348624
#> 11 -1.144730  0.0000000 -1.0000000
#> 12 -1.230908  0.5504587 -0.8348624
#> 13 -1.452215  0.8823529 -0.4705882
#> 14 -1.738057  0.9944751 -0.1049724
#> 15 -2.036728  0.9836066  0.1803279
#> 16 -2.323385  0.9230769  0.3846154
#> 17 -2.589293  0.8490566  0.5283019
#> 18 -2.832979  0.7763401  0.6303142
#> 19 -3.055753  0.7100592  0.7041420
#> 20 -3.259780  0.6513872  0.7587455
#> 21 -3.447315  0.6000000  0.8000000

llikCauchy(x, 3, 1, full=TRUE)
#>       x location scale        fx  dLocation     dScale
#> 1  -3.0        3     1 -4.755648 -0.3243243  0.9459459
#> 2  -2.7        3     1 -4.655977 -0.3404001  0.9402807
#> 3  -2.4        3     1 -4.551246 -0.3580902  0.9336870
#> 4  -2.1        3     1 -4.440937 -0.3776379  0.9259534
#> 5  -1.8        3     1 -4.324449 -0.3993344  0.9168053
#> 6  -1.5        3     1 -4.201087 -0.4235294  0.9058824
#> 7  -1.2        3     1 -4.070040 -0.4506438  0.8927039
#> 8  -0.9        3     1 -3.930358 -0.4811845  0.8766194
#> 9  -0.6        3     1 -3.780926 -0.5157593  0.8567335
#> 10 -0.3        3     1 -3.620428 -0.5550883  0.8317914
#> 11  0.0        3     1 -3.447315 -0.6000000  0.8000000
#> 12  0.3        3     1 -3.259780 -0.6513872  0.7587455
#> 13  0.6        3     1 -3.055753 -0.7100592  0.7041420
#> 14  0.9        3     1 -2.832979 -0.7763401  0.6303142
#> 15  1.2        3     1 -2.589293 -0.8490566  0.5283019
#> 16  1.5        3     1 -2.323385 -0.9230769  0.3846154
#> 17  1.8        3     1 -2.036728 -0.9836066  0.1803279
#> 18  2.1        3     1 -1.738057 -0.9944751 -0.1049724
#> 19  2.4        3     1 -1.452215 -0.8823529 -0.4705882
#> 20  2.7        3     1 -1.230908 -0.5504587 -0.8348624
#> 21  3.0        3     1 -1.144730  0.0000000 -1.0000000

et <- et(-3, 3, length.out=10)
et$location <- 0
et$scale <- 1

model <- function() {
  model({
    fx <- llikCauchy(time, location, scale)
    dLocation <- llikCauchyDlocation(time, location, scale)
    dScale <- llikCauchyDscale(time, location, scale)
  })
}

rxSolve(model, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 6
#>     time    fx dLocation dScale location scale
#>    <dbl> <dbl>     <dbl>  <dbl>    <dbl> <dbl>
#> 1 -3     -3.45    -0.6    0.8          0     1
#> 2 -2.33  -3.01    -0.724  0.690        0     1
#> 3 -1.67  -2.47    -0.882  0.471        0     1
#> 4 -1     -1.84    -1      0            0     1
#> 5 -0.333 -1.25    -0.6   -0.8          0     1
#> 6  0.333 -1.25     0.600 -0.8          0     1
#> # ℹ 4 more rows
# }
```
