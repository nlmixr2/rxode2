# Log likelihood of T and it's derivatives (from stan)

Log likelihood of T and it's derivatives (from stan)

## Usage

``` r
llikT(x, df, mean = 0, sd = 1, full = FALSE)
```

## Arguments

- x:

  Observation

- df:

  degrees of freedom (\\\> 0\\, maybe non-integer). `df = Inf` is
  allowed.

- mean:

  Mean for the likelihood

- sd:

  Standard deviation for the likelihood

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dDf` `dMean` and
`dSd` that has the derivatives with respect to the parameters at the
observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikT()` but you have to use all arguments. You can
also get the derivative of `df`, `mean` and `sd` with `llikTDdf()`,
`llikTDmean()` and `llikTDsd()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{
x <- seq(-3, 3, length.out = 21)

llikT(x, 7, 0, 1)
#>            fx          dDf      dMean        dSd
#> 1  -4.2612484 -0.086858773 -1.5000000  3.5000000
#> 2  -3.8091335 -0.060260433 -1.5115465  3.0811756
#> 3  -3.3561547 -0.037201674 -1.5047022  2.6112853
#> 4  -2.9088542 -0.018379169 -1.4723926  2.0920245
#> 5  -2.4761000 -0.004340221 -1.4062500  1.5312500
#> 6  -2.0693878  0.004691380 -1.2972973  0.9459459
#> 7  -1.7028228  0.009010785 -1.1374408  0.3649289
#> 8  -1.3925134  0.009569214 -0.9218950 -0.1702945
#> 9  -1.1551333  0.007927361 -0.6521739 -0.6086957
#> 10 -1.0056349  0.005918024 -0.3385049 -0.8984485
#> 11 -0.9545342  0.005051942  0.0000000 -1.0000000
#> 12 -1.0056349  0.005918024  0.3385049 -0.8984485
#> 13 -1.1551333  0.007927361  0.6521739 -0.6086957
#> 14 -1.3925134  0.009569214  0.9218950 -0.1702945
#> 15 -1.7028228  0.009010785  1.1374408  0.3649289
#> 16 -2.0693878  0.004691380  1.2972973  0.9459459
#> 17 -2.4761000 -0.004340221  1.4062500  1.5312500
#> 18 -2.9088542 -0.018379169  1.4723926  2.0920245
#> 19 -3.3561547 -0.037201674  1.5047022  2.6112853
#> 20 -3.8091335 -0.060260433  1.5115465  3.0811756
#> 21 -4.2612484 -0.086858773  1.5000000  3.5000000

llikT(x, 15, 0, 1, full=TRUE)
#>            fx           dDf      dMean        dSd
#> 1  -4.6956220 -0.0338931511 -2.0000000  5.0000000
#> 2  -4.1042965 -0.0225073150 -1.9380888  4.2328398
#> 3  -3.5354158 -0.0134033865 -1.8497110  3.4393064
#> 4  -2.9974985 -0.0065857823 -1.7310665  2.6352396
#> 5  -2.5001272 -0.0019378861 -1.5789474  1.8421053
#> 6  -2.0536885  0.0007929097 -1.3913043  1.0869565
#> 7  -1.6689304  0.0019903977 -1.1678832  0.4014599
#> 8  -1.3563325  0.0021369166 -0.9108159 -0.1802657
#> 9  -1.1253251  0.0017504002 -0.6250000 -0.6250000
#> 10 -0.9834495  0.0012985422 -0.3180915 -0.9045726
#> 11 -0.9355929  0.0011086635  0.0000000 -1.0000000
#> 12 -0.9834495  0.0012985422  0.3180915 -0.9045726
#> 13 -1.1253251  0.0017504002  0.6250000 -0.6250000
#> 14 -1.3563325  0.0021369166  0.9108159 -0.1802657
#> 15 -1.6689304  0.0019903977  1.1678832  0.4014599
#> 16 -2.0536885  0.0007929097  1.3913043  1.0869565
#> 17 -2.5001272 -0.0019378861  1.5789474  1.8421053
#> 18 -2.9974985 -0.0065857823  1.7310665  2.6352396
#> 19 -3.5354158 -0.0134033865  1.8497110  3.4393064
#> 20 -4.1042965 -0.0225073150  1.9380888  4.2328398
#> 21 -4.6956220 -0.0338931511  2.0000000  5.0000000

et <- et(-3, 3, length.out=10)
et$nu <- 7
et$mean <- 0
et$sd <- 1

model <- function() {
  model({
    fx <- llikT(time, nu, mean, sd)
    dDf <- llikTDdf(time, nu, mean, sd)
    dMean <- llikTDmean(time, nu, mean, sd)
    dSd   <- llikTDsd(time, nu, mean, sd)
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
#> # A tibble: 10 × 8
#>     time    fx       dDf  dMean    dSd    nu  mean    sd
#>    <dbl> <dbl>     <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1 -3     -4.26 -0.0869   -1.5    3.5       7     0     1
#> 2 -2.33  -3.26 -0.0326   -1.5    2.5       7     0     1
#> 3 -1.67  -2.29  0.000289 -1.36   1.27      7     0     1
#> 4 -1     -1.49  0.00971  -1      0         7     0     1
#> 5 -0.333 -1.02  0.00611  -0.375 -0.875     7     0     1
#> 6  0.333 -1.02  0.00611   0.375 -0.875     7     0     1
#> # ℹ 4 more rows
# }
```
