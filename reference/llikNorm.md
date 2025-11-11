# Log likelihood for normal distribution

Log likelihood for normal distribution

## Usage

``` r
llikNorm(x, mean = 0, sd = 1, full = FALSE)
```

## Arguments

- x:

  Observation

- mean:

  Mean for the likelihood

- sd:

  Standard deviation for the likelihood

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the pdf value of with `dMean` and `dSd` that
has the derivatives with respect to the parameters at the observation
time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikNorm()` but you have to use all arguments. You
can also get the derivatives with `llikNormDmean()` and `llikNormDsd()`

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

llikNorm(0)
#>           fx dMean dSd
#> 1 -0.9189385     0  -1

llikNorm(seq(-2,2,length.out=10), full=TRUE)
#>             x mean sd         fx      dMean        dSd
#> 1  -2.0000000    0  1 -2.9189385 -2.0000000  3.0000000
#> 2  -1.5555556    0  1 -2.1288151 -1.5555556  1.4197531
#> 3  -1.1111111    0  1 -1.5362225 -1.1111111  0.2345679
#> 4  -0.6666667    0  1 -1.1411608 -0.6666667 -0.5555556
#> 5  -0.2222222    0  1 -0.9436299 -0.2222222 -0.9506173
#> 6   0.2222222    0  1 -0.9436299  0.2222222 -0.9506173
#> 7   0.6666667    0  1 -1.1411608  0.6666667 -0.5555556
#> 8   1.1111111    0  1 -1.5362225  1.1111111  0.2345679
#> 9   1.5555556    0  1 -2.1288151  1.5555556  1.4197531
#> 10  2.0000000    0  1 -2.9189385  2.0000000  3.0000000

# With rxode2 you can use:
 
et <- et(-3, 3, length.out=10)
et$mu <- 0
et$sigma <- 1

model <- function(){
  model({
    fx <- llikNorm(time, mu, sigma)
    dMean <- llikNormDmean(time, mu, sigma)
    dSd <- llikNormDsd(time, mu, sigma)
  })
 }

ret <- rxSolve(model, et)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’
ret
#> ── Solved rxode2 object ──
#> ── Parameters (value$params): ──
#> # A tibble: 1 × 0
#> ── Initial Conditions (value$inits): ──
#> named numeric(0)
#> ── First part of data (object): ──
#> # A tibble: 10 × 6
#>     time     fx  dMean    dSd    mu sigma
#>    <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl>
#> 1 -3     -5.42  -3      8         0     1
#> 2 -2.33  -3.64  -2.33   4.44      0     1
#> 3 -1.67  -2.31  -1.67   1.78      0     1
#> 4 -1     -1.42  -1      0         0     1
#> 5 -0.333 -0.974 -0.333 -0.889     0     1
#> 6  0.333 -0.974  0.333 -0.889     0     1
#> # ℹ 4 more rows
# }
```
