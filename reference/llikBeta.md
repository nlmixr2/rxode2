# Calculate the log likelihood of the binomial function (and its derivatives)

Calculate the log likelihood of the binomial function (and its
derivatives)

## Usage

``` r
llikBeta(x, shape1, shape2, full = FALSE)
```

## Arguments

- x:

  Observation

- shape1, shape2:

  non-negative parameters of the Beta distribution.

- full:

  Add the data frame showing x, mean, sd as well as the fx and
  derivatives

## Value

data frame with `fx` for the log pdf value of with `dShape1` and
`dShape2` that has the derivatives with respect to the parameters at the
observation time-point

## Details

In an [`rxode2()`](https://nlmixr2.github.io/rxode2/reference/rxode2.md)
model, you can use `llikBeta()` but you have to use all arguments. You
can also get the derivative of `shape1` and `shape2` with
`llikBetaDshape1()` and `llikBetaDshape2()`.

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

x <- seq(1e-4, 1 - 1e-4, length.out = 21)

llikBeta(x, 0.5, 0.5)
#>             fx     dShape1     dShape2
#> 1   3.46049030 -7.82404601  1.38619436
#> 2   0.37793108 -1.60763953  1.33490633
#> 3   0.05888752 -0.91549105  1.28084495
#> 4  -0.11510253 -0.51035907  1.22369308
#> 5  -0.22855163 -0.22284360  1.16307581
#> 6  -0.30780832  0.00019998  1.09854562
#> 7  -0.36444410  0.18245488  1.02956227
#> 8  -0.40444714  0.33655795  0.95546529
#> 9  -0.43118004  0.47005363  0.87543540
#> 10 -0.44655956  0.58780889  0.78843918
#> 11 -0.45158271  0.69314718  0.69314718
#> 12 -0.44655956  0.78843918  0.58780889
#> 13 -0.43118004  0.87543540  0.47005363
#> 14 -0.40444714  0.95546529  0.33655795
#> 15 -0.36444410  1.02956227  0.18245488
#> 16 -0.30780832  1.09854562  0.00019998
#> 17 -0.22855163  1.16307581 -0.22284360
#> 18 -0.11510253  1.22369308 -0.51035907
#> 19  0.05888752  1.28084495 -0.91549105
#> 20  0.37793108  1.33490633 -1.60763953
#> 21  3.46049030  1.38619436 -7.82404601

llikBeta(x, 1, 3, TRUE)
#>          x shape1 shape2           fx     dShape1     dShape2
#> 1  0.00010      1      3   1.09841228 -7.37700704  0.33323333
#> 2  0.05009      1      3   0.99583622 -1.16060056  0.28194530
#> 3  0.10008      1      3   0.88771347 -0.46845208  0.22788392
#> 4  0.15007      1      3   0.77340972 -0.06332009  0.17073205
#> 5  0.20006      1      3   0.65217518  0.22419538  0.11011478
#> 6  0.25005      1      3   0.52311481  0.44723895  0.04558459
#> 7  0.30004      1      3   0.38514811  0.62949385 -0.02339876
#> 8  0.35003      1      3   0.23695415  0.78359692 -0.09749574
#> 9  0.40002      1      3   0.07689437  0.91709260 -0.17752562
#> 10 0.45001      1      3  -0.09709808  1.03484786 -0.26452185
#> 11 0.50000      1      3  -0.28768207  1.14018615 -0.35981385
#> 12 0.54999      1      3  -0.49835866  1.23547815 -0.46515214
#> 13 0.59998      1      3  -0.73386918  1.32247438 -0.58290740
#> 14 0.64997      1      3  -1.00086054  1.40250426 -0.71640308
#> 15 0.69996      1      3  -1.30906667  1.47660124 -0.87050615
#> 16 0.74995      1      3  -1.67357647  1.54558459 -1.05276105
#> 17 0.79994      1      3  -2.11966363  1.61011478 -1.27580462
#> 18 0.84993      1      3  -2.69469457  1.67073205 -1.56332009
#> 19 0.89992      1      3  -3.50495854  1.72788392 -1.96845208
#> 20 0.94991      1      3  -4.88925549  1.78194530 -2.66060056
#> 21 0.99990      1      3 -17.32206846  1.83323333 -8.87700704

et <- et(seq(1e-4, 1-1e-4, length.out=21))
et$shape1 <- 0.5
et$shape2 <- 1.5

model <- function() {
  model({
    fx <- llikBeta(time, shape1, shape2)
    dShape1 <- llikBetaDshape1(time, shape1, shape2)
    dShape2 <- llikBetaDshape2(time, shape1, shape2)
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
#> # A tibble: 21 × 6
#>     time     fx dShape1 dShape2 shape1 shape2
#>    <dbl>  <dbl>   <dbl>   <dbl>  <dbl>  <dbl>
#> 1 0.0001 4.15   -6.82    0.386     0.5    1.5
#> 2 0.0501 1.02   -0.608   0.335     0.5    1.5
#> 3 0.100  0.647   0.0845  0.281     0.5    1.5
#> 4 0.150  0.415   0.490   0.224     0.5    1.5
#> 5 0.200  0.241   0.777   0.163     0.5    1.5
#> 6 0.250  0.0976  1.00    0.0985    0.5    1.5
#> # ℹ 15 more rows
# }
```
