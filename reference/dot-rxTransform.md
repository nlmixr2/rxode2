# rxode2 general transformation function

rxode2 general transformation function

## Usage

``` r
.rxTransform(
  x,
  lambda = 1,
  low = 0,
  high = 1,
  transform = c("boxCox", "yeoJohnson", "untransformed", "lnorm", "logit",
    "logit + yeoJohnson", "probit", "probit + yeoJohnson", "logit + boxCox",
    "probit + boxCox"),
  inverse = FALSE
)
```

## Arguments

- x:

  value that will be transformed

- lambda:

  lambda value for the transformation

- low:

  lower bound for the transformation

- high:

  upper bound for the transformation

- transform:

  transformation to use (can be integer or string matching supported
  transformations)

- inverse:

  boolean if the inverse transformation should be performed

## Value

transformed value

## Author

Matthew L. Fidler

## Examples

``` r
logit(0.25)
#> [1] -1.098612

.rxTransform(0.25, transform="logit")
#> [1] -1.098612

expit(-1.09)
#> [1] 0.2516183

.rxTransform(-1.09, transform="logit", inverse=TRUE)
#> [1] 0.2516183
```
