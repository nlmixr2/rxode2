# Print out a table in the documentation

Print out a table in the documentation

## Usage

``` r
.rxDocTable(table, caption = "none")
```

## Arguments

- table:

  data frame

- caption:

  a character vector representing the caption for the latex table

## Value

based on the `knitr` context:

- output a
  [`kableExtra::kbl`](https://rdrr.io/pkg/kableExtra/man/kbl.html) for
  `latex` output

- output a [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html)
  for html output

- otherwise output a
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html)

## Author

Matthew L. Fidler

## Examples

``` r
.rxDocTable(rxReservedKeywords)
#> 
#> 
#> |Reserved Name  |Meaning                                                                                                                                                                           |Alias  |
#> |:--------------|:---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|:------|
#> |time           |solver time                                                                                                                                                                       |t      |
#> |podo           |In Transit compartment models, last dose amount                                                                                                                                   |       |
#> |tlast          |Time of Last dose                                                                                                                                                                 |       |
#> |M_E            |Exp(1)                                                                                                                                                                            |       |
#> |M_LOG2E        |log2(e)                                                                                                                                                                           |       |
#> |M_LOG10E       |log10(e)                                                                                                                                                                          |       |
#> |M_LN2          |log(2)                                                                                                                                                                            |       |
#> |M_LN10         |log(10)                                                                                                                                                                           |       |
#> |M_PI           |pi                                                                                                                                                                                |       |
#> |M_PI_2         |pi/2                                                                                                                                                                              |       |
#> |M_PI_4         |pi/4                                                                                                                                                                              |       |
#> |M_1_PI         |1/pi                                                                                                                                                                              |       |
#> |M_2_PI         |2/pi                                                                                                                                                                              |       |
#> |M_2_SQRTPI     |2/sqrt(pi)                                                                                                                                                                        |       |
#> |M_SQRT2        |sqrt(2)                                                                                                                                                                           |       |
#> |M_SQRT1_2      |1/sqrt(2)                                                                                                                                                                         |       |
#> |M_SQRT_3       |sqrt(3)                                                                                                                                                                           |       |
#> |M_SQRT_32      |sqrt(32)                                                                                                                                                                          |       |
#> |M_LOG10_2      |Log10(2)                                                                                                                                                                          |       |
#> |M_2PI          |2*pi                                                                                                                                                                              |       |
#> |M_SQRT_PI      |sqrt(pi)                                                                                                                                                                          |       |
#> |M_1_SQRT_2PI   |1/(sqrt(2*pi))                                                                                                                                                                    |       |
#> |M_LN_SQRT_PI   |log(sqrt(pi))                                                                                                                                                                     |       |
#> |M_LN_SQRT_2PI  |log(sqrt(2*pi))                                                                                                                                                                   |       |
#> |M_LN_SQRT_PId2 |log(sqrt(pi/2))                                                                                                                                                                   |       |
#> |pi             |pi                                                                                                                                                                                |       |
#> |NA             |R's NA value                                                                                                                                                                      |       |
#> |NaN            |Not a Number Value                                                                                                                                                                |       |
#> |Inf            |Infinite Value                                                                                                                                                                    |       |
#> |newind         |1: First record of individual; 2: Subsequent record of individual                                                                                                                 |NEWIND |
#> |rxFlag         |Flag for what part of the rxode2 model is being run; 1: ddt; 2: jac; 3: ini; 4: F; 5: lag; 6: rate; 7: dur; 8: mtime; 9: matrix exponential; 10: inductive linearization; 11: lhs |       |
```
