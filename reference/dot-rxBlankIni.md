# Get a blank, theta1, or eta1 initialization block for iniDf

Get a blank, theta1, or eta1 initialization block for iniDf

## Usage

``` r
.rxBlankIni(type = c("empty", "theta", "eta"))
```

## Arguments

- type:

  type of initialization block to return

## Value

A data.frame with the appropriate number/type of columns.

For type="empty", the data.frame will have 0 rows but all the correct
types.

For type="theta", the data.frame will have 1 row with the correct types
and default values. The "name" and "est" will likely need to be updated.

For type="eta", the data.frame will have 1 row with the correct types
and default values for the a single eta being added. The "name" and
"est" will likely need to be updated.

## Author

Matthew L. Fidler

## Examples

``` r
.rxBlankIni("empty")
#>  [1] ntheta neta1  neta2  name   lower  est    upper  fix    err    label 
#> <0 rows> (or 0-length row.names)

.rxBlankIni("theta")
#>   ntheta neta1 neta2 name lower est upper   fix  err label
#> 1      1    NA    NA <NA>  -Inf   0   Inf FALSE <NA>  <NA>

.rxBlankIni("eta")
#>   ntheta neta1 neta2 name lower est upper   fix  err label
#> 1     NA     1     1 <NA>     0 0.1   Inf FALSE <NA>  <NA>
```
