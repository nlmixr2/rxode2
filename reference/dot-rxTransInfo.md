# Get the information about the rxode2 derived parameter transformation

Get the information about the rxode2 derived parameter transformation

## Usage

``` r
.rxTransInfo(...)
```

## Arguments

- ...:

  Parameters translated, should be unquoted and not assigned to
  anything.

## Value

Translation information; This list contains:

- `$str` A named string of the parameters as seen in the underlying
  C/C++ code. The parameters that are NA are not used in the linear
  compartment model calculations.

- `$ncmt` the number of compartments in the model

- `$trans` the rxode2 translation number of the parameterization

This contains the linCmt() translation number, the number of
compartments and the parameters

## Author

Matthew L. Fidler

## Examples

``` r
.rxTransInfo(cl, v , Vp, vp2, q, q2)
#> $str
#>      p1      v1      p2      p3      p4      p5      ka 
#>      NA      NA    "cl"     "v"     "q"    "Vp" "rate1" 
#> 
#> $ncmt
#> [1] 3
#> 
#> $trans
#> [1] 1
#> 

.rxTransInfo(k12, k21, k13, k31, kel, v)
#> $str
#>      p1      v1      p2      p3      p4      p5      ka 
#>      NA      NA   "kel"     "v"   "k12"   "k21" "rate1" 
#> 
#> $ncmt
#> [1] 3
#> 
#> $trans
#> [1] 2
#> 

.rxTransInfo(k12, k21, k13, k31, kel, v, ka)
#> $str
#>      p1      v1      p2      p3      p4      p5      ka 
#>      NA      NA   "kel"     "v"   "k12"   "k21" "rate1" 
#> 
#> $ncmt
#> [1] 3
#> 
#> $trans
#> [1] 2
#> 

.rxTransInfo(CL, V)
#> $str
#>      p1      v1      p2      p3      p4      p5      ka 
#>      NA      NA    "CL"     "V"      NA      NA "rate1" 
#> 
#> $ncmt
#> [1] 1
#> 
#> $trans
#> [1] 1
#> 
```
