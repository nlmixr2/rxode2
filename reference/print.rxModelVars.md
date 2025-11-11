# Print Values

`print` prints its argument and returns it *invisibly* (via
[`invisible`](https://rdrr.io/r/base/invisible.html)`(x)`). It is a
generic function which means that new printing methods can be easily
added for new [`class`](https://rdrr.io/r/base/class.html)es.

## Usage

``` r
# S3 method for class 'rxModelVars'
print(x, ...)
```

## Arguments

- x:

  an object used to select a method.

- ...:

  further arguments passed to or from other methods.

## Value

This returns invisibly the model variables object

## Details

The default method,
[`print.default`](https://rdrr.io/r/base/print.default.html) has its own
help page. Use
[`methods`](https://rdrr.io/r/utils/methods.html)`("print")` to get all
the methods for the `print` generic.

`print.factor` allows some customization and is used for printing
[`ordered`](https://rdrr.io/r/base/factor.html) factors as well.

`print.table` for printing [`table`](https://rdrr.io/r/base/table.html)s
allows other customization. As of R 3.0.0, it only prints a description
in case of a table with 0-extents (this can happen if a classifier has
no valid data).

See [`noquote`](https://rdrr.io/r/base/noquote.html) as an example of a
class whose main purpose is a specific `print` method.

## References

Chambers, J. M. and Hastie, T. J. (1992) *Statistical Models in S.*
Wadsworth & Brooks/Cole.

## See also

The default method
[`print.default`](https://rdrr.io/r/base/print.default.html), and help
for the methods above; further
[`options`](https://rdrr.io/r/base/options.html),
[`noquote`](https://rdrr.io/r/base/noquote.html).

For more customizable (but cumbersome) printing, see
[`cat`](https://rdrr.io/r/base/cat.html),
[`format`](https://rdrr.io/r/base/format.html) or also
[`write`](https://rdrr.io/r/base/write.html). For a simple prototypical
print method, see
[`.print.via.format`](https://rdrr.io/r/tools/print.via.format.html) in
package tools.

## Examples

``` r
require(stats)

ts(1:20)  #-- print is the "Default function" --> print.ts(.) is called
#> Time Series:
#> Start = 1 
#> End = 20 
#> Frequency = 1 
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
for(i in 1:3) print(1:i)
#> [1] 1
#> [1] 1 2
#> [1] 1 2 3

## Printing of factors
attenu$station ## 117 levels -> 'max.levels' depending on width
#>   [1] 117  1083 1095 283  135  475  113  1008 1028 2001 117  1117 1438 1083 1013
#>  [16] 1014 1015 1016 1095 1011 1028 270  280  116  266  117  113  112  130  475 
#>  [31] 269  135  1093 1093 111  116  290  112  113  128  126  127  141  266  110 
#>  [46] 1027 111  125  135  475  262  269  1052 411  290  130  272  1096 1102 112 
#>  [61] 113  1028 2714 2708 2715 3501 655  272  1032 1377 1028 1250 1051 1293 1291
#>  [76] 1292 283  885  <NA> 2734 <NA> 2728 1413 1445 1408 1411 1410 1409 1377 1492
#>  [91] 1251 1422 1376 <NA> 286  <NA> 5028 942  <NA> 5054 958  952  5165 117  955 
#> [106] 5055 <NA> <NA> 5060 412  5053 5058 5057 <NA> 5051 <NA> 5115 <NA> 931  5056
#> [121] 5059 5061 <NA> 5062 5052 <NA> 724  <NA> 5066 5050 2316 5055 942  5028 5165
#> [136] 952  958  955  117  412  5053 5054 5058 5057 5115 5056 5060 1030 1418 1383
#> [151] 1308 1298 1299 1219 <NA> <NA> 1030 1418 1383 <NA> 1299 1308 1219 1456 5045
#> [166] 5044 5160 5043 5047 c168 5068 c118 5042 5067 5049 c204 5070 c266 c203 5069
#> [181] 5073 5072
#> 117 Levels: 1008 1011 1013 1014 1015 1016 1027 1028 1030 1032 1051 1052 ... c266

## ordered factors: levels  "l1 < l2 < .."
esoph$agegp[1:12]
#>  [1] 25-34 25-34 25-34 25-34 25-34 25-34 25-34 25-34 25-34 25-34 25-34 25-34
#> Levels: 25-34 < 35-44 < 45-54 < 55-64 < 65-74 < 75+
esoph$alcgp[1:12]
#>  [1] 0-39g/day 0-39g/day 0-39g/day 0-39g/day 40-79     40-79     40-79    
#>  [8] 40-79     80-119    80-119    80-119    120+     
#> Levels: 0-39g/day < 40-79 < 80-119 < 120+

## Printing of sparse (contingency) tables
set.seed(521)
t1 <- round(abs(rt(200, df = 1.8)))
t2 <- round(abs(rt(200, df = 1.4)))
table(t1, t2) # simple
#>     t2
#> t1    0  1  2  3  4  5  6  7  8 10 17 21 30
#>   0  21 22 14  4  1  0  1  1  1  0  1  0  0
#>   1  25 21  7  3  4  2  1  1  1  1  0  0  0
#>   2   8 16  9  2  0  0  0  0  0  0  0  1  1
#>   3   3  7  0  2  0  0  0  0  0  0  0  0  0
#>   4   1  5  2  0  0  0  1  0  0  0  0  0  0
#>   5   1  1  1  0  0  0  0  0  0  0  0  0  0
#>   6   1  3  0  0  0  0  0  0  0  0  0  0  0
#>   7   1  0  0  0  0  0  0  0  0  0  0  0  0
#>   9   0  0  0  1  0  0  0  0  0  0  0  0  0
#>   12  1  0  0  0  0  0  0  0  0  0  0  0  0
print(table(t1, t2), zero.print = ".") # nicer to read
#>     t2
#> t1    0  1  2  3  4  5  6  7  8 10 17 21 30
#>   0  21 22 14  4  1  .  1  1  1  .  1  .  .
#>   1  25 21  7  3  4  2  1  1  1  1  .  .  .
#>   2   8 16  9  2  .  .  .  .  .  .  .  1  1
#>   3   3  7  .  2  .  .  .  .  .  .  .  .  .
#>   4   1  5  2  .  .  .  1  .  .  .  .  .  .
#>   5   1  1  1  .  .  .  .  .  .  .  .  .  .
#>   6   1  3  .  .  .  .  .  .  .  .  .  .  .
#>   7   1  .  .  .  .  .  .  .  .  .  .  .  .
#>   9   .  .  .  1  .  .  .  .  .  .  .  .  .
#>   12  1  .  .  .  .  .  .  .  .  .  .  .  .

## same for non-integer "table":
T <- table(t2,t1)
T <- T * (1+round(rlnorm(length(T)))/4)
print(T, zero.print = ".") # quite nicer,
#>     t1
#> t2       0     1     2     3     4     5     6     7     9    12
#>   0  26.25 25.00 10.00  3.75  1.50  1.25  1.25  1.00     .  2.00
#>   1  22.00 26.25 16.00 10.50 27.50  1.25  3.00     .     .     .
#>   2  21.00  7.00 13.50     .  3.00  1.25     .     .     .     .
#>   3   5.00  3.75  2.50  2.50     .     .     .     .  1.50     .
#>   4   1.25  5.00     .     .     .     .     .     .     .     .
#>   5      .  2.00     .     .     .     .     .     .     .     .
#>   6   1.25  1.50     .     .  1.25     .     .     .     .     .
#>   7   1.25  1.25     .     .     .     .     .     .     .     .
#>   8   1.50  1.00     .     .     .     .     .     .     .     .
#>   10     .  1.00     .     .     .     .     .     .     .     .
#>   17  1.00     .     .     .     .     .     .     .     .     .
#>   21     .     .  2.75     .     .     .     .     .     .     .
#>   30     .     .  5.00     .     .     .     .     .     .     .
print.table(T[,2:8] * 1e9, digits=3, zero.print = ".")
#>     t1
#> t2          1        2        3        4        5        6        7
#>   0  2.50e+10 1.00e+10 3.75e+09 1.50e+09 1.25e+09 1.25e+09 1.00e+09
#>   1  2.62e+10 1.60e+10 1.05e+10 2.75e+10 1.25e+09 3.00e+09        .
#>   2  7.00e+09 1.35e+10        . 3.00e+09 1.25e+09        .        .
#>   3  3.75e+09 2.50e+09 2.50e+09        .        .        .        .
#>   4  5.00e+09        .        .        .        .        .        .
#>   5  2.00e+09        .        .        .        .        .        .
#>   6  1.50e+09        .        . 1.25e+09        .        .        .
#>   7  1.25e+09        .        .        .        .        .        .
#>   8  1.00e+09        .        .        .        .        .        .
#>   10 1.00e+09        .        .        .        .        .        .
#>   17        .        .        .        .        .        .        .
#>   21        . 2.75e+09        .        .        .        .        .
#>   30        . 5.00e+09        .        .        .        .        .
## still slightly inferior to  Matrix::Matrix(T)  for larger T

## Corner cases with empty extents:
table(1, NA) # < table of extent 1 x 0 >
#> < table of extent 1 x 0 >
```
