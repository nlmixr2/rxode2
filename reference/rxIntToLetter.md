# Convert a positive integer to a letter series

Convert a positive integer to a letter series

## Usage

``` r
rxIntToLetter(x, base = 26L)
```

## Arguments

- x:

  integer to convert

- base:

  can be 2 to 26

## Value

a sequence of letters representing the number(s) input

## Author

Matthew L. Fidler

## Examples

``` r
rxIntToLetter(1:100)
#>   [1] "b"  "c"  "d"  "e"  "f"  "g"  "h"  "i"  "j"  "k"  "l"  "m"  "n"  "o"  "p" 
#>  [16] "q"  "r"  "s"  "t"  "u"  "v"  "w"  "x"  "y"  "z"  "ba" "bb" "bc" "bd" "be"
#>  [31] "bf" "bg" "bh" "bi" "bj" "bk" "bl" "bm" "bn" "bo" "bp" "bq" "br" "bs" "bt"
#>  [46] "bu" "bv" "bw" "bx" "by" "bz" "ca" "cb" "cc" "cd" "ce" "cf" "cg" "ch" "ci"
#>  [61] "cj" "ck" "cl" "cm" "cn" "co" "cp" "cq" "cr" "cs" "ct" "cu" "cv" "cw" "cx"
#>  [76] "cy" "cz" "da" "db" "dc" "dd" "de" "df" "dg" "dh" "di" "dj" "dk" "dl" "dm"
#>  [91] "dn" "do" "dp" "dq" "dr" "ds" "dt" "du" "dv" "dw"
```
