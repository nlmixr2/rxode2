# Convert a positive base

Convert a positive base

## Usage

``` r
rxIntToBase(x, base = 36L)
```

## Arguments

- x:

  integer to convert

- base:

  can be 2 to 36

## Value

a sequence of letters and representing the number(s) input

## Author

Matthew L. Fidler

## Examples

``` r
rxIntToBase(1:100)
#>   [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "a"  "b"  "c"  "d"  "e"  "f" 
#>  [16] "g"  "h"  "i"  "j"  "k"  "l"  "m"  "n"  "o"  "p"  "q"  "r"  "s"  "t"  "u" 
#>  [31] "v"  "w"  "x"  "y"  "z"  "10" "11" "12" "13" "14" "15" "16" "17" "18" "19"
#>  [46] "1a" "1b" "1c" "1d" "1e" "1f" "1g" "1h" "1i" "1j" "1k" "1l" "1m" "1n" "1o"
#>  [61] "1p" "1q" "1r" "1s" "1t" "1u" "1v" "1w" "1x" "1y" "1z" "20" "21" "22" "23"
#>  [76] "24" "25" "26" "27" "28" "29" "2a" "2b" "2c" "2d" "2e" "2f" "2g" "2h" "2i"
#>  [91] "2j" "2k" "2l" "2m" "2n" "2o" "2p" "2q" "2r" "2s"
```
