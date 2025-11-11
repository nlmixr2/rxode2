# This function splits a function based on + or - terms

It uses the parser and does not disturb terms within other functions.
For example:

## Usage

``` r
rxSplitPlusQ(x, level = 0, mult = FALSE)
```

## Arguments

- x:

  Quoted R expression for splitting

- level:

  Internal level of parsing

- mult:

  boolean to split based on \* and / expressions instead. By default
  this is turned off.

## Value

character vector of the split expressions

## Details

a*exp(b+c)+d*log(e-f)-g\*f

would return

c("a \* exp(b + c)", "d \* log(e - f)", "- g \* f")

## Author

Matthew L. Fidler
