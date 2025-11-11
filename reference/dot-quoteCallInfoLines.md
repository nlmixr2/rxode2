# Returns quoted call information

Returns quoted call information

## Usage

``` r
.quoteCallInfoLines(callInfo, envir = parent.frame(), iniDf = NULL)
```

## Arguments

- callInfo:

  Call information

- envir:

  Environment for evaluation (if needed)

- iniDf:

  The parent model `iniDf` when piping in a `ini` block (`NULL`
  otherwise)

## Value

Quote call information. for `name=expression`, change to
`name<-expression` in quoted call list. For expressions that are within
brackets ie [`{}`](https://rdrr.io/r/base/Paren.html), unlist the
brackets as if they were called in one single sequence.

## Author

Matthew L. Fidler
