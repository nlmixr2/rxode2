# Compress/Decompress `rxode2` ui

Compress/Decompress `rxode2` ui

## Usage

``` r
rxUiDecompress(ui)

rxUiCompress(ui)
```

## Arguments

- ui:

  rxode2 ui object

## Value

A compressed or decompressed rxui object

## Author

Matthew L. Fidler

## Examples

``` r
one.cmt <- function() {
  ini({
    ## You may label each parameter with a comment
    tka <- 0.45 # Log Ka
    tcl <- log(c(0, 2.7, 100)) # Log Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- 3.45; label("log V")
    ## the label("Label name") works with all models
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    linCmt() ~ add(add.sd) | tmp
  })
}

f <- rxode2(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
print(class(f))
#> [1] "rxUi" "list"
print(is.environment(f))
#> [1] FALSE

f  <- rxUiDecompress(f)
print(class(f))
#> [1] "rxUi"
print(is.environment(f))
#> [1] TRUE

f  <- rxUiCompress(f)
print(class(f))
#> [1] "rxUi" "list"
print(is.environment(f))
#> [1] FALSE
```
