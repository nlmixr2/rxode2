# Test if rxode2 uses linear solved systems

Test if rxode2 uses linear solved systems

## Usage

``` r
testRxLinCmt(ui, extra = "", .var.name = .vname(ui))

assertRxLinCmt(ui, extra = "", .var.name = .vname(ui))
```

## Arguments

- ui:

  rxode2 model

- extra:

  Extra text to append to the error message (like "for focei")

- .var.name:

  \[`character(1)`\]  
  Name of the checked object to print in assertions. Defaults to the
  heuristic implemented in
  [`vname`](https://mllg.github.io/checkmate/reference/vname.html).

## Value

TRUE if the model uses linear solved systems, FALSE otherwise

## Functions

- `assertRxLinCmt()`: Assert that the rxode2 uses linear solved systems

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
   linCmt() ~ add(add.sd)
 })
}

testRxLinCmt(one.cmt)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> [1] TRUE
```
