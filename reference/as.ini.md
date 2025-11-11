# Turn into an ini block for initialization

Turn into an ini block for initialization

## Usage

``` r
as.ini(x)

# S3 method for class 'character'
as.ini(x)

# S3 method for class 'data.frame'
as.ini(x)

# S3 method for class 'call'
as.ini(x)

# S3 method for class 'lotriFix'
as.ini(x)

# S3 method for class 'matrix'
as.ini(x)

# Default S3 method
as.ini(x)
```

## Arguments

- x:

  Item to convert to a rxode2/nlmixr2 ui ini expression

## Value

rxode2 ini expression

## Author

Matthew L. Fidler

## Examples

``` r
ini <- quote(ini({
  tka <- log(1.57)
  tcl <- log(2.72)
  tv <- log(31.5)
  eta.ka ~ 0.6
  eta.cl ~ 0.3
  eta.v ~ 0.1
  add.sd <- 0.7
}))

as.ini(ini)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

l <- quote(lotri({
  tka <- log(1.57)
  tcl <- log(2.72)
  tv <- log(31.5)
  eta.ka ~ 0.6
  eta.cl ~ 0.3
  eta.v ~ 0.1
  add.sd <- 0.7
 }))

as.ini(l)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

m <- lotri({
   eta.ka ~ 0.6
   eta.cl ~ 0.3
   eta.v ~ 0.1
})

as.ini(m)
#> ini({
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

one.compartment <- function() {
  ini({
    tka <- log(1.57)
    tcl <- log(2.72)
    tv <- log(31.5)
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v
    cp ~ add(add.sd)
  })
}

as.ini(one.compartment)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- c(0, 0.7)
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

ui <- one.compartment()

as.ini(ui)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- c(0, 0.7)
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

ui$iniDf
#>   ntheta neta1 neta2   name lower       est upper   fix label backTransform
#> 1      1    NA    NA    tka  -Inf 0.4510756   Inf FALSE  <NA>          <NA>
#> 2      2    NA    NA    tcl  -Inf 1.0006319   Inf FALSE  <NA>          <NA>
#> 3      3    NA    NA     tv  -Inf 3.4499875   Inf FALSE  <NA>          <NA>
#> 4      4    NA    NA add.sd     0 0.7000000   Inf FALSE  <NA>          <NA>
#> 5     NA     1     1 eta.ka  -Inf 0.6000000   Inf FALSE  <NA>          <NA>
#> 6     NA     2     2 eta.cl  -Inf 0.3000000   Inf FALSE  <NA>          <NA>
#> 7     NA     3     3  eta.v  -Inf 0.1000000   Inf FALSE  <NA>          <NA>
#>   condition  err
#> 1      <NA> <NA>
#> 2      <NA> <NA>
#> 3      <NA> <NA>
#> 4        cp  add
#> 5        id <NA>
#> 6        id <NA>
#> 7        id <NA>

as.ini(ui$iniDf)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- c(0, 0.7)
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

ini <- c("ini({",
          "tka <- log(1.57)",
          "tcl <- log(2.72)",
          "tv <- log(31.5)",
          "eta.ka ~ 0.6",
          "eta.cl ~ 0.3",
          "eta.v ~ 0.1",
          "add.sd <- 0.7",
          "})")

as.ini(ini)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })

ini <- paste(ini, collapse="\n")

as.ini(ini)
#> ini({
#>     tka <- 0.451075619360217
#>     tcl <- 1.00063188030791
#>     tv <- 3.44998754583159
#>     add.sd <- 0.7
#>     eta.ka ~ 0.6
#>     eta.cl ~ 0.3
#>     eta.v ~ 0.1
#> })
```
