# Turn into a model expression

Turn into a model expression

## Usage

``` r
as.model(x)

# S3 method for class 'character'
as.model(x)

# S3 method for class 'call'
as.model(x)

# S3 method for class 'list'
as.model(x)

# Default S3 method
as.model(x)
```

## Arguments

- x:

  item to convert to a `model({})` expression

## Value

model expression

## Author

Matthew L. Fidler

## Examples

``` r
model <- quote(model({
  ka <- exp(tka + eta.ka)
  cl <- exp(tcl + eta.cl)
  v <- exp(tv + eta.v)
  d/dt(depot) = -ka * depot
  d/dt(center) = ka * depot - cl / v * center
  cp = center / v
  cp ~ add(add.sd)
}))

as.model(model)
#> model({
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     d/dt(depot) = -ka * depot
#>     d/dt(center) = ka * depot - cl/v * center
#>     cp = center/v
#>     cp ~ add(add.sd)
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

as.model(one.compartment)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> model({
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     d/dt(depot) = -ka * depot
#>     d/dt(center) = ka * depot - cl/v * center
#>     cp = center/v
#>     cp ~ add(add.sd)
#> })

ui <- one.compartment()

as.model(ui)
#> model({
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     d/dt(depot) = -ka * depot
#>     d/dt(center) = ka * depot - cl/v * center
#>     cp = center/v
#>     cp ~ add(add.sd)
#> })

model <- c("model({",
           "ka <- exp(tka + eta.ka)",
           "cl <- exp(tcl + eta.cl)",
           "v <- exp(tv + eta.v)",
           "d/dt(depot) = -ka * depot",
           "d/dt(center) = ka * depot - cl / v * center",
           "cp = center / v",
           "cp ~ add(add.sd)",
           "})")

as.model(model)
#> model({
#>     ka <- exp(tka + eta.ka)
#>     cl <- exp(tcl + eta.cl)
#>     v <- exp(tv + eta.v)
#>     d/dt(depot) = -ka * depot
#>     d/dt(center) = ka * depot - cl/v * center
#>     cp = center/v
#>     cp ~ add(add.sd)
#> })

model <- paste(model, collapse="\n")

as.model(model)
#> model({
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> })
```
