# Extract model lines from a rxui model

Extract model lines from a rxui model

## Usage

``` r
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)

# S3 method for class '`function`'
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)

# S3 method for class 'rxUi'
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)

# S3 method for class 'rxode2'
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)

# S3 method for class 'rxModelVars'
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)

# Default S3 method
modelExtract(
  x,
  ...,
  expression = FALSE,
  endpoint = FALSE,
  lines = FALSE,
  envir = parent.frame()
)
```

## Arguments

- x:

  model to extract lines from

- ...:

  variables to extract. When it is missing, it will extract the entire
  model (conditioned on the endpoint option below)

- expression:

  return expressions (if `TRUE`) or strings (if `FALSE`)

- endpoint:

  include endpoint. This can be:

  - `NA` – Missing means include both the endpoint and non-endpoint
    lines

  - `TRUE` – Only include endpoint lines

  - `FALSE` – Only include non-endpoint lines

- lines:

  is a boolean. When `TRUE` this will add the lines as an attribute to
  the output value ie `attr(, "lines")`

- envir:

  Environment for evaluating variables

## Value

expressions or strings of extracted lines. Note if there is a duplicated
lhs expression in the line, it will return both lines

## Author

Matthew L. Fidler

## Examples

``` r
one.compartment <- function() {
  ini({
    tka <- 0.45 # Log Ka
    tcl <- 1 # Log Cl
    tv <- 3.45    # Log V
    eta.ka ~ 0.6
    eta.cl ~ 0.3
    eta.v ~ 0.1
    add.sd <- 0.7
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v  <- exp(tv + eta.v)
    d/dt(depot)  <- -ka * depot
    d/dt(center) <-  ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
 }

 f <- one.compartment()

 modelExtract(f, cp)
#> [1] "cp <- center/v"

 modelExtract(one.compartment, d/dt(depot))
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> [1] "d/dt(depot) <- -ka * depot"

 # from variable
 var <- "d/dt(depot)"

 modelExtract(one.compartment, var)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> [1] "d/dt(depot) <- -ka * depot"

 modelExtract(f, endpoint=NA, lines=TRUE, expression=TRUE)
#> [[1]]
#> ka <- exp(tka + eta.ka)
#> 
#> [[2]]
#> cl <- exp(tcl + eta.cl)
#> 
#> [[3]]
#> v <- exp(tv + eta.v)
#> 
#> [[4]]
#> d/dt(depot) <- -ka * depot
#> 
#> [[5]]
#> d/dt(center) <- ka * depot - cl/v * center
#> 
#> [[6]]
#> cp <- center/v
#> 
#> [[7]]
#> cp ~ add(add.sd)
#> 
#> attr(,"lines")
#> [1] 1 2 3 4 5 6 7
```
