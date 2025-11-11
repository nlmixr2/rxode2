# Rename items inside of a `rxode2` ui model

`rxRename()` changes the names of individual variables, lhs, and ode
states using `new_name = old_name` syntax

## Usage

``` r
rxRename(.data, ..., envir = parent.frame())

.rxRename(.data, ..., envir = parent.frame())

rename.rxUi(.data, ...)

rename.function(.data, ...)

# S3 method for class 'rxUi'
rxRename(.data, ...)

# S3 method for class '`function`'
rxRename(.data, ...)

# Default S3 method
rxRename(.data, ...)
```

## Arguments

- .data:

  rxode2 ui function, named data to be consistent with
  [`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)

- ...:

  rename items

- envir:

  Environment for evaluation

## Value

New model with items renamed

## Details

This is similar to `dplyr`'s `rename()` function. When `dplyr` is
loaded, the `s3` methods work for the ui objects.

Note that the `.rxRename()` is the internal function that is called when
renaming and is likely not what you need to call unless you are writing
your own extension of the function

## Author

Matthew L. Fidler

## Examples

``` r
ocmt <- function() {
  ini({
    tka <- exp(0.45) # Ka
    tcl <- exp(1) # Cl
    ## This works with interactive models
    ## You may also label the preceding line with label("label text")
    tv <- exp(3.45) # log V
    ## the label("Label name") works with all models
    add.sd <- 0.7
  })
  model({
    ka <- tka
    cl <- tcl
    v <- tv
    d/dt(depot) = -ka * depot
    d/dt(center) = ka * depot - cl / v * center
    cp = center / v
    cp ~ add(add.sd)
  })
}

ocmt %>% rxRename(cpParent=cp)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd 
#>  1.568312  2.718282 31.500392  0.700000 
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 1.56831218549017
#>         tcl <- 2.71828182845905
#>         tv <- 31.5003923087479
#>         add.sd <- c(0, 0.7)
#>     })
#>     model({
#>         ka <- tka
#>         cl <- tcl
#>         v <- tv
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cpParent = center/v
#>         cpParent ~ add(add.sd)
#>     })
#> }
```
