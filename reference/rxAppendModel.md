# Append two rxui models together

Append two rxui models together

## Usage

``` r
rxAppendModel(..., common = TRUE)
```

## Arguments

- ...:

  models to append together

- common:

  boolean that determines if you need a common value to bind

## Value

New model with both models appended together

## Author

Matthew L. Fidler

## Examples

``` r
# \donttest{

ocmt <- function() {
  ini({
    tka <- exp(0.45) # Ka
    tcl <- exp(1) # Cl
    tv <- exp(3.45); # log V
    ## the label("Label name") works with all models
    add.sd <- 0.7
  })
  model({
    ka <- tka
    cl <- tcl
    v <- tv
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl / v * center
    cp <- center / v
    cp ~ add(add.sd)
  })
}

idr <- function() {
  ini({
    tkin <- log(1)
    tkout <- log(1)
    tic50 <- log(10)
    gamma <- fix(1)
    idr.sd <- 1
  })
  model({
    kin <- exp(tkin)
    kout <- exp(tkout)
    ic50 <- exp(tic50)
    d/dt(eff) <- kin - kout*(1-ceff^gamma/(ic50^gamma+ceff^gamma))
    eff ~ add(idr.sd)
  })
}

rxAppendModel(ocmt %>% model(ceff=cp,append=TRUE), idr)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd      tkin     tkout     tic50     gamma 
#>  1.568312  2.718282 31.500392  0.700000  0.000000  0.000000  2.302585  1.000000 
#>    idr.sd 
#>  1.000000 
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#> 3                  3              eff
#>  ── Multiple Endpoint Model ($multipleEndpoint): ──  
#>   variable                cmt                dvid*
#> 1   cp ~ …  cmt='cp' or cmt=4  dvid='cp' or dvid=1
#> 2  eff ~ … cmt='eff' or cmt=3 dvid='eff' or dvid=2
#>   * If dvids are outside this range, all dvids are re-numered sequentially, ie 1,7, 10 becomes 1,2,3 etc
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 1.56831218549017
#>         tcl <- 2.71828182845905
#>         tv <- 31.5003923087479
#>         add.sd <- c(0, 0.7)
#>         tkin <- 0
#>         tkout <- 0
#>         tic50 <- 2.30258509299405
#>         gamma <- fix(1)
#>         idr.sd <- c(0, 1)
#>     })
#>     model({
#>         ka <- tka
#>         cl <- tcl
#>         v <- tv
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>         ceff <- cp
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ic50 <- exp(tic50)
#>         d/dt(eff) <- kin - kout * (1 - ceff^gamma/(ic50^gamma + 
#>             ceff^gamma))
#>         eff ~ add(idr.sd)
#>     })
#> }

# }
```
