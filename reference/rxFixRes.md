# Literally fix residual parameters

Literally fix residual parameters

## Usage

``` r
rxFixRes(ui, returnNull = FALSE)
```

## Arguments

- ui:

  rxode2 ui function

- returnNull:

  boolean for if unchanged values should return the original ui
  (`FALSE`) or null (`TRUE`)

## Value

model with residual parameters literally fixed in the model

## Author

Matthew L. Fidler

## Examples

``` r
One.comp.transit.allo <- function() {
 ini({
   # Where initial conditions/variables are specified
   lktr <- log(1.15)  #log k transit (/h)
   lcl  <- log(0.15)  #log Cl (L/hr)
   lv   <- log(7)     #log V (L)
   ALLC <- 0.75  #allometric exponent cl
   ALLV <- 1.00  #allometric exponent v
   prop.err <- fix(0.15)   #proportional error (SD/mean)
   add.err <- fix(0.6)     #additive error (mg/L)
   eta.ktr ~ 0.5
   eta.cl ~ 0.1
   eta.v ~ 0.1
 })
 model({
   #Allometric scaling on weight
   cl <- exp(lcl + eta.cl + ALLC * logWT70)
   v  <- exp(lv + eta.v + ALLV * logWT70)
   ktr <- exp(lktr + eta.ktr)
   # RxODE-style differential equations are supported
   d/dt(depot)   = -ktr * depot
   d/dt(central) =  ktr * trans - (cl/v) * central
   d/dt(trans)   =  ktr * depot - ktr * trans
   ## Concentration is calculated
   cp = central/v
   # And is assumed to follow proportional and additive error
   cp ~ prop(prop.err) + add(add.err)
 })
}

m <- rxFixRes(One.comp.transit.allo)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
```
