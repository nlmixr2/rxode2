# Apply the fixed population estimated parameters

Apply the fixed population estimated parameters

## Usage

``` r
rxFixPop(ui, returnNull = FALSE)
```

## Arguments

- ui:

  rxode2 ui function

- returnNull:

  boolean for if unchanged values should return the original ui
  (`FALSE`) or null (`TRUE`)

## Value

when `returnNull` is TRUE, NULL if nothing was changed, or the changed
model ui. When `returnNull` is FALSE, return a ui no matter if it is
changed or not.

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
   ALLC <- fix(0.75)  #allometric exponent cl
   ALLV <- fix(1.00)  #allometric exponent v
   prop.err <- 0.15   #proportional error (SD/mean)
   add.err <- 0.6     #additive error (mg/L)
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

m <- rxFixPop(One.comp.transit.allo)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
m
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       lktr        lcl         lv   prop.err    add.err 
#>  0.1397619 -1.8971200  1.9459101  0.1500000  0.6000000 
#> 
#> Omega ($omega): 
#>         eta.ktr eta.cl eta.v
#> eta.ktr     0.5    0.0   0.0
#> eta.cl      0.0    0.1   0.0
#> eta.v       0.0    0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2          central
#> 3                  3            trans
#>  ── μ-referencing ($muRefTable): ──  
#>   theta     eta level
#> 1   lcl  eta.cl    id
#> 2    lv   eta.v    id
#> 3  lktr eta.ktr    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         lktr <- 0.139761942375159
#>         lcl <- -1.89711998488588
#>         lv <- 1.94591014905531
#>         prop.err <- c(0, 0.15)
#>         add.err <- c(0, 0.6)
#>         eta.ktr ~ 0.5
#>         eta.cl ~ 0.1
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         cl <- exp(lcl + eta.cl + 0.75 * logWT70)
#>         v <- exp(lv + eta.v + 1 * logWT70)
#>         ktr <- exp(lktr + eta.ktr)
#>         d/dt(depot) = -ktr * depot
#>         d/dt(central) = ktr * trans - (cl/v) * central
#>         d/dt(trans) = ktr * depot - ktr * trans
#>         cp = central/v
#>         cp ~ prop(prop.err) + add(add.err)
#>     })
#> }

# now everything is already fixed, so calling again will do nothing

rxFixPop(m)
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       lktr        lcl         lv   prop.err    add.err 
#>  0.1397619 -1.8971200  1.9459101  0.1500000  0.6000000 
#> 
#> Omega ($omega): 
#>         eta.ktr eta.cl eta.v
#> eta.ktr     0.5    0.0   0.0
#> eta.cl      0.0    0.1   0.0
#> eta.v       0.0    0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2          central
#> 3                  3            trans
#>  ── μ-referencing ($muRefTable): ──  
#>   theta     eta level
#> 1   lcl  eta.cl    id
#> 2    lv   eta.v    id
#> 3  lktr eta.ktr    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         lktr <- 0.139761942375159
#>         lcl <- -1.89711998488588
#>         lv <- 1.94591014905531
#>         prop.err <- c(0, 0.15)
#>         add.err <- c(0, 0.6)
#>         eta.ktr ~ 0.5
#>         eta.cl ~ 0.1
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         cl <- exp(lcl + eta.cl + 0.75 * logWT70)
#>         v <- exp(lv + eta.v + 1 * logWT70)
#>         ktr <- exp(lktr + eta.ktr)
#>         d/dt(depot) = -ktr * depot
#>         d/dt(central) = ktr * trans - (cl/v) * central
#>         d/dt(trans) = ktr * depot - ktr * trans
#>         cp = central/v
#>         cp ~ prop(prop.err) + add(add.err)
#>     })
#> }

# if you call it with returnNull=TRUE when no changes have been
# performed, the function will return NULL

rxFixPop(m, returnNull=TRUE)
#> NULL
```
