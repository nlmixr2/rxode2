# Assign covariates for piping

Assign covariates for piping

## Usage

``` r
rxSetCovariateNamesForPiping(covariates = NULL)
```

## Arguments

- covariates:

  NULL (for no covariates), or the list of covariates. nlmixr uses this
  function to set covariates if you pipe from a nlmixr fit.

## Value

Nothing, called for side effects

## Author

Matthew L. Fidler

## Examples

``` r
# First set the name of known covariates
# Note this is case sensitive

rxSetCovariateNamesForPiping(c("WT","HT", "TC"))

one.compartment <- function() {
 ini({
   tka <- 0.45 ; label("Log Ka")
   tcl <- 1 ; label("Log Cl")
   tv <- 3.45 ; label("Log V")
   eta.ka ~ 0.6
   eta.cl ~ 0.3
   eta.v ~ 0.1
   add.err <- 0.7
 })
 model({
   ka <- exp(tka + eta.ka)
   cl <- exp(tcl + eta.cl)
   v <- exp(tv + eta.v)
   d / dt(depot) <- -ka * depot
   d/dt(depot) <- -ka * depot
   d / dt(center) <- ka * depot - cl / v * center
   cp <- center / v
   cp ~ add(add.err)
 })
}

# now TC is detected as a covariate instead of a population parameter

one.compartment %>%
  model({ka <- exp(tka + eta.ka + TC * cov_C)})
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ add covariate `TC` (known covariate)
#> ℹ add population parameter `cov_C` and set estimate to 1
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>     tka     tcl      tv add.err   cov_C 
#>    0.45    1.00    3.45    0.70    1.00 
#> 
#> Omega ($omega): 
#>        eta.ka eta.cl eta.v
#> eta.ka    0.6    0.0   0.0
#> eta.cl    0.0    0.3   0.0
#> eta.v     0.0    0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level covariates
#> 1   tka eta.ka    id   TC*cov_C
#> 2   tcl eta.cl    id           
#> 3    tv  eta.v    id           
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Log Ka")
#>         tcl <- 1
#>         label("Log Cl")
#>         tv <- 3.45
#>         label("Log V")
#>         add.err <- c(0, 0.7)
#>         cov_C <- 1
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka + TC * cov_C)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.err)
#>     })
#> }

# You can turn it off by simply adding it back

rxSetCovariateNamesForPiping()

one.compartment %>%
  model({ka <- exp(tka + eta.ka + TC * cov_C)})
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ add population parameter `TC` and set estimate to 1
#> ℹ add population parameter `cov_C` and set estimate to 1
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>     tka     tcl      tv add.err      TC   cov_C 
#>    0.45    1.00    3.45    0.70    1.00    1.00 
#> 
#> Omega ($omega): 
#>        eta.ka eta.cl eta.v
#> eta.ka    0.6    0.0   0.0
#> eta.cl    0.0    0.3   0.0
#> eta.v     0.0    0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Log Ka")
#>         tcl <- 1
#>         label("Log Cl")
#>         tv <- 3.45
#>         label("Log V")
#>         add.err <- c(0, 0.7)
#>         TC <- 1
#>         cov_C <- 1
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka + TC * cov_C)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.err)
#>     })
#> }

# The covariates you set with `rxSetCovariateNamesForPiping()`
# are turned off every time you solve (or fit in nlmixr)
```
