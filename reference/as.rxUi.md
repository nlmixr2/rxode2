# As rxode2 ui

As rxode2 ui

## Usage

``` r
as.rxUi(x)

# S3 method for class 'rxode2'
as.rxUi(x)

# S3 method for class 'rxode2tos'
as.rxUi(x)

# S3 method for class 'rxModelVars'
as.rxUi(x)

# S3 method for class '`function`'
as.rxUi(x)

# S3 method for class 'rxUi'
as.rxUi(x)

# Default S3 method
as.rxUi(x)
```

## Arguments

- x:

  Object to convert to `rxUi` object

## Value

rxUi object (or error if it cannot be converted)

## Author

Matthew L. Fidler

## Examples

``` r
mod1 <- function() {
 ini({
   # central 
   KA=2.94E-01
   CL=1.86E+01
   V2=4.02E+01
   # peripheral
   Q=1.05E+01
   V3=2.97E+02
   # effects
   Kin=1
   Kout=1
   EC50=200 
 })
 model({
   C2 <- centr/V2
   C3 <- peri/V3
   d/dt(depot) <- -KA*depot
   d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
   d/dt(peri)  <- Q*C2 - Q*C3
   eff(0) <- 1
   d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
 })
}

as.rxUi(mod1)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 4-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>      KA      CL      V2       Q      V3     Kin    Kout    EC50 
#>   0.294  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2            centr
#> 3                  3             peri
#> 4                  4              eff
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         KA <- 0.294
#>         CL <- 18.6
#>         V2 <- 40.2
#>         Q <- 10.5
#>         V3 <- 297
#>         Kin <- 1
#>         Kout <- 1
#>         EC50 <- 200
#>     })
#>     model({
#>         C2 <- centr/V2
#>         C3 <- peri/V3
#>         d/dt(depot) <- -KA * depot
#>         d/dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
#>         d/dt(peri) <- Q * C2 - Q * C3
#>         eff(0) <- 1
#>         d/dt(eff) <- Kin - Kout * (1 - C2/(EC50 + C2)) * eff
#>     })
#> }
```
