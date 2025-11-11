# Set random effects and residual error to zero

Set random effects and residual error to zero

## Usage

``` r
zeroRe(object, which = c("omega", "sigma"), fix = TRUE)
```

## Arguments

- object:

  The model to modify

- which:

  The types of parameters to set to zero

- fix:

  Should the parameters be fixed to the zero value?

## Value

The `object` with some parameters set to zero

## See also

Other Initial conditions:
[`ini.rxUi()`](https://nlmixr2.github.io/rxode2/reference/ini.md)

## Author

Bill Denney

## Examples

``` r
one.compartment <- function() {
  ini({
    tka <- log(1.57); label("Ka")
    tcl <- log(2.72); label("Cl")
    tv <- log(31.5); label("V")
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
zeroRe(one.compartment)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd 
#> 0.4510756 1.0006319 3.4499875 0.0000000 
#> 
#> Omega ($omega): 
#>        eta.ka eta.cl eta.v
#> eta.ka      0      0     0
#> eta.cl      0      0     0
#> eta.v       0      0     0
#> attr(,"lotriFix")
#>        eta.ka eta.cl eta.v
#> eta.ka   TRUE  FALSE FALSE
#> eta.cl  FALSE   TRUE FALSE
#> eta.v   FALSE  FALSE  TRUE
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
#>         tka <- 0.451075619360217
#>         label("Ka")
#>         tcl <- 1.00063188030791
#>         label("Cl")
#>         tv <- 3.44998754583159
#>         label("V")
#>         add.sd <- fix(0, 0)
#>         eta.ka ~ fix(0)
#>         eta.cl ~ fix(0)
#>         eta.v ~ fix(0)
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```
