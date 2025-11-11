# Ini block for rxode2/nlmixr models

The ini block controls initial conditions for 'theta' (fixed effects),
'omega' (random effects), and 'sigma' (residual error) elements of the
model.

## Usage

``` r
# S3 method for class 'rxUi'
ini(x, ..., envir = parent.frame(), append = NULL)

# Default S3 method
ini(x, ..., envir = parent.frame(), append = NULL)

ini(x, ..., envir = parent.frame(), append = NULL)
```

## Arguments

- x:

  expression

- ...:

  Other expressions for `ini()` function

- envir:

  the `environment` in which unevaluated model expressions is to be
  evaluated. May also be `NULL`, a list, a data frame, a pairlist or an
  integer as specified to `sys.call`.

- append:

  Reorder theta parameters. `NULL` means no change to parameter order. A
  parameter name (as a character string) means to put the new parameter
  after the named parameter. A number less than or equal to zero means
  to put the parameter at the beginning of the list. A number greater
  than the last parameter number means to put the parameter at the end
  of the list.

## Value

ini block

## Details

The `ini()` function is used in two different ways. The main way that it
is used is to set the initial conditions and associated attributes
(described below) in a model. The other way that it is used is for
updating the initial conditions in a model, often using the pipe
operator.

'theta' and 'sigma' can be set using either `<-` or `=` such as
`tvCL <- 1` or equivalently `tvCL = 1`. 'omega' can be set with a `~`
such as `etaCL ~ 0.1`.

Parameters can be named or unnamed (though named parameters are
preferred). A named parameter is set using the name on the left of the
assignment while unnamed parameters are set without an assignment
operator. `tvCL <- 1` would set a named parameter of `tvCL` to `1`.
Unnamed parameters are set using just the value, such as `1`.

For some estimation methods, lower and upper bounds can be set for
'theta' and 'sigma' values. To set a lower and/or upper bound, use a
vector of values. The vector is `c(lower, estimate, upper)`. The vector
may be given with just the estimate (`estimate`), the lower bound and
estimate (`c(lower, estimate)`), or all three
(`c(lower, estimate, upper)`). To set an estimate and upper bound
without a lower bound, set the lower bound to `-Inf`,
`c(-Inf, estimate, upper)`. When an estimation method does not support
bounds, the bounds will be ignored with a warning.

'omega' values can be set as a single value or as the values of a
lower-triangular matrix. The values may be set as either a
variance-covariance matrix (the default) or as a correlation matrix for
the off-diagonals with the standard deviations on the diagonals. Names
may be set on the left side of the `~`. To set a variance-covariance
matrix with variance values of 2 and 3 and a covariance of -2.5 use
`~c(2, 2.5, 3)`. To set the same matrix with names of `iivKa` and
`iivCL`, use `iivKa + iivCL~c(2, 2.5, 3)`. To set a correlation matrix
with standard deviations on the diagonal, use
[`cor()`](https://rdrr.io/r/stats/cor.html) like
`iivKa + iivCL~cor(2, -0.5, 3)`. As of rxode2 3.0 you can also use
`iivKa ~ 2, iivCL ~ c(2.5, 3)` for covariance matrices as well.

Values may be fixed (and therefore not estimated) using either the name
`fixed` at the end of the assignment or by calling `fixed()` as a
function for the value to fix. For 'theta' and 'sigma', either the
estimate or the full definition (including lower and upper bounds) may
be included in the fixed setting. For example, the following are all
effectively equivalent to set a 'theta' or 'sigma' to a fixed value
(because the lower and upper bounds are ignored for a fixed value):
`tvCL <- fixed(1)`, `tvCL <- fixed(0, 1)`, `tvCL <- fixed(0, 1, 2)`,
`tvCL <- c(0, fixed(1), 2)`, or `tvCL <- c(0, 1, fixed)`. For 'omega'
assignment, the full block or none of the block must be set as `fixed`.
Examples of setting an 'omega' value as fixed are: `iivKa~fixed(1)`,
`iivKa + iivCL~fixed(1, 2, 3)`, or `iivKa + iivCL~c(1, 2, 3, fixed)`.
Anywhere that `fixed` is used, `FIX`, `FIXED`, or `fix` may be used
equivalently.

For any value, standard mathematical operators or functions may be used
to define the value. For example, `log(2)` and `24*30` may be used to
define a value anywhere that a number can be used (e.g. lower bound,
estimate, upper bound, variance, etc.).

Values may be labeled using the `label()` function after the assignment.
Labels are are used to make reporting easier by giving a human-readable
description of the parameter, but the labels do not have any effect on
estimation. The typical way to set a label so that the parameter `tvCL`
has a label of "Typical Value of Clearance (L/hr)" is
`tvCL <- 1; label("Typical Value of Clearance (L/hr)")`.

Off diagonal values of 'omega' can be set to zero using the
[`diag()`](https://rdrr.io/r/base/diag.html) to remove all off-diagonals
can be removed with `ini(diag())`. To remove covariances of 'omega' item
with `iivKa`, you can use `%>% ini(diag(iivKa))`. Or to remove
covariances that contain either `iivKa` or `iivCl` you can use
`%>% ini(diag(iivKa, iivCl))`. For finer control you can remove the
covariance between two items (like `iivKa` and `iivCl`) by \`%\>%
ini(-cov(iivKa, iivCl))

`rxode2`/`nlmixr2` will attempt to determine some back-transformations
for the user. For example, `CL <- exp(tvCL)` will detect that `tvCL`
must be back-transformed by [`exp()`](https://rdrr.io/r/base/Log.html)
for easier interpretation. When you want to control the
back-transformation, you can specify the back-transformation using
`backTransform()` after the assignment. For example, to set the
back-transformation to [`exp()`](https://rdrr.io/r/base/Log.html), you
can use `tvCL <- 1; backTransform(exp())`.

## See also

Other Initial conditions:
[`zeroRe()`](https://nlmixr2.github.io/rxode2/reference/zeroRe.md)

## Author

Matthew Fidler

## Examples

``` r
# Set the ini() block in a model
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

# Use piping to update initial conditions
one.compartment %>% ini(tka <- log(2))
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.693147180559945`
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd 
#> 0.6931472 1.0006319 3.4499875 0.7000000 
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
#>         tka <- 0.693147180559945
#>         label("Ka")
#>         tcl <- 1.00063188030791
#>         label("Cl")
#>         tv <- 3.44998754583159
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
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
one.compartment %>% ini(tka <- label("Absorption rate, Ka (1/hr)"))
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd 
#> 0.4510756 1.0006319 3.4499875 0.7000000 
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
#>         tka <- 0.451075619360217
#>         label("Absorption rate, Ka (1/hr)")
#>         tcl <- 1.00063188030791
#>         label("Cl")
#>         tv <- 3.44998754583159
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
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
# Move the tka parameter to be just below the tv parameter (affects parameter
# summary table, only)
one.compartment %>% ini(tka <- label("Absorption rate, Ka (1/hr)"), append = "tv")
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tcl        tv       tka    add.sd 
#> 1.0006319 3.4499875 0.4510756 0.7000000 
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
#>         tcl <- 1.00063188030791
#>         label("Cl")
#>         tv <- 3.44998754583159
#>         label("V")
#>         tka <- 0.451075619360217
#>         label("Absorption rate, Ka (1/hr)")
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
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
# When programming with rxode2/nlmixr2, it may be easier to pass strings in
# to modify the ini
one.compartment %>% ini("tka <- log(2)")
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.693147180559945`
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>       tka       tcl        tv    add.sd 
#> 0.6931472 1.0006319 3.4499875 0.7000000 
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
#>         tka <- 0.693147180559945
#>         label("Ka")
#>         tcl <- 1.00063188030791
#>         label("Cl")
#>         tv <- 3.44998754583159
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
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
