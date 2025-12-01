# Modifying Models

``` r
library(rxode2)
#> rxode2 5.0.0 using 2 threads (see ?getRxThreads)
#>   no cache: create with `rxCreateCache()`
```

There are two fundamental operations that you may wish to do in
`rxode2`/`nlmixr2`. First you might want to modify your model (ie add
covariate effects, add between subject variability, etc). The second
thing you may wish to do is change initial estimates, change the
boundaries of the problem, fix/unfix the initial estimates, etc.

## Modifying model

There are a few tasks you might want to do with the overall model:

- Change a line in the model

- Add a line to the model

- Rename parameters in the model

- Combine different models

- Create functions to add certain model features to the model

We will go over the model piping and other functions that you can use to
modify models and even add your own functions that modify models.

We will not cover any of the model modification functions in
`nlmixr2lib`

### Modifying a model line

In my opinion, modifying lines in a model is likely the most common task
in modifying a model. We may wish to modify the model to have a between
subject variability or add a covariate effects.

To begin of course you need a base model to modify. Let’s start with a
very simple PK example, using the single-dose theophylline dataset
generously provided by Dr. Robert A. Upton of the University of
California, San Francisco:

``` r
one.compartment <- function() {
  ini({
    tka <- 0.45; label("Ka")
    tcl <- 1; label("Cl")
    tv <- 3.45; label("V")
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
```

If we believed we did not have enough absorption to support between
subject variability you can change the line to drop the between subject
by modifying a single line. To do this simply type the line you want in
the model piping expression:

``` r
mod <- one.compartment |>
  model(ka <- exp(tka))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ! remove between subject variability `eta.ka`

print(mod)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v
#> eta.cl    0.3   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tcl eta.cl    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

As expected, the line is modified. Also you can notice that the initial
estimate for the between subject variability is dropped since it is no
longer part of the model.

If for some reason you wanted to add it back to the model you can modify
the model and add it back:

``` r
mod2 <- mod |>
  model(ka <- tka * exp(eta.ka))
#> ℹ add between subject variability `eta.ka` and set estimate to 1

print(mod2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.ka
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.ka    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tcl eta.cl    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.ka ~ 1
#>     })
#>     model({
#>         ka <- tka * exp(eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

In this modification, the `eta.ka` is automatically assumed to be a
between subject variability parameter. Also since `eta.ka` is not
mu-referenced `rxode2` points this out.

The automatic detection of `eta.ka` is because the name follows a
convention. Parameters starting or ending with the following names are
assumed to be between subject variability parameters:

- eta (from NONMEM convention)
- ppv (per patient variability)
- psv (per subject variability)
- iiv (inter-individual variability)
- bsv (between subject variability)
- bpv (between patient variability)

If this is not functioning correctly you can change it to a covariate
which you can add a type of initial estimate to later:

``` r
mod2 <- mod |>
  model(ka <- tka * exp(eta.ka) + WT * covWt, cov="eta.ka")
#> ℹ add covariate `eta.ka` (as requested by cov option)
#> ℹ add covariate `WT`
#> ℹ add population parameter `covWt` and set estimate to 1

print(mod2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd  covWt 
#>   0.45   1.00   3.45   0.70   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v
#> eta.cl    0.3   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level covariates
#> 1   tcl eta.cl    id           
#> 2    tv  eta.v    id           
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         covWt <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- tka * exp(eta.ka) + WT * covWt
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

As seen above, the `eta.ka` in the above model is assumed to be a
data-input parameter or covariate instead of an estimated parameter.

You can also note that `WT` is automatically recognized as a covariate
and `covWt` is automatically recognized as a covariate parameter.

In general covariates and typical/population parameters are
automatically converted to estimated parameters based on the parameter
name starting with (or ending with):

- tv (for typical value)
- t (also for typical value)
- pop (for population parameter)
- err (for error parameter)
- eff (for effect parameter)
- cov (for covariate parameters)

This has a few notable exceptions for parameters like (`wt`, `sex` and
`crcl`) which are assumed to be covariates.

If you don’t want any automatic variable conversion, you can also use
`auto=FALSE`:

``` r
mod3 <- mod |>
  model(ka <- tka * exp(eta.ka) + WT * covWt, auto=FALSE)
#> ℹ add covariate `eta.ka`
#> ℹ add covariate `WT`
#> ℹ add covariate `covWt`

print(mod3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v
#> eta.cl    0.3   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level covariates
#> 1   tcl eta.cl    id           
#> 2    tv  eta.v    id           
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- tka * exp(eta.ka) + WT * covWt
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

In this case all additional parameters (`eta.ka`, `WT`, and `covWt`) are
assumed to be parameters in the dataset.

### Note on automatic detection of variables

The automatic detection of variables is convenient for many models but
may not suit your style; If you do not like it you can always change it
by using [`options()`](https://rdrr.io/r/base/options.html):

``` r
options(rxode2.autoVarPiping=FALSE)
```

With this option disabled, all variables will be assumed to be
covariates and you will have to promote them to population parameters
with the `ini` block

In the last example with this option enabled none of the variables
starting with `t` will be added to the model

``` r
mod7 <- mod3 |>
  model({
    emax <- exp(temax)
    e0 <- exp(te0 + eta.e0)
    ec50 <- exp(tec50)
    kin <- exp(tkin)
    kout <- exp(tkout)
  }, append=FALSE)

print(mod7)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v
#> eta.cl    0.3   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level covariates
#> 1   tcl eta.cl    id           
#> 2    tv  eta.v    id           
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- tka * exp(eta.ka) + WT * covWt
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

Of course you could use it and then turn it back on:

``` r
options(rxode2.autoVarPiping=TRUE)
mod8 <- mod |>
  model({
    emax <- exp(temax)
    e0 <- exp(te0 + eta.e0)
    ec50 <- exp(tec50)
    kin <- exp(tkin)
    kout <- exp(tkout)
  }, append=FALSE)
#> ℹ promote `temax` to population parameter with initial estimate 1
#> ℹ promote `te0` to population parameter with initial estimate 1
#> ℹ promote `eta.e0` to between subject variability with initial estimate 1
#> ℹ promote `tec50` to population parameter with initial estimate 1
#> ℹ promote `tkin` to population parameter with initial estimate 1
#> ℹ promote `tkout` to population parameter with initial estimate 1

print(mod8)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd  temax    te0  tec50   tkin  tkout 
#>   0.45   1.00   3.45   0.70   1.00   1.00   1.00   1.00   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         temax <- 1
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tkout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

Or you can use the
`withr::with_options(list(rxode2.autoVarPiping=FALSE), ...)` to turn the
option on temporarily.

If you don’t like the defaults for changing variables you could change
them as well with
[`rxSetPipingAuto()`](https://nlmixr2.github.io/rxode2/reference/rxSetPipingAuto.md)

For example if you only wanted variables starting or ending with `te`
you can change this with:

``` r
rxSetPipingAuto(thetamodelVars = rex::rex("te"))

mod9 <- mod |>
  model({
    emax <- exp(temax)
    e0 <- exp(te0 + eta.e0)
    ec50 <- exp(tec50)
    kin <- exp(tkin)
    kout <- exp(tkout)
  }, append=FALSE)
#> ℹ promote `temax` to population parameter with initial estimate 1
#> ℹ promote `te0` to population parameter with initial estimate 1
#> ℹ promote `eta.e0` to between subject variability with initial estimate 1
#> ℹ promote `tec50` to population parameter with initial estimate 1

print(mod9)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd  temax    te0  tec50 
#>   0.45   1.00   3.45   0.70   1.00   1.00   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         temax <- 1
#>         te0 <- 1
#>         tec50 <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

And as requested only the population parameters starting with `te` are
added to the `ini` block.

If you want to reset the defaults you simply call
[`rxSetPipingAuto()`](https://nlmixr2.github.io/rxode2/reference/rxSetPipingAuto.md)
without any arguments:

``` r
rxSetPipingAuto()
mod10 <- mod |>
  model({
    emax <- exp(temax)
    e0 <- exp(te0 + eta.e0)
    ec50 <- exp(tec50)
    kin <- exp(tkin)
    kout <- exp(tkout)
  }, append=FALSE)
#> ℹ promote `temax` to population parameter with initial estimate 1
#> ℹ promote `te0` to population parameter with initial estimate 1
#> ℹ promote `eta.e0` to between subject variability with initial estimate 1
#> ℹ promote `tec50` to population parameter with initial estimate 1
#> ℹ promote `tkin` to population parameter with initial estimate 1
#> ℹ promote `tkout` to population parameter with initial estimate 1
```

### Adding model lines

There are three ways to insert lines in a `rxode2`/`nlmixr2` model. You
can add lines to the end of the model, after an expression or to the
beginning of the model all controlled by the `append` option.

Let’s assume that there are two different assays that were run with the
same compound and you have noticed that they both have different
variability.

You can modify the model above by adding some lines to the end of the
model by using `append=TRUE`:

``` r
mod4 <- mod |>
  model({
    cp2 <- cp
    cp2 ~ lnorm(lnorm.sd)
  }, append=TRUE)
#> ℹ add residual parameter `lnorm.sd` and set estimate to 1

print(mod4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>      tka      tcl       tv   add.sd lnorm.sd 
#>     0.45     1.00     3.45     0.70     1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v
#> eta.cl    0.3   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── Multiple Endpoint Model ($multipleEndpoint): ──  
#>   variable                cmt                dvid*
#> 1   cp ~ …  cmt='cp' or cmt=3  dvid='cp' or dvid=1
#> 2  cp2 ~ … cmt='cp2' or cmt=4 dvid='cp2' or dvid=2
#>   * If dvids are outside this range, all dvids are re-numered sequentially, ie 1,7, 10 becomes 1,2,3 etc
#> 
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tcl eta.cl    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         lnorm.sd <- c(0, 1)
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>         cp2 <- cp
#>         cp2 ~ lnorm(lnorm.sd)
#>     })
#> }
```

Perhaps instead you may want to add an indirect response model in
addition to the concentrations, you can choose where to add this: with
`append=lhsVar` where `lhsVar` is the left handed variable above where
you want to insert the new lines:

``` r
mod5 <- mod |>
  model({
    PD <- 1-emax*cp/(ec50+cp)
    ##
    effect(0) <- e0
    kin <- e0*kout
    d/dt(effect) <- kin*PD -kout*effect
  }, append=d/dt(center))
```

The last type of insertion you may wish to do is to add lines to the
beginning of the model by using `append=FALSE`:

``` r
mod6 <- mod5 |>
  model({
    emax <- exp(temax)
    e0 <- exp(te0 + eta.e0)
    ec50 <- exp(tec50)
    kin <- exp(tkin)
    kout <- exp(tkout)
  }, append=FALSE)
#> ℹ promote `temax` to population parameter with initial estimate 1
#> ℹ promote `te0` to population parameter with initial estimate 1
#> ℹ promote `eta.e0` to between subject variability with initial estimate 1
#> ℹ promote `tec50` to population parameter with initial estimate 1
#> ℹ promote `tkin` to population parameter with initial estimate 1
#> ℹ promote `tkout` to population parameter with initial estimate 1

print(mod6)
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd  temax    te0  tec50   tkin  tkout 
#>   0.45   1.00   3.45   0.70   1.00   1.00   1.00   1.00   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#> 3                  3           effect
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         temax <- 1
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tkout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         PD <- 1 - emax * cp/(ec50 + cp)
#>         effect(0) <- e0
#>         kin <- e0 * kout
#>         d/dt(effect) <- kin * PD - kout * effect
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Remove lines in the model

The lines in a model can be removed in one of 2 ways either use `-param`
or `param <- NULL` in model piping:

``` r
mod7 <- mod6 |>
  model(-emax)
#> ! remove population parameter `temax`

print(mod7)
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd    te0  tec50   tkin  tkout 
#>   0.45   1.00   3.45   0.70   1.00   1.00   1.00   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#> 3                  3           effect
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tkout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         PD <- 1 - emax * cp/(ec50 + cp)
#>         effect(0) <- e0
#>         kin <- e0 * kout
#>         d/dt(effect) <- kin * PD - kout * effect
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# Equivalently

mod8 <- mod6 |>
  model(emax <- NULL)
#> ! remove population parameter `temax`

print(mod8)
#>  ── rxode2-based free-form 3-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd    te0  tec50   tkin  tkout 
#>   0.45   1.00   3.45   0.70   1.00   1.00   1.00   1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#> 3                  3           effect
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tkout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         PD <- 1 - emax * cp/(ec50 + cp)
#>         effect(0) <- e0
#>         kin <- e0 * kout
#>         d/dt(effect) <- kin * PD - kout * effect
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Rename parameters in a model

You may want to rename parameters in a model, which is easy to do with
[`rxRename()`](https://nlmixr2.github.io/rxode2/reference/rxRename.md).
When `dplyr` is loaded you can even replace it with
[`rename()`](https://dplyr.tidyverse.org/reference/rename.html). The
semantics are similar between the two functions, that is you assigning
`newVar=oldVar`. For example:

``` r
mod11 <- mod10 |>
  rxRename(drug1kout=kout, tv.drug1kout=tkout)

print(mod11)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>          tka          tcl           tv       add.sd        temax          te0 
#>         0.45         1.00         3.45         0.70         1.00         1.00 
#>        tec50         tkin tv.drug1kout 
#>         1.00         1.00         1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         temax <- 1
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tv.drug1kout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         drug1kout <- exp(tv.drug1kout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You can see every instance of the variable is named in the model is
renamed inside the `model` and `ini` block.

For completeness you can see this with the `dplyr` verb (since it is a
S3 method):

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
mod12 <- mod10 |>
  rename(drug1kout=kout, tv.drug1kout=tkout)

print(mod12)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>          tka          tcl           tv       add.sd        temax          te0 
#>         0.45         1.00         3.45         0.70         1.00         1.00 
#>        tec50         tkin tv.drug1kout 
#>         1.00         1.00         1.00 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.e0
#> eta.cl    0.3   0.0      0
#> eta.v     0.0   0.1      0
#> eta.e0    0.0   0.0      1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   te0 eta.e0    id
#> 2   tcl eta.cl    id
#> 3    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Ka")
#>         tcl <- 1
#>         label("Cl")
#>         tv <- 3.45
#>         label("V")
#>         add.sd <- c(0, 0.7)
#>         temax <- 1
#>         te0 <- 1
#>         tec50 <- 1
#>         tkin <- 1
#>         tv.drug1kout <- 1
#>         eta.cl ~ 0.3
#>         eta.v ~ 0.1
#>         eta.e0 ~ 1
#>     })
#>     model({
#>         emax <- exp(temax)
#>         e0 <- exp(te0 + eta.e0)
#>         ec50 <- exp(tec50)
#>         kin <- exp(tkin)
#>         drug1kout <- exp(tv.drug1kout)
#>         ka <- exp(tka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) = -ka * depot
#>         d/dt(center) = ka * depot - cl/v * center
#>         cp = center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Combine different models

You can also combine different models with
[`rxAppendModel()`](https://nlmixr2.github.io/rxode2/reference/rxAppendModel.md).
In general they need variables in common to combine. This is because you
generally want the models to link between each other. In the below
example a pk and pd model this is done by renaming `cp` in the first
model to `ceff` in the second model:

``` r
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

rxAppendModel(ocmt %>% rxRename(ceff=cp), idr)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
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
#>   variable                 cmt                 dvid*
#> 1 ceff ~ … cmt='ceff' or cmt=4 dvid='ceff' or dvid=1
#> 2  eff ~ …  cmt='eff' or cmt=3  dvid='eff' or dvid=2
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
#>         ceff <- center/v
#>         ceff ~ add(add.sd)
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ic50 <- exp(tic50)
#>         d/dt(eff) <- kin - kout * (1 - ceff^gamma/(ic50^gamma + 
#>             ceff^gamma))
#>         eff ~ add(idr.sd)
#>     })
#> }
```

You will get an error if you try to combine models without variables in
common:

``` r
try(rxAppendModel(ocmt, idr))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> Error : not all the models have variables in common (use `common=FALSE` to allow this)
```

If you want to combine the models without respecting the having the
variables in common, you can use `common=FALSE`:

``` r
mod2 <- rxAppendModel(ocmt, idr, common=FALSE) |>
  model(ceff=cp, append=ic50) # here we add the translation after the
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
                              # ic50 line to make it reasonable

print(mod2)
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
#>         kin <- exp(tkin)
#>         kout <- exp(tkout)
#>         ic50 <- exp(tic50)
#>         ceff <- cp
#>         d/dt(eff) <- kin - kout * (1 - ceff^gamma/(ic50^gamma + 
#>             ceff^gamma))
#>         eff ~ add(idr.sd)
#>     })
#> }
```

### Creating more complex model modification functions

These are pretty flexible, but you may want to do even more, so there
are some helper functions to help you create functions to do more. We
will discuss how to extract the model from the function and how to
update it.

Lets start with a model:

``` r
f <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    eta.ka ~ 0.6
    eta.v ~ 0.1
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl/v * center
    cp <- center/v
  })
}
```

Lets assume for a moment you want to remove an eta to `cl`. First you
probably want to get all the model lines. You can do that with
[`modelExtract()`](https://nlmixr2.github.io/rxode2/reference/modelExtract.md):

``` r
totLines <- modelExtract(f, endpoint=NA) # endpoints should be included
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(totLines)
#> [1] "ka <- exp(tka + eta.ka)"                   
#> [2] "cl <- exp(tcl)"                            
#> [3] "v <- exp(tv + eta.v)"                      
#> [4] "d/dt(depot) <- -ka * depot"                
#> [5] "d/dt(center) <- ka * depot - cl/v * center"
#> [6] "cp <- center/v"
```

Now you want to only worry about the `cl` line, you can subset here:

``` r
clLine <- modelExtract(f, cl, lines=TRUE)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
line <- attr(clLine, "lines")
```

Now I wish to change the line to “cl \<- exp(tcl+eta.cl)”

``` r
totLines[line] <- "cl <- exp(tcl+eta.cl)"

# For now lets remove the entire `ini` block (so you don't have to
# worry about syncing parameters).

#

ini(f) <- NULL
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

model(f) <- totLines
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f)
#> function () 
#> {
#>     ini({
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>     })
#> }
#> <environment: 0x565217019e98>
```

Note that these functions do not modify the `ini({})` block. You may
have to modify the ini block first to make it a valid `rxode2`/`nlmixr2`
model.

In this particular case, using model piping would be easier, but it
simply demonstrates two different way to extract model information and a
way to add information to the final model.

These methods can be tricky because when using them you have to have
model that is parsed correctly. This means you have to make sure the
parameters and endpoints follow the correct rules

## Modifying initial estimates

The common items you want to do with initial estimates are:

- Fix/Unfix a parameter

- Change the initial condition values and bounds

- Change the initial condition type

- Change labels and transformations

- Reorder parameters

- Remove covariances between all parameters or a group of parameters

You may wish to create your own functions; we will discuss this too.

### Fixing or unfixing a parameter

You can fix model estimates in two ways. The first is to fix the value
to whatever is in the model function, this is done by piping the model
parameter name (like `tka`) and setting it equal to `fix`
(`%>% ini(tka=fix)`). Below is a full example:

``` r
f <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    add.sd <- c(0, 0.7)
    eta.ka ~ 0.6
    eta.v ~ 0.1
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl/v * center
    cp <- center/v
    cp ~ add(add.sd)
  })
}

f2 <- f |>
  ini(tka=fix)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ fix `tka` to `0.45`

print(f2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- fix(0.45)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You can also fix the parameter to a different value if you wish; This is
very similar you can specify the value to fix inside of a `fix`
pseudo-function as follows: `%>% ini(tka=fix(0.1))`. A fully worked
example is below:

``` r
f <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    add.sd <- c(0, 0.7)
    eta.ka ~ 0.6
    eta.v ~ 0.1
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl/v * center
    cp <- center/v
    cp ~ add(add.sd)
  })
}

f2 <- f |>
  ini(tka=fix(0.1))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ fix `tka` to `0.45`
#> ℹ change initial estimate of `tka` to `0.1`

print(f2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- fix(0.1)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Unfixing parameters

You an unfix parameters very similarly to fixing. Instead of using the
`fix` keyword, you use the `unfix` keyword. So to unfix a parameter
(keeping its value) you would pipe the model using
(`|> ini(tka=unfix)`). Starting with the fixed model above a fully
worked example is:

``` r
print(f2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- fix(0.1)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

f3 <- f2 |> ini(tka=unfix)
#> ℹ unfix `tka` keeping initial estimate `0.1`

print(f3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You can also unfix and change the initial estimate with
`ini(parameter=unfix(newEst))`:

``` r
print(f2)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- fix(0.1)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

f3 <- f2 |>
  ini(tka=unfix(10))
#> ℹ unfix `tka` keeping initial estimate `0.1`
#> ℹ change initial estimate of `tka` to `10`

print(f3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>  10.00   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 10
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Changing the parameter values and possibly bounds

#### Multiple parameter assignment

You can also assign multiple parameters by providing them:

- As a vector/list

- As multiple lines in a piped
  [`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) block

- Using a covariance matrix

In the case of a vector you can specify them and then pipe the model.

For example:

``` r
ini1 <- c(tka=0.1, tcl=1, tv=3)

f4 <- f |> ini(ini1)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`
#> ℹ change initial estimate of `tcl` to `1`
#> ℹ change initial estimate of `tv` to `3`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>    0.1    1.0    3.0    0.7 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# or equivalently

ini1 <- list(tka=0.1, tcl=1, tv=3)

f4a <- f |> ini(ini1)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`
#> ℹ change initial estimate of `tcl` to `1`
#> ℹ change initial estimate of `tv` to `3`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>    0.1    1.0    3.0    0.7 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

This can also be added with multiple lines or commas separating
estimates:

``` r
# commas separating values:
f4 <- f |> ini(tka=0.1, tcl=1, tv=3)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`
#> ℹ change initial estimate of `tcl` to `1`
#> ℹ change initial estimate of `tv` to `3`
print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>    0.1    1.0    3.0    0.7 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# multiple lines in {}
f4 <- f |>
  ini({
    tka <- 0.2
    tcl <- 2
    tv <- 6
  })
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.2`
#> ℹ change initial estimate of `tcl` to `2`
#> ℹ change initial estimate of `tv` to `6`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>    0.2    2.0    6.0    0.7 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.2
#>         tcl <- 2
#>         tv <- 6
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You could also use a matrix to specify the covariance:

``` r
ome <- lotri(eta.ka + eta.v ~ c(0.6,
                                0.01, 10.1))

f4 <- f |> ini(ome)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `eta.ka` to `0.6`
#> ℹ add covariance between `eta.v` and `eta.ka` with initial estimate `0.01`
#> ℹ change initial estimate of `eta.v` to `10.1`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka   0.60  0.01
#> eta.v    0.01 10.10
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ c(0.01, 10.1)
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# or equavialtly use the lotri-type syntax for the omega:

f4 <- f |> ini(eta.ka + eta.v ~ c(0.6,
                                  0.01, 0.2))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `eta.ka` to `0.6`
#> ℹ add covariance between `eta.v` and `eta.ka` with initial estimate `0.01`
#> ℹ change initial estimate of `eta.v` to `0.2`
print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka   0.60  0.01
#> eta.v    0.01  0.20
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ c(0.01, 0.2)
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

Another option is to use a variable, but prefix it with `~` so that they
are promoted to between subject variabilities:

``` r
eta <- c(eta.ka=0.6, eta.v=0.6)

f5 <- f |> ini(~eta)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `eta.ka` to `0.6`
#> ℹ change initial estimate of `eta.v` to `0.6`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka   0.60  0.01
#> eta.v    0.01  0.20
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ c(0.01, 0.2)
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

#### Single parameter assignment

The simplest way to change the initial parameter estimates is to simply
use `ini(parameter=newValue)`. You can also use `<-` or `~` to change
the value:

A fully worked example showing all three types of initial value
modification is:

``` r
f3 <- f |>
  ini(tka <- 0.1)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`

f4 <- f |>
  ini(tka=0.1)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`

f5 <- f |>
  ini(tka ~ 0.1)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate of `tka` to `0.1`

print(f5)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You can change the bounds like you do in the model specification by
using a numeric vector of `c(low, estimate)` or `c(low, estimate, hi)`.
Here is a worked example:

``` r
f3 <- f |>
  ini(tka <- c(0, 0.1, 0.2))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate (0.1) and upper/lower bound (0 to 0.2) of `tka`

print(f3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- c(0, 0.1, 0.2)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }


f3 <- f |>
  ini(tka <- c(0, 0.1))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate (0.1) and lower bound (0) of `tka`

print(f3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- c(0, 0.1)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

Note by changing the parameters to their default values they might not
show up in the parameter printout:

``` r
f3 <- f |>
  ini(tka <- c(0, 0.1, 0.2))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ change initial estimate (0.1) and upper/lower bound (0 to 0.2) of `tka`

print(f3)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- c(0, 0.1, 0.2)
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# Now reassign
f4 <- f3 |>
  ini(tka <- c(-Inf, 0.1, Inf))
#> ℹ change initial estimate (0.1) and upper/lower bound (-Inf to Inf) of `tka`

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.10   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.1
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

#### Changing parameter types

You can change the parameter type by two operators either by using
`-par` to convert the parameter to a covariate or `~par` to toggle
between population and individual parameters.

Here is an example that does all 3:

``` r
# Switch population parameter to between subject variability parameter:
f4 <- f |>
  ini( ~ tcl)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ convert 'tcl' from population parameter to between subject variability

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd 
#>   0.45   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v tcl
#> eta.ka    0.6   0.0   0
#> eta.v     0.0   0.1   0
#> tcl       0.0   0.0   1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>         tcl ~ 1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# Switch back to population parameter
f5 <- f4 |>
  ini( ~ tcl)
#> ℹ convert 'tcl' from between subject variability to population parameter

print(f5)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd    tcl 
#>   0.45   3.45   0.70   1.00 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         tcl <- 1
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# Change the variable to a covariate parameter (ie it doesn't have an
# initial estimate so remove it with the `-` operator):

f6 <- f4 |>
  ini(-tcl)
#> ℹ changing between subject variability parameter 'tcl' to covariate parameter

print(f6)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd 
#>   0.45   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# You can change the covariate or remove the parameter estimate by
# `tcl <- NULL`:

f6 <- f4 |>
  ini(tcl <- NULL)
#> ℹ changing between subject variability parameter 'tcl' to covariate parameter

print(f6)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd 
#>   0.45   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# to add it back as a between subject variability or population
# parameter you can pipe it as follows:

f7 <- f6 |>
  ini(tcl=4)
#> ℹ promote `tcl` to population parameter with initial estimate 4
#> ℹ change initial estimate of `tcl` to `4`

print(f7)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd    tcl 
#>   0.45   3.45   0.70   4.00 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         tcl <- 4
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }


f8 <- f6 |>
  ini(tcl ~ 0.1)
#> ℹ promote `tcl` to between subject variability with initial estimate 0.1
#> ℹ change initial estimate of `tcl` to `0.1`

print(f8)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka     tv add.sd 
#>   0.45   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v tcl
#> eta.ka    0.6   0.0 0.0
#> eta.v     0.0   0.1 0.0
#> tcl       0.0   0.0 0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>         tcl ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

#### Changing parameter labels

If you want to change/add a parameter label you assign the parameter to
`label("label to add")`. For example:

``` r
f4 <- f |>
  ini(tka=label("Typical Ka (1/hr)"))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f4)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         label("Typical Ka (1/hr)")
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

You can also change the order while performing operations:

``` r
f5 <- f |>
  ini(tka=label("Typical Ka (1/hr)"), append=tcl)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f5)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tcl    tka     tv add.sd 
#>   1.00   0.45   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tcl <- 1
#>         tka <- 0.45
#>         label("Typical Ka (1/hr)")
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

If you want to remove the labels you can remove them with
`ini(par=label(NULL))`; For example:

``` r
f6 <- f |>
  ini(tka=label(NULL))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f6)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

#### Changing parameter transformations

Back-transformations over-ride the back transformations in `nlmixr2`
models. They are very similar to the modification of the labels.

Here you use `|> ini(tka=backTransform(exp))` to add an exponential
back-transformation for data:

``` r
f7 <- f |>
  ini(tka=backTransform(exp))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f7)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         backTransform("exp")
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

If you wish to remove them you can also do that with
`|> ini(tka=backTransform(NULL))`:

``` r
f8 <- f |>
  ini(tka=backTransform(NULL))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

print(f8)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 0.45
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### Removing covariances between between subject varaibilities

There are two approaches to removing covarinaces for between subject
variabilities: [`diag()`](https://rdrr.io/r/base/diag.html) and
`-cov(var1, var2)`

The [`diag()`](https://rdrr.io/r/base/diag.html) removes either all
covariance elements (with no arguments) or any covariance elements
included in the argument list:

``` r
fd <- function() {
  ini({
    tka <- 0.45
    tcl <- 1
    tv <- 3.45
    add.sd <- c(0, 0.7)
    eta.ka ~ 0.6
    eta.v ~ c(0.01, 0.1)
    eta.cl ~ c(0.01, 0.01, 1)
  })
  model({
    ka <- exp(tka + eta.ka)
    cl <- exp(tcl + eta.cl)
    v <- exp(tv + eta.v)
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - cl/v * center
    cp <- center/v
    cp ~ add(add.sd)
  })
}

# If you want to remove all covariances you can use diag() with no
# arguments

fd %>% ini(diag())
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ remove covariance `(eta.ka,eta.v)`
#> ℹ remove covariance `(eta.ka,eta.cl)`
#> ℹ remove covariance `(eta.v,eta.cl)`
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v eta.cl
#> eta.ka    0.6   0.0      0
#> eta.v     0.0   0.1      0
#> eta.cl    0.0   0.0      1
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
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>         eta.cl ~ 1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# if you want to remove only covariances with eta.ka  you can use:
fd %>% ini(diag(eta.ka))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ remove covariance `(eta.ka,eta.v)`
#> ℹ remove covariance `(eta.ka,eta.cl)`
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.cl eta.v eta.ka
#> eta.cl   1.00  0.01    0.0
#> eta.v    0.01  0.10    0.0
#> eta.ka   0.00  0.00    0.6
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
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.cl ~ 1
#>         eta.v ~ c(0.01, 0.1)
#>         eta.ka ~ 0.6
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }

# if you want to remove only the covariances with eta.ka and eta.v you can use:
fd %>% ini(-cov(eta.ka, eta.v))
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> ℹ remove covariance `(eta.ka, eta.v)`
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   0.45   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.v eta.cl eta.ka
#> eta.v   0.10   0.01   0.00
#> eta.cl  0.01   1.00   0.01
#> eta.ka  0.00   0.01   0.60
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
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.v ~ 0.1
#>         eta.cl ~ c(0.01, 1)
#>         eta.ka ~ c(0, 0.01, 0.6)
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl + eta.cl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```

### More granular access of initial conditions

Just like with
[`model()`](https://nlmixr2.github.io/rxode2/reference/model.md) you can
modify the underlying data frame that represents the
[`ini()`](https://nlmixr2.github.io/rxode2/reference/ini.md) block. In
this case I will simply change the initial estimate of the first
parameter (`tka`):

``` r
f <- rxode2(f)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments

ini <- f$iniDf

print(ini)
#>   ntheta neta1 neta2   name lower  est upper   fix label backTransform
#> 1      1    NA    NA    tka  -Inf 0.45   Inf FALSE  <NA>          <NA>
#> 2      2    NA    NA    tcl  -Inf 1.00   Inf FALSE  <NA>          <NA>
#> 3      3    NA    NA     tv  -Inf 3.45   Inf FALSE  <NA>          <NA>
#> 4      4    NA    NA add.sd     0 0.70   Inf FALSE  <NA>          <NA>
#> 5     NA     1     1 eta.ka  -Inf 0.60   Inf FALSE  <NA>          <NA>
#> 6     NA     2     2  eta.v  -Inf 0.10   Inf FALSE  <NA>          <NA>
#>   condition  err
#> 1      <NA> <NA>
#> 2      <NA> <NA>
#> 3      <NA> <NA>
#> 4        cp  add
#> 5        id <NA>
#> 6        id <NA>

ini$est[1] <- 7

ini(f) <- ini

print(f)
#>  ── rxode2-based free-form 2-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>    tka    tcl     tv add.sd 
#>   7.00   1.00   3.45   0.70 
#> 
#> Omega ($omega): 
#>        eta.ka eta.v
#> eta.ka    0.6   0.0
#> eta.v     0.0   0.1
#> 
#> States ($state or $stateDf): 
#>   Compartment Number Compartment Name
#> 1                  1            depot
#> 2                  2           center
#>  ── μ-referencing ($muRefTable): ──  
#>   theta    eta level
#> 1   tka eta.ka    id
#> 2    tv  eta.v    id
#> 
#>  ── Model (Normalized Syntax): ── 
#> function() {
#>     ini({
#>         tka <- 7
#>         tcl <- 1
#>         tv <- 3.45
#>         add.sd <- c(0, 0.7)
#>         eta.ka ~ 0.6
#>         eta.v ~ 0.1
#>     })
#>     model({
#>         ka <- exp(tka + eta.ka)
#>         cl <- exp(tcl)
#>         v <- exp(tv + eta.v)
#>         d/dt(depot) <- -ka * depot
#>         d/dt(center) <- ka * depot - cl/v * center
#>         cp <- center/v
#>         cp ~ add(add.sd)
#>     })
#> }
```
