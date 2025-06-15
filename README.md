
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rxode2

<!-- badges: start -->

![Cran updating
status](https://img.shields.io/badge/CRAN-Not%20Updating-green)
[![R-CMD-check](https://github.com/nlmixr2/rxode2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nlmixr2/rxode2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nlmixr2/rxode2/graph/badge.svg)](https://app.codecov.io/gh/nlmixr2/rxode2)
[![CRAN
version](http://www.r-pkg.org/badges/version/rxode2)](https://cran.r-project.org/package=rxode2)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/grand-total/rxode2)](https://cran.r-project.org/package=rxode2)
[![CRAN total
downloads](https://cranlogs.r-pkg.org/badges/rxode2)](https://cran.r-project.org/package=rxode2)
[![CodeFactor](https://www.codefactor.io/repository/github/nlmixr2/rxode2/badge)](https://www.codefactor.io/repository/github/nlmixr2/rxode2)
![r-universe](https://nlmixr2.r-universe.dev/badges/rxode2)
<!-- badges: end -->

## Overview

**rxode2** is an R package for solving and simulating from ode-based
models. These models are convert the rxode2 mini-language to C and
create a compiled dll for fast solving. ODE solving using rxode2 has a
few key parts:

- `rxode2()` which creates the C code for fast ODE solving based on a
  [simple
  syntax](https://nlmixr2.github.io/rxode2/articles/rxode2-syntax.html)
  related to Leibnitz notation.
- The event data, which can be:
  - a `NONMEM` or `deSolve` [compatible data
    frame](https://nlmixr2.github.io/rxode2/articles/rxode2-event-types.html),
    or
  - created with `et()` or `eventTable()` for [easy simulation of
    events](https://nlmixr2.github.io/rxode2/articles/rxode2-event-table.html)
  - The data frame can be augmented by adding
    [time-varying](https://nlmixr2.github.io/rxode2/articles/rxode2-covariates.html#time-varying-covariates)
    or adding [individual
    covariates](https://nlmixr2.github.io/rxode2/articles/rxode2-covariates.html#individual-covariates)
    (`iCov=` as needed)
- `rxSolve()` which solves the system of equations using initial
  conditions and parameters to make predictions
  - With multiple subject data, [this may be
    parallelized](https://nlmixr2.github.io/rxode2/articles/rxode2-speed.html).
  - With single subject the [output data frame is
    adaptive](https://nlmixr2.github.io/rxode2/articles/rxode2-data-frame.html)
  - Covariances and other metrics of uncertanty can be used to [simulate
    while
    solving](https://nlmixr2.github.io/rxode2/articles/rxode2-sim-var.html)

## Installation

You can install the released version of rxode2 from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rxode2")
```

The fastest way to install the development version of `rxode2` is to use
the `r-universe` service. This service compiles binaries of the
development version for MacOS and for Windows so you don’t have to wait
for package compilation:

``` r
install.packages(c("dparser", "rxode2ll", "rxode2"),
                 repos=c(nlmixr2="https://nlmixr2.r-universe.dev",
                         CRAN="https://cloud.r-project.org"))
```

If this doesn’t work you install the development version of rxode2 with

``` r
devtools::install_github("nlmixr2/rxode2ll")
devtools::install_github("nlmixr2/rxode2")
```

To build models with rxode2, you need a working c compiler. To use
parallel threaded solving in rxode2, this c compiler needs to support
open-mp.

You can check to see if R has working c compiler you can check with:

``` r
## install.packages("pkgbuild")
pkgbuild::has_build_tools(debug = TRUE)
```

If you do not have the toolchain, you can set it up as described by the
platform information below:

### Windows

In windows you may simply use installr to install rtools:

``` r
install.packages("installr")
library(installr)
install.rtools()
```

Alternatively you can
[download](https://cran.r-project.org/bin/windows/Rtools/) and install
rtools directly.

### Mac OSX

To get the most speed you need OpenMP enabled and compile rxode2 with
that compiler. There are various options and the most up to date
discussion about this is likely the [data.table installation FAQ for
MacOS](https://github.com/Rdatatable/data.table/wiki/Installation#openmp-enabled-compiler-for-mac).
The last thing to keep in mind is that `rxode2` uses the code very
similar to the original `lsoda` which requires the `gfortran` compiler
to be setup as well as the `OpenMP` compilers.

If you are going to be using `rxode2` and `nlmixr` together and have an
older mac computer, I would suggest trying the following:

``` r
library(symengine)
```

If this crashes your R session then the binary does not work with your
Mac machine. To be able to run nlmixr, you will need to compile this
package manually. I will proceed assuming you have `homebrew` installed
on your system.

On your system terminal you will need to install the dependencies to
compile `symengine`:

``` sh
brew install cmake gmp mpfr libmpc
```

After installing the dependencies, you need to re-install `symengine`:

``` r
install.packages("symengine", type="source")
library(symengine)
```

### Linux

To install on linux make sure you install `gcc` (with openmp support)
and `gfortran` using your distribution’s package manager.

You will also have to install system dependencies like `udunits` and the
[symengine
dependencies](https://github.com/symengine/symengine.R?tab=readme-ov-file#installation)
for the complete installation to work in linux. You could also have this
done by system packages in your package manager if you add the
appropriate repositories. This is the point of the
[r2u](https://github.com/eddelbuettel/r2u) project.

### R versions 4.0 and 4.1

For installation on R versions 4.0.x and 4.1.x, please see the
instructions on how to install `symengine` in the `nlmixr2` installation
instructions:
<https://github.com/nlmixr2/nlmixr2#r-package-installation>

## Development version

Since the development version of rxode2 uses StanHeaders, you will need
to make sure your compiler is setup to support C++14, as described in
the [rstan setup
page](https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started#configuration-of-the-c-toolchain).
For R 4.0, I do not believe this requires modifying the windows
toolchain any longer (so it is much easier to setup).

Once the C++ toolchain is setup appropriately, you can install the
development version from [GitHub](https://github.com/nlmixr2/rxode2)
with:

``` r
# install.packages("devtools")
devtools::install_github("nlmixr2/rxode2ll")
devtools::install_github("nlmixr2/rxode2")
```

# Illustrated Example

The model equations can be specified through a text string, a model file
or an R expression. Both differential and algebraic equations are
permitted. Differential equations are specified by `d/dt(var_name) =`.
Each equation can be separated by a semicolon.

To load `rxode2` package and compile the model:

``` r
library(rxode2)
#> rxode2 3.0.4.9000 using 8 threads (see ?getRxThreads)

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
```

Model parameters may be specified in the `ini({})` model block, initial
conditions can be specified within the model with the `cmt(0)= X`, like
in this model `eff(0) <- 1`.

You may also specify between subject variability initial conditions and
residual error components just like nlmixr2. This allows a single
interface for `nlmixr2`/`rxode2` models. Also note, the classic `rxode2`
interface still works just like it did in the past (so don’t worry about
breaking code at this time).

In fact, you can get the classic `rxode2` model `$simulationModel` in
the ui object:

``` r
mod1 <- mod1() # create the ui object (can also use `rxode2(mod1)`)
mod1

summary(mod1$simulationModel)
```

## Specify Dosing and sampling in rxode2

`rxode2` provides a simple and very flexible way to specify dosing and
sampling through functions that generate an event table. First, an empty
event table is generated through the “et()” function. This has an
interface that is similar to NONMEM event tables:

``` r
ev  <- et(amountUnits="mg", timeUnits="hours") %>%
  et(amt=10000, addl=9,ii=12,cmt="depot") %>%
  et(time=120, amt=2000, addl=4, ii=14, cmt="depot") %>%
  et(0:240) # Add sampling 
```

You can see from the above code, you can dose to the compartment named
in the rxode2 model. This slight deviation from NONMEM can reduce the
need for compartment renumbering.

These events can also be combined and expanded (to multi-subject events
and complex regimens) with `rbind`, `c`, `seq`, and `rep`. For more
information about creating complex dosing regimens using rxode2 see the
[rxode2 events
vignette](https://nlmixr2.github.io/rxode2/articles/rxode2-event-types.html).

## Solving ODEs

The ODE can now be solved using `rxSolve`:

``` r
x <- mod1 %>% rxSolve(ev)
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
#> → creating rxode2 include directory
#> → getting R compile options
#> → precompiling headers
#> ✔ done
#> using C compiler: 'gcc.exe (GCC) 14.2.0'
x
#> ── Solved rxode2 object ──
#> ── Parameters (x$params): ──
#>      KA      CL      V2       Q      V3     Kin    Kout    EC50 
#>   0.294  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
#> ── Initial Conditions (x$inits): ──
#> depot centr  peri   eff 
#>     0     0     0     1 
#> ── First part of data (object): ──
#> # A tibble: 241 × 7
#>   time    C2    C3  depot centr  peri   eff
#>    [h] <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
#> 1    0   0   0     10000     0     0   1   
#> 2    1  44.4 0.920  7453. 1784.  273.  1.08
#> 3    2  54.9 2.67   5554. 2206.  794.  1.18
#> 4    3  51.9 4.46   4140. 2087. 1324.  1.23
#> 5    4  44.5 5.98   3085. 1789. 1776.  1.23
#> 6    5  36.5 7.18   2299. 1467. 2132.  1.21
#> # ℹ 235 more rows
```

This returns a modified data frame. You can see the compartment values
in the plot below:

``` r
library(ggplot2)
plot(x,C2) + ylab("Central Concentration")
```

<img src="man/figures/README-intro-central-1.png" width="100%" />

Or,

``` r
plot(x,eff)  + ylab("Effect")
```

<img src="man/figures/README-intro-effect-1.png" width="100%" />

Note that the labels are automatically labeled with the units from the
initial event table. rxode2 extracts `units` to label the plot (if they
are present).

# Related R Packages

## ODE solving

This is a brief comparison of pharmacometric ODE solving R packages to
`rxode2`.

There are several [R packages for differential
equations](https://CRAN.R-project.org/view=DifferentialEquations). The
most popular is [deSolve](https://CRAN.R-project.org/package=deSolve).

However for pharmacometrics-specific ODE solving, there are only 2
packages other than [rxode2](https://cran.r-project.org/package=rxode2)
released on CRAN. Each uses compiled code to have faster ODE solving.

- [mrgsolve](https://cran.r-project.org/package=mrgsolve), which uses
  C++ lsoda solver to solve ODE systems. The user is required to write
  hybrid R/C++ code to create a mrgsolve model which is translated to
  C++ for solving.

  In contrast, `rxode2` has a R-like mini-language that is parsed into C
  code that solves the ODE system.

  Unlike `rxode2`, `mrgsolve` does not currently support symbolic
  manipulation of ODE systems, like automatic Jacobian calculation or
  forward sensitivity calculation (`rxode2` currently supports this and
  this is the basis of
  [nlmixr2](https://CRAN.R-project.org/package=nlmixr2)’s FOCEi
  algorithm)

- [dMod](https://CRAN.R-project.org/package=dMod), which uses a unique
  syntax to create “reactions”. These reactions create the underlying
  ODEs and then created c code for a compiled deSolve model.

  In contrast `rxode2` defines ODE systems at a lower level. `rxode2`’s
  parsing of the mini-language comes from C, whereas `dMod`’s parsing
  comes from R.

  Like `rxode2`, `dMod` supports symbolic manipulation of ODE systems
  and calculates forward sensitivities and adjoint sensitivities of
  systems.

  Unlike `rxode2`, `dMod` is not thread-safe since `deSolve` is not yet
  thread-safe.

- [PKPDsim](https://github.com/InsightRX/PKPDsim) which defines models
  in an R-like syntax and converts the system to compiled code.

  Like `mrgsolve`, `PKPDsim` does not currently support symbolic
  manipulation of ODE systems.

  `PKPDsim` is not thread-safe.

The open pharmacometrics open source community is fairly friendly, and
the rxode2 maintainers has had positive interactions with all of the
ODE-solving pharmacometric projects listed.

## PK Solved systems

`rxode2` supports 1-3 compartment models with gradients (using stan
math’s auto-differentiation). This currently uses the same equations as
`PKADVAN` to allow time-varying covariates.

`rxode2` can mix ODEs and solved systems.

### The following packages for solved PK systems are on CRAN

- [mrgsolve](https://cran.r-project.org/package=mrgsolve) currently has
  1-2 compartment (poly-exponential models) models built-in. The solved
  systems and ODEs cannot currently be mixed.

- [pmxTools](https://github.com/kestrel99/pmxTools) currently have 1-3
  compartment (super-positioning) models built-in. This is a R-only
  implementation.

- [PKPDsim](https://github.com/InsightRX/PKPDsim) uses 1-3 “ADVAN”
  solutions using non-superpositioning.

- [PKPDmodels](https://CRAN.R-project.org/package=PKPDmodels) has a
  one-compartment model with gradients.

### Non-CRAN libraries:

- [PKADVAN](https://github.com/abuhelwa/PKADVAN_Rpackage) Provides 1-3
  compartment models using non-superpositioning. This allows
  time-varying covariates.
