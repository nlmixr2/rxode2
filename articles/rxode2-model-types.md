# rxode2 additional model types

As suggested in the name, rxode2 is often concerned with solutions to
ordinary differential equations. The syntax of the ODE models is covered
in the [rxode2 syntax
vignette](https://nlmixr2.github.io/rxode2/articles/rxode2-syntax.md)

You can create other types of models with rxode2:

- Prediction only models without ODE systems in them (`$PRED` models in
  NONMEM).
- 1, 2 and 3 solved compartment models (`ADVAN/TRANS` in NONMEM).
- Mixing any of these items with ODE systems.

## Prediction only models

Prediction only models are simple to create. You use the rxode2 syntax
without any ODE systems in them. A very simple example is a
one-compartment model.

``` r
library(rxode2)
```

    ## rxode2 4.1.1.9000 using 2 threads (see ?getRxThreads)
    ##   no cache: create with `rxCreateCache()`

``` r
mod <- function(){
  model({
    ipre <- 10 * exp(-ke * t)
  })
}
```

Solving the rxode2 models are the same as saving the simple ODE system,
but faster of course.

``` r
et  <- et(seq(0,24,length.out=50))
cmt1 <- rxSolve(mod,et,params=c(ke=0.5))
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

    ## using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

``` r
cmt1
```

    ## -- Solved rxode2 object --
    ## -- Parameters (x$params): --
    ##  ke 
    ## 0.5 
    ## -- Initial Conditions (x$inits): --
    ## named numeric(0)
    ## -- First part of data (object): --
    ## # A tibble: 50 x 2
    ##    time  ipre
    ##   <dbl> <dbl>
    ## 1 0     10   
    ## 2 0.490  7.83
    ## 3 0.980  6.13
    ## 4 1.47   4.80
    ## 5 1.96   3.75
    ## 6 2.45   2.94
    ## # i 44 more rows

## Solved compartment models

Solved models are also simple to create. You simply place the `linCmt()`
pseudo-function into your code. The `linCmt()` function figures out the
type of model to use based on the parameter names specified.

Most often, pharmacometric models are parameterized in terms of volume
and clearances. Clearances are specified by NONMEM-style names of `CL`,
`Q`, `Q1`, `Q2`, etc. or distributional clearances `CLD`, `CLD2`.
Volumes are specified by Central (`VC` or `V`), Peripheral/Tissue (`VP`,
`VT`). While more translations are available, some example translations
are below:

Another popular parameterization is in terms of micro-constants. rxode2
assumes compartment `1` is the central compartment. The elimination
constant would be specified by `K`, `Ke` or `Kel`. Some example
translations are below:

The last parameterization possible is using `alpha` and `V` and/or
`A`/`B`/`C`. Some example translations are below:

Once the `linCmt()` sleuthing is complete, the `1`, `2` or `3`
compartment model solution is used as the value of `linCmt()`. After the
linear `linCmt()` models the following variables/state values will
become available (depenting on the type of compartment model): `depot`,
`central`, `peripheral1` and `peripheral2`

The compartments where you can dose in a linear solved system are
`depot` and `central` when there is an linear absorption constant in the
model `ka`. Without any additional ODEs, these compartments are numbered
`depot=1` and `central=2`.

When the absorption constant `ka` is missing, you may only dose to the
`central` compartment. Without any additional ODEs the compartment
number is `central=1`.

The peripheral compartments may be dosed to as well, though their
numbers depend on if they are mixed with the ODEs. The `central` and
`depot` compartment can have infusions, and all of the departments can
have non-infusion doses.

These compartments take the same sort of events that a ODE model can
take, and are discussed in the [rxode2 events
vignette](https://nlmixr2.github.io/rxode2/articles/rxode2-events.md).

``` r
mod <- function() {
  ini({
    kel <- 0.5
    V <- 1
  })
  model({
    ipre <- linCmt(V, kel)
  })
}
```

This then acts as an ODE model; You specify a dose to the depot
compartment and then solve the system:

``` r
et  <- et(amt=10,time=0,cmt=depot) %>%
  et(seq(0,24,length.out=50))
cmt1 <- rxSolve(mod,et)
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

    ## using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

    ## Warning: dose to compartment 2 ignored (not in system; 'id=1')

``` r
cmt1
```

    ## -- Solved rxode2 object --
    ## -- Parameters (x$params): --
    ## kel   V 
    ## 0.5 1.0 
    ## -- Initial Conditions (x$inits): --
    ## central 
    ##       0 
    ## -- First part of data (object): --
    ## # A tibble: 50 x 3
    ##    time  ipre central
    ##   <dbl> <dbl>   <dbl>
    ## 1 0         0       0
    ## 2 0.490     0       0
    ## 3 0.980     0       0
    ## 4 1.47      0       0
    ## 5 1.96      0       0
    ## 6 2.45      0       0
    ## # i 44 more rows

## Mixing Solved Systems and ODEs

In addition to pure ODEs, you may mix solved systems and ODEs. The prior
2-compartment indirect response model can be simplified with a
`linCmt()` function:

``` r
library(rxode2)
## Setup example model
mod1 <-function() {
  model({
    C2 = centr/V2
    C3 = peri/V3
    d/dt(depot) =-KA*depot
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  =                    Q*C2 - Q*C3
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff
  })
}

## Seup parameters and initial conditions

theta <-
    c(KA=2.94E-01, CL=1.86E+01, V2=4.02E+01, # central
      Q=1.05E+01,  V3=2.97E+02,              # peripheral
      Kin=1, Kout=1, EC50=200)               # effects

inits <- c(eff=1)

## Setup dosing event information
ev <- et(amountUnits="mg", timeUnits="hours") %>%
    et(amt=10000, addl=9, ii=12) %>%
    et(amt=20000, addl=4, time=120, ii=24) %>%
    add.sampling(0:240)

## Setup a mixed solved/ode system:
mod2 <- function() {
  model({
    ## the order of variables do not matter, the type of compartmental
    ## model is determined by the parameters specified.
    C2   = linCmt(KA, CL, V2, Q, V3);
    eff(0) = 1  ## This specifies that the effect compartment starts at 1.
    d/dt(eff) =  Kin - Kout*(1-C2/(EC50+C2))*eff;
  })
}
```

This allows the indirect response model above to assign the
2-compartment model to the `C2` variable and the used in the indirect
response model.

When mixing the solved systems and the ODEs, the solved system’s
compartment is always the last compartment. This is because the solved
system technically isn’t a compartment to be solved. Adding the dosing
compartment to the end will not interfere with the actual ODE to be
solved.

Therefore,in the two-compartment indirect response model, the effect
compartment is compartment \#1 while the PK dosing compartment for the
depot is compartment \#2.

This compartment model requires a new event table since the compartment
number changed:

``` r
ev <- et(amountUnits='mg', timeUnits='hours') %>%
  et(amt=10000, addl=9, ii=12, cmt=2) %>%
  et(amt=20000, addl=4, time=120, ii=24, cmt=2) %>%
  et(0:240)
```

This can be solved with the following command:

``` r
x <- mod2 %>%  solve(theta, ev)
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

    ## using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

``` r
print(x)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ##      KA      CL      V2       Q      V3     Kin    Kout    EC50 
    ##   0.294  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
    ## -- Initial Conditions ($inits): --
    ##         eff       depot     central peripheral1 
    ##           1           0           0           0 
    ## -- First part of data (object): --
    ## # A tibble: 241 x 6
    ##   time    C2   eff depot central peripheral1
    ##    [h] <dbl> <dbl> <dbl>   <dbl>       <dbl>
    ## 1    0 249.   1        0  10000           0 
    ## 2    1 121.   1.35     0   4877.       1825.
    ## 3    2  60.3  1.38     0   2424.       2659.
    ## 4    3  31.0  1.28     0   1248.       3018.
    ## 5    4  17.0  1.18     0    683.       3152.
    ## 6    5  10.2  1.11     0    411.       3178.
    ## # i 235 more rows

Note this solving did not require specifying the effect compartment
initial condition to be `1`. Rather, this is already pre-specified by
`eff(0)=1`.

This can be solved for different initial conditions easily:

``` r
x <- mod2 %>%  solve(theta, ev,c(eff=2))
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

``` r
print(x)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ##      KA      CL      V2       Q      V3     Kin    Kout    EC50 
    ##   0.294  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
    ## -- Initial Conditions ($inits): --
    ##         eff       depot     central peripheral1 
    ##           2           0           0           0 
    ## -- First part of data (object): --
    ## # A tibble: 241 x 6
    ##   time    C2   eff depot central peripheral1
    ##    [h] <dbl> <dbl> <dbl>   <dbl>       <dbl>
    ## 1    0 249.   2        0  10000           0 
    ## 2    1 121.   1.93     0   4877.       1825.
    ## 3    2  60.3  1.67     0   2424.       2659.
    ## 4    3  31.0  1.41     0   1248.       3018.
    ## 5    4  17.0  1.23     0    683.       3152.
    ## 6    5  10.2  1.13     0    411.       3178.
    ## # i 235 more rows

The rxode2 detective also does not require you to specify the variables
in the `linCmt()` function if they are already defined in the block.
Therefore, the following function will also work to solve the same
system.

``` r
mod3 <- function() {
  ini({
    KA <- 2.94E-01
    CL <- 1.86E+01
    V2 <- 4.02E+01
    Q <- 1.05E+01
    V3 <- 2.97E+02
    Kin <- 1
    Kout <- 1
    EC50 <- 200
  })
  model({
    # Since the parameters are in the ini block, put them in linCmt so
    # that the model is detected correctly
    C2   <- linCmt(KA, CL, V2, Q, V3)
    eff(0) <- 1  ## This specifies that the effect compartment starts at 1.
    d/dt(eff) <-  Kin - Kout*(1-C2/(EC50+C2))*eff;
  })
}

x <- mod3 %>%  solve(ev)
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

``` r
print(x)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ##      KA      CL      V2       Q      V3     Kin    Kout    EC50 
    ##   0.294  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
    ## -- Initial Conditions ($inits): --
    ##         eff       depot     central peripheral1 
    ##           1           0           0           0 
    ## -- First part of data (object): --
    ## # A tibble: 241 x 6
    ##   time    C2   eff depot central peripheral1
    ##    [h] <dbl> <dbl> <dbl>   <dbl>       <dbl>
    ## 1    0 249.   1        0  10000           0 
    ## 2    1 121.   1.35     0   4877.       1825.
    ## 3    2  60.3  1.38     0   2424.       2659.
    ## 4    3  31.0  1.28     0   1248.       3018.
    ## 5    4  17.0  1.18     0    683.       3152.
    ## 6    5  10.2  1.11     0    411.       3178.
    ## # i 235 more rows

Note that you do not specify the parameters when solving the system
since they are built into the model, but you can override the
parameters:

``` r
x <- mod3 %>%  solve(c(KA=10),ev)
```

    ## i parameter labels from comments are typically ignored in non-interactive mode

    ## i Need to run with the source intact to parse comments

``` r
print(x)
```

    ## -- Solved rxode2 object --
    ## -- Parameters ($params): --
    ##    KA    CL    V2     Q    V3   Kin  Kout  EC50 
    ##  10.0  18.6  40.2  10.5 297.0   1.0   1.0 200.0 
    ## -- Initial Conditions ($inits): --
    ##         eff       depot     central peripheral1 
    ##           1           0           0           0 
    ## -- First part of data (object): --
    ## # A tibble: 241 x 6
    ##   time    C2   eff depot central peripheral1
    ##    [h] <dbl> <dbl> <dbl>   <dbl>       <dbl>
    ## 1    0 249.   1        0  10000           0 
    ## 2    1 121.   1.35     0   4877.       1825.
    ## 3    2  60.3  1.38     0   2424.       2659.
    ## 4    3  31.0  1.28     0   1248.       3018.
    ## 5    4  17.0  1.18     0    683.       3152.
    ## 6    5  10.2  1.11     0    411.       3178.
    ## # i 235 more rows
