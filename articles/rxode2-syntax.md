# rxode2 ODE solving syntax

    ## rxode2 4.1.1.9000 using 2 threads (see ?getRxThreads)
    ##   no cache: create with `rxCreateCache()`

### Introduction

This briefly describes the syntax used to define models that `rxode2`
will translate into R-callable compiled code. It also describes the
communication of variables between `R` and the `rxode2` modeling
specification.

### Creating rxode2 models

The ODE-based model specification may be coded inside four places:

- Inside a `rxode2({})` block statements:

``` r
library(rxode2)
mod <- rxode2({
  # simple assignment
  C2 <- centr/V2

  # time-derivative assignment
  d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
})
```

- Inside a `rxode2("")` string statement:

``` r
mod <- rxode2("
  # simple assignment
  C2 <- centr/V2

  # time-derivative assignment
  d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
")
```

- In a file name to be loaded by rxode2:

``` r
writeLines("
  # simple assignment
  C2 <- centr/V2

  # time-derivative assignment
  d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
", "modelFile.rxode2")
mod <- rxode2(filename='modelFile.rxode2')
unlink("modelFile.rxode2")
```

- In a model function which can be parsed by `rxode2`:

``` r
mod <- function() {
  model({
    # simple assignment
    C2 <- centr/V2

    # time-derivative assignment
    d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
  })
}

mod <- rxode2(mod) # or simply mod() if the model is at the end of the function

# These model functions often have residual components and initial
# (`ini({})`) conditions attached as well. For example the
# theophylline model can be written as:

one.compartment <- function() {
  ini({
    tka <- 0.45 # Log Ka
    tcl <- 1 # Log Cl
    tv <- 3.45    # Log V
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

# after parsing the model
mod <- one.compartment()
```

For the block statement, character string or text file an internal
`rxode2` compilation manager translates the ODE system into C, compiles
it and loads it into the R session. The call to `rxode2` produces an
object of class `rxode2` which consists of a list-like structure
(environment) with various member functions.

For the last type of model (a model function), a call to `rxode2`
creates a parsed `rxode2` ui that can be translated to the `rxode2`
compilation model.

``` r
mod$simulationModel

# or
mod$simulationIniModel
```

This is the same type of function required for `nlmixr2` estimation and
can be extended and modified by model piping. For this reason will be
focused on in the documentation.

### Syntax

This basic model specification consists of one or more statements
optionally terminated by semi-colons `;` and optional comments (comments
are delimited by `#` and an end-of-line).

A block of statements is a set of statements delimited by curly braces,
`{ ... }`.

Statements can be either assignments, conditional `if`/`else if`/`else`,
`while` loops (can be exited by
[`break`](https://rdrr.io/r/base/Control.html)), special statements, or
printing statements (for debugging/testing).

Assignment statements can be:

- **simple** assignments, where the left hand is an identifier (i.e.,
  variable). This includes string assignments

- special **time-derivative** assignments, where the left hand specifies
  the change of the amount in the corresponding state variable
  (compartment) with respect to time e.g., `d/dt(depot)`:

- special **initial-condition** assignments where the left hand
  specifies the compartment of the initial condition being specified,
  e.g. `depot(0) = 0`

- special model event changes including **bioavailability**
  (`f(depot)=1`), **lag time** (`alag(depot)=0`), **modeled rate**
  (`rate(depot)=2`) and **modeled duration** (`dur(depot)=2`). An
  example of these model features and the event specification for the
  modeled infusions the rxode2 data specification is found in [rxode2
  events
  vignette](https://nlmixr2.github.io/rxode2/articles/rxode2-event-types.html).

- special **change point syntax, or model times**. These model times are
  specified by `mtime(var)=time`

- special **Jacobian-derivative** assignments, where the left hand
  specifies the change in the compartment ode with respect to a
  variable. For example, if `d/dt(y) = dy`, then a Jacobian for this
  compartment can be specified as `df(y)/dy(dy) = 1`. There may be some
  advantage to obtaining the solution or specifying the Jacobian for
  very stiff ODE systems. However, for the few stiff systems we tried
  with LSODA, this actually slightly slowed down the solving.

- Special **string value declarations** which tell what values a string
  variable will take within a `rxode2` solving structure. These values
  will then cause a factor to be created for this variable on solving
  the `rxode2` model. As such, they are declared in much the same way as
  `R`, that is: `labels(a) <- c("a1", "a2")`.

Note that assignment can be done by `=`, `<-` or `~`.

When assigning with the `~` operator, the **simple assignments** and
**time-derivative** assignments will not be output. Note that with the
`rxode2` model functions assignment with `~` can also be overloaded with
a residual distribution specification.

Special statements can be:

- **Compartment declaration statements**, which can change the default
  dosing compartment and the assumed compartment number(s) as well as
  add extra compartment names at the end (useful for multiple-endpoint
  nlmixr models); These are specified by `cmt(compartmentName)`

- **Parameter declaration statements**, which can make sure the input
  parameters are in a certain order instead of ordering the parameters
  by the order they are parsed. This is useful for keeping the parameter
  order the same when using 2 different ODE models. These are specified
  by `param(par1, par2,...)`

- **Variable interpolation statements**, which tells the interpolation
  for specific covariates. These include `locf(cov1, cov2, ...)` for
  last observation carried forward, `nocb(cov1, cov2, ...)` for next
  observation carried backward, `linear(cov1, cov2, ...)` for linear
  interpolation and `midpoint(cov1, cov2, ...)` for midpoint
  interpolation.

An example model is shown below:

       # simple assignment
       C2 <- centr/V2

       # time-derivative assignment
       d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;

Expressions in assignment and `if` statements can be numeric or logical.

Numeric expressions can include the following numeric operators
`+, -, *, /, ^` and those mathematical functions defined in the C or the
R math libraries (e.g., `fabs`, `exp`, `log`, `sin`, `abs`).

You may also access the R’s functions in the [R math
libraries](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Numerical-analysis-subroutines),
like `lgammafn` for the log gamma function.

The `rxode2` syntax is case-sensitive, i.e., `ABC` is different than
`abc`, `Abc`, `ABc`, etc.

#### Identifiers

Like R, Identifiers (variable names) may consist of one or more
alphanumeric, underscore `_` or period `.` characters, but the first
character cannot be a digit or underscore `_`.

Identifiers in a model specification can refer to:

- State variables in the dynamic system (e.g., compartments in a
  pharmacokinetics model).
- Implied input variable, `t` (time), `tlast` (last time point), and
  `podo` (oral dose, in the undocumented case of absorption transit
  models).
- Special constants like `pi` or [R’s predefined
  constants](https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Mathematical-constants).
- Model parameters (e.g., `ka` rate of absorption, `CL` clearance, etc.)
- Others, as created by assignments as part of the model specification;
  these are referred as *LHS* (left-hand side) variable.

Currently, the `rxode2` modeling language only recognizes system state
variables and “parameters”, thus, any values that need to be passed from
R to the ODE model (e.g., `age`) should be either passed in the `params`
argument of the integrator function
[`rxSolve()`](https://nlmixr2.github.io/rxode2/reference/rxSolve.md) or
be in the supplied event data-set.

There are certain variable names that are in the `rxode2` event tables.
To avoid confusion, the following event table-related items cannot be
assigned, or used as a state but can be accessed in the rxode2 code:

- `cmt`
- `dvid`
- `addl`
- `ss`
- `amt`
- `dur`
- `rate`
- `Rprintf`
- `print`
- `printf`
- `id`

However the following variables are cannot be used in a model
specification:

- `evid`
- `ii`

Sometimes rxode2 generates variables that are fed back to rxode2.
Similarly, nlmixr2 generates some variables that are used in nlmixr
estimation and simulation. These variables start with the either the
`rx` or `nlmixr` prefixes. To avoid any problems, it is suggested to not
use these variables starting with either the `rx` or `nlmixr` prefixes.

### Logical Operators

Logical operators support the standard R operators `==`, `!=` `>=` `<=`
`>` and `<`. Like R these can be in `if()` or `while()` statements,
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) expressions.
Additionally they can be in a standard assignment. For instance, the
following is valid:

    cov1 = covm*(sexf == "female") + covm*(sexf != "female")

Notice that you can also use character expressions in comparisons. This
convenience comes at a cost since character comparisons are slower than
numeric expressions. Unlike R, `as.numeric` or `as.integer` for these
logical statements is not only not needed, but will cause an syntax
error if you try to use the function.

### Supported functions

All the supported functions in rxode2 can be seen with the
[`rxSupportedFuns()`](https://nlmixr2.github.io/rxode2/reference/rxSupportedFuns.md).

A brief description of the built-in functions are in the following
table:

Note that `lag(cmt) =` is equivalent to `alag(cmt) =` and not the same
as `= lag(wt)`

### Reserved keywords

There are a few reserved keywords in a rxode2 model. They are in the
following table:

Note that `rxFlag` will always output `11` or `calc_lhs` since that is
where the final variables are calculated, though you can tweak or test
certain parts of `rxode2` by using this flag.

### Residual functions when using rxode2 functions

In addition to `~` hiding output for certain types of output, it also is
used to specify a residual output or endpoint when the input is an
`rxode2` model function (that includes the residual in the `model({})`
block).

These specifications are of the form:

``` r
var ~ add(add.sd)
```

Indicating the variable `var` is the variable that represents the
individual central tendencies of the model and it also represents the
compartment specification in the data-set.

You can also change the compartment name using the `|` syntax, that is:

``` r
var ~ add(add.sd) | cmt
```

In the above case `var` represents the central tendency and `cmt`
represents the compartment or `dvid` specification.

#### Transformations

For normal and related distributions, you can apply the transformation
on both sides by using some keywords/functions to apply these
transformations.

By default for the likelihood for all of these transformations is
calculated on the **untransformed** scale.

For bounded variables like logit-normal or probit-normal the low and
high values are defaulted to 0 and 1 if missing.

For models where you wish to have a proportional model on one of these
transformation you can replace the standard deviation with `NA`

To allow for more transformations, `lnorm()`, `probitNorm()` and
`logitNorm()` can be combined the variance stabilizing
[`yeoJohnson()`](https://nlmixr2.github.io/rxode2/reference/boxCox.md)
transformation.

#### Normal and t-related distributions

For the normal and t-related distributions, we wanted to keep the
ability to use skewed distributions additive and proportional in the
t/cauchy-space, so these distributions are specified differently in
comparison to the other supported distributions within `nlmixr2`:

Note that with the normal and t-related distributions `nlmixr2` will
calculate `cwres` and `npde` under the normal assumption to help assess
the goodness of the fit of the model.

Also note that the `+dnorm()` is mostly for testing purposes and will
slow down the estimation procedure in `nlmixr2`. We suggest not adding
it (except for explicit testing). When there are multiple endpoint
models that mix non-normal and normal distributions, the whole problem
is shifted to a log-likelihood method for estimation in `nlmixr2`.

#### Notes on additive + proportional models

There are two different ways to specify additive and proportional
models, which we will call **combined1** and **combined2**, the same way
that Monolix calls the two distributions (to avoid between software
differences in naming).

The first, **combined1**, assumes that the additive and proportional
differences are on the standard deviation scale, or:

y=f+(a+b\* f^c)\*err

The second, **combined2**, assumes that the additive and proportional
differences are combined on a variance scale:

y=f+$$sqrt\left( a^{2} + b^{2}*f^{(2c)} \right)$$\*err

The default in `nlmixr2`/`rxode2` if not otherwise specified is
**combined2** since it mirrors how adding 2 normal distributions in
statistics will add their variances (not the standard deviations).
However, the **combined1** can describe the data possibly even better
than **combined2** so both are possible options in `rxode2`/`nlmixr2`.

#### Distributions of known likelihoods

For residuals that are not related to normal, t-distribution or cauchy,
often the residual specification is of the form:

``` r
cmt ~ dbeta(alpha, beta)
```

Where the compartment specification is on the left handed side of the
specification.

For generalized likelihood you can specify:

``` r
ll(cmt) ~ llik specification
```

#### Ordinal likelihoods

Finally, ordinal likelihoods/simulations can be specified in 2 ways. The
first is:

``` r
err ~ c(p0, p1, p2)
```

Here `err` represents the compartment and `p0` is the probability of
being in a specific category:

| Category | Probability |
|----------|-------------|
| 1        | p0          |
| 2        | p1          |
| 3        | p2          |
| 4        | 1-p0-p1-p2  |

It is up to the model to ensure that the sum of the `p` values are less
than `1`. Additionally you can write an arbitrary number of categories
in the ordinal model described above.

It seems a little off that `p0` is the probability for category `1` and
sometimes scores are in non-whole numbers. This can be modeled as
follows:

``` r
err ~ c(p0=0, p1=1, p2=2, 3)
```

Here the numeric categories are specified explicitly, and the
probabilities remain the same:

| Category | Probability |
|----------|-------------|
| 0        | p0          |
| 1        | p1          |
| 2        | p2          |
| 3        | 1-p0-p1-p2  |

#### General table of supported residual distributions

In general all the that are supported are in the following table
(available in
[`rxode2::rxResidualError`](https://nlmixr2.github.io/rxode2/reference/rxResidualError.md))

## Note on strings in rxode2

Strings are converted to double values inside of `rxode2`, hence you can
refer to them as an integer corresponding to the string value or the
string value itself. For covariates these are calculated on the fly
based on your data and you should likely not try this, though you should
be aware. For strings defined in the model, this is fixed and both could
be used.

For example:

    if (APGAR == 10 || APGAR == 8 || APGAR == 9) {
        tAPGAR <- "High"
      } else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {
        tAPGAR <- "Low"
      } else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {
        tAPGAR <- "Med"
      } else {
        tAPGAR<- "Med"
      }

Could also be replaced by:

    if (APGAR == 10 || APGAR == 8 || APGAR == 9) {
        tAPGAR <- "High"
      } else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {
        tAPGAR <- "Low"
      } else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {
        tAPGAR <- "Med"
      } else {
        tAPGAR<- 3
      }

Since `"Med"` is already defined

If you wanted you can pre-declare what levels it has (and the order) to
give you better control of this:

    levels(tAPGAR) <- c("Med", "Low", "High")
    if (APGAR == 10 || APGAR == 8 || APGAR == 9) {
        tAPGAR <- 3
      } else if (APGAR == 1 || APGAR == 2 || APGAR == 3) {
        tAPGAR <- 2
      } else if (APGAR == 4 || APGAR == 5 || APGAR == 6 || APGAR == 7) {
        tAPGAR <- 1
      } else {
        tAPGAR<- 1
      }

You can see that the number changed since the declaration change the
numbers in each variable for `tAPGAR`. These
[`levels()`](https://rdrr.io/r/base/levels.html) statements need to be
declared before the variable occurs to ensure the numbering is
consistent with what is declared.

### Bugs and/or deficiencies

- The modulo operator `%%` is currently unsupported.

### Note

The ODE specification mini-language is parsed with the help of the open
source tool , Plevyak (2015).

### Example model

Below is a commented example to quickly show the capabilities of
`rxode2` syntax.

### Example

``` r
f <- function() {
  ini({

  })
  model({
    # An rxode2 model specification (this line is a comment).

    if(comed==0) {  # concomitant medication (con-med)?
      F <- 1.0     # full bioavailability w.o. con-med
    } else {
      F <- 0.80    # 20% reduced bioavailability
    }

    C2 <- centr/V2  # concentration in the central compartment
    C3 <- peri/V3   # concentration in the peripheral compartment

    # ODE describing the PK and PD

    d/dt(depot) <- -KA*depot
    d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3
    d/dt(peri)  <-                      Q*C2 - Q*C3
    d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
    eff(0)      <- 1
  })
}
```

### Interface and data handling between R and the generated C code

Users specify which variables are the dynamic system’s state variables
via the `d/dt(identifier)` operator as part of the model specification,
and which are model parameters via the `params=` argument in `rxode2`
[`solve()`](https://rdrr.io/pkg/symengine/man/solve.html) method:

``` r
m1 <- rxode2(model = ode, modName = "m1")

# model parameters -- a named vector is required
theta <-
   c(KA=0.29, CL=18.6, V2=40.2, Q=10.5, V3=297, Kin=1, Kout=1, EC50=200)

# state variables and their amounts at time 0 (the use of names is
# encouraged, but not required)
inits <- c(depot=0, centr=0, peri=0, eff=1)

# qd1 is an eventTable specification with a set of dosing and sampling
# records (code not shown here)

solve(theta, event = qd1, inits = inits)
```

The values of these variables at pre-specified time points are saved
during model fitting/integration and returned as part of the fitted
values (see the function
[`et()`](https://nlmixr2.github.io/rxode2/reference/et.md), to define a
set of time points when to capture the values of these variables) and
returned as part of the modeling output.

The ODE specification mini-language is parsed with the help of the open
source tool *DParser*, Plevyak (2015).
