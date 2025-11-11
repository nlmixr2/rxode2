# Create an ODE-based model specification

Create a dynamic ODE-based model object suitably for translation into
fast C code

## Usage

``` r
rxode2(
  model,
  modName = basename(wd),
  wd = getwd(),
  filename = NULL,
  extraC = NULL,
  debug = FALSE,
  calcJac = NULL,
  calcSens = NULL,
  collapseModel = FALSE,
  package = NULL,
  ...,
  linCmtSens = c("linCmtA", "linCmtB"),
  indLin = FALSE,
  verbose = FALSE,
  fullPrint = getOption("rxode2.fullPrint", FALSE),
  envir = parent.frame()
)

RxODE(
  model,
  modName = basename(wd),
  wd = getwd(),
  filename = NULL,
  extraC = NULL,
  debug = FALSE,
  calcJac = NULL,
  calcSens = NULL,
  collapseModel = FALSE,
  package = NULL,
  ...,
  linCmtSens = c("linCmtA", "linCmtB"),
  indLin = FALSE,
  verbose = FALSE,
  fullPrint = getOption("rxode2.fullPrint", FALSE),
  envir = parent.frame()
)

rxode(
  model,
  modName = basename(wd),
  wd = getwd(),
  filename = NULL,
  extraC = NULL,
  debug = FALSE,
  calcJac = NULL,
  calcSens = NULL,
  collapseModel = FALSE,
  package = NULL,
  ...,
  linCmtSens = c("linCmtA", "linCmtB"),
  indLin = FALSE,
  verbose = FALSE,
  fullPrint = getOption("rxode2.fullPrint", FALSE),
  envir = parent.frame()
)
```

## Arguments

- model:

  This is the ODE model specification. It can be:

  - a string containing the set of ordinary differential equations (ODE)
    and other expressions defining the changes in the dynamic system.

  - a file name where the ODE system equation is contained

  An ODE expression enclosed in `\{\}`

  (see also the `filename` argument). For details, see the sections
  “Details” and `rxode2 Syntax` below.

- modName:

  a string to be used as the model name. This string is used for naming
  various aspects of the computations, including generating C symbol
  names, dynamic libraries, etc. Therefore, it is necessary that
  `modName` consists of simple ASCII alphanumeric characters starting
  with a letter.

- wd:

  character string with a working directory where to create a
  subdirectory according to `modName`. When specified, a subdirectory
  named after the “`modName.d`” will be created and populated with a C
  file, a dynamic loading library, plus various other working files. If
  missing, the files are created (and removed) in the temporary
  directory, and the rxode2 DLL for the model is created in the current
  directory named `rx_????_platform`, for example
  `rx_129f8f97fb94a87ca49ca8dafe691e1e_i386.dll`

- filename:

  A file name or connection object where the ODE-based model
  specification resides. Only one of `model` or `filename` may be
  specified.

- extraC:

  Extra c code to include in the model. This can be useful to specify
  functions in the model. These C functions should usually take `double`
  precision arguments, and return `double` precision values.

- debug:

  is a boolean indicating if the executable should be compiled with
  verbose debugging information turned on.

- calcJac:

  boolean indicating if rxode2 will calculate the Jacobain according to
  the specified ODEs.

- calcSens:

  boolean indicating if rxode2 will calculate the sensitivities
  according to the specified ODEs.

- collapseModel:

  boolean indicating if rxode2 will remove all LHS variables when
  calculating sensitivities.

- package:

  Package name for pre-compiled binaries.

- ...:

  ignored arguments.

- linCmtSens:

  The method to calculate the linCmt() solutions

- indLin:

  Calculate inductive linearization matrices and compile with inductive
  linearization support.

- verbose:

  When `TRUE` be verbose with the linear compartmental model

- fullPrint:

  When using `printf` within the model, if `TRUE` print on every step
  (except ME/indLin), otherwise when `FALSE` print only when calculating
  the `d/dt`

- envir:

  is the environment to look for R user functions (defaults to parent
  environment)

## Value

An object (environment) of class `rxode2` (see Chambers and Temple Lang
(2001)) consisting of the following list of strings and functions:

    * `model` a character string holding the source model specification.
    * `get.modelVars`a function that returns a list with 3 character
        vectors, `params`, `state`, and `lhs` of variable names used in the model
        specification. These will be output when the model is computed (i.e., the ODE solved by integration).

      * `solve`{this function solves (integrates) the ODE. This
          is done by passing the code to [rxSolve()].
          This is as if you called `rxSolve(rxode2object, ...)`,
          but returns a matrix instead of a rxSolve object.

          `params`: a numeric named vector with values for every parameter
          in the ODE system; the names must correspond to the parameter
          identifiers used in the ODE specification;

          `events`: an `eventTable` object describing the
          input (e.g., doses) to the dynamic system and observation
          sampling time points (see  [eventTable()]);

          `inits`: a vector of initial values of the state variables
          (e.g., amounts in each compartment), and the order in this vector
          must be the same as the state variables (e.g., PK/PD compartments);


          `stiff`: a logical (`TRUE` by default) indicating whether
          the ODE system is stiff or not.

          For stiff ODE systems (`stiff = TRUE`), `rxode2` uses
          the LSODA (Livermore Solver for Ordinary Differential Equations)
          Fortran package, which implements an automatic method switching
          for stiff and non-stiff problems along the integration interval,
          authored by Hindmarsh and Petzold (2003).

          For non-stiff systems (`stiff = FALSE`), `rxode2` uses `DOP853`,
          an explicit Runge-Kutta method of order 8(5, 3) of Dormand and Prince
          as implemented in C by Hairer and Wanner (1993).

          `trans_abs`: a logical (`FALSE` by default) indicating
          whether to fit a transit absorption term
          (TODO: need further documentation and example);

          `atol`: a numeric absolute tolerance (1e-08 by default);

          `rtol`: a numeric relative tolerance (1e-06 by default).

          The output of \dQuote{solve} is a matrix with as many rows as there
          are sampled time points and as many columns as system variables
          (as defined by the ODEs and additional assignments in the rxode2 model
              code).}

      * `isValid` a function that (naively) checks for model validity,
          namely that the C object code reflects the latest model
          specification.
      * `version` a string with the version of the `rxode2`
          object (not the package).
      * `dynLoad` a function with one `force = FALSE` argument
          that dynamically loads the object code if needed.
      * `dynUnload` a function with no argument that unloads
          the model object code.
      * `delete` removes all created model files, including C and DLL files.
          The model object is no longer valid and should be removed, e.g.,
          `rm(m1)`.
      * `run` deprecated, use `solve`.
      * `get.index` deprecated.
      * `getObj` internal (not user callable) function.

## Details

The `Rx` in the name `rxode2` is meant to suggest the abbreviation *Rx*
for a medical prescription, and thus to suggest the package emphasis on
pharmacometrics modeling, including pharmacokinetics (PK),
pharmacodynamics (PD), disease progression, drug-disease modeling, etc.

The ODE-based model specification may be coded inside four places:

- Inside a `rxode2({})` block statements:

    library(rxode2)
    mod <- rxode2({
      # simple assignment
      C2 <- centr/V2

      # time-derivative assignment
      d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
    })

- Inside a `rxode2("")` string statement:

    mod <- rxode2("
      # simple assignment
      C2 <- centr/V2

      # time-derivative assignment
      d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
    ")

- In a file name to be loaded by rxode2:

    writeLines("
      # simple assignment
      C2 <- centr/V2

      # time-derivative assignment
      d/dt(centr) <- F*KA*depot - CL*C2 - Q*C2 + Q*C3;
    ", "modelFile.rxode2")
    mod <- rxode2(filename='modelFile.rxode2')
    unlink("modelFile.rxode2")

- In a model function which can be parsed by `rxode2`:

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

For the block statement, character string or text file an internal
`rxode2` compilation manager translates the ODE system into C, compiles
it and loads it into the R session. The call to `rxode2` produces an
object of class `rxode2` which consists of a list-like structure
(environment) with various member functions.

For the last type of model (a model function), a call to `rxode2`
creates a parsed `rxode2` ui that can be translated to the `rxode2`
compilation model.

    mod$simulationModel

    # or
    mod$simulationIniModel

This is the same type of function required for `nlmixr2` estimation and
can be extended and modified by model piping. For this reason will be
focused on in the documentation.

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

### Identifiers

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

|                                     |                                                                                                                                                                                                                              |                                                  |
|-------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------|
| Function                            | Description                                                                                                                                                                                                                  | Aliases                                          |
| gamma(x)                            | The Gamma function                                                                                                                                                                                                           | gammafn                                          |
| lgamma(x)                           | Natural logarithm of absolute value of gamma function                                                                                                                                                                        | digamma                                          |
| digamma(x)                          | First derivative of lgamma                                                                                                                                                                                                   |                                                  |
| trigamma(x)                         | Second derivative of lgamma                                                                                                                                                                                                  |                                                  |
| tetragamma(x)                       | Third derivative of lgamma                                                                                                                                                                                                   |                                                  |
| pentagamma(x)                       | Fourth derivative of lgamma                                                                                                                                                                                                  |                                                  |
| psigamma(x, deriv)                  | n-th derivative of Psi, the digamma function, which is the derivative of lgammafn. In other words, digamma(x) is the same as psigamma(x,0), trigamma(x) == psigamma(x,1), etc.                                               |                                                  |
| cospi(x)                            | cos(pi\*x)                                                                                                                                                                                                                   |                                                  |
| sinpi(x)                            | sin(pi\*x)                                                                                                                                                                                                                   |                                                  |
| tanpi(x)                            | tan(pi\*x)                                                                                                                                                                                                                   |                                                  |
| beta(a, b)                          | Beta function                                                                                                                                                                                                                |                                                  |
| lbeta(a, b)                         | log Beta function                                                                                                                                                                                                            |                                                  |
| bessel_i(x, nu, expo)               | Bessel function type I with index nu                                                                                                                                                                                         | expo==1 is unscaled expo==2 is scaled by exp(-x) |
| bessel_j(x, nu)                     | Bessel function type J with index nu                                                                                                                                                                                         |                                                  |
| bessel_k(x, ku, expo)               | Bessel function type K with index nu                                                                                                                                                                                         | expo==1 is unscaled expo==2 is scaled by exp(x)  |
| bessel_y(x, nu)                     | Bessel function type Y with index nu                                                                                                                                                                                         |                                                  |
| R_pow(x, y)                         | x^y                                                                                                                                                                                                                          |                                                  |
| R_pow_di(x, I)                      | x^y                                                                                                                                                                                                                          | y is an integer                                  |
| log1pmx                             | log(1+x) - x                                                                                                                                                                                                                 |                                                  |
| log1pexp                            | log(1+exp(x))                                                                                                                                                                                                                |                                                  |
| expm1(x)                            | exp(x)-1                                                                                                                                                                                                                     |                                                  |
| lgamma1p(x)                         | log(gamma(x+1))                                                                                                                                                                                                              |                                                  |
| sign(x)                             | Compute the signum function where sign(x) is 1, 0 -1                                                                                                                                                                         |                                                  |
| fsign(x, y)                         | abs(x)\*sign(y)                                                                                                                                                                                                              |                                                  |
| fprec(x, digits)                    | x rounded to digits (after the decimal point, used by signif()                                                                                                                                                               |                                                  |
| fround(x, digits)                   | Round, used by R’s round()                                                                                                                                                                                                   |                                                  |
| ftrunc(x)                           | Truncated towards zero                                                                                                                                                                                                       |                                                  |
| abs(x)                              | absolute value of x                                                                                                                                                                                                          | fabs                                             |
| sin(x)                              | sine of x                                                                                                                                                                                                                    |                                                  |
| cos(x)                              | cos of x                                                                                                                                                                                                                     |                                                  |
| tan(x)                              | tan of x                                                                                                                                                                                                                     |                                                  |
| factorial(x)                        | factorial of x                                                                                                                                                                                                               |                                                  |
| lfactorial(x)                       | log(factorial(x))                                                                                                                                                                                                            |                                                  |
| log10(x)                            | log base 10                                                                                                                                                                                                                  |                                                  |
| log2(x)                             | log base 2                                                                                                                                                                                                                   |                                                  |
| pnorm(x)                            | Normal CDF of x                                                                                                                                                                                                              | normcdf, phi                                     |
| qnorm(x)                            | Normal pdf of x                                                                                                                                                                                                              | norminv                                          |
| probit(x, low=0, hi=1)              | Probit (normal pdf) of x transforming into a range                                                                                                                                                                           |                                                  |
| probitInv(q, low=0, hi=1)           | Inverse probit of x transforming into a range                                                                                                                                                                                |                                                  |
| acos(x)                             | Inverse cosine                                                                                                                                                                                                               |                                                  |
| asin(x)                             | Inverse sine                                                                                                                                                                                                                 |                                                  |
| atan(x)                             | Inverse tangent                                                                                                                                                                                                              |                                                  |
| atan2(a, b)                         | Four quadrant inverse tangent                                                                                                                                                                                                |                                                  |
| sinh(x)                             | Hyperbolic sine                                                                                                                                                                                                              |                                                  |
| cosh(x)                             | Hyperbolic cosine                                                                                                                                                                                                            |                                                  |
| tanh(x)                             | Hyperbolic tangent                                                                                                                                                                                                           |                                                  |
| floor(x)                            | Downward rounding                                                                                                                                                                                                            |                                                  |
| ceil(x)                             | Upward rounding                                                                                                                                                                                                              |                                                  |
| logit(x, low=0, hi=1)               | Logit transformation of x transforming into a range                                                                                                                                                                          |                                                  |
| expit(x, low=0, hi=1)               | expit transofmration in range                                                                                                                                                                                                | invLogit, logitInv                               |
| gammaq(a, z)                        | Normalized incomplete gamma from boost                                                                                                                                                                                       |                                                  |
| gammaqInv(a, q)                     | Normalized incomplete gamma inverse from boost                                                                                                                                                                               |                                                  |
| ifelse(cond, trueValue, falseValue) | if else function                                                                                                                                                                                                             |                                                  |
| gammap(a, z)                        | Normalized lower incomplete gamma from boost                                                                                                                                                                                 |                                                  |
| gammapInv(a, p)                     | Inverse of Normalized lower incomplete gamma from boost                                                                                                                                                                      |                                                  |
| gammapInva(x, p)                    | Inverse of Normalized lower incomplete gamma from boost                                                                                                                                                                      |                                                  |
| rxnorm(x)                           | Generate one deviate of from a normal distribution for each observation scale                                                                                                                                                |                                                  |
| rxnormV(x)                          | Generate one deviate from low discrepancy normal for each observation                                                                                                                                                        |                                                  |
| rxcauchy                            | Generate one deviate from the cauchy distribution for each observation                                                                                                                                                       |                                                  |
| rxchisq                             | Generate one deviate from the chisq distribution for each observation                                                                                                                                                        |                                                  |
| rxexp                               | Generate one deviate from the exponential distribution for each observation                                                                                                                                                  |                                                  |
| rxf                                 | Generate one deviate from low discrepancy normal for each observation                                                                                                                                                        |                                                  |
| rxgamma                             | Generate one deviate from the gamma distribution for each observation                                                                                                                                                        |                                                  |
| rxbeta                              | Generate one deviate from the beta distribution for each observation                                                                                                                                                         |                                                  |
| rxgeom                              | Generate one deviate from the geometric distribution for each observation                                                                                                                                                    |                                                  |
| rxpois                              | Generate one deviate from the poission distribution for each observation                                                                                                                                                     |                                                  |
| rxt                                 | Generate one deviate from the t distribtuion for each observation                                                                                                                                                            |                                                  |
| tad() or tad(x)                     | Time after dose (tad()) or time after dose for a compartment tad(cmt); no dose=NA                                                                                                                                            |                                                  |
| tad0() or tad0(x)                   | Time after dose (tad0()) or time after dose for a compartment tad0(cmt); no dose=0                                                                                                                                           |                                                  |
| tafd() or tafd(x)                   | Time after first dose (tafd()) or time after first dose for a compartment tafd(cmt); no dose=NA                                                                                                                              |                                                  |
| tafd0() or tafd0(x)                 | Time after first dose (tafd()) or time after first dose for a compartment tafd(cmt); no dose=NA                                                                                                                              |                                                  |
| dosenum()                           | Dose Number                                                                                                                                                                                                                  |                                                  |
| tlast() or tlast(cmt)               | Time of Last dose; This takes into consideration any lag time, so if there is a dose at time 3 and a lag of 1, the time of last dose would be 4. tlast(cmt) calculates the time since last dose of a compartment; no dose=NA |                                                  |
| tlast0() or tlast0(cmt)             | Time of Last dose; This takes into consideration any lag time, so if there is a dose at time 3 and a lag of 1, the time of last dose would be 4. tlast(cmt) calculates the time since last dose of a compartment; no dose=0  |                                                  |
| tfirst() or tfirst(cmt)             | Time since first dose or time since first dose of a compartment; no dose=NA                                                                                                                                                  |                                                  |
| tfirst0() or tfirst0(cmt)           | Time since first dose or time since first dose of a compartment; no dose=0                                                                                                                                                   |                                                  |
| prod(…)                             | product of terms; This uses PreciseSums so the product will not have as much floating point errors (though it will take longer)                                                                                              |                                                  |
| sum(…)                              | sum of terms; This uses PreciseSums so the product will not have as much floating point errors (though it will take longer)                                                                                                  |                                                  |
| max(…)                              | maximum of a group of numbers                                                                                                                                                                                                |                                                  |
| min(…)                              | Min of a group of numbers                                                                                                                                                                                                    |                                                  |
| lag(parameter, number=1)            | Get the lag of an input parameter; You can specify a number of lagged observations                                                                                                                                           |                                                  |
| lead(parameter, number=2)           | Get the lead of an input parameter; You can specify a number of lead observation                                                                                                                                             |                                                  |
| diff(par, number=1)                 | Get the difference between the current parameter and the last parameter; Can change the parameter number                                                                                                                     |                                                  |
| first(par)                          | Get the first value of an input parameter                                                                                                                                                                                    |                                                  |
| last(par)                           | Get the last value of an input parameter                                                                                                                                                                                     |                                                  |
| transit()                           | The transit compartment psuedo function                                                                                                                                                                                      |                                                  |
| is.na()                             | Determine if a value is NA                                                                                                                                                                                                   |                                                  |
| is.nan()                            | Determine if a value is NaN                                                                                                                                                                                                  |                                                  |
| is.infinite()                       | Check to see if the value is infinite                                                                                                                                                                                        |                                                  |
| rinorm(x)                           | Generate one deviate of from a normal distribution for each individual                                                                                                                                                       |                                                  |
| rinormV(x)                          | Generate one deviate from low discrepancy normal for each individual                                                                                                                                                         |                                                  |
| ricauchy                            | Generate one deviate from the cauchy distribution for each individual                                                                                                                                                        |                                                  |
| richisq                             | Generate one deviate from the chisq distribution for each individual                                                                                                                                                         |                                                  |
| riexp                               | Generate one deviate from the exponential distribution for each individual                                                                                                                                                   |                                                  |
| rif                                 | Generate one deviate from low discrepancy normal for each individual                                                                                                                                                         |                                                  |
| rigamma                             | Generate one deviate from the gamma distribution for each individual                                                                                                                                                         |                                                  |
| ribeta                              | Generate one deviate from the beta distribution for each individual                                                                                                                                                          |                                                  |
| rigeom                              | Generate one deviate from the geometric distribution for each individual                                                                                                                                                     |                                                  |
| ropois                              | Generate one deviate from the poission distribution for each individual                                                                                                                                                      |                                                  |
| rit                                 | Generate one deviate from the t distribtuion for each individual                                                                                                                                                             |                                                  |
| simeps                              | Simulate EPS from possibly truncated sigma matrix. Will take sigma matrix from the current study. Simulated at the very last moment.                                                                                         |                                                  |
| simeta                              | Simulate ETA from possibly truncated omega matrix. Will take the omega matrix from the current study. Simulated at the initilization of the ODE system or the intialization of lhs                                           |                                                  |

Note that `lag(cmt) =` is equivalent to `alag(cmt) =` and not the same
as `= lag(wt)`

### Reserved keywords

There are a few reserved keywords in a rxode2 model. They are in the
following table:

|                |                                                                                                                                                                                   |        |
|----------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------|
| Reserved Name  | Meaning                                                                                                                                                                           | Alias  |
| time           | solver time                                                                                                                                                                       | t      |
| podo           | In Transit compartment models, last dose amount                                                                                                                                   |        |
| tlast          | Time of Last dose                                                                                                                                                                 |        |
| M_E            | Exp(1)                                                                                                                                                                            |        |
| M_LOG2E        | log2(e)                                                                                                                                                                           |        |
| M_LOG10E       | log10(e)                                                                                                                                                                          |        |
| M_LN2          | log(2)                                                                                                                                                                            |        |
| M_LN10         | log(10)                                                                                                                                                                           |        |
| M_PI           | pi                                                                                                                                                                                |        |
| M_PI_2         | pi/2                                                                                                                                                                              |        |
| M_PI_4         | pi/4                                                                                                                                                                              |        |
| M_1_PI         | 1/pi                                                                                                                                                                              |        |
| M_2_PI         | 2/pi                                                                                                                                                                              |        |
| M_2_SQRTPI     | 2/sqrt(pi)                                                                                                                                                                        |        |
| M_SQRT2        | sqrt(2)                                                                                                                                                                           |        |
| M_SQRT1_2      | 1/sqrt(2)                                                                                                                                                                         |        |
| M_SQRT_3       | sqrt(3)                                                                                                                                                                           |        |
| M_SQRT_32      | sqrt(32)                                                                                                                                                                          |        |
| M_LOG10_2      | Log10(2)                                                                                                                                                                          |        |
| M_2PI          | 2\*pi                                                                                                                                                                             |        |
| M_SQRT_PI      | sqrt(pi)                                                                                                                                                                          |        |
| M_1_SQRT_2PI   | 1/(sqrt(2\*pi))                                                                                                                                                                   |        |
| M_LN_SQRT_PI   | log(sqrt(pi))                                                                                                                                                                     |        |
| M_LN_SQRT_2PI  | log(sqrt(2\*pi))                                                                                                                                                                  |        |
| M_LN_SQRT_PId2 | log(sqrt(pi/2))                                                                                                                                                                   |        |
| pi             | pi                                                                                                                                                                                |        |
| NA             | R’s NA value                                                                                                                                                                      |        |
| NaN            | Not a Number Value                                                                                                                                                                |        |
| Inf            | Infinite Value                                                                                                                                                                    |        |
| newind         | 1: First record of individual; 2: Subsequent record of individual                                                                                                                 | NEWIND |
| rxFlag         | Flag for what part of the rxode2 model is being run; 1: ddt; 2: jac; 3: ini; 4: F; 5: lag; 6: rate; 7: dur; 8: mtime; 9: matrix exponential; 10: inductive linearization; 11: lhs |        |

Note that `rxFlag` will always output `11` or `calc_lhs` since that is
where the final variables are calculated, though you can tweak or test
certain parts of `rxode2` by using this flag.

### Residual functions when using rxode2 functions

In addition to `~` hiding output for certain types of output, it also is
used to specify a residual output or endpoint when the input is an
`rxode2` model function (that includes the residual in the `model({})`
block).

These specifications are of the form:

    var ~ add(add.sd)

Indicating the variable `var` is the variable that represents the
individual central tendencies of the model and it also represents the
compartment specification in the data-set.

You can also change the compartment name using the `|` syntax, that is:

    var ~ add(add.sd) | cmt

In the above case `var` represents the central tendency and `cmt`
represents the compartment or `dvid` specification.

#### Transformations

For normal and related distributions, you can apply the transformation
on both sides by using some keywords/functions to apply these
transformations.

|                |                                 |
|----------------|---------------------------------|
| Transformation | rxode2/nlmixr2 code             |
| Box-Cox        | +boxCox(lambda)                 |
| Yeo-Johnson    | +yeoJohnson(lambda)             |
| logit-normal   | +logitNorm(logit.sd, low, hi)   |
| probit-normal  | +probitNorm(probid.sd, low, hi) |
| log-normal     | +lnorm(lnorm.sd)                |

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

|                         |            |                              |
|-------------------------|------------|------------------------------|
| Distribution            | How to Add | Example                      |
| Normal (log-likelihood) | +dnorm()   | cc ~ add(add.sd) + dnorm()   |
| T-distribution          | +dt(df)    | cc ~a dd(add.sd) + dt(df)    |
| Cauchy (t with df=1)    | +dcauchy() | cc ~ add(add.sd) + dcauchy() |

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

y=f+\[sqrt(a^2+b^2 \*f^(2c))\]\*err

The default in `nlmixr2`/`rxode2` if not otherwise specified is
**combined2** since it mirrors how adding 2 normal distributions in
statistics will add their variances (not the standard deviations).
However, the **combined1** can describe the data possibly even better
than **combined2** so both are possible options in `rxode2`/`nlmixr2`.

#### Distributions of known likelihoods

For residuals that are not related to normal, t-distribution or cauchy,
often the residual specification is of the form:

    cmt ~ dbeta(alpha, beta)

Where the compartment specification is on the left handed side of the
specification.

For generalized likelihood you can specify:

    ll(cmt) ~ llik specification

#### Ordinal likelihoods

Finally, ordinal likelihoods/simulations can be specified in 2 ways. The
first is:

    err ~ c(p0, p1, p2)

Here `err` represents the compartment and `p0` is the probability of
being in a specific category:

|          |             |
|----------|-------------|
| Category | Probability |
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

    err ~ c(p0=0, p1=1, p2=2, 3)

Here the numeric categories are specified explicitly, and the
probabilities remain the same:

|          |             |
|----------|-------------|
| Category | Probability |
| 0        | p0          |
| 1        | p1          |
| 2        | p2          |
| 3        | 1-p0-p1-p2  |

#### General table of supported residual distributions

In general all the that are supported are in the following table
(available in
[`rxode2::rxResidualError`](https://nlmixr2.github.io/rxode2/reference/rxResidualError.md))

|                              |                 |                      |                                                                                                    |           |                                     |
|------------------------------|-----------------|----------------------|----------------------------------------------------------------------------------------------------|-----------|-------------------------------------|
| Error model                  | Functional Form | Transformation       | code                                                                                               | addProp   | lhs                                 |
| constant                     |                 | None                 | var ~ add(add.sd)                                                                                  |           | response variable                   |
| proportional                 |                 | None                 | var ~ prop(prop.sd)                                                                                |           | response variable                   |
| power                        |                 | None                 | var ~ pow(pow.sd, exponent)                                                                        |           | response variable                   |
| additive+proportional        | combined1       | None                 | var ~ add(add.sd) + prop(prop.sd) + combined1()                                                    | addProp=1 | response variable                   |
| additive+proportional        | combined2       | None                 | var ~ add(add.sd) + prop(prop.sd) + combined2()                                                    | addProp=2 | response variable                   |
| additive+power               | combined1       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + combined1()                                            | addProp=1 | response variable                   |
| additive+power               | combined2       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + combined2()                                            | addProp=2 | response variable                   |
| constant                     |                 | log                  | var ~ lnorm(add.sd)                                                                                |           | response variable                   |
| proportional                 |                 | log                  | var ~ lnorm(NA) + prop(prop.sd)                                                                    |           | response variable                   |
| power                        |                 | log                  | var ~ lnorm(NA) + pow(pow.sd, exponent)                                                            |           | response variable                   |
| additive+proportional        | combined1       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + combined1()                                                  | addProp=1 | response variable                   |
| additive+proportional        | combined2       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + combined2()                                                  | addProp=2 | response variable                   |
| additive+power               | combined1       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + combined1()                                          | addProp=1 | response variable                   |
| additive+power               | combined2       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + combined2()                                          | addProp=2 | response variable                   |
| constant                     |                 | boxCox               | var ~ boxCox(lambda) + add(add.sd)                                                                 |           | response variable                   |
| proportional                 |                 | boxCox               | var ~ boxCox(lambda) + prop(prop.sd)                                                               |           | response variable                   |
| power                        |                 | boxCox               | var ~ boxCox(lambda) + pow(pow.sd, exponent)                                                       |           | response variable                   |
| additive+proportional        | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + combined1()                                   | addProp=1 | response variable                   |
| additive+proportional        | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + combined2()                                   | addProp=2 | response variable                   |
| additive+power               | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + combined1()                           | addProp=1 | response variable                   |
| additive+power               | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + combined2()                           | addProp=2 | response variable                   |
| constant                     |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd)                                                             |           | response variable                   |
| proportional                 |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + prop(prop.sd)                                                           |           | response variable                   |
| power                        |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + pow(pow.sd, exponent)                                                   |           | response variable                   |
| additive+proportional        | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + combined1()                               | addProp=1 | response variable                   |
| additive+proportional        | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + combined2()                               | addProp=2 | response variable                   |
| additive+power               | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + combined1()                       | addProp=1 | response variable                   |
| additive+power               | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + combined2()                       | addProp=2 | response variable                   |
| constant                     |                 | logit                | var ~ logitNorm(logit.sd)                                                                          |           | response variable                   |
| proportional                 |                 | logit                | var ~ logitNorm(NA) + prop(prop.sd)                                                                |           | response variable                   |
| power                        |                 | logit                | var ~ logitNorm(NA) + pow(pow.sd, exponent)                                                        |           | response variable                   |
| additive+proportional        | combined1       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd)                                                          | addProp=1 | response variable                   |
| additive+proportional        | combined2       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd)                                                          | addProp=2 | response variable                   |
| additive+power               | combined1       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent)                                                  | addProp=1 | response variable                   |
| additive+power               | combined2       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent)                                                  | addProp=2 | response variable                   |
| additive                     |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd)                                                     |           | response variable                   |
| proportional                 |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + prop(prop.sd)                                           |           | response variable                   |
| power                        |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + pow(pow.sd, exponent)                                   |           | response variable                   |
| additive+proportional        | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd)                                     | addProp=1 | response variable                   |
| additive+proportional        | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd)                                     | addProp=2 | response variable                   |
| additive+power               | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent)                             | addProp=1 | response variable                   |
| additive+power               | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent)                             | addProp=2 | response variable                   |
| constant                     |                 | logit                | var ~ probitNorm(probit.sd)                                                                        |           | response variable                   |
| proportional                 |                 | probit               | var ~ probitNorm(NA) + prop(prop.sd)                                                               |           | response variable                   |
| power                        |                 | probit               | var ~ probitNorm(NA) + pow(pow.sd, exponent)                                                       |           | response variable                   |
| additive+proportional        | combined1       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + combined1()                                          | addProp=1 | response variable                   |
| additive+proportional        | combined2       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + combined2()                                          | addProp=2 | response variable                   |
| additive+power               | combined1       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + combined1()                                  | addProp=1 | response variable                   |
| additive+power               | combined2       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + combined2()                                  | addProp=2 | response variable                   |
| additive                     |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd)                                                   |           | response variable                   |
| proportional                 |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + prop(prop.sd)                                          |           | response variable                   |
| power                        |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + pow(pow.sd, exponent)                                  |           | response variable                   |
| additive+proportional        | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + combined1()                     | addProp=1 | response variable                   |
| additive+proportional        | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + combined2()                     | addProp=2 | response variable                   |
| additive+power               | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + combined1()             | addProp=1 | response variable                   |
| additive+power               | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + combined2()             | addProp=2 | response variable                   |
| constant+t                   |                 | None                 | var ~ add(add.sd) + dt(df)                                                                         |           | response variable                   |
| proportional+t               |                 | None                 | var ~ prop(prop.sd) + dt(df)                                                                       |           | response variable                   |
| power+t                      |                 | None                 | var ~ pow(pow.sd, exponent) + dt(df)                                                               |           | response variable                   |
| additive+proportional+t      | combined1       | None                 | var ~ add(add.sd) + prop(prop.sd) + dt(df) + combined1()                                           | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | None                 | var ~ add(add.sd) + prop(prop.sd) + dt(df) + combined2()                                           | addProp=2 | response variable                   |
| additive+power+t             | combined1       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + dt(df) +combined1()                                    | addProp=1 | response variable                   |
| additive+power+t             | combined2       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + dt(df) +combined2()                                    | addProp=2 | response variable                   |
| constant+t                   |                 | log                  | var ~ lnorm(add.sd) + dt(df)                                                                       |           | response variable                   |
| proportional+t               |                 | log                  | var ~ lnorm(NA) + prop(prop.sd) + dt(df)                                                           |           | response variable                   |
| power+t                      |                 | log                  | var ~ lnorm(NA) + pow(pow.sd, exponent) + dt(df)                                                   |           | response variable                   |
| additive+proportional+t      | combined1       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + dt(df) +combined1()                                          | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + dt(df) + combined2()                                         | addProp=2 | response variable                   |
| additive+power+t             | combined1       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + dt(df) + combined1()                                 | addProp=1 | response variable                   |
| additive+power+t             | combined2       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + dt(df) + combined2()                                 | addProp=2 | response variable                   |
| constant+t                   |                 | boxCox               | var ~ boxCox(lambda) + add(add.sd)+dt(df)                                                          |           | response variable                   |
| proportional+t               |                 | boxCox               | var ~ boxCox(lambda) + prop(prop.sd)+dt(df)                                                        |           | response variable                   |
| power+t                      |                 | boxCox               | var ~ boxCox(lambda) + pow(pow.sd, exponent)+dt(df)                                                |           | response variable                   |
| additive+proportional+t      | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + dt(df) + combined1()                          | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + dt(df) + combined2()                          | addProp=2 | response variable                   |
| additive+power+t             | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + dt(df) + combined1()                  | addProp=1 | response variable                   |
| additive+power+t             | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + dt(df) + combined2()                  | addProp=2 | response variable                   |
| constant+t                   |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + dt(df)                                                    |           | response variable                   |
| proportional+t               |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + prop(prop.sd) + dt(df)                                                  |           | response variable                   |
| power+t                      |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + pow(pow.sd, exponent) + dt(df)                                          |           | response variable                   |
| additive+proportional+t      | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + dt(df) + combined1()                      | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + dt(df) + combined2()                      | addProp=2 | response variable                   |
| additive+power+t             | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + dt(df) + combined1()              | addProp=1 | response variable                   |
| additive+power+t             | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + dt(df) + combined2()              | addProp=2 | response variable                   |
| constant+t                   |                 | logit                | var ~ logitNorm(logit.sd)+dt(df)                                                                   |           | response variable                   |
| proportional+t               |                 | logit                | var ~ logitNorm(NA) + prop(prop.sd)+dt(df)                                                         |           | response variable                   |
| power+t                      |                 | logit                | var ~ logitNorm(NA) + pow(pow.sd, exponent) + dt(df)                                               |           | response variable                   |
| additive+proportional+t      | combined1       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd) + dt(df) + combined1()                                   | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd) + dt(df) + combined2()                                   | addProp=2 | response variable                   |
| additive+power+t             | combined1       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent) + dt(df) + combined1()                           | addProp=1 | response variable                   |
| additive+power+t             | combined2       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent) + dt(df) + combined2()                           | addProp=2 | response variable                   |
| additive+t                   |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + dt(df)                                            |           | response variable                   |
| proportional+t               |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + prop(prop.sd) + dt(df)                                  |           | response variable                   |
| power+t                      |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + pow(pow.sd, exponent) + dt(df)                          |           | response variable                   |
| additive+proportional+t      | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd) + dt(df) + combined1()              | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd) + dt(df) + combined2()              | addProp=2 | response variable                   |
| additive+power+t             | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent) + dt(df) + combined1()      | addProp=1 | response variable                   |
| additive+power+t             | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent) + dt(df) + combined2()      | addProp=2 | response variable                   |
| constant+t                   |                 | logit                | var ~ probitNorm(probit.sd) + dt(df)                                                               |           | response variable                   |
| proportional+t               |                 | probit               | var ~ probitNorm(NA) + prop(prop.sd) + dt(df)                                                      |           | response variable                   |
| power+t                      |                 | probit               | var ~ probitNorm(NA) + pow(pow.sd, exponent) + dt(df)                                              |           | response variable                   |
| additive+proportional+t      | combined1       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + dt(df) + combined1()                                 | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + dt(df) + combined2()                                 | addProp=2 | response variable                   |
| additive+power+t             | combined1       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + dt(df) + combined1()                         | addProp=1 | response variable                   |
| additive+power+t             | combined2       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + dt(df) + combined2()                         | addProp=2 | response variable                   |
| additive+t                   |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + dt(df)                                          |           | response variable                   |
| proportional+t               |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + prop(prop.sd) + dt(df)                                 |           | response variable                   |
| power+t                      |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + pow(pow.sd, exponent) + dt(df)                         |           | response variable                   |
| additive+proportional+t      | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + dt(df) + combined1()            | addProp=1 | response variable                   |
| additive+proportional+t      | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + dt(df) + combined2()            | addProp=2 | response variable                   |
| additive+power+t             | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + dt(df) + combined1()    | addProp=1 | response variable                   |
| additive+power+t             | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + dt(df) +combined2()     | addProp=2 | response variable                   |
| constant+cauchy              |                 | None                 | var ~ add(add.sd) + dcauchy()                                                                      |           | response variable                   |
| proportional+cauchy          |                 | None                 | var ~ prop(prop.sd) + dcauchy()                                                                    |           | response variable                   |
| power+cauchy                 |                 | None                 | var ~ pow(pow.sd, exponent) + dcauchy()                                                            |           | response variable                   |
| additive+proportional+cauchy | combined1       | None                 | var ~ add(add.sd) + prop(prop.sd) + dcauchy() + combined1()                                        | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | None                 | var ~ add(add.sd) + prop(prop.sd) + dcauchy() + combined2()                                        | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + dcauchy() +combined1()                                 | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | None                 | var ~ add(add.sd) + pow(pow.sd, exponent) + dcauchy() +combined2()                                 | addProp=2 | response variable                   |
| constant+cauchy              |                 | log                  | var ~ lnorm(add.sd) + dcauchy()                                                                    |           | response variable                   |
| proportional+cauchy          |                 | log                  | var ~ lnorm(NA) + prop(prop.sd) + dcauchy()                                                        |           | response variable                   |
| power+cauchy                 |                 | log                  | var ~ lnorm(NA) + pow(pow.sd, exponent) + dcauchy()                                                |           | response variable                   |
| additive+proportional+cauchy | combined1       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + dcauchy() +combined1()                                       | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | log                  | var ~ lnorm(add.sd) + prop(prop.sd) + dcauchy() + combined2()                                      | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + dcauchy() + combined1()                              | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | log                  | var ~ lnorm(add.sd) + pow(pow.sd, exponent) + dcauchy() + combined2()                              | addProp=2 | response variable                   |
| constant+cauchy              |                 | boxCox               | var ~ boxCox(lambda) + add(add.sd)+dcauchy()                                                       |           | response variable                   |
| proportional+cauchy          |                 | boxCox               | var ~ boxCox(lambda) + prop(prop.sd)+dcauchy()                                                     |           | response variable                   |
| power+cauchy                 |                 | boxCox               | var ~ boxCox(lambda) + pow(pow.sd, exponent)+dcauchy()                                             |           | response variable                   |
| additive+proportional+cauchy | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + dcauchy() + combined1()                       | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + prop(prop.sd) + dcauchy() + combined2()                       | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + dcauchy() + combined1()               | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | boxCox               | var ~ boxCox(lambda) + add(add.sd) + pow(pop.sd, exponent) + dcauchy() + combined2()               | addProp=2 | response variable                   |
| constant+cauchy              |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + dcauchy()                                                 |           | response variable                   |
| proportional+cauchy          |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + prop(prop.sd) + dcauchy()                                               |           | response variable                   |
| power+cauchy                 |                 | yeoJohnson           | var ~ yeoJohnson(lambda) + pow(pow.sd, exponent) + dcauchy()                                       |           | response variable                   |
| additive+proportional+cauchy | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + dcauchy() + combined1()                   | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + prop(prop.sd) + dcauchy() + combined2()                   | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + dcauchy() + combined1()           | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | yeoJohnson           | var ~ yeoJohnson(lambda) + add(add.sd) + pow(pop.sd, exponent) + dcauchy() + combined2()           | addProp=2 | response variable                   |
| constant+cauchy              |                 | logit                | var ~ logitNorm(logit.sd)+dcauchy()                                                                |           | response variable                   |
| proportional+cauchy          |                 | logit                | var ~ logitNorm(NA) + prop(prop.sd)+dcauchy()                                                      |           | response variable                   |
| power+cauchy                 |                 | logit                | var ~ logitNorm(NA) + pow(pow.sd, exponent) + dcauchy()                                            |           | response variable                   |
| additive+proportional+cauchy | combined1       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd) + dcauchy() + combined1()                                | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | logit                | var ~ logitNorm(logit.sd) + prop(prop.sd) + dcauchy() + combined2()                                | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent) + dcauchy() + combined1()                        | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | logit                | var ~ logitNorm(logit.sd) + pow(pow.sd, exponent) + dcauchy() + combined2()                        | addProp=2 | response variable                   |
| additive+cauchy              |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + dcauchy()                                         |           | response variable                   |
| proportional+cauchy          |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + prop(prop.sd) + dcauchy()                               |           | response variable                   |
| power+cauchy                 |                 | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(NA) + pow(pow.sd, exponent) + dcauchy()                       |           | response variable                   |
| additive+proportional+cauchy | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd) + dcauchy() + combined1()           | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + prop(prop.sd) + dcauchy() + combined2()           | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent) + dcauchy() + combined1()   | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | yeoJohnson(logit())  | var ~ yeoJohnson(lambda) + logitNorm(logit.sd) + pow(pow.sd, exponent) + dcauchy() + combined2()   | addProp=2 | response variable                   |
| constant+cauchy              |                 | logit                | var ~ probitNorm(probit.sd) + dcauchy()                                                            |           | response variable                   |
| proportional+cauchy          |                 | probit               | var ~ probitNorm(NA) + prop(prop.sd) + dcauchy()                                                   |           | response variable                   |
| power+cauchy                 |                 | probit               | var ~ probitNorm(NA) + pow(pow.sd, exponent) + dcauchy()                                           |           | response variable                   |
| additive+proportional+cauchy | combined1       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + dcauchy() + combined1()                              | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | probit               | var ~ probitNorm(probit.sd) + prop(prop.sd) + dcauchy() + combined2()                              | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + dcauchy() + combined1()                      | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | probit               | var ~ probitNorm(probit.sd) + pow(pow.sd, exponent) + dcauchy() + combined2()                      | addProp=2 | response variable                   |
| additive+cauchy              |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + dcauchy()                                       |           | response variable                   |
| proportional+cauchy          |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + prop(prop.sd) + dcauchy()                              |           | response variable                   |
| power+cauchy                 |                 | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(NA) + pow(pow.sd, exponent) + dcauchy()                      |           | response variable                   |
| additive+proportional+cauchy | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + dcauchy() + combined1()         | addProp=1 | response variable                   |
| additive+proportional+cauchy | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + prop(prop.sd) + dcauchy() + combined2()         | addProp=2 | response variable                   |
| additive+power+cauchy        | combined1       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + dcauchy() + combined1() | addProp=1 | response variable                   |
| additive+power+cauchy        | combined2       | yeoJohnson(probit()) | var ~ yeoJohnson(lambda) + probitNorm(probit.sd) + pow(pow.sd, exponent) + dcauchy() +combined2()  | addProp=2 | response variable                   |
| poission                     |                 | none                 | cmt ~ dpois(lamba)                                                                                 |           | compartment specification           |
| binomial                     |                 | none                 | cmt ~ dbinom(n, p)                                                                                 |           | compartment specification           |
| beta                         |                 | none                 | cmt ~ dbeta(alpha, beta)                                                                           |           | compartment specification           |
| chisq                        |                 | none                 | cmt ~ dchisq(nu)                                                                                   |           | compartment specification           |
| exponential                  |                 | none                 | cmt ~ dexp(r)                                                                                      |           | compartment specification           |
| uniform                      |                 | none                 | cmt ~ dunif(a, b)                                                                                  |           | compartment specification           |
| weibull                      |                 | none                 | cmt ~ dweibull(a, b)                                                                               |           | compartment specification           |
| gamma                        |                 | none                 | cmt ~ dgamma(a, b)                                                                                 |           | compartment specification           |
| geometric                    |                 | none                 | cmt ~ dgeom(a)                                                                                     |           | compartment specification           |
| negative binomial form \#1   |                 | none                 | cmt ~ dnbinom(n, p)                                                                                |           | compartment specification           |
| negative binomial form \#2   |                 | none                 | cmt ~ dnbinomMu(size, mu)                                                                          |           | compartment specification           |
| ordinal probability          |                 | none                 | cmt ~ c(p0=0, p1=1, p2=2, 3)                                                                       |           | compartment specification           |
| log-likelihood               |                 | none                 | ll(cmt) ~ log likelihood expression                                                                |           | likelihood + compartment expression |

## Creating rxode2 models

NA

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

## References

Chamber, J. M. and Temple Lang, D. (2001) *Object Oriented Programming
in R*. R News, Vol. 1, No. 3, September 2001.
<https://cran.r-project.org/doc/Rnews/Rnews_2001-3.pdf>.

Hindmarsh, A. C. *ODEPACK, A Systematized Collection of ODE Solvers*.
Scientific Computing, R. S. Stepleman et al. (Eds.), North-Holland,
Amsterdam, 1983, pp. 55-64.

Petzold, L. R. *Automatic Selection of Methods for Solving Stiff and
Nonstiff Systems of Ordinary Differential Equations*. Siam J. Sci. Stat.
Comput. 4 (1983), pp. 136-148.

Hairer, E., Norsett, S. P., and Wanner, G. *Solving ordinary
differential equations I, nonstiff problems*. 2nd edition, Springer
Series in Computational Mathematics, Springer-Verlag (1993).

Plevyak, J. *`dparser`*, <https://dparser.sourceforge.net/>. Web. 12
Oct. 2015.

## See also

[`eventTable()`](https://nlmixr2.github.io/rxode2/reference/eventTable.md),
[`et()`](https://nlmixr2.github.io/rxode2/reference/et.md),
[`add.sampling()`](https://nlmixr2.github.io/rxode2/reference/add.sampling.md),
[`add.dosing()`](https://nlmixr2.github.io/rxode2/reference/add.dosing.md)

## Author

Melissa Hallow, Wenping Wang and Matthew Fidler

## Examples

``` r
# \donttest{

mod <- function() {
  ini({
    KA   <- .291
    CL   <- 18.6
    V2   <- 40.2
    Q    <- 10.5
    V3   <- 297.0
    Kin  <- 1.0
    Kout <- 1.0
    EC50 <- 200.0
  })
  model({
    # A 4-compartment model, 3 PK and a PD (effect) compartment
    # (notice state variable names 'depot', 'centr', 'peri', 'eff')
    C2 <- centr/V2
    C3 <- peri/V3
    d/dt(depot) <- -KA*depot;
    d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  <-                    Q*C2 - Q*C3;
    d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff;
    eff(0)      <- 1
  })
}

m1 <- rxode2(mod)
#>  
#>  
#> ℹ parameter labels from comments are typically ignored in non-interactive mode
#> ℹ Need to run with the source intact to parse comments
print(m1)
#>  ── rxode2-based free-form 4-cmt ODE model ────────────────────────────────────── 
#>  ── Initalization: ──  
#> Fixed Effects ($theta): 
#>      KA      CL      V2       Q      V3     Kin    Kout    EC50 
#>   0.291  18.600  40.200  10.500 297.000   1.000   1.000 200.000 
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
#>         KA <- 0.291
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
#>         d/dt(eff) <- Kin - Kout * (1 - C2/(EC50 + C2)) * eff
#>         eff(0) <- 1
#>     })
#> }

# Step 2 - Create the model input as an EventTable,
# including dosing and observation (sampling) events

# QD (once daily) dosing for 5 days.

qd <- et(amountUnits = "ug", timeUnits = "hours") %>%
  et(amt = 10000, addl = 4, ii = 24)

# Sample the system hourly during the first day, every 8 hours
# then after
qd <- qd %>% et(0:24) %>%
  et(from = 24 + 8, to = 5 * 24, by = 8)

# Step 3 - solve the system

qd.cp <- rxSolve(m1, qd)
#>  
#>  
#> using C compiler: ‘gcc (Ubuntu 13.3.0-6ubuntu2~24.04) 13.3.0’

head(qd.cp)
#>    time       C2        C3     depot    centr      peri      eff
#> 1 0 [h]  0.00000 0.0000000 10000.000    0.000    0.0000 1.000000
#> 2 1 [h] 43.99334 0.9113641  7475.157 1768.532  270.6751 1.083968
#> 3 2 [h] 54.50866 2.6510696  5587.797 2191.248  787.3677 1.179529
#> 4 3 [h] 51.65163 4.4243597  4176.966 2076.396 1314.0348 1.227523
#> 5 4 [h] 44.37513 5.9432612  3122.347 1783.880 1765.1486 1.233503
#> 6 5 [h] 36.46382 7.1389804  2334.004 1465.845 2120.2772 1.214084

# }
```
