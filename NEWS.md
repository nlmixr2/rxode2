# rxode2 (development version)

## New features

- Allow `ini(model) <- NULL` to drop ini block and `as.ini(NULL)`
  gives `ini({})` (Issue #523)

- Add a function `modelExtract()` to extract model lines to allow
  modifying them and then changing the model by piping or simply
  assigning the modified lines with `model(ui) <- newModifiedLines`

- Add Algebraic mu-referencing detection (mu2) that allows you to
  express mu-referenced covariates as:

``` r
cl <- exp(tcl + eta.cl + wt_cl * log(WT/70.5))
```

Instead of the

``` r
cl <- exp(tcl + eta.cl + wt_cl * log.WT.div.70.5)
```

That was previously required (where `log.WT.div.70.5` was calculated
in the data) for mu expressions.  The `ui` now has more information to
allow transformation of data internally and transformation to the old
mu-referencing style to run the optimization.

## Internal new features

- Add `as.model()` for list expressions, which implies `model(ui) <-
  ui$lstExpr` will assign model components.  It will also more
  robustly work with character vectors

# rxode2 2.0.13

# Bug fixes

- A bug was fixed so that the `zeroRe()` function works with correlated omega
  values.

- A bug was fixed so that the `rename()` function works with initial
  conditions for compartments (`cmt(0)`)

## New features

- A new function `zeroRe()` allows simple setting of omega and/or sigma values
  to zero for a model (#456)

- Diagonal zeros in the `omega` and `sigma` matrices are treated as
  zeros in the model. The corresponding `omega` and `sigma` matrices
  drop columns/rows where the diagonals are zero to create a new
  `omega` and `sigma` matrix for simulation.  This is the same idiom
  that NONMEM uses for simulation from these matrices.

- Add the ability to pipe model estimates from another model by
  `parentModel %>% ini(modelWithNewEsts)`

- Add the ability to append model statements with piping using `%>%
  model(x=3, append=d/dt(depot))`, still supports appending with
  `append=TRUE` and pre-pending with `append=NA` (the default is to
  replace lines with `append=FALSE`)

- rxSolve's keep argument will now maintain character and factor classes from
  input data with the same class (#190)

- Parameter labels may now be modified via `ini(param = label("text"))` (#351).

- Parameter order may be modified via the `append` argument to `ini()`
  when piping a model.  For example, `ini(param = 1, append = 0)` or
  `ini(param = label("text"), append = "param2")` (#352).

## Internal changes

- If lower/upper bounds are outside the required bounds, the
  adjustment is displayed.

- When initial values are piped that break the model's boundary
  condition reset the boundary to unbounded and message which boundary
  was reset.

- Added `as.rxUi()` function to convert the following objects to
  `rxUi` objects: `rxode2`, `rxModelVars`, `function`.  Converting
  nlmixr2 fits to `rxUi` will be placed in the `s3` method in the
  corresponding package.

- `assertRxUi(x)` now uses `as.rxUi()` so that it can be extended
  outside of `rxode2`/`nlmixr2`.

- `rxode2` now supports `addl` with `ss` doses

- Moved `rxDerived` to `rxode2parse` (and re-exported it here).

- Added test for transit compartment solving in absence of dosing to the
  transit compartment (fixed in `rxode2parse` but solving tested
  here)

- Using `ini()` without any arguments on a `rxode2` type function will
  return the `ini()` block.  Also added a method `ini(mod) <-
  iniBlock` to modify the `ini` block is you wish.  `iniBlock` should
  be an expression.

- Using `model()` without any arguments on a `rxode2` type function
  will return the `model()` block.  Also added a new method
  `model(mod) <- modelBlock`

- Added a new method `rxode2(mod) <- modFunction` which allows
  replacing the function with a new function while maintaining the
  meta information about the ui (like information that comes from
  `nonmem2rx` models).  The `modFunction` should be the body of the
  new function, the new function, or a new `rxode2` ui.

- `rxode2` ui objects now have a `$sticky` item inside the internal
  (compressed) environment.  This `$sticky` tells what variables to
  keep if there is a "significant" change in the ui during piping or
  other sort of model change.  This is respected during model piping,
  or modifying the model with `ini(mod)<-`, `model(mod)<-`,
  `rxode2(mod)<-`.  A significant change is a change in the model
  block, a change in the number of estimates, or a change to the value
  of the estimates.  Estimate bounds, weather an estimate is fixed or
  estimate label changes are not considered significant.

- Added `as.ini()` method to convert various formats to an ini
  expression.  It is used internally with `ini(mod)<-`.  If you want to
  assign something new that you can convert to an ini expression, add
  a method for `as.ini()`.

- Added `as.model()` method to convert various formats to a model
  expression.  It is used internally with `model(mod)<-`.  If you want to
  assign something new that you can convert to a model expression, add
  a method for `as.model()`.

# rxode2 2.0.11

- Give a more meaningful error for 'rxode2' ui models with only error
  expressions

- Break the ABI requirement between `roxde2()` and `rxode2parse()`

- The new `rxode2parse` will fix the `sprintf` exclusion shown on CRAN.

# rxode2 2.0.10

- Time invariant covariates can now contain 'NA' values.

- When a column has 'NA' for the entire id, now 'rxode2' warns about
  both the id and column instead of just the id.

- To fix some CRAN issues in 'nlmixr2est', make the version dependency
  explicit.

# rxode2 2.0.9

- Remove log likelihoods from 'rxode2' to reduce compilation time and
  increase maintainability of 'rxode2'. They were transferred to
  'rxode2ll' (requested by CRAN).

- Remove the parsing from 'rxode2' and solved linear compartment code
  and move to 'rxode2parse' to reduce the compilation time (as requested
  by CRAN).

- Remove the random number generation from 'rxode2' and move to
  'rxode2random' to reduce the compilation time (as requested by
  CRAN).

- Remove the event table translation and generation from 'rxode2' and
  move to 'rxode2et' to reduce the compilation time (as requested by
  CRAN).

- Change the `rxode2` ui object so it is a compressed, serialized
  object by default.  This could reduce the `C stack size` problem
  that occurs with too many environments in R.

- Warn when ignoring items during simulations

- Export a method to change 'rxode2' solve methods into internal integers

- Bug fix for time invariant covariates identified as time variant
  covariate when the individual's time starts after `0`.

# rxode2 2.0.8

## Breaking changes

- `rxgamma` now only allows a `rate` input.  This aligns with the
  internal `rxode2` version of `rxgamma` and clarifies how this will
  be used. It is also aligned with the `llikGamma` function used for
  generalized likelihood estimation.

- ui `cauchy` simulations now follow the ui for `normal` and `t`
  distributions, which means you can combine with transformations.
  This is because the `cauchy` is a `t` distribution with one degree
  of freedom.

- ui `dnorm()` and `norm()` are no longer equivalent to `add()`.  Now
  it allows you to use the loglik `llikNorm()` instead of the standard
  `nlmixr2` style focei likelihood.  This is done by adding `dnorm()`
  at the end of the line.  It also means `dnorm()` now doesn't take
  any arguments.

- Vandercorput normal removed (non-random number generator)

## New features

- Allow models in the `nlmixr2` form without an `ini({})` block

- Allow model piping of an omega matrix by `f %>% ini(omegaMatrix)`

- Standard models created with `rxode2()` can no be piped into a model function

- Families of log-likelihood were added to `rxode2` so that mixed
  likelihood nonlinear mixed effects models may be specified and run.

- The memory footprint of a `rxode2` solving has been reduced

- Piping now allow named strings (issue #249)

## Bug fixes

- `rxode2`'s symengine would convert `sqrt(2)` to `M_SQRT_2` when it
  should be `M_SQRT2`.  This has been fixed; it was most noticeable in
  nlmixr2 log-likelihood estimation methods

- `rxode2` treats `DV` as a non-covariate with `etTran` (last time it
  would duplicate if it is in the model).  This is most noticeable in
  the nlmixr2 log-likelihood estimation methods.

## New features

- A new flag (`rxFlag`) has been created to tell you where in the
  `rxode2` solving process you are.  This is useful for debugging. If
  outputting this variable it will always be `11` or calculating the
  left handed equations.  If you are using in conjunction with the
  `printf()` methods, it is a double variable and should be formatted
  with `"%f"`.

- An additional option of `fullPrint` has been added to `rxode2()`
  which allows `rprintf()` to be used in almost all of `rxode2()`
  steps (inductive linearization and matrix exponential are the
  exception here) instead of just the integration `ddt` step.  It
  defaults to `FALSE`.

# rxode2 2.0.7

- Removed accidental `^S` from news as requested by CRAN.

- Bug fix for more complicated mu-referencing.

- Change rxode2 md5 to only depend on the C/C++/Fortran code and
  headers not the R files.  That way if there is binary compatibility
  between `nlmixr2est` and `rxode2`, a new version of `nlmixr2est`
  will not need to be submitted to CRAN.

# rxode2 2.0.6

## Breaking changes

### Solving controls

* The options for `rxControl` and `rxSolve` are more strict.
  `camelCase` is now always used.  Old options like `add.cov` and
  `transit_abs` are no longer supported, only `addCov` is supported.

* A new option, `sigdig` has been added to `rxControl()`, which
  controls some of the more common significant figure options like
  `atol`, `rtol`, `ssAtol`, `ssRtol`, with a single option.

### Simulations

* For simulations, `$simulationSigma` now assumes a diagonal matrix.
  The sigma values are assumed to be standard normal, and uncorrelated
  between endpoints.  Simulation with uncertainty will still draw from
  this identity diagonal matrix

* Parallel solving now seeds each simulation per each individual based
    on the initial seed plus the simulation id.  This makes the
    simulation reproducible regardless of the number of cores running
    the simulation.

### Other breaking changes

* Solved objects now access the underlying rxode model with `$rxode2`
  instead of `$rxode`

* Since this change names, `rxode2`, `rxode` and `RxODE` all perform
  the same function.

* Options were changed from `RxODE.syntax` to `rxode2.syntax`.

* Assigning states with `rxode2.syntax.assign.state` (was
  `RxODE.syntax.assign.state`) is no longer supported.

* Enforcing "pure" assignment syntax with `=` syntax is no longer
  supported so `rxode2.syntax.assign` is no longer supported (was
  `RxODE.syntax.assign`).

* Since R supports `**` as an exponentiation operator, the pure syntax
  without `**` can no longer be enabled. Hence
  `rxode2.syntax.star.pow` (was `RxODE.syntax.star.pow`) no longer has
  any effect.

* The "pure" syntax that requires a semicolon can no longer be
  enabled.  Therefore `rxode2.syntax.require.semicolon` (was
  `RxODE.syntax.require.semicolon`) no longer has any effect.

* The syntax `state(0)` can no longer be turned
  off. `rxode2.syntax.allow.ini0` (was `RxODE.syntax.allow.ini0`) has
  been removed.

* Variable with dots in variable and state names like
  `state.name` works in R. Therefore, "pure" syntax of excluding `.` values
  from variables cannot be enforced with `rxode2.syntax.allow.dots`
  (was `RxODE.syntax.allow.dots`).

* The mnemonic `et(rate=model)` and `et(dur=model)` mnemonics have
  been removed.  `rate` needs to be set to `-1` and `-2` manually instead.

* The function `rxode2Test()` has been removed in favor of using testthat
  directly.

* Transit compartments need to use a new `evid`, `evid=7`.  That being
  said, the `transitAbs` option is no longer supported.

* `ID` columns in input parameter data frames are not sorted or merged
  with original dataset any more; The underlying assumption of ID
  order should now be checked outside of `rxode2()`.  Note that the
  event data frame is still sorted.

## Additional features

* The UI functions of `nlmixr` have been ported to work in `rxode2`
  directly.

* `rxModelVars({})` is now supported.

* You may now combine 2 models in `rxode2` with `rxAppendModel()`. In
  fact, as long as the first value is a rxode2 evaluated ui model, you can
  use  `c`/`rbind` to bind 2 or more models together.

* You may now append model lines with piping using `%>% model(lines,
  append=TRUE)` you can also pre-pend lines by `%>% model(lines,
  append=NA)`

* You may now rename model variables, states and defined parameters
  with `%>% rxRename(new=old)` or if `dplyr` is loaded: `%>%
  rename(new=old)`

* You can fix parameters with `%>% ini(tcl=fix)` or `%>% ini(fix(tcl))` as well as unfix parameters with
  `%>% ini(tcl=unfix)` or `%>% ini(unfix(tcl))`

## Internal changes

* Strict R headers are enforced more places

* Since there are many changes that could be incompatible, this
  version has been renamed to `rxode2`

* `rxode2()` printout no longer uses rules and centered headings to
  make it display better on a larger variety of systems.

## Bug fixes

* `tad()` and related time features only reset at the start of an
  infusion (as opposed to starting at the beginning and end of an
  infusion)

# RxODE 1.1.3

* Change handling of missing covariates while interpolating "nocb" so
  that the time-varying covariates use "nocb" interpolation (#469)

# RxODE 1.1.2

* Fix subject initialization of `focei` problem (#464)

* Fix LHS offset to allow internal threading and more parallel
  processing in the future.

* Remove warnings for duration and rate

* Don't export pillar methods any more (simply register at load if present)

* As requested by CRAN, change fortran and C binding for BLAS an LINPACK

# RxODE 1.1.1

* Fix the LTO issue that CRAN identified.

* Move the omp files so they come first to support clang13, as identified by CRAN.

* For now, be a little more conservative in `dur()` and `rate()`
  warnings because `linCmt()` models in `nlmixr` currently produce
  irrelevant warnings.

# RxODE 1.1.0

* Always calculate "nolhs" for using numeric differences when the
  inner problem. This allows the inner problem to fallback to a finite
  difference approximation to the focei objective function.

* Updated the parser C code grammar using latest dparser CRAN package

* Added a new cbind function that is used to mix data frame input with
  simulated individual parameters and residual parameters,
  `rxCbindStudyIndividual()`.

* Now data frame input can be mixed with simulating from omega and
  sigma matrices (though not yet in nested simulations)

* Race conditions when simulating random numbers is solved by chunking
  each simulation into groups that will always be performed per each
  thread.  This way the simulation is now reproducible regardless of
  load.  Because of the chunking, simulations with random numbers generated
  inside of it are now threaded by default (though a warning is
  produced about the simulation only be reproducible when run with the
  same number of threads)

* Simulations were double checked and made sure to use the engine
  reserved for each core run in parallel; Some of the random
  generators were not taking random numbers from the correct engine,
  which was corrected.  Therefore, simulations from this version are
  expected to be different (in parallel) than previous versions.

* Added function `rxSetSeed()` to set the internal RxODE seed instead
  of grabbing it from a uniform random number tied to the original R
  seed.  This will avoid the possibility of [duplicate
  seeds](https://tinyurl.com/m62v3kv9) and is the best practice.

* Updating parameter pointers is done once per ID and locked based on
  ID to remove the recursion in #399, but still have the correct
  behavior see #430

* Parsing updated to retain "param()" in normalized model, #432.

* Handle edge case of interpolation at first index correctly, fixes #433

* Instead of storing each dose information sequentially, store dose
  information at the same index of the `evid` defining the dose.  This
  memory rewrite is to fix the issue #435.

* Start using strict headers as it is required for the forthcoming
  release of `Rcpp`.  Thanks to Dirk Eddelbuettel for some of the
  fixes and alerting us to this change.

* Check arguments for `add.dosing()` more strictly. See Issue #441

* Issue a warning when either `dur()` or `rate()` is in the model but
  the modeled rate and duration is not included in the event table.

* When the data requires a modeled rate and modeled duration but it is
  not in the model, warn about the mismatch in data

* Added a back-door for debugging. If you specify
  `options(RxODE.debug=TRUE)` then each solve saves the solving
  information to the file `"last-rxode.qs"` before actually solving
  the system.

* Only will try to solve RxODE problems on compatible models; If the
  model is not supported it will throw an error instead of crashing
  (See #449)

* Turn off parallel ODE solving whenever the system needs to sort
  times based on model dosing.  Currently this type of solving is not
  thread safe.

* Update timsort headers to latest version.

# RxODE 1.0.9

* At the request of CRAN, stripping the debugging symbols for the CRAN
  release is no longer performed.  This means a larger binary size for
  RxODE in this release.

* At the request of CRAN the `liblsoda` code has been changed so that
  the memory in C defined by `_C()` is now defined by `_rxC()`. This
  will be seen in some of the error messages, which will no longer
  match the error messages of unmodified liblsoda.

* `iCov` behavior has shifted to merge on the input event dataset.
  See Issue #409; This is more in line with expectations of `iCov`
  behavior, and reduces the amount of code needed to maintain `iCov`.

  The `iCov` in the pipeline is no longer supported because it simply
  is a merge with the event dataset.

  This can be a breaking change depending on the code you use.  Note
  that clinical trial simulations, resampling is likely better than
  trying to fill out `iCov` for every individual which was the prior
  use.

* Bug fix for crashes with string covariates or factor covariates,
  issue #410. Also factor column names are compared with case
  insensitivity just like the rest of the column names for event
  tables or data sets in `RxODE`.

# RxODE 1.0.8

* Fix issue #399

# RxODE 1.0.7

* Change syntax vignette to use markdown option
  `screenshot.force=FALSE`.  This should get rid of the `webshot`
  error

* Change to depend on dparser 1.3.0, which has some memory fixes

# RxODE 1.0.6

* RxODE imports but does not link to `checkmate` any longer.  This change
   should make recompilation of RxODE to work with different releases
   of `checkmate` unnecessary.

* Default Solaris solver changed back to "lsoda"

* Fix Bug #393, where in certain circumstances `rxSolve(...,theta=)`
  did not solve for all subjects.

* Will not ignore NEWS and README when building the package so that
  they will show up on CRAN.  You can also access the news by
  `news(package="RxODE")`

* Changed `ODR` model names from time id to `_rx` followed by the
  `md5` hash id and a per-session counter id; For packages the id is
  `_rxp` followed by the `md5` hash and a per-session counter id.

* Changed `qs` to be more conservative in hash creation. Add a check
  hash as well as NOT using altrep stringfish representation.

# RxODE 1.0.5

* Maintenance release -- use `std::floor` and cast variables to
  `double` for internal C functions.  This should allow a successful
  compile on Solaris CRAN.

* Changed `units` from an Imports to a Suggests to allow testing on
  Solaris rhub

* Changed `ODR` model names from time id to `_rx` followed by the
  `md5` hash id; For packages the id is `_rxp` followed by the `md5`
  hash.

* Removed AD linear compartment solutions for Windows R 3.6, though
  they still work for Windows R 4.0 (You can get them back for Windows
  R 3.6 if you install `BH` 1.66.0-1 and then recompile from source).

   - This will cause `nlmixr` to fail with solved systems on Windows 3.6.
     Currently the Stan Headers do not compile on this system so they are
     disabled at this time.

 * RxODE imports but does not link to `qs` any longer; This change
   should make recompilation of RxODE to work with different releases
   of `qs` unnecessary.

 * RxODE now checks for binary compatibility for `Rcpp`, `dparser`,
   `checkmate`, and `PreciseSums`

# RxODE 1.0.4
## Breaking changes

* RxODE can only use supported functions (could be breaking); You may
  add your own functions with `rxFun` and their derivatives with `rxD`

* RxODE now uses its own internal truncated multivariate normal
  simulations based on the threefry sitmo library.  Therefore random
  numbers generated within `RxODE` like providing
  `rxSolve(...,omega=)` will have different results with this new
  random number generator.  This was done to allow internal re-sampling
  of sigmas/etas with thread-safe random number generators (calling R
  through `mvnfast` or R's simulation engines are not thread safe).

* `RxODE` now moved the precise sum/product type options for `sum()`
  and `prod()` to `rxSolve` or `rxControl`

* `cvPost` now will returned a named list of matrices if the input
  matrix was named

* `rxSolve` will now return an integer `id` instead of a factor `id`
  when `id` is integer or integerish (as defined by checkmate).
  Otherwise a factor will be returned.

* When mixing ODEs and `linCmt()` models, the `linCmt()` compartments
  are 1 and possibly 2 instead of right after the last internal ODE.
  This is more aligned with how PK/PD models are typically defined.

* `EVID=3` and `EVID=4` now (possibly) reset time as well.  This
  occurs when the input dataset is sorted before solving.

* When `EVID=2` is present, an `evid` column is output to distinguish
  `evid=0` and `evid=2`

## New features

* Add the ability to order input parameters with the `param()`
  pseudo-function

* Add the ability to resample covariates with `resample=TRUE` or
  `resample=c("SEX", "CRCL")`.  You can resample all the covariates by
  `ID` with `resampleID=TRUE` or resample the covariates without
  respect to `ID` with `resampleID=FALSE`

* Comparison of factors/strings is now supported in `RxODE`; Therefore
  ID=="Study-1" is now allowed.

* Completion for elements of `rxSolve()` objects, and `et()`
  objects have been added (accessed through `$`)

* Completion of `rxSolve()` arguments are now included since they are
  part of the main method

* Allow simulation with zero matrices, that provide the simulation
  without variability.  This affects `rxSolve` as well as `rxMvnrnd` and
  `cvPost` (which will give a zero matrix whenever one is specified)

* `et()` can dose with `length(amt) > 1` as long as the other
  arguments can create a event table.

* Rstudio notebook output makes more sense

* Printing upgraded to cli 2.0

* Caching of internal C data setup is now supported increasing speed
  of `optim` code when:
  - Event Table doesn't change
  - The size of the parameters doesn't change
  - `inits` do not change (though you can specify them as `cmt(0)=...`
    in the model and change them by parameters)
  - See Issue #109

* Allow `while(logical)` statements with ability to break out if them
  by `break`. The while has an escape valve controlled by `maxwhere`
  which by default is 10000 iterations. It can be change with
  `rxSolve(..., maxwhere = NNN)`

* Allow accessing different time-varying components of an input
  dataset for each individual with:

  - `lag(var, #)`
  - `lead(var, #)`
  - `first(var)`
  - `last(var)`
  - `diff(var)`

Each of these are similar to the R `lag`, `lead`, `first`, `last` and
`diff`.  However when undefined, it returns `NA`

* Allow sticky left-handed side of the equation; This means for an
  observation the left handed values are saved for the next
  observations and then reassigned to the last calculated value.

  This allows NONMEM-style of calculating parameters like tad:

```r
mod1 <-RxODE({
    KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    if (!is.na(amt)){
        tdose <- time
    } else {
        tad <- time - tdose
    }
})
```

It is still simpler to use:

```r
mod1 <-RxODE({
    KA=2.94E-01;
    CL=1.86E+01;
    V2=4.02E+01;
    Q=1.05E+01;
    V3=2.97E+02;
    Kin=1;
    Kout=1;
    EC50=200;
    C2 = centr/V2;
    C3 = peri/V3;
    d/dt(depot) =-KA*depot;
    d/dt(centr) = KA*depot - CL*C2 - Q*C2 + Q*C3;
    d/dt(peri)  =                    Q*C2 - Q*C3;
    d/dt(eff)  = Kin - Kout*(1-C2/(EC50+C2))*eff;
    tad <- time - tlast
})
```
If the `lhs` parameters haven't been defined yet, they are `NA`

* Now the NONMEM-style `newind` flag can be used to initialize `lhs`
  parameters.

* Added `tad()`, `tad(cmt)` functions for time since last dose and time
  since last dose for a compartment; Also added time after first dose
  and time after first dose for a compartment `tafd()`, `tafd(cmt)`;
  time of last dose `tlast()`, `tlast(cmt)` and dose number
  `dosenum()` (currently not for each compartment)

* Changed linear solved systems to use "advan" style `linCmt()`
  solutions, to allow correct solutions of time-varying covariates
  values with solved systems; As such, the solutions may be slightly
  different.  Infusions to the depot compartment are now supported.


* Added sensitivity auto-differentiation of `linCmt()` solutions.
  This allows sensitivities of `linCmt()` solutions and enables
  `nlmixr` focei to support solved systems.
  - One solution is to use Stan's auto-differentiation which requires
    `C++14`

* When calculating the empirical Bayesian estimates for with `rxInner`
  (used for nlmixr's 'focei') ignore any variable beginning with `rx_`
  and `nlmixr_` to hide internal variables from table output.  This
  also added `tad=tad()` and `dosenum=dosenum()` to the `ebe` output
  allowing grouping by id, dose number and use TAD for individual plot
  stratification.

* Added ability to prune branching with `rxPrune`. This converts
  `if`/`else` or `ifelse` to single line statements without any
  `if`/`then` branching within them.

* Added ability to take more complex conditional expressions, including:
  - `ifelse(expr, yes, no)`
  - `x = (x==1)*1 + (!(x==1))*2`
  - `if (logic){ expr} else if (logic) {expr} else {}`.  The preferred
    syntax is still only `if`/`else` and the corresponding parsed code
    reflects this preference.
    - Note `ifelse` is not allowed as an ODE compartment or a variable.

* Switched to `symengine` instead of using `sympy`
  - Remove dependence on python.
  - Since symengine is C-based and doesn't require the python
    interface it is much faster than `sympy`, though some functions in
    `sympy` are no longer accessible.
  - Also symengine requires R 3.6, so now RxODE requires R 3.6

* Added new ODE solving method "indLin", or inductive linearization.
  When the full model is a linear ODE system this becomes simply the
  matrix exponential solution.  Currently this requires a different
  setup.

* Added arbitrary function definition to RxODE using `rxFun`
  - Requires function, arguments and corresponding C-code
  - Derivatives (if required) can be added to the derivative table
    `rxD`.  When taking deviates without a derivative function, RxODE
    will use numerical differences.

* Will error if RxODE does not know of a function that you are trying
  to use; This could be a breaking change.  Currently:
  - C's functions from `math.h` are supported
  - R's function returning and taking doubles are supported
  - Other functions can be added using `rxFun` and `rxD`

* Added `NA`, `NaN`, `Inf` and `+Inf` handling to a RxODE model.  Can
  be useful to diagnose problems in models and provide alternate
  solutions. In addition, added R-like functions `is.nan`, `is.na`,
  `is.finite` and `is.infinite` which can be called within the RxODE
  block.

* Allowed the following data variables can be accessed (but not
  assigned or used as a state):
  - `cmt`
  - `dvid`
  - `addl`
  - `ss`
  - `amt`
  - `rate`
  - `id` which requires calling the id as factor `ID=="1"` for
    instance.

* Kept `evid` and `ii` as restricted items since they are not part of
  the covariate table and are restricted in use.

* Added the following random number generators; They are thread safe
  (based on `threefry` `sitmo` and c++11) and your simulations with
  them will depend on the number of cores used in your simulation (Be
  careful about reproducibility with large number of threads; Also
  use parallel-solve type of RxODE simulations to avoid the [birthday
  problem](https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/)).


  During ODE solving, the values of these are `0`, but while
  calculating the final output the variable is randomized at least for
  every output. These are:

  - `rxnorm()` and `rxnormV()` (low discrepancy normal)
  - `rxcauchy()`
  - `rxchisq()`
  - `rxexp()`
  - `rxf()`
  - `rxgamma()`
  - `rxbeta()`
  - `rxgeom()`
  - `rxpois()`
  - `rxt()`
  - `rxunif()`
  - `rxweibull()`

  In addition, while initializing the system, the following values are
  simulated and retained for each individual:

  - `rinorm()` and `rinormV()` (low discrepancy normal)
  - `ricauchy()`
  - `richisq()`
  - `riexp()`
  - `rif()`
  - `rigamma()`
  - `ribeta()`
  - `rigeom()`
  - `ripois()`
  - `rit()`
  - `riunif()`
  - `riweibull()`

* Added `simeta()` which simulates a new `eta` when called based
  on the possibly truncated normal `omega` specified by the original
  simulation.  This simulation occurs at the same time as the ODE is
  initialized or when an ODE is missing, before calculating the final
  output values.  The `omega` will reflect whatever study is being simulated.

*  Added `simeps()` which simulates a new `eps` from the possibly
  truncated normal `sigma` at the same time as calculating the final
  output values. Before this time, the `sigma` variables are zero.

  All these change the solving to single thread by default to make sure the
  simulation is reproducible. With high loads/difficult problems the
  random number generator may be on a different thread and give a
  different number than another computer/try.

  Also please note that the `clang` and `gcc` compiler use different
  methods to create the more complex random numbers.  Therefore
  `MacOS` random numbers will be different than `Linux`/`Windows` at
  this time (with the exception of uniform numbers).

  These numbers are still non-correlated random numbers (based on the
  sitmo test) with the exception of the vandercorput distributions, so
  if you increase the number of threads (cores=...) the results should
  still be valid, though maybe harder to reproduce.  The faster the
  random number generation, the more likely these results will be
  reproduced across platforms.

* Added the ability to integrate standard deviations/errors of omega
  diagonals and sigma diagonals.  This is done by specifying the omega
  diagonals in the theta matrix and having them represent the
  variabilities or standard deviations. Then these standard deviations
  are simulated along with the correlations using the IJK correlation
  matrix (omega dimension < 10) or a correlation matrix or Inverse
  Wishart-based correlation matrix (omega dimension > 10).  The
  information about how to simulate this is in the variability
  simulation vignette.

* Now have a method to use `lotri` to simulate between occasion
  variability and other levels of nesting.

* Added lower gamma functions See Issue #185

* Upgraded comparison sort to timsort 2.0.1

* Changed in-place sort to a modified radix sort from
  `data.table`.  The radix search was modified to:
 - Work directly with `RxODE` internal solved structures
 - Assume no infinite values or `NA`/`NaN` values of time
 - Always sort time in ascending order
 - Changed sorting to run in a single thread instead of taking over
   all the threads like data.table

* Changed method for setting/getting number of threads based on
  `data.table`'s method

* Added function `rxDerived` which will calculate derived parameters
  for 1, 2, and 3 compartment models

* More descriptive errors when types of input are different than expected

## Engine changes

* Moved many C functions to C++.  CRAN OpenMP support requires C++
  only when C and C++ are mixed.  See:

  https://stackoverflow.com/questions/54056594/cran-acceptable-way-of-linking-to-openmp-some-c-code-called-from-rcpp

* No longer produces C code that create the model variables. Instead,
  use `qs` to serialize, compress and encode in base91 and then write
  the string into the C file. The `qs` package then decodes all of
  that into the model variables.  This also increases the compilation
  speed for models in RxODE.

* Pre-compile RxODE headers once (if cache is enabled), which
  increases compilation speed for models in RxODE

* `RxODE`'s translation from the mini-language to C has been refactored

## Bug fixes:
 - Occasionally RxODE misidentified dual `lhs`/`param` values.  An
   additional check is performed so that this does not happen.

 - For solved matrices with similar names (like "tadd" and "tad")
   RxODE will now prefer exact matches instead of the first match
   found when accessing the items with `$tad`.

 - A fix where all ID information is kept with `keep=c(""..."")`

 - Transit compartment models using the `transit` ODE or variable are
   now allowed.  Also check for more internally parsed items (see
   Issue #145).

 - Bug fix for `etSeq` and `etRep` where greater than 2 items were
   mis-calculated

# RxODE v0.9.2-0
* New plotting engine
* Various bug fixes for upcoming R 4.0 release:
  - Dropped some imports for 21 imports restriction
  - Fixed incompatibility with new `ggplot2` 3.3.0
  - Fixed allowing `NA`s in RxODE dataset
  - Fixed setting all compartment default values for bioavailability, rate, etc.
  - Added additional protection against floating point -> NaN for power functions

# RxODE v0.9.1-9
* Minor namespace/documentation changes for R 4.0 compatibility

# RxODE v0.9.1-8
* Added the ability to have an input parameter to be assigned to a new
  value (Issue #135)
* Added LINPACK authors as contributors
* Added a `NEWS.md` file to track changes to the package

<!--  LocalWords:  resample covariates Rstudio NONMEM advan focei
 -->
<!--  LocalWords:  nlmixr's symengine linearization RxODE
 -->
<!--  LocalWords:  reproducibility
 -->
