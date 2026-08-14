# rxode2 5.1.7 (development version)

## New features

- `rxSolve()` simulates parameter uncertainty from the prior distributions
  the model's `ini({})` block specifies, which is what NONMEM does with
  `$PRIOR NWPRI` and `$PRIOR TNPRI`.  Writing a prior in the `ini({})`
  block needs `lotri` 1.0.7 or newer; with an older `lotri` the block
  cannot express one and prior simulation simply does not engage.
  `omegaSeparation="tnpri"` below works with any `lotri`.  A model that carries priors uses them
  whenever variability is simulated, so `rxSolve(model, ev, nStud=100)` is
  all that is needed.

  Each omega block is drawn from an inverse Wishart with **its own**
  degrees of freedom (`prior(eta.cl, eta.v) ~ invWishart(20)`), which the
  single `dfSub` argument cannot express; a block with no prior is left at
  its point estimate.  A normal prior on a population parameter
  (`tka ~ 0.01`) gives the `thetaMat`, and a block may name omega elements
  as well (`tcl + om.eta.cl ~ c(...)`) for one joint variance over the
  thetas and the omega values.

  A prior mean has to be what the model already says the entry is, since
  the draw is added to that value; a prior centered elsewhere is an error
  rather than a simulation that quietly differs from the model.

  Because a jointly drawn omega is not guaranteed positive definite, such a
  draw is retried up to `priorPdRetry` times (10 by default); if none is,
  the nearest positive definite matrix of the kept draws is used with a
  warning, since the projection is biased toward singularity.

  `usePrior=FALSE` ignores the priors.  Priors take precedence over a
  `thetaMat`/`dfSub` carried in the model's `meta` block, with a warning;
  one given at the call site wins over the priors instead.  Nested/occasion
  models (#1253) and chunked solves (#1252) are a clear error rather than a
  solve that silently drops the prior.

- `rxSolve(omegaSeparation="tnpri")` (and `sigmaSeparation="tnpri"`) draws
  the omega/sigma entries carried in a `thetaMat` jointly with the thetas,
  rather than redrawing their correlations with a separation strategy.
  This is the general form of the `TNPRI` above, for a `thetaMat` that did
  not come from an `ini({})` block prior.

  A covariance step already gives one: `nonmem2rx` emits a `thetaMat` with
  columns like `IIVCL, omega1.2, IIVV1, ...` and a nlmixr2 fit's `$cov`
  uses `om.<eta>`/`cov.<eta1>.<eta2>`.  Both spellings are recognized, as
  are the sigma equivalents.  Until now the off-diagonal entries were
  dropped as "too many items" and the correlations were redrawn from LKJ,
  discarding what the covariance step measured; drawing them jointly also
  keeps the covariance *between* a theta and an omega entry, which no
  separation strategy can carry.

  It is opt-in because an eta-named `thetaMat` column already means that
  eta's variance under the existing strategy, so the same column cannot
  silently change meaning.  `omega` has to be a matrix, since the draws
  are added to it.

- `rxUiPriors()` returns the priors a model specifies, with the parameter
  name, the prior, its `neta1`/`neta2` (`NA` for a population parameter) and
  the parameter's `lower`/`upper` bounds.  The predicates
  `testRxUiPriors()`, `testRxUiNormalPriors()`, `testRxUiOmegaDf()` and
  `testRxUiOmegaNormalPriors()` report what kind of priors are present, so a
  method that *implements* priors can branch instead of asserting.
  `testRxUiOmegaDf()` and `testRxUiOmegaNormalPriors()` are mutually
  exclusive, since an omega prior is either degrees of freedom (`NWPRI`) or
  a normal prior (`TNPRI`) and lotri rejects a model that gives both.

- `assertRxUiNoOmegaDf()` and `assertRxUiNoOmegaNormalPriors()` reject the
  omega prior forms a method cannot use, the `NWPRI` and `TNPRI` ones
  respectively.

- `assertRxUiNoPriors()` and `assertRxUiNormalPriors()` let an estimation
  method declare which prior distributions it can use.  A prior specified in
  the `ini({})` block must never be silently ignored -- that would make the
  fit do something other than what the model says -- so a method that cannot
  use priors calls `assertRxUiNoPriors()` and one that only handles normal
  priors calls `assertRxUiNormalPriors()`.  Both are no-ops when the
  installed `lotri` has no prior support, since then there are no priors to
  reject.

- `rxSolve(zeroVarParamHandle=)` says what happens when `params` supplies a
  value for an omega/sigma item whose variance is zero (say
  `eta.base ~ fix(0)`).  Such an item is dropped from the matrix that is
  simulated from and given to the model as a literal zero instead, which
  discards the supplied value: `"warn"` (the default) does that and says so,
  `"ignore"` does it silently, and `"keep"` uses the supplied value.

- `rxSolve(safeLog=2)` floors `log(0)` at `log(.Machine$double.eps)` the way
  `safeLog=TRUE` does, but treats a **negative** argument as a domain error and
  returns `NaN`.  `safeLog=TRUE` (the default) and `safeLog=FALSE` are
  unchanged.  This is for a hand-written likelihood taking `log()` of a
  parameter that must stay positive: under `safeLog=TRUE` an invalid negative
  value returns a large finite number, which `-log(sigma)` turns into a reward
  of roughly `+36` per observation instead of a rejection.

- A function that produces models can now name them.  `rxModelName()` is an
  `s3` generic dispatched on the name of the function that was called, so a
  `rxModelName.readModelDb()` method names every model
  `rxode2(readModelDb("PK_1cmt"))` builds (here, `"PK_1cmt"`) instead of
  leaving it named after the text of the call.  The method is given the call
  and its (unevaluated) arguments, matched to the argument names of the
  function being called; a call with no method keeps the default name.

- `rxModelVars(m)$indLin$wIndLin` now reports the states whose
  `indLin(<state>) <- <expr>` forcing references a compartment, rather than
  always being empty.  It is worked out by replaying the parsed assignments and
  forcings in source order, so hand-written `matExp()` models are covered as
  well as converted ones, and a forcing that reaches a compartment only through
  an assigned variable (`cp <- central/20`) counts too; a forcing built only
  from parameters or covariates (e.g. `indLin(Gc) <- Gprod`) stays unflagged, as
  does one whose variables were reassigned to something state free before it
  reads them.  A forcing inside an `if`/`while` may not run, so it adds to what
  the forcings before it established rather than replacing them.  In a model
  that also has a `linCmt()`, every forcing is flagged: a solved concentration
  moves within the step, so such a forcing cannot be treated as constant over
  the interval the way a locf covariate can.  The entries are the 0-indexed
  positions in `$state`, named with those states.

- `rxModelNameLhs()` registers the name an assignment is making, for
  assignment operators like `nlmixr2save`'s `:=` (`fit := nlmixr2(...)`).  It
  names the model when the model expression itself names nothing -- an
  anonymous model function, or a call with no `rxModelName()` method -- so the
  model is built with that name rather than none.  `rxModelNameFromExpr()`
  exposes the whole naming sequence for packages that capture a model
  expression with `substitute()`.

- `rxMemoryEstimate()` now accounts for what `method="indLin"` allocates, as
  two new components: `indLinExpCache` (the per-thread matrix-exponential
  cache) and `indLinWork` (the per-thread solver scratch).  Both depend on
  which driver the model runs -- a pure `matExp()` model holds one rate matrix,
  while true inductive linearization iterates and carries a Jacobian, `P(h)`
  and its inverse as well -- and both scale with `cores` rather than with
  subjects, so `rxSolve()` reaches the same out-of-memory decision for
  `method="indLin"` that it already reached for every other solver, and
  `rxSolveChunked()` sizes its chunks without charging per-thread buffers to
  each subject.

- `rxSolve(method="indLin")` now solves subjects in parallel and honors
  `cores`.  Inductive linearization was held to a single core because it was
  listed with the Fortran COMMON-block solvers (`lsoda`, `lsode`, `bdf`); it is
  not one of them, and its matrix-exponential and scheme caches were already
  per thread.  It now goes through the same thread-safety switch as
  `liblsoda`, so a model whose functions are not thread safe still drops to one
  core with the usual warning.  The answer, and the `rxIndLinSteps()` step
  counts, are unchanged from the single-core solve.

- Solve-time hooks let a package change what a solve sees from outside the
  model text.  `rxForcedPars(ui) <- c(cl = 1.2)` sets parameter values that
  override `params`/data and the initial estimates on every solve; they are
  stored on the ui (hidden from the printed model, registered `sticky`), so
  they survive piping and travel into a `nlmixr2` fit built from that model,
  which keeps a fit carrying externally-owned values (e.g. trained weights)
  self-contained.  For a block that is computed rather than fixed, a package
  registers a C **par-loader** (`rxRegisterParLoader()`) that runs once per
  solve, single-threaded, after the global parameter matrix is laid out and
  before integration; `rxInjectedPars()` reports what it changed, and those
  values are saved on the solved object so re-solving reproduces them in a
  session where the injecting package's buffer is gone.  A loader registered
  with `rxRegisterParLoaderNamed("<pkg>:<fn>")` runs *only* for a model that
  flags that name with `rxParLoader(ui) <- "<pkg>:<fn>"`, so an injector cannot
  reach an unrelated model; an unnamed loader keeps running on every solve.
  `rxRegisterDydtForce()` adds a term to a state derivative at the end of the
  generated model's RHS, so it is integrated like any other -- it runs inside
  the parallel per-subject solve, so such a callback must be thread safe and
  must check `neq[0]` before writing a `dydt` slot.  `rxRegisterUiPrep(name,
  fn)` calls `fn(ui)` at the start of every ui solve, before parameters are
  loaded, so a package can rebuild C-side state that a saved-and-reloaded ui no
  longer has; resolve positions by name rather than a stored index, and keep it
  cheap and a no-op for models it does not own, since it runs on every ui
  solve.  A failing prep hook is downgraded to a warning.  Pair each
  registration with `rxRemoveParLoader()` / `rxRemoveDydtForce()` /
  `rxRemoveUiPrep()` in `.onUnload()`.  See the [solve-time hooks
  article](https://nlmixr2.github.io/rxode2/articles/rxode2-solve-hooks.html).

## Bug fixes

### Initial conditions data frame

- The `iniDf` now tolerates the `prior` column that `lotri` 1.0.5 adds for
  prior distributions (#1248).  `testIniDf()`/`assertIniDf()` used to
  reject every model built with such a `lotri`, and the ini rows that are
  constructed by hand internally (adding a covariance between two etas,
  promoting a parameter, `linMod()`) hard-coded the column list and so failed
  to `rbind()` with "numbers of columns of arguments do not match".  These now
  match whatever columns the `iniDf` actually has, so an `iniDf` without the
  column still works.

### Parsing

- A variable that is used *only* as an argument to an adaptive dosing call
  (`evid_()`, `bolus()`, `infuse()`, `infuseDur()`, `replace()`, `multiply()`,
  `phantom()`, `obs()`) is now a parse-time error instead of an uncompilable
  model.  These statements consume their arguments as text, so such a variable
  was never registered and the generated C referenced an undeclared identifier;
  the failure only showed up as a compiler error that looked like a broken
  toolchain.  The message now names the variable, the argument and the
  function, and points at the fix (assign it to a model variable first):

  ```
  undeclared 'DOSE' in 'amt' of 'infuseDur()'; assign first: 'amtVal <- DOSE'
  ```

  The check runs once the whole model is parsed, so a variable assigned below
  the dosing statement still counts as declared (#1231).

### Compilation

- A model that fails to build now shows the compiler's own error lines (and
  only those -- warnings and progress chatter are dropped, and the list is
  capped by `options(rxode2.compileErrLines=)`), followed by how to get the
  rest (`rxode2::rxLastCompile("stderr")` for the full compiler output,
  `rxode2::rxLastCompile("c")` for the generated C code).  The
  Rtools/C-compiler advice is only given when the failure actually looks like
  a toolchain problem: a diagnostic naming a source file and line is about the
  code that was compiled, so the message says the generated C code is at fault
  and points at the issue tracker, while a driver, linker or loader that fails
  without reaching the source still gets the setup advice.  Previously every
  failure blamed the toolchain, which sent users off validating Rtools when
  the compiler had already named the generated-code defect (#1197).

- A model that compiles but will not load reports what the loader said rather
  than the loader's error replacing the diagnosis, and a build failure found
  without recompiling (the model's dll was already present) no longer errors
  with `could not find function ".badBuild"` or reports a previous model's
  compiler output.

- `rxLastCompile()` now prints its section rules -- `cli::rule()` was called
  but its result was never messaged -- and takes `what=` to choose which
  sections are messaged (`rxLastCompile("stderr")` for the compiler error
  alone).  The returned list is unchanged.

### Model interface

- `ui$modelName` is now always a single character string, as it was always
  documented to be.  It came from `as.character()` of the substituted model
  expression, which returns one element per part of a call, so
  `rxode2(readModelDb("PK_1cmt"))` gave `c("readModelDb", "PK_1cmt")` and an
  anonymous model function gave a four-element vector including the deparsed
  body.  The name is the tidied first deparsed line of the expression instead:
  a symbol keeps its name and a call becomes its own text
  (`readModelDb("PK_1cmt")`), unless a `rxModelName()` method or
  `rxModelNameLhs()` names it better.  Names wider than 60 characters are
  truncated.  An anonymous model function names nothing, so its `modelName` is
  `NULL` rather than a piece of its body.  Values assigned by other packages
  (or read from models saved by earlier versions) are also collapsed to a
  single string on access (#1019).

- A trailing `#` comment on an `ini({})` line may now contain a double quote or
  a backslash.  Such a comment is promoted to a `label()` call when the model is
  parsed with its source refs intact, and while the label text was escaped
  correctly it was then interpolated into the replacement argument of `sub()`,
  which parses backslashes and strips one level.  The generated
  `label("fixed to a "small value"")` did not parse, so the model failed with a
  bare syntax error pointing into regenerated text rather than at the offending
  source line.  Because the promotion only runs when source refs are kept, the
  same model resolved fine without them -- so a package build could be green
  while a test suite run with `keep.source = TRUE` was red on the identical file
  (#1195).

- A trailing `#` comment on an `ini({})` line keeps its `label()` when the
  comment itself contains a `#`, including the common `## comment` form.  The
  code portion of the line was matched greedily, so on a line with two `#` it
  ran on to the last one and left the first sitting in the generated code, where
  it commented out the `label()` that had just been appended.  The label was
  dropped silently -- the model still parsed and built, it simply lost the label
  (#1205).

### Estimation / symengine translation

- A model using the modulo operator `%%` can now be estimated.  `%%` was
  missing from the infix operator tables of the `if`/`else` rewriter
  (`rxPrune()`) and of `rxOptExpr()`, so both emitted it as the prefix call
  `%%(a, b)`, which is not parsable rxode2.  Since every nlmixr2 estimation
  method runs those two stages, a model that solved fine failed to fit with a
  syntax error -- blocking `%%` as the way to write a square-wave or circadian
  time-dependent parameter.  Operands that are not a plain name or number are
  parenthesized, as the grammar requires (#1229).

- `floor()`, `ceil()`, `round()`, `trunc()`, `sign()`, `fround()`, `fprec()` and
  `fsign()` can now be used with the nlmixr2 estimation methods.  They parsed
  and solved, but symengine's `Math` group generic has no method for them, so
  loading such a model raised `non-numeric argument to binary operator` and no
  estimation method could run it -- which ruled out `floor(time/24)`, the
  natural way to write a circadian or square-wave switch.  They are now loaded
  as opaque function symbols (like `rxMod()`) and are locally constant, so their
  derivative is 0 at every order.  `fsign(x, y)` transfers the sign of `y` onto
  `abs(x)`, so it gets a real derivative instead: `sign(x)*fsign(1, y)` in `x`
  and 0 in `y` (#1230).

- Every other parser-known function symengine has no method for now loads too,
  rather than silently corrupting the model.  This covers the special functions
  (`bessel_i()`, `bessel_j()`, `bessel_k()`, `bessel_y()`, `logspace_add()`,
  `logspace_sub()`, `fmax2()`, `fmin2()`, `gammaq()`, `gammapDer()`,
  `gammapInv()`, `gammapInva()`, `gammaqInv()`, `gammaqInva()`) and the
  derivative helpers rxode2 itself emits (`llikNormDmean()`, `dSELU()`,
  `d4GELU()`, `d2PReLU()`, `dSwish()`, ...).  The failed assignment used to be
  stored as the variable's value and written into the model as `<var>=.expr`,
  which failed later with no hint of where it came from -- or not at all, when
  nothing read the variable.  The set is now a deny list of the functions
  symengine differentiates itself, so a function added to the parser is loadable
  by default, and an assignment that still cannot be loaded says which variable
  and why instead of continuing.

- `ftrunc(x)` builds.  Its arity was recorded as two arguments while C's
  `Rf_ftrunc()` takes one, so `ftrunc(x)` was rejected by the parser and
  `ftrunc(x, digits)` failed to compile -- the function could not be used at
  all.

- `dSwish()` can be used with the estimation methods.  Its symengine expansion
  was missing a closing parenthesis, so the text could not be parsed back and
  the model failed to load.

- The parser no longer accepts a function it cannot generate compilable C for.
  `abs0()` and `polygamma()` exist only between `rxToSE()` and `rxFromSE()`
  (`abs0(x)` is written `abs(x)`/`fabs(x)`, and `polygamma(n, x)` is
  `psigamma(x, n)`), and `d2PReLU()` had no implementation anywhere -- `PReLU()`
  is piecewise linear, so its second `x` derivative is the literal 0
  `rxode2parseD()` already returns.  Writing any of the three built C with an
  undeclared function, which rxode2 reported as a code-generation bug and asked
  the user to file; they now fail at the model text with the usual unsupported
  function message.  Both symengine directions still convert them.

- The description of `fsign()` in `rxSyntaxFunctions` said `abs(x)*sign(y)`,
  which is wrong when `y` is 0: the function carries the sign of `y` onto
  `abs(x)` and treats 0 as positive, so it returns `abs(x)` there rather than 0.

### Solving

- Modeled duration (`rate = -2`) and modeled rate (`rate = -1`) doses that fall
  at exactly the same time now solve.  Each such dose is expanded into a
  start/stop pair sharing one time and the solver pairs the two positionally, but
  the event sort keys on the compartment-bearing `evid`, so tied doses
  interleaved (`start2 start1 stop2 stop1`) and the solve failed with data errors
  686/886 (or 797/997 for a modeled rate) -- even for doses into different
  compartments, which is a legal data set.  `etTrans()` now re-pairs each start
  with its own stop after the sort, matching on compartment and infusion type;
  the pass only runs when the data set has a modeled rate/duration dose and it
  leaves already-correct records in place.  The four data-error messages now say
  what the problem is instead of only naming a number (#1218).

- Fixed an out-of-bounds read of the extra-dose pool while advancing to the
  first extra dose at or after the current step.  The index was bounds checked
  before it was incremented rather than after, so a subject whose extra doses all
  precede the step -- reachable with tied modeled duration steady state doses --
  read one element past the end and then dereferenced it as a record index,
  corrupting the heap.
- A parallel chunked solve (`rxSolve(file=, chunkSize=, parallel=)`) no longer
  fails outright when the `mirai` daemons load a different rxode2 than the
  parent is running -- a source checkout, or a library updated underneath a
  long-lived pool.  The whole control list is forwarded to each daemon by name,
  and `rxSolve()` rejects an argument it has no formal for, so a parent one
  version ahead lost every chunk to `unused argument`.  A control the daemons
  cannot take is now dropped, with a warning naming it and the version they
  loaded, rather than losing the solve over a setting that version had no notion
  of.  What they can take is asked of the daemon itself, so a matching pool
  drops nothing.
- An event pushed by the model with `evid_()` (and the `bolus()`, `infuse()`,
  `replace()`, `multiply()`, `reset()`, `phantom()` and `obs()` helpers) now
  gives the same solution as the identical event written in the data, on every
  solving method.  The ODE methods fired the model body from `dydt()` at the
  start of the next integration interval: the time value was right, but the
  event was inserted only after the solver had been asked to integrate past it,
  so `liblsoda`, `dop853` and `cvode` applied the jump one observation late and
  `lsoda` dropped it altogether.  `evid_()` now fires from a single shared point
  at the record itself -- once per distinct record time, with the pushed event
  landing in the slot immediately after that record -- so ODE, `linCmt()` and
  `indLin()` models agree with each other and with the explicit event.  A model
  that pushes an event but defines no `lhs` variable also compiled to an empty
  `calc_lhs()` and never pushed anything; its body is now emitted.  A pushed
  event that extends the timeline past its original last record is no longer
  truncated by the dense `dop853` driver, and `dense=TRUE` is now dropped (with
  a warning) for a model that pushes: a dense segment integrates across every
  observation between two key events at once, which cannot honour an event the
  model decides on at one of those observations.  A model that combines
  `delay()` with a pushed event is now an error rather than silently returning
  one of two wrong answers: `delay()` requires the dense output that a pushed
  event rules out.

- An adaptive dosing helper guarded by `t == <mtime>` no longer pushes its dose
  twice when that `mtime()` names a time the event table already contains.  The
  same model written as a function (`ini({})`/`model({})`) and as an
  `rxode2({})` block disagreed, because `rxSolve()` defaults to
  `useLinCmt=TRUE` for a function model: that one was auto-converted to a
  `linCmt()` model, and the `linCmt()` driver fired `evid_()` from both its own
  internal model evaluation and a second pass for the same-time observation.
  Both forms now push once, and the doubled dose (silent except in the state at
  the next time point) is gone.

- `rxSolve()` no longer returns silently wrong, run-to-run varying results when
  a multi-row `params` data.frame (one parameter set per `id`) is combined with
  `omega = NA` or `sigma = NA`.  `c()` on a data.frame drops the data.frame
  class and yields a ragged list -- the per-id columns keep their length while
  the appended zeros have length one -- which was then read out of bounds while
  solving, so the random effects that `omega = NA` fixes at zero were filled
  from unrelated memory instead.  With eight or more subjects this changed the
  solved values on every solve of identical input, occasionally to non-finite
  ones.  A multi-row `params` matrix hit the same problem from the other side:
  `c()` dropped its `dim`, so `omega = NA`/`sigma = NA` failed outright with
  "The following parameter(s) are required for solving".  `omega = NA` on a
  model with no between subject variability (which failed with "invalid 'times'
  argument") and `sigma = NA` on a model with no residual error are now the
  no-ops they should be.

- An `omega`/`sigma` entry whose variance is zero (say `eta.base ~ fix(0)`) is
  now supplied to the model as a literal zero when `params` is a matrix, as it
  already was for a data.frame or a named numeric vector.  Such an entry is
  dropped from the matrix that is simulated from, so a matrix `params` reached
  the solver without it and `rxSolve()` failed with "The following parameter(s)
  are required for solving".  A matrix that did supply the item kept its value
  where a data.frame had it replaced by zero; both replace it now, and
  `zeroVarParamHandle=` chooses (see New features).

- A `params` matrix that supplies a random effect is now recognized as
  supplying it, so that effect is no longer simulated on top of the supplied
  value.  `rxSolve()` decided whether `params` already had a random effect with
  `names(params)`, which is `NULL` for a matrix -- its names are the column
  names -- so the answer was always "no".  The supplied column was silently
  ignored and a random draw used in its place: with `eta.base = 100` supplied
  for every subject, a data.frame gave `101 102 103 ...` and a matrix gave
  `0.92 1.84 2.67 ...`.  There was no warning, and the values look reasonable
  unless you know what they should be.

- Supplying a value for one random effect no longer stops the others from being
  simulated.  A supplied effect is dropped from the `omega` before solving, but
  the subset that drops it took a single remaining effect down to a scalar,
  whose `dim` is `NULL`, and `all(NULL == c(0L, 0L))` is `TRUE` -- so the whole
  `omega` was dropped and the remaining effect was neither simulated nor
  supplied (`The following parameter(s) are required for solving: eta.b`).  The
  matching `sigma` code already guarded this.

### Sensitivities

- `linCmtB()` gained a dose-time (moving boundary) sensitivity, `which1 = -3`:
  the derivative of a `linCmt()` model with respect to a delay applied to every
  dose feeding it, which is what a modeled `alag()` on its dosed compartment
  produces.  `which2 = -3` gives it for the reported concentration, `which2 >= 0`
  for the amount in that compartment; chain-rule it with `d(alag)/dp` for the
  sensitivity wrt a model parameter.  The system is linear and its whole input
  is delayed together, so the derivative is exactly `-dA/dt` -- it matches a
  finite difference to round-off for bolus and steady-state-bolus regimens
  across one to three compartments, IV and oral.  It reports `NA` for an
  individual with an infusion (`dA/dt` needs the infusion rate, which is not
  carried into the pass that computes the output) and requires that every dose
  reaching the linear system share the same `alag()` (#1119).

- A model that mixes `linCmt()` with `d/dt()` now expands its sensitivities
  once.  The `linCmt()` call has to be resolved before the sensitivity
  expansion, and the model was re-parsed with `calcSens=` afterwards, which
  differentiated the already-expanded model a second (and, with
  `eventSens=`, a third) time.  The result carried
  `rx__sens_rx__sens_<state>_BY_<p>___BY_<p>__` compartments nobody asked for
  and an interleaved compartment layout, which the event-sensitivity map then
  read as a second-order Hessian block.  The `linCmt()` text is now built
  first and the sensitivities expanded once, from that text.  As a
  consequence `summary()` of such a model prints the `linCmt()` model as
  written, without the generated `rx__sens_*` equations after it (#1119).

- `.rxLinCmt()` no longer invents a `peripheral1` compartment for a one
  compartment oral `linCmt()` (nor a `peripheral2` for a two compartment oral
  one): the compartment count it decodes includes the depot, and it was read as
  the number of disposition compartments.  An ODE state named like the invented
  compartment was dropped from `rxStateOde()`, so it never got a sensitivity
  expansion -- its `rx__sens_<state>_BY_<param>__` compartment did not exist at
  all -- and it also raised a bogus "share a name with linCmt() reserved
  compartments" warning (#1119).

- `eventSens="jump"` now applies to the ODE compartments of a model that also
  has a `linCmt()`.  Every `linCmt()` model was downgraded to finite differences
  because the moving-boundary jump for a modeled `alag()`/`f()` on a `linCmt()`
  compartment is not implemented; the ODE compartments of such a model carry
  ordinary solved sensitivity compartments and are unaffected by that.  The
  downgrade is now limited to the models that need it: a pure `linCmt()` model,
  a reserved-name collision, or a modeled `alag()`/`f()`/`rate()`/`dur()` on a
  `linCmt()` compartment itself (#1119).

- The event-sensitivity jump map is now checked against the true compartment
  indices rather than assuming them.  The runtime injection addresses the
  sensitivity compartment of (state `k`, parameter `p`) as
  `nState + p*nState + k`; a model whose compartments do not lie that way falls
  back to finite differences instead of having jumps written into the wrong
  compartment (#1119).

### Compilation

- The statement form of `ifelse()` -- `ifelse(cond, stmt, stmt)`, where each
  branch is a statement rather than a value -- now compiles anywhere in a model.
  Its handler appended `if (` to the code buffers without first clearing the
  preceding statement's text, so the generated C ran the two together
  (`kin=3if (t<2) {`) and only a model whose *first* statement was an `ifelse()`
  compiled.  The construct now emits and normalizes exactly like the equivalent
  `if (...) {...} else {...}`, so it round-trips through `rxNorm()` and
  translates for symengine derivatives (sensitivities, FOCEi) the same way
  (#1211).

- `rxCompile()` now re-parses the model it is handed whenever the parser's
  current model is a different one.  Code generation reads the parser's global
  model state, and the old guard only checked whether *some* model was loaded,
  so a re-compile requested while an unrelated model was parsed wrote that other
  model's C under this model's name and handed back its model variables.
  Building a model with `rxode2()` never hit this (it parses, then compiles
  immediately), but re-loading one whose `.so` is gone did -- as when a saved
  fit is restored in a new session, since its DLL lived in the original
  session's `tempdir()`.  Such a fit came back solving a different model, e.g. a
  restored SAEM fit failing with "The following parameter(s) are required for
  solving: eta.v, eta.cl".

- Event ("jump") sensitivities now compile when a dosing modifier (`dur()`,
  `f()`, `alag()`, ...) depends on more than one estimated parameter.  Each such
  parameter contributes its own assignment line to the same generated buffer,
  but the rewrite of nlmixr2's indexed `THETA[n]`/`ETA[n]` to the codegen locals
  `_THETA_n_`/`_ETA_n_` only collected the indices used by the *first* line, so
  an index appearing only in a later line survived as raw symengine array syntax
  and the model failed with "'ETA' undeclared".  This hit any model with, say, a
  food-effect duration built from two etas, whether or not the parameters were
  mu-referenced (#1196).

### Delay differential equations

- `rxOptExpr()` no longer fails on a `past(state, tau)` whose delay duration is
  an expression rather than a name or a number (`past(G, exp(lT))`,
  `past(G, tau*2)`), which raised `unsupported lhs in optimize expression` and
  printed the duration into the middle of the progress bar.  This made
  `optExpression=TRUE` unusable for such a delay differential equation; it now
  optimizes, and the duration follows the same common subexpression its
  `delay()` terms do, so the history stays matched to them.

- A generated delay differential equation model (`rxode2(..., calcJac=TRUE)`,
  `calcSens=`, or an nlmixr2 estimation model) now resolves the `past()` delay
  duration the same way it resolves the history itself.  A duration written as
  an intermediate (`T <- exp(lT)`) was emitted verbatim while every `delay()`
  had its duration inlined, so the generated model named a duration no `delay()`
  used any more and `rxSolve()` rejected it with `duration 'T' does not match
  any delay(...)`.  This also covers a duration or a history written with
  `THETA[n]`/`ETA[n]`, as every mu-referenced model is: they were left
  unresolved, and an unresolved history additionally emitted no per-parameter
  sensitivity pre-history at all.

### Matrix exponential / inductive linearization

- `meOnly()`/`indLin()` no longer write past the end of their buffers when a
  downstream package sets a per-individual effective state count
  (`setIndNeqOverride()`).  Those buffers were sized by the effective count
  while the model-generated `ME()`/`IndF()`/`calc_jac()` bodies always index by
  the compiled state count, so a shortened count overran them -- quadratically
  for `ME()`.  The generated code is now called through a full-size buffer and
  the leading effective block copied back; with no override, which is every
  path rxode2 itself takes, the calls and the numerics are unchanged.

- `rxToIndLin()` -- and therefore `rxSolve(method="indLin")` -- now converts a
  model that mixes `linCmt()` with `d/dt()`.  It walked `$state`, which counts
  the `linCmt()` pseudo-compartments (`depot`, `central`, `peripheral*`); those
  have no `d/dt()` behind them, so it emitted `cmt()`/`indLin()` lines for
  derivatives that do not exist -- one of them the literal R variable name
  `.tmp` -- and the generated model did not parse.  Only the `d/dt()` block is
  converted now; the solved compartments stay with the analytic solver and are
  copied back after each step.  A term reading a `linCmt()` goes to the
  `indLin()` forcing rather than into a rate constant, since a solved
  compartment moves within the matrix-exponential step, and such a forcing takes
  the iterating path so the driver refines it.

- A `df(<state>)/dy(<state>)` Jacobian entry may now reference `linCmt()`.  A
  `linCmt()` call retyped the whole statement, so the entry lost its Jacobian
  routing and was emitted into `dydt()`, where `__PDStateVar__` does not exist:
  the model failed to compile.  For the same reason a `matExp()` rate constant
  or `indLin()` forcing built from a `linCmt()` concentration now reaches the
  `ME()`/`IndF()` functions instead of reading a stale value.

- `rxSensMatExp(calcSens3=)` now carries the `indLin()` forcing at third order,
  as `calcSens` and `calcSens2` already did.  Only the rate-matrix cross terms
  were generated, so third-order sensitivities of a nonlinear model were short
  every term the forcing contributes; the warning that said so is gone.

- The `Al-Mohy` matrix exponential evaluated the wrong Pade numerator below
  degree 13.  The coefficients depend on the degree, and the routine read a
  fixed table -- the degree-13 row -- and truncated it, which is not the
  degree-p numerator.  The answer stayed convergent but only to a few `1e-12`
  where every other backend reaches machine precision, and only against an
  exact solution is that visible.  The row is now built for whichever degree was
  selected.

- The `Al-Mohy` matrix exponential returned a wrong answer for a very large
  matrix norm.  The squaring count was returned as the factor `2^s` in an `int`
  and clamped so it could not overflow, but clamping caps the scaling while
  leaving the norm untouched, so degree-13 Pade ran far outside its range and
  produced a plausible finite number: a one-compartment model with a rate
  constant of `1e20` returned `5.1e-08` for a quantity that underflows to zero.
  The squaring count is now carried as a count.

- `rxSolve(indLinMatExpType=)` now defaults to `"Al-Mohy"` rather than
  `"expokit"`.  With the degree bug below fixed, all four backends agree to
  solver tolerance and take the same steps on every problem tested, and
  `"Al-Mohy"` is the cheapest per exponential: about 4-5% on a Michaelis-Menten
  population and 34% on a stiff van der Pol one, where an exponential-Rosenbrock
  step rebuilds its operator every step and the exponential cache cannot help.
  On a linear model the difference is unmeasurable, the cache serving nearly
  every call.  Results move in the last digits, as any change of exponential
  kernel does; pass `indLinMatExpType="expokit"` to keep the previous one.

- `rxSolve(indLinMatExpType="Al-Mohy")` chose its Pade degree and its scaling
  inconsistently, which could return a silently wrong answer or a solve that
  never finished.  The scaling came from the Al-Mohy-Higham threshold table,
  whose entries each belong to one specific degree, while the degree itself came
  from `indLinMatExpOrder` (default 6) -- so any matrix with a 1-norm up to the
  table's largest threshold, 5.37, was evaluated at degree 6 with no scaling at
  all where the table calls for degree 13.  Both are now taken together from the
  norm, as the `taylor` backend already did.  A two-compartment linear model
  returned `1.8e-06` against about `1e-11` for every other backend, and one
  van der Pol subject at `mu = 95.7866` under an exponential Rosenbrock step ran
  for over 390 seconds -- a bad exponential can make the error estimate
  unsatisfiable, so the step controller shrinks the step without limit instead
  of failing -- where the other backends took 0.03 s.  Both now agree with the
  other backends, and on a 50-subject stiff population `Al-Mohy` goes from not
  completing in 418 s to 0.92 s, the fastest of the four.  Consequently
  `indLinMatExpOrder` no longer applies to `Al-Mohy`; it still applies to
  `expokit`.

- `rxSolve(<function or rxUi model>, method="indLin")` failed with "Can only
  parse scalar data".  With the default `useLinCmt=TRUE` the ODE was first
  rewritten into `linCmt()`, leaving a model with `linCmt()` pseudo-compartments
  and no `d/dt()` for the matrix-exponential conversion to work from.  That
  rewrite is now skipped when `method="indLin"` is requested, so such a model
  integrates its own rate matrix rather than being replaced by the analytic
  solution.

- A steady-state infusion (`ss=1` or `ss=2` with a `rate`) gave a diverging
  solve under `method="indLin"`.  Its solver was the only one that never drained
  the pending-dose queue, which is where the infusion's off record is held, so
  the steady state itself was found correctly and the infusion was then left
  running for the rest of the timeline.  Steady-state boluses and ordinary
  (non-steady-state) infusions were unaffected.

- `method="indLin"` is substantially faster.  The ODE-to-`matExp()` conversion
  ran on every `rxSolve()` call although it is a pure function of the model, and
  cost several times the solve it was preparing for; it is now done once per
  model (`options(rxode2.indLinConvCache=FALSE)` restores the old behaviour).
  The matrix exponential itself was recomputed on every fixed-point pass even
  though the rate matrix cannot change between them, and identical exponentials
  are now reused (`RXODE2_INDLIN_NO_EXP_CACHE` disables this).  Together these
  are several times faster on a nonlinear model and more on a linear one; no
  result changes.

- `indLinRichardson` extrapolated `indLinIteration="exprb32"` with the factors
  for a second-order base step, which exprb32 is not -- it is third order, so
  each level took its leading term down by a constant instead of removing it,
  and the step was sized from an estimate a whole order off.  Asking for a level
  therefore made the answer worse: on a Michaelis-Menten model at `1e-8`,
  `indLinRichardson="always"` delivered `3.7e-6` where `"never"` delivered
  `1.0e-7`.  The tableau now takes both the base order and how far a level
  advances it from the scheme, so `"always4"` is `1.2e-8` for a ninth of the
  steps `"never"` needs.  Only `exprb32` is affected: it is neither the default
  nor reachable from `"auto"`, which never raised its level.

- `rxSolve(indLinForcing=)` chooses how `method="indLin"` carries the
  `indLin()` forcing across one relinearization step.  It was folded into an
  augmented column exactly as a constant infusion rate is, so it was frozen for
  the whole step.  `"ramp"` (the new default) evaluates it at both ends of the
  step and integrates the line between them exactly -- the phi2 term -- with the
  rate matrix taken at the step midpoint; `"constant"` is the previous scheme,
  which reaches the same second order by averaging a start-linearized and an
  end-linearized answer.  It applies to the `"picard"` and `"newton"` schemes;
  the exponential Rosenbrock ones never freeze the forcing.

  Only the endpoint value moves with the iterate, so the rest of the step is
  built once and a pass costs a forcing evaluation and a matrix-vector product
  rather than a matrix exponential.  The converged ramp step is symmetric, so
  its error expands in even powers of the step alone and `indLinRichardson` now
  removes two orders per level instead of one -- third order becomes fourth,
  fourth becomes sixth, fifth becomes eighth.  That is where the difference
  shows up: under the default `indLinRichardson="auto"` a nonlinear model is
  several times to a hundred times more accurate at the same tolerance for the
  same or fewer steps, while with no extrapolation the two are a wash, both
  being second order there.

- `rxSolve(indLinJac=)` chooses where the forcing Jacobian comes from when
  `method="indLin"` needs one, which is only under `"newton"`, `"exprb"` and
  `"exprb32"` -- Picard needs none, so a non-stiff model under the default
  scheme never forms one.  `"symbolic"` uses the model's own analytic Jacobian,
  which the `matExp()` conversion already emits as `df()/dy()` lines, and costs
  no extra forcing evaluations; `"fd"` central-differences the forcing at `2n`
  evaluations.  `"auto"` (the default) takes the symbolic one when the model
  carries it and falls back to finite differences otherwise, which is what
  happens above `getOption("rxode2.indLinJacMaxStates")` states where the
  emission is skipped.

  On cost the two are a wash at compartmental sizes -- within about 25% of each
  other either way from 3 to 16 states, with no consistent ordering, and the
  symbolic emission adds a fraction of a second once at model build.  The
  reason `"auto"` prefers symbolic anyway is exactness rather than speed: an
  exponential Rosenbrock step's order conditions assume the Jacobian is exact,
  and on a stiff van der Pol the symbolic one delivered a smaller error for the
  same work.

- `rxSolve(indLinIteration="exprb32")` adds the Luan-Ostermann third-order
  exponential Rosenbrock pair.  Its embedded second-order member is `"exprb"`
  itself, so the two differ by a computable quantity and it sizes its step from
  that rather than from the extrapolation column -- which is what `"exprb"` has
  to use and why `"exprb"` is held at fourth order.  It is NOT the default and
  is not selected by `"auto"`: measured at matched delivered accuracy it wins
  only on a stiff problem at a loose tolerance, by about 1.2 to 1.7 times, and
  loses elsewhere, badly so on a non-stiff population.  The reason is the cost
  of the third phi function, which needs an augmented matrix three rows wider
  than the plain step; at the small dense systems compartmental models produce,
  widening the exponential costs more than the extra order saves.

- `rxSolve(indLinIteration=)` chooses how `method="indLin"` solves each
  relinearization step: `"picard"` (the previous and only behaviour),
  `"newton"`, or `"exprb"`, an exponential Rosenbrock step that does not
  iterate at all.  Which is cheapest depends entirely on the problem -- on a
  non-stiff model the iteration never limits the step and Picard is cheapest,
  while on a stiff one it is the only thing limiting it -- so `"auto"` (the
  default) starts on Picard and switches only once steps are actually being cut
  for non-convergence.  A model that never needs a Jacobian therefore never
  forms one.  On a van der Pol oscillator integrated over a full relaxation
  period at matched accuracy this is about 39 times faster than Picard at
  `mu = 100` (593 relinearizations against 45,913) and about 426 times faster at
  `mu = 1000` (581 against 1,001,968), which takes a full cycle at that
  stiffness from impractical to routine; a Michaelis-Menten model is left on
  Picard and unchanged.  With both schemes
  given their best extrapolation level, that division holds: Picard is ahead on
  a non-stiff model at working tolerances and the exponential Rosenbrock step is
  ahead on a stiff one, and at a delivered error of 1e-8 on a non-stiff model.
  `"exprb"` runs at fourth order or above, since its error estimate comes from
  the extrapolation column and the third-order one is not reliable enough to
  size a step from.

- `method="indLin"` extrapolates further when it pays.  Each relinearization
  step could previously be raised from second to third order by running it also
  at half length; it can now use a Romberg column of up to four entries (`h`,
  `h/2`, `h/4`, `h/8`) for up to fifth order, at 3, 7 and 15 fixed-point solves
  per step.  `indLinRichardson="auto"` (the default) raises the level as the
  step the controller settles on crosses each break-even.  `"always4"` and
  `"always5"` force the new levels.  On a 200-subject Michaelis-Menten solve
  this halves the time at `atol=rtol=1e-8`, and on a single subject at `1e-12`
  it is over seven times faster than the third-order step.

- `indLinRichardson="auto"` keeps the extrapolation level it has earned for the
  rest of the subject, instead of dropping back to second order at every
  observation and re-earning it.  A step that only needs a few relinearizations
  never reached the break-even, so a model observed at a dozen times ran most of
  its profile at second order however low the break-even was set: on a
  200-subject Michaelis-Menten solve the default took 0.626 s to reach a
  delivered error of 1e-4 where forcing the fourth-order column took 0.098 s.
  It is now 0.112 s, and 0.341 s rather than 0.646 s at 1e-6.  The break-evens
  themselves are also measured rather than derived, and differ between the
  fixed-point and exponential-Rosenbrock steps, whose costs per level differ.  A
  model whose forcing reads no state is unaffected: the matrix exponential is
  already exact for it and there is nothing to extrapolate.

  Two consequences for anyone reading step counts.  A loose tolerance now does
  use extrapolation -- it turns out to pay there too, taking fewer steps than
  the second-order path rather than the same number -- and the delivered error
  at a loose tolerance is much smaller than before, so a ratio of errors across
  a tolerance sweep is no longer a way to read off the order of the default
  path.  Use `indLinRichardson="never"` for that.

- `rxSolve(indLinMatExpType="taylor")` adds a Taylor scaling-and-squaring
  matrix exponential, which needs no linear solve; its degree is chosen per
  call from the norm.  It is as accurate as the default `"expokit"` on every
  problem tested, including a linear system where `"Al-Mohy"` at its default
  order is six orders of magnitude worse.  The default is unchanged: profiling
  puts all of LAPACK at roughly 3% of a solve, so avoiding the linear solve does
  not pay on nonlinear problems.

- `$counts$dadt` and `$counts$jac` report the matrix exponentials computed and
  reused for a `method="indLin"` solve.  Both counters were previously unused on
  this path.

- `method="indLin"` no longer uses the R API from inside the parallel solve.
  The Al-Mohy matrix-exponential backend took its workspace from `R_alloc`, and
  the default expokit backend warned through `RWarn` on a singular Pade
  denominator; neither is safe from a worker thread.  The singular case also
  used to continue with an unfinished matrix, and now reports and returns zeros.

- `method="indLin"` no longer throws from inside the parallel solve.  Two code
  paths in the inductive-linearization solver raised an R-level error from a
  worker thread, which crashes the session rather than reporting an error; both
  now report through the usual per-subject error flag.

- An `indLin(<state>) <- <expr>` forcing that references a compartment is now
  evaluated at that compartment's current value.  The generated forcing
  function took no state vector, so the compartment kept its `NA_REAL`
  declaration and any state-dependent forcing (e.g. Michaelis-Menten
  elimination, `indLin(central) <- -vmax*central/(km+central)`) solved to `NA`
  under `method="indLin"`.  A forcing that references no state is unchanged.

- `method="indLin"` iterates again, so it is inductive linearization rather than
  one relinearization per `hmax` substep.  Within each substep the matrix and
  the forcing are rebuilt at the latest iterate while propagation restarts from
  the substep's starting state, until the states reported by
  `rxModelVars(m)$indLin$wIndLin` stop moving to within `atol`/`rtol`.  Plain
  Picard iteration only barely contracts once the substep is comparable to the
  forcing's own time scale -- a Michaelis-Menten forcing with no linear
  elimination sits right at the stability boundary, oscillating for ~1e5 passes
  -- so each step is relaxed by a secant estimate of the iteration's contraction
  ratio.  Relaxation does not move the fixed point, so the converged answer is
  the undamped one.  Models with no forcing, or with a forcing that reads no
  state, keep the single-pass path and are unchanged.

- Converting an ODE model to `matExp()` form (`rxToIndLin()`, and therefore
  `rxode2(..., indLin = TRUE)` and `method="indLin"`'s auto-conversion) now puts
  the nonlinear part of a right-hand side into an `indLin()` forcing instead of
  into a rate constant.  A rate constant that reads a compartment is not a rate
  constant -- the matrix exponential assumes the rate matrix is constant in the
  states -- and burying the nonlinearity there meant the solver could not
  iterate it.  Michaelis-Menten elimination now converts to
  `indLin(central) <- -vmax*central/(km + central)` with an empty rate matrix
  rather than to `k_central_output = vmax/(km + central)`.  This also removes a
  rate constant that was singular when the compartment was empty.  Because these
  models now reach the iterating solver path, they are far more accurate: a
  one-compartment Michaelis-Menten solve that was about 70% off at the default
  settings is now within about 0.01%.  `rxIndLinStrategy()` and
  `rxIndLinState()` no longer affect the conversion, since no way of factoring a
  multi-state product yields a state-free rate constant; both are kept so
  existing code keeps working.

- `rxSensMatExp()` (`rxToIndLin(calcSens=)`) splits the system the same way.  It
  used to take its rate matrix from the full Jacobian, so for a nonlinear model
  every rate constant read a compartment and the propagated primal was `A(X).X`
  rather than `f(X)`.  The nonlinear part now rides in an `indLin()` forcing, and
  each sensitivity compartment gets its own forcing
  `d(f)/dp + (df/dy).S^p`, at first and second order.  A state-free input term
  (`d/dt(x) = k0 - ke*x`), which the Jacobian never saw, is carried too instead
  of being dropped.  Michaelis-Menten forward sensitivities now match the
  ordinary ODE `calcSens` path, and since the rate matrix is constant the
  matrix exponential is cached across substeps.  A rate constant that reads a
  compartment is a parse error for a sensitivity model as well now.  Third-order
  sensitivities do not yet get a forcing contribution.

- `eventSens="jump"` gets the right event-time (`alag`) jump sensitivity on a
  `matExp()` model that has an `indLin()` forcing.  The `replace()`/`multiply()`
  jump rows need the right-hand side at the pre-event state, which was taken
  from the model Jacobian dotted with the state -- correct only while the whole
  right-hand side is the rate matrix times the state.  With a forcing it is
  short by the forcing's own contribution, which on a Michaelis-Menten model put
  those sensitivities about 3.6% out.  It now comes from the rate matrix and the
  forcing function directly.  This also affects hand-written `indLin()` models,
  not only the ones `rxSensMatExp()` generates.

- `rxSolve(indLinRichardson=)` Richardson-extrapolates each `method="indLin"`
  relinearization step, raising it from second to third order: the step is run
  once whole and twice at half length, and since a second-order step has a
  quarter the error at half the length, the two answers together cancel it.
  That costs three fixed-point solves per step instead of one, so it only pays
  once the tolerance is tight enough that taking far fewer steps outweighs it.
  `"auto"` (the default) decides per interval: after the first accepted step it
  compares the step the controller settled on against what is left of the
  interval, and switches when finishing at that step would take more than 27 of
  them -- the break-even point, since a second-order step needing `N` steps
  becomes a third-order one needing about `N^(2/3)` at three times the cost
  each.  `"always"`/`TRUE` and `"never"`/`FALSE` force the choice.  On a
  Michaelis-Menten model the switch-over lands at about `atol=rtol=1e-5`; at
  `1e-8` `"auto"` takes 544 steps where the second-order step takes 12,865.

- `rxSolve(indLinStepSearch=)` and `rxSolve(indLinMaxIter=)` control the
  fixed-point iteration `method="indLin"` runs inside each relinearization step.
  `indLinStepSearch="secant"` (the default) estimates the iteration's
  contraction ratio from the last two residuals and relaxes by it, which costs
  nothing extra and is what makes an oscillating iteration converge at all;
  `"exact"` spends one more matrix exponential per iteration to locate the
  residual-minimizing factor in closed form; `"none"` is plain, undamped Picard.
  All three converge to the same answer -- relaxation does not move the fixed
  point -- so the choice trades iterations against work per iteration; on a
  Michaelis-Menten model the default is about five times faster than `"none"`.
  `indLinMaxIter` (default 20) caps the iterations per step; running out is not
  an error, since the iteration contracts in proportion to the step and the
  solver reads it as a step that is too long.

- A `matExp()` rate constant that depends on a compartment is now a parse error
  rather than a silently invalid model.  The matrix exponential is only correct
  when the rate matrix is constant over the step, so a `k_from_to` that reads a
  state breaks the method's central assumption; the error names the constant and
  the compartment it reaches and points at `indLin()`, which is where a
  state-dependent term belongs and where the solver can iterate it.  This
  applies to sensitivity models built by `rxSensMatExp()` as well.

- `method="indLin"` chooses its own relinearization step for models with a
  state-dependent `indLin()` forcing, instead of subdividing each interval
  evenly by `hmax`.  The forward answer (matrix built at the step's starting
  state) and the converged backward answer bracket the truth symmetrically, so
  their difference is a local error estimate that costs nothing extra; the step
  is then chosen from it the same way the other adaptive solvers choose theirs.
  `atol`/`rtol` and `maxsteps` now control the accuracy of these models and
  `hmax` only bounds the step.  An iteration that will not converge is treated
  as a step that is too long and is retried shorter rather than reported, so
  stiff forcings that previously failed outright now solve; non-convergence is
  reported only once the step or the step budget runs out.  `$counts$slvr`
  reports the relinearization steps actually taken, where it used to read zero.
  One consequence worth knowing: as with every adaptive method, the solution is
  now a piecewise function of the parameters, which adds a little noise to
  finite-difference gradients taken through it.

  Each step also advances on the average of the two answers the error estimate
  is built from, whose leading errors are equal and opposite, so what is
  propagated is second order where either alone is first.  This costs nothing --
  both are already in hand -- and it is what brings the step count down: the
  error now falls roughly in proportion to `atol`/`rtol` rather than to their
  square root, so the work needed for a given accuracy grows like
  `1/sqrt(error)` instead of `1/error`.  On the Michaelis-Menten model above,
  matching the accuracy the old scheme delivered at its default now takes about
  a twentieth of the steps, and the gap widens the more accuracy is asked for.

  The forward answer is evaluated at the step's starting time as well as its
  starting state, so that it and the converged answer really are the two ends of
  one quadrature.  Evaluating both at the step end cancels the state error but
  leaves the explicit-time error, which silently dropped any forcing that reads
  `t` back to first order: on a Michaelis-Menten model with an `exp(-t)` input
  the error at `atol=rtol=1e-9` falls from 4.6e-03 to 1.1e-07.

### Serialization

- A saved solver state now round-trips the `indLin()` convergence set
  (`op->indLin`, from `rxModelVars()$indLin$wIndLin`).  Only its length was
  written, so restoring a state for a model with an `indLin()` forcing left the
  set itself empty and the relinearization iteration indexed a null pointer.

- A saved solver state now round-trips the initial-condition and scale vectors
  it claims to.  Their lengths were taken from the distance to the next pointer
  in the `gsolve` slab rather than from the vectors themselves, so they spanned
  the intervening `lhs` and tolerance blocks and no state could be restored at
  all: `rxLoadState()` failed with a size mismatch for every model.  The two
  lengths now travel with the state, which is what the format version is bumped
  to 3 for; a state written by an earlier version is rejected with a message
  asking for it to be re-saved.

### Installation / linking

- On Windows, `STAN_THREADS` and the TBB link are kept when building against
  `RcppParallel` >= 6.2.0, which ships `tbb.dll`/`tbbmalloc.dll` with the
  package again.  `configure` now decides whether to strip the TBB flags by
  looking for the TBB library in `RcppParallel`'s `lib` directory rather than
  by the shape of the `-L` flags it emits, so the TBB-less build introduced in
  5.1.6 is used only with `RcppParallel` 6.0.0--6.1.1, which shipped no TBB
  library on Windows.  (The 5.1.6 release notes had this backwards:
  `RcppParallel` 6.2.0 restored the TBB library on Windows rather than
  dropping it.)

# rxode2 5.1.6

## New features

- Added the event ("jump") sensitivity shape to rxode2's linked
  function-pointer API, so a downstream package can install a model's shape
  from C++ without an R round trip: `rxode2EventSensLoadFull()` (all six dims,
  where the older `rxode2EventSensLoad()` omitted `nParam3`/`useCalcJac`),
  `rxode2EventSensGetDims()`/`rxode2EventSensSetDims()`,
  `rxode2EventSensSetActive()`, `rxode2EventSensDeactivate()`, and
  `rxode2EventSensShapeSize()`/`rxode2EventSensShapeSave()`/
  `rxode2EventSensShapeRestore()`, which snapshot and reinstall a whole shape
  (dims plus the model's dosing-derivative function pointers) through a
  caller-owned opaque buffer.  This lets several peer models with different
  shapes be solved through one shared solve pool, installing each batch's
  shape and restoring the previous one afterwards.

- Added `setIndCmt()` to the function-pointer API, the writer counterpart of
  `getIndCmt()`, so a downstream package can re-base the per-observation `CMT`
  covariate without reaching into `op->cmtCov`/`ind->cov_ptr` by field.
  `getIndCmt()` reports a missing `CMT` as `NA_INTEGER`, distinct from the `1`
  it returns for a model with no `CMT` covariate at all (where every observation
  really is compartment 1), so a caller re-basing the column can leave missing
  rows alone.

## Bug fixes

### Compilation cache

- The compiled-model cache key now includes `eventSensCode`, so two builds of one
  model whose generated C differs -- event sensitivities on vs off -- no longer
  share a `.c`/`.so` path in the rxode2 cache directory.  Previously the second
  build overwrote the first while any model object created earlier kept resolving
  its entry points by name, so it silently began executing the other variant: the
  declared `lhs` width was unchanged but most slots were never written, and
  `rxSolve()` returned whatever was left in the buffer.  A model with no
  event-sensitivity code keeps exactly the prefix it had, so no existing cache
  entry is invalidated (#1171).

### Dependencies

- The suggested `xgxr` is now required to be `>= 1.1.6`.  Its
  `xgx_scale_x_log10()`/`xgx_scale_y_log10()` return the `ggplot2` scale
  itself from that version on, rather than a length-one list wrapping it.

### Installation / linking

- Fixed the Windows build against `RcppParallel` 6.2.0, which no longer ships
  the TBB library there (6.0.0 still built).  `configure` already dropped the
  `-ltbb`/`-ltbbmalloc`
  link flags when they are unavailable, but still compiled with `-DSTAN_THREADS`
  and `-DRCPP_PARALLEL_USE_TBB=1`, which pulls `stan::math`'s `ad_tape_observer`
  (a `tbb::task_scheduler_observer`) into the objects and left undefined
  references to `tbb::detail::r1::observe` at link time.  Those defines are now
  dropped together with the link flags, `stan-math`'s `init_chainablestack.hpp`
  is kept out of the build, and the main thread's AD tape -- which that
  observer would otherwise have created -- is constructed in `src/linCmt.cpp`
  instead (by Jeroen Ooms).

### Solving

- Fixed heap corruption when `simeta()`/`simeps()` resample inside a solve.
  Both go through `simvar()`, which reseeded its threefry stream with
  `getRxSeed1()`; unless `rxSetSeed()` had been called that draws from R's own
  random number generator, which allocates R objects and can trigger a garbage
  collection.  Doing so from an OpenMP worker thread corrupted R's heap, and
  the session then failed later in an unrelated place (`cannot get data pointer
  of 'NULL' objects`, `'rho' must be an environment`, `corrupted double-linked
  list`, or a segfault).  The in-solve resample now draws from a per-thread
  engine seeded on the main thread and touches no R API.

- The `simeta()`/`simeps()` resample no longer replays the simulated
  parameters.  Its engines are keyed off a threefry draw rather than off the
  `runif()`-derived seed handed out at solve setup, which `rxSolve()` goes on
  to reuse for the simulated `omega`/`sigma` deviates; a resampled `eta` could
  therefore come out exactly equal to another subject's simulated `eta`.

# rxode2 5.1.5

## New features

- `library(rxode2)` is faster.  `.onLoad()` no longer calls
  `requireNamespace()` on the suggested packages (`pillar`, `tibble`,
  `arrow`, `dplyr`, `nlme`, `units`, `digest`) before registering their
  S3 methods.  The registration helper already installs an `onLoad`
  hook when the other namespace is not loaded yet, so the eager loads
  only added startup cost; the methods are still registered at the same
  point in time from the user's perspective.

- `rxControl(sigdig=)` now derives the ODE solver tolerances with one
  solver-independent formula -- the same for stiff, non-stiff and auto-switching
  solvers.  The `rtol` exponent IS `sigdig` and `atol` sits three orders below it:
  `rtol = 10^(-sigdig)` and `atol = 10^(-sigdig-3)`.  The sensitivity tolerances
  match the main solve (`rtolSens = rtol`, `atolSens = atol`), since gradients and
  covariances are built from them, and the steady-state tolerances run one order
  looser (`ssRtol = ssRtolSens = 10*rtol`, `ssAtol = ssAtolSens = 10*atol`).  This
  matches how `nlmixr2est` derives solver tolerances from its optimization
  `sigdig`, so the same `sigdig` means the same thing whether it is used for
  estimation or for a plain `rxSolve()`.

  `sigdig` remains `NULL` by default and continues to have no effect unless you
  pass it, so solves that do not name `sigdig` are unchanged.

  Two notes for callers who do pass it.  First, the mapping is keyed to `sigdig`
  as a request for that many significant digits, which for small `sigdig` is
  *looser* than what the previous symmetric `atol = rtol = 0.5*10^(-sigdig-2)`
  gave: at `sigdig = 4`, `rtol` moves from `5e-7` to `1e-4`, which is also looser
  than the `1e-6` default `rtol` (`atol` moves the other way, from `5e-7` to
  `1e-7`).  If you were using `sigdig` to tighten a solve, raise it or set
  `atol`/`rtol` directly.  Second, each tolerance is resolved independently and
  only when you did not supply it, so an explicit `atol`/`rtol` overrides the main
  solve but does not propagate to the sensitivity or steady-state tolerances --
  set those directly if they should change too.
- The SUNDIALS public headers are now vendored into the package
  (`src/sundials_inc/`) alongside the already-vendored SUNDIALS C sources,
  and the `LinkingTo: sundialr` dependency has been dropped.  This
  guarantees the vendored sources always compile against headers from the
  same SUNDIALS release, instead of silently drifting when sundialr updates
  its bundled SUNDIALS (#1155).  The vendored include is injected via
  `PKG_CPPFLAGS` so it precedes the LinkingTo include flags; otherwise the
  older SUNDIALS copy bundled inside StanHeaders would shadow it.

- `rxSerialize()` now writes the base R types only (`"xz"`, `"bzip2"`,
  `"base"`); `qs2` is no longer a write format.  `rxDeserialize()` still reads
  `qs2`/`qdata`-serialized data and base91-encoded strings, so objects stored
  by earlier versions remain readable.  Test data was converted from `.qs2` to
  `.rds`.

## Bug fixes

### Serialization

- `qs2` moved to `Imports`.  `rxDeserialize()` used it without declaring it
  anywhere, and because the call sites named the package as a string, the
  dependency was invisible to `R CMD check` as well.  Environments that build
  their library from the declared dependency graph could therefore end up
  without `qs2` and fail to read objects stored while `qs2` was an allowed
  `rxode2.serialize.type` -- for example the `origData` slot of fits saved by
  earlier versions.  `qs2` is now only ever read, never written.

### Error models / transformations

- The first and second derivatives of the Yeo-Johnson transform (`rxTBSd()` and
  `rxTBSd2()`) had the wrong sign for negative values when `lambda` was exactly
  `2`.  There `yj(x) = -log(1 - x)`, so the derivatives are `1/(1 - x)` and
  `1/(1 - x)^2`, both positive; the special case returned them negated, which
  also contradicted the general formula's limit and made the derivative
  discontinuous in `lambda` at `2`.  Since Yeo-Johnson is monotone increasing,
  its first derivative must be positive everywhere.

- Review of the fix above found further errors in the same transform family
  (all pre-existing, none introduced by that fix):
  - `rxTBSd2()` returned a wrong second derivative for the `logit` transform
    (an algebra error in the closed form) and for the composed
    `logit + yeoJohnson` / `probit + yeoJohnson` transforms, where the chain
    rule used the first Yeo-Johnson derivative in place of the second and
    dropped the inner-transform curvature term.
  - `rxTBSi()` did not invert the composed `logit + yeoJohnson` /
    `probit + yeoJohnson` transforms: it applied the forward Yeo-Johnson
    transform (or skipped it entirely) instead of the inverse, so
    `rxTBSi(rxTBS(x))` did not return `x` for `lambda != 1`.  This affected
    simulation back-transforms of those error models.
  - The `lambda` gradient of the transform log-Jacobian (`powerDL`, used by
    estimation routines) was wrong on the negative Yeo-Johnson branch
    (`-log1p(x)` instead of `-log1p(-x)`, `NaN` for `x < -1`), returned a
    spurious `0` at exactly `lambda == 1` for `boxCox`/`yeoJohnson`, was
    missing the `probit + yeoJohnson` case (returned `NA`), and returned a
    spurious `log(x)` (instead of `0`) for the lambda-free `lnorm` transform.
    The log-Jacobian itself (`powerL`) clamped the wrong term in its `logit`
    guard, giving an unprotected `log(0)` at the upper bound.
  - For `boxCox`/`lnorm`, `rxTBSd()` and `rxTBSd2()` returned the clamp
    constant `sqrt(.Machine$double.eps)` itself for `x` at or below the clamp
    instead of clamping `x` and evaluating the derivative formula, making the
    derivatives discontinuous (and ~15 orders of magnitude too small) at the
    boundary.  The clamp now feeds the usual formula, matching how every other
    transform in the family handles the guard.

### Estimation / symengine translation

- The symbolic derivatives of the relational operators (`>`, `<`, `>=`, `<=`)
  are now centered on the discontinuity `a == b`: the `atanh(2*tol - 1)` shift
  that placed the smoothed nascent-delta bump at `a - b ~ +/-0.46` was removed.
  Since the forward pass evaluates relationals as hard booleans, the shifted
  bump gave sensitivity/exact-gradient consumers (e.g. FOCEI's analytic
  gradient paths) a spurious derivative in a band next to the threshold; the
  centered rule makes the derivative consistent with the forward value.  This
  also makes the first derivatives of `abs()`, `min()`, and `max()` exact away
  from the boundary (#1159).

### Solving

- Fixed heap corruption when `OMP_NUM_THREADS` is set below the number of
  cores a solve asks for -- as it is on CRAN check machines, which set
  `OMP_NUM_THREADS=2`.  The extra-dosing pools were sized once when the package
  loaded, from `omp_get_max_threads()` (which honors `OMP_NUM_THREADS`), but
  they are indexed by the solving thread id, which is bounded by `op$cores`;
  `rxSolve(cores=)` overrides `OMP_NUM_THREADS` through OpenMP's `num_threads`
  clause.  Every thread past the first `OMP_NUM_THREADS` therefore wrote off
  the end of those arrays, corrupting the heap and crashing the session later
  in an unrelated allocation.  The pools now grow to cover `op$cores` at solve
  setup, like the other per-thread pools.

- Fixed an out-of-bounds thread index that could segfault a solve.  The
  internal thread id used to slice the per-thread solving buffers was not
  bounded by the number of threads those buffers were allocated for
  (`op$cores`).  A larger id read past the end of `gInfusionRate[]` -- an
  array of pointers -- and the resulting garbage pointer crashed
  `iniSubject()`; the flat per-thread arrays were silently overrun in the
  same way.  The id is now clamped to the last valid slot, matching what
  `rx_get_thread()` already did.

- Fixed a cross-subject leak in batched multi-subject `linCmt()` solves: the
  per-thread inter-event amount buffer was never cleared between subjects, so
  with `cores < nSub` every subject after the first on a thread could start
  from the previous subject's compartment amounts (surfaced by a modeled
  `alag()`) (#1153; by Hidde van de Beek).

- `delay()`/`past()` models containing an `if`/`else` block failed to solve
  with `unexpected 'else'`: the DDE helpers parsed the `rxNorm()` text
  directly, which puts `}` and `else` on separate top-level lines; the
  normalized text is now parsed wrapped in a `{ }` block.  In addition, a
  `past()` history inside an `if`/`else` branch is now rejected with a clear
  error (it was invisible to validation), and delay-duration root-variable
  resolution now sees assignments made inside `if`/`else` branches (#1151).

### Event tables

- `ev$id` on an event table now returns the per-row `id` column (matching
  `as.data.frame(ev)$id`) instead of the unique subject ids, so idiomatic
  subsets like `ev[ev$id == 3, ]` and per-subject assignments like
  `ev$wt <- 50 + 20 * ev$id` no longer silently recycle a short vector; the
  unique ids remain available via `ev$env$ids`.  `[.rxEt` now errors on a
  logical row index whose length matches neither 1 nor the number of rows,
  and columns assigned with `ev$col <- value` (new covariates as well as
  previously hidden canonical columns such as `cmt`) now round-trip through
  `as.data.frame(ev)` (#1154).

- Columns assigned explicitly on an event table (`ev$wt <- 70`) are now shown in
  the tibble printed by `print(ev)`, in `ev$get.EventTable()`, and in the
  compressed preview printed for `ev$get.dosing()`/`ev$get.sampling()`, matching
  `as.data.frame(ev)`.  They were kept and used when solving, but never
  displayed, so they looked like they had disappeared (#1154).

- `ev$get.dosing()` and `ev$get.sampling()` now print the same columns
  `print(ev)` does regardless of how the event table is stored internally.
  Previously an un-grouped table printed every internal column, including
  hidden ones such as `low`/`high`/`dur` and covariates that only rode along
  with an imported data frame, while a compressed one printed only the shown
  columns.  Every column is still present on the returned data frame for
  programmatic access, a column added or renamed on the returned frame still
  prints, and `dplyr` verbs turn it back into a plain data frame the way they
  already did for `rxEt` -- including the column verbs (`select()`,
  `relocate()`), which subset with `[` rather than going through
  `dplyr_reconstruct()`.

- Explicitly assigned columns now survive a round trip through a data frame.
  `as.data.frame()` tags them in a `rxEtExtraCols` attribute that `et()`,
  `as.et()` and `$import.EventTable()`/`$importEventTable()` read back, so
  `et(as.data.frame(ev))` keeps showing `wt` instead of demoting it to a hidden
  imported covariate.  A data frame built by hand carries no tag, so its
  covariate columns stay hidden as before.

- `as.data.frame()` on an event table still hides covariate columns that simply
  rode along with an imported data frame (`et(data)`), while showing columns
  assigned explicitly on the event table (`ev$wt <- 70`, #1154).  The covariate
  is still used when solving.  Showing every non-canonical column broke code
  that imports events and then joins its own covariates back onto
  `as.data.frame(ev)`, since the join produced `wt.x`/`wt.y` and the model
  parameter disappeared.

### Model compilation

- The `parsed_md5` of a model no longer depends on how many models were built
  before it in the session.  `linCmtSens` was folded into the hash but only
  assigned *after* the model was parsed, so the first build of a session hashed
  with an unset value and every later build hashed with the *previous* call's
  value.  Because the compiled DLL is named from `parsed_md5`, the same model
  could get two different cache keys (and hence a redundant recompile) depending
  on build order.  It is now set before the parse.

### Installation / linking

- `RcppParallel` is now a runtime import (added to `Imports` with an
  `importFrom`), so its shared library is loaded into the process before
  `rxode2`'s.  `rxode2` links against `RcppParallel` (`-lRcppParallel`); with
  `RcppParallel` only in `LinkingTo` its DLL was not guaranteed to be loaded
  first, so on Windows `library(rxode2)` could fail with `LoadLibrary failure:
  The specified module could not be found`.  This surfaced with RcppParallel
  6.0.0, which statically links TBB and no longer ships the `tbb.dll` stub that
  previously happened to pull the library in.

- On Windows with RcppParallel >= 6.0.0 (which statically links TBB through
  Rtools and no longer loads `tbb.dll`), the stale `-ltbb`/`-ltbbmalloc` flags
  and the `-L` path to RcppParallel's old dynamic TBB directory that
  `StanHeaders::LdFlags()` still emits are stripped at configure time, so the
  rxode2 DLL no longer records an unresolvable runtime dependency on
  `tbb.dll`.  The strip is keyed to that stale `-L<RcppParallel/lib>`
  signature, so a future StanHeaders that emits corrected flags -- or a
  user-supplied TBB via `TBB_LINK_LIB`/`TBB_LIB` -- is left untouched (#1161).

- The vendored SUNDIALS `*NewEmpty` constructors now allocate with `calloc`
  instead of `malloc`, so any struct fields added by a newer SUNDIALS
  release are NULL (and safely ignored) rather than uninitialized (#1155).

# rxode2 5.1.4

## Bug fixes

### Model piping

- Model piping no longer shares the `meta` environment by reference between
  the original and the piped model.  `.newModelAdjust()` assigned the previous
  model's `meta` env directly (to retain sticky items), so both models shared
  one env -- including the cached simulation model (`$meta$.simModelBase`).
  Whichever model was solved first cached its simulation model for both, so a
  piped model could silently drop an appended compartment/state (e.g. a
  `nonmem2rx` import: `mod %>% model(d/dt(AUC) <- f, append=TRUE)`) or the
  original model could silently gain the piped model's states/estimates.  The
  meta env is now copied via `.copyEnv()` (which drops `.simModelBase`), so
  each model keeps its own cache.

### Compilation

- Silenced the CRAN `-Wlto-type-mismatch` warnings seen with LTO/gcc builds.
  The `rxSolveWarnPush()` forward declaration in `src/init.c` was missing the
  variadic `...` of its definition, and the ODEPACK `/DLS001/` common block was
  declared with two inconsistent (but memory-equivalent) layouts across the
  LSODE/LSODA step routines.  Both are now declared consistently; the fixes are
  layout-preserving and the Fortran solvers produce identical results.

# rxode2 5.1.3

## New features

- `rxOptExpr()` gains `chunkLines` and `parallel`, to optimize a large
  machine-generated model (a sensitivity- or Jacobian-augmented model) in
  contiguous cost-balanced chunks rather than in a single pass.

- Delay differential equations: `delay(state, T)` evaluates an ODE state at
  `t - T` (Monolix semantics), with `past(state, T) <- expr` defining the
  pre-history.  Delayed states are interpolated from the solver's dense output;
  delay models default to the `"dop853+ros4"` composite and cap the step size
  to the smallest delay.  The dense-output/history machinery is adapted from
  the 'dde' package (Rich FitzJohn, Wes Hinsley, Imperial College), whose
  authors are added as contributors.

- Forward sensitivities for delay models, so `delay()` models estimate with
  gradient-based methods such as FOCEi.  Parameter-dependent delays are
  supported at first order (`rxDelayD()`); second/third order are generated for
  constant delays (`rxDelayD2()`/`rxDelayD3()`) and rejected for
  parameter-dependent delays (use a numeric or Gauss-Newton Hessian there).

- Many new ODE solver methods: a large suite of explicit Runge-Kutta tableaus
  (orders 3-14), stiff Rosenbrock and implicit Runge-Kutta methods (`"ros43"`,
  `"ros6"`, `"radauiia5"`, `"gauss6"`, `"sdirk43"`, `"backwardEuler"`, ...),
  symplectic steppers, SUNDIALS CVODE (`"cvode"`, linear solver selectable via
  `cvodeLinSolver=`), and LSODE/BDF.  Implicit methods auto-generate an
  analytic Jacobian.  New helpers `rxIsStiff()`, `rxIsNonStiff()`,
  `rxIsImplicit()`, `rxIsDense()`, and `rxIsAutoSwitch()` classify methods; see
  the new "ODE solvers" article.

- AutoSwitch composite methods written `"primary+secondary"` (e.g.
  `"dop853+ros4"`): a non-stiff primary with reactive fallback to a stiff
  secondary, in both the standard and dense-output paths.

- Adjoint sensitivity solving: `rxSolveAdjoint()` and `rxSolveAdjointRk4()`
  return the same `rx__sens_<state>_BY_<param>__` output as forward
  sensitivities via a backward sweep.  Exact discrete adjoints exist for the
  one-step methods (`"s"` suffix, e.g. `"dop853s"`), `"liblsodaadj"`, and
  `"cvodesadj"`, including event jumps (dose/reset/replace/multiply), modeled
  `alag`/`rate`/`dur`, and steady state.  Stiff adjoint and forward-sensitivity
  solvers integrate the augmented system with its analytic Jacobian.

- Jump sensitivities for dosing events (based on
  https://github.com/dkaschek/EventSensitivities), replacing finite differences
  as the default (`rxode2.eventSens` option: `"jump"`, `"fd"`, `"both"`).
  Hybrid jump sensitivities are used for matrix exponential and `linCmt()`
  models (up to 3rd order for the ODE/matrix exponential cases).

- Automatic conversion of linear ODE models to `linCmt()` at solve time
  (`rxSolve(..., useLinCmt=TRUE)`, the default), passing detected PK parameters
  explicitly.  Handles a central sub-system with an output-only peripheral
  observable; systems `linCmt()` cannot represent stay explicit ODEs, and a
  conversion that will not compile falls back to the ODE (only rxode2).

- Adaptive dosing helpers (`bolus()`, `infuse()`, `replace()`, etc.) now work
  inside `linCmt()` and mixed `linCmt()`+ODE models, with Jacobian handling of
  the dosing events; `odeToLin()` preserves and renames them when converting.

- `linCmt()` sensitivity (`linCmtB`) solves now run in parallel across subjects
  on the default forward-mode AD Jacobian path (`linCmtSensType="AD"`), which is
  stack-local with no shared Stan arena.  The reverse-mode AD (`"ADr"`) and
  finite-difference paths remain single threaded.

- Inductive linearization and matrix exponentials rewritten with a more
  NONMEM-like interface (automatic ODE->syntax translation retained) and
  symbolic-differentiation gradients.

- Added a forward automatic-derivative linear compartment model.

- `ar(cor)` residual term simulating continuous-time AR(1) residuals for
  normal, t, and cauchy error models, addable per endpoint alongside any
  transform; `cor` is in `[0, 1)` and the lag correlation decays as
  `cor^(time gap)` (Karlsson, Beal and Sheiner 1995).  Estimation is supported
  in nlmixr2est (nlm and focei families).

- `lag0()`/`lead0()`/`diff0()`: like `lag()`/`lead()`/`diff()` but return `0`
  instead of `NA` when there is no prior/following record.  A calculated
  variable may now reference itself through `lag()`/`lag0()`/`diff()` (a
  first-order recurrence); a non-lag self-reference is still a required input
  parameter.

- `rxOmegaVarCovDeriv()`: non-Cholesky `Omega` path returning `Omega^{-1}`,
  `log|Omega|`, and their first/second derivatives with respect to each free
  variance-covariance element.

- `rxExpandSens3_()` generates analytic third-order forward sensitivity
  equations; `.rxSens()` gained a `vars3` argument.

- For downstream packages: `rxSetSolveAtolRtol()`/`rxGetSolveAtolRtol()` in the
  C function-pointer API, and `setRxThreadId()` so a package can drive the
  per-subject solve from its own OpenMP team.

- `rxTest()` test blocks now muffle stray progress messages (e.g. "calculate
  sensitivities"); set `options(rxode2.test.verbose = TRUE)` to see them.
  Messages asserted with `expect_message()` are unaffected.

- `coef()` methods for `rxUi` models (and model functions).  By default
  `coef()` returns the fixed-effect (`theta`) estimates; `coef(model,
  level = "omega")` returns the random-effect variability matrix and
  `coef(model, level = "all")` returns both.  `nlme::fixef()` continues to
  return the fixed effects.

## Bug fixes

- The C accessors exposed through the function-pointer API (`getRxNsub()`,
  `getSolvingOptions()`, `getSolvingOptionsInd()`, and the other `rx_solve*`
  accessors) no longer segfault when handed a `NULL` or uninitialized solve.
  They fall back to the global solve; a scalar counter/flag accessor (`nsub`,
  `nall`, `nobs`, `npars`, ...) simply reports zero before any solve, exactly as
  before, so downstream code that probes those counts at load time keeps working
  (for example babelmixr2's PopED integration, which queries them from
  `.onLoad`).  An accessor that must dereference a per-subject record
  (`getSolvingOptionsInd()`) instead raises a normal catchable R error stating
  that the solving environment is not set up, rather than dereferencing a `NULL`
  pointer and crashing the R process.  This hardens downstream packages that call
  an accessor before their solve pointer has been populated (for example a cold
  first `nls`/`nlm` fit in `nlmixr2est`).

- A Jacobian entry `df(state)/dy(THETA[n])` or `df(state)/dy(ETA[n])` (a
  bracketed parameter reference, which the grammar accepts) no longer segfaults.
  The synthetic `_THETA_n_`/`_ETA_n_` symbol was never registered, so its index
  stayed `-2` and the model validator read `tb.ss.line[-2]` out of bounds.  This
  crashed `nlm`/FOCEi fits that re-parse their generated `calcJac` model (whose
  parameters are `THETA[n]`) in the residual/table step -- notably for a
  delay-differential-equation model whose delay parameter appears in a product
  of delayed states.

- `past(state, tau)` on a state with no `d/dt(state)` now reports that cleanly
  instead of corrupting the heap.  The error path appended nothing to the
  message buffer and then trimmed a trailing `', ` that was never written,
  moving the write offset before the start of the buffer; the damage surfaced
  as a `double free or corruption` abort on a *later*, unrelated parse rather
  than at the offending model.  The message now names the property
  (`'past(G)' present, but d/dt(G) not defined`), and a property with no
  message branch can no longer underflow the buffer.

- `rxOptExpr()` no longer fails on a model that uses `past(state, tau)` and is
  long enough to be optimized in chunks.  A `past()` line only parses in a
  chunk that also holds the matching `d/dt()`, and sensitivity augmentation
  appends `past()` after every `d/dt()` -- so it reliably landed in a chunk of
  its own.  It is now disguised for the duration of the optimization like any
  other compartment-scoped left-hand side, and restored byte-exactly
  afterwards.  Together with the fix above this unblocks estimating a
  non-constant-history DDE (e.g. the rheumatoid arthritis model of Koch et al.
  2014, J Pharmacokinet Pharmacodyn 41:291-318, Example 6).

- `rxAppendModel()` now warns (instead of erroring) when the appended models
  have no variables in common, so the combined model is still returned; use
  `common=FALSE` to suppress the warning (#520).

- `rxFixPop()` no longer tries to literally substitute a fixed mixture
  proportion (`mix()`).  A mixture proportion must stay a named model-block
  variable, so substituting its value made the re-parse throw from `mix()`
  ("the probabilities in a mixture must be in the model block ..."); a downstream
  caller wrapping `rxFixPop()` in `try()` leaked that error to the console during
  otherwise-successful mixture fits.  Fixed mixture proportions are now excluded
  from the substitution.

- Tests that use datasets from the suggested `nlmixr2data` package (`theo_sd`,
  `warfarin`, `nmtest`) now guard their use with
  `skip_if_not_installed("nlmixr2data")`, so the test suite runs cleanly when
  `nlmixr2data` is not installed (#95).

### Estimation / symengine translation (`rxFromSE()`)

- Convert raw R comparison/logical operators (`>`, `==`, `&`, ...), not only
  their `rxGt()`/`rxEq()` symengine forms; fixes "user function '>' requires 0
  arguments" in FOCEi models with inter-occasion variability
  (nlmixr2/nlmixr2#390).

- Recognize bare relationals on the second conversion pass of a `Subs()` over a
  `Derivative()`; unblocks FOCEi IOV models that also have a between-subject
  eta on a parameter without IOV.

- The numeric-constant canonicalization now evaluates operands in `baseenv()`
  only and guards zero-length results, fixing an "argument is of length zero"
  error and silent substitution of user-workspace variables (#1109).

- A trig function (`sin`/`cos`/`tan`) whose argument is a compound expression
  divided by something (for example `sin(2 * 3.14 * (time - mtime1) / period)`)
  no longer drops its whole argument.  The division branch fell through without
  returning when the numerator was not a single token, so the argument became
  `NULL` and the emitted C code was `sin()` -- which failed to compile with "too
  few arguments to function 'sin'".  Such models (for example an enterohepatic
  gallbladder model with a sinusoidal release) now build and fit
  (nlmixr2/nlmixr2est#513).

### Model parsing / mu-referencing

- Summing two or more population parameters in an expression that has no
  random effect (for example a combined residual error
  `W <- sqrt(sigma.1. + sigma.2.)`) is no longer misreported as
  "2+ single population parameters in a single mu-referenced expression".
  That check now fires only for a genuine mu-referenced expression (one that
  also contains an eta), and the message names the parameters that were
  actually summed instead of the first parameters in the model (#471).

### Delay models

- `calcJac=TRUE` rewriting (also used by the stiff `ros4`/`dop853+ros4` path)
  no longer breaks delay models declaring literal `THETA_n_`/`ETA_n_`
  parameters: constant `~` intermediates stay bound, the literal names are
  restored, and `past()` history lines are re-emitted.

- A state read by `delay()` is always kept as a real ODE, so delayed states
  named like sensitivities (`rx__sens_*`) keep their defining `d/dt()` and can
  use the stiff/dense composite directly.

- Delay models whose analytic Jacobian cannot be generated now fall back to
  `dop853` (dense) instead of `liblsoda`, which recorded no delay history and
  silently returned pre-history values.

- An lhs reading `delay()` is now reported correctly in the output data frame
  (#1140).  The dense delay history was freed at the end of each subject's
  solve, so the post-solve lhs recalculation returned the constant pre-history
  (0) at every record even though the delayed value drove the ODE.  The history
  is now kept until `rxSolveFree()` releases the subject, which also plugs a
  leak on the discrete-adjoint (`rk4s`) path where it was never freed.

### `linCmt()` models

- Fixed a compartment-indexing bug where a model with both an error model and
  an in-equation compartment reference (e.g. `Cp <- peripheral1 / vp`) read an
  unwritten slot.

- `tad(<state>)`/`tlast(<state>)` no longer return `NA` or the wrong value when
  the model also declares an extra `cmt()` for an algebraic observable
  (nlmixr2est#685).

- The automatic `linCmt()` conversion no longer fires on a nonlinear model
  whose nonlinearity is written through a state-derived observable (e.g.
  Michaelis-Menten via `Cc <- central / vc`).

- The automatic `linCmt()` conversion no longer changes results when the event
  data addresses a compartment (in a dose or an observation record) by the
  *name* of an ODE compartment the conversion renames (e.g. an ODE `centre`
  compartment addressed as `CMT = "centre"`, which the conversion renames to
  `central`).  Such a solve now falls back to the original ODE model instead of
  routing the record nowhere and returning all-zero predictions.

- Fixed the automatic `linCmt()` conversion cache reusing the first model's
  initial estimates for a later model that shares the same `model({})`
  equations but has a different `ini({})` block, which made structurally
  identical models with different parameters return identical predictions.

- Fixed the string form of the compartment argument in the adaptive dosing
  helpers (e.g. `bolus(50, cmt = "depot")`).

### Solving

- Zero the LSODA solver work memory on allocation (`alloc_mem`, `calloc` instead
  of `malloc`).  The shared work block (Nordsieck history `yh`, Jacobian
  workspace `wm`, `acor`/`savf`, ...) was left uninitialised and parts are read
  before the integrator writes them on some paths (e.g. a first stiff/BDF step at
  an extreme point), making a solve non-deterministic.  Surfaced by valgrind as
  reads of uninitialised LSODA memory inside FOCEi/impmap inner solves, and
  downstream as an occasional blown-up importance-sampling fit run after a prior
  (parallel) fit.  Solving is otherwise unchanged.

- `lag()`/`diff()` (and `first()`/`last()`) previously returned a constant
  instead of the prior record's value for calculated variables and time-varying
  covariates; they now read the prior record (`NA` on each individual's first
  record) and work through the estimation/symengine path.  Only
  `lag(x, 1)`/`diff(x, 1)` are supported for calculated variables.

- Bug fix for `mix()` models and `iCov` models.

- The `rxMemoryEstimate()` RAM detection no longer calls the defunct
  `utils::memory.limit()` (which warned on every Windows solve); total RAM is
  now queried natively in C (`GlobalMemoryStatusEx` on Windows,
  `sysctl` on macOS, `sysconf` on Linux) and available memory reuses the
  allocator preflight estimate (`rxAvailableMemoryBytes()`).  This also drops
  the `memuse` suggested dependency and the shell-command fallbacks.

- Fixed out-of-bounds heap reads (AddressSanitizer-confirmed; results
  unchanged): `rxSolve()` parameter setup when subjects share one event table
  in an `nsim > 1` sorted solve; `syncIdx()` dose-index lookup; `cvPost()` with
  a 1x1 `omega`; `linCmt.h` `linCmtStan2ssInf8`; `etTran()` `combineDvid`;
  `rxDerived()` `derived1`.

# rxode2 5.1.2

- `geom_cens()` / `stat_cens()` no longer emit "Ignoring unknown
  aesthetics" warnings when censoring aesthetics are mapped.
  Documentation corrected to describe the two supported lowercase
  forms: `lower`/`upper` (both required) or `cens` (with optional
  `limit`). The two forms cannot be mixed, `lower` and `upper` are
  now required together, and `limit` without `cens` is rejected
  rather than silently ignored.

- Checks for `is.loaded()` before loading a rxode2 model.  This helps fix
  the m1 ODR issue shown in nlmixr2est.

- Moved `dim.rxEt()` here instead of in nlmixr2est

# rxode2 5.1.1

- Various low level fixes to allow `nlmixr2est` to have parallelized
  focei.

- Parallelized the `rxode2` data.frame creation.

- Added parallel solving `mirai` for clusters and HPC support.

- Added out of memory solve using `arrow`/`duckdb`.  These
  out-of-memory (`rxSolveOom`) solves behave like a standard
  solved object: it prints the `$params` and `$inits` (mirroring the
  `rxSolve` console output), supports `$`, `head()`, `nrow()`,
  `ncol()`/`dim()` and the usual `as.data.frame()`/`as_tibble()`/
  `as.data.table()` coercions, and exposes the per-subject parameter
  table and initial conditions that are now persisted alongside the
  chunked data.  A DuckDB query layer over the parquet chunks is used
  for lazy access (`head()`, single-column extraction, schema) when
  available.  The chunks can also be queried lazily with `dplyr` (via
  `as.arrow()` or `arrow::to_duckdb()`) so that filtering and
  aggregation are pushed down to the on-disk chunks and a possibly
  out-of-memory result never has to be fully materialized.  The
  storage/query engine can be pinned with the
  `rxode2.oom.backend` option (`"auto"`, `"duckdb"`, `"arrow"` or
  `"rds"`); the option is also forwarded to parallel (`mirai`)
  workers.

- Use ALTREP for `id`, `sim.id`, repeated simulation event columns
  (`evid`, `cmt`, `ss`, `amt`, `rate`, `dur`, `ii`, `time`),
  covariates and kept variables when blocks are identical across
  simulations; falls back to filled out columns when runtime event
  mutation is detected (`evid_()` push growth / per-individual event
  reallocation). Also factors cannot currently be represented by
  altrep, so they are forced to be fully represented.

- Change compile flags and compiler directives for rxode2 models to
  speed up how they run.

- Have a pre-allocated context pool for lsoda in both liblsoda and
  lsoda (faster because memory doesn't need to allocated and
  deallocated so often)

- Change OMP scheduling to dynamic to try to help load-balance the ode
  solving per subject.

- Simulation normal random numbers before integrating them into your
  solve.

- Add `evid_()` function to allow arbitrary doses and observations in
  a rxode2 model.

- Add `splitBolus()` function to split or relocate doses in the final
  output.  This is done at translation time (but is respected by
  `evid_()`) so in general is a bit faster then arbitrary doses in an
  estimation step for `nlmixr2`

- Add `%%` operator to valid rxode2 syntax

- Create per-individual ODE solving tolerances for use in focei.

- Fix potential security and memory-management issues that could lead
  to crashes or undefined behavior including integer overflow

- Change `dop853` to allow per state tolerances and parallel solving
  like `liblsoda`.

- Change `dop853` to be able to use `dense=TRUE` for the 8th order
  dense polynomial interpolation between dosing events.

- Now `dop853` can be parallelized per thread.

- Change mtime state-based dosing to use less memory.

- Add `plogis()` translation inside `rxode2` to it's c-based `expit()`
  functions

- Refactored `et()` to be mostly in R, fixing many issues (#722 , #725, #858,
  #732, #723, #721, and #724) and allowing dosing/sampling windows to
  use `ii`, `addl` and `until` (realized immediately)

- Add `linToOde()` convert `linCmt()` models to ODEs.

- Fix IOV simulation issue observed in #982.

- Fix sticky variable calculation (#1013, #1025)

- More easily identify initial conditions (#948)

- Fix sensitivities in the `linCmt()` that did not match the ODE (#1018, #1012)

- Added in-solve addition of observations (`obs()`), bolus doses
  `bolus()`, infusion doses `infuse()` or `infuseDur()`, system resets
  `reset()`, compartment replacement `replace()`, multiplicaton events
  `multiply()`, and phantom/transit compartment events `phantom()`.
  For more granular control you can also use `evid_()`.

- Refactor string comparison in `rxode2` so that it is actually doing
  an integer comparison when running the ODE solving routine
  (simulation and estimation) instead of using a string comparison.
  It makes using strings like (sex == "male") run faster.

- Add `rxMemoryEstimate()` and `rxMemSummary()` to estimate the amount
  of memory that is required for a rxode2 solve.

- Add `tolFactor`, a per individual change of the tolerances to be
  used in solving. This is used have individualized tolerances from
  `nlmixr2est`.

- Add `serializeFile` as an option to save the rxode2 C fitting data and
  then restore as needed.

- Add out of memory solve capabilities

# rxode2 5.0.2

- Allow state-dependent `dur()`, `rate()`, `alag()`, `mtime()` now
  allow states to modify their behavior.  The state value at the time
  of the event is used to calculate any changes.

- Fix: all six ODE solve loops now use precomputed `timeThread` values for
  event times instead of recomputing via `getTime_()` with `ypNA`, preventing
  NA propagation for any state-dependent lag scenario.

- Export the internal `.rxGetSeed()` and `.rxSetSeed()` for use in the
  `nlmixr2save` package.

- Bug fix for `.copyUi()` with the new format (5.0+) of rxode2 ui models

- With new versions of R, `getOption()` is no longer a bottleneck, so
  syncing to local variables is no longer done internally

- Allow transforms to return `NA`.

- Drop `magrittr` and use `|>` instead of `%>%` in the examples
  (requires R 4.1)

- Change default model serialization to `bzip2` and move binary code
  generation inside of C.

- Fix where getting seed saves/modifies the RNG scope, as well as a bug fix
  for restoring the random seed state

# rxode2 5.0.1

- Change random number generation to always return doubles internally
  as well as no longer take a rxode2 individual structure, this is inferred
  by the thread number.

- Change string representation of model variables to internal binary C
  code (to avoid macOS M1 sanitizer issues with strings).

- Allow user to change the internal serialization type with
  `options("rxode2.serialize.type")`; Currently can be one of "qs2",
  "qdata", "base", "bzip2" and "xz".  This option must be set before
  `rxode2` is loaded, once loaded it keeps the option initially
  set. This is set to `xz` which is from base R, but could be sped up
  with either `"qs2"` (more future proof) or `"qdata"` (a bit faster).

- Removed lsoda `CDIR$ IVDEP` directive, as requested by CRAN.

# rxode2 5.0.0

- Better error for `tad(depot)` when `linCmt()` doesn't include a
  depot compartment.

- Remove `qs` dependency; For rxode2 ui objects, use lists instead of
  serialized objects. The internal C++ code still generates `qs2`
  sterilization objects (#950)

- Fixed translation for censoring/limit to account for a possible
  `CMT` variable before the `CENS` / `LIMIT` column (#951, #952)

- Added `dmexpit()` for getting the diagonal Jacobian.

- Added special handling of `mixest` and `mixunif`.

# rxode2 4.1.1

- Stacking for multiple-endpoint `ipredSim` now matches
  multiple-endpoint `sim`; Issue #929

- Fix occasional `$props` that threw an error with empty properties
  (when using properties like `tad0()`); Issue #924

- Allow mixture models `mix()` to be loaded with `rxS()` as a step to
  support mixtures in nlmixr2's focei; Issue #933.

- Identify the correct transformation type for `iov` variables (#936)

- Fix multiple compartment simulation edge cases where simulations
  were not being performed (#939)

- When referencing `cmt` in models, the variable is forced to be `CMT`
  (related to #939)

- Added ability to use `mixest` or `mixunif` to preserve the selected
  mixture estimates when performing a table step for a nlmixr2 mixture
  model (#942)

# rxode2 4.1.0

- Change rxui `$` evaluation when completing in rstudio, fixes strange
  calculations popping up in `rstudio` (#909)

- Add orphan `rxode2` model unloading when using `rxUnloadAll()`, and
  change the return type to always be a boolean.

- Add `assertRxUiIovNoCor` to assert IOVs have no correlations in them.

- Handle the levels for inter-occasion variability in the ui better (#614)

- Create a new function `mix()` that will allow mixture models to be
    simulated in preparation of mixture support in `nlmixr2`.  This
    allows mixture models to be specified as: `v = mix(v1, p1, v2, p2,
    v3)` where the probability of having `v=v1` is modeled by `p1`,
    `v=v2` is modeled by `p2`, and `v=v3` is modeled by probability
    `1-p1-p2`.

- Created new functions `mlogit()` and `mexpit()` to convert
  probabilities used in mixture models to log-scaled values.
  `mlogit()` converts the probabilities to log-scaled values (using
  root-finding) and `mexpit()` converts the log values into
  probabilities.  The equation for the conversion of log to
  probabilities is $p_i = \frac{exp(x_i)}{1+\sum_{j=1}^{N-1}exp(x_j)}$

- Added new assertion `assertRxUiNoMix` which throws an error when a
  mixture model is present (ie `mix()`)

- Fix for label processing when calling `rxode2(uiModel)`

# rxode2 4.0.3

- For CRAN's m1 ASAN checks of nlmixr2est, loading and unloading the
  same dll or by deleting the dll and recreating the exact same code,
  and then loading the dll will cause the ASAN check to flag an odr
  violation.  Because of this, a mechanism to not unload dlls has been
  added.  This allows the next version of `nlmixr2est` to not have
  issues with Mac m1 san checks.

# rxode2 4.0.2

- At the request of CRAN, be a bit more careful so that names are not
  duplicated.  Now include the md5 hash, a global counter and random 4
  digit and number combination. In addition add the name of the
  original function so it will be easier to debug in the future.

- Fall back to data.frame `rbind` when `rbind.rxSolve()` fails

# rxode2 4.0.1

- Add the ability to use `rbind` for solved `rxode2` frames.

- Fix `LTO` issue for `_rxode2_calcDerived`

# rxode2 4.0.0

- Add more information errors about `NA`s during solving.

- Fix `rxDerived()` for mixed vector and non-vector input.

- Fix model variables for `alag(cmt)` when they are defined before
  `d/dt()` or `linCmt()`

- Just in time use of `state.ignore` in the model variables, fixes
  negative length error observed in #857.

- Fix steady state bug with time-varying covariates.  Now the
  covariates are inferred at the time of the steady state (instead of
  searching through the subject based on the projected time).

- Rework the linear solved systems to use the wnl solutions, and
  threaded linear systems solve (for non-gradient solutions). This new
  method closes a variety of linear compartment model bugs (#261,
  #272, #441, #504, #564, #717, #728, #827, and #855)

- Added new types of bounds for event tables:

  - 3 point bounds `et(list(c(low, mid, high)))` when specified this way,
    they will not change. Perfect for use with `babelmixr2`'s `PopED` (#862,
    #863, #854)

  - Intervals simulated by normal values instead of uniform.  In this
    case the first seen interval will be 3 elements with NA at the end
    `et(list(c(mean, sd, NA), c(mean, sd)))`, and the other elements
    can simply be 2 declaring the `c(mean, sd)`

  - Of course the uniform windows of `et(list(c(low, high)))` still work

  - Currently these different types of windows cannot be mixed.

- Add ability to pipe a list or named numeric as an eta with
  `%>% ini(~eta)`

- Added a fix for event tables where expanding IDs in non-sequential
  order.  In particular if the first ID is not the minimum ID when expanding
  the first event table, the smallest ID was not in the output table. Now
  the smallest ID is in the event table. (Fixes #878, #869, #870)

- Added ability to pipe `ini()` or `lotri()`, or any other expression
  that can be converted to an ini with `as.ini()`. Also allows `ini()`
  expressions to be converted to lotri with `as.lotri()`. Fixes #871

- Added new type of variability expression for simulation and
  estimation with focei and likelihood related methods: `+var()`. This
  changes standard deviation parameters to variance parameters.

- Added new type of endpoint expression for focei estimation
  `+dv()`. This only transforms the data and not the predictions. I
  can only see it being useful in model linearization.

- Bug fix for parameters that are in both input (`$params`) and output
  (`$lhs`) that respects the order of the `$lhs` declaration (Fixes
  #876)

- Add `rxFixRes` to literally fix the residual estimates in a model (#889)

- Now modeled duration of 0 is treated as a bolus dose (#892)

# rxode2 3.0.4

- Add stable hashes for rxUi objects (#838, #689)

- Fix for iov simulation (#842)

- Fix for `rxnbinom()` called directly from R (#847) and expand it to
  match more close with R's `rnbinom()` including allowing named `mu=`
  calls.  In rxode2 ui, these are also now allowed.

# rxode2 3.0.3

- Add `logit`/`expit` named expressions, that is `logit(x, high=20)`
  becomes `logit(x, 0, 20)` in ui models.

- Updated random ui models like `rxnorm(sd=10)` to accept complex
  numeric expressions like `rxnorm(sd=10+1)`.

- Updated random ui models to accept complex non-numeric expressions
  like `rxnorm(sd=a+b)`

- Rework the `tad()` and related functions so they use the same
  interface as compartments (this way they do not depend on the order
  of compartments); See #815.  For mu-referencing, Also allow dummy
  variables to ignore state requirements (ie `podo(depot)` in a single
  line will not error when parsing mu-referenced equations).

- Add `getRxNpars` to api.  This allows the development version of
  `babelmixr2` to better check what model is loaded and unload/reload
  as necessary.

- Add `rxUdfUiControl()` to rxode2 user function to get control
  information from something like `nlmixr2`

- Bug fix for tracking time after dose when dosing to 2 compartments
  occur at the exact same time (#804, #819)

- Change `transit()` model so that it uses `tad0()`, `podo0()` and
  related functions for a bit more stable simulation and estimation

- Fix compile flags to work with BH 1.87 (#826)

# rxode2 3.0.2

- Bug fix for `api`, the censoring function pointer has been updated
  (#801).

- Query `rxode2.verbose.pipe` at run time instead of requiring it to
  be set before loading `rxode2`.

- Have correct values at boundaries for `logit`, `expit`, `probit`,
  and `probitInv` (instead of `NA`). For most cases this does not
  break anything.

- Add a new style of user function that modifies the `ui` while
  parsing or just before using the function (in the presence of
  `data`).

- Used the new user function interface to allow all random functions
  in `rxode2` ui functions to be named.  For example, you can use
  `rxnorm(sd=3)` instead of having to use `rxnorm(0, 3)`, although
  `rxnorm()` still works.

# rxode2 3.0.1

- Explicitly initialize the order vector to stop valgrind warning
  (requested from CRAN)

# rxode2 3.0.0

## Breaking Changes

- The model properties was moved from `$params` to `$props` so it does
  not conflict with the low level `rxode2` model `$params`

- Error when specifying `wd` without `modName`

- With Linear and midpoint of a time between two points, how `rxode2`
  handles missing values has changed.  When the missing value is lower
  than the requested time, it will look backward until it finds the
  first non-missing value (or if all are missing start looking
  forward).  When the missing value is higher than the requested time,
  the algorithm will look forward until it finds the first non-missing
  value (or if all are missing, start looking backward).

- The order of ODEs is now only determined by the order of `cmt()` and
  `d/dt()`. Compartment properties, `tad()` and other compartment
  related variables no no longer affect compartment sorting.  The
  option `rxode2.syntax.require.ode.first` no longer does anything.

- The handling of zeros "safely" has changed (see #775)

  - when `safeZero=TRUE` and the denominator of a division expression
    is zero, use the Machine's small number/`eps` (you can see this
    value with `.Machine$double.eps`)

  - when `saveLog=TRUE` and the x in the `log(x)` is less than or
    equal to zero, change this to `log(eps)`

  - when `safePow=TRUE` and the expression `x^y` has a zero for `x`
    and a negative number for `y` replace `x` with `eps`.

  Since the protection for divide by zero has changed, the results
  will also change. This is a more conservative protection mechanism
  than was applied previously.

- Random numbers from `rxode2` are different when using `dop853`,
  `lsoda` or `indLin` methods.  These now seed the random numbers in
  the same way as `liblsoda`, so the random number provided will be
  the same with different solving methods.

- The arguments saved in the `rxSolve` for items like `thetaMat` will
  be the reduced matrices used in solving, not the full matrices (this
  will likely not break very many items)

## Possible breaking changes (though unlikely)

- `iCov` is no longer merged to the event dataset.  This makes solving
  with `iCov` slightly faster (#743)


## New features

- You can remove covariances for every omega by piping with `%>%
  ini(diag())` you can be a bit more granular by removing all
  covariances that have either `eta.ka` or `eta.cl` by: `%>%
  ini(diag(eta.ka, eta.cl))` or anything with correlations with
  `eta.cl` with `%>% ini(diag(eta.cl))`

- You can also remove individual covariances by `%>% ini(-cov(a, b))`
  or `%>% ini(-cor(a,b))`.

- You can specify the type of interpolation applied for added dosing
  records (or other added records) for columns that are kept with the
  `keep=` option in `rxSolve()`. This new option is
  `keepInterpolation` and can be `locf` for last observation carried
  forward, `nocb` which is the next observation carried backward, as
  well as `NA` which puts a `NA` in all imputed data rows. See #756.

   - Note: when interpolation is linear/midpoint for
     factors/characters it changes to locf with a warning (#759)

   - Also note, that the default keep interpolation is `na`

- Now you can specify the interpolation method per covariate in the model:

  - `linear(var1, var2)` says both `var1` and `var2` would use linear
    interpolation when they are a time-varying covariate. You could
    also use `linear(var1)`

  - `locf()` declares variables using last observation carried forward

  - `nocb()` declares variables using next observation carried backward

  - `midpoint()` declares variables using midpoint interpolation

- `linear()`, `locf()`, `locb()`, `midpoint()`, `params()`, `cmt()`
  and `dvid()` declarations are now ignored when loading a `rxode2`
  model with `rxS()`

- Strings can be assigned to variables in `rxode2`.

- Strings can now be enclosed with a single quote as well as a double
  quote.  This limitation was only in the rxode2 using string since
  the R-parser changes single quotes to double quotes. (This has no
  impact with `rxode2({})` and ui/function form).

- More robust string encoding for symengine (adapted from
  `utils::URLencode()` and `utils::URLdecode()`)

- Empty arguments to `rxRename()` give a warning (#688)

- Promoting from covariates to parameters with model piping (via `ini()`) now
  allows setting bounds (#692)

 - Added `assertCompartmentName()`, `assertCompartmentExists()`,
  `assertCompartmentNew()`, `testCompartmentExists()`,
  `assertVariableExists()` `testVariableExists()`,
  `assertVariableNew()`, `assertVariableName()`, and
  `assertParameterValue()` to verify that a value is a valid nlmixr2
  compartment name, nlmixr2 compartment/variable exists in the model,
  variable name, or parameter value (#726; #733)

- Added `assertRxUnbounded()`, `testRxUnbounded()`, `warnRxBounded()`
  to allow `nlmixr2` warn about methods that ignore boundaries #760

- Added functions `tad0()`, `tafd0()`, `tlast0()` and `tfirst0()` that
  will give `0` instead of `NA` when the dose has not been
  administered yet.  This is useful for use in ODEs since `NA`s will
  break the solving (so can be used a bit more robustly with models
  like Weibull absorption).

- `rxode2` is has no more binary link to `lotri`, which means that
  changes in the `lotri` package will not require `rxode2` to be
  recompiled (in most cases) and will not crash the system.

- `rxode2` also has no more binary linkage to `PreciseSums`

- The binary linkage for `dparser` is reduced to C structures only,
  making changes in dparser less likely to cause segmentation faults
  in `rxode2` if it wasn't recompiled.

- A new model property has been added to `$props$cmtProp` and
  `$statePropDf`.  Both are data-frames showing which compartment has
  properties (currently `ini`, `f`, `alag`, `rate` and `dur`)
  in the `rxode2` ui model.  This comes from the lower
  level model variable `$stateProp` which has this information
  encoded in integers for each state.

- A new generic method `rxUiDeparse` can be used to deparse meta
  information into more readable expressions; This currently by
  default supports lower triangular matrices by lotri, but can be
  extended to support other types of objects like 'nlmixr2's
  `foceiControl()` for instance.

## Bug fixes

- Fix `ui$props$endpoint` when the ui endpoint is defined in terms of
  the ode instead of lhs. See #754

- Fix `ui$props` when the ui is a linear compartment model without `ka` defined.

- Model extraction `modelExtract()` will now extract model properties.  Note that the model property of `alag(cmt)` and `lag(cmt)` will give the same value. See #745

- When assigning reserved variables, the parser will error. See #744

- Linear interpolation will now adjust the times as well as the values
  when `NA` values are observed.

- Fix when keeping data has `NA` values that it will not crash R; Also
  fixed some incorrect `NA` interpolations. See #756

- When using `cmt()` sometimes the next statement would be corrupted
  in the normalized syntax (like for instance `locf`); This bug was
  fixed (#763)

- `keep` will now error when trying to keep items that are in the
  rxode2 output data-frame and will be calculated (#764)

## Big change

- At the request of CRAN, combine `rxode2parse`, `rxode2random`, and
 `rxode2et` into this package; The changes in each of the packages are
 now placed here:

### rxode2et (no changes before merge)

#### rxode2et 2.0.13

* Fix import of data where there are NA times

#### rxode2et 2.0.12

* Fix formatting issues identified by m1mac, as requested by CRAN

#### rxode2et 2.0.11

* Make the stacking more flexible to help rxode2 have more types of plots

* Add `toTrialDuration` by Omar Elashkar to convert event data to trial duration data

* Fix Issue #23 and prefer variable values over NSE values

#### rxode2et 2.0.10

* Fix dollar sign accessing of objects (like data frames), as pointed
  out by @frbrz (issue #16)

* Use `rxode2parse` functions for internal event table creation (where
  they were moved to).

* Dropped C++14 and let the system decide.

#### rxode2et 2.0.9

* Split off `et()`, `eventTable()` and related functions.

* Also split off `rxStack()` and `rxCbindStudyIndividual()` in this
  package.

* Added a `NEWS.md` file to track changes to the package.

### rxode2random (before merge)

- Fix a bug when simulating nested variables (#25)

#### rxode2random 2.1.0

- **Breaking Change** changed distributions from the standard C++
  `<random>` to `boost::random`.  Since this is not dependent on the
  compiler, it makes the random numbers generated from Mac, Windows
  and Linux the same for every distribution.  Unfortunately with a new
  random number transformation, the simulation results will likely be
  different than they were before.  The exception to this is the
  uniform number, which was always the same between platforms.

#### rxode2random 2.0.13

- Fixed formatting issues (as requested by CRAN and identified on `m1mac`)

#### rxode2random 2.0.12

- Added function `dfWishart` which gives (by simulation) an
  approximation of the degrees of freedom of a Wishart to match a
  `rse` value.

- Added function `swapMatListWithCube` which swaps omegaList with
  omegaCube values

- Ensure that the outputs are integers (instead of long integers) as
  requested by CRAN for some checking functions.

#### rxode2random 2.0.11

- Fix qassert LTO

#### rxode2random 2.0.10

- Moved fast factor to `rxode2parse` to allow `etTrans` to be moved there

#### rxode2random 2.0.9

* Initial release of `rxode2random`, which separates the parallel
  safe, random number generation from 'rxode2' into a separate package to
  reduce 'rxode2' compilation time. This should make CRAN maintenance
  a bit easier.

* Added a `NEWS.md` file to track changes to the package.


### rxode2parse (fixed before merging)

* As requested by CRAN remove the C code `SET_TYPEOF` which is no
  longer part of the C R API.

#### rxode2parse 2.0.19

* Added a evid suffix of 60 for cases where evid=2 adds an on event
  (fixes tad() calculation in certain edge cases)

* Initialize all variables to `NA`

#### rxode2parse 2.0.18

* Removed linear compartment solutions with gradients from rxode2parse
  (and rxode2) when compiled with intel c++ compiler (since it crashes
  while compiling).

* Fixed `m1mac` string issues as requested by CRAN

#### rxode2parse 2.0.17

* Added ability to query R user functions in a rxode2 model (will
  force single threaded solve)

* Moved core `rxFunParse` and `rxRmFunParse` here so that C and R user
  function clashes can be handled

* Model variables now tracks which compartments have a lag-time
  defined

* For compartment with steady state doses (NONMEM equivalent SS=1,
  SS=2), an additional tracking time-point is added at to track the
  time when the lagged dose is given.  As an upshot, the lagged dose
  will start at the steady state concentration shifted by + ii - lag
  in `rxode2` (currently for ode systems only)

* This release calculates non bio-availability adjusted duration for
  all rates instead of trying to figure the rate duration during
  solving.

* Make double assignment an error, ie  `a <- b <-`

* `NA` times are ignored (with warning)

* Steady state bolus doses with `addl` are treated as non steady state
  events (like what is observed in `NONMEM`)

* Timsort was upgraded; drop radix support in rxode2 structure

* `etTrans` now supports keeping logical vectors (with the appropriate
  version of `rxode2`).

* Security fixes were applied as requested by CRAN

#### rxode2parse 2.0.16

* Import `data.table` explicitly in the R code (before was imported only in C/C++ code)

#### rxode2parse 2.0.15

* Updates the make flags to support CXX17.

#### rxode2parse 2.0.14

* 'linCmt()' translations of 'alpha', 'beta', 'gamma', 'k21', 'k31',
  'vc' now error instead of ignoring 'gamma' and 'k31' to give 2 cmt
  solution

* transit compartment internal code now changes dose to 0.0 when no
  dose has been administered to the depot compartment. This way dosing
  to the central compartment (without dosing to the transit
  compartment) will not give a `NA` for the depot compartment (and
  consequently for the central compartment)

* Moved `rxDerived` here and added tests for it here as well.

* Moved `etTransParse` here and added tests for it here as well (makes
  up most of `etTrans`). In addition the following changes were made
  to `etTransParse()`/`etTrans()`:

  * The internal translation (`etTrans()`) will not drop times when
    infusions stop. Before, if the infusion stopped after the last
    observation the time when the infusion stopped would be dropped.
    This interferes with `linCmt()` models.

  * Breaking change/bug fix `evid=2` are considered observations when
    translating data to internal `rxode2` event structure

  * Fix edge case to find infusion duration when it is the first item
    of the dosing record at time 0.

 * Fixed a bug for certain infusions where the `rate`, `ii` and/or
   `ss` data items were dropped from the output when `addDosing=TRUE`


* Also have internal functions to convert between classic NONMEM
  events and rxode2 events

* Have an internal function that gives information on the linear
  compartmental model translation type, which could be useful for
  babelmixr2

* 'time' in model is now case insensitive

* Use function declaration in `rxode2parseGetTranslation()` to
  determine thread safety of functions available to rxode2

* Add check for correct number of function arguments to parser.

* Like R, known functions can be assigned as a variable and the
  function can still be called (while not changing the variable
  value).  For example you can have a variable `gamma` as well as a
  function `gamma()`.

* Fix garbled error messages that occur with certain messages.

* Fixed errors that occurred when using capitalized AMT variables in
  the model.

#### rxode2parse 2.0.13

* Version bump for dparser (so binaries will be built correctly)

#### rxode2parse 2.0.12

* Bug fix for strict prototypes

* Removed `sprintf` as noted by CRAN

* Made `rxode2parse` dll binary independent of `rxode2()`

#### rxode2parse 2.0.11

* Bug fix for strict aliasing as requested by CRAN

#### rxode2parse 2.0.10

* Use strict aliasing as requested by CRAN

#### rxode2parse 2.0.9

* Initial release to split of rxode2parse from rxode2 to reduce
  compilation time of 'rxode2'


# rxode2 2.1.3

## Bug fixes

- Make sure that the object is a uncompressed rxode2 ui for solving with `rxSolve` (See #661)

- Fix #670 by using the last simulated observation residual when there
  are trailing doses.

## New features

- Create a function to see if a rxode2 solve is loaded in memory
  (`rxode2::rxSolveSetup()`)

- Create a new function that fixes the rxode2 population values in the
  model (and drops them in the initial estimates); `rxFixPop()`

## Other changes

- Pendantic no-remap (as requested by CRAN)

- gcc USBAN fix (as requested by CRAN)

# rxode2 2.1.2

## Other changes

- `rxUi` compression now defaults to fast compression

- Fixes String literal formatting issues as identified by CRAN (#643)

- Removes linear compartment solutions with gradients for intel c++
  compiler (since they crash the compiler).

# rxode2 2.1.0

## Breaking changes

- Steady state with lag times are no longer shifted by the lag time
  and then solved to steady state by default.  In addition the steady
  state at the original time of dosing is also back-calculated. If you
  want the old behavior you can bring back the option with
  `ssAtDoseTime=FALSE`.

- "dop853" now uses the `hmax`/`h0` values from the `rxControl()` or
  `rxSolve()`.  This may change some ODE solving using "dop853"

- When not specified (and xgxr is available), the x axis is no longer
  assumed to be in hours

## New features

- User defined functions can now be R functions.  For many of these R
  functions they can be converted to C with `rxFun()` (you can see the
  C code afterwards with `rxC("funName")`)

- Parallel solving of models that require sorting (like modeled lag
  times, modeled duration etc) now solve in parallel instead of downgrading
  to single threaded solving

- Steady state infusions with a duration of infusions greater than the
  inter-dose interval are now supported.

- Added `$symengineModelNoPrune` and `$symengineModelPrune` for
  loading models into rxode2 with `rxS()`

- When plotting and creating confidence intervals for multiple
  endpoint models simulated from a rxode2 ui model, you can
  plot/summarize each endpoint with `sim`. (ie. `confint(model,
  "sim")` or `plot(model, sim)`).

  If you only want to summarize a subset of endpoints, you can focus
  on the endpoint by pre-pending the endpoint with `sim.`  For example
  if you wanted to plot/summarize only the endpoint `eff` you would
  use `sim.eff`. (ie `confint(model, "sim.eff")` or `plot(model,
  sim.eff)`)

- Added `model$simulationIniModel` which prepend the initial
  conditions in the `ini({})` block to the classic `rxode2({})` model.

- Now `model$simulationModel` and `model$simulationIniModel` will save
  and use the initialization values from the compiled model, and will
  solve as if it was the original ui model.

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

- Allow steady state infusions with a duration of infusion greater than
  the inter-dose interval to be solved.

- Solves will now possibly print more information when issuing a
  "could not solve the system" error

- The function `rxSetPipingAuto()` is now exported to change the way you
  affect piping in your individual setup

- Allow covariates to be specified in the model piping, that is `mod
  %>% model(a=var+3, cov="var")` will add `"var"` as a covariate.

- When calculating confidence intervals for `rxode2` simulated objects
  you can now use `by` to stratify the simulation summary.  For
  example you can now stratify by gender and race by: `confint(sim,
  "sim", by=c("race", "gender"))`

- When calculating the intervals for `rxode2` simulated objects you
  can now use `ci=FALSE` so that it only calculates the default
  intervals without bands on each of the percentiles; You can also
  choose not to match the secondary bands limits with `levels` but use
  your own `ci=0.99` for instance

- A new function was introduced `meanProbs()` which calculates the
  mean and expected confidence bands under either the normal or t
  distribution

- A related new function was introduced that calculates the mean and
  confidence bands under the Bernoulli/Binomial distribution
  (`binomProbs()`)

- When calculating the intervals for `rxode2` simulated objects you
  can also use `mean=TRUE` to use the mean for the first level of
  confidence using `meanProbs()`. For this confidence interval you can
  override the `n` used in the confidence interval by using `n=#`. You
  can also change this to a prediction interval instead using
  `pred=TRUE`.

- Also when calculating the intervals for `rxode2` simulated object
  you can also use `mean="binom"` to use the binomial distributional
  information (and ci) for the first level of confidence using
  `binomProbs()`.  For this confidence interval you can override the
  `n` used in the confidence interval by using `n=#`. You can also
  change this to a prediction interval instead using `pred=TRUE`. With
  `pred=TRUE` you can override the number of predicted samples with
  `m=#`

- When plotting the `confint` derived intervals from an `rxode2`
  simulation, you can now subset based on a simulated value like
  `plot(ci, Cc)` which will only plot the variable `Cc` that you
  summarized even if you also summarized `eff` (for instance).

- When the rxode2 ui is a compressed ui object, you can modify the ini
  block with `$ini <-` or modify the model block with `$model <-`.
  These are equivalent to `ini(model) <-` and `model(model) <-`,
  respectively. Otherwise, the object is added to the user defined
  components in the function (ie `$meta`).  When the object is
  uncompressed, it simply assigns it to the environment instead (just
  like before).

- When printing meta information that happens to be a `lotri`
  compatible matrix, use `lotri` to express it instead of the default
  R expression.

- Allow character vectors to be converted to expressions for piping
  (#552)

- `rxAppendModel()` will now take an arbitrary number of models and
  append them together; It also has better handling of models with
  duplicate parameters and models without `ini()` blocks (#617 / #573
  / #575).

- `keep` will now also keep attributes of the input data (with special
  handling for `levels`); This means a broader variety of classes will
  be kept carrying more information with it (for example ordered
  factors, data frame columns with unit information, etc)

- Piping arguments `append` for `ini()` and `model()` have been
  aligned to perform similarly.  Therefore `ini(append=)` now can take
  expressions instead of simply strings and `model(append=)` can also
  take strings.  Also model piping now can specify the integer line
  number to be modified just like the `ini()` could.  Also
  `model(append=FALSE)` has been changed to `model(append=NULL)`.
  While the behavior is the same when you don't specify the argument,
  the behavior has changed to align with `ini()` when piping.  Hence
  `model(append=TRUE)` will append and `model(append=FALSE)` will now
  pre-pend to the model.  `model(append=NULL)` will modify lines like
  the behavior of `ini(append=NULL)`.  The default of `model(line)`
  modifying a line in-place still applies.  While this is a breaking
  change, most code will perform the same.

- Labels can now be dropped by `ini(param=label(NULL))`. Also
  parameters can be dropped with the idiom `model(param=NULL)` or
  `ini(param=NULL)` changes the parameter to a covariate to align with
  this idiom of dropping parameters

- `rxRename` has been refactored to run faster

## Internal new features

- Add `as.model()` for list expressions, which implies `model(ui) <-
  ui$lstExpr` will assign model components.  It will also more
  robustly work with character vectors

- Simulated objects from `rxSolve` now can access the model variables
  with `$rxModelVars`

- Simulation models from the UI now use `rxerr.endpoint` instead of
  `err.endpoint` for the `sigma` residual error.  This is to align
  with the convention that internally generated variables start with
  `rx` or `nlmixr`

- Sorting only uses timsort now, and was upgraded to the latest
  version from Morwenn

## Bug fixes

- Simulating/solving from functions/ui now prefers params over `omega`
  and `sigma` in the model (#632)

- Piping does not add constants to the initial estimates

- When constants are specified in the `model({})` block (like `k <- 1`), they will not
  be  to the `ini` block

- Bug fix for `geom_amt()` when the `aes` transformation has `x`

- Bug fix for some covariate updates that may affect multiple compartment
  models (like issue #581)

## Maintenance fixes

- Modify plot code to work with development `xgxr`

# rxode2 2.0.14

- CRAN requested that FORTRAN `kind` be changed as it was not portable;
  This was commented code, and simply removed the comment.

- Bug-fix for `geom_amt()`; also now uses `linewidth` and at least `ggplot2 3.4.0`

- Some documentation was cleaned up from `rxode2` 2.0.13

# rxode2 2.0.13

## Bug fixes

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
  seeds](https://www.johndcook.com/blog/2016/01/29/random-number-generator-seed-mistakes/)
  and is the best practice.

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
