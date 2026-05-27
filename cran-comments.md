# rxode2 5.1.1

* Tested on the Debian to fix `linux-arm64` on R 4.5, ran successfully

* Bumped rxode2 for 5.1.1, it seemed to not be picked up by CRAN (no
  checking stalled in waiting queue and one small fix for event tables
  was added).

* For Makefile generation use compilerPath <- tools::Rcmd("config CC",
  stdout=TRUE) as indicated by Kurt.

* Added fix for the following issue seen in the reverse dependency check:

Package check result: WARNING

Check: whether package can be installed, Result: WARNING
  Found the following significant warnings:
    par_solve.cpp:3060:20: warning: ISO C++ forbids variable length array ‘_evid_tmpydot’ [-Wvla]
  See ‘/home/hornik/tmp/CRAN_recheck/rxode2.Rcheck/00install.out’ for details.
  Used C compiler: ‘gcc-16 (Debian 16.1.0-1) 16.1.0’
  Used Fortran compiler: ‘GNU Fortran (Debian 16.1.0-1) 16.1.0’
  Used C++ compiler: ‘g++-16 (Debian 16.1.0-1) 16.1.0’

The other reverse dependency checks are unrelated to rxode2.  I have opened two github issues:

- https://github.com/LeidenAdvancedPKPD/amp.sim/issues/4

- https://github.com/RichardHooijmaijers/shinyMixR/issues/50

To suggest the maintainer properly declare the suggested dependencies

Other changes:

- Various low level fixes to allow `nlmixr2est` to have parallelized
  focei.

- Parallelized the `rxode2` data.frame creation.

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
