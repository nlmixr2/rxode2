rxTest({
  # Steady-state doses pushed with evid_() into a compartment with a modeled
  # alag() must expand like the event table does (rxode2#1349).
  .obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
  .pars <- c(cl = 1, v = 10)

  ## A steady-state dose into a compartment with a modeled alag() (rxode2#1349)
  ## has to be expanded into an UNLAGGED steady-state record plus the lagged
  ## records that actually dose the subject.  Only the event table did that, so
  ## the same regimen pushed from inside the model solved to something else.
  .obsLag <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
  .refLag <- rxode2({
    d/dt(central) <- -cl / v * central
    alag(central) <- 3
    cp <- central / v
  })
  .expectSameAsEventTableLag <- function(mod, ev, ref = .refLag, tolerance = 1e-5) {
    got <- rxSolve(mod, .pars, et(.obsLag))
    want <- rxSolve(ref, .pars, ev |> et(.obsLag))
    expect_equal(got$time, want$time)
    ## the t=0 row differs by construction: a dose pushed at the current time
    ## is applied after that record's own observation
    expect_equal(got$cp[-1], want$cp[-1], tolerance = tolerance)
  }

  test_that("a pushed steady-state dose into an alag() compartment matches the event table (#1349)", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1)
    )
  })

  test_that("a pushed ss=2 dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 2)
      }
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = 10, ii = 12, ss = 2)
    )
  })

  test_that("a pushed steady-state bolus into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(mod, et(amt = 100, time = 2, ii = 12, ss = 1))
  })

  test_that("a pushed steady-state modeled-rate dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 0, 1)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      rate(central) <- 10
      cp <- central / v
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = -1, ii = 12, ss = 1),
      ref = ref
    )
  })

  test_that("a pushed steady-state modeled-duration dose into an alag() compartment matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 12, 0, 1)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      dur(central) <- 10
      cp <- central / v
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = -2, ii = 12, ss = 1),
      ref = ref
    )
  })

  test_that("only the first occurrence of a pushed ss+addl series into an alag() compartment expands", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 2, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2, ss = 1)
    )
  })

  test_that("a pushed evid=4 steady-state dose into an alag() compartment matches the event table", {
    # the widest expansion: a reset plus the four steady-state-with-lag records
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 12, 0, 1)
      }
    })
    .expectSameAsEventTableLag(
      mod,
      et(amt = 100, time = 2, rate = 10, evid = 4, ii = 12, ss = 1)
    )
  })

  test_that("ssAtDoseTime=FALSE turns the pushed lagged expansion off too", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 1)
      }
    })
    got <- rxSolve(mod, .pars, et(.obsLag), ssAtDoseTime = FALSE)
    want <- rxSolve(
      .refLag,
      .pars,
      et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1) |>
        et(.obsLag),
      ssAtDoseTime = FALSE
    )
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed phantom dose keeps a modeled rate, as the event table does", {
    mod <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      rate(depot) <- 25
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 7, 100, 1, -1, 0, 0, 0)
      }
    })
    ref <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      rate(depot) <- 25
      cp <- central / v
    })
    pars <- c(ka = 0.5, cl = 1, v = 10)
    got <- rxSolve(mod, pars, et(.obs))
    want <- rxSolve(ref, pars, et(amt = 100, time = 2, rate = -1, evid = 7) |> et(.obs))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed lagged steady-state bolus splits into every target", {
    # the lagged steady-state expansion yields TWO splittable bolus records
    # (the flg 9 steady-state record and its plain flg 1 companion), so
    # _rxPushDose() has to split each of them, not just the first
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 1)
      }
    })
    # the same model with the dose in the event table instead: splitBolus is a
    # post-pass over already-translated records on both sides, so this is the
    # comparison that has to hold
    mBase <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      alag(central) <- 3
      cp <- central / v
    })
    p <- c(ka = 0.5, cl = 1, v = 10)
    got <- rxSolve(mSplit, p, et(obs))
    want <- rxSolve(mBase, p, et(amt = 100, time = 2, ii = 12, ss = 1, cmt = 1) |> et(obs))
    expect_true(all(is.finite(got$cp)))
    expect_equal(got$depot[-1], want$depot[-1], tolerance = 1e-5)
    expect_equal(got$central[-1], want$central[-1], tolerance = 1e-5)
  })
})
