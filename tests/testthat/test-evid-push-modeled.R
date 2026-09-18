rxTest({
  # Modeled rate/duration doses (rate=-1/-2) pushed with evid_(): the companion
  # "off" record lets updateRate()/updateDur() fill in the end, with addl, at
  # steady state, above compartment 100 and with a modeled lag.  Fixed-rate
  # infusions are in test-evid-push-infusion.R.

  .obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
  .ref <- rxode2({
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })
  .pars <- c(cl = 1, v = 10)

  .expectSameAsEventTable <- function(mod, ev, pars = .pars, tolerance = 1e-5) {
    got <- rxSolve(mod, pars, et(.obs))
    want <- rxSolve(.ref, .pars, ev |> et(.obs))
    expect_equal(got$time, want$time)
    expect_equal(got$cp, want$cp, tolerance = tolerance)
  }

  test_that("a pushed modeled rate (rate=-1) dose solves (#1322 follow-up)", {
    # the modeled on record was pushed without its off record, so
    # handleTurnOnModeledRate() reported data error 997 and the solve failed
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10))
  })

  test_that("a pushed modeled duration (rate=-2) dose solves (#1322 follow-up)", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, dur = 10))
  })

  test_that("pushed modeled rate/duration doses repeat with addl", {
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    modRate <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 2, 0)
      }
    })
    modDur <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -2, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars, et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
    expect_equal(rxSolve(modRate, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
    expect_equal(rxSolve(modDur, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a pushed modeled rate dose reaches steady state", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 12, 0, 1)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 1))
  })

  test_that("a pushed modeled rate constant infusion (ss=1, ii=0, amt=0) solves", {
    # flg 40 never turns off -- getTime__() skips the infusion-time calculation
    # for it and etTran.cpp emits no off record either, so the pushed path must
    # not add one.  The extra record would be numerically inert, so this checks
    # the trajectory; that the record itself is gone is checked by comparing
    # what the two translators emit, which the fix keeps in step.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 0, 1, -1, 0, 0, 1)
      }
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 0, time = 0, rate = 10, ss = 1, ii = 0) |> et(.obs))
    # the push happens during the first evaluation, so the steady state is in
    # place from the next output row onward rather than at time 0 itself
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })

  test_that("a pushed modeled infusion works above compartment 100 and with a modeled lag", {
    # cmt100 has to survive into the off record, and the off record's time is
    # written by updateRate() as laggedStart + dur
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      alag(central) <- 3
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, -1, 0, 0, 0)
      }
    })
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      alag(central) <- 3
      cp <- central / v
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(ref, .pars, et(amt = 100, time = 2, rate = 10) |> et(.obs))
    expect_equal(got$cp, want$cp, tolerance = 1e-5)

    # a compartment above 100: the pushed dose and the event-table dose must
    # reach the same state
    .states <- paste0("a", seq_len(120))
    .mod <- paste0("d/dt(", .states, ") <- -0.1*", .states, collapse = "\n")
    modHi <- rxode2(paste0(.mod, "\nrate(a110) <- 10\n",
                           "if (t < 1e-8) { evid_(2, 1, 100, 110, -1, 0, 0, 0) }"))
    refHi <- rxode2(.mod)
    gotHi <- rxSolve(modHi, et(seq(0, 24, by = 1)))
    wantHi <- rxSolve(
      refHi,
      et(amt = 100, time = 2, rate = 10, cmt = "a110") |>
        et(seq(0, 24, by = 1))
    )
    expect_equal(gotHi$a110, wantHi$a110, tolerance = 1e-5)
  })
})
