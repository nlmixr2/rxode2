rxTest({

  # Infusions pushed from inside the model with evid_().  _rxTranslateOneEvent()
  # has to emit the record that turns the infusion back off: a fixed rate/
  # duration dose needs a -rate record at time + dur, and a modeled rate/duration
  # dose needs its companion "off" record at the same time for updateRate()/
  # updateDur() to fill in.  Each pushed regimen is compared against the same
  # regimen written into the event table.

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

  test_that("a pushed evid=4 infusion turns off (#1322 follow-up)", {
    # evid=4 is reset + dose, and the translated event only had room for those
    # two records, so the infusion stop was dropped and the infusion ran for the
    # rest of the solve.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, evid = 4))
  })

  test_that("a pushed evid=4 bolus is unchanged", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 0, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, evid = 4))
  })

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

  test_that("a pushed evid=4 modeled rate dose resets and solves", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      rate(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, -1, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, evid = 4))
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
    want <- rxSolve(.ref, .pars,
                    et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
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

  test_that("a pushed fixed-duration (infuseDur) dose matches the event table", {
    # infuseDur() sets isDur, so rate carries the DURATION and rateI is 2
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuseDur(100, 10, 1, 0, 0, 0)
      }
    })
    got <- rxSolve(mod, .pars, et(.obs))
    want <- rxSolve(.ref, .pars, et(amt = 100, time = 0, dur = 10) |> et(.obs))
    expect_equal(got$cp, want$cp, tolerance = 1e-5)
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
    wantHi <- rxSolve(refHi, et(amt = 100, time = 2, rate = 10, cmt = "a110") |>
                        et(seq(0, 24, by = 1)))
    expect_equal(gotHi$a110, wantHi$a110, tolerance = 1e-5)
  })

  test_that("a pushed evid=4 modeled duration dose resets and solves", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      dur(central) <- 10
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, -2, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, dur = 10, evid = 4))
  })

  test_that("a pushed steady state (ss=2) infusion matches the event table", {
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 0, 2)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, rate = 10, ii = 12, ss = 2))
  })

  test_that("pushed fixed-rate doses repeat with addl", {
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars,
                    et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
    expect_equal(rxSolve(mod, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a split bolus pushed as evid=4 reserves enough room (#1322 follow-up)", {
    # splitBolus expands one translated event into splitBolusN-1 records, so an
    # evid=4 push writes 1 + (splitBolusN-1) records where the capacity check
    # only reserved ev.n = 2.  Push repeatedly so the EVID_EXTRA_SIZE slack that
    # hid the overrun is used up.
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 4, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    eBase <- e |> et(amt = 100, time = 0, cmt = 2)
    for (.t in seq(6, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1, evid = 4) |>
        et(amt = 50, time = .t, cmt = 2)
    }
    p <- c(ka = 0.5, cl = 1, v = 10)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("a split bolus pushed as evid=1 reserves enough room (#1322 follow-up)", {
    # the same under-reservation as the evid=4 case above, on the commoner
    # spelling: one translated event, splitBolusN-1 records
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1) {
        evid_(t + 6, 1, 50, 1, 0, 6, 9, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 72, by = 1))
    eBase <- e |> et(amt = 100, time = 0, cmt = 2)
    for (.t in seq(6, 60, by = 6)) {
      eBase <- eBase |>
        et(amt = 50, time = .t, cmt = 1) |>
        et(amt = 50, time = .t, cmt = 2)
    }
    p <- c(ka = 0.5, cl = 1, v = 10)
    rSplit <- rxSolve(mSplit, p, e)
    rBase <- rxSolve(mBase, p, eBase)
    expect_true(all(is.finite(rSplit$cp)))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("a pushed infusion into a splitBolus compartment is not split", {
    # _rxShouldSplitTranslatedBolus() requires whI == 0, so only a bolus splits;
    # an infusion into the same compartment stays whole
    mSplit <- rxode2({
      splitBolus(depot, depot, central)
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        infuse(100, 10, 1, 0, 0, 0)
      }
    })
    mBase <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    p <- c(ka = 0.5, cl = 1, v = 10)
    obs <- c(0, 1e-8, seq(0.5, 30, by = 0.5))
    rSplit <- rxSolve(mSplit, p, et(obs))
    rBase <- rxSolve(mBase, p, et(amt = 100, time = 0, rate = 10, cmt = 1) |> et(obs))
    expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
    expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
  })

  test_that("pushed fixed-rate and bolus doses are unchanged", {
    modRate <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(modRate, et(amt = 100, time = 2, rate = 10))

    modBolus <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 0, 0, 0)
      }
    })
    .expectSameAsEventTable(modBolus, et(amt = 100, time = 2))
  })

})
