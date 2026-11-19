rxTest({
  # Infusions pushed from inside the model with evid_().  _rxTranslateOneEvent()
  # has to emit the record that turns the infusion back off: a fixed rate/
  # duration dose needs a -rate record at time + dur, and a modeled rate/duration
  # dose needs its companion "off" record at the same time for updateRate()/
  # updateDur() to fill in (those are in test-evid-push-modeled.R).  Each pushed
  # regimen is compared against the same regimen written into the event table.

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
  test_that("a pushed negative compartment turns that compartment off", {
    # a negative compartment is the turn-off signal the event table takes from
    # a negative CMT column; evid_() spells it the same way, by name or number
    pars <- c(ka = 0.5, cl = 1, v = 10)
    mkMod <- function(cmtExpr) {
      eval(bquote(rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t < 1e-8) {
          evid_(6, 2, 0, .(cmtExpr), 0, 0, 0, 0)
        }
      })))
    }
    ref <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      cp <- central / v
    })
    want <- rxSolve(
      ref,
      pars,
      et(amt = 100, time = 0) |>
        et(time = 6, cmt = "-depot", evid = 2) |>
        et(.obs)
    )
    for (cmtExpr in list(quote(-depot), -1)) {
      got <- rxSolve(mkMod(cmtExpr), pars, et(amt = 100, time = 0) |> et(.obs))
      expect_equal(got$depot, want$depot, tolerance = 1e-5)
      expect_equal(got$cp, want$cp, tolerance = 1e-5)
    }
    # and it really turned the compartment off
    got <- rxSolve(mkMod(quote(-depot)), pars, et(amt = 100, time = 0) |> et(.obs))
    expect_gt(got$depot[got$time == 5.5], 0)
    expect_equal(got$depot[got$time == 6.5], 0)
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
