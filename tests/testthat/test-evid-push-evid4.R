rxTest({
  # Pushed evid=4 (reset + dose) records: an infusion or modeled rate/duration
  # dose still turns off after the reset, and a bolus is unchanged.  Each pushed
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
})
