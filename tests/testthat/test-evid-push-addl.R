rxTest({
  # Pushed doses repeated with addl: each occurrence carries the same ii as the
  # data record, and a reset/'other' record is emitted once (rxode2#1351).

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

  test_that("pushed fixed-rate doses repeat with addl", {
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 10, 12, 2, 0)
      }
    })
    want <- rxSolve(.ref, .pars, et(amt = 100, time = 2, rate = 10, ii = 12, addl = 2) |> et(obs))
    expect_equal(rxSolve(mod, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("a pushed evid=4 dose repeated with addl resets only once (rxode2#1351/#1352)", {
    # _rxPushDose()'s addl loop used to pass the original evid (4, reset+dose)
    # unmodified to every repeat, so every addl repetition re-reset the
    # compartment -- unlike the data-table addl expansion in etTran.cpp, which
    # resets only on the first evid=4 occurrence (matches NONMEM, see
    # rxode2#1351). Both spellings must now agree.
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 0, 12, 2, 0)
      }
    })
    .expectSameAsEventTable(mod, et(amt = 100, time = 2, evid = 4, ii = 12, addl = 2))
  })

  test_that("a pushed evid=4 infusion repeated with addl resets only once", {
    # the infusion form of the test above: the first occurrence translates to
    # three records (reset, on, off) and every repeat to two (on, off)
    obs <- c(0, 1e-8, seq(0.5, 48, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 4, 100, 1, 10, 12, 2, 0)
      }
    })
    want <- rxSolve(
      .ref,
      .pars,
      et(amt = 100, time = 2, rate = 10, evid = 4, ii = 12, addl = 2) |>
        et(obs)
    )
    expect_equal(rxSolve(mod, .pars, et(obs))$cp, want$cp, tolerance = 1e-5)
  })

  test_that("addl does not repeat a pushed reset or 'other' record", {
    # _rxPushDose()'s addl loop used to repeat ANY evid, so a pushed evid=3
    # with addl reset the system once per repetition.  The event table warns
    # and ignores addl for evid 0/2/3 (etTran.cpp), and the shared
    # _rxAddlApplies() now gives the push path the same rule.
    obs <- seq(0, 48, by = 1)
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(0, 1, 100, 1, 0, 0, 0, 0)
        evid_(15, 1, 100, 1, 0, 0, 0, 0)
        evid_(30, 1, 100, 1, 0, 0, 0, 0)
        evid_(10, 3, 0, 1, 0, 12, 2, 0)
      }
    })
    got <- rxSolve(mod, .pars, et(obs))
    oneReset <- rxSolve(
      .ref,
      .pars,
      et(amt = 100, time = 0) |>
        et(amt = 100, time = 15) |>
        et(amt = 100, time = 30) |>
        et(time = 10, evid = 3) |>
        et(obs)
    )
    threeResets <- rxSolve(
      .ref,
      .pars,
      et(amt = 100, time = 0) |>
        et(amt = 100, time = 15) |>
        et(amt = 100, time = 30) |>
        et(time = 10, evid = 3) |>
        et(time = 22, evid = 3) |>
        et(time = 34, evid = 3) |>
        et(obs)
    )
    # the t=0 row differs by construction: a dose pushed at the current time
    # lands after that record's own observation
    .i <- got$time > 0
    expect_equal(got$cp[.i], oneReset$cp[.i], tolerance = 1e-5)
    expect_false(isTRUE(all.equal(got$cp[.i], threeResets$cp[.i], tolerance = 1e-5)))
  })

  test_that("a pushed dose carries the same ii as the identical data record", {
    # rep 0 of an addl series keeps ii only when it means something (steady
    # state); this is _rxAddlOccurrence()'s rule, shared with the event table.
    # et() zeroes a meaningless ii before etTrans() ever sees it, so the
    # comparable reference is a raw data.frame.
    obs <- c(0, 1e-8, seq(0.5, 24, by = 0.5))
    mod <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
      if (t < 1e-8) {
        evid_(2, 1, 100, 1, 0, 12, 0, 0)
      }
    })
    raw <- rbind(
      data.frame(id = 1, time = obs, amt = NA_real_, evid = 0, ii = 0, addl = 0, ss = 0),
      data.frame(id = 1, time = 2, amt = 100, evid = 1, ii = 12, addl = 0, ss = 0)
    )
    raw <- raw[order(raw$time), ]
    got <- rxSolve(mod, .pars, et(obs), addDosing = TRUE)
    want <- rxSolve(.ref, .pars, raw, addDosing = TRUE)
    expect_equal(sort(names(got)), sort(names(want)))
    expect_equal(got$cp[-1], want$cp[-1], tolerance = 1e-5)
  })
})
