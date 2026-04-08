rxTest({
  test_that("exceeding maxExtra raises an error (single subject)", {
    # This model unconditionally pushes a new event at t+0.1 on every ODE
    # evaluation step.  Each integration interval (0.1 wide) produces one push,
    # so seq(0, 2, by=0.1) creates 20 intervals → 20 pushes, which exceeds
    # maxExtra=10 and must raise an error.
    mCascade <- rxode2({
      d/dt(x) <- -x
      evid_(t + 0.1, 1, 1, 10, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mCascade, c(x = 1), e, maxExtra = 10L),
      regexp = "maxExtra"
    )
  })

  test_that("exceeding maxExtra does not crash R with parallel subjects", {
    # Same cascading model, multiple subjects solved in parallel.
    # Must raise an error rather than segfaulting.
    mCascade <- rxode2({
      d/dt(x) <- -x
      evid_(t + 0.1, 1, 1, 10, 0, 0, 0)
    })
    nSub <- 4L
    params <- data.frame(id = seq_len(nSub), x = rep(1, nSub))
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mCascade, params, e, maxExtra = 10L, cores = 2L),
      regexp = "maxExtra"
    )
  })

  test_that("maxExtra=0 allows unlimited pushes (no error)", {
    # Limited cascade: condition prevents indefinite growth.
    # maxExtra=0 disables the guard entirely; should succeed without error.
    m <- rxode2({
      d/dt(x) <- -x
      evid_(t + 0.1, 1, 1, 10, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_no_error(rxSolve(m, c(x = 1), e, maxExtra = 0L))
  })

  test_that("bolus push via evid_() adds extra timepoints and affects trajectory", {
    m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl/vd * central
      cp <- central / vd
      if (t < 24) {
        evid_(t + 12, 1, 1, 50, 0, 0, 0)  # bolus 50 units to cmt 1 at t+12
      }
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 24, by = 1))
    p <- c(ka = 0.5, cl = 1, vd = 10)
    r <- rxSolve(m, p, e)
    # The pushed dose at t+12 should create an extra event, affecting cp after t=12
    expect_true(nrow(r) > 0)
    # The bolus at t=12 should be visible — cp should rise after t=12
    cp12 <- r$cp[r$time == 12]
    cp14 <- r$cp[r$time == 14]
    expect_true(length(cp12) > 0 && length(cp14) > 0)
    expect_true(cp14 > cp12)
  })

  test_that("infusion push via evid_() adds start+stop events", {
    m2 <- rxode2({
      d/dt(central) <- -cl / vd * central
      cp <- central / vd
      if (t < 30) {
        evid_(t + 6, 1, 1, 100, 0, 0, 10)  # rate=10, so dur=10h
      }
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 30, by = 1))
    p <- c(cl = 1, vd = 10)
    r <- rxSolve(m2, p, e)
    expect_true(nrow(r) > 0)
    # Central should rise during the pushed infusion window (t=6 to t=16)
    cp7  <- r$cp[r$time == 7]
    cp20 <- r$cp[r$time == 20]
    expect_true(length(cp7) > 0 && length(cp20) > 0)
    # At t=7, infusion is active; at t=20, it has stopped and cp is declining
    expect_true(cp7 > 0)
  })

  test_that("past-time evid_() produces a warning", {
    m3 <- rxode2({
      d/dt(x) <- -x
      if (t < 5) {
        evid_(t - 1, 1, 1, 10, 0, 0, 0)  # past time — should warn
      }
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 5, by = 1))
    expect_warning(
      rxSolve(m3, c(x = 0), e),
      regexp = "evid_\\(\\).*time"
    )
  })

  test_that("evid=0 observation push adds an output row without dosing", {
    m4 <- rxode2({
      d/dt(central) <- -cl / vd * central
      cp <- central / vd
      if (t < 12) {
        evid_(t + 5.5, 0, 1, 0, 0, 0, 0)  # push observation at t+5.5
      }
    })
    e <- et(amt = 100, time = 0) |> et(c(0, 6, 12))
    p <- c(cl = 1, vd = 10)
    r <- rxSolve(m4, p, e)
    expect_true(nrow(r) > 0)
  })

  test_that("classic rxode2 internal evid >= 100 passes through verbatim", {
    # internal evid for a bolus to cmt 1: cmt100=0, rateI=0, cmt99=1, flg=1 => evid=101
    m5 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 24) {
        evid_(t + 12, 101, 1, 50, 0, 0, 0)
      }
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 24, by = 1))
    p <- c(ka = 0.5, cl = 1, vd = 10)
    r <- rxSolve(m5, p, e)
    expect_true(nrow(r) > 0)
    cp12 <- r$cp[r$time == 12]
    cp14 <- r$cp[r$time == 14]
    expect_true(length(cp12) > 0 && length(cp14) > 0)
    expect_true(cp14 > cp12)
  })
})
