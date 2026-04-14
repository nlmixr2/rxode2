rxTest({
  test_that("exceeding maxExtra raises an error (single subject)", {
    # This model unconditionally pushes a new event at t+0.1 on every ODE
    # evaluation step.  Each integration interval (0.1 wide) produces one push,
    # so seq(0, 2, by=0.1) creates 20 intervals → 20 pushes, which exceeds
    # maxExtra=10 and must raise an error.
    mCascade <- rxode2({
      d/dt(x) <- -x
      evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mCascade, c(x = 1), e, maxExtra = 10L),
      regexp = "maxExtra"
    )

    # Now with linear compartment model -- bolus
    mLinCascade <- rxode2({
      cp <- linCmt(cl, v)
      evid_(t+0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mLinCascade, c(cl = 2, v=1), e, maxExtra = 10L),
      regexp = "maxExtra")

    # Oral
    mLinCascade <- rxode2({
      cp <- linCmt(ka, cl, v)
      evid_(t+0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mLinCascade, c(ka=0.02, cl = 2, v=1), e, maxExtra = 10L),
      regexp = "maxExtra")

  })

  test_that("exceeding maxExtra does not crash R with parallel subjects", {
    # Same cascading model, multiple subjects solved in parallel.
    # Must raise an error rather than segfaulting.
    mCascade <- rxode2({
      d/dt(x) <- -x
      evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    nSub <- 4L
    params <- data.frame(id = seq_len(nSub), x = rep(1, nSub))
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_error(
      rxSolve(mCascade, params, e, maxExtra = 10L, cores = 2L),
      regexp = "maxExtra"
    )

    # Try with linear compartment model
    mLinCascade <- rxode2({
      cp <- linCmt(cl, v)
      evid_(t+0.1, 1, 10, 1, 0, 0, 0, 0)
    })

    params <- data.frame(id = seq_len(nSub), cl = rep(2, nSub),
                         v=rep(1, nSub))

    expect_error(
      rxSolve(mCascade, params, e, maxExtra = 10L, cores = 2L),
      regexp = "maxExtra"
    )

    mLinCascade <- rxode2({
      cp <- linCmt(ka, cl, v)
      evid_(t+0.1, 1, 10, 1, 0, 0, 0, 0)
    })

    params <- data.frame(id = seq_len(nSub), cl = rep(2, nSub),
                         v=rep(1, nSub), ka=rep(0.02, nSub))

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
      evid_(t + 0.1, 1, 10, 1, 0, 0, 0, 0)
    })
    e <- et(amt = 1, time = 0) |> et(seq(0, 2, by = 0.1))
    expect_no_error(rxSolve(m, c(x = 1), e, maxExtra = 0L))
  })

  test_that("bolus push via evid_() adds extra timepoints and affects trajectory", {

    for (meth in c("dop853", "liblsoda")) {
      m <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl/vd * central
        cp <- central / vd
        if (t < 24) {
          evid_(t + 12, 1, 50, 1, 0, 0, 0, 0)  # bolus 50 units to cmt 1 at t+12
        }
      })
      e <- et(amt = 100, time = 0) |>
        et(seq(0, 24, by = 1))
      p <- c(ka = 0.5, cl = 1, vd = 10)
      r <- rxSolve(m, p, e, method = meth)
      # The pushed dose at t+12 should create an extra event, affecting cp after t=12
      expect_true(nrow(r) > 0)
      # The bolus at t=12 should be visible — cp should rise after t=12
      cp12 <- r$cp[r$time == 12]
      cp14 <- r$cp[r$time == 14]
      expect_true(length(cp12) > 0 && length(cp14) > 0)
      expect_true(cp14 > cp12)

      m2 <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl/vd * central
        cp <- central / vd
      })
      e2 <- et(amt = 100, time = 0) |>
        et(seq(0, 24, by = 1)) |>
        et(amt = 50, time = 12, ii=1, until=30)

      p <- c(ka = 0.5, cl = 1, vd = 10)
      r2 <- rxSolve(m2, p, e2, method = meth)

      expect_equal(r$cp, r2$cp, tolerance = 1e-5)
    }

    # Now try with linCmt() only
    m <- rxode2({
      cp <- linCmt(ka, cl, v)
      if (t < 24) {
        evid_(t + 12, 1, 50, 1, 0, 0, 0, 0)  # bolus 50 units to cmt 1 at t+12
      }
    })
    e <- et(amt = 100, time = 0) |>
      et(seq(0, 24, by = 1))
    p <- c(ka = 0.5, cl = 1, v = 10)
    rLin <- rxSolve(m, p, e)
    # rLin$time >= 19 should be non-zero
    expect_true(all(rLin$cp[rLin$time >= 19] > 0))
    # The pushed dose at t+12 should create an extra event, affecting cp after t=12
    expect_true(nrow(r) > 0)
    # The bolus at t=12 should be visible — cp should rise after t=12
    cp12 <- rLin$cp[rLin$time == 12]
    cp14 <- rLin$cp[rLin$time == 14]
    expect_true(length(cp12) > 0 && length(cp14) > 0)
    expect_true(cp14 > cp12)

    m2 <- rxode2({
      cp <- linCmt(ka, cl, v)
    })
    e2 <- et(amt = 100, time = 0) |>
      et(seq(0, 24, by = 1)) |>
      et(amt = 50, time = 12, ii=1, until=30)

    rLin2 <- rxSolve(m, p, e)

    expect_equal(rLin$cp, rLin2$cp)

  })

  test_that("infusion push via evid_() adds start+stop events", {
    for (meth in c("dop853", "liblsoda")) {
      m <- rxode2({
        d/dt(central) <- -cl / vd * central
        cp <- central / vd
        if (t >= 14 && t < 15) {
          evid_(15, 1, 100, 1, 10, 0, 0, 0)  # rate=10, so dur=10h; push from t=14
        }
      })
      e <- et(amt = 100, time = 0) |> et(seq(0, 30, by = 1))
      p <- c(cl = 1, vd = 10)
      r <- rxSolve(m, p, e, method=meth)
      expect_true(nrow(r) > 0)
      # Central should rise during the pushed infusion window (t=15 to t=25)
      cp7  <- r$cp[r$time == 7]
      cp20 <- r$cp[r$time == 20]
      expect_true(length(cp7) > 0 && length(cp20) > 0)
      # At t=7, infusion is active; at t=20, it has stopped and cp is declining
      expect_true(cp7 > 0)

      m2 <- rxode2({
        d/dt(central) <- -cl / vd * central
        cp <- central / vd
      })

      e2 <- et(amt = 100, time = 0) |>
        et(seq(0, 30, by = 1)) |>
        et(amt=100, time=15, rate=10)

      r2 <- rxSolve(m2, p, e2, method=meth)

      expect_equal(r$cp, r2$cp, tolerance = 1e-5)
    }
  })

  test_that("past-time evid_() produces a warning", {
    m3 <- rxode2({
      d/dt(x) <- -x
      if (t < 5) {
        evid_(t - 1, 1, 10, 1, 0, 0, 0, 0)  # past time — should warn
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
        evid_(t + 5.5, 0, 0, 1, 0, 0, 0, 0)  # push observation at t+5.5
      }
    })
    e <- et(amt = 100, time = 0) |> et(c(0, 6, 12))
    p <- c(cl = 1, vd = 10)
    r <- rxSolve(m4, p, e)
    expect_equal(nrow(r), 5) # 5 observations should be in the output
  })

  test_that("addl doses via evid_() push multiple doses at ii intervals", {
    # evid_(t+6, 1, 50, 1, 0, 12, 2, 0) pushes 3 boluses: t+6, t+18, t+30
    m6 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 1) {
        evid_(t + 6, 1, 50, 1, 0, 12, 2, 0)
      }
    })
    e <- et(amt = 100, time = 0) |> et(seq(0, 36, by = 1))
    p <- c(ka = 0.5, cl = 1, vd = 10)
    r <- rxSolve(m6, p, e)
    expect_true(nrow(r) > 0)
    # At t=28: long after t=18 dose, before t=30 dose — cp should be low
    # At t=31: 1h after t=30 pushed dose — cp should be rising
    cp28 <- r$cp[r$time == 28]
    cp31 <- r$cp[r$time == 31]
    expect_true(length(cp28) > 0 && length(cp31) > 0)
    expect_true(cp31 > cp28)
  })

  test_that("classic rxode2 internal evid >= 100 passes through verbatim", {
    # internal evid for a bolus to cmt 1: cmt100=0, rateI=0, cmt99=1, flg=1 => evid=101
    m5 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 24) {
        evid_(t + 12, 101, 50, 1, 0, 0, 0, 0)
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

  test_that("evid_() ui changes work", {

    f <- function() {
      model({
        evid_(t + 12, 101, 50)
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "evid_(t + 12, 101, 50, 1, 0, 0, 0, 0);\n")

  })
})
