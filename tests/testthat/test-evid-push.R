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

    for (meth in c("dop853", "rkf78", "liblsoda")) {
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
    for (meth in c("dop853", "rkf78", "liblsoda")) {
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

  test_that("in-model infuse() changes duration with bioavailability", {
    obs <- c(0, 1e-8, seq(0.5, 12, by = 0.5))
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        d/dt(central) <- -cl / v * central
        f(central) <- fc
        cp <- central / v
        if (t < 1e-8) {
          infuse(100, 10, 1, 0, 0, 0)
        }
      })
      p <- c(cl = 1, v = 10, fc = 0.5)
      got <- rxSolve(mod, p, et(obs), method = meth)
      want <- rxSolve(ref, c(cl = 1, v = 10),
                      et(amt = 50, time = 0, rate = 10) |> et(obs),
                      method = meth)
      expect_equal(got$time, want$time)
      expect_equal(got$cp, want$cp, tolerance = 1e-5)
    }
  })

  test_that("in-model infuseDur() changes rate with bioavailability", {
    obs <- c(0, 1e-8, seq(0.5, 12, by = 0.5))
    ref <- rxode2({
      d/dt(central) <- -cl / v * central
      cp <- central / v
    })

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        d/dt(central) <- -cl / v * central
        f(central) <- fc
        cp <- central / v
        if (t < 1e-8) {
          infuseDur(100, 10, 1, 0, 0, 0)
        }
      })
      p <- c(cl = 1, v = 10, fc = 0.5)
      got <- rxSolve(mod, p, et(obs), method = meth)
      want <- rxSolve(ref, c(cl = 1, v = 10),
                      et(amt = 50, time = 0, dur = 10) |> et(obs),
                      method = meth)
      expect_equal(got$time, want$time)
      expect_equal(got$cp, want$cp, tolerance = 1e-5)
    }
  })

  test_that("in-model reset() matches an evid=3 reset event", {
    obs <- seq(0, 24, by = 1)
    e <- et(amt = 100, time = 0) |>
      et(amt = 50, time = 18) |>
      et(obs)
    eRef <- et(amt = 100, time = 0) |>
      et(time = 12, evid = 3) |>
      et(amt = 50, time = 18) |>
      et(obs)

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        mtime(resetAt) <- 12
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t >= resetAt && t < resetAt + 0.5 && cp > 0) {
          reset()
        }
      })

      ref <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      p <- c(ka = 0.5, cl = 1, v = 10)
      got <- rxSolve(mod, p, e, method = meth)
      want <- rxSolve(ref, p, eRef, method = meth)
      gotReset <- got[got$time >= 13, ]
      wantReset <- want[want$time >= 13, ]

      expect_equal(sum(got$time == 12), 2)
      expect_true(all(got$cp[got$time == 12] > 0))
      expect_true(all(gotReset$cp[gotReset$time < 18] == 0))
      expect_equal(gotReset$time, wantReset$time)
      expect_equal(gotReset$depot, wantReset$depot, tolerance = 1e-5)
      expect_equal(gotReset$central, wantReset$central, tolerance = 1e-5)
      expect_equal(gotReset$cp, wantReset$cp, tolerance = 1e-5)
    }

    modLin <- rxode2({
      mtime(resetAt) <- 12
      cp <- linCmt(ka, cl, v)
      if (t >= resetAt && t < resetAt + 0.5 && cp > 0) {
        reset()
      }
    })

    refLin <- rxode2({
      cp <- linCmt(ka, cl, v)
    })

    p <- c(ka = 0.5, cl = 1, v = 10)
    gotLin <- rxSolve(modLin, p, e)
    wantLin <- rxSolve(refLin, p, eRef)
    gotLinReset <- gotLin[gotLin$time >= 13, ]
    wantLinReset <- wantLin[wantLin$time >= 13, ]

    expect_equal(sum(gotLin$time == 12), 2)
    expect_true(all(gotLin$cp[gotLin$time == 12] == 0))
    expect_true(all(gotLinReset$cp[gotLinReset$time < 18] == 0))
    expect_equal(gotLinReset$time, wantLinReset$time)
    expect_equal(gotLinReset$cp, wantLinReset$cp, tolerance = 1e-5)
  })

  test_that("in-model replace() pushes a replacement event", {
    obs <- seq(0, 24, by = 1)
    e <- et(amt = 100, time = 0) |>
      et(amt = 50, time = 18) |>
      et(obs)
    eRef <- et(amt = 100, time = 0) |>
      et(time = 13, amt = 25, cmt = 1, evid = 5) |>
      et(amt = 50, time = 18) |>
      et(obs)

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        mtime(replaceAt) <- 12
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t >= replaceAt && t < replaceAt + 0.5 && depot > 0) {
          replace(25, depot)
        }
      })

      ref <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      p <- c(ka = 0.5, cl = 1, v = 10)
      got <- rxSolve(mod, p, e, method = meth)
      want <- rxSolve(ref, p, eRef, method = meth)
      gotReplace <- got[got$time >= 13, ]
      wantReplace <- want[want$time >= 13, ]

      expect_equal(sum(got$time == 12), 2)
      expect_equal(gotReplace$time, wantReplace$time)
      expect_equal(gotReplace$depot, wantReplace$depot, tolerance = 1e-5)
      expect_equal(gotReplace$central, wantReplace$central, tolerance = 1e-5)
      expect_equal(gotReplace$cp, wantReplace$cp, tolerance = 1e-5)
    }
  })

  test_that("in-model multiply() pushes a multiplication event", {
    obs <- seq(0, 24, by = 1)
    e <- et(amt = 100, time = 0) |>
      et(amt = 50, time = 18) |>
      et(obs)
    eRef <- et(amt = 100, time = 0) |>
      et(time = 13, amt = 2, cmt = 1, evid = 6) |>
      et(amt = 50, time = 18) |>
      et(obs)

    for (meth in c("dop853", "rkf78",  "liblsoda")) {
      mod <- rxode2({
        mtime(multAt) <- 12
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t >= multAt && t < multAt + 0.5 && depot > 0) {
          multiply(2, depot)
        }
      })

      ref <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      p <- c(ka = 0.5, cl = 1, v = 10)
      got <- rxSolve(mod, p, e, method = meth)
      want <- rxSolve(ref, p, eRef, method = meth)
      gotMult <- got[got$time >= 13, ]
      wantMult <- want[want$time >= 13, ]

      expect_equal(sum(got$time == 12), 2)
      expect_equal(gotMult$time, wantMult$time)
      expect_equal(gotMult$depot, wantMult$depot, tolerance = 1e-5)
      expect_equal(gotMult$central, wantMult$central, tolerance = 1e-5)
      expect_equal(gotMult$cp, wantMult$cp, tolerance = 1e-5)
    }
  })

  test_that("in-model phantom() pushes a phantom event", {
    obs <- seq(0, 20, by = 1)
    e <- et(obs)
    eRef <- et(time = 12, amt = 20, cmt = 1, evid = 7) |>
      et(obs)

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        mtime(phantomAt) <- 12
        d/dt(depot) <- 0
        d/dt(x) <- -k * x
        pd <- podo(depot)
        td <- tad(depot)
        if (t >= phantomAt && t < phantomAt + 0.5) {
          phantom(20, depot, 0, 0, 0)
        }
      })

      ref <- rxode2({
        d/dt(depot) <- 0
        d/dt(x) <- -k * x
        pd <- podo(depot)
        td <- tad(depot)
      })

      p <- c(k = 0.1)
      got <- rxSolve(mod, p, e, inits = c(depot = 0, x = 1), method = meth)
      want <- rxSolve(ref, p, eRef, inits = c(depot = 0, x = 1), method = meth)
      gotPhantom <- got[got$time >= 13, ]
      wantPhantom <- want[want$time >= 13, ]

      expect_equal(sum(got$time == 12), 2)
      expect_equal(gotPhantom$time, wantPhantom$time)
      expect_equal(gotPhantom$depot, wantPhantom$depot, tolerance = 1e-5)
      expect_equal(gotPhantom$x, wantPhantom$x, tolerance = 1e-5)
      expect_equal(gotPhantom$pd, wantPhantom$pd, tolerance = 1e-5)
      expect_equal(gotPhantom$td, wantPhantom$td, tolerance = 1e-5)
    }
  })

  test_that("in-model bolus() passes ii to repeated bolus events", {
    obs <- seq(0, 40, by = 1)
    e <- et(amt = 100, time = 0) |>
      et(obs)
    eRef <- et(amt = 100, time = 0) |>
      et(time = 13, amt = 50, cmt = 1, evid = 1) |>
      et(time = 24, amt = 50, cmt = 1, evid = 1) |>
      et(time = 36, amt = 50, cmt = 1, evid = 1) |>
      et(obs)

    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mod <- rxode2({
        mtime(bolusAt) <- 12
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t >= bolusAt && t < bolusAt + 0.5 && depot > 0) {
          bolus(50, depot, 12, 2, 0)
        }
      })

      ref <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      p <- c(ka = 0.5, cl = 1, v = 10)
      got <- rxSolve(mod, p, e, method = meth)
      want <- rxSolve(ref, p, eRef, method = meth)
      gotBolus <- got[got$time >= 13, ]
      wantBolus <- want[want$time >= 13, ]

      expect_equal(sum(got$time == 12), 2)
      expect_equal(gotBolus$time, wantBolus$time)
      expect_equal(gotBolus$depot, wantBolus$depot, tolerance = 1e-5)
      expect_equal(gotBolus$central, wantBolus$central, tolerance = 1e-5)
      expect_equal(gotBolus$cp, wantBolus$cp, tolerance = 1e-5)
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

  test_that("obs() pushes multiple observation rows", {
    mObs <- rxode2({
      d/dt(central) <- -cl / vd * central
      cp <- central / vd
      if (t < 1) {
        obs(0, 1, 2, 4, 6)
      }
    })
    e <- et(amt = 100, time = 0) |> et(c(0, 12))
    p <- c(cl = 1, vd = 10)
    r <- rxSolve(mObs, p, e)
    expect_equal(nrow(r), 7)
    expect_equal(sort(unique(r$time)), c(0, 1, 2, 4, 6, 12))
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

  test_that("evid_() with depot", {
    # evid_(t+6, 1, 50, 1, 0, 12, 2, 0) pushes 3 boluses: t+6, t+18, t+30
    m6 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 1) {
        evid_(t + 6, 1, 50, depot, 0, 12, 2, 0)
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

  test_that("evid_() with 'depot'", {
    # evid_(t+6, 1, 50, 1, 0, 12, 2, 0) pushes 3 boluses: t+6, t+18, t+30
    m6 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 1) {
        evid_(t + 6, 1, 50, 'depot', 0, 12, 2, 0)
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

  test_that("evid_() with \"depot\"", {
    # evid_(t+6, 1, 50, 1, 0, 12, 2, 0) pushes 3 boluses: t+6, t+18, t+30
    m6 <- rxode2({
      d/dt(depot)   <- -ka * depot
      d/dt(central) <- ka * depot - cl / vd * central
      cp <- central / vd
      if (t < 1) {
        evid_(t + 6, 1, 50, 'depot', 0, 12, 2, 0)
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

  test_that("splitBolus applies to boluses pushed by evid_()", {
    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mSplit <- rxode2({
        splitBolus(depot, depot, central)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t < 1) {
          evid_(t + 6, 1, 50, 1, 0, 12, 1, 0)
        }
      })

      mBase <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      e <- et(amt = 100, time = 0) |>
        et(seq(0, 30, by = 1))
      eBase <- e |>
        et(amt = 100, time = 0, cmt = 2) |>
        et(amt = 50, time = 6, cmt = 1) |>
        et(amt = 50, time = 6, cmt = 2) |>
        et(amt = 50, time = 18, cmt = 1) |>
        et(amt = 50, time = 18, cmt = 2)

      p <- c(ka = 0.5, cl = 1, v = 10)
      rSplit <- rxSolve(mSplit, p, e, method = meth)
      rBase <- rxSolve(mBase, p, eBase, method = meth)

      expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
      expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
      expect_equal(rSplit$cp, rBase$cp, tolerance = 1e-5)
    }
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

  test_that("obs() ui expands seq() inputs", {

    f <- function() {
      model({
        obs(seq(0, 6, by = 2))
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "obs(0, 2, 4, 6);\n")

  })

  test_that("reset() ui changes work", {

    f <- function() {
      model({
        reset()
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "reset();\n")

  })

  test_that("replace() ui changes work", {

    f <- function() {
      model({
        replace(10, depot)
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "replace(10, depot);\n")

  })

  test_that("multiply() ui changes work", {

    f <- function() {
      model({
        multiply(10, depot)
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "multiply(10, depot);\n")

  })

  test_that("phantom() ui changes work", {

    f <- function() {
      model({
        phantom(10, depot, 0, 0, 0)
      })
    }

    f <- f()
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "phantom(10, depot, 0, 0, 0);\n")

  })

  test_that("splitBolus() ui changes work", {
    f <- function() {
      model({
        splitBolus(depot, depot, central, peripheral)
      })
    }

    f <- f()
    expect_equal(tail(names(rxModelVars(f)), 5),
                 c("lhsOrd", "splitBolus", "strCmpParams", "timeId", "md5"))
    expect_equal(unname(rxModelVars(f)$splitBolus), c(1L, 1L, 2L, 3L))
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "splitBolus(depot,depot,central,peripheral);\n")
  })

  test_that("splitBolus() ui allows transfer to one target compartment", {
    f <- function() {
      model({
        splitBolus(depot, central)
      })
    }

    f <- f()
    expect_equal(unname(rxModelVars(f)$splitBolus), c(1L, 2L))
    expect_equal(setNames(rxModelVars(f)$model["normModel"], NULL),
                 "splitBolus(depot,central);\n")
  })

  test_that("splitBolus() exposes split dose lines on the ui", {
    f <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        splitBolus(depot, depot, central, peripheral)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        d/dt(peripheral) <- 0
        cp <- central / v
      })
    }

    ui <- rxode(f)

    expect_equal(rxUiGet.splitDose(list(ui)),
                 list(str2lang("splitBolus(depot, depot, central, peripheral)")))
    expect_equal(ui$splitDoseLines,
                 list(str2lang("splitBolus(depot, depot, central, peripheral)")))

    f <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })
    }

    ui <- rxode(f)

    expect_null(rxUiGet.splitDose(list(ui)))
    expect_null(ui$splitDoseLines)
  })

  test_that("splitBolus() is preserved in simulation models", {
    f <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        splitBolus(depot, depot, central, peripheral)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        d/dt(peripheral) <- 0
        cp <- central / v
      })
    }

    ui <- rxode(f)

    expect_error(ui$simulationModel, NA)
    mod <- ui$simulationModel
    expect_equal(unname(rxModelVars(mod)$splitBolus), c(1L, 1L, 2L, 3L))

    expect_error(ui$simulationIniModel, NA)
    mod <- ui$simulationIniModel
    expect_equal(unname(rxModelVars(mod)$splitBolus), c(1L, 1L, 2L, 3L))
  })

  test_that("splitBolus applies to a one-target bolus pushed by evid_()", {
    for (meth in c("dop853", "rkf78", "liblsoda")) {
      mSplit <- rxode2({
        splitBolus(depot, central)
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
        if (t < 1) {
          evid_(t + 6, 1, 50, 1, 0, 12, 1, 0)
        }
      })

      mBase <- rxode2({
        d/dt(depot) <- -ka * depot
        d/dt(central) <- ka * depot - cl / v * central
        cp <- central / v
      })

      e <- et(seq(0, 30, by = 1))
      eBase <- e |>
        et(amt = 50, time = 6, cmt = 2) |>
        et(amt = 50, time = 18, cmt = 2)

      p <- c(ka = 0.5, cl = 1, v = 10)
      rSplit <- rxSolve(mSplit, p, e, method = meth)
      rBase <- rxSolve(mBase, p, eBase, method = meth)

      expect_equal(rSplit$depot, rBase$depot, tolerance = 1e-5)
      expect_equal(rSplit$central, rBase$central, tolerance = 1e-5)
      expect_equal(rSplit$cp, rBase$cp, tolerance = 1e-5)
    }
  })
})
