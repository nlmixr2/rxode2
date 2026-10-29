rxTest({
  # A steady-state cycle runs on a clock from 0 to ii, as in NONMEM, so a
  # model reading t has a fixed point (rxode2#1585)
  .ssClockMod <- rxode2({
    d / dt(central) <- -(1 / 20) * central * (1 + 0.003 * t)
    f <- central / 20
  })
  .ssClockObs <- c(482, 485, 488, 491)
  .ssClockData <- data.frame(
    ID = 1,
    TIME = c(480, .ssClockObs),
    EVID = c(1, 0, 0, 0, 0),
    AMT = c(100, 0, 0, 0, 0),
    CMT = 1,
    II = c(12, 0, 0, 0, 0),
    SS = c(1, 0, 0, 0, 0)
  )
  # the cycle integrated over [0, ii], then the absolute clock from the dose
  .k <- function(t) (1 / 20) * (1 + 0.003 * t)
  .peak <- 100 / (1 - exp(-integrate(.k, 0, 12)$value))
  .ssClockExpected <- vapply(
    .ssClockObs,
    function(t) .peak * exp(-integrate(.k, 480, t)$value) / 20,
    numeric(1)
  )

  test_that("a steady-state bolus of a model reading t matches NONMEM", {
    for (.method in c("liblsoda", "lsoda", "dop853")) {
      for (.tol in c(1e-8, 1e-12)) {
        .f <- rxSolve(
          .ssClockMod,
          .ssClockData,
          method = .method,
          atol = .tol,
          rtol = .tol,
          ssAtol = 1e-12,
          ssRtol = 1e-12,
          returnType = "data.frame"
        )$f
        expect_equal(.f, .ssClockExpected, tolerance = 1e-6)
      }
    }
  })

  test_that("a steady-state bolus of a model reading t does not depend on maxSS", {
    .f <- function(maxSS) {
      rxSolve(
        .ssClockMod,
        .ssClockData,
        atol = 1e-8,
        rtol = 1e-8,
        maxSS = maxSS,
        returnType = "data.frame"
      )$f
    }
    expect_equal(.f(70), .f(1000), tolerance = 1e-6)
  })

  test_that("a lagged steady-state bolus of a model reading t converges", {
    .lagMod <- rxode2({
      d / dt(central) <- -(1 / 20) * central * (1 + 0.003 * t)
      alag(central) <- 2
      f <- central / 20
    })
    .f <- function(maxSS) {
      rxSolve(
        .lagMod,
        .ssClockData,
        atol = 1e-10,
        rtol = 1e-10,
        maxSS = maxSS,
        returnType = "data.frame"
      )$f
    }
    .f70 <- .f(70)
    expect_true(all(is.finite(.f70)))
    expect_equal(.f70, .f(1000), tolerance = 1e-6)
  })
  test_that("a lagged steady-state bolus reads covariates at the dose time", {
    .covMod <- rxode2({
      d / dt(central) <- -(cl / 20) * central
      alag(central) <- 2
      f <- central / 20
    })
    .early <- data.frame(
      ID = 1,
      TIME = 0,
      EVID = 2,
      AMT = 0,
      CMT = 1,
      II = 0,
      SS = 0,
      cl = 50
    )
    .d <- rbind(.early, cbind(.ssClockData, cl = 1))
    .f <- function(d) {
      rxSolve(
        .covMod,
        d,
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )$f
    }
    .ref <- .f(cbind(.ssClockData, cl = 1))
    .fCov <- .f(.d)
    expect_equal(.fCov[-1], .ref, tolerance = 1e-6)
  })
  # The steady-state infusion cycle on the 0..ii clock is the periodic solution
  # of the model reading the time since the cycle started
  .ssInfMod <- rxode2({
    d / dt(central) <- -(1 / 20) * central * (1 + 0.003 * t)
    f <- central / 20
  })
  .ssInfCycleMod <- rxode2({
    tc <- t - 12 * floor(t / 12 + 1e-9)
    d / dt(central) <- -(1 / 20) * central * (1 + 0.003 * tc)
    f <- central / 20
  })
  .ssInfObs <- c(1, 2.5, 5, 8, 11.5)
  .ssInf <- function(dur, maxSS = 1000) {
    .ev <- et(amt = 100, ii = 12, ss = 1, dur = dur, time = 0) |>
      et(.ssInfObs)
    rxSolve(
      .ssInfMod,
      .ev,
      atol = 1e-10,
      rtol = 1e-10,
      ssAtol = 1e-10,
      ssRtol = 1e-10,
      maxSS = maxSS,
      returnType = "data.frame"
    )$f
  }
  .ssInfPeriodic <- function(dur) {
    .ev <- et(amt = 100, ii = 12, addl = 299, dur = dur, time = 0) |>
      et(3588 + .ssInfObs)
    rxSolve(
      .ssInfCycleMod,
      .ev,
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )$f
  }

  test_that("a steady-state infusion of a model reading t is periodic", {
    for (.dur in c(3, 30)) {
      expect_equal(.ssInf(.dur), .ssInfPeriodic(.dur), tolerance = 1e-6)
      expect_equal(.ssInf(.dur, 70), .ssInf(.dur), tolerance = 1e-6)
    }
  })

  test_that("a lagged steady-state infusion of a model reading t converges", {
    .lagMod <- rxode2({
      d / dt(central) <- -(1 / 20) * central * (1 + 0.003 * t)
      alag(central) <- 2
      f <- central / 20
    })
    .f <- function(dur, maxSS) {
      .ev <- et(amt = 100, ii = 12, ss = 1, dur = dur, time = 480) |>
        et(480 + .ssInfObs)
      rxSolve(
        .lagMod,
        .ev,
        atol = 1e-10,
        rtol = 1e-10,
        maxSS = maxSS,
        returnType = "data.frame"
      )$f
    }
    for (.dur in c(3, 10, 11, 30)) {
      .f70 <- .f(.dur, 70)
      expect_true(all(is.finite(.f70)))
      expect_equal(.f70, .f(.dur, 1000), tolerance = 1e-6)
    }
  })
  test_that("tad() in a steady-state cycle counts from the cycle's dose", {
    .tadMod <- rxode2({
      d / dt(central) <- -(1 / 20) * central * (1 + 0.03 * tad())
      f <- central / 20
      dn <- dosenum()
    })
    .k <- function(t) (1 / 20) * (1 + 0.03 * t)
    .peak <- 100 / (1 - exp(-integrate(.k, 0, 12)$value))
    .expected <- vapply(
      .ssClockObs - 480,
      function(t) .peak * exp(-integrate(.k, 0, t)$value) / 20,
      numeric(1)
    )
    .s <- rxSolve(
      .tadMod,
      .ssClockData,
      atol = 1e-10,
      rtol = 1e-10,
      ssAtol = 1e-10,
      ssRtol = 1e-10,
      returnType = "data.frame"
    )
    expect_equal(.s$f, .expected, tolerance = 1e-6)
    expect_equal(.s$dn, rep(1, 4))
  })

  test_that("tad() in a steady-state infusion cycle counts from its start", {
    .tadMod <- rxode2({
      d / dt(central) <- -(1 / 20) * central * (1 + 0.03 * tad())
      f <- central / 20
    })
    .tadCycleMod <- rxode2({
      tc <- t - 12 * floor(t / 12 + 1e-9)
      d / dt(central) <- -(1 / 20) * central * (1 + 0.03 * tc)
      f <- central / 20
    })
    for (.dur in c(3, 30)) {
      .ss <- rxSolve(
        .tadMod,
        et(amt = 100, ii = 12, ss = 1, dur = .dur, time = 0) |>
          et(.ssInfObs),
        atol = 1e-10,
        rtol = 1e-10,
        ssAtol = 1e-10,
        ssRtol = 1e-10,
        returnType = "data.frame"
      )$f
      .periodic <- rxSolve(
        .tadCycleMod,
        et(amt = 100, ii = 12, addl = 299, dur = .dur, time = 0) |>
          et(3588 + .ssInfObs),
        atol = 1e-10,
        rtol = 1e-10,
        returnType = "data.frame"
      )$f
      expect_equal(.ss, .periodic, tolerance = 1e-6)
    }
  })
})
