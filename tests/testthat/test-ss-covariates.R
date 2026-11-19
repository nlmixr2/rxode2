rxTest({
  # A steady-state dose reaches steady state with the covariates of its own
  # record, never another record's (rxode2#1589).
  .k <- function(wt) 10 * (wt / 70)^0.75 / 50
  .mod <- rxode2({
    cl <- 10 * (WT / 70)^0.75
    v <- 50
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })
  .solveFirst <- function(mod, d, interp = "nocb", params = NULL) {
    rxSolve(
      mod,
      d,
      params = params,
      covsInterpolation = interp,
      atol = 1e-12,
      rtol = 1e-12,
      ssAtol = 1e-12,
      ssRtol = 1e-12
    )$cp[1]
  }

  test_that("the off phase of an ss infusion uses the dose record's covariates (#1589)", {
    .inf <- function(wt6, interp) {
      d <- data.frame(
        ID = 1,
        TIME = c(0, 0.5, 6),
        EVID = c(1, 0, 0),
        AMT = c(600, 0, 0),
        RATE = c(100, 0, 0),
        SS = c(1, 0, 0),
        II = c(12, 0, 0),
        WT = c(70, 70, wt6)
      )
      .solveFirst(.mod, d, interp)
    }
    .kk <- .k(70)
    .trough <- 100 / .kk * (1 - exp(-.kk * 6)) * exp(-.kk * 6) / (1 - exp(-.kk * 12))
    .expected <- (100 / .kk + (.trough - 100 / .kk) * exp(-.kk * 0.5)) / 50
    for (.interp in c("nocb", "locf")) {
      expect_equal(.inf(70, .interp), .expected, tolerance = 1e-7)
      expect_equal(.inf(140, .interp), .expected, tolerance = 1e-7)
    }
  })

  test_that("an ss infusion longer than ii uses the dose record's covariates (#1589)", {
    .inf <- function(wt12, interp) {
      d <- data.frame(
        ID = 1,
        TIME = c(0, 0.5, 12),
        EVID = c(1, 0, 0),
        AMT = c(1800, 0, 0),
        RATE = c(100, 0, 0),
        SS = c(1, 0, 0),
        II = c(12, 0, 0),
        WT = c(70, 70, wt12)
      )
      .solveFirst(.mod, d, interp)
    }
    for (.interp in c("nocb", "locf")) {
      expect_equal(.inf(140, .interp), .inf(70, .interp), tolerance = 1e-7)
    }
  })

  .modLag <- rxode2({
    cl <- 10 * (WT / 70)^0.75
    v <- 50
    alag(central) <- lag
    d/dt(central) <- -cl / v * central
    cp <- central / v
  })

  test_that("a lagged ss bolus places the lag with the dose record's covariates (#1589)", {
    .bolus <- function(wtLast, lag, interp) {
      d <- data.frame(
        ID = 1,
        TIME = c(0, 1, 24),
        EVID = c(1, 0, 0),
        AMT = c(100, 0, 0),
        SS = c(1, 0, 0),
        II = c(12, 0, 0),
        WT = c(70, 70, wtLast)
      )
      .solveFirst(.modLag, d, interp, params = c(lag = lag))
    }
    for (.interp in c("nocb", "locf")) {
      for (.lag in c(0, 0.7)) {
        .expected <- 100 * exp(-.k(70) * (1 - .lag)) / (1 - exp(-.k(70) * 12)) / 50
        expect_equal(.bolus(70, .lag, .interp), .expected, tolerance = 1e-7)
        expect_equal(.bolus(140, .lag, .interp), .expected, tolerance = 1e-7)
      }
    }
  })

  test_that("a lagged ss infusion places the lag with the dose record's covariates (#1589)", {
    .inf <- function(wtLast, lag, amt, interp) {
      .t <- c(0, seq(0.5, 24, by = 0.5))
      d <- data.frame(
        ID = 1,
        TIME = .t,
        EVID = c(1, rep(0, length(.t) - 1)),
        AMT = c(amt, rep(0, length(.t) - 1)),
        RATE = c(100, rep(0, length(.t) - 1)),
        SS = c(1, rep(0, length(.t) - 1)),
        II = c(12, rep(0, length(.t) - 1)),
        WT = c(rep(70, length(.t) - 1), wtLast)
      )
      .s <- rxSolve(
        .modLag,
        d,
        params = c(lag = lag),
        covsInterpolation = interp,
        atol = 1e-12,
        rtol = 1e-12,
        ssAtol = 1e-12,
        ssRtol = 1e-12
      )
      .s$cp[.s$time < 24]
    }
    ## lag 6 (lag + dur == ii), lag 7 (lag > ii - dur), dur > ii
    .cases <- list(c(0, 600), c(0.7, 600), c(6, 600), c(7, 600), c(0.7, 1800))
    for (.interp in c("nocb", "locf")) {
      for (.c in .cases) {
        expect_equal(.inf(140, .c[1], .c[2], .interp), .inf(70, .c[1], .c[2], .interp), tolerance = 1e-7)
      }
    }
  })
})
