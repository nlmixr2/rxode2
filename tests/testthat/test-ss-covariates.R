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
})
