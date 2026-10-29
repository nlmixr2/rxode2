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
})
