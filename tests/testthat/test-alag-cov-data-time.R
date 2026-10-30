rxTest({
  # A lagged dose's arrival is not a record: covariates stay on the data
  # records' own times, as in NONMEM (#1584)
  .alagCovMod <- function() {
    rxode2({
      cl <- 10 * (WT / 70)^0.75
      v <- 50
      alag(central) <- 0.7
      d / dt(central) <- -cl / v * central
      ipred <- central / v
    })
  }
  .alagCovDat <- data.frame(
    ID = 1,
    TIME = c(0, 5, 6, 6.5, 7),
    AMT = c(100, 0, 100, 0, 0),
    EVID = c(1, 0, 1, 0, 0),
    WT = c(70, 150, 150, 75, 75)
  )
  .alagK <- function(wt) 10 * (wt / 70)^0.75 / 50

  test_that("nocb covariates follow the data records with a lagged dose", {
    k <- .alagK
    a5 <- 100 * exp(-k(150) * 4.3)
    a65 <- a5 * exp(-k(150)) * exp(-k(75) * 0.5)
    a7 <- (a65 * exp(-k(75) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(.alagCovMod(), .alagCovDat, covsInterpolation = "nocb", atol = 1e-12, rtol = 1e-12)
    expect_equal(s$ipred, c(a5, a65, a7) / 50, tolerance = 1e-7)
  })

  test_that("locf covariates follow the data records with a lagged dose", {
    k <- .alagK
    a5 <- 100 * exp(-k(70) * 4.3)
    a65 <- a5 * exp(-k(150) * 1.5)
    a7 <- (a65 * exp(-k(75) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(.alagCovMod(), .alagCovDat, covsInterpolation = "locf", atol = 1e-12, rtol = 1e-12)
    expect_equal(s$ipred, c(a5, a65, a7) / 50, tolerance = 1e-7)
  })
})

rxTest({
  test_that("same-time records keep their own covariates with alag()", {
    mod <- rxode2({
      alag(central) <- 0.5
      d / dt(central) <- -0.1 * central
      w <- WT
    })
    d <- data.frame(
      ID = 1,
      TIME = c(0, 1, 1, 2),
      AMT = c(100, 0, 0, 0),
      EVID = c(1, 0, 0, 0),
      WT = c(70, 70, 140, 140)
    )
    s <- rxSolve(mod, d, covsInterpolation = "nocb")
    expect_equal(s$w, c(70, 140, 140))
  })
})
