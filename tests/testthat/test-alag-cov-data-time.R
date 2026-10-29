rxTest({
  # A lagged dose takes the covariates imputed at its arrival; they stay on the
  # data records' own times unless addlKeepsCov carries its record's (#1584)
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

  test_that("addlKeepsCov = TRUE carries a lagged dose's record covariates", {
    k <- .alagK
    a5 <- 100 * exp(-k(150) * 4.3)
    b65 <- a5 * exp(-k(75) * 1.5)
    b7 <- (b65 * exp(-k(150) * 0.2) + 100) * exp(-k(75) * 0.3)
    s <- rxSolve(
      .alagCovMod(),
      .alagCovDat,
      covsInterpolation = "nocb",
      addlKeepsCov = TRUE,
      atol = 1e-12,
      rtol = 1e-12
    )
    expect_equal(s$ipred, c(a5, b65, b7) / 50, tolerance = 1e-7)
  })

  test_that("nonmem = TRUE turns on addlKeepsCov unless it is given", {
    .s <- function(...) {
      rxSolve(
        .alagCovMod(),
        .alagCovDat,
        covsInterpolation = "nocb",
        nonmem = TRUE,
        atol = 1e-12,
        rtol = 1e-12,
        ...
      )$ipred
    }
    expect_equal(.s(), .s(addlKeepsCov = TRUE))
    expect_false(isTRUE(all.equal(.s(), .s(addlKeepsCov = FALSE))))
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

rxTest({
  test_that("f() of a lagged dose follows addlKeepsCov", {
    mod <- rxode2({
      f(central) <- WT / 70
      alag(central) <- 0.7
      d / dt(central) <- -0.1 * central
    })
    d <- data.frame(
      ID = 1,
      TIME = c(0, 6, 6.5, 7, 8),
      AMT = c(0, 100, 0, 0, 0),
      EVID = c(0, 1, 0, 0, 0),
      WT = c(70, 140, 70, 35, 35)
    )
    .f <- function(ci, keep) {
      s <- rxSolve(mod, d, covsInterpolation = ci, addlKeepsCov = keep, atol = 1e-12, rtol = 1e-12)
      s$central[s$time == 8] / (100 * exp(-0.1 * 1.3))
    }
    # imputed at the arrival, t = 6.7
    expect_equal(.f("locf", FALSE), 1, tolerance = 1e-6)
    expect_equal(.f("nocb", FALSE), 0.5, tolerance = 1e-6)
    expect_equal(.f("linear", FALSE), 0.8, tolerance = 1e-6)
    # the dose record's WT
    for (ci in c("locf", "nocb", "linear")) {
      expect_equal(.f(ci, TRUE), 2, tolerance = 1e-6)
    }
  })
})

rxTest({
  # linCmt() reads one covariate value per interval, so the lagged dose's data
  # time must end an interval of its own (#1584)
  .alagCovDat <- data.frame(
    ID = 1,
    TIME = c(0, 5, 6, 6.5, 7),
    AMT = c(100, 0, 100, 0, 0),
    EVID = c(1, 0, 1, 0, 0),
    WT = c(70, 150, 150, 75, 75)
  )
  .alagK <- function(wt) 10 * (wt / 70)^0.75 / 50
  .alagRef <- function(interp) {
    k <- .alagK
    if (interp == "nocb") {
      a5 <- 100 * exp(-k(150) * 4.3)
      a65 <- a5 * exp(-k(150)) * exp(-k(75) * 0.5)
    } else {
      a5 <- 100 * exp(-k(70) * 4.3)
      a65 <- a5 * exp(-k(150) * 1.5)
    }
    a7 <- (a65 * exp(-k(75) * 0.2) + 100) * exp(-k(75) * 0.3)
    c(a5, a65, a7) / 50
  }

  test_that("linCmt() covariates follow the data records with a lagged dose", {
    f <- function() {
      model({
        cl <- 10 * (WT / 70)^0.75
        v <- 50
        alag(central) <- 0.7
        ipred <- linCmt()
      })
    }
    for (interp in c("nocb", "locf")) {
      s <- rxSolve(f, .alagCovDat, covsInterpolation = interp)
      expect_equal(s$ipred, .alagRef(interp), tolerance = 1e-7)
    }
  })

  test_that("linCmt() with addlKeepsCov = TRUE carries a lagged dose's covariates", {
    k <- .alagK
    a5 <- 100 * exp(-k(150) * 4.3)
    b65 <- a5 * exp(-k(75) * 1.5)
    b7 <- (b65 * exp(-k(150) * 0.2) + 100) * exp(-k(75) * 0.3)
    f <- function() {
      model({
        cl <- 10 * (WT / 70)^0.75
        v <- 50
        alag(central) <- 0.7
        ipred <- linCmt()
      })
    }
    s <- rxSolve(f, .alagCovDat, covsInterpolation = "nocb", addlKeepsCov = TRUE)
    expect_equal(s$ipred, c(a5, b65, b7) / 50, tolerance = 1e-7)
  })

  test_that("linCmt() with an ODE splits at the lagged dose's data time", {
    f <- function() {
      model({
        cl <- 10 * (WT / 70)^0.75
        v <- 50
        alag(central) <- 0.7
        ipred <- linCmt()
        d / dt(eff) <- ipred - eff
      })
    }
    for (interp in c("nocb", "locf")) {
      s <- rxSolve(f, .alagCovDat, covsInterpolation = interp)
      expect_equal(s$ipred, .alagRef(interp), tolerance = 1e-7)
    }
  })
})

rxTest({
  test_that("linCmt() lagged dose covariates are unchanged after a reset", {
    f <- function() {
      model({
        cl <- 10 * (WT / 70)^0.75
        v <- 50
        alag(central) <- 0.7
        ipred <- linCmt()
      })
    }
    d <- data.frame(
      ID = 1,
      TIME = c(0, 5, 6, 6.5, 7),
      AMT = c(100, 0, 100, 0, 0),
      EVID = c(1, 0, 1, 0, 0),
      WT = c(70, 150, 150, 75, 75)
    )
    d2 <- rbind(
      data.frame(
        ID = 1,
        TIME = c(0, 50),
        AMT = c(100, 0),
        EVID = c(1, 3),
        WT = c(200, 70)
      ),
      transform(d, TIME = TIME + 100)
    )
    for (interp in c("nocb", "locf")) {
      a <- rxSolve(f, d, covsInterpolation = interp)
      b <- rxSolve(f, d2, covsInterpolation = interp)
      expect_equal(tail(b$ipred, 3), a$ipred, tolerance = 1e-7)
    }
  })
})
