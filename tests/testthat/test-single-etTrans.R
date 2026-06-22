rxTest({

  m <- function() {
    ini({ ka <- 1; cl <- 4; v <- 25 })
    model({
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl / v * central
      cp = central / v
    })
  }

  test_that("single=TRUE matches single=FALSE: simple bolus", {
    ev <- et(amt = 100) |> et(seq(0, 24, by = 0.5))
    r1 <- rxSolve(m, ev, single = FALSE)
    r2 <- rxSolve(m, ev, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: ADDL dosing", {
    ev <- et(amt = 100, ii = 24, addl = 4) |> et(seq(0, 120, by = 1))
    r1 <- rxSolve(m, ev, single = FALSE)
    r2 <- rxSolve(m, ev, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: SS dosing", {
    ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 0.5))
    r1 <- rxSolve(m, ev, single = FALSE)
    r2 <- rxSolve(m, ev, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: time-varying covariate", {
    m_cov <- function() {
      ini({ ka <- 1; cl <- 4; v <- 25 })
      model({
        d/dt(depot)   = -ka * depot
        d/dt(central) = ka * depot - (cl * wt / 70) / v * central
        cp = central / v
      })
    }
    ev <- et(amt = 100, ii = 24, addl = 2) |>
      et(seq(0, 72, by = 1)) |>
      as.data.frame()
    ev$wt <- ifelse(ev$time < 36, 70, 80)
    r1 <- rxSolve(m_cov, ev, single = FALSE)
    r2 <- rxSolve(m_cov, ev, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: iCov", {
    m_cov <- function() {
      ini({ ka <- 1; cl <- 4; v <- 25 })
      model({
        d/dt(depot)   = -ka * depot
        d/dt(central) = ka * depot - (cl * wt / 70) / v * central
        cp = central / v
      })
    }
    ev <- et(amt = 100, ii = 24, addl = 2) |> et(seq(0, 72, by = 1))
    ic <- data.frame(id = 1, wt = 70)
    r1 <- rxSolve(m_cov, ev, iCov = ic, single = FALSE)
    r2 <- rxSolve(m_cov, ev, iCov = ic, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: named CMT column", {
    ev <- et(amt = 100, cmt = "depot") |> et(seq(0, 24, by = 0.5))
    r1 <- rxSolve(m, ev, single = FALSE)
    r2 <- rxSolve(m, ev, single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
  })

  test_that("single=TRUE matches single=FALSE: keep= columns", {
    ev <- et(amt = 100, ii = 24, addl = 2) |>
      et(seq(0, 72, by = 1)) |>
      as.data.frame()
    ev$label <- 1L
    r1 <- rxSolve(m, ev, keep = "label", single = FALSE)
    r2 <- rxSolve(m, ev, keep = "label", single = TRUE)
    expect_equal(r1$cp, r2$cp, tolerance = 1e-10)
    expect_equal(r1$label, r2$label)
  })

})
