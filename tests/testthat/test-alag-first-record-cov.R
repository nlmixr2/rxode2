rxTest({
  # A lagged first dose re-sorts the records, so the record at sorted slot 0
  # is no longer record 0; time-zero covariates must come from the right one
  # (nlmixr2/nlmixr2est#1182)
  test_that("time-zero covariates follow the sorted record with a lagged dose", {
    mod <- rxode2({
      alag(depot) <- 1
      d/dt(depot) <- -depot
      d/dt(central) <- depot - 0.1 * central
      w <- wt
    })
    d <- data.frame(
      id = 1,
      time = c(0, 0, 0.5, 2),
      evid = c(1, 0, 0, 0),
      amt = c(100, 0, 0, 0),
      cmt = 1,
      wt = c(10, 20, 30, 40)
    )
    s <- rxSolve(mod, d, covsInterpolation = "locf")
    expect_equal(s$time, c(0, 0.5, 2))
    expect_equal(s$w, c(20, 30, 40))
  })

  test_that("a time-zero observation keeps its endpoint with a lagged dose", {
    f <- function() {
      ini({
        tlag <- fixed(1)
        r0 <- fixed(100)
        addPk <- 1
        addPd <- 1
      })
      model({
        alag(depot) <- tlag
        response(0) <- r0
        d/dt(depot) <- -depot
        d/dt(central) <- depot - 0.1 * central
        d/dt(response) <- 0.1 * r0 - 0.1 * response
        conc <- central / 10
        conc ~ add(addPk)
        response ~ add(addPd)
      })
    }
    d <- data.frame(
      ID = 1,
      TIME = c(0, 0, 0.5, 2),
      AMT = c(100, 0, 0, 0),
      DV = c(0, 100, 100, 6),
      DVID = c(0, 2, 2, 1),
      EVID = c(1, 0, 0, 0)
    )
    s <- rxSolve(f, d, returnType = "data.frame")
    expect_equal(s$time, c(0, 0.5, 2))
    expect_equal(s$CMT, c(3L, 3L, 4L))
  })
})
