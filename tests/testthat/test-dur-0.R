rxTest({

  test_that("dur=0 is treated as a bolus in non steady-state doses (ODE)", {

    f <- rxode2({
      cl <- 1.1
      v <- 20
      d/dt(central) <- - (cl/v)*central
      dur(central) <- dur2
      cp <- central/(v/1000)
    })

    e <- et(rate=-2, amt=100)
    s <- rxSolve(f, e, c(dur2=0))

    e <- et(amt=100)
    s2 <- rxSolve(f, e, c(dur2=100))

    expect_equal(s$central, s2$central)
    expect_equal(s$cp, s2$cp)

  })

  test_that("dur=0 with f is treated the same as bolus with f (ODE)", {

    f <- rxode2({
      cl <- 1.1
      v <- 20
      d/dt(central) <- - (cl/v)*central
      dur(central) <- dur2
      f(central) <- fcentral
      cp <- central/(v/1000)
    })

    e <- et(rate=-2, amt=100)
    s <- rxSolve(f, e, c(dur2=0, fcentral=2))
    sb <- rxSolve(f, e, c(dur2=0, fcentral=0.5))

    e <- et(amt=100)
    s2 <- rxSolve(f, e, c(dur2=100, fcentral=2))
    s2b <- rxSolve(f, e, c(dur2=100, fcentral=0.5))

    expect_equal(s$central, s2$central)
    expect_equal(s$cp, s2$cp)

  })

  test_that("dur=0 is treated as a bolus in non steady-state doses (ODE, SS)", {

    f <- rxode2({
      cl <- 1.1
      v <- 20
      d/dt(central) <- - (cl/v)*central
      dur(central) <- dur2
      cp <- central/(v/1000)
    })

    e <- et(rate=-2, amt=100, ss=1, ii=12)
    s <- rxSolve(f, e, c(dur2=0))

    e <- et(amt=100, ss=1, ii=12)
    s2 <- rxSolve(f, e, c(dur2=100))

    expect_equal(s$central, s2$central)
    expect_equal(s$cp, s2$cp)

    f <- rxode2({
      cl <- 1.1
      v <- 20
      d/dt(central) <- - (cl/v)*central
      dur(central) <- dur2
      lag(central) <- lag2
      cp <- central/(v/1000)
    })

    e <- et(rate=-2, amt=100, ss=1, ii=12)
    s <- rxSolve(f, e, c(dur2=0, lag2=2))

    e <- et(amt=100, ss=1, ii=12)
    s2 <- rxSolve(f, e, c(dur2=100, lag2=2))

    expect_equal(s$central, s2$central, tolerance=1e-6)
    expect_equal(s$cp, s2$cp, tolerance=1e-6)

    expect_equal(s$time, s2$time)

  })


})
