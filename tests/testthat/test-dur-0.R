rxTest({

  test_that("dur=0 is treated as a bolus in non steady-state doses", {

    f <- rxode2({
      cl <- 1.1
      v <- 20
      d/dt(central) <- - (cl/v)*central
      dur(central) <- dur2
      cp <- central/(v/1000)
    })

    e <- et(rate=-2, amt=100) %>%
      seq(0, 10)

    s <- rxSolve(f, e, c(dur2=0))

    e <- et(amt=100) %>%
      seq(0, 10)

    s2 <- rxSolve(f, e, c(dur2=100))

    expect_equal(s$central, s2$central)
    expect_equal(s$cp, s2$cp)

  })



})
