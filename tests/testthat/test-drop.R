rxodeTest(
  {
    context("'drop' test")

    ode <- rxode2("
         d/dt(X) = a*X + Y*Z;
         d/dt(Y) = b*(Y - Z);
         d/dt(Z) = -X*Y + c*Y - Z;")

    et <- eventTable() # default time units
    et$add.sampling(seq(from = 0, to = 10, by = 1))

    out0 <- rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1)
    )

    out <- rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1),
      drop = "X"
    )

    expect_equal(names(out), c("time", "Y", "Z"))
    expect_equal(out$time, out0$time)
    expect_equal(out$Y, out0$Y)
    expect_equal(out$Z, out0$Z)

    out <- rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1),
      drop = c("X", "Y")
    )

    expect_equal(names(out), c("time", "Z"))
    expect_equal(out$time, out0$time)
    expect_equal(out$Z, out0$Z)


    expect_warning(rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1),
      drop = c("X", "Y", "ww")
    ))

    expect_warning(rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1),
      drop = c("X", "Y", "ww"), warnDrop = FALSE
    ), NA)

    expect_warning(rxSolve(ode,
      params = c(a = -8 / 3, b = -10, c = 28),
      events = et, inits = c(X = 1, Y = 1, Z = 1),
      drop = "time"
    ))
  },
  test = "cran"
)
