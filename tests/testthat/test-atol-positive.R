rxTest({
  # A zero absolute tolerance gives a failed or wrong solve (#1440)
  test_that("rxControl() rejects a zero absolute tolerance (#1440)", {
    expect_error(rxControl(atol = 0), "'atol' must be > 0")
    expect_error(rxControl(atolSens = 0), "'atolSens' must be > 0")
    expect_error(rxControl(ssAtol = 0), "'ssAtol' must be > 0")
    expect_error(rxControl(ssAtolSens = 0), "'ssAtolSens' must be > 0")
    expect_error(rxControl(atol = c(1e-8, 0)), "'atol' must be > 0")
    expect_error(rxControl(ssAtol = c(0, 1e-8)), "'ssAtol' must be > 0")
    expect_error(rxControl(atol = -1e-8), "atol")
    expect_error(rxControl(atol = NA_real_), "atol")
    expect_error(rxControl(atol = Inf), "atol")
    # rtol = 0 (pure absolute error control) is still allowed
    expect_error(rxControl(rtol = 0, rtolSens = 0, ssRtol = 0, ssRtolSens = 0), NA)
    .c <- rxControl(atol = 1e-10, atolSens = 1e-9, ssAtol = c(1e-7, 1e-6), ssAtolSens = 1e-5)
    expect_equal(.c$atol, 1e-10)
    expect_equal(.c$atolSens, 1e-9)
    expect_equal(.c$ssAtol, c(1e-7, 1e-6))
    expect_equal(.c$ssAtolSens, 1e-5)
  })

  test_that("rxSolve() rejects a zero absolute tolerance (#1440)", {
    m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
    })
    ev <- et(amt = 320) |> et(seq(0.25, 24, by = 0.25))
    p <- c(ka = 1.5, cl = 2.7, v = 31)
    for (meth in c("liblsoda", "lsoda", "dop853")) {
      expect_error(rxSolve(m, p, ev, method = meth, atol = 0), "'atol' must be > 0", info = meth)
      expect_error(rxSolve(m, p, ev, method = meth, atol = c(1e-8, 0)), "'atol' must be > 0", info = meth)
    }
    expect_error(rxSolve(m, p, ev, ssAtol = 0), "'ssAtol' must be > 0")
  })

  test_that("a zero atolSens cannot reach the solve through rxControlUpdateSens() (#1440)", {
    m <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
      d/dt(sdepot) <- -depot - ka * sdepot
      d/dt(scentral) <- depot + ka * sdepot - cl / v * scentral
    })
    ev <- et(amt = 320) |> et(seq(0.25, 24, by = 0.25))
    p <- c(ka = 1.5, cl = 2.7, v = 31)
    .c <- rxControlUpdateSens(rxControl(atolSens = 1e-6), 2L, 4L)
    expect_equal(.c$atol, c(1e-8, 1e-8, 1e-6, 1e-6))
    expect_true(all(.c$ssAtol > 0))
    # a control list whose atolSens was overwritten after rxControl()
    .c <- rxControl()
    .c$atolSens <- 0
    .c <- rxControlUpdateSens(.c, 2L, 4L)
    expect_error(rxSolve(m, p, ev, atol = .c$atol, rtol = .c$rtol), "'atol' must be > 0")
    .c <- rxControl()
    .c$ssAtolSens <- 0
    .c <- rxControlUpdateSens(.c, 2L, 4L)
    expect_error(rxSolve(m, p, ev, ssAtol = .c$ssAtol), "'ssAtol' must be > 0")
  })
})
