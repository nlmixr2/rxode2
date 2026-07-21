rxTest({
  # sigdig sets the ODE solver tolerances with the common convention: atol well
  # below rtol, and tighter for a stiff (or auto-switching) solver than for a
  # purely non-stiff one.  Sensitivity + steady-state solves run one order looser.

  test_that("sigdig gives stiff tolerances for a stiff / auto-switching solver", {
    for (m in c("liblsoda", "lsoda", "ros4", "cvode", "bdf", "dop853+ros4")) {
      .c <- rxControl(sigdig = 4, method = m)
      expect_equal(.c$rtol, 1e-7, info = m)
      expect_equal(.c$atol, 1e-9, info = m)
      expect_equal(.c$rtolSens, 1e-6, info = m)
      expect_equal(.c$atolSens, 1e-8, info = m)
      expect_equal(.c$ssRtol[1], 1e-6, info = m)
      expect_equal(.c$ssAtol[1], 1e-8, info = m)
    }
  })

  test_that("sigdig gives looser non-stiff tolerances for a purely non-stiff solver", {
    for (m in c("dop853", "dop5", "rk4", "vern65")) {
      .c <- rxControl(sigdig = 4, method = m)
      expect_equal(.c$rtol, 1e-4, info = m)
      expect_equal(.c$atol, 1e-7, info = m)
      expect_equal(.c$rtolSens, 1e-3, info = m)
      expect_equal(.c$atolSens, 1e-6, info = m)
    }
  })

  test_that("sigdig scales the tolerances by one order per significant digit", {
    .c <- rxControl(sigdig = 6, method = "liblsoda")   # stiff
    expect_equal(.c$rtol, 1e-9)
    expect_equal(.c$atol, 1e-11)
    .d <- rxControl(sigdig = 6, method = "dop853")      # non-stiff
    expect_equal(.d$rtol, 1e-6)
    expect_equal(.d$atol, 1e-9)
  })

  test_that("an explicit atol/rtol overrides the sigdig-derived value", {
    .c <- rxControl(sigdig = 4, method = "liblsoda", atol = 1e-12, rtol = 1e-10)
    expect_equal(.c$atol, 1e-12)
    expect_equal(.c$rtol, 1e-10)
  })
})
