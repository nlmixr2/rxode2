rxTest({
  # sigdig sets the ODE solver tolerances with one simple, solver-independent
  # formula: rtol = 10^-sigdig, atol = 10^(-sigdig-3).  Sensitivity solves match
  # the main solve; steady-state solves run one order looser.  Same for every solver
  # (stiff, non-stiff, auto-switching).

  test_that("sigdig sets the same tolerances for every solver", {
    for (m in c("liblsoda", "lsoda", "dop853", "dop5", "ros4", "cvode", "bdf",
                "dop853+ros4")) {
      .c <- rxControl(sigdig = 4, method = m)
      expect_equal(.c$rtol, 1e-4, info = m)
      expect_equal(.c$atol, 1e-7, info = m)
      expect_equal(.c$rtolSens, 1e-4, info = m)
      expect_equal(.c$atolSens, 1e-7, info = m)
      expect_equal(.c$ssRtol[1], 1e-3, info = m)
      expect_equal(.c$ssAtol[1], 1e-6, info = m)
      expect_equal(.c$ssRtolSens[1], 1e-3, info = m)
      expect_equal(.c$ssAtolSens[1], 1e-6, info = m)
    }
  })

  test_that("sigdig scales the tolerances by one order per significant digit", {
    .c <- rxControl(sigdig = 6)
    expect_equal(.c$rtol, 1e-6)
    expect_equal(.c$atol, 1e-9)
    .d <- rxControl(sigdig = 3)
    expect_equal(.d$rtol, 1e-3)
    expect_equal(.d$atol, 1e-6)
  })

  test_that("an explicit atol/rtol overrides the sigdig-derived value", {
    .c <- rxControl(sigdig = 4, atol = 1e-12, rtol = 1e-10)
    expect_equal(.c$atol, 1e-12)
    expect_equal(.c$rtol, 1e-10)
  })

  test_that("each tolerance is resolved independently of the others", {
    # an explicit atol/rtol wins for the main solve but does not propagate to the
    # sensitivity or steady-state tolerances; those stay sigdig-derived
    .c <- rxControl(sigdig = 4, atol = 1e-12, rtol = 1e-10)
    expect_equal(.c$atolSens, 1e-7)
    expect_equal(.c$rtolSens, 1e-4)
    expect_equal(.c$ssAtol[1], 1e-6)
    expect_equal(.c$ssRtol[1], 1e-3)
    # and an explicit sensitivity tolerance wins over the sigdig-derived one
    .d <- rxControl(sigdig = 4, atolSens = 1e-12, rtolSens = 1e-10)
    expect_equal(.d$atolSens, 1e-12)
    expect_equal(.d$rtolSens, 1e-10)
    expect_equal(.d$atol, 1e-7)
    expect_equal(.d$rtol, 1e-4)
  })

  test_that("sigdig is unused unless it is explicitly requested", {
    # sigdig defaults to NULL and must not touch any tolerance
    .d <- rxControl()
    expect_null(.d$sigdig)
    expect_equal(.d$atol, 1e-8)
    expect_equal(.d$rtol, 1e-6)
    expect_equal(.d$atolSens, 1e-8)
    expect_equal(.d$rtolSens, 1e-6)
    expect_equal(.d$ssAtol[1], 1e-8)
    expect_equal(.d$ssRtol[1], 1e-6)
    expect_equal(.d$ssAtolSens[1], 1e-8)
    expect_equal(.d$ssRtolSens[1], 1e-6)
    # an explicit NULL is the same as not passing it
    expect_equal(rxControl(sigdig = NULL)$rtol, 1e-6)
  })
})
