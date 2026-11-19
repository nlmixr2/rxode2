rxTest({
  # the `output` sink an indLin conversion adds never reaches a steady state,
  # so it is left out of the ssAtol/ssRtol test (rxode2#1749)
  .ssSinkMod <- rxode2({
    V <- 20
    Cl <- 1
    C2 <- center / V
    d / dt(center) ~ -Cl * C2
  })

  test_that("an indLin steady state stops when the model states converge", {
    for (.dur in c(0, 1)) {
      .ev <- et(amt = 3, ss = 1, ii = 12, dur = .dur) |>
        et(seq(0, 24, length.out = 5))
      .s1 <- rxSolve(.ssSinkMod, .ev, method = "indLin", maxSS = 10000)
      .s2 <- rxSolve(.ssSinkMod, .ev, method = "indLin", maxSS = 20000)
      # converged before maxSS, so maxSS does not change the solve
      expect_equal(.s1$output, .s2$output)
      expect_true(all(.s1$output < 1000))
      .ref <- rxSolve(.ssSinkMod, .ev, method = "liblsoda")
      expect_equal(.s1$C2, .ref$C2, tolerance = 1e-5)
    }
  })

  test_that(".rxIndLinSsAtol only changes the invented output sink", {
    expect_equal(.rxIndLinSsAtol(1e-8, "center", "center"), 1e-8)
    expect_equal(
      .rxIndLinSsAtol(1e-8, "center", c("center", "output")),
      c(1e-8, .Machine$double.xmax)
    )
    expect_equal(
      .rxIndLinSsAtol(c(1e-8, 1e-6), c("a", "b"), c("a", "output", "b")),
      c(1e-8, .Machine$double.xmax, 1e-6)
    )
    # a model state named output is kept
    expect_equal(
      .rxIndLinSsAtol(1e-8, c("center", "output"), c("center", "output")),
      1e-8
    )
  })
})
