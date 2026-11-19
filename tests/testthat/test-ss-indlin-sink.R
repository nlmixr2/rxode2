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

  test_that("re-solving an indLin solve keeps the sink out of the test", {
    .ev <- et(amt = 3, ss = 1, ii = 12) |> et(c(0, 6))
    .s <- rxSolve(.ssSinkMod, .ev, method = "indLin")
    .s2 <- rxSolve(.s, .ev)
    expect_equal(.s2$output, .s$output)
    expect_true(all(.s2$output < 1000))
    .o <- .s$output
    rxSolve(.s, .ev, updateObject = TRUE)
    expect_equal(.s$output, .o)
  })

  test_that("a model converted with rxToIndLin() leaves the sink out too", {
    .me <- rxode2(rxToIndLin(
      "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - cl/v*central\n"
    ))
    .ev <- et(amt = 100, cmt = "depot", ii = 12, ss = 1) |> et(c(0, 6))
    .p <- c(ka = 1, cl = 1, v = 10)
    .s1 <- rxSolve(.me, .p, .ev, method = "indLin", maxSS = 10000)
    .s2 <- rxSolve(.me, .p, .ev, method = "indLin", maxSS = 20000)
    expect_equal(.s1$output, .s2$output)
    expect_equal(.rxIndLinSinkState(rxModelVars(.me)), "output")
    expect_equal(.rxIndLinSinkState(rxModelVars(.ssSinkMod)), character(0))
  })

  test_that(".rxIndLinSsAtol only changes the output sink", {
    expect_equal(.rxIndLinSsAtol(1e-8, "center", character(0)), 1e-8)
    expect_equal(
      .rxIndLinSsAtol(1e-8, c("center", "output"), "output"),
      c(1e-8, .Machine$double.xmax)
    )
    # positional, as the solver reads it
    expect_equal(
      .rxIndLinSsAtol(c(1e-8, 1e-7, 1e-5), c("a", "output", "b"), "output"),
      c(1e-8, .Machine$double.xmax, 1e-5)
    )
    .x <- .rxIndLinSsAtol(c(1e-8, 1e-6), c("a", "b", "output"), "output")
    expect_equal(.x, c(1e-8, 1e-6, .Machine$double.xmax))
    expect_equal(.rxIndLinSsAtol(.x, c("a", "b", "output"), "output"), .x)
  })

  test_that("an output compartment with a rate out of it is not a sink", {
    .m <- rxode2("matExp()\nk_center_output ~ k12\nk_output_center ~ k21\n")
    expect_equal(.rxIndLinSinkState(rxModelVars(.m)), character(0))
    .m2 <- rxode2("matExp()\nk_center_output = k12\nk_output_center = k21\n")
    expect_equal(.rxIndLinSinkState(rxModelVars(.m2)), character(0))
  })

  test_that("$solve() on a compiled rxToIndLin() model leaves the sink out", {
    .me <- rxode2(rxToIndLin(
      "d/dt(depot) = -ka*depot\nd/dt(central) = ka*depot - cl/v*central\n"
    ))
    .ev <- et(amt = 100, cmt = "depot", ii = 12, ss = 1) |> et(c(0, 6))
    .p <- c(ka = 1, cl = 1, v = 10)
    expect_equal(
      as.data.frame(.me$solve(.p, .ev, method = "indLin"))$output,
      rxSolve(.me, .p, .ev, method = "indLin")$output
    )
    expect_true(all(rxSolve(.me, .p, .ev, method = "indLin")$output < 1e4))
  })
})
