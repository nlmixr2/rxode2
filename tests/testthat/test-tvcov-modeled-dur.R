rxTest({
  # nonmem2rx#263: interpolating a time-varying covariate looked up
  # neighbouring records with getTime(), which decoded their evid into the
  # subject's current compartment/infusion flags.  With modeled lag and
  # duration on two compartments that sent the second infusion into the
  # wrong compartment, driving `central` far below zero.
  .base <- "
    alag(depot) <- 0.4
    dur(depot) <- 0.4
    f(depot) <- 0.7
    alag(central) <- 22
    dur(central) <- 6
    f(central) <- 0.3
    d/dt(depot) = -0.8 * depot
    d/dt(central) = 0.8 * depot - 0.02 * central
  "
  .ev <- et(amt = 60, cmt = "depot", ii = 24, addl = 4, rate = -2) |>
    et(amt = 60, cmt = "central", ii = 24, addl = 4, rate = -2) |>
    et(seq(0, 168, by = 0.5))

  .ref <- rxSolve(rxode2(.base), .ev, returnType = "data.frame")

  test_that("a DV reference does not change modeled-duration doses", {
    .s <- rxSolve(rxode2(paste(.base, "res <- DV - central")), .ev,
                  returnType = "data.frame")
    expect_true(min(.s$central) >= 0)
    expect_equal(.s$central, .ref$central)
    expect_equal(.s$depot, .ref$depot)
  })

  test_that("a time-varying covariate does not change modeled-duration doses", {
    .d <- as.data.frame(.ev)
    .d$WT <- .d$time
    .s <- rxSolve(rxode2(paste(.base, "res <- WT - central")), .d,
                  returnType = "data.frame")
    expect_equal(.s$central, .ref$central)
    expect_equal(.s$depot, .ref$depot)
  })
})
