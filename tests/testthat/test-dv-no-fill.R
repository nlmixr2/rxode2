rxTest({
  # DV is a measurement, not a covariate: a record without one keeps NA
  test_that("DV is NA on dose records, not filled from a neighbouring record", {
    mod <- rxode2({
      d/dt(central) <- -0.1 * central
      e <- DV - central
      ep <- lag0(e, 1)
    })
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, 3, 4),
      evid = c(1, 0, 0, 1, 0),
      amt = c(100, 0, 0, 100, 0),
      cmt = 1,
      dv = c(NA, 2, 3, NA, 5)
    )
    s <- as.data.frame(rxSolve(mod, d, addDosing = TRUE))
    expect_equal(s$DV, c(NA, 2, 3, NA, 5))
    expect_true(all(is.na(s$e[s$evid != 0])))
    # the record after a dose sees the dose's missing residual as 0
    expect_equal(s$ep[c(2, 5)], c(0, 0))
    expect_equal(s$ep[3], s$e[2])
  })

  test_that("a covariate is still filled on a dose record", {
    mod <- rxode2({
      d/dt(central) <- -0.1 * central
      w <- wt
    })
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2),
      evid = c(1, 0, 0),
      amt = c(100, 0, 0),
      cmt = 1,
      dv = c(NA, 2, 3),
      wt = c(NA, 20, 30)
    )
    s <- as.data.frame(rxSolve(mod, d, addDosing = TRUE, covsInterpolation = "locf"))
    expect_false(anyNA(s$w))
  })
})
