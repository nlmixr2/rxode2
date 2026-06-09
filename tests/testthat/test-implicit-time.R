rxTest({
  test_that("implicit solvers use stage-specific evaluation times", {
    mod <- rxode2::rxode2({
      d/dt(y) <- time
    })
    et <- rxode2::eventTable()
    et$add.sampling(seq(0, 1, by = 0.1))
    .expected <- 0.5 * seq(0, 1, by = 0.1)^2

    for (.method in c("backwardEuler", "gauss6", "iiic6", "radauiia5",
                      "geng5", "sdirk43", "ros6")) {
      out <- rxode2::rxSolve(mod, params = c(), events = et, inits = c(y = 0),
                             method = .method, hmin = 1e-3, hmax = 0.05)
      .tol <- if (.method == "ros6") 1e-2 else 5e-3
      expect_equal(out$y, .expected, tolerance = .tol,
                   info = paste("method", .method))
    }
  })
})
