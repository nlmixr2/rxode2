rxTest({
  # With ssAtol/ssRtol disabled the steady state stops at minSS, which is the
  # number of doses before the steady-state dose (rxode2#1492)
  .ssMinSSMod <- rxode2({
    d / dt(depot) <- -ka * depot
    d / dt(c) <- ka * depot - k * c
    alag(depot) <- lag
  })
  .ii <- 12
  .tobs <- c(0.5, 3, 7)
  # steady state stopped at minSS
  .ssMinSS <- function(minSS, extra = list(), ss = 1, lag = 0, cmt = "depot", method = "liblsoda") {
    .e <- do.call(et, c(list(amt = 100, ss = ss, ii = .ii, cmt = cmt), extra))
    .s <- rxSolve(
      .ssMinSSMod,
      et(.e, .tobs),
      params = c(k = 0.05, ka = 0.7, lag = lag),
      minSS = minSS,
      maxSS = minSS + 2,
      ssAtol = 1e8,
      ssRtol = 1e8,
      method = method,
      returnType = "data.frame"
    )
    .s$c
  }
  # the same after n explicit doses before the observed one
  .nDose <- function(n, extra = list(), lag = 0, cmt = "depot") {
    .e <- do.call(et, c(list(amt = 100, ii = .ii, addl = n, cmt = cmt), extra))
    .s <- rxSolve(
      .ssMinSSMod,
      et(.e, .tobs + n * .ii),
      params = c(k = 0.05, ka = 0.7, lag = lag),
      atol = 1e-10,
      rtol = 1e-10,
      returnType = "data.frame"
    )
    .s$c
  }

  test_that("minSS with disabled tolerances is the number of prior bolus doses", {
    for (.meth in c("liblsoda", "lsoda", "cvode", "rk4")) {
      for (.n in c(5, 8)) {
        expect_equal(
          .ssMinSS(.n, method = .meth),
          .nDose(.n),
          tolerance = 1e-5
        )
      }
    }
    for (.n in c(5, 8)) {
      expect_equal(.ssMinSS(.n, cmt = "c"), .nDose(.n, cmt = "c"), tolerance = 1e-5)
      expect_equal(.ssMinSS(.n, ss = 2), .nDose(.n), tolerance = 1e-5)
      expect_equal(.ssMinSS(.n, lag = 2), .nDose(.n, lag = 2), tolerance = 1e-5)
    }
  })

  test_that("minSS with disabled tolerances is the number of prior infusions", {
    # rate, dur < ii, and dur > ii with one and two overlapping infusions
    for (.extra in list(list(rate = 50), list(dur = 3), list(dur = 18), list(dur = 30))) {
      for (.lag in c(0, 2, 14)) {
        for (.n in c(5, 8)) {
          expect_equal(
            .ssMinSS(.n, extra = .extra, lag = .lag),
            .nDose(.n, extra = .extra, lag = .lag),
            tolerance = 1e-5
          )
        }
      }
      expect_equal(
        .ssMinSS(5, extra = .extra, cmt = "c"),
        .nDose(5, extra = .extra, cmt = "c"),
        tolerance = 1e-5
      )
    }
  })
})
