rxTest({
  # With ssAtol/ssRtol disabled the steady state stops at minSS, so it must be
  # a whole number of doses for every solver (rxode2#1492)
  test_that("minSS with disabled tolerances gives a whole number of bolus doses", {
    .k <- 0.01
    .ii <- 12
    .r <- exp(-.k * .ii)
    .m <- rxode2({
      d / dt(c) <- -k * c
    })
    .e <- et(amt = 100, ss = 1, ii = .ii) |> et(0.5)
    # amount at t=0.5 after n doses every ii, the last at t=0
    .nDose <- function(n) 100 * (1 - .r^n) / (1 - .r) * exp(-.k * 0.5)
    for (.meth in c("liblsoda", "lsoda", "cvode", "rk4")) {
      for (.n in c(5, 7, 10)) {
        .s <- rxSolve(
          .m,
          .e,
          params = c(k = .k),
          minSS = .n,
          maxSS = .n + 2,
          ssAtol = 100,
          ssRtol = 100,
          method = .meth,
          returnType = "data.frame"
        )
        expect_equal(.s$c[.s$time == 0.5], .nDose(.n + 3), tolerance = 1e-5)
      }
    }
  })
})
