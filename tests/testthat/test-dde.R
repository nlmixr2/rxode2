rxTest({
  # Delay differential equations: delay(state, T) gives the value of `state`
  # at the past time t - T (Monolix semantics).

  ## Classic linear DDE  y'(t) = -delay(y, 1),  history y(t) = 1 for t <= 0.
  ## Method of steps gives the exact piecewise solution:
  ##   [0,1]: y = 1 - t
  ##   [1,2]: y = t^2/2 - 2t + 3/2
  .exact <- function(t) ifelse(t <= 1, 1 - t, t^2 / 2 - 2 * t + 3 / 2)

  .dde <- rxode2({
    y(0) <- 1
    d/dt(y) <- -delay(y, 1)
  })

  test_that("delay() parses and round-trips in the normalized model", {
    expect_true(grepl("delay(y,1)", rxNorm(.dde), fixed = TRUE))
    expect_equal(unname(rxModelVars(.dde)$flags[["hasDelay"]]), 1L)
  })

  test_that("delay differential equation matches the analytic solution", {
    .tt <- seq(0, 2, by = 0.1)
    .s <- rxSolve(.dde, et(.tt), atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s$y - .exact(.s$time))) < 1e-6)
  })

  test_that("delay models default to the dense dop853+ros4 composite", {
    .tt <- seq(0, 2, by = 0.5)
    ## no method specified -> auto dense dop853+ros4
    .s <- rxSolve(.dde, et(.tt), atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s$y - .exact(.s$time))) < 1e-6)
    ## explicit dop853 also works (dense enabled automatically)
    .s2 <- rxSolve(.dde, et(.tt), method = "dop853", atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s2$y - .exact(.s2$time))) < 1e-6)
    ## explicit composite works
    .s3 <- rxSolve(.dde, et(.tt), method = "dop853+ros4", atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s3$y - .exact(.s3$time))) < 1e-6)
  })

  test_that("small delays (below the natural step size) stay accurate", {
    ## y'(t) = -delay(y, 0.5), history 1.  Method of steps:
    ##   [0,0.5]:   y = 1 - t
    ##   [0.5,1]:   y = t^2/2 - 1.5 t + 1.125
    .exact05 <- function(t) {
      ifelse(t <= 0.5, 1 - t, t^2 / 2 - 1.5 * t + 1.125)
    }
    .dde05 <- rxode2({
      y(0) <- 1
      d/dt(y) <- -delay(y, 0.5)
    })
    .tt <- seq(0, 1, by = 0.1)
    ## an intentionally huge hmax must be overridden by the delay step cap
    .s <- rxSolve(.dde05, et(.tt), atol = 1e-10, rtol = 1e-10, hmax = 100)
    expect_true(max(abs(.s$y - .exact05(.tt))) < 1e-6)
  })

  test_that("delay models reject solvers that cannot record dense history", {
    .tt <- seq(0, 2, by = 0.5)
    for (.m in c("lsoda", "liblsoda", "dop5", "bs", "ros4")) {
      if (.m == "liblsoda") next # the default token is silently switched
      expect_error(rxSolve(.dde, et(.tt), method = .m), "dop853")
    }
  })

  test_that("delay() works with an expression for the delay duration", {
    .m <- rxode2({
      tau <- 0.5
      y(0) <- 2
      d/dt(y) <- -delay(y, tau * 2)
    })
    ## delay duration is 1; on [0,1] history is constant 2 so y' = -2, y = 2 - 2t
    .s <- rxSolve(.m, et(seq(0, 1, by = 0.25)), atol = 1e-10, rtol = 1e-10)
    expect_equal(.s$y, 2 - 2 * .s$time, tolerance = 1e-6)
  })

  test_that("multi-subject DDE solves keep per-subject history isolated", {
    .tt <- seq(0, 2, by = 0.25)
    .ev <- et(.tt)
    .ev$id <- 1
    .e3 <- do.call(rbind, lapply(1:3, function(i) {
      d <- .ev
      d$id <- i
      d
    }))
    .s3 <- rxSolve(.dde, .e3, atol = 1e-10, rtol = 1e-10)
    expect_true(max(abs(.s3$y - .exact(.s3$time))) < 1e-6)
  })

  test_that("a delayed-logistic DDE solves without error", {
    ## Hutchinson's equation: N'(t) = r N (1 - N(t-tau)/K)
    .log <- rxode2({
      r <- 0.5
      K <- 10
      tau <- 1
      N(0) <- 2
      d/dt(N) <- r * N * (1 - delay(N, tau) / K)
    })
    .s <- rxSolve(.log, et(seq(0, 20, by = 0.5)))
    expect_false(any(is.na(.s$N)))
    expect_true(all(.s$N > 0))
  })
})
