rxTest({
  # delay() pre-history is the evaluated x(0), whether x(0) is a literal, a
  # parameter or a computed value (rxode2#1441).
  .ev <- et(seq(0, 3, by = 0.5))
  .lit <- rxode2({
    x(0) <- 2
    d / dt(x) <- -delay(x, 1)
    dx <- delay(x, 1)
  })
  .par <- rxode2({
    x(0) <- x0
    d / dt(x) <- -delay(x, 1)
    dx <- delay(x, 1)
  })
  .cmp <- rxode2({
    x0c <- x0 * 1
    x(0) <- x0c
    d / dt(x) <- -delay(x, 1)
    dx <- delay(x, 1)
  })

  for (.meth in c("dop853", "liblsoda", "ros4")) {
    test_that(paste0("parameter/computed x(0) drive the delay() history (#1441, ", .meth, ")"), {
      .a <- rxSolve(.lit, .ev, method = .meth)
      .b <- rxSolve(.par, .ev, params = c(x0 = 2), method = .meth)
      .c <- rxSolve(.cmp, .ev, params = c(x0 = 2), method = .meth)
      .w <- .a$time <= 1
      expect_equal(.a$x[.w], 2 - 2 * .a$time[.w], tolerance = 1e-6)
      expect_equal(.b$x, .a$x, tolerance = 1e-6)
      expect_equal(.c$x, .a$x, tolerance = 1e-6)
      # the output pass sees the same pre-history
      expect_equal(.b$dx[.w], rep(2, sum(.w)), tolerance = 1e-6)
      expect_equal(.c$dx[.w], rep(2, sum(.w)), tolerance = 1e-6)
    })
  }

  test_that("each subject gets its own x(0) history (#1441)", {
    .s <- rxSolve(.par, .ev, params = data.frame(x0 = c(2, 4)), method = "dop853")
    .w1 <- .s$sim.id == 1 & .s$time <= 1
    .w2 <- .s$sim.id == 2 & .s$time <= 1
    expect_equal(.s$x[.w1], 2 - 2 * .s$time[.w1], tolerance = 1e-6)
    expect_equal(.s$x[.w2], 4 - 4 * .s$time[.w2], tolerance = 1e-6)
  })

  test_that("a dose at t0 does not enter the x(0) history (#1441)", {
    .e <- et(amt = 1, time = 0, cmt = "x") |> et(seq(0, 1, by = 0.25))
    .a <- rxSolve(.lit, .e, method = "dop853")
    .b <- rxSolve(.par, .e, params = c(x0 = 2), method = "dop853")
    expect_equal(.a$x, 3 - 2 * .a$time, tolerance = 1e-6)
    expect_equal(.b$x, .a$x, tolerance = 1e-6)
  })

  test_that("states without past() keep the evaluated x(0) history (#1441)", {
    .m <- rxode2({
      x(0) <- x0
      y(0) <- y0
      d / dt(x) <- -delay(x, 1)
      d / dt(y) <- -delay(y, 1)
      past(y, 1) <- 0.5
    })
    .s <- rxSolve(.m, et(seq(0, 1, by = 0.25)), params = c(x0 = 2, y0 = 1))
    expect_equal(.s$x, 2 - 2 * .s$time, tolerance = 1e-6)
    expect_equal(.s$y, 1 - 0.5 * .s$time, tolerance = 1e-6)
  })

  test_that("x(0) parameter sensitivity includes the history (#1441)", {
    .e <- et(seq(0, 3, by = 0.25))
    .mstr <- "y(0) <- a\nd/dt(y) <- -k*delay(y, 1)\n"
    .s <- rxSolve(rxode2(.mstr, calcSens = c("k", "a")), .e, params = c(k = 0.3, a = 1), atol = 1e-11, rtol = 1e-11)
    .fwd <- function(k, a) {
      rxSolve(rxode2(.mstr), .e, params = c(k = k, a = a), atol = 1e-11, rtol = 1e-11)$y
    }
    .eps <- 1e-4
    expect_equal(.s$rx__sens_y_BY_a__, (.fwd(0.3, 1 + .eps) - .fwd(0.3, 1 - .eps)) / (2 * .eps), tolerance = 1e-4)
    expect_equal(.s$rx__sens_y_BY_k__, (.fwd(0.3 + .eps, 1) - .fwd(0.3 - .eps, 1)) / (2 * .eps), tolerance = 1e-4)
  })
})
