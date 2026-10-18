rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # the continuous adjoint converges at O(denseBy^2)
  .db <- 2e-4
  .tol <- list(denseBy = .db, atol = 1e-12, rtol = 1e-12)
  .txt <- "y(0) <- y0\nd/dt(y) <- -k*y"
  .p <- c(k = 0.7, y0 = 3)

  # least-squares gradient sum((y - obs) * dy/dp) for one observation set
  .lsGrad <- function(y, dy, obs) colSums((y - obs) * dy)

  test_that("a replace or multiply at t0 weights the initial-condition term", {
    # an observation reads the post-event state; replace 4 at t0 makes y
    # independent of y0, multiply by 2 doubles its y0 dependence
    .cases <- list(
      replace = list(ev = et(time = 0, evid = 5, amt = 4, cmt = "y"), y = function(t) 4 * exp(-0.7 * t),
        dy0 = function(t) 0 * t, dk = function(t) -t * 4 * exp(-0.7 * t)),
      multiply = list(ev = et(time = 0, evid = 6, amt = 2, cmt = "y"), y = function(t) 6 * exp(-0.7 * t),
        dy0 = function(t) 2 * exp(-0.7 * t), dk = function(t) -t * 6 * exp(-0.7 * t))
    )
    for (.nm in names(.cases)) {
      .k <- .cases[[.nm]]
      for (.tt in list(c(0, 0.5, 1, 2), c(0.5, 1, 2))) {
        .ev <- .k$ev |> et(.tt)
        .a <- do.call(rxSolveAdjoint, c(list(.txt, .p, .ev, c("k", "y0")), .tol))
        .a <- .a[.a$time %in% .tt, , drop = FALSE]
        expect_equal(.a$rx__sens_y_BY_y0__, .k$dy0(.tt), tolerance = 1e-6, label = paste(.nm, "trajectory y0"))
        expect_equal(.a$rx__sens_y_BY_k__, .k$dk(.tt), tolerance = 1e-6, label = paste(.nm, "trajectory k"))
        .obs <- rep(1, length(.tt))
        .gKnown <- .lsGrad(.k$y(.tt), cbind(k = .k$dk(.tt), y0 = .k$dy0(.tt)), .obs)
        .b <- .rxAdjointGradBuild(.txt, c("k", "y0"), "y", .ev)
        .gC <- do.call(.rxAdjointGradEvalC, c(list(.b, .p, .tt, .obs), .tol))
        expect_lt(max(abs(.gC - .gKnown)), 1e-6, label = paste(.nm, "gradient C", length(.tt)))
        # approx() collapses the forward checkpoint's duplicated event-time row
        .gR <- suppressWarnings(do.call(.rxAdjointGradEval, c(list(.b, .p, .tt, .obs), .tol)))
        expect_lt(max(abs(.gR - .gKnown)), 1e-6, label = paste(.nm, "gradient R", length(.tt)))
      }
      expect_error(
        .rxAdjointSolve(.txt, .p, .k$ev |> et(c(0.5, 1)), c("k", "y0"), c(0.5, 1)),
        "the reference continuous adjoint (.rxAdjointSolve) does not apply replace/multiply events (evid = 5/6); use rxSolveAdjoint()",
        fixed = TRUE
      )
    }
  })

  test_that("an observation at a replace or multiply reads the post-event state in the gradient", {
    # dose 10 at t0, then replace by 4 / multiply by 0.98 at t = 1, observed at 1
    .txt2 <- "d/dt(y) <- -k*y"
    .tt <- c(0.5, 1, 2, 3)
    .cases <- list(
      replace = list(ev = et(amt = 10, cmt = "y") |> et(time = 1, evid = 5, amt = 4, cmt = "y") |> et(.tt),
        y = ifelse(.tt < 1, 10 * exp(-0.7 * .tt), 4 * exp(-0.7 * (.tt - 1))),
        dk = ifelse(.tt < 1, -.tt * 10 * exp(-0.7 * .tt), -(.tt - 1) * 4 * exp(-0.7 * (.tt - 1)))),
      multiply = list(ev = et(amt = 10, cmt = "y") |> et(time = 1, evid = 6, amt = 0.98, cmt = "y") |> et(.tt),
        y = ifelse(.tt < 1, 10, 9.8) * exp(-0.7 * .tt),
        dk = -.tt * ifelse(.tt < 1, 10, 9.8) * exp(-0.7 * .tt))
    )
    for (.nm in names(.cases)) {
      .k <- .cases[[.nm]]
      .obs <- c(5, 3, 1, 0.5)
      .gKnown <- sum((.k$y - .obs) * .k$dk)
      .b <- .rxAdjointGradBuild(.txt2, "k", "y", .k$ev)
      .gC <- do.call(.rxAdjointGradEvalC, c(list(.b, c(k = 0.7), .tt, .obs), .tol))
      # a gradient of about 26; the event-time kink converges at O(denseBy)
      expect_equal(.gC[["k"]], .gKnown, tolerance = 2e-6, label = paste(.nm, "gradient C"))
      .gR <- suppressWarnings(do.call(.rxAdjointGradEval, c(list(.b, c(k = 0.7), .tt, .obs), .tol)))
      expect_equal(.gR[["k"]], .gKnown, tolerance = 2e-6, label = paste(.nm, "gradient R"))
    }
  })

  test_that("the continuous adjoint refuses a reset", {
    .msg <- "the continuous adjoint does not support a reset (evid = 3/4); use rxSolveAdjointRk4()"
    for (.ev in list(et(c(0, 1, 3)) |> et(time = 2, evid = 3), et(c(0, 1, 3)) |> et(time = 2, evid = 4, amt = 5, cmt = "y"))) {
      expect_error(.rxAdjointSolve(.txt, .p, .ev, "k", c(1, 3)), .msg, fixed = TRUE)
      expect_error(rxSolveAdjoint(.txt, .p, .ev, "k"), .msg, fixed = TRUE)
      expect_error(.rxAdjointGradBuild(.txt, "k", "y", .ev), .msg, fixed = TRUE)
    }
  })

  test_that("model defaults left unsupplied are filled in at every entry point", {
    # b0 has a default and only enters through the initial condition
    .txt3 <- "b0 <- 1.5\nx(0) <- a*b0\nd/dt(x) <- -k*x"
    .p3 <- c(a = 2, k = 0.7)
    .tt <- c(0.5, 1, 2)
    .ev <- et(.tt)
    .x <- 2 * 1.5 * exp(-0.7 * .tt)
    .da <- 1.5 * exp(-0.7 * .tt)
    .dk <- -.tt * .x
    .s <- suppressWarnings(do.call(.rxAdjointSolve, c(list(.txt3, .p3, .ev, c("a", "k"), .tt), .tol)))
    expect_equal(.s$rx__sens_x_BY_a__, .da, tolerance = 1e-6)
    expect_equal(.s$rx__sens_x_BY_k__, .dk, tolerance = 1e-6)
    .a <- do.call(rxSolveAdjoint, c(list(.txt3, .p3, .ev, c("a", "k")), .tol))
    .a <- .a[.a$time %in% .tt, , drop = FALSE]
    expect_equal(.a$rx__sens_x_BY_a__, .da, tolerance = 1e-6)
    expect_equal(.a$rx__sens_x_BY_k__, .dk, tolerance = 1e-6)
    .obs <- c(2, 1, 0.5)
    .gKnown <- .lsGrad(.x, cbind(a = .da, k = .dk), .obs)
    .b <- .rxAdjointGradBuild(.txt3, c("a", "k"), "x", .ev)
    expect_lt(max(abs(do.call(.rxAdjointGradEvalC, c(list(.b, .p3, .tt, .obs), .tol)) - .gKnown)), 1e-6)
    .gR <- suppressWarnings(do.call(.rxAdjointGradEval, c(list(.b, .p3, .tt, .obs), .tol)))
    expect_lt(max(abs(.gR - .gKnown)), 1e-6)
  })

  test_that("the discrete build fills unsupplied defaults on every call", {
    .B <- .rxDiscreteAdjointBuild("k <- 0.5\nd/dt(y) <- -k*y", "k")
    expect_equal(unname(.B$Fe(1, numeric(0))), -0.5)
    expect_equal(unname(.B$Fe(1, c(k = 1))), -1)
    # a later call without k is back at the default, not the previous value
    expect_equal(unname(.B$Fe(1, numeric(0))), -0.5)
    # RK4 sensitivity at the default against the closed form y = 2 exp(-k t)
    .f <- .rxDiscreteForwardSens(.B, c(y = 2), numeric(0), 0.005, 400L)
    expect_equal(.f$XN[[1]], 2 * exp(-0.5 * 2), tolerance = 1e-9)
    expect_equal(.f$SN[1, 1], -2 * 2 * exp(-0.5 * 2), tolerance = 1e-9)
  })
})
