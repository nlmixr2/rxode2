rxTest({
  # past(state, tau) <- expr : user-specified non-constant delay() pre-history.
  # delay(state, tau) returns expr evaluated at t-tau for t<t0 instead of the
  # constant initial condition.

  test_that("past() parses and round-trips through rxNorm", {
    .m <- rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- exp(0.5*t)\n")
    .n <- rxNorm(.m)
    expect_true(grepl("past(y,1)=exp(0.5 * t)", .n, fixed = TRUE) ||
                grepl("past(y,1)=exp(0.5*t)", .n, fixed = TRUE))
  })

  test_that("constant past() value drives the pre-history (not the IC)", {
    .ev <- et(seq(0, 1, by = 0.25))
    ## for t<1, delay(y,1) = past value; y' = -past
    .p2 <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- 2\n"), .ev)
    .p0 <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- 0.5\n"), .ev)
    ## past=2 -> y decreases by 2 per unit time; past=0.5 -> by 0.5
    expect_equal(.p2$y, 1 - 2 * .p2$time, tolerance = 1e-6)
    expect_equal(.p0$y, 1 - 0.5 * .p0$time, tolerance = 1e-6)
  })

  test_that("non-constant past() matches the analytic solution", {
    ## y'(t) = -delay(y,1), history h(t)=exp(0.5 t); for t<=1 the delayed value
    ## is exp(0.5(t-1)), so y(t) = 1 - 2*(exp(0.5(t-1)) - exp(-0.5)).
    .ev <- et(seq(0, 1, by = 0.05))
    .s <- rxSolve(rxode2("y(0) <- 1\nd/dt(y) <- -delay(y, 1)\npast(y, 1) <- exp(0.5*t)\n"),
                  .ev, atol = 1e-10, rtol = 1e-10)
    .ana <- 1 - 2 * (exp(0.5 * (.ev$get.EventTable()$time - 1)) - exp(-0.5))
    expect_equal(.s$y, .ana, tolerance = 1e-6)
  })

  test_that("past() first-order sensitivities match finite differences", {
    .ev <- et(seq(0, 3, by = 0.25))
    .mstr <- function(k, a, b) sprintf(
      "k=%.12g\na=%.12g\nb=%.12g\ny(0) <- a\nd/dt(y) <- -k*delay(y, 1)\npast(y, 1) <- a*exp(b*t)\n",
      k, a, b)
    .s <- rxSolve(rxode2(.mstr(0.3, 1, 0.5), calcSens = c("k", "a", "b")),
                  .ev, atol = 1e-11, rtol = 1e-11)
    .fwd <- function(k, a, b) rxSolve(rxode2(.mstr(k, a, b)), .ev,
                                      atol = 1e-11, rtol = 1e-11)$y
    .eps <- 1e-4
    .base <- c(k = 0.3, a = 1, b = 0.5)
    for (.p in c("k", "a", "b")) {
      .hi <- .base; .hi[.p] <- .hi[.p] + .eps
      .lo <- .base; .lo[.p] <- .lo[.p] - .eps
      .fd <- (.fwd(.hi[1], .hi[2], .hi[3]) - .fwd(.lo[1], .lo[2], .lo[3])) / (2 * .eps)
      expect_equal(.s[[paste0("rx__sens_y_BY_", .p, "__")]], .fd, tolerance = 1e-4)
    }
  })

  test_that("past() second-order sensitivities match finite differences", {
    .ev <- et(seq(0, 3, by = 0.5))
    .mstr <- function(k, a, b) sprintf(
      "k=%.12g\na=%.12g\nb=%.12g\ny(0) <- a\nd/dt(y) <- -k*delay(y, 1)\npast(y, 1) <- a*exp(b*t)\n",
      k, a, b)
    .pars <- c("k", "a", "b")
    .s <- rxSolve(rxode2(.mstr(0.3, 1, 0.5), calcSens = .pars, calcSens2 = .pars),
                  .ev, atol = 1e-12, rtol = 1e-12)
    .fwd <- function(m) rxSolve(rxode2(.mstr(m[1], m[2], m[3])), .ev,
                                atol = 1e-12, rtol = 1e-12)$y
    .base <- c(k = 0.3, a = 1, b = 0.5); names(.base) <- .pars
    .h <- 1e-3
    .same <- function(p) {
      .hi <- .base; .hi[p] <- .hi[p] + .h; .lo <- .base; .lo[p] <- .lo[p] - .h
      (.fwd(.hi) - 2 * .fwd(.base) + .fwd(.lo)) / (.h * .h)
    }
    .cross <- function(p, q) {
      .pp <- .base; .pp[p] <- .pp[p] + .h; .pp[q] <- .pp[q] + .h
      .pm <- .base; .pm[p] <- .pm[p] + .h; .pm[q] <- .pm[q] - .h
      .mp <- .base; .mp[p] <- .mp[p] - .h; .mp[q] <- .mp[q] + .h
      .mm <- .base; .mm[p] <- .mm[p] - .h; .mm[q] <- .mm[q] - .h
      (.fwd(.pp) - .fwd(.pm) - .fwd(.mp) + .fwd(.mm)) / (4 * .h * .h)
    }
    for (.i in seq_along(.pars)) for (.j in .i:length(.pars)) {
      .p <- .pars[.i]; .q <- .pars[.j]
      .col <- paste0("rx__sens_y_BY_", .p, "_BY_", .q, "__")
      if (is.null(.s[[.col]])) .col <- paste0("rx__sens_y_BY_", .q, "_BY_", .p, "__")
      .fd <- if (.p == .q) .same(.p) else .cross(.p, .q)
      expect_equal(.s[[.col]], .fd, tolerance = 1e-3)
    }
  })
})
