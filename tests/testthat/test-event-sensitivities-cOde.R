rxTest({
  # Event ("jump") sensitivity tests ported from the cOde package
  # (dkaschek/cOde: tests/testthat/test-saltation-root-events.R and
  # test-sensitivitiesSymb.R, 2026).  cOde fixed its event sensitivities for
  # right-hand sides that are nonlinear in the states: the event-time jump is
  # the saltation f(x-) - f(x+), not its linearization J*(x- - x+).  rxode2 has
  # no root-triggered events, so the event time here is a parameter-dependent
  # alag() instead of a root.

  # dx/dt = -k*x^2*t with an event at tau = alag that adds v, replaces by v or
  # multiplies by v (v enters as the bioavailability of a unit amount).  x(t)
  # has a closed form, so every 1st- and 2nd-order sensitivity is checked
  # against its symbolic derivative -- no finite differences.
  .closedForm <- function(method) {
    .xm <- quote(2 * x0 / (k * tau^2 * x0 + 2))
    .post <- switch(method, add = bquote(.(.xm) + v), replace = quote(v), multiply = bquote(.(.xm) * v))
    list(
      pre = quote(2 * x0 / (k * t^2 * x0 + 2)),
      post = bquote(1 / (k * (t^2 - tau^2) / 2 + 1 / .(.post)))
    )
  }

  .saltPars <- c("x0", "k", "v", "tau")
  .saltMod <- function() {
    rxode2("
      x(0) <- x0
      alag(x) <- tau
      f(x) <- v
      d/dt(x) <- -k * x^2 * t
    ", calcSens = .saltPars, calcSens2 = .saltPars, eventSens = "jump")
  }

  test_that("jump sensitivities match the closed form through a nonlinear event (1st and 2nd order)", {
    m <- .saltMod()
    p <- c(x0 = 1, k = 1, v = 1.5, tau = 1.3)
    for (.method in c("add", "replace", "multiply")) {
      .evid <- c(add = 1L, replace = 5L, multiply = 6L)[[.method]]
      e <- et(amt = 1, cmt = "x", evid = .evid) |> et(seq(0.05, 4, by = 0.1))
      s <- rxSolve(m, e, p, atol = 1e-12, rtol = 1e-12)
      .cf <- .closedForm(.method)
      .env <- c(as.list(p), list(t = s$time))
      .pw <- function(a, b) {
        .v <- ifelse(s$time < p[["tau"]], eval(a, .env), eval(b, .env))
        rep_len(.v, length(s$time))
      }
      expect_equal(s$x, .pw(.cf$pre, .cf$post), tolerance = 1e-8, label = paste(.method, "x"))
      for (.a in .saltPars) {
        expect_equal(
          s[[paste0("rx__sens_x_BY_", .a, "__")]],
          .pw(D(.cf$pre, .a), D(.cf$post, .a)),
          tolerance = 1e-8,
          label = paste(.method, "dx/d", .a)
        )
        for (.b in .saltPars) {
          expect_equal(
            s[[paste0("rx__sens_x_BY_", .a, "_BY_", .b, "__")]],
            .pw(D(D(.cf$pre, .a), .b), D(D(.cf$post, .a), .b)),
            tolerance = 1e-7,
            label = paste(.method, "d2x/d", .a, "d", .b)
          )
        }
      }
    }
  })

  test_that("additive-bolus lag jump is the exact saltation for Michaelis-Menten elimination", {
    # f is nonlinear in the dosed state: the linearized jump -J(x-)*delta is
    # off by (km + dose)/km here.
    ode <- "
      alag(central) <- exp(tlag)
      vm <- exp(tvm)
      km <- exp(tkm)
      d/dt(central) = -vm*central/(km+central)
    "
    pars <- c(tvm = log(2), tkm = log(5), tlag = log(0.5))
    e <- et(amt = 10, cmt = "central") |> et(seq(0.55, 8, by = 0.5))
    mj <- rxode2(ode, calcSens = c("tlag", "tvm"), eventSens = "jump")
    mf <- rxode2(ode, eventSens = "fd")
    sj <- rxSolve(mj, e, pars, atol = 1e-12, rtol = 1e-12)
    .fd <- function(a, h = 1e-6) {
      pp <- pars
      pp[a] <- pp[a] + h
      pm <- pars
      pm[a] <- pm[a] - h
      (rxSolve(mf, e, pp, atol = 1e-12, rtol = 1e-12)$central -
        rxSolve(mf, e, pm, atol = 1e-12, rtol = 1e-12)$central) /
        (2 * h)
    }
    expect_equal(sj$rx__sens_central_BY_tlag__, .fd("tlag"), tolerance = 1e-6)
    expect_equal(sj$rx__sens_central_BY_tvm__, .fd("tvm"), tolerance = 1e-6)
  })

  test_that("a state-dependent alag() falls back to finite differences", {
    # The lag is evaluated at the dose's own time, so d(tau)/dp needs the
    # sensitivities there, not at the arrival time the jump runtime sees.
    m <- rxode2("
      d/dt(y) <- -ky * y
      alag(x) <- tau * (1 + y)
      d/dt(x) <- -k * x
    ", calcSens = c("ky", "k", "tau"), eventSens = "jump")
    expect_equal(m$eventSens, "fd")
    m2 <- rxode2("
      d/dt(y) <- -ky * y
      alag(x) <- tau
      d/dt(x) <- -k * x
    ", calcSens = c("ky", "k", "tau"), eventSens = "jump")
    expect_equal(m2$eventSens, "jump")
  })
})
