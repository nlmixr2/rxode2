rxTest({
  # Across a dose or an infusion edge dop5 starts the next segment with the
  # step it last proposed, capped by 1/100 of the time scale of the state just
  # after the event; dop853 and ros4 restart there.  All three must keep their
  # accuracy across every kind of event.

  # exact solution of dx/dt = A x + b with piecewise-constant input: events
  # add amount `add` to state `cmt`, or change its input rate by `rate`
  .kdExact <- function(A, events, obs) {
    .e <- eigen(A)
    .V <- .e$vectors
    .Vi <- solve(.V)
    .l <- .e$values
    .n <- nrow(A)
    .x <- rep(0, .n)
    .b <- rep(0, .n)
    .t <- 0
    .out <- matrix(NA_real_, length(obs), .n)
    for (.tt in sort(unique(c(events$time, obs)))) {
      if (.tt > .t) {
        .dt <- .tt - .t
        .E <- Re(.V %*% diag(exp(.l * .dt), .n) %*% .Vi)
        .G <- Re(.V %*% diag((exp(.l * .dt) - 1) / .l, .n) %*% .Vi)
        .x <- drop(.E %*% .x + .G %*% .b)
        .t <- .tt
      }
      for (.j in which(events$time == .tt)) {
        .x[events$cmt[.j]] <- .x[events$cmt[.j]] + events$add[.j]
        .b[events$cmt[.j]] <- .b[events$cmt[.j]] + events$rate[.j]
      }
      .w <- which(obs == .tt)
      if (length(.w)) .out[.w, ] <- matrix(.x, length(.w), .n, byrow = TRUE)
    }
    .out
  }

  .kdOral <- rxode2({
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - k * center
  })
  .kdInf <- rxode2({
    d/dt(central) <- -(cl / v + q / v) * central + q / vp * periph
    d/dt(periph) <- q / v * central - q / vp * periph
  })
  .kdMethods <- c("dop853", "dop5", "ros4")

  .kdSolve <- function(model, ev, params, method, tol = 1e-10, ...) {
    .s <- rxSolve(model, ev, params = params, method = method, atol = tol, rtol = tol,
                  returnType = "rxSolve", ...)
    list(d = as.data.frame(.s), dadt = .s$counts$dadt)
  }

  test_that("dop853, dop5 and ros4 match the exact solution across doses and infusion edges", {
    .dose <- (0:9) * 12
    .obs <- sort(c(.dose + 6, 48.02, 48.05, 48.1, 48.5))
    # oral doses with fast absorption (ka = 20), observed just after the dose
    # at 48
    .ka <- 20
    .evO <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "depot", rate = 0),
      data.frame(id = 1L, time = .obs, evid = 0L, amt = 0, cmt = "center", rate = 0)
    )
    .exO <- .kdExact(matrix(c(-.ka, .ka, 0, -0.2), 2, 2),
                     data.frame(time = .dose, cmt = 1L, add = 100, rate = 0), .obs)
    # one-hour infusions into a two-compartment model
    .evI <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "central", rate = 100),
      data.frame(id = 1L, time = .obs, evid = 0L, amt = 0, cmt = "periph", rate = 0)
    )
    .exI <- .kdExact(matrix(c(-0.7, 0.5, 0.1, -0.1), 2, 2),
                     rbind(data.frame(time = .dose, cmt = 1L, add = 0, rate = 100),
                           data.frame(time = .dose + 1, cmt = 1L, add = 0, rate = -100)),
                     .obs)
    for (.method in .kdMethods) {
      .o <- .kdSolve(.kdOral, .evO, c(ka = .ka, k = 0.2), .method)$d
      expect_equal(.o$depot, .exO[, 1], tolerance = 1e-7, label = paste(.method, "oral depot"))
      expect_equal(.o$center, .exO[, 2], tolerance = 1e-7, label = paste(.method, "oral center"))
      .i <- .kdSolve(.kdInf, .evI, c(cl = 2, v = 10, q = 5, vp = 50), .method)$d
      expect_equal(.i$central, .exI[, 1], tolerance = 1e-7, label = paste(.method, "infusion central"))
      expect_equal(.i$periph, .exI[, 2], tolerance = 1e-7, label = paste(.method, "infusion periph"))
      # the kept step stays within hmin and hmax
      .h <- .kdSolve(.kdOral, .evO, c(ka = .ka, k = 0.2), .method, hmin = 1e-6, hmax = 0.5)$d
      expect_equal(.h$center, .exO[, 2], tolerance = 1e-7, label = paste(.method, "hmin/hmax"))
    }
  })

  test_that("dop853, dop5 and ros4 match lsoda across resets, replacements, multiplications and steady state", {
    .dose <- (0:9) * 12
    .obs <- seq(0.5, 119.5, by = 1)
    .ev <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "depot", ss = 0L, ii = 0),
      data.frame(id = 1L, time = .obs, evid = 0L, amt = 0, cmt = "center", ss = 0L, ii = 0),
      data.frame(id = 1L, time = c(30.25, 50.25, 70.25), evid = c(3L, 5L, 6L),
                 amt = c(0, 40, 0.5), cmt = c("depot", "center", "depot"), ss = 0L, ii = 0)
    )
    .ev <- .ev[order(.ev$time), ]
    .ref <- .kdSolve(.kdOral, .ev, c(ka = 1.2, k = 0.2), "lsoda", tol = 1e-12)$d
    # a steady-state bolus whose alag() is longer than ii leaves extra doses
    # inside the segments
    .ssModel <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lag
    })
    .evSS <- et(amt = 100, ss = 1, ii = 12) |> et(c(0.5, 7, 30, 60))
    .parSS <- c(k = 0.05, ka = 0.7, lag = 30)
    .refSS <- .kdSolve(.ssModel, .evSS, .parSS, "lsoda", tol = 1e-12)$d
    for (.method in .kdMethods) {
      .s <- .kdSolve(.kdOral, .ev, c(ka = 1.2, k = 0.2), .method)$d
      expect_equal(.s$depot, .ref$depot, tolerance = 1e-6, label = paste(.method, "events depot"))
      expect_equal(.s$center, .ref$center, tolerance = 1e-6, label = paste(.method, "events center"))
      # dop853 applies these extra doses differently from lsoda (as on main)
      if (.method != "dop853") {
        .ss <- .kdSolve(.ssModel, .evSS, .parSS, .method)$d
        expect_equal(.ss$center, .refSS$center, tolerance = 1e-6, label = paste(.method, "steady state"))
      }
    }
  })

  test_that("ros4 restarts after a dose on a stiff problem (Robertson)", {
    # bolus into y1 of the Robertson problem; y2 relaxes in about 4e-5 after
    # each dose, far inside a step kept from before it
    .rob <- rxode2({
      d/dt(y1) <- -0.04 * y1 + 1e4 * y2 * y3
      d/dt(y2) <- 0.04 * y1 - 1e4 * y2 * y3 - 3e7 * y2 * y2
      d/dt(y3) <- 3e7 * y2 * y2
    })
    .td <- c(0, 10, 100, 1000, 3000)
    .to <- sort(unique(c(.td + 0.001, .td + 0.05, .td + 1, .td + 7)))
    .ev <- rbind(
      data.frame(id = 1L, time = .td, evid = 1L, amt = 1, cmt = "y1"),
      data.frame(id = 1L, time = .to, evid = 0L, amt = 0, cmt = "y2")
    )
    .ev <- .ev[order(.ev$time, -.ev$evid), ]
    .ref <- as.data.frame(rxSolve(.rob, .ev, method = "lsoda", atol = 1e-14, rtol = 1e-12))
    .s <- as.data.frame(rxSolve(.rob, .ev, method = "ros4", atol = 1e-7, rtol = 1e-4))
    for (.y in c("y1", "y2", "y3")) {
      # the error in tolerance units
      .tu <- max(abs(.s[[.y]] - .ref[[.y]]) / (1e-7 + 1e-4 * abs(.ref[[.y]])))
      expect_lt(.tu, 1, label = .y)
    }
  })

  test_that("dop5 keeps its step across infusion edges", {
    # ten one-hour infusions observed once per interval, atol = rtol = 1e-4;
    # restarting at every infusion edge takes about a third more evaluations
    .dose <- (0:9) * 12
    .ev <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "central", rate = 100),
      data.frame(id = 1L, time = .dose + 6, evid = 0L, amt = 0, cmt = "periph", rate = 0)
    )
    .ev <- .ev[order(.ev$time), ]
    .w <- .kdSolve(.kdInf, .ev, c(cl = 2, v = 10, q = 5, vp = 50), "dop5", tol = 1e-4)$dadt
    expect_lt(.w, 1300)
  })
})
