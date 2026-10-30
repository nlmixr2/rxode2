rxTest({
  # Across a dose or an infusion edge dop853, dop5 and ros4 start the next
  # segment with the step they last proposed, capped by the time scale of the
  # state just after the event, so a large jump starts small.

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

  .kdSolve <- function(model, ev, params, method, tol = 1e-10) {
    .s <- rxSolve(model, ev, params = params, method = method, atol = tol, rtol = tol,
                  returnType = "rxSolve")
    list(d = as.data.frame(.s), dadt = .s$counts$dadt)
  }

  test_that("dop853, dop5 and ros4 match the exact solution across doses and infusion edges", {
    .dose <- (0:9) * 12
    .obs <- c(.dose + 6, 48.02, 48.05, 48.1, 48.5)
    # oral doses with fast absorption (ka = 20), observed just after the dose
    # at 48: a fast transient after a quiet stretch
    .ka <- 20
    .evO <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "depot", rate = 0),
      data.frame(id = 1L, time = sort(.obs), evid = 0L, amt = 0, cmt = "center", rate = 0)
    )
    .exO <- .kdExact(matrix(c(-.ka, .ka, 0, -0.2), 2, 2),
                     data.frame(time = .dose, cmt = 1L, add = 100, rate = 0), sort(.obs))
    # one-hour infusions into a two-compartment model
    .evI <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "central", rate = 100),
      data.frame(id = 1L, time = sort(.obs), evid = 0L, amt = 0, cmt = "periph", rate = 0)
    )
    .exI <- .kdExact(matrix(c(-0.7, 0.5, 0.1, -0.1), 2, 2),
                     rbind(data.frame(time = .dose, cmt = 1L, add = 0, rate = 100),
                           data.frame(time = .dose + 1, cmt = 1L, add = 0, rate = -100)),
                     sort(.obs))
    for (.method in c("dop853", "dop5", "ros4")) {
      .o <- .kdSolve(.kdOral, .evO, c(ka = .ka, k = 0.2), .method)$d
      expect_equal(.o$depot, .exO[, 1], tolerance = 1e-7, label = paste(.method, "oral depot"))
      expect_equal(.o$center, .exO[, 2], tolerance = 1e-7, label = paste(.method, "oral center"))
      .i <- .kdSolve(.kdInf, .evI, c(cl = 2, v = 10, q = 5, vp = 50), .method)$d
      expect_equal(.i$central, .exI[, 1], tolerance = 1e-7, label = paste(.method, "infusion central"))
      expect_equal(.i$periph, .exI[, 2], tolerance = 1e-7, label = paste(.method, "infusion periph"))
    }
  })

  test_that("dop853, dop5 and ros4 keep their step across infusion edges", {
    # ten one-hour infusions observed once per interval, atol = rtol = 1e-6;
    # restarting the step at every start and stop costs dop853 1549, dop5
    # 2237 and ros4 3102 evaluations, keeping it (capped) 1285, 1704 and 2755
    .dose <- (0:9) * 12
    .ev <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "central", rate = 100),
      data.frame(id = 1L, time = .dose + 6, evid = 0L, amt = 0, cmt = "periph", rate = 0)
    )
    .ev <- .ev[order(.ev$time), ]
    .bound <- c(dop853 = 1420, dop5 = 1970, ros4 = 2930)
    for (.method in names(.bound)) {
      .w <- .kdSolve(.kdInf, .ev, c(cl = 2, v = 10, q = 5, vp = 50), .method, tol = 1e-6)$dadt
      expect_lt(.w, .bound[[.method]], label = .method)
    }
  })
})
