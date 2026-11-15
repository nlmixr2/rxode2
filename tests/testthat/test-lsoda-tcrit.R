rxTest({
  # The lsoda family stops exactly (itask 4, tcrit) at every record that is
  # not a plain observation -- a dose, an infusion edge, an mtime() or evid 2
  # record -- and at a pending extra dose, instead of stepping past it and
  # interpolating back.
  .tcMethods <- c("liblsoda", "lsoda", "lsode", "bdf")

  test_that("the lsoda family stops at an mtime() or evid 2 record that opens a time window", {
    # an input of 400 for 0.05 time units from t = 12, observed at the
    # integers, with an mtime() or an evid 2 record at 12; stepping past t =
    # 12 skips the window
    .m <- rxode2({
      mtime(tp) <- 12
      if (t >= tp && t < tp + 0.05) {
        inp <- 400
      } else {
        inp <- 0
      }
      d/dt(y) <- -0.2 * y + inp
    })
    .m2 <- rxode2({
      if (t >= 12 && t < 12.05) {
        inp <- 400
      } else {
        inp <- 0
      }
      d/dt(y) <- -0.2 * y + inp
    })
    .ev <- data.frame(id = 1L, time = c(0, 1:24), evid = c(1L, rep(0L, 24)),
                      amt = c(10, rep(0, 24)), cmt = "y")
    .ev2 <- .ev
    .ev2$evid[.ev2$time == 12] <- 2L
    .k <- 0.2
    .yw0 <- 10 * exp(-.k * 12)
    .yw1 <- .yw0 * exp(-.k * 0.05) + (400 / .k) * (1 - exp(-.k * 0.05))
    .exact <- .yw1 * exp(-.k * (24 - 12.05))
    for (.method in .tcMethods) {
      .s <- rxSolve(.m, .ev, method = .method, atol = 1e-8, rtol = 1e-6,
                    returnType = "data.frame")
      expect_equal(.s$y[.s$time == 24], .exact, tolerance = 1e-4, label = paste(.method, "mtime"))
      .s2 <- rxSolve(.m2, .ev2, method = .method, atol = 1e-8, rtol = 1e-6,
                     returnType = "data.frame")
      expect_equal(.s2$y[.s2$time == 24], .exact, tolerance = 1e-4, label = paste(.method, "evid 2"))
    }
  })

  test_that("the lsoda family still matches the exact solution across doses and infusion edges", {
    .oral <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
    })
    .dose <- (0:9) * 12
    .obs <- seq(0.5, 119.5, by = 1)
    .ev <- rbind(
      data.frame(id = 1L, time = .dose, evid = 1L, amt = 100, cmt = "depot", rate = 0),
      data.frame(id = 1L, time = .dose + 2, evid = 1L, amt = 50, cmt = "center", rate = 25),
      data.frame(id = 1L, time = .obs, evid = 0L, amt = 0, cmt = "center", rate = 0)
    )
    .ev <- .ev[order(.ev$time), ]
    # exact: oral doses into depot, two-hour infusions of 50 into center
    .ka <- 1.2
    .k <- 0.2
    .exact <- function(t) {
      .c <- 0 * t
      for (.t0 in .dose) {
        .dt <- pmax(t - .t0, 0)
        .c <- .c + (t >= .t0) * 100 * .ka / (.ka - .k) * (exp(-.k * .dt) - exp(-.ka * .dt))
        .ti <- .t0 + 2
        .on <- pmin(pmax(t - .ti, 0), 2)
        .c <- .c + (t >= .ti) * 25 / .k * (1 - exp(-.k * .on)) * exp(-.k * pmax(t - .ti - 2, 0))
      }
      .c
    }
    for (.method in .tcMethods) {
      .s <- rxSolve(.oral, .ev, params = c(ka = .ka, k = .k), method = .method,
                    atol = 1e-10, rtol = 1e-10, returnType = "data.frame")
      expect_equal(.s$center, .exact(.s$time), tolerance = 1e-7, label = .method)
    }
  })

  test_that("the lsoda family agrees across extra doses between records", {
    # a steady-state bolus whose alag() is longer than ii leaves extra doses
    # inside the segments; each one is a stop
    .ss <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- lag
    })
    .ev <- et(amt = 100, ss = 1, ii = 12) |> et(c(0.5, 7, 30, 60))
    .solve <- function(method) {
      rxSolve(.ss, .ev, params = c(k = 0.05, ka = 0.7, lag = 30), method = method,
              atol = 1e-10, rtol = 1e-10, returnType = "data.frame")$center
    }
    .ref <- .solve("lsoda")
    for (.method in setdiff(.tcMethods, "lsoda")) {
      expect_equal(.solve(.method), .ref, tolerance = 1e-6, label = .method)
    }
  })
})
