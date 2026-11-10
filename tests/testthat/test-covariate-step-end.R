rxTest({
  # d/dt(cc) = wt, with wt stepping by 10 at the record t = 5 (records every
  # 0.5): cc is exactly piecewise linear (quadratic for linear interpolation).
  # While a solver integrates toward a record, a step covariate (locf, nocb)
  # keeps at the ends of the interval the value it has inside it, so a solver
  # that lands on the records, as dop853 does, is exact to roundoff; the
  # record itself still reports the record's value.
  .stepModel <- rxode2({
    d/dt(cc) <- wt
    w <- wt
  })
  .stepTimes <- seq(0, 10, by = 0.5)

  .stepData <- function(kind) {
    .d <- do.call(
      rbind,
      lapply(1:4, function(.i) {
        .x <- data.frame(
          id = .i,
          time = .stepTimes,
          evid = 0,
          amt = 0,
          cmt = "cc",
          wt = 5 * .i + 10 * (.stepTimes >= 5)
        )
        if (kind == "evid2") {
          .x <- rbind(.x, data.frame(id = .i, time = 5, evid = 2, amt = 0, cmt = "cc", wt = 5 * .i + 10))
        }
        .x
      })
    )
    .d[order(.d$id, .d$time, -.d$evid), ]
  }

  # the exact cc(t) from the values y at .stepTimes under interpolation interp
  .stepExact <- function(time, y, interp) {
    vapply(
      time,
      function(.t) {
        .s <- 0
        for (.j in seq_len(length(.stepTimes) - 1)) {
          .a <- .stepTimes[.j]
          .b <- .stepTimes[.j + 1]
          if (.t <= .a) {
            break
          }
          .e <- min(.t, .b)
          .s <- .s +
            switch(
              interp,
              locf = y[.j] * (.e - .a),
              nocb = y[.j + 1] * (.e - .a),
              midpoint = y[.j] * (min(.e, (.a + .b) / 2) - .a) + y[.j + 1] * max(0, .e - (.a + .b) / 2),
              linear = y[.j] * (.e - .a) + (y[.j + 1] - y[.j]) / (.b - .a) * (.e - .a)^2 / 2
            )
        }
        .s
      },
      numeric(1)
    )
  }

  # the largest error of cc relative to its range, over the subjects
  .stepError <- function(s, interp) {
    .id <- if ("id" %in% names(s)) "id" else "sim.id"
    max(vapply(
      split(s, s[[.id]]),
      function(.x) {
        .w0 <- .x$w[.x$time == 0][1]
        .ex <- .stepExact(.x$time, .w0 + 10 * (.stepTimes >= 5), interp)
        max(abs(.x$cc - .ex)) / max(abs(.ex))
      },
      numeric(1)
    ))
  }

  .stepSolve <- function(d, method, interp, resample = FALSE) {
    rxWithSeed(
      10,
      rxSolve(
        .stepModel,
        d,
        method = method,
        atol = 1e-12,
        rtol = 1e-10,
        covsInterpolation = interp,
        resample = resample,
        returnType = "data.frame"
      )
    )
  }

  test_that("a step covariate keeps the interval's value at its ends", {
    for (.kind in c("obs", "evid2")) {
      .d <- .stepData(.kind)
      for (.interp in c("locf", "nocb", "midpoint", "linear")) {
        for (.method in c("dop853", "cvode", "liblsoda", "lsoda")) {
          for (.resample in c(FALSE, TRUE)) {
            .err <- .stepError(.stepSolve(.d, .method, .interp, .resample), .interp)
            .lab <- paste(.kind, .interp, .method, if (.resample) "resampled" else "plain")
            # dop853 lands on every record, so a step covariate is integrated
            # exactly (a midpoint step falls between records, inside a step);
            # cvode and the lsoda family step past a record they need not stop
            # at, and stay at the solver tolerance
            if (.method == "dop853" && .interp != "midpoint") {
              expect_lt(.err, 1e-13, label = .lab)
            } else {
              expect_lt(.err, 1e-9, label = .lab)
            }
          }
        }
      }
    }
  })

  test_that("the record reports its own covariate value", {
    .d <- .stepData("obs")
    for (.method in c("dop853", "liblsoda")) {
      .s <- .stepSolve(.d, .method, "locf")
      expect_equal(.s$w, .d$wt, label = .method)
      .s <- .stepSolve(.d, .method, "nocb")
      expect_equal(.s$w, .d$wt, label = .method)
    }
  })
})
