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
        for (.method in c("dop853", "cvode", "liblsoda", "lsoda", "lsode", "bdf")) {
          for (.resample in c(FALSE, TRUE)) {
            .err <- .stepError(.stepSolve(.d, .method, .interp, .resample), .interp)
            .lab <- paste(.kind, .interp, .method, if (.resample) "resampled" else "plain")
            # dop853 lands on every record, so a step covariate is integrated
            # exactly (a midpoint step falls between records, inside a step);
            # the lsoda family stops at the evid 2 record, where a locf
            # covariate steps (a nocb one steps at the observation before it);
            # cvode and the lsoda family step past a record they need not stop
            # at, and stay at the solver tolerance
            .exact <- (.method == "dop853" && .interp != "midpoint") ||
              (.method %in% c("liblsoda", "lsoda", "lsode", "bdf") && .kind == "evid2" && .interp == "locf")
            if (.exact) {
              expect_lt(.err, 1e-13, label = .lab)
            } else {
              expect_lt(.err, 1e-9, label = .lab)
            }
          }
        }
      }
    }
  })

  test_that("a steady-state dose at a step reads the dose record's covariates", {
    # d/dt(a) = -k a with an ss = 1 bolus at t = 10, where k steps from 0.1 to
    # 0.3: the steady state is built with the dose record's k, and the
    # interval after the dose takes the next record's k (0.3 locf, 0.5 nocb)
    .m <- rxode2({
      d/dt(a) <- -k * a
    })
    .d <- function(k) {
      data.frame(
        id = 1L,
        time = c(0, 10, 11, 14, 22),
        evid = c(0L, 1L, 0L, 0L, 0L),
        amt = c(0, 100, 0, 0, 0),
        ss = c(0L, 1L, 0L, 0L, 0L),
        ii = c(0, 12, 0, 0, 0),
        cmt = "a",
        k = k
      )
    }
    .cases <- list(
      locf = list(k = c(0.1, 0.3, 0.3, 0.3, 0.3), after = 0.3),
      nocb = list(k = c(0.1, 0.3, 0.5, 0.5, 0.5), after = 0.5)
    )
    .a10 <- 100 / (1 - exp(-0.3 * 12))
    for (.interp in names(.cases)) {
      for (.method in c("dop853", "cvode", "liblsoda", "lsoda")) {
        .s <- rxSolve(
          .m,
          .d(.cases[[.interp]]$k),
          method = .method,
          covsInterpolation = .interp,
          atol = 1e-12,
          rtol = 1e-12,
          returnType = "data.frame"
        )
        .after <- .s$time > 10
        expect_equal(
          .s$a[.after],
          .a10 * exp(-.cases[[.interp]]$after * (.s$time[.after] - 10)),
          tolerance = 1e-8,
          label = paste(.interp, .method)
        )
      }
    }
  })

  test_that("ss = 2, steady-state infusions and lagged steady states read the dose record's covariates", {
    # d/dt(a) = -k a, where a locf k steps from 0.1 to 0.3 at a steady-state
    # dose at t = 10 (ii = 12); each steady state is built with k = 0.3
    .k <- 0.3
    .ii <- 12
    .obs <- c(11, 14, 22)
    .data <- function(dose) {
      .d <- rbind(
        data.frame(
          id = 1L,
          time = c(0, .obs),
          evid = 0L,
          amt = 0,
          ss = 0L,
          ii = 0,
          rate = 0,
          cmt = "a",
          k = c(0.1, rep(.k, length(.obs)))
        ),
        dose
      )
      .d[order(.d$time, -.d$evid), ]
    }
    .dose <- function(time, amt, ss, ii = 0, rate = 0, k = .k) {
      data.frame(id = 1L, time = time, evid = 1L, amt = amt, ss = ss, ii = ii, rate = rate, cmt = "a", k = k)
    }
    .m <- rxode2({
      d/dt(a) <- -k * a
    })
    .mLag <- rxode2({
      d/dt(a) <- -k * a
      alag(a) <- 2
    })
    .cases <- list(
      # ss = 2 adds the steady state to what is left of a dose at t = 0
      ss2 = list(
        m = .m,
        d = .data(rbind(.dose(0, 50, 0L, k = 0.1), .dose(10, 100, 2L, .ii))),
        exact = function(t) {
          (50 * exp(-0.1 * 10) + 100 / (1 - exp(-.k * .ii))) * exp(-.k * (t - 10))
        }
      ),
      # an infusion of 100 at rate 50 (2 time units) every 12
      infusion = list(
        m = .m,
        d = .data(.dose(10, 100, 1L, .ii, rate = 50)),
        exact = function(t) {
          .r <- 50
          .dur <- 2
          .a0 <- .r / .k * (1 - exp(-.k * .dur)) * exp(-.k * (.ii - .dur)) / (1 - exp(-.k * .ii))
          .tau <- t - 10
          .aEnd <- .r / .k * (1 - exp(-.k * .dur)) + .a0 * exp(-.k * .dur)
          ifelse(
            .tau <= .dur,
            .r / .k * (1 - exp(-.k * .tau)) + .a0 * exp(-.k * .tau),
            .aEnd * exp(-.k * (.tau - .dur))
          )
        }
      ),
      # a bolus that arrives 2 after its dose time, the last one at t = 12
      alag = list(
        m = .mLag,
        d = .data(.dose(10, 100, 1L, .ii)),
        exact = function(t) {
          100 * exp(-.k * (t - ifelse(t >= 12, 12, 0))) / (1 - exp(-.k * .ii))
        }
      )
    )
    for (.case in names(.cases)) {
      .cs <- .cases[[.case]]
      for (.method in c("dop853", "cvode", "liblsoda", "lsoda")) {
        .s <- rxSolve(
          .cs$m,
          .cs$d,
          method = .method,
          covsInterpolation = "locf",
          atol = 1e-12,
          rtol = 1e-12,
          ssAtol = 1e-12,
          ssRtol = 1e-12,
          returnType = "data.frame"
        )
        .after <- .s$time > 10
        expect_equal(
          .s$a[.after],
          .cs$exact(.s$time[.after]),
          tolerance = 1e-8,
          label = paste(.case, .method)
        )
      }
    }
  })

  test_that("a step in the first interval (nocb) or at the last record (locf) keeps the interval's value", {
    .t <- seq(0, 10, by = 0.5)
    for (.method in c("dop853", "cvode", "liblsoda", "lsoda")) {
      .tol <- if (.method == "dop853") 1e-13 else 1e-9
      # nocb: wt is 1 at t = 0 and 3 after, so (0, 0.5] already takes 3
      .d <- data.frame(id = 1L, time = .t, evid = 0L, amt = 0, cmt = "cc", wt = ifelse(.t > 0, 3, 1))
      .s <- .stepSolve(.d, .method, "nocb")
      expect_equal(.s$cc, 3 * .s$time, tolerance = .tol, label = paste("nocb", .method))
      # locf: wt is 1 up to the last record, t = 10, where it is 3, so (9.5, 10) keeps 1
      .d$wt <- ifelse(.t < 10, 1, 3)
      .s <- .stepSolve(.d, .method, "locf")
      expect_equal(.s$cc, .s$time, tolerance = .tol, label = paste("locf", .method))
    }
  })

  test_that("the record reports its own covariate value", {
    .d <- .stepData("obs")
    for (.method in c("dop853", "liblsoda")) {
      for (.interp in c("locf", "nocb")) {
        .lab <- paste(.interp, .method)
        .s <- .stepSolve(.d, .method, .interp)
        expect_equal(.s$w, .d$wt, label = .lab)
        # resampled, each subject takes one subject's wt, 5 j + 10 from t = 5
        .s <- .stepSolve(.d, .method, .interp, resample = TRUE)
        .id <- as.character(if ("id" %in% names(.s)) .s$id else .s$sim.id)
        .w0 <- vapply(split(.s$w, .id), function(.w) .w[1], numeric(1))
        expect_true(all(.w0 %in% c(5, 10, 15, 20)), label = .lab)
        expect_equal(.s$w, unname(.w0[.id]) + 10 * (.s$time >= 5), label = paste(.lab, "resampled"))
      }
    }
  })
})
