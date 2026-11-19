rxTest({
  # rxode2#1439: a resampled time-varying covariate is read from the sampled
  # subject at the right time, whether or not that subject is sorted yet
  .tt <- seq(0, 10, by = 0.05)

  test_that("resampled covariate in alag() uses the dose record time (#1439)", {
    .mod <- rxode2({
      alag(depot) <- wt / 10
      d/dt(depot) <- 0
      w <- wt
    })
    .d <- do.call(
      rbind,
      lapply(1:6, function(i) {
        data.frame(
          id = i,
          time = c(2, .tt),
          evid = c(1, rep(0, length(.tt))),
          amt = c(100, rep(0, length(.tt))),
          cmt = 1,
          wt = 5 * i + 2 * c(2, .tt)
        )
      })
    )
    .d <- .d[order(.d$id, .d$time, -.d$evid), ]
    for (.cores in c(1L, 2L)) {
      for (.interp in c("locf", "linear", "nocb", "midpoint")) {
        .s <- rxWithSeed(
          10,
          rxSolve(.mod, .d, resample = TRUE, covsInterpolation = .interp, cores = .cores, returnType = "data.frame")
        )
        for (.x in split(.s, .s$id)) {
          .w2 <- .x$w[abs(.x$time - 2) < 1e-8][1]
          expect_equal(
            min(.x$time[.x$depot > 50]),
            2 + .w2 / 10,
            tolerance = 1e-8,
            info = paste(.interp, .cores, .x$id[1])
          )
        }
      }
    }
  })

  test_that("resampled covariate in the ODE is interpolated in time (#1439)", {
    .mod <- rxode2({
      d/dt(cc) <- wt
      w <- wt
    })
    .t <- seq(0, 10, by = 0.5)
    .d <- do.call(
      rbind,
      lapply(1:6, function(i) {
        data.frame(id = i, time = .t, evid = 0, amt = 0, cmt = 1, wt = 5 * i + 2 * .t)
      })
    )
    for (.cores in c(1L, 2L)) {
      .s <- rxWithSeed(
        10,
        rxSolve(.mod, .d, resample = TRUE, covsInterpolation = "linear", cores = .cores, returnType = "data.frame")
      )
      for (.x in split(.s, .s$id)) {
        # wt = w0 + 2 t, so cc(10) = 10 w0 + 100
        expect_equal(.x$cc[.x$time == 10], 10 * .x$w[1] + 100, tolerance = 1e-4, info = paste(.cores, .x$id[1]))
      }
    }
  })

  test_that("resampled covariates with modeled rate()/dur() match the sampled subject (#1439)", {
    .t <- seq(0, 10, by = 0.5)
    .one <- function(i, donor = i, rate = -2) {
      .d <- data.frame(
        id = i,
        time = c(1, .t),
        evid = c(1, rep(0, length(.t))),
        amt = c(100, rep(0, length(.t))),
        rate = c(rate, rep(0, length(.t))),
        cmt = 1,
        wt = 5 * donor + ifelse(c(1, .t) >= 5, 10, 0)
      )
      .d[order(.d$time, -.d$evid), ]
    }
    .mods <- list(
      dur = rxode2({
        dur(depot) <- 1 + wt / 50
        d/dt(depot) <- -0.05 * depot * wt / 20
        w <- wt
      }),
      rate = rxode2({
        rate(depot) <- 20 + wt
        d/dt(depot) <- -0.05 * depot * wt / 20
        w <- wt
      })
    )
    for (.m in names(.mods)) {
      .rate <- ifelse(.m == "dur", -2, -1)
      .d <- do.call(rbind, lapply(1:6, function(i) .one(i, rate = .rate)))
      for (.cores in c(1L, 2L)) {
        .s <- rxWithSeed(10, rxSolve(.mods[[.m]], .d, resample = TRUE, cores = .cores, returnType = "data.frame"))
        for (.x in split(.s, .s$id)) {
          .donor <- .x$w[1] / 5
          .ref <- rxSolve(.mods[[.m]], .one(1, .donor, .rate), returnType = "data.frame")
          expect_equal(.x$depot, .ref$depot, tolerance = 1e-6, info = paste(.m, .cores, .x$id[1]))
        }
      }
    }
  })

  test_that("covariates after a dose pushed while solving keep their values", {
    .mod <- rxode2({
      mtime(pushAt) <- 2
      d/dt(depot) <- 0
      d/dt(ca) <- a
      d/dt(cb) <- b
      if (t >= pushAt && t < pushAt + 0.5 && depot < 150) {
        bolus(50, depot, 0, 0, 0)
      }
    })
    .t <- seq(0, 10, by = 1)
    .d <- data.frame(
      id = 1,
      time = c(0, .t),
      evid = c(1, rep(0, length(.t))),
      amt = c(100, rep(0, length(.t))),
      cmt = 1,
      a = 1 + c(0, .t),
      b = 100 + c(0, .t)
    )
    .s <- rxSolve(.mod, .d, covsInterpolation = "linear", returnType = "data.frame")
    .s <- .s[!duplicated(.s$time, fromLast = TRUE), ]
    expect_equal(.s$depot[.s$time == 10], 150)
    # integral of 1 + t and 100 + t
    expect_equal(.s$ca, .s$time + .s$time^2 / 2, tolerance = 1e-5)
    expect_equal(.s$cb, 100 * .s$time + .s$time^2 / 2, tolerance = 1e-5)
  })

  test_that("a pushed dose between data records interpolates its covariates", {
    .mod <- rxode2({
      mtime(pushAt) <- 2.5
      alag(depot) <- a / 10
      d/dt(depot) <- 0
      d/dt(cc) <- depot
      la <- last(a)
      if (t >= pushAt && t < pushAt + 0.01 && depot < 1) {
        bolus(50, depot, 12, 1, 0)
      }
    })
    .d <- data.frame(id = 1, time = c(0, 5, 10), evid = 0, amt = 0, cmt = 1, a = c(1, 6, 11))
    .s <- rxSolve(.mod, .d, covsInterpolation = "linear", returnType = "data.frame")
    # a(2.5) = 3.5 linearly, so the pushed bolus lands at 2.85
    expect_equal(.s$cc[.s$time == 10], 50 * (10 - 2.85), tolerance = 1e-5)
    # the addl repeat at 14.5 is past the data; last(a) is the last data record
    expect_equal(.s$la[.s$time == 10], 11)
  })

  test_that("linear covariates past the last value stay finite", {
    .d <- data.frame(
      id = 1,
      time = c(0, 5, 10, 20),
      evid = 0,
      amt = 0,
      cmt = 1,
      a = c(1, 6, 11, NA)
    )
    .s <- rxSolve(
      rxode2({
      d/dt(cc) <- a
    }),
      .d,
      covsInterpolation = "linear",
      returnType = "data.frame"
    )
    expect_equal(.s$cc, c(0, 17.5, 60, 170), tolerance = 1e-5)
    .mod <- rxode2({
      mtime(pushAt) <- 2
      d/dt(depot) <- 0
      d/dt(cc) <- a
      if (t >= pushAt && t < pushAt + 0.01 && depot < 1) {
        bolus(50, depot, 12, 1, 0)
      }
    })
    .s <- rxSolve(.mod, .d[1:3, ], covsInterpolation = "linear", returnType = "data.frame")
    expect_equal(.s$cc[.s$time == 10], 60, tolerance = 1e-5)
  })
})
