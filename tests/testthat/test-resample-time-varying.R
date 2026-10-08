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
    .d <- do.call(rbind, lapply(1:6, function(i) {
      data.frame(
        id = i, time = c(2, .tt), evid = c(1, rep(0, length(.tt))),
        amt = c(100, rep(0, length(.tt))), cmt = 1,
        wt = 5 * i + 2 * c(2, .tt)
      )
    }))
    .d <- .d[order(.d$id, .d$time, -.d$evid), ]
    for (.cores in c(1L, 2L)) {
      for (.interp in c("locf", "linear", "nocb", "midpoint")) {
        .s <- rxWithSeed(10, rxSolve(.mod, .d,
          resample = TRUE, covsInterpolation = .interp,
          cores = .cores, returnType = "data.frame"
        ))
        for (.x in split(.s, .s$id)) {
          .w2 <- .x$w[abs(.x$time - 2) < 1e-8][1]
          expect_equal(min(.x$time[.x$depot > 50]), 2 + .w2 / 10,
            tolerance = 1e-8, info = paste(.interp, .cores, .x$id[1])
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
    .d <- do.call(rbind, lapply(1:6, function(i) {
      data.frame(id = i, time = .t, evid = 0, amt = 0, cmt = 1, wt = 5 * i + 2 * .t)
    }))
    for (.cores in c(1L, 2L)) {
      .s <- rxWithSeed(10, rxSolve(.mod, .d,
        resample = TRUE, covsInterpolation = "linear",
        cores = .cores, returnType = "data.frame"
      ))
      for (.x in split(.s, .s$id)) {
        # wt = w0 + 2 t, so cc(10) = 10 w0 + 100
        expect_equal(.x$cc[.x$time == 10], 10 * .x$w[1] + 100,
          tolerance = 1e-4, info = paste(.cores, .x$id[1])
        )
      }
    }
  })
})
