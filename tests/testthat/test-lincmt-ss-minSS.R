rxTest({
  # linCmt() steady states follow minSS/maxSS like the ODE solve (rxode2#1500)
  .linSsMod <- list(
    "1" = rxode2({
      cp <- linCmt(ka, cl, v)
      alag(depot) <- lag
    }),
    "2" = rxode2({
      cp <- linCmt(ka, cl, v, q, vp)
      alag(depot) <- lag
    }),
    "3" = rxode2({
      cp <- linCmt(ka, cl, v, q, vp, q2, vp2)
      alag(depot) <- lag
    })
  )
  .linSsPar <- c(ka = 0.7, cl = 1, v = 20, q = 2, vp = 30, q2 = 0.5, vp2 = 50)
  .ii <- 12
  .tobs <- c(0.5, 3, 7)
  .linSs <- function(ncmt, minSS, maxSS = minSS, extra = list(), ss = 1, lag = 0, cmt = "depot", ssTol = 1e-8) {
    .e <- do.call(et, c(list(amt = 100, ss = ss, ii = .ii, cmt = cmt), extra))
    rxSolve(
      .linSsMod[[ncmt]],
      et(.e, .tobs),
      params = c(.linSsPar, lag = lag),
      minSS = minSS,
      maxSS = maxSS,
      ssAtol = ssTol,
      ssRtol = ssTol,
      returnType = "data.frame"
    )$cp
  }
  # the same after n explicit doses before the observed one
  .linNDose <- function(ncmt, n, extra = list(), lag = 0, cmt = "depot") {
    .e <- do.call(et, c(list(amt = 100, ii = .ii, addl = n, cmt = cmt), extra))
    rxSolve(
      .linSsMod[[ncmt]],
      et(.e, .tobs + n * .ii),
      params = c(.linSsPar, lag = lag),
      returnType = "data.frame"
    )$cp
  }

  test_that("linCmt() minSS = maxSS fixes the number of prior bolus doses", {
    for (.ncmt in c("1", "2", "3")) {
      for (.n in c(1, 3, 6)) {
        expect_equal(
          .linSs(.ncmt, .n, cmt = "central"),
          .linNDose(.ncmt, .n, cmt = "central"),
          tolerance = 1e-6
        )
      }
      # alag(depot) needs at least 2 prior depot doses
      for (.n in c(2, 4)) {
        expect_equal(.linSs(.ncmt, .n), .linNDose(.ncmt, .n), tolerance = 1e-6)
        expect_equal(.linSs(.ncmt, .n, ss = 2), .linNDose(.ncmt, .n), tolerance = 1e-6)
        expect_equal(
          .linSs(.ncmt, .n, lag = 2),
          .linNDose(.ncmt, .n, lag = 2),
          tolerance = 1e-6
        )
      }
    }
  })

  test_that("linCmt() minSS = maxSS fixes the number of prior infusions", {
    for (.ncmt in c("1", "2", "3")) {
      for (.extra in list(list(rate = 50), list(dur = 3))) {
        for (.n in c(1, 3, 6)) {
          for (.cmt in c("depot", "central")) {
            expect_equal(
              .linSs(.ncmt, .n, extra = .extra, cmt = .cmt),
              .linNDose(.ncmt, .n, extra = .extra, cmt = .cmt),
              tolerance = 1e-6
            )
          }
        }
      }
    }
  })

  test_that("linCmt() stops at minSS with disabled tolerances, like the ODE solve", {
    for (.ncmt in c("1", "2", "3")) {
      for (.n in c(2, 5)) {
        expect_equal(.linSs(.ncmt, .n, .n + 2, ssTol = 1e8), .linNDose(.ncmt, .n), tolerance = 1e-6)
        expect_equal(
          .linSs(.ncmt, .n, .n + 2, ssTol = 1e8, cmt = "central"),
          .linNDose(.ncmt, .n, cmt = "central"),
          tolerance = 1e-6
        )
        for (.extra in list(list(rate = 50), list(dur = 3))) {
          expect_equal(
            .linSs(.ncmt, .n, .n + 2, extra = .extra, ssTol = 1e8),
            .linNDose(.ncmt, .n, extra = .extra),
            tolerance = 1e-6
          )
        }
      }
    }
  })

  test_that("linCmt() keeps the exact steady state when minSS < maxSS", {
    .exact <- .linNDose("2", 400)
    expect_equal(.linSs("2", 10, 1000), .exact, tolerance = 1e-8)
    expect_equal(.linSs("2", 2, 3), .exact, tolerance = 1e-8)
    expect_equal(
      .linSs("2", 10, 1000, extra = list(dur = 3)),
      .linNDose("2", 400, extra = list(dur = 3)),
      tolerance = 1e-8
    )
  })
})
