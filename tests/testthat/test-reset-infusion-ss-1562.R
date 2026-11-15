rxTest({
  # A steady-state (ss=1/2) dose resets the system and cancels a running
  # infusion; its stop, a record or a pending pushed dose, must not then apply
  # (rxode2#1562).
  .m <- rxode2({
    d/dt(depot) <- -ka * depot
    d/dt(center) <- ka * depot - k * center
  })
  .p <- c(ka = 1.2, k = 0.2)
  .o <- c(1, 3, 4.5, 6, 8)
  .meths <- c("liblsoda", "lsoda", "dop853")

  .solve <- function(mod, ev, meth, cmt = "depot") {
    rxSolve(mod, ev, .p, method = meth)[[cmt]]
  }

  test_that("an infusion started at an ss=1 dose time is cancelled by its reset", {
    ev <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
      et(time = 0, amt = 100, rate = 20, cmt = "center") |>
      et(c(1, 4, 6, 9, 14))
    want <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 6, 9, 14))
    for (meth in .meths) {
      expect_equal(.solve(.m, ev, meth, "center"), .solve(.m, want, meth, "center"), tolerance = 1e-6)
    }
  })

  test_that("ss=1 and ss=2 doses mid-infusion cancel the infusion, stop included", {
    # what is left is the depot amount at t=2, i.e. an infusion that ended there
    want <- et(amt = 50, rate = 25, cmt = "depot") |> et(.o)
    for (ss in 1:2) {
      ev <- et(amt = 100, rate = 25, cmt = "depot") |>
        et(time = 2, amt = 100, ii = 12, ss = ss, cmt = "center") |>
        et(.o)
      for (meth in .meths) {
        d <- .solve(.m, ev, meth)
        if (ss == 1) {
          expect_equal(d[-1], rep(0, 4), tolerance = 1e-6)
        } else {
          expect_equal(d, .solve(.m, want, meth), tolerance = 1e-6)
        }
      }
    }
  })

  test_that("an ss=2 dose cancels a running steady-state infusion, stop included", {
    ev <- et(amt = 100, rate = 25, ii = 12, ss = 1, cmt = "depot") |>
      et(time = 2, amt = 100, ii = 12, ss = 2, cmt = "center") |>
      et(.o)
    for (meth in .meths) {
      for (cancel in c(FALSE, TRUE)) {
        d <- rxSolve(.m, ev, .p, method = meth, ss2cancelAllPending = cancel)$depot
        expect_equal(d[-1], d[2] * exp(-1.2 * (.o[-1] - 3)), tolerance = 1e-5)
      }
    }
  })

  test_that("an ss=2 dose keeps the stop of a pending lagged steady-state infusion", {
    ml <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- 3
    })
    o <- c(0.25, 1, 2, 4, 8, 12, 14, 18)
    # pending at t=0.5: the running stop at 1, then on at 3 and off at 13
    ev <- et(amt = 100, rate = 10, ii = 12, ss = 1, cmt = "depot") |>
      et(time = 0.5, amt = 100, ii = 12, ss = 2, cmt = "center") |>
      et(o)
    for (meth in .meths) {
      s <- rxSolve(ml, ev, .p, method = meth)
      # the depot amount the ss=2 dose leaves at t=0.5
      d0 <- rxSolve(
        ml,
        et(amt = 100, rate = 10, ii = 12, ss = 1, cmt = "depot") |> et(0.5),
        .p,
        method = meth
      )$depot
      want <- et(time = 3, amt = 100, rate = 10, cmt = "depot") |> et(o)
      w <- rxSolve(.m, want, .p, method = meth)$depot
      expect_equal(s$depot[-1], w[-1] + d0 * exp(-1.2 * (o[-1] - 0.5)), tolerance = 1e-5)
    }
  })

  test_that("a reset during an addl repeat of a steady-state infusion cancels it", {
    # the repeat at 12 is cancelled at 14; the one at 24 runs
    o <- c(13, 15, 17, 20, 30)
    ev <- et(amt = 100, rate = 25, ii = 12, ss = 1, addl = 2, cmt = "depot") |>
      et(time = 14, evid = 3) |>
      et(o)
    want <- et(time = 24, amt = 100, rate = 25, cmt = "depot") |> et(o)
    for (meth in .meths) {
      d <- .solve(.m, ev, meth)
      expect_equal(d[o > 14], .solve(.m, want, meth)[o > 14], tolerance = 1e-6)
    }
  })
})
