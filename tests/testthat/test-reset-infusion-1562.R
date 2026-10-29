rxTest({
  # A reset (evid=3/4, ss=1/2) cancels a running infusion; its stop record must
  # not then apply, or the compartment is left at -rate (rxode2#1562).
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

  test_that("evid=3 during an infusion cancels it, stop included", {
    ev <- et(amt = 100, rate = 25, cmt = "depot") |> et(time = 2, evid = 3) |> et(.o)
    for (meth in .meths) {
      d <- .solve(.m, ev, meth)
      expect_equal(d[1], 14.56, tolerance = 1e-3)
      expect_equal(d[-1], rep(0, 4), tolerance = 1e-6)
    }
  })

  test_that("evid=4 during an infusion cancels it and gives the new dose", {
    ev <- et(amt = 100, rate = 25, cmt = "depot") |>
      et(time = 2, evid = 4, amt = 50, cmt = "depot") |>
      et(.o)
    for (meth in .meths) {
      d <- .solve(.m, ev, meth)
      expect_equal(d[-1], 50 * exp(-1.2 * (.o[-1] - 2)), tolerance = 1e-5)
    }
  })

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

  test_that("modeled rate and duration infusions are cancelled by a reset", {
    mr <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      rate(depot) <- 25
    })
    md <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      dur(depot) <- 4
    })
    for (meth in .meths) {
      for (r in list(list(mr, -1), list(md, -2))) {
        ev <- et(amt = 100, rate = r[[2]], cmt = "depot") |> et(time = 2, evid = 3) |> et(.o)
        d <- .solve(r[[1]], ev, meth)
        expect_equal(d[1], 14.56, tolerance = 1e-3)
        expect_equal(d[-1], rep(0, 4), tolerance = 1e-6)
        # a phantom (evid=7) modeled start still sets the rate
        ev <- et(amt = 100, rate = r[[2]], evid = 7, cmt = "depot") |>
          et(time = 2, evid = 3) |>
          et(.o)
        expect_equal(.solve(r[[1]], ev, meth)[-1], rep(0, 4), tolerance = 1e-6)
      }
    }
  })

  test_that("a reset cancels a lagged infusion, stop included", {
    ml <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      alag(depot) <- 0.5
    })
    ev <- et(amt = 100, rate = 25, cmt = "depot") |> et(time = 2, evid = 3) |> et(.o)
    for (meth in .meths) {
      expect_equal(.solve(ml, ev, meth)[-1], rep(0, 4), tolerance = 1e-6)
    }
  })

  test_that("fixed-duration infusions are cancelled by a reset, on any core count", {
    ev <- et(amt = 100, dur = 4, cmt = "depot") |>
      et(time = 2, evid = 3) |>
      et(.o) |>
      et(id = 1:4)
    for (meth in .meths) {
      for (cores in 1:2) {
        d <- rxSolve(.m, ev, .p, method = meth, cores = cores)$depot
        expect_equal(d[rep(.o, 4) > 2], rep(0, 16), tolerance = 1e-6)
      }
    }
  })

  test_that("a reset cancels overlapping pushed modeled infusions", {
    mp <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(center) <- ka * depot - k * center
      rate(depot) <- 25
      if (t < 1e-8) {
        evid_(0, 1, 100, 1, -1, 2, 1, 0)
      }
    })
    # starts at 0 and 2 (4 h each); reset at 3, before either stop
    o <- c(0, 1, 2.5, 4, 5, 7)
    for (meth in .meths) {
      d <- rxSolve(mp, et(time = 3, evid = 3) |> et(o), .p, method = meth)$depot
      expect_equal(d[o > 3], rep(0, 3), tolerance = 1e-6)
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

  test_that("an infusion started after the reset keeps its own stop", {
    o <- c(.o, 3.4, 3.6)
    ev <- et(amt = 100, rate = 25, cmt = "depot") |>
      et(time = 2, evid = 3) |>
      et(time = 2.5, amt = 10, rate = 10, cmt = "depot") |>
      et(o)
    want <- et(time = 2.5, amt = 10, rate = 10, cmt = "depot") |> et(o)
    for (meth in .meths) {
      d <- .solve(.m, ev, meth)
      expect_true(all(d >= 0))
      expect_equal(d[o > 2], .solve(.m, want, meth)[o > 2], tolerance = 1e-6)
    }
  })

  test_that("a reset does not take a later infusion's stop for a start without one", {
    # the classic infusion start at t=0 has no stop record
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, 3, 6, 8, 10),
      evid = c(10101, 3, 1, 0, 0, 0, 0),
      cmt = 1,
      amt = c(10, NA, 50, NA, NA, NA, NA),
      rate = c(0, 0, 10, 0, 0, 0, 0)
    )
    want <- data.frame(
      id = 1,
      time = c(2, 3, 6, 8, 10),
      evid = c(1, 0, 0, 0, 0),
      cmt = 1,
      amt = c(50, NA, NA, NA, NA),
      rate = c(10, 0, 0, 0, 0)
    )
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth)
      w <- rxSolve(.m, want, .p, method = meth)
      expect_equal(s$depot[s$time > 2], w$depot[w$time > 2], tolerance = 1e-6)
    }
  })

  test_that("many resets during infusions keep later subjects intact", {
    # more ignored stops than the initial list holds, on one thread
    ev <- et(amt = 10, rate = 10, ii = 1, addl = 39, cmt = "depot") |>
      et(time = seq(0.5, 39.5, by = 1), evid = 3) |>
      et(seq(0.25, 40, by = 0.5)) |>
      et(id = 1:2)
    s <- rxSolve(.m, ev, .p, cores = 1)
    expect_true(all(s$depot >= -1e-8))
    expect_equal(s$depot[s$id == 2], s$depot[s$id == 1])
    one <- rxSolve(.m, ev |> et(id = 1), .p, cores = 1)
    expect_equal(s$depot[s$id == 1], one$depot)
  })

  test_that("adjoint sensitivities follow a reset during an infusion", {
    skip_on_cran()
    mText <- "d/dt(depot)=-ka*depot\nd/dt(center)=ka*depot-(cl/v)*center"
    cs <- c("ka", "cl", "v")
    p <- c(ka = 1.2, cl = 3.5, v = 25)
    ex <- .rxAdjointExpand(mText, cs)
    madj <- rxode2(ex$text)
    mbase <- rxode2(mText)
    ev <- et(amt = 100, rate = 25, cmt = "depot") |>
      et(time = 2, evid = 4, amt = 50, cmt = "depot") |>
      et(c(1, 3, 4.5, 6, 8))
    solveBase <- function(pp) {
      as.matrix(as.data.frame(
        rxSolve(mbase, ev, params = pp, method = "rk4", cores = 1)
      )[, c("depot", "center")])
    }
    expect_true(all(solveBase(p) >= 0))
    for (meth in c("rk4s", "dop853s", "cvodesadj")) {
      sd <- as.data.frame(rxSolve(madj, ev, params = p, method = meth, cores = 1))
      expect_true(all(sd$depot >= 0))
      fdmax <- 0
      for (pn in cs) {
        hh <- p[[pn]] * 1e-6
        pp <- pm <- p
        pp[pn] <- pp[pn] + hh
        pm[pn] <- pm[pn] - hh
        fd <- (solveBase(pp) - solveBase(pm)) / (2 * hh)
        for (k in seq_along(ex$st)) {
          fdmax <- max(fdmax, max(abs(sd[[sprintf("rx__sens_%s_BY_%s__", ex$st[k], pn)]] - fd[, k])))
        }
      }
      expect_lt(fdmax, if (meth == "cvodesadj") 1e-3 else 1e-4)
    }
  })
})
