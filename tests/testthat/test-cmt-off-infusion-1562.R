rxTest({
  # Turning a compartment off (evid=2, negative cmt) cancels its running
  # infusion; its stop must not then apply, or the compartment is left at
  # -rate and turned back on (rxode2#1562).
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

  test_that("turning a compartment off cancels its running infusion, stop included", {
    d <- data.frame(
      id = 1,
      time = c(0, 0, 1, 2, 3, 4.5, 6, 8),
      evid = c(1, 1, 0, 2, 0, 0, 0, 0),
      cmt = c(1, 2, 1, -1, 1, 1, 1, 1),
      amt = c(100, 50, 0, 0, 0, 0, 0, 0),
      rate = c(25, 10, 0, 0, 0, 0, 0, 0)
    )
    # depot emptied at t=2; the center infusion keeps running (0 to 5)
    want <- et(amt = 50, rate = 25, cmt = "depot") |>
      et(amt = 50, rate = 10, cmt = "center") |>
      et(time = 2, amt = 0, evid = 5, cmt = "depot") |>
      et(c(1, 3, 4.5, 6, 8))
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth)
      expect_equal(s$depot[s$time > 2], rep(0, 4), tolerance = 1e-6)
      w <- rxSolve(.m, want, .p, method = meth)
      expect_equal(s$center[s$time != 2], w$center, tolerance = 1e-6)
    }
  })

  test_that("turning a compartment off cancels a steady-state infusion's pending stop", {
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, 3, 4.5, 6, 8),
      evid = c(1, 0, 2, 0, 0, 0, 0),
      cmt = c(1, 1, -1, 1, 1, 1, 1),
      amt = c(100, 0, 0, 0, 0, 0, 0),
      rate = c(25, 0, 0, 0, 0, 0, 0),
      ii = c(12, 0, 0, 0, 0, 0, 0),
      ss = c(1, 0, 0, 0, 0, 0, 0)
    )
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth)
      expect_equal(s$depot[s$time > 2], rep(0, 4), tolerance = 1e-6)
    }
  })

  test_that("turning a compartment off cancels overlapping steady-state infusions", {
    # dur 20 over ii 1: about 20 overlapping infusions are running at the off
    d <- data.frame(
      id = 1,
      time = c(0, 0.5, 1:30),
      evid = c(1, 2, rep(0, 30)),
      cmt = c(1, -1, rep(1, 30)),
      amt = c(100, rep(0, 31)),
      rate = c(5, rep(0, 31)),
      ii = c(1, rep(0, 31)),
      ss = c(1, rep(0, 31))
    )
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth, cores = 1)
      expect_equal(s$depot[s$time > 0.5], rep(0, 30), tolerance = 1e-6)
    }
  })

  test_that("turning a compartment off keeps a later same-rate infusion's stop", {
    # rate 10 over 0-10 is running at the off; rate 10 over 2-4 starts after it
    o <- c(1.5, 3, 5, 8, 12)
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, o),
      evid = c(1, 2, 1, rep(0, length(o))),
      cmt = c(1, -1, 1, rep(1, length(o))),
      amt = c(100, 0, 20, rep(0, length(o))),
      rate = c(10, 0, 10, rep(0, length(o)))
    )
    want <- et(time = 2, amt = 20, rate = 10, cmt = "depot") |> et(o)
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth)
      expect_equal(s$depot[s$time > 1], .solve(.m, want, meth), tolerance = 1e-6)
    }
  })

  test_that("many compartment-off records keep later subjects intact on two cores", {
    n <- 20
    d <- data.frame(
      time = c(0:(n - 1), 0:(n - 1) + 0.5, seq(0.25, n, by = 0.5)),
      evid = c(rep(1, n), rep(2, n), rep(0, 2 * n)),
      cmt = c(rep(1, n), rep(-1, n), rep(1, 2 * n)),
      amt = c(rep(10, n), rep(0, 3 * n)),
      rate = c(rep(10, n), rep(0, 3 * n))
    )
    d <- d[order(d$time), ]
    d <- do.call(rbind, lapply(1:4, function(i) cbind(id = i, d)))
    one <- rxSolve(.m, d[d$id == 1, ], .p, cores = 1)$depot
    expect_true(all(one >= -1e-8))
    for (cores in 1:2) {
      s <- rxSolve(.m, d, .p, cores = cores)
      for (i in 1:4) {
        expect_equal(s$depot[s$id == i], one)
      }
    }
  })

  test_that("a compartment turned off mid-infusion is turned back on by a dose", {
    d <- data.frame(
      id = 1,
      time = c(0, 1, 2, 3, 5, 6, 8),
      evid = c(1, 0, 2, 0, 1, 0, 0),
      cmt = c(1, 1, -1, 1, 1, 1, 1),
      amt = c(100, 0, 0, 0, 10, 0, 0),
      rate = 0
    )
    d$rate[1] <- 25
    for (meth in .meths) {
      s <- rxSolve(.m, d, .p, method = meth)
      expect_equal(s$depot[s$time == 3], 0, tolerance = 1e-6)
      expect_equal(s$depot[s$time > 5], 10 * exp(-1.2 * (c(6, 8) - 5)), tolerance = 1e-5)
    }
  })

  test_that("event sensitivities follow a compartment turned off mid-infusion", {
    skip_on_cran()
    txt <- "d/dt(depot) <- -ka * depot\nd/dt(center) <- ka * depot - k * center\nrate(depot) <- r"
    ms <- rxode2(txt, calcSens = c("r", "ka"), eventSens = "jump")
    mb <- rxode2(txt)
    p <- c(ka = 1.2, k = 0.2, r = 25)
    o <- c(1, 1.5, 3, 4.5, 6, 8)
    d <- data.frame(
      id = 1,
      time = c(0, 2, o),
      evid = c(1, 2, rep(0, length(o))),
      cmt = c("depot", "-depot", rep("depot", length(o))),
      amt = c(100, rep(0, length(o) + 1)),
      rate = c(-1, rep(0, length(o) + 1))
    )
    d <- d[order(d$time), ]
    s <- as.data.frame(rxSolve(ms, d, p))
    for (pn in c("r", "ka")) {
      h <- p[[pn]] * 1e-6
      pp <- pm <- p
      pp[pn] <- pp[pn] + h
      pm[pn] <- pm[pn] - h
      for (st in c("depot", "center")) {
        fd <- (rxSolve(mb, d, pp)[[st]] - rxSolve(mb, d, pm)[[st]]) / (2 * h)
        expect_equal(s[[sprintf("rx__sens_%s_BY_%s__", st, pn)]], fd, tolerance = 1e-4)
      }
    }
  })
})
