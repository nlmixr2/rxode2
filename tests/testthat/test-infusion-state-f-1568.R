rxTest({
  # A fixed-duration infusion's rate is F*amt/dur with f() read once, at the
  # start; the stop removes that same rate (rxode2#1568).  `center` grows at
  # 1/h, so f(depot) = 1/(1 + center/5) changes during each infusion.
  .m <- rxode2("d/dt(depot) <- 0\nd/dt(center) <- 1\nf(depot) <- 1/(1 + center/5)")
  .f <- function(t) 1 / (1 + t / 5)

  test_that("the stop removes the rate its start added (rxode2#1568)", {
    ev <- et(time = 3, amt = 100, dur = 2.5, cmt = "depot") |>
      et(c(4, 5, 5.5, 6, 8, 10))
    s <- rxSolve(.m, ev)
    expect_equal(s$depot, c(40 * .f(3) * c(1, 2, 2.5), rep(100 * .f(3), 3)), tolerance = 1e-6)
  })

  test_that("overlapping infusions each keep their own start F", {
    ev <- et(time = 3, amt = 100, dur = 2.5, cmt = "depot") |>
      et(time = 4, amt = 50, dur = 4, cmt = "depot") |>
      et(time = 4, amt = 100, dur = 2.5, cmt = "depot") |>
      et(c(10, 12))
    s <- rxSolve(.m, ev)
    expect_equal(s$depot, rep(100 * .f(3) + 50 * .f(4) + 100 * .f(4), 2), tolerance = 1e-6)
  })

  test_that("infusions repeated with addl each read F at their own start", {
    ev <- et(time = 1, amt = 10, dur = 2, ii = 4, addl = 2, cmt = "depot") |>
      et(c(4, 8, 12, 20))
    s <- rxSolve(.m, ev)
    .cum <- cumsum(10 * .f(c(1, 5, 9)))
    expect_equal(s$depot, c(.cum, .cum[3]), tolerance = 1e-6)
  })

  test_that("a pushed fixed-duration infusion reads F at its start", {
    mod <- rxode2({
      d/dt(depot) <- 0
      d/dt(center) <- 1
      f(depot) <- 1 / (1 + center / 5)
      if (t >= 3 && t < 3.5) {
        infuseDur(100, 2.5, 1, 0, 0, 0)
      }
    })
    s <- rxSolve(mod, et(c(3, 4, 5.5, 8, 10)))
    expect_equal(s$depot, c(0, 40 * .f(3), rep(100 * .f(3), 3)), tolerance = 1e-6)
  })

  test_that("forward sensitivities carry a fixed-duration infusion's f() (rxode2#1568)", {
    # f(depot) depends on eta_f directly and on eta_cl through central; the
    # sensitivity forcing is read at the start and removed at the stop
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl + eta_cl); v <- exp(tv)
      f(depot) <- 1 / (1 + central / (5 * exp(eta_f)))
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - cl / v * central
    "
    th <- c(tka = log(0.5), tcl = 0, tv = log(10), eta_f = 0, eta_cl = 0)
    e <- et(time = 0, amt = 30, cmt = "central") |>
      et(time = 3, amt = 100, dur = 2.5, cmt = "depot") |>
      et(seq(0.13, 14, 0.41))
    mj <- rxode2(.mod, calcSens = c("eta_f", "eta_cl"))
    mfd <- rxode2(.mod, calcSens = c("eta_f", "eta_cl"), eventSens = "fd")
    sj <- rxSolve(mj, e, th)
    h <- 1e-4
    for (.sp in c("eta_f", "eta_cl")) {
      pp <- th
      pp[.sp] <- pp[.sp] + h
      pm <- th
      pm[.sp] <- pm[.sp] - h
      fd <- (rxSolve(mfd, e, pp)$central - rxSolve(mfd, e, pm)$central) / (2 * h)
      expect_equal(sj[[paste0("rx__sens_central_BY_", .sp, "__")]], fd, tolerance = 1e-4, info = .sp)
    }
  })
})
