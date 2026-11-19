rxTest({
  # A fixed-rate infusion's f() scales the amount, so the stop t0 + f*amt/rate
  # moves with f(); the forward sensitivity needs that stop-time jump
  # (rxode2#1676).
  .mod <- "
    ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
    alag(depot) <- exp(tlag)
    f(depot) <- exp(eta_f)
    d/dt(depot) <- -ka * depot
    d/dt(central) <- ka * depot - cl / v * central
  "
  .p <- c("eta_f", "tka", "tlag")
  .th <- c(tka = log(0.5), tcl = 0, tv = log(10), tlag = log(0.5), eta_f = 0.2)
  .mj <- rxode2(.mod, calcSens = .p)
  .mfd <- rxode2(.mod, calcSens = .p, eventSens = "fd")

  .expectFd <- function(e) {
    sj <- rxSolve(.mj, e, .th)
    h <- 1e-5
    for (.sp in .p) {
      pp <- .th
      pp[.sp] <- pp[.sp] + h
      pm <- .th
      pm[.sp] <- pm[.sp] - h
      sp <- rxSolve(.mfd, e, pp)
      sm <- rxSolve(.mfd, e, pm)
      for (.st in c("depot", "central")) {
        fd <- (sp[[.st]] - sm[[.st]]) / (2 * h)
        expect_equal(sj[[paste0("rx__sens_", .st, "_BY_", .sp, "__")]], fd, tolerance = 1e-4, info = paste(.st, .sp))
      }
    }
  }

  test_that("a fixed-rate infusion's f() moves the stop in the sensitivities (rxode2#1676)", {
    .expectFd(
      et(time = 3, amt = 100, rate = 40, cmt = "depot") |>
        et(seq(0.13, 14, 0.41))
    )
  })

  test_that("fixed-rate infusions repeated with addl each move their stop", {
    .expectFd(
      et(time = 1, amt = 50, rate = 20, ii = 6, addl = 2, cmt = "depot") |>
        et(seq(0.13, 24, 0.41))
    )
  })

  test_that("overlapping fixed-rate infusions at the same rate keep their own stop", {
    .expectFd(
      et(time = 1, amt = 50, rate = 20, cmt = "depot") |>
        et(time = 2, amt = 100, rate = 20, cmt = "depot") |>
        et(seq(0.13, 14, 0.41))
    )
  })
})
