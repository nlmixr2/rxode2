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
})
