rxTest({
  # An ss=2 infusion into a compartment with f() solves, with or without an
  # alag(), and matches the explicit regimen (rxode2#1602).
  .p <- c(ka = 1.2, k = 0.2, fd = 0.8)
  .o <- c(4, 9, 15, 20)
  .off <- 12 * 300
  .kinds <- list(
    rate = list(rate = 20, model = ""),
    dur = list(dur = 2.2, model = ""),
    modeledRate = list(rate = -1, model = "rate(depot) <- 20\n"),
    modeledDur = list(rate = -2, model = "dur(depot) <- 2.2\n")
  )
  .mod <- function(kind, lag) {
    rxode2(paste0(
      "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\n",
      "f(depot) <- fd\n", kind$model, lag
    ))
  }
  .dose <- function(ev, kind, amt = 50, ...) {
    .a <- list(ev, amt = amt, ii = 12, cmt = "depot", ...)
    if (!is.null(kind$rate)) {
      .a$rate <- kind$rate
    }
    if (!is.null(kind$dur)) {
      .a$dur <- kind$dur
    }
    do.call(et, .a)
  }

  for (.k in names(.kinds)) {
    test_that(paste0("an ss=2 ", .k, " infusion with f() matches the explicit regimen"), {
      .kind <- .kinds[[.k]]
      .ev <- et(amt = 100, cmt = "depot") |>
        .dose(.kind, time = 3, ss = 2) |>
        et(.o)
      # the ss dose repeated back over 300 intervals
      .want <- et(amt = 100, time = .off, cmt = "depot") |>
        .dose(.kind, time = 3, addl = 300) |>
        et(.o + .off)
      for (.lag in c("", "alag(depot) <- 0\n", "alag(depot) <- 0.7\n")) {
        .m <- .mod(.kind, .lag)
        for (.meth in c("liblsoda", "dop853")) {
          .r <- expect_no_warning(rxSolve(.m, .ev, .p, method = .meth))
          .w <- rxSolve(.m, .want, .p, method = .meth)
          expect_equal(.r$center, .w$center, tolerance = 1e-6)
        }
      }
    })
  }

  test_that("an ss=2 infusion lasting longer than ii with f() matches the explicit regimen", {
    # 200 at rate 20 lasts 10 < ii, which f() = 1.5 makes 15 > ii; the
    # durations are 15 > ii outright
    .long <- list(
      rate = list(rate = 20, model = ""),
      dur = list(dur = 15, model = ""),
      modeledRate = list(rate = -1, model = "rate(depot) <- 20\n"),
      modeledDur = list(rate = -2, model = "dur(depot) <- 15\n")
    )
    .p2 <- c(.p[c("ka", "k")], fd = 1.5)
    for (.kind in .long) {
      .m <- .mod(.kind, "")
      .ev <- et(amt = 100, cmt = "depot") |>
        .dose(.kind, amt = 200, time = 3, ss = 2) |>
        et(c(.o, 30))
      .want <- et(amt = 100, time = .off, cmt = "depot") |>
        .dose(.kind, amt = 200, time = 3, addl = 300) |>
        et(c(.o, 30) + .off)
      for (.meth in c("liblsoda", "dop853")) {
        .r <- rxSolve(.m, .ev, .p2, method = .meth)
        .w <- rxSolve(.m, .want, .p2, method = .meth)
        expect_equal(.r$depot, .w$depot, tolerance = 1e-6)
        expect_equal(.r$center, .w$center, tolerance = 1e-6)
      }
    }
  })

  test_that("an ss=2 infusion with f() into a linCmt() depot solves", {
    for (.rate in c(20, -1)) {
      .m <- rxode2({
        cp <- linCmt(v, cl, ka)
        f(depot) <- 0.8
        rate(depot) <- 20
      })
      .lp <- c(ka = 1.2, cl = 2, v = 10)
      .ev <- et(amt = 100, cmt = "depot") |>
        et(time = 3, amt = 50, rate = .rate, ii = 12, ss = 2, cmt = "depot") |>
        et(.o)
      .want <- et(amt = 100, time = .off, cmt = "depot") |>
        et(time = 3, amt = 50, rate = .rate, ii = 12, addl = 300, cmt = "depot") |>
        et(.o + .off)
      .r <- expect_no_warning(rxSolve(.m, .ev, .lp))
      expect_equal(.r$cp, rxSolve(.m, .want, .lp)$cp, tolerance = 1e-6)
    }
  })
})
