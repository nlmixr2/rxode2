rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A bioavailability that depends on the state makes a dose's jump depend on
  # the state it lands on: x+ = x- + F(x-)*amt (bolus), F(x-)*amt (replace) or
  # F(x-)*amt*x- (multiply), so the transpose gains amt*dF/dx at x-.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .fCenter <- "\nf(depot) <- bio/(1 + center/50)"
  .p <- c(ka = 1.2, k = 0.2, bio = 0.8)
  .o <- c(1, 2, 3.5, 5, 7, 9)
  .two <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot")
  .scen <- list(
    bolus = list(txt = paste0(.base, .fCenter), p = .p, ev = .two |> et(.o)),
    # f() of the dosed compartment itself
    bolusSelf = list(txt = paste0(.base, "\nf(depot) <- bio/(1 + depot/40)"), p = .p, ev = .two |> et(.o)),
    replace = list(txt = paste0(.base, "\nf(center) <- bio/(1 + depot/40)"), p = .p,
      ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 30, cmt = "center") |> et(.o)),
    multiply = list(txt = paste0(.base, "\nf(center) <- bio/(1 + depot/40)"), p = .p,
      ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
    multiplySelf = list(txt = paste0(.base, "\nf(center) <- bio/(1 + center/40)"), p = .p,
      ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
    bolusLag = list(txt = paste0(.base, .fCenter, "\nalag(depot) <- lg"), p = c(.p, lg = 0.4), ev = .two |> et(.o)),
    # an observation at the dose time, which is the last record time
    bolusAtObs = list(txt = paste0(.base, .fCenter), p = .p,
      ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |> et(time = 3, amt = 80, cmt = "depot"))
  )
  .chk <- function(k, meths, label) {
    .ref <- .adjFd(k$txt, k$ev, k$p, method = "liblsoda", maxsteps = 1e6)
    for (.m in meths) {
      .a <- rxSolveAdjointRk4(k$txt, k$ev,
        params = k$p, calcSens = names(k$p), method = .m,
        atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
      )
      expect_equal(nrow(.a), length(.ref[[1]]), label = paste(label, .m, "rows"))
      for (.cn in names(.ref)) {
        expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(label, .m, .cn))
      }
    }
  }
  .rk4sFamily <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s")

  test_that("a state-dependent f() on a bolus, replace or multiply is exact in every driver", {
    .meths <- c(.rk4sFamily, "liblsodaadj", "cvodesadj", "abs")
    for (.nm in names(.scen)) .chk(.scen[[.nm]], .meths, .nm)
  })

  test_that("an ss = 1 bolus with a state-dependent f() applies the jump at every period", {
    # each period's dose lands on the trough
    .ss <- list(
      start = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(.o),
      interior = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, ii = 12, ss = 1, cmt = "depot") |> et(.o)
    )
    for (.nm in names(.ss)) {
      .chk(list(txt = paste0(.base, .fCenter), p = .p, ev = .ss[[.nm]]), c(.rk4sFamily, "liblsodaadj"), .nm)
    }
  })

  test_that("an ss = 2 bolus with a state-dependent f() lands its own dose on the total state", {
    # the added regimen's trough follows its own F, the record's dose F(Y_before +
    # trough); liblsodaadj does not carry ss = 2
    .ev <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, ii = 12, ss = 2, cmt = "depot") |> et(.o)
    .chk(list(txt = paste0(.base, .fCenter), p = .p, ev = .ev), .rk4sFamily, "ss2")
  })

  test_that("a fixed-dur infusion with a state-dependent f() carries its start state in every driver", {
    # the rate F(x_on)*amt/dur moves with the start state.  The forward solve
    # re-reads F at the infusion's stop, so its result after the stop is not
    # right yet: the stop lies past the last observation here
    .evs <- list(
      interior = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, dur = 10, cmt = "depot") |> et(.o),
      atStart = et(amt = 100, cmt = "center") |> et(time = 0, amt = 80, dur = 12, cmt = "depot") |> et(.o)
    )
    for (.nm in names(.evs)) {
      .chk(list(txt = paste0(.base, .fCenter), p = .p, ev = .evs[[.nm]]),
        c(.rk4sFamily, "liblsodaadj", "cvodesadj", "abs"), .nm)
    }
  })

  test_that("a lagged dose with a state-dependent f() reads the state where it lands", {
    # a lagged ss dose lands on its period's trough; with a lag longer than ii the
    # earlier periods' doses land after the record (at t = 1 and 6, which the
    # observations avoid); a lagged infusion's start reads x_on a lag later
    .txtL <- paste0(.base, .fCenter, "\nalag(depot) <- lg")
    .oL <- c(0.25, 0.5, 1.5, 2, 4, 8, 11.5)
    .two <- function(...) et(amt = 100, cmt = "depot") |> et(time = 3, amt = 50, ii = 12, ss = 2, cmt = "depot") |>
      et(c(1, 2.2, 4, 6.4, 9, 14, 20))
    .ss1 <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(.oL)
    .chk(list(txt = .txtL, p = c(.p, lg = 0.4), ev = .ss1), c(.rk4sFamily, "liblsodaadj"), "ss1 lag")
    .chk(list(txt = .txtL, p = c(.p, lg = 13), ev = .ss1), c(.rk4sFamily, "liblsodaadj"), "ss1 lag > ii")
    .chk(list(txt = .txtL, p = c(.p, lg = 0.4), ev = .two()), .rk4sFamily, "ss2 lag")
    .chk(list(txt = .txtL, p = c(.p, lg = 15), ev = .two()), .rk4sFamily, "ss2 lag > ii")
    .inf <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, dur = 10, cmt = "depot") |> et(.o)
    .chk(list(txt = .txtL, p = c(.p, lg = 0.4), ev = .inf), c(.rk4sFamily, "liblsodaadj", "cvodesadj", "abs"), "infusion lag")
  })

  test_that("a modeled rate() or dur() infusion with a state-dependent f() moves with its start state", {
    # dur(): the rate F(x_on)*amt/dur; rate(): the stop t_on + F(x_on)*amt/R.
    # Observations before and after the stops (4.6 and 7)
    .oM <- c(1, 2, 3.5, 5, 6.5, 8, 10, 12)
    .mdl <- list(
      rate = list(txt = paste0(.base, .fCenter, "\nrate(depot) <- rr"), p = c(.p, rr = 20), r = -1),
      dur = list(txt = paste0(.base, .fCenter, "\ndur(depot) <- dd"), p = c(.p, dd = 4), r = -2)
    )
    for (.nm in names(.mdl)) {
      .k <- .mdl[[.nm]]
      .ev <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, rate = .k$r, cmt = "depot") |> et(.oM)
      .chk(list(txt = .k$txt, p = .k$p, ev = .ev), c(.rk4sFamily, "liblsodaadj", "cvodesadj", "abs"), .nm)
    }
  })

  test_that("a same-time dose, replace or multiply changes the state an infusion start reads", {
    # the infusion's f() reads center, which an event at t = 3 changes; the start
    # term applies where the start record lies among that time's events
    .txt <- paste0(.base, .fCenter, "\nf(center) <- b2/(1 + depot/40)")
    .inf <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, dur = 10, cmt = "depot")
    .evs <- list(
      bolus = .inf |> et(time = 3, amt = 30, cmt = "center") |> et(.o),
      bolusFirst = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 30, cmt = "center") |>
        et(time = 3, amt = 80, dur = 10, cmt = "depot") |> et(.o),
      replace = .inf |> et(time = 3, evid = 5, amt = 30, cmt = "center") |> et(.o),
      multiply = .inf |> et(time = 3, evid = 6, amt = 0.5, cmt = "center") |> et(.o)
    )
    for (.nm in names(.evs)) {
      .chk(list(txt = .txt, p = c(.p, b2 = 0.9), ev = .evs[[.nm]]),
        c(.rk4sFamily, "liblsodaadj", "cvodesadj", "abs"), .nm)
    }
  })
})
