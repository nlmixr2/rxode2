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

  test_that("a fixed-dur infusion with a state-dependent f() carries its start state", {
    # the rate F(x_on)*amt/dur moves with the start state; the stop lies past
    # the last observation, since the forward re-reads F at the stop
    .ev <- et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, dur = 10, cmt = "depot") |> et(.o)
    .chk(list(txt = paste0(.base, .fCenter), p = .p, ev = .ev), .rk4sFamily, "infusionDur")
  })
})
