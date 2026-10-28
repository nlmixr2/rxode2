rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An ss = 1 dose with alag() leaves the state at its record time ii - L into
  # the steady-state period (L = the lag modulo ii); the dose itself lands L
  # later, and a lagged infusion's starts and stops are pushed doses.  The
  # steady-state term must follow that phase and the lag's own dependence, for
  # a constant and a modeled lag.  Observations avoid the lagged dose and
  # infusion boundary times, where the state has a kink.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .txtC <- paste0(.base, "\nalag(depot) <- 0.7")
  .txtM <- paste0(.base, "\nalag(depot) <- tlag")
  .txtMF <- paste0(.txtM, "\nf(depot) <- fd")
  .p0 <- c(ka = 1.2, k = 0.2)
  .pM <- c(.p0, tlag = 0.7)
  .pMF <- c(.pM, fd = 0.8)
  .o <- c(0.25, 0.5, 1, 2, 4, 8, 11.5)
  .oi <- c(1, 4, 5.5, 6, 7, 9, 13, 16.5)
  .ss <- function(...) et(..., ii = 12, ss = 1, cmt = "depot")
  .int <- function(...) et(amt = 50, cmt = "depot") |> et(time = 5, ..., ii = 12, ss = 1, cmt = "depot")
  .chk <- function(scen, meths) {
    for (.nm in names(scen)) {
      .k <- scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p, method = "liblsoda", maxsteps = 1e6)
      for (.m in meths) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  }
  .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj")

  test_that("an ss = 1 bolus with alag() gives the right sensitivities in every driver", {
    .chk(list(
      constLag = list(txt = .txtC, p = .p0, ev = .ss(amt = 100) |> et(.o)),
      modeledLag = list(txt = .txtM, p = .pM, ev = .ss(amt = 100) |> et(.o)),
      lagF = list(txt = .txtMF, p = .pMF, ev = .ss(amt = 100) |> et(.o)),
      # the lag spans more than ii: an earlier period's dose lands inside the window
      lagOverIi = list(txt = .txtM, p = c(.p0, tlag = 13), ev = .ss(amt = 100) |> et(.o)),
      interior = list(txt = .txtM, p = .pM, ev = .int(amt = 100) |> et(.oi))
    ), .meths)
  })

  test_that("an ss = 1 infusion with alag() gives the right sensitivities in every driver", {
    .chk(list(
      rateConstLag = list(txt = .txtC, p = .p0, ev = .ss(amt = 100, rate = 50) |> et(.o)),
      rate = list(txt = .txtM, p = .pM, ev = .ss(amt = 100, rate = 50) |> et(.o)),
      dur = list(txt = .txtM, p = .pM, ev = .ss(amt = 100, dur = 3) |> et(.o)),
      rateF = list(txt = .txtMF, p = .pMF, ev = .ss(amt = 100, rate = 50) |> et(.o)),
      durF = list(txt = .txtMF, p = .pMF, ev = .ss(amt = 100, dur = 3) |> et(.o)),
      rateLagOverIi = list(txt = .txtM, p = c(.p0, tlag = 13),
        ev = .ss(amt = 100, rate = 50) |> et(c(0.5, 0.99, 1.01, 2, 2.99, 3.01, 6, 11.5))),
      # the infusion of the period before is still on at the record time
      durWrapF = list(txt = .txtMF, p = c(.p0, tlag = 11, fd = 0.8),
        ev = .ss(amt = 100, dur = 3) |> et(c(0.5, 1.5, 1.99, 2.01, 6, 10.99, 11.01, 11.5))),
      rateWrapF = list(txt = .txtMF, p = c(.p0, tlag = 11, fd = 0.8),
        ev = .ss(amt = 100, rate = 25) |> et(c(0.5, 1.5, 2.1, 2.3, 6, 10.99, 11.01, 11.5))),
      # longer than ii: the infusions of earlier periods are still on
      durLongF = list(txt = .txtMF, p = .pMF, ev = .ss(amt = 100, dur = 15) |> et(c(0.5, 1, 2, 2.6, 2.8, 4, 8, 15.6, 15.8, 20))),
      rateLongF = list(txt = .txtMF, p = .pMF, ev = .ss(amt = 100, rate = 4) |> et(c(0.5, 1, 2, 4, 8, 13, 20, 25))),
      durLongWrapF = list(txt = .txtMF, p = c(.p0, tlag = 11, fd = 0.8),
        ev = .ss(amt = 100, dur = 15) |> et(c(0.5, 1, 3, 4.1, 6, 10, 12, 16.1, 20))),
      interiorRate = list(txt = .txtM, p = .pM, ev = .int(amt = 100, rate = 50) |> et(.oi)),
      interiorDurF = list(txt = .txtMF, p = .pMF, ev = .int(amt = 100, dur = 3) |> et(.oi))
    ), .meths)
  })

  test_that("an ss = 1 modeled rate()/dur() infusion with alag() gives the right sensitivities", {
    # the drivers that carry a modeled ss infusion
    .chk(list(
      rate = list(txt = paste0(.txtM, "\nrate(depot) <- rr"), p = c(.pM, rr = 40), ev = .ss(amt = 100, rate = -1) |> et(.o)),
      dur = list(txt = paste0(.txtM, "\ndur(depot) <- dd"), p = c(.pM, dd = 2.5), ev = .ss(amt = 100, rate = -2) |> et(.o)),
      rateF = list(txt = paste0(.txtMF, "\nrate(depot) <- rr"), p = c(.pMF, rr = 40), ev = .ss(amt = 100, rate = -1) |> et(.o)),
      durF = list(txt = paste0(.txtMF, "\ndur(depot) <- dd"), p = c(.pMF, dd = 2.5), ev = .ss(amt = 100, rate = -2) |> et(.o)),
      durWrap = list(txt = paste0(.txtM, "\ndur(depot) <- dd"), p = c(.p0, tlag = 11, dd = 3),
        ev = .ss(amt = 100, rate = -2) |> et(c(0.5, 1.5, 1.99, 2.01, 6, 10.99, 11.01, 11.5)))
    ), c("rk4s", "dop853s"))
  })
})
