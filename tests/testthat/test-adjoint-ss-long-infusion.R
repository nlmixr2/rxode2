rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # An ss = 1 infusion longer than ii: the infusions of the earlier periods are
  # still running and end by pushed stops, at times that may fall between
  # records.  With f(), a fixed duration's rate and a fixed rate's end move with
  # F, both inside the steady-state period and in the window.  Observations avoid
  # the infusion end times, where a fixed rate with f() puts a kink in the state.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .txtF <- paste0(.base, "\nf(depot) <- fd")
  .p0 <- c(ka = 1.2, k = 0.2)
  .pF <- c(.p0, fd = 0.7)
  .ss <- function(...) et(..., ii = 12, ss = 1, cmt = "depot")
  .scen <- list(
    rate = list(txt = .base, p = .p0, ev = .ss(amt = 100, rate = 8) |> et(c(0.25, 0.75, 1, 4, 8, 11))),
    rateTwoEarlier = list(txt = .base, p = .p0, ev = .ss(amt = 100, rate = 4) |> et(c(0.25, 0.9, 1.1, 4, 8, 11))),
    dur = list(txt = .base, p = .p0, ev = .ss(amt = 100, dur = 15) |> et(c(0.25, 2.9, 3.1, 4, 8, 11))),
    rateF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, rate = 4) |> et(c(0.25, 0.9, 1.1, 4, 8, 11))),
    rateTwoEarlierF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, rate = 2) |> et(c(0.25, 0.9, 1.1, 4, 8, 10.5, 11.5))),
    durF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 15) |> et(c(0.25, 2.9, 3.1, 4, 8, 11))),
    durTwoEarlierF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 30) |> et(c(0.25, 5.9, 6.1, 8, 11))),
    interiorDurF = list(txt = .txtF, p = .pF, ev = et(amt = 50, cmt = "depot") |>
      et(time = 5, amt = 100, dur = 15, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 5.25, 7.9, 8.1, 12, 16))),
    interiorRateF = list(txt = .txtF, p = .pF, ev = et(amt = 50, cmt = "depot") |>
      et(time = 5, amt = 100, rate = 4, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 5.25, 10, 10.75, 12, 16))),
    # a fixed duration whose stop lies past the last record solved
    openDurF = list(txt = .txtF, p = .pF, ev = et(amt = 100, dur = 15, cmt = "depot") |> et(c(0.25, 3, 8, 11)))
  )

  test_that("long ss = 1 infusions give the right sensitivities in every driver", {
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p, method = "liblsoda", maxsteps = 1e6)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj")) {
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
  })
})
