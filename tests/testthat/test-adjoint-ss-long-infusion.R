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
  # observations run past every pushed stop (the earlier periods' and the ss
  # dose's own)
  .scen <- list(
    rate = list(txt = .base, p = .p0, ev = .ss(amt = 100, rate = 8) |> et(c(0.25, 0.75, 1, 4, 8, 11, 13, 16))),
    rateTwoEarlier = list(txt = .base, p = .p0, ev = .ss(amt = 100, rate = 4) |> et(c(0.25, 0.9, 1.1, 4, 8, 11, 14, 20, 26, 30))),
    dur = list(txt = .base, p = .p0, ev = .ss(amt = 100, dur = 15) |> et(c(0.25, 2.9, 3.1, 4, 8, 11, 16, 20))),
    rateF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, rate = 4) |> et(c(0.25, 0.9, 1.1, 4, 8, 11, 18, 22))),
    rateTwoEarlierF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, rate = 2) |> et(c(0.25, 0.9, 1.1, 4, 8, 10.5, 11.5, 23.5, 30, 36))),
    durF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 15) |> et(c(0.25, 2.9, 3.1, 4, 8, 11, 16, 20))),
    durTwoEarlierF = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 30) |> et(c(0.25, 5.9, 6.1, 8, 11, 19, 25, 31))),
    interiorDurF = list(txt = .txtF, p = .pF, ev = et(amt = 50, cmt = "depot") |>
      et(time = 5, amt = 100, dur = 15, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 5.25, 7.9, 8.1, 12, 16, 21, 24))),
    interiorRateF = list(txt = .txtF, p = .pF, ev = et(amt = 50, cmt = "depot") |>
      et(time = 5, amt = 100, rate = 4, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 5.25, 10, 10.75, 12, 16, 23, 26))),
    # a regular infusion still running at a pushed stop of the ss dose: each stop
    # closes the window it belongs to
    ss15RegDur = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 15) |> et(time = 14, amt = 60, dur = 4, cmt = "depot") |>
      et(c(1, 5, 11, 13, 14.5, 16, 20, 24, 28))),
    ss30RegDur = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 30) |> et(time = 29, amt = 60, dur = 4, cmt = "depot") |>
      et(c(1, 5, 11, 13, 20, 25, 28.5, 31, 34, 40))),
    ss15RegRate = list(txt = .txtF, p = .pF, ev = .ss(amt = 100, dur = 15) |> et(time = 14, amt = 60, rate = 20, cmt = "depot") |>
      et(c(1, 5, 11, 13, 14.5, 16, 20, 24, 28))),
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

  test_that("a short ss = 1 infusion's pushed stop leaves a running regular infusion open", {
    .o <- c(0.5, 1.5, 2.1, 3, 5, 8, 12, 16, 24)
    .ss24 <- function(...) et(..., ii = 24, ss = 1, cmt = "depot")
    .short <- list(
      rateRate = .ss24(amt = 100, rate = 40) |> et(time = 1, amt = 100, rate = 30, cmt = "depot") |> et(.o),
      rateDur = .ss24(amt = 100, rate = 40) |> et(time = 1, amt = 100, dur = 3, cmt = "depot") |> et(.o),
      durDur = .ss24(amt = 100, dur = 2.5) |> et(time = 1, amt = 100, dur = 3, cmt = "depot") |> et(.o)
    )
    for (.nm in names(.short)) {
      # a short ss infusion's stop is a record, so dop853 is a valid reference
      .ref <- .adjFd(.txtF, .short[[.nm]], .pF)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(.txtF, .short[[.nm]],
          params = .pF, calcSens = names(.pF), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-12, ssRtol = 1e-12, maxSS = 50000
        )
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("an ss = 1 fixed infusion lasting exactly ii carries its f() term", {
    # the total rate is constant, but F scales a fixed duration's rate and moves
    # a fixed rate's end; the steady state is an equilibrium, so the adaptive
    # methods run with hmax = 0.1
    .full <- list(
      rate = .ss(amt = 100, rate = 70 / 12) |> et(c(1, 3, 6, 9, 11.5, 13, 16)),
      dur = .ss(amt = 100, dur = 12) |> et(c(1, 3, 6, 9, 11.5, 13, 16)),
      interiorRate = et(amt = 50, cmt = "depot") |> et(time = 5, amt = 100, rate = 70 / 12, ii = 12, ss = 1, cmt = "depot") |>
        et(c(1, 4, 6, 9, 14, 16.5, 19))
    )
    for (.nm in names(.full)) {
      .ref <- .adjFd(.txtF, .full[[.nm]], .pF, method = "liblsoda", maxsteps = 1e6)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s")) {
        .a <- rxSolveAdjointRk4(.txtF, .full[[.nm]],
          params = .pF, calcSens = names(.pF), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0, hmax = if (.m == "rk4s") NULL else 0.1,
          ssAtol = 1e-12, ssRtol = 1e-12, maxSS = 50000
        )
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("an ss = 1 fixed-rate infusion lasting a multiple of ii carries its f() term", {
    # the steady state is an equilibrium (constant total rate), so an adaptive
    # step grows far past the transient an F change starts; hmax resolves it
    .pM <- c(.p0, fd = 0.5)
    .mult <- list(
      D24 = .ss(amt = 192, rate = 4) |> et(c(1, 3, 6, 11, 13, 18, 23)),
      D36 = .ss(amt = 288, rate = 4) |> et(c(1, 3, 6, 11, 13, 18, 23)),
      interiorD24 = et(amt = 50, cmt = "depot") |> et(time = 5, amt = 192, rate = 4, ii = 12, ss = 1, cmt = "depot") |>
        et(c(1, 4, 6, 8, 11, 16, 18, 23, 28))
    )
    for (.nm in names(.mult)) {
      .ref <- .adjFd(.txtF, .mult[[.nm]], .pM, method = "liblsoda", maxsteps = 1e6)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s")) {
        .a <- rxSolveAdjointRk4(.txtF, .mult[[.nm]],
          params = .pM, calcSens = names(.pM), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0, hmax = if (.m == "rk4s") NULL else 0.1,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000
        )
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
