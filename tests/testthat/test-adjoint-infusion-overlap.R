rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # Overlapping infusion windows into one compartment: each stop closes the
  # window of its own start, and the duals of windows open at one step sum.
  # Observations avoid the infusion end times, where a modeled duration puts
  # a kink in the state.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")
  .scen <- list(
    fixedRateF = list(txt = paste0(.base, "\nf(depot) <- fd"), p = c(ka = 1.2, k = 0.2, fd = 0.7),
      ev = et(amt = 100, rate = 10, cmt = "depot") |> et(time = 4, amt = 100, rate = 10, cmt = "depot") |>
        et(c(1, 3, 5, 8, 12, 16, 24))),
    fixedDurF = list(txt = paste0(.base, "\nf(depot) <- fd"), p = c(ka = 1.2, k = 0.2, fd = 0.7),
      ev = et(amt = 100, dur = 3, ii = 2, addl = 2, cmt = "depot") |> et(c(1, 2.5, 4, 5.5, 7.5, 10, 14))),
    modeledRate = list(txt = paste0(.base, "\nrate(depot) <- r"), p = c(ka = 1.2, k = 0.2, r = 10),
      ev = et(amt = 100, rate = -1, cmt = "depot") |> et(time = 4, amt = 100, rate = -1, cmt = "depot") |>
        et(c(1, 3, 5, 8, 12, 16, 24))),
    modeledDur = list(txt = paste0(.base, "\ndur(depot) <- dd"), p = c(ka = 1.2, k = 0.2, dd = 3),
      ev = et(amt = 100, rate = -2, ii = 2, addl = 2, cmt = "depot") |> et(c(1, 2.5, 4.5, 5.5, 7.5, 10, 14))),
    # modeled infusions into two compartments ending at the same time
    twoCmtSameEnd = list(txt = "d/dt(a) <- -k*a\nd/dt(b) <- -k2*b\nrate(a) <- ra\nrate(b) <- rb",
      p = c(k = 0.3, k2 = 0.5, ra = 10, rb = 15),
      ev = et(amt = 20, rate = -1, cmt = "a") |> et(amt = 30, rate = -1, cmt = "b") |> et(c(1, 3, 5))),
    # the same doses without overlap, as a control
    noOverlap = list(txt = paste0(.base, "\nf(depot) <- fd"), p = c(ka = 1.2, k = 0.2, fd = 0.7),
      ev = et(amt = 100, rate = 10, cmt = "depot") |> et(time = 12, amt = 100, rate = 10, cmt = "depot") |>
        et(c(1, 3, 5, 8, 13, 16, 24)))
  )

  test_that("overlapping infusion windows give the right sensitivities in every driver", {
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p)
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("an ss = 1 fixed infusion overlapped by a regular one into the same compartment", {
    # the ss infusion's stop is pushed (no event-table record), so it has to close
    # the ss window, not the regular one opened after it
    .txtF <- paste0(.base, "\nf(depot) <- fd")
    .pF <- c(ka = 1.2, k = 0.2, fd = 0.7)
    .tt <- c(0.5, 1.5, 2.1, 3, 5, 8, 12, 16, 24)
    .ss <- list(
      rate = et(amt = 100, rate = 40, ss = 1, ii = 24, cmt = "depot") |>
        et(time = 1, amt = 100, rate = 30, cmt = "depot") |> et(.tt),
      dur = et(amt = 100, dur = 2, ss = 1, ii = 24, cmt = "depot") |>
        et(time = 1, amt = 100, dur = 3, cmt = "depot") |> et(.tt)
    )
    for (.nm in names(.ss)) {
      .ref <- .adjFd(.txtF, .ss[[.nm]], .pF)
      for (.m in c("rk4s", "ros4s", "dop853s", "dop853s+ros4s", "liblsodaadj")) {
        # liblsoda's own ss solve does not converge to 1e-13 here
        .ssTol <- if (.m == "liblsodaadj") 1e-8 else 1e-13
        .a <- rxSolveAdjointRk4(.txtF, .ss[[.nm]],
          params = .pF, calcSens = names(.pF), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = .ssTol, ssRtol = .ssTol, maxSS = 50000
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })
})
