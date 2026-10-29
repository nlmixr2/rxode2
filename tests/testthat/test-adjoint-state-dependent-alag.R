rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A lag time that depends on the state is read by the forward solve when it
  # reaches the dose's raw time, from the state there (after the events recorded
  # before it), so the dose's landing moves with that state: the transpose adds
  # s * d(alag)/dx at the raw time, s the weight of the dose's lag term.  Each
  # case has a record at every lagged dose's raw time, so the forward reads the
  # lag there.  Infusions are not compared: the forward solve does not move a
  # fixed-dur or modeled infusion's stop with its start's refreshed lag, and
  # reads a fixed-rate infusion's lag only at the start of the solve.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .lagD <- "\nalag(depot) <- lg + 0.01*center"
  .lagC <- "\nalag(center) <- lg + 0.01*depot"
  .p <- c(ka = 1.2, k = 0.2, lg = 0.4)
  .o <- c(0.5, 1.8, 3, 4.5, 6)
  .d0 <- et(amt = 100, cmt = "depot")
  .scen <- list(
    # an observation at the second dose's raw time
    bolus = list(txt = paste0(.base, .lagD), ev = .d0 |> et(time = 1.8, amt = 80, cmt = "depot") |> et(.o)),
    # the lag of the dosed compartment's own state
    bolusSelf = list(txt = paste0(.base, "\nalag(depot) <- lg + 0.01*depot"),
      ev = .d0 |> et(time = 1.8, amt = 80, cmt = "depot") |> et(.o)),
    # a dose into the state the lag reads, at the raw time: the read follows it
    afterDose = list(txt = paste0(.base, .lagD),
      ev = .d0 |> et(time = 1.8, amt = 30, cmt = "center") |> et(time = 1.8, amt = 80, cmt = "depot") |> et(setdiff(.o, 1.8))),
    afterDoseObs = list(txt = paste0(.base, .lagD),
      ev = .d0 |> et(time = 1.8, amt = 30, cmt = "center") |> et(time = 1.8, amt = 80, cmt = "depot") |> et(.o)),
    replace = list(txt = paste0(.base, .lagC), ev = .d0 |> et(time = 1.8, evid = 5, amt = 20, cmt = "center") |> et(.o)),
    multiply = list(txt = paste0(.base, .lagC), ev = .d0 |> et(time = 1.8, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
    # two lagged doses, each read at its own raw time
    two = list(txt = paste0(.base, .lagD),
      ev = .d0 |> et(time = 1.8, amt = 80, cmt = "depot") |> et(time = 3, amt = 50, cmt = "depot") |> et(.o))
  )
  .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")

  test_that("a lag that depends on the state is exact in every driver", {
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .p, method = "liblsoda", maxsteps = 1e6)
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .p, calcSens = names(.p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        # abs is a fixed-step method: its own solution is about 1e-5 off here
        .tol <- switch(.m, abs = 5e-5, cvodesadj = 2e-6, 1e-6)
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .tol, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("the d(alag)/dx block is emitted only for a lag that depends on a state", {
    .sens <- c("ka", "k", "lg")
    expect_identical(.rxAdjointExpand(paste0(.base, "\nalag(depot) <- lg"), .sens)$dlagxOff, -1L)
    # after the dx0 and dF/dx blocks, at the lhs the solve looks it up by name
    .txt <- paste0(.base, .lagD, "\nf(depot) <- bio/(1 + center/50)\ndepot(0) <- d0")
    .ex <- .rxAdjointExpand(.txt, c(.sens, "bio", "d0"))
    .lhs <- rxModelVars(rxode2(.ex$text))$lhs
    expect_identical(.ex$dlagxOff, which(.lhs == "rx__adjDlagX_0_0__") - 1L)
    expect_identical(.ex$dlagxOff, .ex$dfxOff + 4L)
  })
})
