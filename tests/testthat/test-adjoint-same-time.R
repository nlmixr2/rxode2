rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # Several events at one time are applied in record order (multiply, replace,
  # then dose); the backward sweep must transpose them in reverse, each with
  # the costate of its own post-event state.
  .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .txtF <- paste0(.base, "\nf(depot) <- bio")
  .p0 <- c(ka = 1.2, k = 0.2)
  .pF <- c(ka = 1.2, k = 0.2, bio = 0.8)
  .scen <- list(
    twoMultiplyOneCmt = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 1, evid = 6, amt = 0.5, cmt = "depot") |>
      et(time = 1, evid = 6, amt = 0.8, cmt = "depot") |> et(c(0.5, 2, 3))),
    twoMultiplyTwoCmt = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 1, evid = 6, amt = 0.5, cmt = "depot") |>
      et(time = 1, evid = 6, amt = 0.8, cmt = "center") |> et(c(0.5, 2, 3))),
    replaceMultiply = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 1, evid = 5, amt = 30, cmt = "center") |>
      et(time = 1, evid = 6, amt = 0.5, cmt = "depot") |> et(c(0.5, 2, 3))),
    multiplyReplaceOneCmt = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 1, evid = 6, amt = 0.5, cmt = "center") |>
      et(time = 1, evid = 5, amt = 30, cmt = "center") |> et(c(0.5, 2, 3))),
    # f() also scales the replace value and the multiply factor
    replaceDose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 10, cmt = "depot") |>
      et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 4, 6))),
    multiplyDose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |>
      et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 4, 6))),
    multiplyDoseT0 = list(txt = .txtF, p = .pF, ev = et(time = 0, evid = 6, amt = 3, cmt = "depot") |>
      et(amt = 100, cmt = "depot") |> et(c(1, 3))),
    twoDoses = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot") |>
      et(time = 3, amt = 50, cmt = "depot") |> et(c(1, 4, 6))),
    resetTwoDoses = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 4, amt = 80, cmt = "depot") |>
      et(time = 3, amt = 50, cmt = "depot") |> et(c(1, 4, 6)))
  )

  test_that("events at one time transpose in turn in every driver", {
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p)
      .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("an ss = 1 dose replaces the state of the jumps recorded before it", {
    # the jumps at the ss time sort before the ss dose, which resets the state to
    # its steady state, so they do not reach later observations
    .ss <- list(
      bolusMultiply0 = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 4, 8)),
      bolusReplace0 = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, evid = 5, amt = 30, cmt = "center") |> et(c(1, 4, 8)),
      infusionMultiply0 = et(amt = 100, rate = 20, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 4, 8)),
      infusionMultiply10 = et(amt = 100, cmt = "depot") |> et(time = 10, amt = 100, rate = 20, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 10, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 5, 11, 14, 18)),
      bolusMultiply5 = et(amt = 100, cmt = "depot") |> et(time = 5, amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 5, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 4, 6, 9)),
      resetBolus5 = et(amt = 100, cmt = "depot") |> et(time = 5, evid = 3) |>
        et(time = 5, amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(c(1, 4, 6, 9))
    )
    for (.nm in names(.ss)) {
      .ref <- .adjFd(.base, .ss[[.nm]], .p0)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(.base, .ss[[.nm]], params = .p0, calcSens = names(.p0), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("events closer together than 1e-8 stay distinct in every driver", {
    # distinct times closer than the old absolute matching tolerances (1e-8 in
    # liblsodaadj, 1e-9 in abs) must not be merged into one event time
    .near <- function(d) list(
      multiplyDose = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |>
        et(time = 3 + d, amt = 80, cmt = "depot") |> et(c(1, 4, 6))),
      doseMultiply = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot") |>
        et(time = 3 + d, evid = 6, amt = 0.5, cmt = "depot") |> et(c(1, 4, 6))),
      doseMultiplyF = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot") |>
        et(time = 3 + d, evid = 6, amt = 0.5, cmt = "center") |> et(c(1, 4, 6))),
      multiplyBeforeLastObs = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
        et(time = 3 - d, evid = 6, amt = 0.5, cmt = "depot"))
    )
    .cases <- c(.near(5e-9), stats::setNames(.near(5e-10), paste0(names(.near(5e-10)), "5e-10")))
    for (.nm in names(.cases)) {
      .k <- .cases[[.nm]]
      .ref <- .adjFd(.k$txt, .k$ev, .k$p)
      for (.m in c("rk4s", "ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
        )
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("events a few ulps apart at large times stay distinct in every driver", {
    # the matching tolerance scales with the time, so it must stay at the forward
    # solve's own (2 ulp) rather than a fixed fraction of the time
    .far <- function(t0, d) list(
      obsDose = list(txt = .txtF, p = .pF, ev = et(time = t0, amt = 100, cmt = "depot") |> et(t0 + c(1, 3, 6)) |>
        et(time = t0 + 3 + d, amt = 80, cmt = "depot")),
      multiplyDose = list(txt = .base, p = .p0, ev = et(time = t0, amt = 100, cmt = "depot") |>
        et(time = t0 + 3, evid = 6, amt = 0.5, cmt = "depot") |> et(time = t0 + 3 + d, amt = 80, cmt = "depot") |>
        et(t0 + c(1, 4, 6))),
      doseMultiplyF = list(txt = .txtF, p = .pF, ev = et(time = t0, amt = 100, cmt = "depot") |>
        et(time = t0 + 3, amt = 80, cmt = "depot") |> et(time = t0 + 3 + d, evid = 6, amt = 0.5, cmt = "center") |>
        et(t0 + c(1, 4, 6)))
    )
    # at t = 1e4 the finite difference itself is good to about 2e-6
    .grid <- list(list(t0 = 100, d = 2e-9, tol = 1e-6, meths = c("rk4s", "ros4s", "liblsodaadj", "cvodesadj", "abs")),
                  list(t0 = 1e4, d = 1e-7, tol = 1e-5, meths = c("rk4s", "ros4s", "liblsodaadj", "cvodesadj")))
    for (.g in .grid) {
      .cases <- .far(.g$t0, .g$d)
      for (.nm in names(.cases)) {
        .k <- .cases[[.nm]]
        .ref <- .adjFd(.k$txt, .k$ev, .k$p)
        for (.m in .g$meths) {
          .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
            params = .k$p, calcSens = names(.k$p), method = .m,
            atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0
          )
          expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .g$t0, .m, "rows"))
          for (.cn in names(.ref)) {
            expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .g$tol, label = paste(.nm, .g$t0, .m, .cn))
          }
        }
      }
    }
  })

  test_that("abs groups close events as its forward stepped", {
    # abs takes no step across an interval within its own step tolerance
    # (about 1.5e-11 * |t|), but that interval is still a call, so the events at
    # its two ends stay apart; liblsodaadj and cvodesadj are the controls
    .sc <- function(t0, d, p) list(
      multiplyDose = list(txt = .base, p = p, ev = et(time = t0, amt = 100, cmt = "depot") |>
        et(time = t0 + 3, evid = 6, amt = 0.5, cmt = "depot") |> et(time = t0 + 3 + d, amt = 80, cmt = "depot") |>
        et(t0 + c(1, 4, 6))),
      doseObsAfter = list(txt = .base, p = p, ev = et(time = t0, amt = 100, cmt = "depot") |>
        et(time = t0 + 3, amt = 80, cmt = "depot") |> et(t0 + c(1, 3 + d, 6))),
      replaceDose = list(txt = .base, p = p, ev = et(time = t0, amt = 100, cmt = "depot") |>
        et(time = t0 + 3, evid = 5, amt = 30, cmt = "center") |> et(time = t0 + 3 + d, amt = 80, cmt = "depot") |>
        et(t0 + c(1, 4, 6))),
      resetDose = list(txt = .base, p = p, ev = et(time = t0, amt = 100, cmt = "depot") |> et(time = t0 + 3, evid = 3) |>
        et(time = t0 + 3 + d, amt = 80, cmt = "depot") |> et(t0 + c(1, 4, 6))),
      dosesLagged = list(txt = paste0(.base, "\nalag(depot) <- lg"), p = c(p, lg = 0.5),
        ev = et(time = t0, amt = 100, cmt = "depot") |> et(time = t0 + 3, amt = 80, cmt = "depot") |>
          et(time = t0 + 3 + d, amt = 50, cmt = "depot") |> et(t0 + c(1, 4.5, 6)))
    )
    .slow <- c(ka = 0.05, k = 0.02)
    # at t = 1e4 the finite difference and cvodesadj are good to about 1e-4
    .grid <- list(list(1e4, 1e-7, .slow, 5e-4), list(100, 1e-9, .slow, 1e-5), list(1, 1e-11, .slow, 1e-5),
                  list(1, 1e-12, .slow, 1e-5), list(100, 1e-13, .slow, 1e-5), list(100, 3e-11, .p0, 1e-5))
    for (.g in .grid) {
      .cases <- .sc(.g[[1]], .g[[2]], .g[[3]])
      if (identical(.g[[3]], .slow)) .cases <- .cases[c("multiplyDose", "doseObsAfter")]
      for (.nm in names(.cases)) {
        .k <- .cases[[.nm]]
        .ref <- .adjFd(.k$txt, .k$ev, .k$p, method = "liblsoda", maxsteps = 1e6)
        for (.m in c("abs", "liblsodaadj", "cvodesadj")) {
          .a <- rxSolveAdjointRk4(.k$txt, .k$ev, params = .k$p, calcSens = names(.k$p), method = .m,
            atol = 1e-10, rtol = 1e-10, hmin = if (.m == "abs") 0.001 else 0)
          for (.cn in names(.ref)) {
            expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), .g[[4]], label = paste(.nm, .g[[1]], .g[[2]], .m, .cn))
          }
        }
      }
    }
  })

  test_that("same-time events around continuous, ss = 2 and repeated ss doses", {
    # a continuous ss infusion replaces the state like an ss bolus; an ss = 2 dose
    # adds its regimen at its own position among the step's events; the last of
    # several ss = 1 doses at one time sets the steady state; a plain dose after an
    # ss dose is not part of its period
    .o <- c(1, 4, 6, 9, 14)
    .k <- list(
      contMultiply0 = list(txt = .base, p = .p0, ev = et(amt = 0, rate = 20, ss = 1, cmt = "depot") |>
        et(time = 0, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
      contReplace0 = list(txt = .base, p = .p0, ev = et(amt = 0, rate = 20, ss = 1, cmt = "depot") |>
        et(time = 0, evid = 5, amt = 30, cmt = "center") |> et(.o)),
      contMultiply5 = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |>
        et(time = 5, amt = 0, rate = 20, ss = 1, cmt = "depot") |> et(time = 5, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
      resetCont5 = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 5, evid = 3) |>
        et(time = 5, amt = 0, rate = 20, ss = 1, cmt = "depot") |> et(.o)),
      twoSs0 = list(txt = .base, p = .p0, ev = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, amt = 50, ii = 12, ss = 1, cmt = "center") |> et(.o)),
      twoSsF0 = list(txt = .txtF, p = .pF, ev = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, amt = 50, ii = 12, ss = 1, cmt = "depot") |> et(.o)),
      ssDose0 = list(txt = .txtF, p = .pF, ev = et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 0, amt = 50, cmt = "depot") |> et(.o)),
      ssDose5 = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 5, amt = 100, ii = 12, ss = 1, cmt = "depot") |>
        et(time = 5, amt = 50, cmt = "center") |> et(.o)),
      # liblsodaadj does not carry ss = 2
      ss2Multiply5 = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |>
        et(time = 5, amt = 100, ii = 12, ss = 2, cmt = "depot") |> et(time = 5, evid = 6, amt = 0.5, cmt = "center") |> et(.o)),
      ss2Reset5 = list(txt = .base, p = .p0, ev = et(amt = 100, cmt = "depot") |> et(time = 5, evid = 3) |>
        et(time = 5, amt = 100, ii = 12, ss = 2, cmt = "depot") |> et(.o)),
      ss2Multiply0 = list(txt = .base, p = .p0, ev = et(amt = 100, ii = 12, ss = 2, cmt = "depot") |>
        et(time = 0, evid = 6, amt = 0.5, cmt = "center") |> et(.o))
    )
    for (.nm in names(.k)) {
      .c <- .k[[.nm]]
      .ref <- .adjFd(.c$txt, .c$ev, .c$p)
      .meths <- c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s")
      if (!grepl("^ss2", .nm)) .meths <- c(.meths, "liblsodaadj")
      for (.m in .meths) {
        .a <- rxSolveAdjointRk4(.c$txt, .c$ev, params = .c$p, calcSens = names(.c$p), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("an ss = 1 bolus and a continuous ss = 1 infusion at one time", {
    # both set the steady state together, in either record order, at the first
    # time or later (the infusion into the dosed cmt or another)
    .o <- c(1, 2, 4, 6, 9, 14)
    .cont <- function(t, cmt) et(time = t, amt = 0, rate = 20, ss = 1, cmt = cmt)
    .bol <- function(t) et(time = t, amt = 100, ii = 12, ss = 1, cmt = "depot")
    .k <- list(
      infusionFirst = .cont(0, "depot") |> et(time = 0, amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(.o),
      bolusFirst = .bol(0) |> et(time = 0, amt = 0, rate = 20, ss = 1, cmt = "depot") |> et(.o),
      interior = et(amt = 100, cmt = "depot") |> et(time = 5, amt = 0, rate = 20, ss = 1, cmt = "depot") |>
        et(time = 5, amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(.o),
      interiorCenter = et(amt = 100, cmt = "depot") |> et(time = 5, amt = 0, rate = 20, ss = 1, cmt = "center") |>
        et(time = 5, amt = 100, ii = 12, ss = 1, cmt = "depot") |> et(.o)
    )
    for (.nm in names(.k)) {
      .ref <- .adjFd(.base, .k[[.nm]], .p0)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s", "dop853s+ros4s", "liblsodaadj")) {
        .a <- rxSolveAdjointRk4(.base, .k[[.nm]], params = .p0, calcSens = names(.p0), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m == "rk4s") 0.001 else 0,
          ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
  })

  test_that("the liblsodaadj self-check replay applies same-time events and the initial conditions", {
    # a parameter-dependent initial condition, a reset that restores it, and a
    # replace with a dose at one time
    .txtIc <- paste0(.txtF, "\ncenter(0) <- c0")
    .pIc <- c(.pF, c0 = 5)
    .evs <- list(
      reset = et(amt = 100, cmt = "depot") |> et(time = 2, evid = 3) |> et(time = 2, amt = 60, cmt = "depot") |> et(c(1, 3, 5)),
      replaceDose = .scen$replaceDose$ev
    )
    # compile the model first: compiling inside capture.output() loses the sink
    rxSolveAdjointRk4(.txtIc, .evs$reset, params = .pIc, calcSens = names(.pIc), method = "liblsodaadj")
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    for (.nm in names(.evs)) {
      .out <- utils::capture.output(
        rxSolveAdjointRk4(.txtIc, .evs[[.nm]], params = .pIc, calcSens = names(.pIc), method = "liblsodaadj", cores = 1),
        type = "message"
      )
      .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
      expect_length(.line, 1L)
      .err <- as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line))
      expect_lt(.err, 1e-8, label = .nm)
    }
  })
})
