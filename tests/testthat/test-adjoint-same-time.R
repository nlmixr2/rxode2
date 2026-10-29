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
    # a replace with 0 or a multiply by 0 carries no f() term of its own
    replaceDose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 0, cmt = "depot") |>
      et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 4, 6))),
    multiplyDose = list(txt = .txtF, p = .pF, ev = et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0, cmt = "depot") |>
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

  test_that("an ss = 1 dose replaces the state of the jumps recorded before it in liblsodaadj", {
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
      .a <- rxSolveAdjointRk4(.base, .ss[[.nm]], params = .p0, calcSens = names(.p0), method = "liblsodaadj",
        atol = 1e-10, rtol = 1e-10, ssAtol = 1e-13, ssRtol = 1e-13, maxSS = 50000)
      expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, "rows"))
      for (.cn in names(.ref)) {
        expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .cn))
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
