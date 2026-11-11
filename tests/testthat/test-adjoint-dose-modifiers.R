rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A modeled alag() shifts the dose that actually lands, F*amt, so its
  # transversality term scales with the bioavailability as well as the dose.
  .faTxt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd\nalag(depot) <- lg"
  .faP <- c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.7)
  .faEv <- et(amt = 100, cmt = "depot") |> et(c(1, 2, 4, 8, 12))

  # closed form for a bolus fd*100 landing at lg (every observation is after it)
  .faKnown <- function(p, tt) {
    .lg <- if ("lg" %in% names(p)) "lg" else "0"
    .depot <- deriv(stats::as.formula(sprintf("~ fd * 100 * exp(-ka * (t - %s))", .lg)), names(p),
      function.arg = c(names(p), "t"))
    .center <- deriv(
      stats::as.formula(sprintf("~ fd * 100 * ka / (ka - k) * (exp(-k * (t - %s)) - exp(-ka * (t - %s)))", .lg, .lg)),
      names(p),
      function.arg = c(names(p), "t")
    )
    .gD <- attr(do.call(.depot, c(as.list(p), list(t = tt))), "gradient")
    .gC <- attr(do.call(.center, c(as.list(p), list(t = tt))), "gradient")
    .ret <- list()
    for (.pn in names(p)) {
      .ret[[paste0("rx__sens_depot_BY_", .pn, "__")]] <- unname(.gD[, .pn])
      .ret[[paste0("rx__sens_center_BY_", .pn, "__")]] <- unname(.gC[, .pn])
    }
    .ret
  }

  test_that("f() and alag() sensitivities match the closed form for every driver with their duals", {
    .tt <- .faEv$time[is.na(.faEv$evid) | .faEv$evid == 0]
    .mods <- list(
      # the dose lands inside the sweep
      fAlag = list(txt = .faTxt, p = .faP),
      # the dose lands at t0, where the backward sweep stops
      f = list(txt = "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd", p = .faP[c("ka", "k", "fd")])
    )
    for (.nm in names(.mods)) {
      .known <- .faKnown(.mods[[.nm]]$p, .tt)
      for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(
          .mods[[.nm]]$txt, .faEv,
          params = .mods[[.nm]]$p, calcSens = names(.mods[[.nm]]$p), method = .meth,
          atol = 1e-10, rtol = 1e-10, hmin = if (.meth %in% c("rk4s", "abs")) 0.001 else 0
        )
        .a <- .a[.a$time %in% .tt, , drop = FALSE]
        for (.cn in names(.known)) {
          expect_lt(max(abs(.a[[.cn]] - .known[[.cn]])), 1e-6, label = paste(.nm, .meth, .cn))
        }
      }
    }
  })

  test_that("the DDE dose-induced breaking-point jump carries the bioavailability", {
    # A dose into a delayed state jumps delay(y, tau) by F*amt at t_dose+tau.
    # The DDE adjoint converges at the step rate (see test-adjoint-discrete.R),
    # so the error must shrink with the step and be small at the finest one.
    .txt <- "d/dt(central) <- -k * delay(central, tau)\nf(central) <- fd"
    .ex <- .rxAdjointExpand(.txt, c("k", "tau"))
    .mod <- rxode2(.ex$text)
    .p <- c(k = 0.3, tau = 2, fd = 0.5)
    .ev <- et(amt = 10, cmt = "central") |> et(seq(0.7, 15, by = 1.7))
    .sb <- function(pp, h) {
      as.data.frame(rxSolve(.mod, .ev, params = pp, method = "rk4s", hmin = h, cores = 1))$central
    }
    .errAt <- function(h) {
      .sc <- as.data.frame(rxSolve(.mod, .ev, params = .p, method = "rk4s", hmin = h, cores = 1))
      .hh <- .p[["tau"]] * 5e-4
      .pp <- .p
      .pm <- .p
      .pp["tau"] <- .pp["tau"] + .hh
      .pm["tau"] <- .pm["tau"] - .hh
      max(abs(.sc[["rx__sens_central_BY_tau__"]] - (.sb(.pp, h) - .sb(.pm, h)) / (2 * .hh)))
    }
    .e1 <- .errAt(0.01)
    .e2 <- .errAt(0.002)
    expect_lt(.e2, .e1)
    expect_lt(.e2, 0.01)
  })

  test_that("abs refuses sensitivities through parameter-dependent infusions and still solves everything else", {
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .msg <- "method='abs' cannot calculate sensitivities through %s that depend on sensitivity parameters; use %s"
    .both <- "method='rk4s' or method='liblsodaadj'"
    .rateEv <- et(amt = 100, cmt = "depot", rate = -1) |> et(c(1, 2, 4, 8, 12))
    .durEv <- et(amt = 100, cmt = "depot", rate = -2) |> et(c(1, 2, 4, 8, 12))
    # abs carries the f() and alag() duals of a bolus, not of an infusion
    .fixEv <- et(amt = 100, cmt = "depot", rate = 40) |> et(c(1, 2, 4, 8, 12))
    .kinds <- list(
      list(txt = "f(depot) <- fd", p = c(fd = 0.7), ev = .fixEv, what = "f()", use = .both),
      list(txt = "alag(depot) <- lg", p = c(lg = 0.7), ev = .fixEv, what = "alag()", use = .both),
      list(txt = "rate(depot) <- r", p = c(r = 40), ev = .rateEv, what = "rate()", use = .both),
      list(txt = "dur(depot) <- dd", p = c(dd = 2.5), ev = .durEv, what = "dur()", use = .both),
      list(txt = "f(depot) <- fd\nalag(depot) <- lg", p = c(fd = 0.7, lg = 0.7), ev = .fixEv, what = "f(), alag()", use = .both),
      list(txt = "f(depot) <- fd\nrate(depot) <- r", p = c(fd = 0.7, r = 40), ev = .rateEv, what = "f(), rate()", use = .both)
    )
    for (.k in .kinds) {
      .p <- c(ka = 1.2, k = 0.14, .k$p)
      expect_error(
        rxSolveAdjointRk4(paste(.base, .k$txt, sep = "\n"), .k$ev, params = .p, calcSens = names(.p), method = "abs"),
        sprintf(.msg, .k$what, .k$use),
        fixed = TRUE
      )
    }
    # NONMEM-style upper-case RATE and DUR columns are infusions too
    .up <- data.frame(ID = 1, TIME = c(0, 1, 2, 4, 8, 12), AMT = c(100, 0, 0, 0, 0, 0),
      EVID = c(1, 0, 0, 0, 0, 0), CMT = "depot")
    .p <- c(ka = 1.2, k = 0.14, fd = 0.7)
    for (.col in c("RATE", "DUR")) {
      .ev <- .up
      .ev[[.col]] <- c(if (.col == "RATE") 40 else 2.5, 0, 0, 0, 0, 0)
      expect_error(
        rxSolveAdjointRk4(paste(.base, "f(depot) <- fd", sep = "\n"), .ev, params = .p, calcSens = names(.p), method = "abs"),
        sprintf(.msg, "f()", .both),
        fixed = TRUE
      )
    }
    # constant modifiers, or ones whose parameter is not a sensitivity target,
    # carry no dual: abs solves them
    .tt <- c(1, 2, 4, 8, 12)
    for (.txt in c("f(depot) <- 0.7\nalag(depot) <- 0.5", "f(depot) <- fd\nalag(depot) <- lg")) {
      .p <- c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.5)
      .known <- .faKnown(.p, .tt)
      .a <- rxSolveAdjointRk4(paste(.base, .txt, sep = "\n"), .faEv, params = .p, calcSens = c("ka", "k"), method = "abs", hmin = 0.001)
      .a <- .a[.a$time %in% .tt, , drop = FALSE]
      for (.cn in grep("_BY_(ka|k)__$", names(.known), value = TRUE)) {
        expect_lt(max(abs(.a[[.cn]] - .known[[.cn]])), 1e-6, label = paste(.txt, .cn))
      }
    }
    # without modeled dosing abs still gives the sensitivities
    .tt <- c(1, 2, 4, 8, 12)
    .known <- .faKnown(c(ka = 1.2, k = 0.14, fd = 1), .tt)
    .a <- rxSolveAdjointRk4(.base, .faEv, params = c(ka = 1.2, k = 0.14), calcSens = c("ka", "k"), method = "abs", hmin = 0.001)
    .a <- .a[.a$time %in% .tt, , drop = FALSE]
    for (.cn in grep("_BY_(ka|k)__$", names(.known), value = TRUE)) {
      expect_lt(max(abs(.a[[.cn]] - .known[[.cn]])), 1e-6, label = .cn)
    }
    # and a plain solve (no sensitivities) of a model with f() and alag() is untouched
    .m <- rxode2(.faTxt)
    expect_equal(
      as.data.frame(rxSolve(.m, .faEv, params = .faP, method = "abs", hmin = 0.001)),
      as.data.frame(rxSolve(.m, .faEv, params = .faP, method = "ab", hmin = 0.001)),
      tolerance = 1e-12
    )
  })

  test_that("f() and alag() duals hold with observations at dose times, several doses, resets and subjects", {
    .fTxt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd"
    .scen <- list(
      # an observation at a dose time reads the post-dose state
      obsAtT0 = list(txt = .fTxt, ev = et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 4)), p = c(ka = 1.2, k = 0.14, fd = 0.5)),
      obsAtDose = list(
        txt = .fTxt, ev = et(amt = 100, cmt = "depot") |> et(amt = 60, cmt = "depot", time = 3) |> et(c(1, 3, 5, 8)),
        p = c(ka = 1.2, k = 0.14, fd = 0.5)
      ),
      # a reset, then a dose observed at its own time (a reset and a dose at
      # the same time, and an observation at a reset, are separate cases)
      afterReset = list(
        txt = .fTxt,
        ev = et(amt = 100, cmt = "depot") |> et(time = 4, evid = 3) |> et(amt = 70, cmt = "depot", time = 5) |> et(c(1, 3, 5, 6, 9)),
        p = c(ka = 1.2, k = 0.14, fd = 0.5)
      ),
      fAlagDoses = list(
        txt = paste(.fTxt, "alag(depot) <- lg", sep = "\n"),
        ev = et(amt = 100, cmt = "depot", ii = 6, addl = 2) |> et(c(1, 2, 5, 7.5, 9, 14)),
        p = c(ka = 1.2, k = 0.14, fd = 0.9, lg = 0.6)
      )
    )
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .fd <- .adjFd(.k$txt, .k$ev, .k$p, names(.k$p))
      for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .meth,
          atol = 1e-12, rtol = 1e-12, hmin = if (.meth %in% c("rk4s", "abs")) 0.001 else 0
        )
        for (.cn in names(.fd)) {
          expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = paste(.nm, .meth, .cn))
        }
      }
    }
    # two subjects with different F
    .ev <- et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 4, 8)) |> et(id = 1:2)
    .pd <- data.frame(id = 1:2, ka = 1.2, k = 0.14, fd = c(0.5, 0.9))
    for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
      .a <- rxSolveAdjointRk4(.fTxt, .ev, params = .pd, calcSens = c("ka", "k", "fd"), method = .meth,
        atol = 1e-12, rtol = 1e-12, hmin = if (.meth %in% c("rk4s", "abs")) 0.001 else 0)
      for (.id in 1:2) {
        .fd <- .adjFd(.fTxt, et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 4, 8)), unlist(.pd[.id, -1]), c("ka", "k", "fd"))
        .ai <- .a[.a$id == .id, ]
        for (.cn in names(.fd)) {
          expect_lt(max(abs(.ai[[.cn]] - .fd[[.cn]])), 1e-6, label = paste("subject", .id, .meth, .cn))
        }
      }
    }
  })

  .infBase <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
  .infTt <- c(0.5, 1, 1.5, 2, 4, 5.5, 8)
  .infMeths <- c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")
  .infCheck <- function(scen) {
    for (.nm in names(scen)) {
      .k <- scen[[.nm]]
      .txt <- paste(.infBase, .k$txt, sep = "\n")
      .ev <- .k$ev |> et(.infTt)
      .fd <- .adjFd(.txt, .ev, .k$p, names(.k$p))
      for (.meth in .infMeths) {
        .a <- rxSolveAdjointRk4(.txt, .ev,
          params = .k$p, calcSens = names(.k$p), method = .meth,
          atol = 1e-11, rtol = 1e-11, hmin = if (.meth == "rk4s") 0.001 else 0
        )
        for (.cn in names(.fd)) {
          expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = paste(.nm, .meth, .cn))
        }
      }
    }
  }

  test_that("f() with a modeled rate() or dur() infusion carries its dual in every driver", {
    # F scales the rate of a dur() infusion and the duration of a rate() one
    .infCheck(list(
      fRate = list(txt = "f(depot) <- fd\nrate(depot) <- r", p = c(ka = 1.2, k = 0.14, fd = 0.7, r = 40),
        ev = et(amt = 100, cmt = "depot", rate = -1)),
      fDur = list(txt = "f(depot) <- fd\ndur(depot) <- dd", p = c(ka = 1.2, k = 0.14, fd = 0.7, dd = 2.5),
        ev = et(amt = 100, cmt = "depot", rate = -2))
    ))
  })

  test_that("alag() on an infusion shifts its whole window in every driver", {
    .infCheck(list(
      fixRate = list(txt = "alag(depot) <- lg", p = c(ka = 1.2, k = 0.14, lg = 0.7),
        ev = et(amt = 100, cmt = "depot", rate = 40)),
      fixDurTwice = list(txt = "alag(depot) <- lg", p = c(ka = 1.2, k = 0.14, lg = 0.7),
        ev = et(amt = 100, cmt = "depot", dur = 2) |> et(amt = 50, time = 4, cmt = "depot", dur = 2)),
      rate = list(txt = "alag(depot) <- lg\nrate(depot) <- r", p = c(ka = 1.2, k = 0.14, lg = 0.7, r = 40),
        ev = et(amt = 100, cmt = "depot", rate = -1)),
      dur = list(txt = "alag(depot) <- lg\ndur(depot) <- dd", p = c(ka = 1.2, k = 0.14, lg = 0.7, dd = 2.5),
        ev = et(amt = 100, cmt = "depot", rate = -2)),
      fRateTwice = list(txt = "alag(depot) <- lg\nf(depot) <- fd\nrate(depot) <- r",
        p = c(ka = 1.2, k = 0.14, lg = 0.7, fd = 0.7, r = 40),
        ev = et(amt = 100, cmt = "depot", rate = -1) |> et(amt = 50, time = 4, cmt = "depot", rate = -1))
    ))
  })

  test_that("f() with a fixed-rate or fixed-duration infusion carries its dual in every driver", {
    # F stretches a fixed-rate window and scales a fixed-duration rate
    .infCheck(list(
      fixRateTwice = list(txt = "f(depot) <- fd", p = c(ka = 1.2, k = 0.14, fd = 0.7),
        ev = et(amt = 100, cmt = "depot", rate = 40) |> et(amt = 50, time = 4, cmt = "depot", rate = 40)),
      fixDurTwice = list(txt = "f(depot) <- fd", p = c(ka = 1.2, k = 0.14, fd = 0.7),
        ev = et(amt = 100, cmt = "depot", dur = 2.5) |> et(amt = 50, time = 4, cmt = "depot", dur = 2)),
      fixRateLag = list(txt = "f(depot) <- fd\nalag(depot) <- lg", p = c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.7),
        ev = et(amt = 100, cmt = "depot", rate = 40))
    ))
  })

  test_that("alag() moves a replace or multiply on its compartment in every driver", {
    # the jump moves with the lag, adding mu += dlag/dp * lambda+^T (G f(x-) - f(x+))
    .lTxt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nalag(depot) <- lag"
    .nlTxt <- "d/dt(depot) <- -ka*depot/(1 + depot/40)\nd/dt(center) <- ka*depot/(1 + depot/40) - k*center\nalag(depot) <- lag"
    .lP <- c(ka = 1.2, k = 0.2, lag = 0.4)
    .scen <- list(
      multiply = list(.lTxt, et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |> et(c(1, 2, 4, 6))),
      replace = list(.lTxt, et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 10, cmt = "depot") |> et(c(1, 2, 4, 6))),
      multiplyBeforeDose = list(.lTxt, et(time = 0.1, evid = 6, amt = 0.5, cmt = "depot") |> et(amt = 100, cmt = "depot") |> et(c(1, 2, 4, 6))),
      multiplyInLag = list(.lTxt, et(amt = 100, cmt = "depot") |> et(time = 3, amt = 50, cmt = "depot") |>
        et(time = 3.2, evid = 6, amt = 0.5, cmt = "depot") |> et(c(1, 2, 4, 6))),
      # nonlinear f: the term uses f at both states, not a linearization
      nonlinear = list(.nlTxt, et(time = 0, evid = 5, amt = 60, cmt = "depot") |> et(time = 1, evid = 6, amt = 0.5, cmt = "depot") |>
        et(time = 2, evid = 5, amt = 30, cmt = "depot") |> et(c(0.2, 1.6, 3, 5)))
    )
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k[[1]], .k[[2]], .lP)
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.k[[1]], .k[[2]], params = .lP, calcSens = names(.lP), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
        expect_equal(nrow(.a), length(.ref[[1]]), label = paste(.nm, .m, "rows"))
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
    # the liblsodaadj discrete forward replay carries the same term
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    .out <- utils::capture.output(
      rxSolveAdjointRk4(.lTxt, .scen$multiplyInLag[[2]], params = .lP, calcSens = names(.lP), method = "liblsodaadj", cores = 1),
      type = "message"
    )
    .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_lt(as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line)), 1e-8)
  })

  test_that("f() on a replaced or multiplied compartment carries its dual in every driver", {
    # the forward replaces with F*amt and multiplies by F*amt, so a replace adds
    # lambda_c*amt*dF and a multiply lambda_c*x-_c*amt*dF
    .b <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .tF <- paste0(.b, "\nf(depot) <- fd")
    .tFL <- paste0(.tF, "\nalag(depot) <- lag")
    .nlF <- "d/dt(depot) <- -ka*depot/(1 + depot/40)\nd/dt(center) <- ka*depot/(1 + depot/40) - k*center\nf(depot) <- fd"
    .pF <- c(ka = 1.2, k = 0.2, fd = 0.7)
    .pFL <- c(.pF, lag = 0.4)
    .scen <- list(
      multiply = list(.tF, .pF, et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |> et(c(1, 4, 6))),
      replace = list(.tF, .pF, et(amt = 100, cmt = "depot") |> et(time = 3, evid = 5, amt = 10, cmt = "depot") |> et(c(1, 4, 6))),
      replaceAtLastRecord = list(.tF, .pF, et(amt = 100, cmt = "depot") |> et(c(1, 3)) |>
        et(time = 3, evid = 5, amt = 10, cmt = "depot")),
      lagDoseMultiply = list(.tFL, .pFL, et(amt = 100, cmt = "depot") |> et(time = 3, amt = 80, cmt = "depot") |>
        et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |> et(c(1, 2, 4, 6))),
      lagMultiplyDose = list(.tFL, .pFL, et(amt = 100, cmt = "depot") |> et(time = 3, evid = 6, amt = 0.5, cmt = "depot") |>
        et(time = 3, amt = 80, cmt = "depot") |> et(c(1, 2, 4, 6))),
      nonlinear = list(.nlF, .pF, et(amt = 100, cmt = "depot") |> et(time = 1, evid = 6, amt = 0.5, cmt = "depot") |>
        et(time = 2, evid = 5, amt = 30, cmt = "depot") |> et(c(0.5, 1.6, 3, 5)))
    )
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k[[1]], .k[[3]], .k[[2]])
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.k[[1]], .k[[3]], params = .k[[2]], calcSens = names(.k[[2]]), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 1e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
    # the liblsodaadj discrete forward replay carries the same terms
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    .out <- utils::capture.output(
      rxSolveAdjointRk4(.tFL, .scen$lagMultiplyDose[[3]], params = .pFL, calcSens = names(.pFL), method = "liblsodaadj", cores = 1),
      type = "message"
    )
    .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_lt(as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line)), 1e-8)
  })

  test_that("alag() on a nonlinear dosed compartment uses f at both states in every driver", {
    # the dose-time transversality is lambda^T (f(x-) - f(x+)), not its F_X
    # linearization, which is off when the dosed compartment is nonlinear
    .nl <- "d/dt(depot) <- -ka*depot/(1 + depot/40)\nd/dt(center) <- ka*depot/(1 + depot/40) - k*center\nalag(depot) <- lag"
    .mm <- "d/dt(center) <- -vm*center/(km + center)\nf(center) <- fc\nalag(center) <- lc"
    .scen <- list(
      saturable = list(.nl, c(ka = 1.2, k = 0.2, lag = 0.4), et(amt = 100, cmt = "depot") |> et(c(0.5, 1.6, 3, 5))),
      saturableTwice = list(.nl, c(ka = 1.2, k = 0.2, lag = 0.4),
        et(amt = 100, cmt = "depot") |> et(time = 2, amt = 30, cmt = "depot") |> et(c(0.5, 1.6, 3, 5))),
      michaelisMenten = list(.mm, c(vm = 20, km = 10, fc = 0.8, lc = 0.5), et(amt = 100, cmt = "center") |> et(c(1, 2, 4, 6)))
    )
    for (.nm in names(.scen)) {
      .k <- .scen[[.nm]]
      .ref <- .adjFd(.k[[1]], .k[[3]], .k[[2]])
      for (.m in c("rk4s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj", "abs")) {
        .a <- rxSolveAdjointRk4(.k[[1]], .k[[3]], params = .k[[2]], calcSens = names(.k[[2]]), method = .m,
          atol = 1e-10, rtol = 1e-10, hmin = if (.m %in% c("rk4s", "abs")) 0.001 else 0)
        for (.cn in names(.ref)) {
          expect_lt(max(abs(.a[[.cn]] - .ref[[.cn]])), 2e-6, label = paste(.nm, .m, .cn))
        }
      }
    }
    # the liblsodaadj discrete forward replay carries the same term
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    .out <- utils::capture.output(
      rxSolveAdjointRk4(.mm, .scen$michaelisMenten[[3]], params = .scen$michaelisMenten[[2]],
        calcSens = names(.scen$michaelisMenten[[2]]), method = "liblsodaadj", cores = 1),
      type = "message"
    )
    .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
    expect_length(.line, 1L)
    expect_lt(as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line)), 1e-8)
  })

  test_that("the liblsodaadj self-check replay agrees with the adjoint for f() and alag()", {
    withr::local_envvar(RX_LSADJ_SELFCHECK = "1")
    .out <- utils::capture.output(
      rxSolveAdjointRk4(.faTxt, .faEv, params = .faP, calcSens = names(.faP), method = "liblsodaadj", cores = 1),
      type = "message"
    )
    .line <- grep("max\\|adjoint - discrete_forward_sens\\|", .out, value = TRUE)
    expect_length(.line, 1L)
    .err <- as.numeric(sub("^.*discrete_forward_sens\\| = ([^ ]+) .*$", "\\1", .line))
    expect_lt(.err, 1e-8)
  })
})
