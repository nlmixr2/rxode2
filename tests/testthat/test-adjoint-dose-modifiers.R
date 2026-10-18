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
      # abs has no f()/alag() dual
      for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")) {
        .a <- rxSolveAdjointRk4(
          .mods[[.nm]]$txt, .faEv,
          params = .mods[[.nm]]$p, calcSens = names(.mods[[.nm]]$p), method = .meth,
          atol = 1e-10, rtol = 1e-10, hmin = if (.meth == "rk4s") 0.001 else 0
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

  test_that("abs refuses sensitivities through parameter-dependent dosing and still solves everything else", {
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .msg <- "method='abs' cannot calculate sensitivities through %s that depend on sensitivity parameters; use %s"
    .both <- "method='rk4s' or method='liblsodaadj'"
    .rateEv <- et(amt = 100, cmt = "depot", rate = -1) |> et(c(1, 2, 4, 8, 12))
    .durEv <- et(amt = 100, cmt = "depot", rate = -2) |> et(c(1, 2, 4, 8, 12))
    .kinds <- list(
      list(txt = "f(depot) <- fd", p = c(fd = 0.7), ev = .faEv, what = "f()", use = .both),
      list(txt = "alag(depot) <- lg", p = c(lg = 0.7), ev = .faEv, what = "alag()", use = .both),
      list(txt = "rate(depot) <- r", p = c(r = 40), ev = .rateEv, what = "rate()", use = .both),
      list(txt = "dur(depot) <- dd", p = c(dd = 2.5), ev = .durEv, what = "dur()", use = .both),
      list(txt = "f(depot) <- fd\nalag(depot) <- lg", p = c(fd = 0.7, lg = 0.7), ev = .faEv, what = "f(), alag()", use = .both),
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

  # central FD of rxSolve(): dop853 at a relative step of 1e-5
  .doseFd <- function(txt, ev, p, cs) {
    .m <- rxode2(txt)
    .ret <- list()
    for (.pn in cs) {
      .h <- max(abs(p[[.pn]]), 1) * 1e-5
      .pu <- p
      .pd <- p
      .pu[.pn] <- .pu[.pn] + .h
      .pd[.pn] <- .pd[.pn] - .h
      .u <- as.data.frame(rxSolve(.m, ev, params = .pu, method = "dop853", atol = 1e-13, rtol = 1e-13))
      .d <- as.data.frame(rxSolve(.m, ev, params = .pd, method = "dop853", atol = 1e-13, rtol = 1e-13))
      for (.s in rxStateOde(.m)) {
        .ret[[paste0("rx__sens_", .s, "_BY_", .pn, "__")]] <- (.u[[.s]] - .d[[.s]]) / (2 * .h)
      }
    }
    .ret
  }

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
      .fd <- .doseFd(.k$txt, .k$ev, .k$p, names(.k$p))
      for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")) {
        .a <- rxSolveAdjointRk4(.k$txt, .k$ev,
          params = .k$p, calcSens = names(.k$p), method = .meth,
          atol = 1e-12, rtol = 1e-12, hmin = if (.meth == "rk4s") 0.001 else 0
        )
        for (.cn in names(.fd)) {
          expect_lt(max(abs(.a[[.cn]] - .fd[[.cn]])), 1e-6, label = paste(.nm, .meth, .cn))
        }
      }
    }
    # two subjects with different F
    .ev <- et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 4, 8)) |> et(id = 1:2)
    .pd <- data.frame(id = 1:2, ka = 1.2, k = 0.14, fd = c(0.5, 0.9))
    for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")) {
      .a <- rxSolveAdjointRk4(.fTxt, .ev, params = .pd, calcSens = c("ka", "k", "fd"), method = .meth,
        atol = 1e-12, rtol = 1e-12, hmin = if (.meth == "rk4s") 0.001 else 0)
      for (.id in 1:2) {
        .fd <- .doseFd(.fTxt, et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 4, 8)), unlist(.pd[.id, -1]), c("ka", "k", "fd"))
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
      .fd <- .doseFd(.txt, .ev, .k$p, names(.k$p))
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
