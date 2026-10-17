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
    .errAt <- function(h) {
      .sc <- as.data.frame(rxSolve(.mod, .ev, params = .p, method = "rk4s", hmin = h, cores = 1))
      .sb <- function(pp) {
        as.data.frame(rxSolve(.mod, .ev, params = pp, method = "rk4s", hmin = h, cores = 1))$central
      }
      .hh <- .p[["tau"]] * 5e-4
      .pp <- .p
      .pm <- .p
      .pp["tau"] <- .pp["tau"] + .hh
      .pm["tau"] <- .pm["tau"] - .hh
      max(abs(.sc[["rx__sens_central_BY_tau__"]] - (.sb(.pp) - .sb(.pm)) / (2 * .hh)))
    }
    .e1 <- .errAt(0.01)
    .e2 <- .errAt(0.002)
    expect_lt(.e2, .e1)
    expect_lt(.e2, 0.01)
  })

  test_that("abs refuses sensitivities for modeled dosing and still solves everything else", {
    .base <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center"
    .msg <- "method='abs' cannot calculate sensitivities for a model with %s; use method='rk4s' or method='liblsodaadj'"
    .kinds <- list(
      list(txt = "f(depot) <- fd", p = c(fd = 0.7), ev = .faEv, what = "f()"),
      list(txt = "alag(depot) <- lg", p = c(lg = 0.7), ev = .faEv, what = "alag()"),
      list(txt = "rate(depot) <- r", p = c(r = 40), ev = et(amt = 100, cmt = "depot", rate = -1) |> et(c(1, 2, 4, 8, 12)), what = "rate()"),
      list(txt = "dur(depot) <- dd", p = c(dd = 2.5), ev = et(amt = 100, cmt = "depot", rate = -2) |> et(c(1, 2, 4, 8, 12)), what = "dur()"),
      list(txt = "f(depot) <- fd\nalag(depot) <- lg", p = c(fd = 0.7, lg = 0.7), ev = .faEv, what = "f(), alag()")
    )
    for (.k in .kinds) {
      .p <- c(ka = 1.2, k = 0.14, .k$p)
      expect_error(
        rxSolveAdjointRk4(paste(.base, .k$txt, sep = "\n"), .k$ev, params = .p, calcSens = names(.p), method = "abs"),
        sprintf(.msg, .k$what),
        fixed = TRUE
      )
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
})
