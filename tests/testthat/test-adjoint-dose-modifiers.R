rxTest({
  withr::local_options(rxode2.useLinCmt = FALSE)

  # A modeled alag() shifts the dose that actually lands, F*amt, so its
  # transversality term scales with the bioavailability as well as the dose.
  .faTxt <- "d/dt(depot) <- -ka*depot\nd/dt(center) <- ka*depot - k*center\nf(depot) <- fd\nalag(depot) <- lg"
  .faP <- c(ka = 1.2, k = 0.14, fd = 0.7, lg = 0.7)
  .faEv <- et(amt = 100, cmt = "depot") |> et(c(1, 2, 4, 8, 12))

  test_that("f() and alag() sensitivities match the closed form for every driver with their duals", {
    # closed form for a bolus F*amt landing at lg (every observation is after it)
    .tt <- .faEv$time[is.na(.faEv$evid) | .faEv$evid == 0]
    .depot <- deriv(~ fd * 100 * exp(-ka * (t - lg)), names(.faP), function.arg = c(names(.faP), "t"))
    .center <- deriv(~ fd * 100 * ka / (ka - k) * (exp(-k * (t - lg)) - exp(-ka * (t - lg))),
      names(.faP), function.arg = c(names(.faP), "t"))
    .gD <- attr(do.call(.depot, c(as.list(.faP), list(t = .tt))), "gradient")
    .gC <- attr(do.call(.center, c(as.list(.faP), list(t = .tt))), "gradient")
    .known <- list()
    for (.pn in names(.faP)) {
      .known[[paste0("rx__sens_depot_BY_", .pn, "__")]] <- unname(.gD[, .pn])
      .known[[paste0("rx__sens_center_BY_", .pn, "__")]] <- unname(.gC[, .pn])
    }
    # abs has no f()/alag() dual
    for (.meth in c("rk4s", "dop853s", "ros4s", "radauiia5s", "dop853s+ros4s", "liblsodaadj", "cvodesadj")) {
      .a <- rxSolveAdjointRk4(
        .faTxt, .faEv,
        params = .faP, calcSens = names(.faP), method = .meth,
        atol = 1e-10, rtol = 1e-10, hmin = if (.meth == "rk4s") 0.001 else 0
      )
      .a <- .a[.a$time %in% .tt, , drop = FALSE]
      # cvodesadj has no dF/dtheta dual
      .cols <- if (.meth == "cvodesadj") grep("_BY_fd__$", names(.known), value = TRUE, invert = TRUE) else names(.known)
      for (.cn in .cols) {
        expect_lt(max(abs(.a[[.cn]] - .known[[.cn]])), 1e-6, label = paste(.meth, .cn))
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
})
