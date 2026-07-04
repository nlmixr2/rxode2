rxTest({

  # Forward-sensitivity support for delay() models (DDE).  Phase A: cataloging
  # delayed terms and validating that delay durations do not depend on the
  # sensitivity parameters.
  test_that(".rxDelayTerms catalogs distinct delayed terms", {
    .m <- .rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(cen) <- ka * depot - cl / v * cen + kin * delay(cen, tau)
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv); kin <- 0.1; tau <- 1.5
    })
    .t <- .rxDelayTerms(.m)
    expect_s3_class(.t, "data.frame")
    expect_equal(nrow(.t), 1L)
    expect_equal(.t$state, "cen")
    expect_equal(.t$tau, "tau")
    expect_match(.t$surrogate, "^rx__dly_cen_1__$")
  })

  test_that(".rxDelayTerms returns NULL when there is no delay()", {
    .m <- .rxode2({
      d/dt(x) <- -k * x
      k <- 0.1
    })
    expect_null(.rxDelayTerms(.m))
  })

  test_that(".rxDelayTerms distinguishes multiple delays / durations", {
    .m <- .rxode2({
      d/dt(a) <- -a + delay(a, 1) + delay(a, 2)
      d/dt(b) <- -b + delay(b, 1)
    })
    .t <- .rxDelayTerms(.m)
    expect_equal(nrow(.t), 3L)
    expect_setequal(paste(.t$state, .t$tau), c("a 1", "a 2", "b 1"))
    expect_equal(length(unique(.t$surrogate)), 3L)
  })

  test_that(".rxResolveRootVars resolves through model assignments", {
    .m <- .rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- exp(ltau)
    })
    .defs <- .rxModelDefs(.m)
    expect_setequal(.rxResolveRootVars("tau", .defs), "ltau")
    expect_setequal(.rxResolveRootVars("cl / v", .defs), c("tcl", "tv"))
  })

  test_that(".rxDelayValidateTau passes for constant / covariate delays", {
    .m <- .rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- 1.5
    })
    expect_true(.rxDelayValidateTau(.m, c("tcl", "tv")))
    ## covariate-dependent delay is allowed (covariate is not a sens parameter)
    .mc <- .rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- WT / 70
    })
    expect_true(.rxDelayValidateTau(.mc, c("tcl", "tv")))
  })

  test_that("delay() sensitivity ODEs include the delayed variational term", {
    .m <- .rxode2({
      d/dt(cen) <- -k * cen + kin * delay(cen, tau)
      cen(0) <- 10
      k <- 0.2; kin <- 0.5; tau <- 1
    })
    .ms <- .rxode2(.m, calcSens = c("k", "kin"))
    .n <- rxNorm(.ms)
    ## dS_k/dt has the delayed sensitivity of cen w.r.t. k
    expect_match(.n, "delay(rx__sens_cen_BY_k__,tau)", fixed = TRUE)
    expect_match(.n, "delay(rx__sens_cen_BY_kin__,tau)", fixed = TRUE)
    ## and the sens states inherit the DDE flag once re-parsed
    expect_equal(unname(rxModelVars(.ms)$flags[["hasDelay"]]), 1L)
  })

  test_that("delay() forward sensitivities match central finite differences", {
    .mk <- function(k, kin) {
      .rxode2(paste0("d/dt(cen) <- -k*cen + kin*delay(cen, tau)\n",
                    "cen(0) <- 10\nk <- ", k, "; kin <- ", kin, "; tau <- 1"))
    }
    .ev <- et(seq(0, 8, by = 0.5))
    .s <- rxSolve(.rxode2(.mk(0.2, 0.5), calcSens = c("k", "kin")), .ev,
                  atol = 1e-12, rtol = 1e-12)
    .h <- 1e-4
    .fdk <- (rxSolve(.mk(0.2 + .h, 0.5), .ev, atol = 1e-12, rtol = 1e-12)$cen -
             rxSolve(.mk(0.2 - .h, 0.5), .ev, atol = 1e-12, rtol = 1e-12)$cen) / (2 * .h)
    .fdkin <- (rxSolve(.mk(0.2, 0.5 + .h), .ev, atol = 1e-12, rtol = 1e-12)$cen -
               rxSolve(.mk(0.2, 0.5 - .h), .ev, atol = 1e-12, rtol = 1e-12)$cen) / (2 * .h)
    ## non-trivial sensitivities (delay feedback amplifies them)
    expect_gt(diff(range(.s[["rx__sens_cen_BY_k__"]])), 1)
    expect_lt(max(abs(.s[["rx__sens_cen_BY_k__"]] - .fdk)), 1e-2)
    expect_lt(max(abs(.s[["rx__sens_cen_BY_kin__"]] - .fdkin)), 1e-2)
  })

  test_that("delay() sensitivities are correct in a 2-state model (delay on one state)", {
    ## depot is not delayed; cen is.  The depot sens must have no delay term and
    ## the cen sens must include delay(S_cen, tau).
    .mk <- function(ka, ke) {
      .rxode2(paste0("d/dt(depot) <- -ka*depot\n",
                    "d/dt(cen) <- ka*depot - ke*cen + 0.3*delay(cen, 1)\n",
                    "depot(0) <- 100\nka <- ", ka, "; ke <- ", ke))
    }
    .ev <- et(seq(0, 10, by = 0.5))
    .ms <- .rxode2(.mk(1.0, 0.3), calcSens = c("ka", "ke"))
    .n <- rxNorm(.ms)
    expect_match(.n, "delay(rx__sens_cen_BY_ka__,1)", fixed = TRUE)
    expect_false(grepl("delay(rx__sens_depot", .n, fixed = TRUE))
    .s <- rxSolve(.ms, .ev, atol = 1e-12, rtol = 1e-12)
    .h <- 1e-4
    .fdke <- (rxSolve(.mk(1.0, 0.3 + .h), .ev, atol = 1e-12, rtol = 1e-12)$cen -
              rxSolve(.mk(1.0, 0.3 - .h), .ev, atol = 1e-12, rtol = 1e-12)$cen) / (2 * .h)
    expect_lt(max(abs(.s[["rx__sens_cen_BY_ke__"]] - .fdke)), 1e-2)
  })

  test_that("delay() sensitivity is correct when the initial condition depends on a parameter", {
    ## cen(0) = base, so S_base(0) = 1 and the delayed pre-history of the sens
    ## state (for t < tau) must return that initial sensitivity.
    .mk <- function(base) {
      .rxode2(paste0("d/dt(cen) <- -k*cen + kin*delay(cen, tau)\n",
                    "cen(0) <- base\nk <- 0.2; kin <- 0.5; tau <- 1; base <- ", base))
    }
    .ev <- et(seq(0, 6, by = 0.5))
    .ms <- .rxode2(.mk(10), calcSens = "base")
    expect_match(rxNorm(.ms), "rx__sens_cen_BY_base__(0)=1", fixed = TRUE)
    .s <- rxSolve(.ms, .ev, atol = 1e-12, rtol = 1e-12)
    expect_equal(.s[["rx__sens_cen_BY_base__"]][1], 1, tolerance = 1e-8)
    .h <- 1e-3
    .fd <- (rxSolve(.mk(10 + .h), .ev, atol = 1e-12, rtol = 1e-12)$cen -
            rxSolve(.mk(10 - .h), .ev, atol = 1e-12, rtol = 1e-12)$cen) / (2 * .h)
    expect_lt(max(abs(.s[["rx__sens_cen_BY_base__"]] - .fd)), 1e-4)
  })

  test_that(".rxDelayValidateTau errors for parameter-dependent delays", {
    .m <- .rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- exp(ltau)
    })
    expect_error(.rxDelayValidateTau(.m, c("tcl", "tv", "ltau")), "not yet supported")
    ## direct dependence (tau is itself the parameter)
    .m2 <- .rxode2({
      d/dt(cen) <- -k * cen + delay(cen, tau)
      k <- 0.1
    })
    expect_error(.rxDelayValidateTau(.m2, c("tau")), "parameter")
  })

  test_that("parameter-dependent delay sensitivities match finite differences", {
    ## tau depends on the sensitivity parameter ltau (tau = exp(ltau)).  The
    ## sensitivity gains the delayed-derivative correction -ydot(t-tau)*dtau/dp.
    .m <- .rxode2("d/dt(cen) <- -k*cen + kin*delay(cen, tau)
cen(0) <- 10
k <- 0.2; kin <- 0.5; tau <- exp(ltau)")
    .ms <- .rxode2(.m, calcSens = "ltau")
    ## the correction term uses the analytic delayed time-derivative rxDelayD()
    expect_match(rxNorm(.ms), "rxDelayD(cen,exp(ltau))", fixed = TRUE)
    .ev <- et(seq(0, 8, by = 0.25))
    .l0 <- log(1.5)
    .s <- rxSolve(.ms, .ev, params = c(ltau = .l0), atol = 1e-12, rtol = 1e-12)
    .h <- 1e-4
    .fd <- (rxSolve(.ms, .ev, params = c(ltau = .l0 + .h), atol = 1e-12, rtol = 1e-12)$cen -
            rxSolve(.ms, .ev, params = c(ltau = .l0 - .h), atol = 1e-12, rtol = 1e-12)$cen) / (2 * .h)
    .an <- .s[["rx__sens_cen_BY_ltau__"]]
    expect_gt(diff(range(.an)), 1) # non-trivial
    ## analytic delayed derivative: matches the FD reference to its own accuracy
    expect_lt(max(abs(.an - .fd)), 1e-3)
  })

  test_that("rxDelayD() is the exact time-derivative of the delayed state", {
    ## d/dt(w) = rxDelayD(y, 1) with y = 10 exp(-t) integrates the delayed
    ## derivative ydot(t-1), so w(t) = y(t-1) - y(prehistory) = 10 exp(-(t-1)) - 10
    ## for t > 1 and 0 for t <= 1.  Validates the analytic dense-interpolant
    ## derivative against a closed form (machine precision).
    .m <- .rxode2({
      d/dt(y) <- -y
      d/dt(w) <- rxDelayD(y, 1)
      y(0) <- 10
      w(0) <- 0
    })
    .tt <- seq(0, 4, by = 0.5)
    .s <- rxSolve(.m, et(.tt), atol = 1e-12, rtol = 1e-12)
    .expect <- ifelse(.tt > 1, 10 * exp(-(.tt - 1)) - 10, 0)
    expect_lt(max(abs(.s$w - .expect)), 1e-7)
  })

  test_that("rxDelayD2/rxDelayD3 are the exact higher time-derivatives", {
    ## y = 10 exp(-t): yddot = y, ydddot = -y, so for tau = 1.5 and t > 1.5
    ##   d/dt(b2) = rxDelayD2(y,1.5) = y(t-1.5)  -> b2 = 10 - 10 exp(-(t-1.5))
    ##   d/dt(b3) = rxDelayD3(y,1.5) = -y(t-1.5) -> b3 = 10 exp(-(t-1.5)) - 10
    .m <- .rxode2({
      d/dt(y) <- -y
      d/dt(b2) <- rxDelayD2(y, 1.5)
      d/dt(b3) <- rxDelayD3(y, 1.5)
      y(0) <- 10; b2(0) <- 0; b3(0) <- 0
    })
    .tt <- seq(0, 5, by = 0.5)
    .s <- rxSolve(.m, et(.tt), atol = 1e-12, rtol = 1e-12)
    expect_lt(max(abs(.s$b2 - ifelse(.tt > 1.5, 10 - 10 * exp(-(.tt - 1.5)), 0))), 1e-6)
    expect_lt(max(abs(.s$b3 - ifelse(.tt > 1.5, 10 * exp(-(.tt - 1.5)) - 10, 0))), 1e-6)
  })

  test_that("state-dependent delays are rejected for sensitivities", {
    .m <- .rxode2({
      d/dt(cen) <- -k * cen + delay(cen, tau)
      tau <- 1 + 0.01 * cen
      k <- 0.2
    })
    expect_error(.rxode2(.m, calcSens = "k"), "state-dependent")
  })

  ## Second-order (constant-delay) sensitivities -- Phase A.  There is no
  ## production assembly path for 2nd-order sensitivities yet (that arrives with
  ## the 3rd-order work), so build the augmented model directly from the pieces.
  .build2nd <- function(rhs, pars) {
    .m <- .rxode2(paste0(rhs, "\ncen(0) <- 10"))
    .mod <- .rxLoadPrune(rxModelVars(.m), FALSE)
    invisible(.rxJacobian(.mod, c("cen", pars)))
    invisible(.rxSens(.mod, pars))
    invisible(.rxSens(.mod, pars, pars))
    .st <- rxStateOde(.mod)
    .rxode2(paste(c(paste0("cmt(", .st, ");"), .mod$..ddt, .mod$..jacobian,
                   .mod$..sens, .mod$..sens2), collapse = "\n"))
  }

  test_that("second-order delay() sensitivities match finite differences", {
    .m2 <- .build2nd("d/dt(cen) <- -k*cen + kin*delay(cen, 1)", c("k", "kin"))
    expect_match(rxNorm(.m2), "delay(rx__sens_cen_BY_k_BY_kin__,1)", fixed = TRUE)
    .ev <- et(seq(0, 6, by = 0.5))
    ## the default composite can stall on the larger 2nd-order DDE system; the
    ## pure dense dop853 path is exact to the method order.
    .sv <- function(k, kin) {
      rxSolve(.m2, .ev, params = c(k = k, kin = kin), method = "dop853",
              atol = 1e-10, rtol = 1e-10)
    }
    .h <- 1e-4
    .s <- .sv(0.2, 0.5)
    ## S^{k,kin} = d(S^k)/dkin : central diff of the analytic first-order S^k
    .fd <- (.sv(0.2, 0.5 + .h)[["rx__sens_cen_BY_k__"]] -
            .sv(0.2, 0.5 - .h)[["rx__sens_cen_BY_k__"]]) / (2 * .h)
    .an <- .s[["rx__sens_cen_BY_k_BY_kin__"]]
    expect_gt(diff(range(.an)), 1)
    expect_lt(max(abs(.an - .fd)), 0.05)
    ## pure second S^{kin,kin}
    .fd2 <- (.sv(0.2, 0.5 + .h)[["rx__sens_cen_BY_kin__"]] -
             .sv(0.2, 0.5 - .h)[["rx__sens_cen_BY_kin__"]]) / (2 * .h)
    expect_lt(max(abs(.s[["rx__sens_cen_BY_kin_BY_kin__"]] - .fd2)), 0.05)
  })

  test_that("parameter-dependent delays are rejected for second-order sensitivities", {
    ## tau depends on ltau, so the DDE breaking points move with the parameter
    ## and the second-order sensitivities pick up jump discontinuities at them
    ## (not yet handled).  This must error cleanly (not a masked "Aborted").
    .m <- .rxode2("d/dt(cen) <- -k*cen + kin*delay(cen, tau)\ncen(0)<-10\ntau<-exp(ltau)")
    .mod <- .rxLoadPrune(rxModelVars(.m), FALSE)
    invisible(.rxJacobian(.mod, c("cen", "k", "kin", "ltau")))
    invisible(.rxSens(.mod, c("k", "ltau")))
    expect_error(.rxSens(.mod, c("k", "ltau"), c("k", "ltau")),
                 "breaking points")
  })

  ## Third-order (constant-delay) sensitivities.  The general third-order
  ## expansion is interim (R, to be superseded by the C++ one in
  ## nlmixr2/rxode2#1092); the delay augmentation is the part being tested.
  .build3rd <- function(rhs, pars) {
    .m <- .rxode2(paste0(rhs, "\ncen(0) <- 10"))
    .mod <- .rxLoadPrune(rxModelVars(.m), FALSE)
    invisible(.rxJacobian(.mod, c("cen", pars)))
    invisible(.rxSens(.mod, pars))
    invisible(.rxSens(.mod, pars, pars))
    invisible(.rxSens(.mod, pars, pars, pars))
    .st <- rxStateOde(.mod)
    .rxode2(paste(c(paste0("cmt(", .st, ");"), .mod$..ddt, .mod$..jacobian,
                   .mod$..sens, .mod$..sens2, .mod$..sens3), collapse = "\n"))
  }

  test_that("third-order delay() sensitivities match finite differences", {
    .m3 <- .build3rd("d/dt(cen) <- -k*cen + kin*delay(cen, 1)", c("k", "kin"))
    expect_match(rxNorm(.m3), "delay(rx__sens_cen_BY_k_BY_kin_BY_kin__,1)", fixed = TRUE)
    .ev <- et(seq(0, 6, by = 0.5))
    .sv <- function(k, kin) {
      rxSolve(.m3, .ev, params = c(k = k, kin = kin), method = "dop853",
              atol = 1e-10, rtol = 1e-10)
    }
    .h <- 1e-4
    .s <- .sv(0.2, 0.5)
    ## S^{k,kin,kin} = d(S^{k,kin})/dkin : central diff of the analytic 2nd order
    .fd <- (.sv(0.2, 0.5 + .h)[["rx__sens_cen_BY_k_BY_kin__"]] -
            .sv(0.2, 0.5 - .h)[["rx__sens_cen_BY_k_BY_kin__"]]) / (2 * .h)
    .an <- .s[["rx__sens_cen_BY_k_BY_kin_BY_kin__"]]
    expect_gt(diff(range(.an)), 1)
    expect_lt(max(abs(.an - .fd)), 0.05)
  })

  test_that("nonlinear delays are rejected for third-order sensitivities", {
    ## the delayed value multiplies a state -> extra third-order tensor terms
    .m <- .rxode2("d/dt(cen) <- -k*cen*delay(cen, 1)\ncen(0)<-10")
    .mod <- .rxLoadPrune(rxModelVars(.m), FALSE)
    invisible(.rxJacobian(.mod, c("cen", "k")))
    invisible(.rxSens(.mod, "k"))
    invisible(.rxSens(.mod, "k", "k"))
    expect_error(.rxSens(.mod, "k", "k", "k"), "nonlinear")
  })
})
