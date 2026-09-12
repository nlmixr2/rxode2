rxTest({
  # rxode2#1359, downstream of the symengine environment: the subsystems that
  # differentiate a loaded model by one of its own names.  The environment's own
  # behaviour is in test-symengine-constants.R.
  # NB: rxFromSE() poisons the next `$`/`[[` read of a symengine env, so every
  # Basic is captured BEFORE the first rxFromSE() in each test.
  test_that("sensitivities by a parameter named like a symengine constant", {
    .m <- rxode2({
      cl <- exp(tcl + e)
      d/dt(center) <- -cl * center
    }, calcSens = TRUE)
    .n <- rxNorm(.m)
    expect_true(grepl("rx__sens_center_BY_e__", .n, fixed = TRUE))
    expect_false(grepl("rx_SymPy_Res_", .n, fixed = TRUE))
    expect_true(grepl(paste0("d/dt(rx__sens_center_BY_e__)=-exp(e+tcl)*center-",
                             "exp(e+tcl)*rx__sens_center_BY_e__"),
                      .n, fixed = TRUE))
  })

  test_that("a parameter named like a constant solves like any other name", {
    .mk <- function(v) {
      rxode2(sprintf(paste0("ka=exp(tka);\ncl=exp(tcl+%s);\n",
                            "d/dt(depot)=-ka*depot;\n",
                            "d/dt(center)=ka*depot-cl*center;\n"), v),
             calcSens = TRUE)
    }
    .ev <- et(amt = 100) |> et(seq(0, 24, by = 2))
    .a <- rxSolve(.mk("e"), .ev, params = c(tka = 0.4, tcl = -0.1, e = 0.2),
                  returnType = "data.frame", atol = 1e-11, rtol = 1e-11)
    .b <- rxSolve(.mk("ee"), .ev, params = c(tka = 0.4, tcl = -0.1, ee = 0.2),
                  returnType = "data.frame", atol = 1e-11, rtol = 1e-11)
    names(.a) <- sub("_BY_e__", "_BY_ee__", names(.a), fixed = TRUE)
    expect_equal(sort(names(.a)), sort(names(.b)))
    expect_equal(as.matrix(.a[names(.b)]), as.matrix(.b))
  })

  test_that("jump event-sensitivities wrt a parameter named like a constant", {
    # .rxEventSensDExpr() tested the model-side name against symengine-side free
    # symbols, so the term was silently dropped rather than erroring
    .mk <- function(v) {
      sprintf(paste0("ka=exp(tka);\ncl=exp(tcl);\nf(depot)=expit(%s);\n",
                     "d/dt(depot)=-ka*depot;\n",
                     "d/dt(center)=ka*depot-cl*center;\n"), v)
    }
    .ev <- et(amt = 100) |> et(seq(0, 24, by = 2))
    .p <- c(tka = 0.4, tcl = -0.1)
    .a <- rxSolve(rxode2(.mk("e"), calcSens = "e", eventSens = "jump"), .ev,
                  params = c(.p, e = 0.2), returnType = "data.frame",
                  atol = 1e-11, rtol = 1e-11)
    .b <- rxSolve(rxode2(.mk("ee"), calcSens = "ee", eventSens = "jump"), .ev,
                  params = c(.p, ee = 0.2), returnType = "data.frame",
                  atol = 1e-11, rtol = 1e-11)
    expect_equal(.a$rx__sens_center_BY_e__, .b$rx__sens_center_BY_ee__)
    # and it is the real derivative, not zero
    .mb <- rxode2(.mk("e"))
    .h <- 1e-6
    .fd <- (rxSolve(.mb, .ev, params = c(.p, e = 0.2 + .h),
                    returnType = "data.frame", atol = 1e-11, rtol = 1e-11)$center -
            rxSolve(.mb, .ev, params = c(.p, e = 0.2 - .h),
                    returnType = "data.frame", atol = 1e-11, rtol = 1e-11)$center) / (2 * .h)
    expect_lt(max(abs(.a$rx__sens_center_BY_e__ - .fd)), 1e-5)
    expect_gt(max(abs(.fd)), 1)
  })

  test_that("matExp()/indLin() rate constants keep a parameter named e", {
    # .multCollapse() re-parsed model-side text with symengine::S(), which read
    # the parameter `e` as Euler's number: k_p_q=exp(1) instead of k_p_q=e
    .n <- rxNorm(rxode2("d/dt(p)=-e*p;\nd/dt(q)=e*p-k*q;\n", indLin = TRUE))
    expect_true(grepl("k_p_q=e;", .n, fixed = TRUE))
    expect_false(grepl("exp(1)", .n, fixed = TRUE))
  })

  test_that("adjoint sensitivities accept a parameter named like a constant", {
    .m <- rxS(rxGetModel("d/dt(depot)=-ka*depot;\nd/dt(center)=ka*depot-(e/v)*center;\n"),
              TRUE, promoteLinSens = FALSE)
    .v <- c("ka", "e", "v")
    invisible(.rxJacobian(.m, c(rxStateOde(.m), .v)))
    .adj <- .rxAdjoint(.m, .v, "center")
    expect_true(any(grepl("d/dt(rx__sens_center_BY_e__)=rx__adjLambda_center_center__*center/v",
                          .adj, fixed = TRUE)))
    expect_false(any(grepl("rx_SymPy_Res_", .adj, fixed = TRUE)))
    expect_false(any(grepl("2.718", .adj, fixed = TRUE)))
  })

  test_that("delay() terms resolve against a parameter named like a constant", {
    .m <- .rxode2({
      d/dt(cen) <- -e * cen + 0.1 * delay(cen, tau)
      tau <- 1.5
    })
    .t <- .rxDelayTerms(.m)
    expect_equal(.t$state, "cen")
    expect_equal(.t$tau, "tau")
    .s <- rxS(.m)
    .b <- .s$..ddt
    expect_equal(.b, "d/dt(cen)=-cen*e+0.1*delay(cen, 1.5)")
  })

  test_that("a parameter-dependent delay differentiates by a name like a constant", {
    # d(tau)/dp is symengine::D(.tauRes, ...) in R/dde.R.  With the raw model
    # name that call failed inside a tryCatch, so dtauByP stayed "0" and the
    # breaking-point correction terms were silently dropped from the model.
    .n <- rxNorm(rxode2("d/dt(cen)=-e*cen+0.1*delay(cen, 1.5*e);\n",
                        calcSens = "e"))
    expect_true(grepl("rxDelayD(cen,1.5*e)", .n, fixed = TRUE))
    expect_true(grepl("alag(rx__sens_cen_BY_e__)=1.5*e", .n, fixed = TRUE))
    expect_true(grepl("f(rx__sens_cen_BY_e__)=", .n, fixed = TRUE))
  })

  test_that("the delay jump map keeps a parameter named like a constant", {
    # .rxDelaySensJumpMap() re-parses the rxFromSE() text of d/dt() with
    # symengine::S(), which reads a model-side `e` as Euler's number: the jump
    # amplitude came out as -(M_E)*(1.5) instead of -(e)*(1.5)
    .m <- .rxDelaySensJumpMap("cl=0.3;\nd/dt(cen)=-cl*cen+e*delay(cen,1.5*e);\n",
                              "e")
    expect_true(any(grepl("f(rx__sens_cen_BY_e__)=-(e)*(1.5)", .m$alagf, fixed = TRUE)))
    expect_false(any(grepl("M_E", .m$alagf, fixed = TRUE)))
  })

  test_that("a delay duration depending on E still resolves", {
    # `E` is deliberately left bound to Euler's number in the environment, so the
    # duration text has to be translated before it is evaluated there -- otherwise
    # it evaluates to a plain numeric, the "Basic" check fails and the
    # breaking-point corrections are dropped
    .m <- .rxDelaySensJumpMap("cl=0.3;\nd/dt(cen)=-cl*cen+0.1*delay(cen,1.5*E);\n",
                              "E")
    expect_true(any(grepl("alag(rx__sens_cen_BY_E__)", .m$alagf, fixed = TRUE)))
    expect_true(any(grepl("f(rx__sens_cen_BY_E__)=-(0.1)*(1.5)", .m$alagf, fixed = TRUE)))
  })

  test_that("mu-referencing keeps a covariate parameter named like a constant", {
    .f <- function() {
      ini({
        tcl <- 1
        e <- 0.5
        add.sd <- 0.7
        eta.cl ~ 0.1
      })
      model({
        cl <- exp(tcl + e * WT + eta.cl)
        d/dt(center) <- -cl * center
        cp <- center
        cp ~ add(add.sd)
      })
    }
    .d <- .f()$muRefCovariateDataFrame
    expect_equal(.d$covariateParameter, "e")
    expect_equal(.d$covariate, "WT")
  })
})
