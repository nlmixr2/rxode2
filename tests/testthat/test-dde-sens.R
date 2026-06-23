rxTest({
  # Forward-sensitivity support for delay() models (DDE).  Phase A: cataloging
  # delayed terms and validating that delay durations do not depend on the
  # sensitivity parameters.

  test_that(".rxDelayTerms catalogs distinct delayed terms", {
    .m <- rxode2({
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
    .m <- rxode2({
      d/dt(x) <- -k * x
      k <- 0.1
    })
    expect_null(.rxDelayTerms(.m))
  })

  test_that(".rxDelayTerms distinguishes multiple delays / durations", {
    .m <- rxode2({
      d/dt(a) <- -a + delay(a, 1) + delay(a, 2)
      d/dt(b) <- -b + delay(b, 1)
    })
    .t <- .rxDelayTerms(.m)
    expect_equal(nrow(.t), 3L)
    expect_setequal(paste(.t$state, .t$tau), c("a 1", "a 2", "b 1"))
    expect_equal(length(unique(.t$surrogate)), 3L)
  })

  test_that(".rxResolveRootVars resolves through model assignments", {
    .m <- rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- exp(ltau)
    })
    .defs <- .rxModelDefs(.m)
    expect_setequal(.rxResolveRootVars("tau", .defs), "ltau")
    expect_setequal(.rxResolveRootVars("cl / v", .defs), c("tcl", "tv"))
  })

  test_that(".rxDelayValidateTau passes for constant / covariate delays", {
    .m <- rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- 1.5
    })
    expect_true(.rxDelayValidateTau(.m, c("tcl", "tv")))
    ## covariate-dependent delay is allowed (covariate is not a sens parameter)
    .mc <- rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- WT / 70
    })
    expect_true(.rxDelayValidateTau(.mc, c("tcl", "tv")))
  })

  test_that(".rxDelayValidateTau errors for parameter-dependent delays", {
    .m <- rxode2({
      d/dt(cen) <- -cl / v * cen + delay(cen, tau)
      cl <- exp(tcl); v <- exp(tv); tau <- exp(ltau)
    })
    expect_error(.rxDelayValidateTau(.m, c("tcl", "tv", "ltau")), "not yet supported")
    ## direct dependence (tau is itself the parameter)
    .m2 <- rxode2({
      d/dt(cen) <- -k * cen + delay(cen, tau)
      k <- 0.1
    })
    expect_error(.rxDelayValidateTau(.m2, c("tau")), "parameter")
  })
})
