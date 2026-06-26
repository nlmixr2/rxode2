rxTest({
  # Event ("jump") sensitivities -- Phase A: build-time index map and the
  # symbolic total-derivatives of the dosing parameters (alag, F).
  # See ~/src/rxode2-event-sensitivities-plan.md.

  .mkMod <- function(modStr) {
    # depot+central first-order absorption with modeled alag + F, sensitivities
    # wrt two etas, full Jacobian (as eventSens="jump" will force).
    rxode2(modStr, calcSens = c("eta_ka", "eta_lag"), calcJac = TRUE)
  }

  .modConstF <- "
    ka <- exp(tka + eta_ka)
    cl <- exp(tcl)
    v  <- exp(tv)
    alag(depot) <- exp(tlag + eta_lag)
    f(depot)    <- expit(tf)
    d/dt(depot)   <- -ka * depot
    d/dt(central) <-  ka * depot - cl / v * central
  "

  .modStateF <- "
    ka <- exp(tka + eta_ka)
    cl <- exp(tcl)
    v  <- exp(tv)
    alag(depot) <- exp(tlag + eta_lag)
    f(depot)    <- 1 / (1 + exp(-(tf + 0.1 * central)))
    d/dt(depot)   <- -ka * depot
    d/dt(central) <-  ka * depot - cl / v * central
  "

  test_that(".rxEventSensMap relates sens compartments to states/params", {
    im <- .rxEventSensMap(.mkMod(.modConstF))
    expect_equal(im$states, c("depot", "central"))
    expect_equal(im$nState, 2L)
    expect_equal(unname(im$stateCmt[c("depot", "central")]), c(1L, 2L))
    expect_setequal(im$sensParams, c("eta_ka", "eta_lag"))
    # one map row per (state, param): 2 states x 2 params
    expect_equal(nrow(im$map), 4L)
    # rx__sens_depot_BY_eta_ka__ must be a real compartment mapped to depot
    .row <- im$map[im$map$state == "depot" & im$map$param == "eta_ka", ]
    expect_equal(.row$stateCmt, 1L)
    expect_true(.row$sensCmt > im$nState) # sens compartments come after states
    # depot carries modeled alag + F, not rate/dur
    expect_equal(im$lagCmt, 1L)
    expect_equal(im$fCmt, 1L)
    expect_length(im$rateCmt, 0L)
    expect_length(im$durCmt, 0L)
  })

  test_that(".rxEventSensMap returns NULL without sensitivities", {
    m <- rxode2("ka <- 1\nd/dt(depot) <- -ka * depot")
    expect_null(.rxEventSensMap(m))
  })

  test_that(".rxEventSensSplit handles underscored state/param names", {
    s <- c("rx__sens_depot_BY_eta_ka__", "rx__sens_central_BY_eta_lag__")
    sp <- .rxEventSensSplit(s, c("depot", "central"))
    expect_equal(sp$state, c("depot", "central"))
    expect_equal(sp$param, c("eta_ka", "eta_lag"))
    # second-order names are not first-order: state/param NA
    sp2 <- .rxEventSensSplit("rx__sens_depot_BY_eta_ka_BY_eta_lag__",
                             c("depot", "central"))
    expect_true(is.na(sp2$state))
  })

  test_that("alag total-derivative is the plain partial when state-independent", {
    d <- .rxEventSensDerivs(.mkMod(.modConstF))
    # alag depends only on eta_lag (no states) -> single non-zero row, no coupling
    expect_equal(nrow(d$lag), 1L)
    expect_equal(d$lag$param, "eta_lag")
    expect_false(any(grepl("rx__sens_", d$lag$expr)))
    # d(exp(eta_lag+tlag))/d(eta_lag) = exp(eta_lag+tlag)
    expect_equal(eval(parse(text = d$lag$expr),
                      list(eta_lag = 0.3, tlag = log(2))),
                 exp(0.3 + log(2)))
  })

  test_that("F total-derivative picks up the state-coupling term", {
    d <- .rxEventSensDerivs(.mkMod(.modStateF))
    # F depends on `central` -> d(F)/d(eta) is purely the coupling term
    # (no direct eta in F), nonzero for BOTH etas via S_central.
    expect_setequal(d$f$param, c("eta_ka", "eta_lag"))
    expect_true(all(grepl("rx__sens_central_BY_", d$f$expr)))
  })

  test_that(".rxEventSensMode resolves and validates the mode", {
    expect_equal(.rxEventSensMode("jump"), "jump")
    expect_equal(.rxEventSensMode("both"), "both")
    expect_equal(.rxEventSensMode("fd"), "fd")
    expect_error(.rxEventSensMode("bogus"), "eventSens")
    withr::with_options(list(rxode2.eventSens = "jump"), {
      expect_equal(.rxEventSensMode(NULL), "jump")
    })
    withr::with_options(list(rxode2.eventSens = NULL), {
      expect_equal(.rxEventSensMode(NULL), "fd") # shipped default during dev
    })
  })

  test_that("eventSens='fd' (default) is a no-op on rxode2()", {
    m <- rxode2("d/dt(depot) <- -ka*depot\nka <- 1",
                calcSens = "ka")
    expect_equal(m$eventSens, "fd")
    expect_null(m$eventSensInfo)
  })

  test_that("eventSens='jump' forces calcJac and attaches Phase-A info", {
    m <- rxode2(.modConstF, calcSens = c("eta_ka", "eta_lag"),
                eventSens = "jump")
    expect_equal(m$eventSens, "jump")
    expect_true(isTRUE(m$calcJac))
    info <- m$eventSensInfo
    expect_equal(info$mode, "jump")
    expect_equal(nrow(info$map$map), 4L)
    # constant F (expit(tf)) -> no eta-dependent F derivative; alag depends on
    # eta_lag -> exactly one lag derivative row.
    expect_equal(nrow(info$derivs$lag), 1L)
    expect_equal(nrow(info$derivs$f), 0L)
  })

  test_that("eventSens='jump' on a model without sensitivities is a no-op", {
    m <- rxode2("d/dt(depot) <- -depot", eventSens = "jump")
    expect_null(m$eventSensInfo)
  })

  test_that(".rxEventSensCLines emits correctly-indexed C assignment lines", {
    m <- rxode2(.modStateF, calcSens = c("eta_ka", "eta_lag"),
                eventSens = "jump")
    cl <- .rxEventSensCLines(m$eventSensInfo)
    expect_equal(cl$nSensParam, 2L)
    expect_equal(unname(cl$paramIdx[c("eta_ka", "eta_lag")]), c(0L, 1L))
    # alag(depot): cmt0=0, eta_lag idx 1 -> _dLagSave[0*2+1] = _dLagSave[1]
    expect_length(cl$lag, 1L)
    expect_match(cl$lag, "_dLagSave\\[1\\] = exp\\(eta_lag\\+tlag\\);", fixed = FALSE)
    # state-dependent F: both etas via S_central, indices 0 and 1
    expect_length(cl$f, 2L)
    expect_true(any(grepl("_dFSave[0]", cl$f, fixed = TRUE)))
    expect_true(any(grepl("_dFSave[1]", cl$f, fixed = TRUE)))
    # right-hand sides reference the exact local names the F function declares
    expect_true(all(grepl("rx__sens_central_BY_", cl$f)))
  })

  test_that(".rxEventSensCLines is NULL for fd / no-sens", {
    expect_null(.rxEventSensCLines(NULL))
  })

  test_that("eventSens='jump' model with state-dependent F compiles and solves", {
    # Exercises the full lines channel: R-generated dLag/dF assignment lines are
    # pushed to codegen, spliced into the function bodies (with the state-coupling
    # term referencing populated sens locals), and the model compiles + solves.
    # (The exact emitted C is asserted at the R level by the .rxEventSensCLines
    # tests above; here we verify the end-to-end build/solve.)
    m <- rxode2(.modStateF, calcSens = c("eta_ka", "eta_lag"),
                eventSens = "jump")
    expect_s3_class(m, "rxode2")
    e <- et(amt = 100, cmt = "depot")
    e <- et(e, seq(0, 12, 4))
    ini <- c(tka = 0, tcl = 1, tv = 2, tlag = 0, tf = 0,
             eta_ka = 0, eta_lag = 0)
    s <- rxSolve(m, e, ini)
    expect_true(nrow(s) > 0L)
    # jump-mode solve matches fd-mode for the states (dLag/dF are not yet called
    # by handle_evid, so the trajectory is identical -- a useful invariant until
    # the A2 runtime injection lands).
    mfd <- rxode2(.modStateF, calcSens = c("eta_ka", "eta_lag"),
                  eventSens = "fd")
    sfd <- rxSolve(mfd, e, ini)
    expect_equal(s$central, sfd$central)
  })

  test_that("eventSens mode is folded into the model cache key", {
    mfd <- rxode2(.modConstF, calcSens = c("eta_ka", "eta_lag"),
                  eventSens = "fd")
    mj <- rxode2(.modConstF, calcSens = c("eta_ka", "eta_lag"),
                 eventSens = "jump")
    # same model text, different mode -> distinct parsed md5 (distinct DLLs)
    expect_false(identical(
      unname(rxModelVars(mfd)$md5["parsed_md5"]),
      unname(rxModelVars(mj)$md5["parsed_md5"])
    ))
    # the cache key is reset after each build (no leak to later non-jump builds)
    expect_identical(.rxEventSensCacheKey, "")
  })
})
