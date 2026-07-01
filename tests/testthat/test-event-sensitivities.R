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
    expect_equal(.rxEventSensMode("fdAll"), "fdAll")
    expect_error(.rxEventSensMode("bogus"), "eventSens")
    withr::with_options(list(rxode2.eventSens = "jump"), {
      expect_equal(.rxEventSensMode(NULL), "jump")
    })
    withr::with_options(list(rxode2.eventSens = NULL), {
      expect_equal(.rxEventSensMode(NULL), "jump")
    })
  })

  test_that("eventSens='jump' (default) is a no-op on rxode2()", {
    m <- rxode2("d/dt(depot) <- -ka*depot\nka <- 1",
                calcSens = "ka")
    expect_equal(m$eventSens, "jump")
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

  test_that("eventSens='jump' works with linCmt()+mtime() models", {
    m <- rxode2({
      C2 <- linCmt(CL, V)
      mtime(t1) <- mt1
      mtime(t2) <- mt2
    }, eventSens = "jump", linCmtSens = "linCmtA", linCmtSensType = "A")
    expect_equal(m$eventSens, "jump")
    expect_null(m$eventSensInfo)
    e <- eventTable() |>
      add.dosing(dose = 3, nbr.doses = 2, dosing.interval = 8) |>
      add.sampling(0:16)
    s <- rxSolve(m, e, params = c(V = 20, CL = 25, mt1 = 0.5, mt2 = 1.75))
    expect_true(all(c(0.5, 1.75) %in% s$time))
  })

  test_that("eventSens='fd' resolves to hybrid jump for mixed ODE+linCmt", {
    m <- rxode2({
      C2 <- linCmt(CL, V)
      alag(eff) <- exp(tlag + eta_lag)
      d/dt(eff) <- kin - kout * eff
    }, calcSens = "eta_lag", eventSens = "fd",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    expect_equal(m$eventSens, "jump")
    expect_false(is.null(m$eventSensInfo))
    expect_equal(m$eventSensInfo$map$states, "eff")
  })

  test_that("eventSens='fdAll' keeps full finite-difference fallback", {
    m <- rxode2({
      C2 <- linCmt(CL, V)
      d/dt(eff) <- kin - kout * eff
    }, eventSens = "fdAll", linCmtSens = "linCmtA", linCmtSensType = "A")
    expect_equal(m$eventSens, "fd")
    expect_null(m$eventSensInfo)
  })

  test_that("eventSens='fdAll' works correctly in a population (multi-subject) solve", {
    # Regression for a previously-untested combination (plan Section 0.4 gap):
    # confirm fdAll's per-subject solving (each subject independently, no
    # jump machinery/eventSensInfo at all) isn't corrupted by the population
    # (multi-subject/parallel) solve path -- each subject's trajectory must
    # match an INDEPENDENT single-subject solve with that subject's own
    # modeled-lag parameter.
    mLin <- rxode2({
      C2 <- linCmt(CL, V)
      alag(central) <- exp(tlag + eta_lag)
    }, calcSens = "eta_lag", eventSens = "fdAll",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    expect_equal(mLin$eventSens, "fd")
    expect_null(mLin$eventSensInfo)

    th <- c(V = 20, CL = 25, tlag = 0)
    ev <- data.frame(eta_lag = c(0, 0.3, -0.2))
    e <- et(amt = 100, cmt = "central", id = 1:3) |> et(seq(0, 12, 0.5), id = 1:3)

    sPop <- rxSolve(mLin, e, th, iCov = ev)
    for (.id in 1:3) {
      e1 <- eventTable() |>
        add.dosing(dose = 100, cmt = "central") |>
        add.sampling(seq(0, 12, 0.5))
      s1 <- rxSolve(mLin, e1, c(th, eta_lag = ev$eta_lag[.id]))
      expect_equal(sPop$central[sPop$id == .id], s1$central,
        tolerance = 1e-8, info = paste0("id: ", .id)
      )
    }
  })

  test_that("eventSens='fd' resolves to symbolic jump for pure linCmt", {
    m <- rxode2({
      C2 <- linCmt(CL, V)
      alag(central) <- exp(tlag + eta_lag)
    }, calcSens = "eta_lag", eventSens = "fd",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    expect_equal(m$eventSens, "jump")
    expect_false(is.null(m$eventSensInfo))
  })

  test_that("pure linCmt hybrid event path matches the ODE equivalent", {
    mLin <- rxode2({
      C2 <- linCmt(CL, V)
      alag(central) <- exp(tlag + eta_lag)
    }, calcSens = "eta_lag", eventSens = "fd",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    mOde <- rxode2({
      d/dt(central) <- -CL / V * central
      alag(central) <- exp(tlag + eta_lag)
    }, calcSens = "eta_lag", eventSens = "jump")

    e <- eventTable() |>
      add.dosing(dose = 100, cmt = "central") |>
      add.sampling(seq(0, 12, 0.5))
    p <- c(V = 20, CL = 25, tlag = 0, eta_lag = 0)

    sLin <- rxSolve(mLin, e, params = p)
    sOde <- rxSolve(mOde, e, params = p)

    expect_equal(sLin$central, sOde$central, tolerance = 1e-5)
  })

  test_that("mixed ODE+linCmt jump keeps only ODE-scoped jump sensitivities", {
    m <- rxode2({
      C2 <- linCmt(CL, V)
      alag(eff) <- exp(tlag + eta_lag)
      d/dt(eff) <- kin - kout * eff
    }, calcSens = "eta_lag", eventSens = "jump",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    .info <- m$eventSensInfo
    expect_equal(.info$map$states, "eff")
    expect_equal(.info$map$sensParams, "eta_lag")
    expect_equal(unique(.info$map$map$state), "eff")
    expect_equal(.info$map$lagCmt, 1L)
  })

  test_that("mixed ODE+linCmt hybrid event path matches the ODE equivalent", {
    mLin <- rxode2({
      C2 <- linCmt(CL, V)
      alag(central) <- exp(tlag + eta_lag)
      d/dt(eff) <- kin - kout * eff
    }, calcSens = "eta_lag", eventSens = "fd",
    linCmtSens = "linCmtA", linCmtSensType = "A")
    mOde <- rxode2({
      d/dt(central) <- -CL / V * central
      alag(central) <- exp(tlag + eta_lag)
      d/dt(eff) <- kin - kout * eff
    }, calcSens = "eta_lag", eventSens = "jump")

    e <- eventTable() |>
      add.dosing(dose = 100, cmt = "central") |>
      add.sampling(seq(0, 12, 0.5))
    p <- c(V = 20, CL = 25, kin = 1, kout = 0.2, tlag = 0, eta_lag = 0)

    sLin <- rxSolve(mLin, e, params = p)
    sOde <- rxSolve(mOde, e, params = p)

    expect_equal(sLin$central, sOde$central, tolerance = 1e-5)
    expect_equal(sLin$eff, sOde$eff, tolerance = 1e-5)
  })

  test_that("eventSens='jump' works with ODE+mtime() models", {
    .mod <- "
      d/dt(depot) <- -ka*depot
      d/dt(central) <- ka*depot-kel*central
      ka <- exp(tka)
      kel <- exp(tkel)
      mtime(t1) <- mt1
      mtime(t2) <- mt2
    "
    mj <- rxode2(.mod, calcSens = "tka", eventSens = "jump")
    mfd <- rxode2(.mod, calcSens = "tka", eventSens = "fd")
    e <- eventTable() |>
      add.dosing(dose = 100, cmt = "depot") |>
      add.sampling(0:12)
    .p <- c(tka = 0, tkel = log(0.2), mt1 = 0.5, mt2 = 1.75)
    sj <- rxSolve(mj, e, params = .p)
    sfd <- rxSolve(mfd, e, params = .p)
    expect_true(isTRUE(mj$calcJac))
    expect_equal(sj$central, sfd$central, tolerance = 1e-5)
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
    # jump-mode solve matches fd-mode for the physical states: the jump injection
    # only touches the sensitivity compartments, never the physical state.  The
    # match is to solver tolerance (not bit-identical) because jump forces
    # calcJac=TRUE -> the analytic Jacobian gives LSODA a slightly different step
    # path than fd's numerical Jacobian (~1e-7 on the trajectory).
    mfd <- rxode2(.modStateF, calcSens = c("eta_ka", "eta_lag"),
                  eventSens = "fd")
    sfd <- rxSolve(mfd, e, ini)
    expect_equal(s$central, sfd$central, tolerance = 1e-5)
  })

  test_that("additive-bolus F jump (ddelta row) matches finite differences", {
    # Constant alag (no eta) + F = expit(tf + eta_f): the only event-sensitivity
    # contribution at the dose is the ddelta row, d(delta)/d(eta_f) = amt*dF.
    # The analytic jump-mode sensitivity must match a central difference of the
    # central state wrt eta_f; the fd-mode sens ODE alone (no jump) must NOT.
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- 1
      f(depot)    <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0, tcl = 1, tv = 2, tf = 0.3, eta_ka = 0, eta_f = 0)
    e <- et(amt = 100, cmt = "depot")
    e <- et(e, seq(0, 12, 2))
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = c("eta_ka", "eta_f"), eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")
    analytic <- sj[["rx__sens_central_BY_eta_f__"]]
    h <- 1e-4
    pp <- pars; pp["eta_f"] <- pars["eta_f"] + h
    pm <- pars; pm["eta_f"] <- pars["eta_f"] - h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    # analytic jump matches FD to FD precision
    expect_equal(analytic, fd, tolerance = 1e-4)
    # fd-mode sens ODE alone is missing the dF contribution -> clearly different
    sfd <- .central(pars, "fd")[["rx__sens_central_BY_eta_f__"]]
    expect_gt(max(abs(sfd - fd)), 1)
  })

  test_that("additive-bolus lag jump (dtau row) matches finite differences", {
    # alag = exp(tlag + eta_lag) depends on eta_lag, F constant: the jump is the
    # dtau row, -J[k][c]*delta*d(alag)/d(eta_lag), with J taken from a central
    # difference of dydt.  Observe AWAY from the (lagged) dose time, since at the
    # exact event time the sensitivity is discontinuous and a central FD straddles
    # the jump (the analytic value is the correct right-limit).
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- exp(tlag + eta_lag)
      f(depot)    <- expit(tf)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tf = 0.5, tlag = 0,
              eta_ka = 0, eta_lag = 0)
    e <- et(amt = 100, cmt = "depot")
    e <- et(e, seq(0.5, 12, 1)) # off the dose time (alag = 1)
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = c("eta_ka", "eta_lag"), eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")
    h <- 1e-4
    pp <- pars; pp["eta_lag"] <- h
    pm <- pars; pm["eta_lag"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    expect_equal(sj[["rx__sens_central_BY_eta_lag__"]], fd, tolerance = 1e-4)
    # eta_ka has no lag/F dependence -> no jump; must still match FD
    ppk <- pars; ppk["eta_ka"] <- h
    pmk <- pars; pmk["eta_ka"] <- -h
    fdK <- (.central(ppk, "fd")$central - .central(pmk, "fd")$central) / (2 * h)
    expect_equal(sj[["rx__sens_central_BY_eta_ka__"]], fdK, tolerance = 1e-4)
  })

  test_that("additive-bolus jumps accumulate across a multi-dose regimen", {
    # alag(depot)=exp(tlag+eta_lag), F=expit(tf+eta_f): every dose contributes
    # both a dtau and a ddelta jump.  The rows use method "add", so the jumps
    # must accumulate across the regimen.  A multi-dose schedule must still match
    # a central difference of the solution wrt each eta.
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- exp(tlag + eta_lag)
      f(depot)    <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tf = 0.3, tlag = 0,
              eta_ka = 0, eta_lag = 0, eta_f = 0)
    e <- et(amt = 100, cmt = "depot", ii = 6, addl = 3) |>
      et(seq(0.5, 30, 1.5))
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = c("eta_lag", "eta_f"), eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")
    h <- 1e-4
    for (.eta in c("eta_lag", "eta_f")) {
      pp <- pars; pp[.eta] <- h
      pm <- pars; pm[.eta] <- -h
      fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
      expect_equal(sj[[paste0("rx__sens_central_BY_", .eta, "__")]], fd,
                   tolerance = 1e-3)
    }
  })

  test_that("replacement event jump (dp_j -> 0) matches finite differences", {
    # Replace central with a constant (50) at t=5 via an evid=5 event.  The
    # replaced state's value no longer depends on eta_ka, so d(central)/d(eta_ka)
    # must jump to 0 at the event and rebuild afterwards.  The analytic jump must
    # match a central difference of the solution; the fd-mode sensitivity ODE
    # (which never zeroes the sens compartment) must NOT.
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, eta_ka = 0)
    e <- et(amt = 100, cmt = "depot") |>
      et(time = 5, amt = 50, cmt = "central", evid = 5) |>
      et(seq(0.5, 12, 1))
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = "eta_ka", eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")
    h <- 1e-4
    pp <- pars; pp["eta_ka"] <- h
    pm <- pars; pm["eta_ka"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    # observations at/after the replace must match FD (analytic jump correct)
    .post <- sj$time >= 5
    expect_equal(sj[["rx__sens_central_BY_eta_ka__"]][.post], fd[.post],
                 tolerance = 1e-4)
    # fd-mode sens ODE alone never zeroes the replaced state's sens -> differs
    sfd <- .central(pars, "fd")[["rx__sens_central_BY_eta_ka__"]]
    expect_gt(max(abs(sfd[.post] - fd[.post])), 1)
  })

  test_that("multiplicative event jump (dp_j *= alpha) matches finite differences", {
    # Multiply central by alpha=0.5 at t=5 via an evid=6 event.  d(central)/deta
    # must be scaled by the same 0.5 at the event.  Analytic jump matches FD; the
    # fd-mode sens ODE (no scaling of the sens compartment) does not.
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, eta_ka = 0)
    e <- et(amt = 100, cmt = "depot") |>
      et(time = 5, amt = 0.5, cmt = "central", evid = 6) |>
      et(seq(0.5, 12, 1))
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = "eta_ka", eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")
    h <- 1e-4
    pp <- pars; pp["eta_ka"] <- h
    pm <- pars; pm["eta_ka"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    .post <- sj$time >= 5
    expect_equal(sj[["rx__sens_central_BY_eta_ka__"]][.post], fd[.post],
                 tolerance = 1e-4)
    sfd <- .central(pars, "fd")[["rx__sens_central_BY_eta_ka__"]]
    expect_gt(max(abs(sfd[.post] - fd[.post])), 0.1)
  })

  test_that("replace/multiply zero the 2nd-order (Hessian) compartment too", {
    # Extends Phase F's additive-bolus-only 2nd-order jump (the "dx1/dp_j"
    # row, NOT the dtau row) to replace/multiply: a constant replacement
    # value has an identically zero 2nd derivative wrt any parameter pair
    # (same reasoning as 1st order), and alpha (treated as parameter-fixed
    # for this row, same convention as 1st order) scales the 2nd-order
    # compartment the same way. Validated against a finite difference of the
    # analytic 1st-order sensitivity (the plan's standard 2nd-order
    # validation strategy -- a raw 2nd central difference of the solution is
    # too imprecise to catch a jump-sized correction).
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2)
    m1 <- rxode2(.mod, calcSens = "tka", eventSens = "jump")
    m2 <- rxode2(.mod, calcSens = "tka", calcSens2 = "tka", eventSens = "jump")
    h <- 1e-4
    pp <- pars; pp["tka"] <- pars["tka"] + h
    pm <- pars; pm["tka"] <- pars["tka"] - h

    e_replace <- et(amt = 100, cmt = "depot") |>
      et(time = 5, amt = 50, cmt = "central", evid = 5) |>
      et(seq(0.5, 12, 1))
    fd_replace <- (rxSolve(m1, e_replace, pp)$rx__sens_central_BY_tka__ -
      rxSolve(m1, e_replace, pm)$rx__sens_central_BY_tka__) / (2 * h)
    s_replace <- rxSolve(m2, e_replace, pars)
    expect_equal(s_replace$rx__sens_central_BY_tka_BY_tka__, fd_replace, tolerance = 1e-3)

    e_mult <- et(amt = 100, cmt = "depot") |>
      et(time = 5, amt = 0.5, cmt = "central", evid = 6) |>
      et(seq(0.5, 12, 1))
    fd_mult <- (rxSolve(m1, e_mult, pp)$rx__sens_central_BY_tka__ -
      rxSolve(m1, e_mult, pm)$rx__sens_central_BY_tka__) / (2 * h)
    s_mult <- rxSolve(m2, e_mult, pars)
    expect_equal(s_mult$rx__sens_central_BY_tka_BY_tka__, fd_mult, tolerance = 1e-3)
  })

  test_that("raw event-table replace with a modeled lag gets a dtau row (Phase B, B2)", {
    # Regression (Phase B "B2" gap, plan Section 4): a raw et(..., evid=5)
    # replace record on a compartment with a MODELED alag was found to
    # completely omit the dx1/dtau row (paper Table 1) -- the modeled lag
    # correctly shifts the event's OWN time (confirmed: alag applies to
    # evid=5/6 records exactly like normal doses), but the sensitivity was
    # silently exactly zero even though the true value is clearly nonzero
    # (confirmed via a hand-derived closed form and FD, ~20.2 here). The
    # in-model replace()/multiply() plugins were previously found NOT to need
    # this row (captured-dosing re-emits correctly) -- this gap is specific
    # to raw event-table evid=5/6 records.
    .mod <- "
      cl <- exp(tcl); v <- exp(tv)
      alag(central) <- exp(tlag + eta_lag)
      d/dt(central) <- -cl / v * central
    "
    pars <- c(tcl = 1, tv = 2, tlag = log(1.1), eta_lag = 0)
    e <- et(time = 5, amt = 50, cmt = "central", evid = 5) |> et(seq(0, 10, 0.05))
    mj <- rxode2(.mod, calcSens = "eta_lag", eventSens = "jump")
    mfd <- rxode2(.mod, calcSens = "eta_lag", eventSens = "fd")
    sj <- rxSolve(mj, e, pars)
    h <- 1e-4
    pp <- pars; pp["eta_lag"] <- pars["eta_lag"] + h
    pm <- pars; pm["eta_lag"] <- pars["eta_lag"] - h
    # tight atol/rtol on the FD reference solves: the default LSODA tolerance
    # is loose enough, relative to h, that its own step-to-step numerical
    # noise dominates a naive central difference here (~1% spurious offset
    # observed with default tolerances; confirmed to vanish under atol/rtol
    # 1e-12 -- same class of pitfall as too-small an h, not a real signal).
    fd <- (rxSolve(mfd, e, pp, atol = 1e-12, rtol = 1e-12)$central -
             rxSolve(mfd, e, pm, atol = 1e-12, rtol = 1e-12)$central) / (2 * h)
    # skip the sample landing exactly on the lagged event time (t=6.1): a
    # central FD straddling a true discontinuity is a known artifact, not a
    # correctness signal (documented in the plan's infusion-jump validation
    # note; the same subtlety applies here).
    .post <- sj$time >= 6.15 & sj$time < 10
    expect_equal(sj[["rx__sens_central_BY_eta_lag__"]][.post], fd[.post], tolerance = 1e-3)
    expect_gt(max(abs(sj[["rx__sens_central_BY_eta_lag__"]])), 15)
  })

  test_that("raw event-table multiply with a modeled lag gets a dtau row (Phase B, B2)", {
    # Same gap as above, for evid=6 (multiply): checked on a compartment OTHER
    # than the multiplied one (depot's multiply shifts central's future
    # absorption input), since a single-compartment homogeneous-linear decay
    # makes the paper's dx1/dtau row (J_cc*x1 - f_c) identically (and
    # correctly) zero -- not a useful regression target for THIS row.
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- exp(tlag + eta_lag)
      depot(0) <- 100
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.3, tcl = 1, tv = 2, tlag = log(1.1), eta_lag = 0)
    e <- et(time = 5, amt = 0.6, cmt = "depot", evid = 6) |> et(seq(0, 15, 0.1))
    mj <- rxode2(.mod, calcSens = "eta_lag", eventSens = "jump")
    mfd <- rxode2(.mod, calcSens = "eta_lag", eventSens = "fd")
    sj <- rxSolve(mj, e, pars)
    h <- 1e-3
    pp <- pars; pp["eta_lag"] <- pars["eta_lag"] + h
    pm <- pars; pm["eta_lag"] <- pars["eta_lag"] - h
    sp <- rxSolve(mfd, e, pp, atol = 1e-11, rtol = 1e-11)
    sm <- rxSolve(mfd, e, pm, atol = 1e-11, rtol = 1e-11)
    fd <- (sp$central - sm$central) / (2 * h)
    .post <- sj$time >= 6.15 & sj$time < 15
    expect_equal(sj[["rx__sens_central_BY_eta_lag__"]][.post], fd[.post], tolerance = 1e-3)
    expect_gt(max(abs(sj[["rx__sens_central_BY_eta_lag__"]])), 0.01)
  })

  test_that("constant-rate infusion needs no jump (sens ODE alone is correct)", {
    # An infusion rate that does not depend on any parameter (and a fixed start
    # time) enters dydt as a parameter-independent forcing, so the symbolic
    # sensitivity ODE already captures it -- no jump contribution is required.
    # Both modes must agree with each other and with a finite difference; this
    # also guards that future infusion jumps stay zero in the constant-rate case.
    .mod <- "
      ka <- exp(tka + eta_ka)
      cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, eta_ka = 0)
    e <- et(amt = 100, rate = 20, cmt = "depot") |> et(seq(0.5, 12, 1))
    .central <- function(p, mode) {
      m <- rxode2(.mod, calcSens = "eta_ka", eventSens = mode)
      rxSolve(m, e, p)
    }
    sj <- .central(pars, "jump")[["rx__sens_central_BY_eta_ka__"]]
    sf <- .central(pars, "fd")[["rx__sens_central_BY_eta_ka__"]]
    h <- 1e-4
    pp <- pars; pp["eta_ka"] <- h
    pm <- pars; pm["eta_ka"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    expect_equal(sj, sf)                    # jump adds nothing for constant rate
    expect_equal(sj, fd, tolerance = 1e-3)  # and both match the finite difference
  })

  test_that("in-model evid_() dosing plugins match finite differences (jump)", {
    # bolus(), replace(), multiply(), reset() and constant-rate infuse()/
    # infuseDur() pushed from inside the model must all yield jump-mode
    # sensitivities that match a central difference of the solution.  (Modeled
    # rate/dur -- where the infusion magnitude depends on a parameter -- is the
    # separate continuous-forcing case covered elsewhere.)
    .skel <- function(dose) paste0("
      ka <- exp(tka + eta_ka); cl <- exp(tcl); v <- exp(tv); rt <- 5
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
      if (t >= rt && t < rt + 0.4) { ", dose, " }")
    .doses <- c(
      bolus      = "bolus(50, central, 0, 0, 0)",
      infuse     = "infuse(100, 50, central, 0, 0, 0)",
      infuseDur  = "infuseDur(100, 2, central, 0, 0, 0)",
      replace    = "if (central > 0) replace(40, central)",
      multiply   = "if (central > 0) multiply(0.5, central)",
      reset      = "reset()"
    )
    pars <- c(tka = 0.2, tcl = 1, tv = 2, eta_ka = 0)
    e <- et(amt = 100, cmt = "depot") |> et(seq(0.5, 14, 0.5))
    h <- 1e-4
    for (.nm in names(.doses)) {
      .mod <- .skel(.doses[[.nm]])
      .central <- function(p, mode) {
        rxSolve(rxode2(.mod, calcSens = "eta_ka", eventSens = mode), e, p)
      }
      sj <- .central(pars, "jump")
      pp <- pars; pp["eta_ka"] <- h
      pm <- pars; pm["eta_ka"] <- -h
      fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
      .post <- sj$time >= 5
      expect_equal(sj[["rx__sens_central_BY_eta_ka__"]][.post], fd[.post],
                   tolerance = 1e-3,
                   info = paste0("plugin: ", .nm))
    }
  })

  test_that("modeled-rate infusion continuous forcing matches finite differences", {
    # rate(central) = exp(tr + eta_r) with a rate=-1 dose: the infusion rate
    # depends on eta_r.  The rate is solver-applied forcing (not a symbolic term
    # in f), so the symbolic sens ODE misses d(rate)/dp -- the jump injects it as
    # a forcing on the sensitivity compartment over the infusion.  The analytic
    # jump must match a central difference; the fd-mode sens ODE must NOT.
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      rate(central) <- exp(tr + eta_r)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tr = 0.7, eta_r = 0)
    e <- et(amt = 100, cmt = "central", rate = -1) |> et(seq(0.25, 14, 0.25))
    .central <- function(p, mode) {
      rxSolve(rxode2(.mod, calcSens = "eta_r", eventSens = mode), e, p)
    }
    sj <- .central(pars, "jump")[["rx__sens_central_BY_eta_r__"]]
    sf <- .central(pars, "fd")[["rx__sens_central_BY_eta_r__"]]
    h <- 1e-4
    pp <- pars; pp["eta_r"] <- h
    pm <- pars; pm["eta_r"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    expect_equal(sj, fd, tolerance = 1e-3)
    expect_gt(max(abs(sf - fd)), 1)   # fd-mode misses the forcing sensitivity
  })

  test_that("modeled-duration infusion (forcing + moving boundary) matches FD", {
    # dur(central) = exp(tr + eta_r) with a rate=-2 dose: rate = amt/dur depends
    # on eta_r AND the infusion end tau2 = tau1 + dur(eta_r) moves with eta_r.
    # The jump needs BOTH the continuous d(rate)/dp forcing and the moving-
    # boundary state jump rate*d(dur)/dp at the infusion end.  Validated vs FD;
    # the fd-mode sens ODE (neither contribution) is badly wrong.
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      dur(central) <- exp(tr + eta_r)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tr = 0.7, eta_r = 0)
    e <- et(amt = 100, cmt = "central", rate = -2) |> et(seq(0.25, 14, 0.25))
    .central <- function(p, mode) {
      rxSolve(rxode2(.mod, calcSens = "eta_r", eventSens = mode), e, p)
    }
    sj <- .central(pars, "jump")[["rx__sens_central_BY_eta_r__"]]
    sf <- .central(pars, "fd")[["rx__sens_central_BY_eta_r__"]]
    h <- 1e-4
    pp <- pars; pp["eta_r"] <- h
    pm <- pars; pm["eta_r"] <- -h
    fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
    expect_equal(sj, fd, tolerance = 1e-2)
    expect_gt(max(abs(sf - fd)), 1)   # fd-mode misses forcing + boundary
  })

  test_that("modeled-dur infusion with estimated F and duration matches FD", {
    # dur(central)=exp(tr+eta_r) AND f(central)=expit(tf+eta_f): rate=F*amt/dur
    # depends on BOTH parameters.  The continuous forcing carries both
    # d(rate)/dp pieces ((amt/dur)*dF and -(rate/dur)*dDur) and the moving
    # boundary applies only to the duration (tau2 = tau1 + dur, independent of F).
    # Both eta sensitivities must match a central difference.
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      dur(central) <- exp(tr + eta_r)
      f(central)   <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tr = 0.7, tf = 0.3,
              eta_r = 0, eta_f = 0)
    e <- et(amt = 100, cmt = "central", rate = -2) |> et(seq(0.25, 14, 0.25))
    .central <- function(p, mode) {
      rxSolve(rxode2(.mod, calcSens = c("eta_r", "eta_f"), eventSens = mode), e, p)
    }
    sj <- .central(pars, "jump")
    h <- 1e-4
    for (.eta in c("eta_r", "eta_f")) {
      pp <- pars; pp[.eta] <- h
      pm <- pars; pm[.eta] <- -h
      fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
      expect_equal(sj[[paste0("rx__sens_central_BY_", .eta, "__")]], fd,
                   tolerance = 1e-2, info = paste0("param: ", .eta))
    }
  })

  test_that("infusion with a modeled lag matches FD (all infusion types)", {
    # alag(central) = exp(tl + eta_l): the whole infusion window [tau1, tau2]
    # shifts by d(alag)/dp (tau1 = t0 + alag, tau2 = tau1 + dur), giving moving
    # start/stop boundary jumps [S] = -/+ rate*d(alag)/dp.  Validated for fixed-
    # rate, modeled-rate and modeled-duration infusions.  An irregular grid keeps
    # observations off the (discontinuous) boundary times so the FD is well-posed.
    .obs <- seq(0.1, 14, 0.37)
    .lagLine <- "alag(central) <- exp(tl + eta_l)"
    .ode <- "d/dt(depot) <- -ka * depot\n      d/dt(central) <- ka * depot - cl / v * central"
    .cases <- list(
      fixed = list(extra = "", dose = et(amt = 100, rate = 30, cmt = "central"),
                   p = c(tka = 0.2, tcl = 1, tv = 2, tl = log(1.1), eta_l = 0)),
      mrate = list(extra = "rate(central) <- exp(tr)",
                   dose = et(amt = 100, rate = -1, cmt = "central"),
                   p = c(tka = 0.2, tcl = 1, tv = 2, tl = log(1.1), tr = log(30), eta_l = 0)),
      mdur  = list(extra = "dur(central) <- exp(td)",
                   dose = et(amt = 100, rate = -2, cmt = "central"),
                   p = c(tka = 0.2, tcl = 1, tv = 2, tl = log(1.1), td = log(3), eta_l = 0))
    )
    h <- 1e-4
    for (.nm in names(.cases)) {
      .cs <- .cases[[.nm]]
      .mod <- paste("ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)",
                    .lagLine, .cs$extra, .ode, sep = "\n      ")
      e <- .cs$dose |> et(.obs)
      .central <- function(p, mode) {
        rxSolve(rxode2(.mod, calcSens = "eta_l", eventSens = mode), e, p)
      }
      sj <- .central(.cs$p, "jump")[["rx__sens_central_BY_eta_l__"]]
      pp <- .cs$p; pp["eta_l"] <- .cs$p["eta_l"] + h
      pm <- .cs$p; pm["eta_l"] <- .cs$p["eta_l"] - h
      fd <- (.central(pp, "fd")$central - .central(pm, "fd")$central) / (2 * h)
      expect_equal(sj, fd, tolerance = 1e-2, info = paste0("infusion: ", .nm))
    }
  })

  test_that("jump sensitivities are correct per subject (population solve)", {
    # FOCEi solves many subjects at once (in parallel).  Each subject has its
    # own etas, so its own lagged dose time and bioavailability; the jump
    # injection must use per-subject state/dose.  Three subjects with distinct
    # etas must each match a per-subject central difference.
    .mod <- "
      ka <- exp(tka + eta_ka); cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- exp(tlag + eta_lag)
      f(depot)    <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    th <- c(tka = 0.2, tcl = 1, tv = 2, tf = 0.3, tlag = log(1.1))
    ev <- data.frame(eta_ka = c(0, 0.3, -0.2),
                     eta_lag = c(0, 0.1, -0.15),
                     eta_f = c(0, -0.2, 0.25))
    e <- et(amt = 100, cmt = "depot", id = 1:3) |> et(seq(0.13, 12, 0.41), id = 1:3)
    mj <- rxode2(.mod, calcSens = c("eta_lag", "eta_f"), eventSens = "jump")
    mfd <- rxode2(.mod, calcSens = c("eta_lag", "eta_f"), eventSens = "fd")
    sj <- rxSolve(mj, e, th, iCov = ev)
    h <- 1e-4
    for (.sp in c("eta_lag", "eta_f")) {
      evp <- ev; evp[[.sp]] <- evp[[.sp]] + h
      evm <- ev; evm[[.sp]] <- evm[[.sp]] - h
      fd <- (rxSolve(mfd, e, th, iCov = evp)$central -
             rxSolve(mfd, e, th, iCov = evm)$central) / (2 * h)
      expect_equal(sj[[paste0("rx__sens_central_BY_", .sp, "__")]], fd,
                   tolerance = 1e-2, info = paste0("param: ", .sp))
    }
  })

  test_that(".rxEventSensMap exposes a second-order (Hessian) compartment map", {
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      f(depot) <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    m <- rxode2(.mod, calcSens = "eta_f", calcSens2 = "eta_f")
    im <- .rxEventSensMap(m)
    expect_false(is.null(im$map2))
    # one (state, p, q) row per physical state for the single (eta_f, eta_f) pair
    expect_setequal(im$map2$state, c("depot", "central"))
    expect_true(all(im$map2$p == "eta_f" & im$map2$q == "eta_f"))
    # the depot 2nd-order compartment exists and comes after the 1st-order ones
    .row <- im$map2[im$map2$state == "depot", ]
    expect_true(.row$sensCmt > nrow(im$map))
  })

  test_that(".rxEventSensCLines indexes multi-parameter calcSens2 consistently with calcSens", {
    # Regression: .rxEventSensMap() used to sort `map2` alphabetically by
    # (p, q, stateCmt); .rxEventSensCLines()'s .qIdx (built from
    # unique(map2$q)) then inherited that alphabetical order instead of
    # calcSens2's *as-passed* order (matching the compiled compartment
    # layout rxExpandSens2_ actually used) -- for any calcSens2 with more
    # than one parameter where the as-passed order isn't alphabetical, the
    # 2nd-order derivative values were written into the WRONG compartment
    # (found while validating the 2nd-order infusion jump, 2026-06-30;
    # masked in every earlier test, which all used a single-parameter
    # calcSens2 where reordering is a no-op). trate/tlag deliberately spelled
    # so alphabetical order ("tlag","trate") differs from the as-passed order
    # ("trate","tlag").
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- exp(tlag)
      rate(depot) <- exp(trate)
      d/dt(depot)   = -ka * depot
      d/dt(central) =  ka * depot - cl/v * central
    "
    m <- rxode2(.mod, calcSens = c("trate", "tlag"), calcSens2 = c("trate", "tlag"),
                eventSens = "jump")
    info <- m$eventSensInfo
    expect_equal(names(info$map$sensParams), NULL) # sanity: sensParams unnamed char vec
    expect_equal(info$map$sensParams, c("trate", "tlag"))
    # .qIdx must be built in the SAME (as-passed) order as .pIdx/sensParams,
    # not alphabetically -- this is the actual bug: previously "tlag" (unique(map2$q)'s
    # alphabetically-first value) would come first here instead of "trate".
    expect_equal(unique(info$map$map2$q), c("trate", "tlag"))
    expect_equal(unique(info$map$map2$p), c("trate", "tlag"))
  })

  test_that(".rxEventSensD2Expr is the correct second total derivative", {
    # d2(F)/d(eta_f)^2 for F = expit(tf + eta_f) must equal expit''(tf+eta_f)
    # = p(1-p)(1-2p).  State-independent F -> only the direct second partial.
    m <- rxode2("ka<-exp(tka)\nf(depot)<-expit(tf+eta_f)\nd/dt(depot)<- -ka*depot\nd/dt(central)<-ka*depot-central",
                calcSens = "eta_f", calcSens2 = "eta_f")
    mdl <- .rxLoadPrune(m)
    e2 <- .rxEventSensD2Expr(get("rx_f_depot_", envir = mdl), "eta_f", "eta_f",
                             .rxEventSensMap(m)$states)
    expect_true(nzchar(e2) && e2 != "0")
    val <- eval(parse(text = e2),
                list(eta_f = 0, tf = 0.3, Rx_pow_di = function(a, b) a^b,
                     expit = function(x) 1 / (1 + exp(-x))))
    p <- 1 / (1 + exp(-0.3))
    expect_equal(val, p * (1 - p) * (1 - 2 * p), tolerance = 1e-8)
  })

  test_that("calcSens2 generates correct second-order sensitivity ODEs", {
    # rxode2(..., calcSens2=) emits the 2nd-order sensitivity compartments
    # rx__sens_<state>_BY_<p>_BY_<q>__ (the Hessian path).  For a parameter that
    # does not touch dosing there is no jump, so the 2nd-order sensitivity comes
    # purely from the continuous variational ODEs.  S^{pq} = d(S^p)/dq, so it must
    # match a (precise) first central difference of the first-order sensitivity.
    .mod <- "
      ka <- exp(tka + eta_ka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, eta_ka = 0)
    e <- et(amt = 100, cmt = "depot") |> et(seq(0.5, 12, 0.5))
    m1 <- rxode2(.mod, calcSens = "eta_ka")
    m2 <- rxode2(.mod, calcSens = "eta_ka", calcSens2 = "eta_ka")
    expect_true("rx__sens_central_BY_eta_ka_BY_eta_ka__" %in% rxModelVars(m2)$state)
    s2 <- rxSolve(m2, e, pars)[["rx__sens_central_BY_eta_ka_BY_eta_ka__"]]
    h <- 1e-5
    .s1 <- function(p) rxSolve(m1, e, p)[["rx__sens_central_BY_eta_ka__"]]
    pp <- pars; pp["eta_ka"] <- h
    pm <- pars; pm["eta_ka"] <- -h
    fd <- (.s1(pp) - .s1(pm)) / (2 * h)
    expect_equal(s2, fd, tolerance = 1e-2)
  })

  test_that("calcSens3 generates correct third-order sensitivity ODEs (Phase H0)", {
    # rxode2(..., calcSens2=, calcSens3=) emits the 3rd-order sensitivity
    # compartments rx__sens_<state>_BY_<p>_BY_<q>_BY_<r>__ via
    # rxExpandSens3_() (PR #1092), exposed at the rxode2() build level for
    # the first time here (previously only reachable through the DDE-
    # specific delay() sensitivity path). No jump-sensitivity content is
    # involved -- this is pure continuous-ODE 3rd-order sensitivity, so it
    # is validated standalone against a nested finite difference of the
    # analytic 2nd-order sensitivity (the plan's H0 acceptance criterion),
    # with calcSens2 == calcSens3 == calcSens (the common "full Hessian/
    # third-order" case, matching how calcSens2 is used everywhere else).
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2)
    e <- et(amt = 100, cmt = "depot") |> et(seq(0.5, 12, 1))
    m2 <- rxode2(.mod, calcSens = "tka", calcSens2 = "tka")
    m3 <- rxode2(.mod, calcSens = "tka", calcSens2 = "tka", calcSens3 = "tka")
    expect_true("rx__sens_central_BY_tka_BY_tka_BY_tka__" %in% rxModelVars(m3)$state)
    s3 <- rxSolve(m3, e, pars)[["rx__sens_central_BY_tka_BY_tka_BY_tka__"]]
    h <- 1e-4
    .s2 <- function(p) rxSolve(m2, e, p)[["rx__sens_central_BY_tka_BY_tka__"]]
    pp <- pars; pp["tka"] <- pars["tka"] + h
    pm <- pars; pm["tka"] <- pars["tka"] - h
    fd <- (.s2(pp) - .s2(pm)) / (2 * h)
    expect_equal(s3, fd, tolerance = 1e-3)

    # calcSens3 requires calcSens2
    expect_error(rxode2(.mod, calcSens = "tka", calcSens3 = "tka"))
  })

  test_that("second-order additive-bolus F jump matches finite differences", {
    # F = expit(tf + eta_f) feeds the dose only (ka/cl/v independent of eta_f),
    # so the second-order sensitivity S^{ff} of the dosed compartment is *entirely*
    # the explicit jump amt*d2F/d eta_f^2 -- without it S^{ff} is exactly 0.  The
    # jump-mode 2nd-order sensitivity must match d(S^f)/d eta_f (a precise first
    # central difference of the jump-correct first-order sensitivity).
    .mod <- "
      ka <- exp(tka); cl <- exp(tcl); v <- exp(tv)
      alag(depot) <- 1
      f(depot)    <- expit(tf + eta_f)
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
    "
    pars <- c(tka = 0.2, tcl = 1, tv = 2, tf = 0.3, eta_f = 0)
    e <- et(amt = 100, cmt = "depot") |> et(seq(0.5, 12, 0.5))
    m1 <- rxode2(.mod, calcSens = "eta_f", eventSens = "jump")
    m2 <- rxode2(.mod, calcSens = "eta_f", calcSens2 = "eta_f", eventSens = "jump")
    s2 <- rxSolve(m2, e, pars)[["rx__sens_central_BY_eta_f_BY_eta_f__"]]
    h <- 1e-5
    .s1 <- function(p) rxSolve(m1, e, p)[["rx__sens_central_BY_eta_f__"]]
    pp <- pars; pp["eta_f"] <- h
    pm <- pars; pm["eta_f"] <- -h
    fd <- (.s1(pp) - .s1(pm)) / (2 * h)
    expect_equal(s2, fd, tolerance = 1e-2)
    # without the 2nd-order jump the contribution would be entirely missing
    expect_gt(max(abs(fd)), 1)
  })

  test_that("additive-bolus F jump fires for indexed THETA[n]/ETA[n] params", {
    # The nlmixr2 FOCEi inner model writes sensitivities wrt the indexed
    # parameters ETA[n]/THETA[n] (compartments rx__sens_<state>_BY_ETA_n___).
    # rxFromSE renders these as `ETA[3]`, whose all.vars() collapses to "ETA" and
    # never matched the SE-mangled sensParam "ETA_3_" -- the dosing-parameter
    # derivatives were silently dropped and no jump was injected.  Guard both the
    # symbolic detection (free_symbols) and the codegen rewrite (ETA[n]->_ETA_n_).
    code <- paste(
      "param(THETA[1],THETA[2],THETA[3],THETA[4],ETA[1],ETA[3]);",
      "cmt(depot);",
      "cmt(central);",
      "f(depot)=1/(1+exp(-(ETA[3]+THETA[4])));",
      "d/dt(depot)=-exp(ETA[1]+THETA[1])*depot;",
      "d/dt(central)=exp(ETA[1]+THETA[1])*depot-exp(THETA[2]-THETA[3])*central;",
      "d/dt(rx__sens_depot_BY_ETA_3___)=-exp(ETA[1]+THETA[1])*rx__sens_depot_BY_ETA_3___;",
      "d/dt(rx__sens_central_BY_ETA_3___)=exp(ETA[1]+THETA[1])*rx__sens_depot_BY_ETA_3___-exp(THETA[2]-THETA[3])*rx__sens_central_BY_ETA_3___;",
      sep = "\n")
    mj <- rxode2(code, eventSens = "jump")
    # the F derivative wrt ETA[3] must be detected (non-empty derivs table)
    .df <- mj$eventSensInfo$derivs$f
    expect_true(nrow(.df) >= 1L)
    expect_true("ETA_3_" %in% .df$param)

    p <- c("THETA[1]" = 0.45, "THETA[2]" = 1, "THETA[3]" = 3.45,
           "THETA[4]" = 0.9, "ETA[1]" = 0, "ETA[3]" = 0.2)
    # sample immediately after the dose to capture the jump peak before decay
    e <- et(amt = 100, cmt = "depot") |> et(c(1e-4, seq(0.13, 24, length.out = 30)))
    s <- rxSolve(mj, p, e, atol = 1e-10, rtol = 1e-10)
    # the additive-bolus F jump makes the depot sensitivity wrt ETA[3] non-zero;
    # analytic value at t=0+ is amt*F*(1-F) with F = expit(0.2 + 0.9).
    .F <- 1 / (1 + exp(-(0.2 + 0.9)))
    expect_equal(max(s[["rx__sens_depot_BY_ETA_3___"]]), 100 * .F * (1 - .F),
                 tolerance = 1e-2)
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

  test_that("reproduces the paper's PK/PD IMAX infusion example (dEffect/dtlag, ddose, dtinf)", {
    # Kaschek & Fidler, "Forward Sensitivity Equations in the Presence of
    # Events" (~/src/EventSensitivities/paper.tex, Sec. Example): a depot-
    # absorption model with a zero-order infusion into the depot compartment,
    # parameterized directly by lag time (tlag), infusion duration (tinf), and
    # dose (not etas) -- alag(depot)=tlag, dur(depot)=tinf, f(depot)=doseAmt
    # (bioavailability standing in for the dose, routed through the
    # duration-fixed F/amt convention: rate = F*amt/dur = doseAmt*1/tinf,
    # matching the paper's r1 = dose/tinf exactly). Effect is a downstream LHS
    # of central, so dEffect/dp = dEffect/dcentral * dcentral/dp (closed-form
    # chain rule below) using the jump-computed rx__sens_central_BY_p__.
    .mod <- "
      ka <- exp(lka)
      cl <- exp(lcl)
      v  <- exp(lv)
      imax <- exp(limax)
      ic50 <- exp(lic50)
      e0 <- exp(le0)
      alag(depot) <- tlag
      dur(depot)  <- tinf
      f(depot)    <- doseAmt
      d/dt(depot)   <- -ka * depot
      d/dt(central) <-  ka * depot - cl / v * central
      effect <- e0 * (1 - (central / v) * imax / (ic50 + (central / v)))
    "
    pars <- c(
      lka = log(1), lcl = log(6), lv = log(60), limax = log(1), lic50 = log(1),
      le0 = log(15), tlag = 10, doseAmt = 200, tinf = 10
    )
    mj <- rxode2(.mod, calcSens = c("tlag", "doseAmt", "tinf"), eventSens = "jump")
    e <- et(amt = 1, cmt = "depot", rate = -2) |> et(seq(0, 60, by = 0.5))
    sj <- rxSolve(mj, e, pars)

    dEffect_dCentral <- with(sj, -e0 * imax * ic50 / (v * (ic50 + central / v)^2))
    d_tlag <- dEffect_dCentral * sj$rx__sens_central_BY_tlag__
    d_tinf <- dEffect_dCentral * sj$rx__sens_central_BY_tinf__
    d_dose <- dEffect_dCentral * sj$rx__sens_central_BY_doseAmt__

    # (1) matches a true finite difference of the solved "effect" output
    # (the plan's primary correctness gate, Section 5) for all three params.
    .h <- 1e-4
    .fd <- function(pname) {
      .pp <- pars; .pp[pname] <- pars[pname] + .h
      .pm <- pars; .pm[pname] <- pars[pname] - .h
      (rxSolve(mj, e, .pp)$effect - rxSolve(mj, e, .pm)$effect) / (2 * .h)
    }
    expect_equal(d_tlag, .fd("tlag"), tolerance = 1e-3)
    expect_equal(d_tinf, .fd("tinf"), tolerance = 1e-3)
    expect_equal(d_dose, .fd("doseAmt"), tolerance = 1e-3)

    # (2) qualitative claims from the paper (Sec. Example):
    # "higher doses lead to stronger inhibition" -- Effect decreases (relative
    # to E0) as dose increases, i.e. dEffect/ddose <= 0 everywhere.
    expect_true(all(d_dose <= 1e-8))
    # "a longer infusion time will decrease the inhibition during the
    # infusion and increase it afterwards. The same holds for [tlag]" --
    # Effect increases (dEffect/dp > 0) during [tlag, tlag+tinf] and
    # decreases (dEffect/dp < 0) after tlag+tinf, for both tlag and tinf.
    .during <- sj$time > pars["tlag"] + 0.5 & sj$time < pars["tlag"] + pars["tinf"] - 0.5
    .after <- sj$time > pars["tlag"] + pars["tinf"] + 0.5 & sj$time < 40
    expect_true(all(d_tinf[.during] > 0))
    expect_true(all(d_tinf[.after] < 0))
    expect_true(all(d_tlag[.during] > 0))
    expect_true(all(d_tlag[.after] < 0))
    # "the impact of the lag time is five times higher than the impact of the
    # infusion time" -- loose bound around the paper's approximate reading.
    .ratio <- max(abs(d_tlag)) / max(abs(d_tinf))
    expect_true(.ratio > 3 && .ratio < 10)
  })
})
