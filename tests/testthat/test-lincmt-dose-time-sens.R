rxTest({
  # linCmtB(which1 = -3): the dose-time (moving boundary) sensitivity of a
  # linCmt() model -- the derivative wrt a delay applied to every dose feeding
  # the linear system, i.e. what a modeled alag() on its dosed compartment
  # produces.  Chain-ruled with d(alag)/dp it is the modeled-lag sensitivity a
  # linCmt() compartment could not report before (nlmixr2/rxode2#1119).
  #
  # The system is linear and its whole input is delayed together, so
  # A(t; L) = A(t - L; 0) and the derivative is exactly -dA/dt.  Every case
  # below is checked against a central finite difference of the concentration.

  .p <- c(tcl = log(4), tv = log(20), tka = log(1.1), tq = log(2),
          tv2 = log(50), eta_lag = 0.1)

  .fdCol <- function(m, e, col, h = 1e-5) {
    .p1 <- .p; .p1[["eta_lag"]] <- .p[["eta_lag"]] + h
    .p0 <- .p; .p0[["eta_lag"]] <- .p[["eta_lag"]] - h
    (rxSolve(m, e, params = .p1)[[col]] - rxSolve(m, e, params = .p0)[[col]]) / (2 * h)
  }

  test_that("linCmtB(-3) dose-time sensitivity matches finite differences", {
    .cases <- list(
      list(
        nm = "1cmt IV bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, cmt = "central") |>
          add.sampling(seq(0.1, 12, 0.25))
      ),
      list(
        nm = "1cmt IV multiple bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = eventTable() |>
          add.dosing(dose = 100, nbr.doses = 4, dosing.interval = 6, cmt = "central") |>
          add.sampling(seq(0.1, 30, 0.25))
      ),
      list(
        nm = "1cmt IV steady-state bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = et(amt = 100, ii = 8, ss = 1, cmt = "central") |> et(seq(0.1, 8, 0.25))
      ),
      list(
        nm = "1cmt oral multiple bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); lag <- 2 * exp(eta_lag)
          alag(depot) <- lag
          cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
          dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
        }),
        e = eventTable() |>
          add.dosing(dose = 100, nbr.doses = 4, dosing.interval = 6, cmt = "depot") |>
          add.sampling(seq(0.1, 30, 0.25))
      ),
      list(
        nm = "2cmt IV bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); q <- exp(tq); v2 <- exp(tv2)
          lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 2, 2, 0, -1, -1, 1, cl, v, q, v2, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 2, 2, 0, -3, -3, 1, cl, v, q, v2, 0, 0, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, cmt = "central") |>
          add.sampling(seq(0.1, 24, 0.25))
      ),
      list(
        nm = "2cmt oral multiple bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); q <- exp(tq); v2 <- exp(tv2); ka <- exp(tka)
          lag <- 2 * exp(eta_lag)
          alag(depot) <- lag
          cp <- linCmtB(rx__PTR__, t, 3, 2, 1, -1, -1, 1, cl, v, q, v2, 0, 0, ka)
          dcp <- lag * linCmtB(rx__PTR__, t, 3, 2, 1, -3, -3, 1, cl, v, q, v2, 0, 0, ka)
        }),
        e = eventTable() |>
          add.dosing(dose = 100, nbr.doses = 3, dosing.interval = 8, cmt = "depot") |>
          add.sampling(seq(0.1, 30, 0.25))
      ),
      list(
        nm = "3cmt IV bolus",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); q <- exp(tq); v2 <- exp(tv2)
          q2 <- 1; v3 <- 100; lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 3, 3, 0, -1, -1, 1, cl, v, q, v2, q2, v3, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 3, 3, 0, -3, -3, 1, cl, v, q, v2, q2, v3, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, cmt = "central") |>
          add.sampling(seq(0.1, 24, 0.25))
      )
    )
    for (.c in .cases) {
      .s <- rxSolve(.c$m, .c$e, params = .p)
      expect_true(max(abs(.s$dcp)) > 1e-3, info = .c$nm)  # not trivially zero
      expect_equal(.s$dcp, .fdCol(.c$m, .c$e, "cp"),
                   tolerance = 1e-5, info = .c$nm)
    }
  })

  test_that("linCmtB(-3, cmt) gives the per-compartment amount derivative", {
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); lag <- 2 * exp(eta_lag)
      alag(depot) <- lag
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      dDepot <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, 0, 1, cl, v, 0, 0, 0, 0, ka)
      dCentral <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, 1, 1, cl, v, 0, 0, 0, 0, ka)
    })
    e <- eventTable() |> add.dosing(dose = 100, cmt = "depot") |>
      add.sampling(seq(0.1, 12, 0.25))
    s <- rxSolve(m, e, params = .p)
    expect_equal(s$dDepot, .fdCol(m, e, "depot"), tolerance = 1e-5)
    expect_equal(s$dCentral, .fdCol(m, e, "central"), tolerance = 1e-5)
    # the concentration form is the central amount scaled by the volume
    expect_equal(s$dCentral / exp(.p[["tv"]]), .fdCol(m, e, "cp"), tolerance = 1e-5)
  })

  test_that("linCmtB(-3) matches finite differences for an infusion (#1236)", {
    # dA/dt includes the infusion rate; ind->InfusionRate is only maintained
    # while solving, so the output pass (which recomputes the lhs from the
    # saved amounts after clearing the rates) recovers it from the per-idx
    # linCmtBRateSlot() cache written while genuinely solving instead.
    .cases <- list(
      list(
        nm = "1cmt IV single infusion",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, rate = 25, cmt = "central") |>
          add.sampling(seq(0.1, 12, 0.25))
      ),
      list(
        nm = "1cmt IV multiple infusions",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = eventTable() |>
          add.dosing(dose = 100, rate = 25, nbr.doses = 4, dosing.interval = 6, cmt = "central") |>
          add.sampling(seq(0.1, 30, 0.25))
      ),
      list(
        nm = "1cmt IV infusion specified by duration",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, dur = 4, cmt = "central") |>
          add.sampling(seq(0.1, 12, 0.25))
      ),
      list(
        nm = "1cmt oral infusion into depot",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); lag <- 2 * exp(eta_lag)
          alag(depot) <- lag
          cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
          dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
        }),
        e = eventTable() |> add.dosing(dose = 100, rate = 10, cmt = "depot") |>
          add.sampling(seq(0.1, 24, 0.25))
      ),
      list(
        nm = "2cmt IV infusion into central (with peripheral)",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); q <- exp(tq); v2 <- exp(tv2)
          lag <- 2 * exp(eta_lag)
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 2, 2, 0, -1, -1, 1, cl, v, q, v2, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 2, 2, 0, -3, -3, 1, cl, v, q, v2, 0, 0, 0)
        }),
        e = eventTable() |> add.dosing(dose = 100, rate = 10, cmt = "central") |>
          add.sampling(seq(0.1, 24, 0.25))
      ),
      list(
        nm = "linCmt() mixed with an ODE compartment (numLin < neq), infusion",
        m = rxode2({
          cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag); ke0 <- 0.5
          alag(central) <- lag
          cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
          dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
          d/dt(ce) <- (cp - ce) * ke0
        }),
        e = eventTable() |> add.dosing(dose = 100, rate = 25, cmt = "central") |>
          add.sampling(seq(0.1, 12, 0.25))
      )
    )
    for (.c in .cases) {
      .s <- rxSolve(.c$m, .c$e, params = .p)
      expect_false(any(is.na(.s$dcp)), info = .c$nm)
      expect_true(max(abs(.s$dcp)) > 1e-3, info = .c$nm)  # not trivially zero
      expect_equal(.s$dcp, .fdCol(.c$m, .c$e, "cp"),
                   tolerance = 1e-5, info = .c$nm)
    }
  })

  test_that("linCmtB(-3) matches finite differences for an infusion with method='dop853'", {
    # The default (liblsoda/advan) and dop853 solve drivers walk ind->idx the
    # same way for a linCmt()-only model (dense dop853 output, which does not,
    # is disabled for any model with numLin > 0 -- see rxData.cpp), but
    # exercise it explicitly since linCmtBRateSlot()'s cache is keyed on
    # ind->idx/solvedIdx/doSS, common state every driver sets.
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
      alag(central) <- lag
      cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
      dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
    })
    e <- eventTable() |> add.dosing(dose = 100, rate = 25, cmt = "central") |>
      add.sampling(seq(0.1, 12, 0.25))
    s <- rxSolve(m, e, params = .p, method = "dop853")
    expect_false(any(is.na(s$dcp)))
    h <- 1e-5
    p1 <- .p; p1[["eta_lag"]] <- .p[["eta_lag"]] + h
    p0 <- .p; p0[["eta_lag"]] <- .p[["eta_lag"]] - h
    fd <- (rxSolve(m, e, params = p1, method = "dop853")$cp -
             rxSolve(m, e, params = p0, method = "dop853")$cp) / (2 * h)
    expect_equal(s$dcp, fd, tolerance = 1e-5)
  })

  test_that("linCmtB(-3) reports NA for a steady-state infusion", {
    # A steady-state infusion establishes its amounts analytically
    # (handleSSinf8()/solveSSinf()); ind->InfusionRate afterward reflects
    # solve-order happenstance rather than a well-defined "rate at this
    # index", so -dA/dt is not trustworthy there -- refused (#1236). A
    # steady-state BOLUS is unaffected (never touches InfusionRate) and stays
    # exact -- see the "1cmt IV steady-state bolus" case above.
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
      alag(central) <- lag
      cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
      dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
    })
    .es <- list(
      "periodic ss" = et(amt = 100, rate = 25, ii = 24, ss = 1, cmt = "central") |>
        et(seq(0.1, 24, 0.25)),
      "continuous ss" = et(amt = 0, rate = 25, ss = 1, cmt = "central") |>
        et(seq(0.1, 24, 0.25))
    )
    for (.nm in names(.es)) {
      s <- rxSolve(m, .es[[.nm]], params = .p)
      expect_true(all(is.na(s$dcp)), info = .nm)
      expect_false(any(is.na(s$cp)), info = .nm)   # the model itself still solves
    }
  })

  test_that("linCmtB(-3) is exactly 0 when no dose reaches the lagged compartment", {
    # The identity -dA/dt is the derivative wrt a delay carried by every dose.
    # If this individual doses no lagged compartment its amounts do not depend
    # on the delay at all, so the derivative is exactly 0 -- NOT -dA/dt, which
    # is the (nonzero) derivative the undelayed doses would have had if they
    # were delayed.  The IV arm of a paired IV/oral design lands here
    # (nlmixr2/rxode2#1237).
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); lag <- 2 * exp(eta_lag)
      alag(depot) <- lag
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
    })
    e <- et(amt = 100, cmt = "central") |> et(seq(0.1, 24, 0.25))
    s <- rxSolve(m, e, params = .p)
    expect_false(any(is.na(s$dcp)))
    expect_equal(s$dcp, rep(0, length(s$dcp)))
    # the finite difference agrees: cp does not move with the lag parameter
    expect_equal(.fdCol(m, e, "cp"), rep(0, nrow(s)), tolerance = 1e-8)
  })

  test_that("linCmtB(-3) refuses a regimen mixing lagged and unlagged doses", {
    # A lagged depot plus an unlagged bolus into central is out of scope for
    # which1 = -3 (an unlagged dose is a lag of 0, still a different delay).
    # Which compartments the model lags is build-time information, but which
    # ones an individual doses is DATA, so this can only be caught while
    # solving -- report NA rather than the single-delay answer, which is
    # biased by the undelayed dose's contribution (nlmixr2/rxode2#1237).
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka); lag <- 2 * exp(eta_lag)
      alag(depot) <- lag
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
    })
    e <- et(amt = 100, cmt = "depot") |> et(amt = 50, cmt = "central", time = 0) |>
      et(seq(0.1, 24, 0.25))
    s <- rxSolve(m, e, params = .p)
    expect_true(all(is.na(s$dcp)))
    expect_false(any(is.na(s$cp)))   # the model itself still solves
  })

  test_that("linCmtB(-3) ignores doses into a mixed model's ODE compartments", {
    # Only doses reaching the LINEAR system can violate the shared-delay
    # assumption; an ODE compartment of a mixed model is not part of it, so
    # dosing it must not turn the answer into NA.
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag); ke0 <- 0.5
      alag(central) <- lag
      cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
      dcp <- lag * linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
      d/dt(ce) <- (cp - ce) * ke0
    })
    e <- et(amt = 100, cmt = "central") |> et(amt = 5, cmt = "ce", time = 1) |>
      et(seq(0.1, 12, 0.25))
    s <- rxSolve(m, e, params = .p)
    expect_false(any(is.na(s$dcp)))
    expect_equal(s$dcp, .fdCol(m, e, "cp"), tolerance = 1e-5)
  })

  test_that("linCmtB(-3) rejects a mismatched model shape", {
    # which1 = -3 reads the amounts cached by the which1/which2 = -1 call, so a
    # call describing a different model returns NA rather than reading them.
    m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
      alag(central) <- lag
      cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
      bad <- linCmtB(rx__PTR__, t, 1, 1, 0, -3, 7, 1, cl, v, 0, 0, 0, 0, 0)
    })
    e <- eventTable() |> add.dosing(dose = 100, cmt = "central") |>
      add.sampling(seq(0.1, 12, 0.25))
    s <- rxSolve(m, e, params = .p)
    expect_true(all(is.na(s$bad)))
  })
})
