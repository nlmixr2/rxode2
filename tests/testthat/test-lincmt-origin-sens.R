rxTest({
  # linCmtB(which1 = -9 / -10): the PER-COMPARTMENT dose-time and
  # bioavailability sensitivities of a linCmt() model (nlmixr2/rxode2#1119
  # part B).  which1 = -3 differentiates wrt ONE delay shared by every dose
  # feeding the linear system, so it has to refuse a regimen that doses a
  # lagged compartment alongside an unlagged one (#1237); these modes read the
  # per-origin decomposition of the amounts instead and answer it.
  #
  # which2 packs the origin compartment and the wanted output: q*8 + out, with
  # out = 7 meaning the reported concentration.  Every case below is checked
  # against a central finite difference of that concentration.

  .p <- c(tcl = log(4), tv = log(20), tka = log(1.1), tq = log(2),
          tv2 = log(50), tq2 = log(1), tv3 = log(80),
          eta_lag = 0.1, eta_f = 0.05)

  # Build a pure linCmt() model of the requested shape whose dosed compartment
  # (depot when oral, central otherwise) carries a modeled alag(), reporting
  # the concentration, the shared-delay sensitivity (-3) and the
  # per-compartment one (-9) for origin `q` and the next compartment up.
  .rxOriginModel <- function(ncmt, oral0, q) {
    .pars <- c("cl, v, 0, 0, 0, 0", "cl, v, qq, v2, 0, 0",
               "cl, v, qq, v2, q2, v3")[ncmt]
    .ka <- if (oral0) "ka" else "0"
    .m <- ncmt + oral0
    .cmt <- if (oral0) "depot" else "central"
    .call <- function(w1, w2) {
      sprintf("linCmtB(rx__PTR__, t, %d, %d, %d, %s, %s, 1, %s, %s)",
              .m, ncmt, oral0, w1, w2, .pars, .ka)
    }
    .b <- if (q + 1L < .m) .call("-9", (q + 1L) * 8 + 7) else "0"
    rxode2(sprintf("
      cl <- exp(tcl); v <- exp(tv); qq <- exp(tq); v2 <- exp(tv2)
      q2 <- exp(tq2); v3 <- exp(tv3); ka <- exp(tka)
      lag <- 2 * exp(eta_lag)
      alag(%s) <- lag
      cp  <- %s
      d3  <- lag * %s
      d9  <- lag * %s
      d9b <- lag * (%s)
    ", .cmt, .call("-1", "-1"), .call("-3", "-3"),
       .call("-9", q * 8 + 7), .b))
  }

  .fd <- function(m, e, par, h = 1e-6) {
    .p1 <- .p; .p1[[par]] <- .p[[par]] + h
    .p0 <- .p; .p0[[par]] <- .p[[par]] - h
    (rxSolve(m, e, params = .p1)$cp - rxSolve(m, e, params = .p0)$cp) / (2 * h)
  }
  .rel <- function(a, b) max(abs(a - b)) / (max(abs(b)) + 1e-8)

  test_that("linCmtB(-9) matches finite differences across model shapes", {
    # q = 0 is the dosed compartment in every case (depot when oral, central
    # otherwise), so -9 asks for exactly the delay the model declares.
    .cases <- list(
      list(nm = "1cmt IV bolus", ncmt = 1, oral0 = 0,
           e = et(amt = 100, cmt = "central") |> et(seq(0.1, 24, 0.5))),
      list(nm = "1cmt IV multiple infusion", ncmt = 1, oral0 = 0,
           e = et(amt = 100, rate = 50, ii = 8, addl = 3, cmt = "central") |>
             et(seq(0.1, 40, 0.5))),
      list(nm = "1cmt IV steady-state bolus", ncmt = 1, oral0 = 0,
           e = et(amt = 100, ii = 8, ss = 1, cmt = "central") |>
             et(seq(0.1, 8, 0.25))),
      list(nm = "1cmt oral multiple bolus", ncmt = 1, oral0 = 1,
           e = et(amt = 100, ii = 8, addl = 3, cmt = "depot") |>
             et(seq(0.1, 40, 0.5))),
      list(nm = "2cmt IV bolus", ncmt = 2, oral0 = 0,
           e = et(amt = 100, cmt = "central") |> et(seq(0.1, 24, 0.5))),
      list(nm = "2cmt oral infusion", ncmt = 2, oral0 = 1,
           e = et(amt = 100, rate = 50, cmt = "depot") |> et(seq(0.1, 24, 0.5))),
      list(nm = "3cmt IV multiple bolus", ncmt = 3, oral0 = 0,
           e = et(amt = 100, ii = 8, addl = 3, cmt = "central") |>
             et(seq(0.1, 40, 0.5))),
      list(nm = "3cmt oral steady-state bolus", ncmt = 3, oral0 = 1,
           e = et(amt = 100, ii = 8, ss = 1, cmt = "depot") |>
             et(seq(0.1, 8, 0.25)))
    )
    for (.case in .cases) {
      .m <- .rxOriginModel(.case$ncmt, .case$oral0, 0L)
      .s <- rxSolve(.m, .case$e, params = .p)
      .f <- .fd(.m, .case$e, "eta_lag")
      expect_true(.rel(.s$d9, .f) < 1e-6, label = .case$nm)
      # Where the shared-delay assumption does hold, -9 on the only dosed
      # compartment is the same answer -3 gives.
      expect_true(.rel(.s$d9, .s$d3) < 1e-8, label = paste(.case$nm, "vs -3"))
    }
  })

  test_that("linCmtB(-9) answers a mixed-route regimen that -3 must refuse", {
    # A lagged oral depot alongside an unlagged IV bolus into central: two
    # different delays, so -3 reports NA rather than the biased single-delay
    # answer.  -9 asks only about the depot's own delay.
    .m <- .rxOriginModel(2L, 1L, 0L)
    .e <- et(amt = 100, cmt = "depot", ii = 12, addl = 1) |>
      et(amt = 50, cmt = "central", time = 1) |>
      et(seq(0.1, 30, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    .f <- .fd(.m, .e, "eta_lag")
    expect_true(all(is.na(.s$d3)))
    expect_true(.rel(.s$d9, .f) < 1e-6)
    # The unlagged central's own origin is a real, nonzero derivative too --
    # it is what would be added if THAT dose were delayed.
    expect_true(max(abs(.s$d9b)) > 0)
  })

  test_that("linCmtB(-9) handles compartments lagged differently", {
    # Two modeled alag()s with different expressions -- the model shape
    # .rxLinCmtDoseTimeSensCheck() refuses outright for -3.
    .m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      lag1 <- 2 * exp(eta_lag)
      lag2 <- 0.75
      alag(depot) <- lag1
      alag(central) <- lag2
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      d9 <- lag1 * linCmtB(rx__PTR__, t, 2, 1, 1, -9, 7, 1, cl, v, 0, 0, 0, 0, ka)
    })
    .e <- et(amt = 100, cmt = "depot", ii = 12, addl = 1) |>
      et(amt = 40, cmt = "central", time = 1) |>
      et(seq(0.1, 30, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    .f <- .fd(.m, .e, "eta_lag")
    expect_true(.rel(.s$d9, .f) < 1e-6)
  })

  test_that("linCmtB(-10) gives the per-compartment bioavailability sensitivity", {
    # d(pred)/dF_q = A^(q)/F_q: the system is linear in the dose, but only in
    # the part of the state that arrived through q.  With F = exp(eta_f) the
    # chain rule cancels F, so the reported concentration derivative IS
    # A^(depot) scaled by the central volume.
    .m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      fdep <- exp(eta_f)
      f(depot) <- fdep
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      dF <- linCmtB(rx__PTR__, t, 2, 1, 1, -10, 7, 1, cl, v, 0, 0, 0, 0, ka)
    })
    .e <- et(amt = 100, cmt = "depot", ii = 12, addl = 1) |>
      et(amt = 40, cmt = "central", time = 1) |>
      et(seq(0.1, 30, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    .f <- .fd(.m, .e, "eta_f")
    expect_true(.rel(.s$dF, .f) < 1e-6)
    # The whole-concentration answer (what a model blind to the mixed route
    # would use) is wrong here by a wide margin -- pin that the decomposition
    # is what makes the difference.
    expect_true(.rel(.s$cp, .f) > 0.1)
  })

  test_that("linCmtB(-9) is exactly 0 when no dose reaches the origin", {
    .m <- .rxOriginModel(1L, 1L, 0L)
    .e <- et(amt = 50, cmt = "central") |> et(seq(0.1, 24, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    expect_true(all(.s$d9 == 0))
  })

  test_that("linCmtB(-9) reports NA for a steady-state infusion", {
    # The SS infusion's rate is not carried past the SS solve, so -dA/dt is
    # not recoverable for that origin (same limit as -3, see linCmtDoseScan).
    .m <- .rxOriginModel(1L, 0L, 0L)
    .e <- et(amt = 100, rate = 25, ii = 12, ss = 1, cmt = "central") |>
      et(seq(0.1, 12, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    expect_true(all(is.na(.s$d9)))
  })

  test_that("linCmtB(-9)/(-10) reject an out of range origin or output", {
    # q or out past the model's compartment count is NA, never an out of
    # bounds read.
    .m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); lag <- 2 * exp(eta_lag)
      alag(central) <- lag
      cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
      badQ <- linCmtB(rx__PTR__, t, 1, 1, 0, -9, 3 * 8 + 7, 1, cl, v, 0, 0, 0, 0, 0)
      badO <- linCmtB(rx__PTR__, t, 1, 1, 0, -10, 2, 1, cl, v, 0, 0, 0, 0, 0)
    })
    .e <- et(amt = 100, cmt = "central") |> et(seq(0.1, 12, 0.5))
    .s <- rxSolve(.m, .e, params = .p)
    expect_true(all(is.na(.s$badQ)))
    expect_true(all(is.na(.s$badO)))
  })

  test_that("linCmtB(-9) is right when linCmt() is mixed with an ODE", {
    # A model that also has d/dt() re-enters linCmtB() many times within one
    # event row (dydt fires at every internal solver step), so the advance of
    # the decomposition has to be a pure function of the row's entry state.
    # Accumulating instead read ~16% off here.
    .m <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      lag <- 2 * exp(eta_lag)
      alag(depot) <- lag
      d/dt(eff) <- -0.1 * eff
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      d3 <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
      d9 <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -9, 7, 1, cl, v, 0, 0, 0, 0, ka)
    })
    .e <- et(amt = 100, cmt = "depot") |> et(seq(0.1, 24, 0.5))
    .s <- rxSolve(.m, .e, params = .p, inits = c(eff = 5))
    .f <- .fd(.m, .e, "eta_lag")
    expect_false(anyNA(.s$d9))
    expect_true(.rel(.s$d9, .f) < 1e-6)
    # the whole regimen doses one lagged compartment, so -3 agrees
    expect_true(.rel(.s$d9, .s$d3) < 1e-8)
  })

  test_that("linCmtB(-9)/(-10) refuse a record the decomposition cannot follow", {
    # A replace or multiply rewrites a compartment that may hold mass from
    # several origins, and the amounts cannot say how the rewrite divided among
    # them -- so the decomposition stops being recoverable.  NA, not a number
    # that keeps reporting the pre-rewrite origin forever.
    .m <- .rxOriginModel(1L, 1L, 0L)
    .base <- et(amt = 100, cmt = "depot")
    # ss = 2 ADDS a steady state to whatever was already there, so the result
    # is neither a fresh SS solution to attribute to the SS compartment nor a
    # dose the amounts reveal.  Attributing it wholesale read ~8% off.
    .eSs2 <- .base |> et(amt = 50, cmt = "depot", ii = 8, ss = 2, time = 10) |>
      et(seq(0.1, 40, 0.5))
    expect_true(all(is.na(rxSolve(.m, .eSs2, params = .p)$d9)))
    # ss = 1 replaces, which the decomposition does follow
    .eSs1 <- .base |> et(amt = 50, cmt = "depot", ii = 8, ss = 1, time = 10) |>
      et(seq(0.1, 40, 0.5))
    .s1 <- rxSolve(.m, .eSs1, params = .p)
    expect_false(anyNA(.s1$d9))
    expect_true(.rel(.s1$d9, .fd(.m, .eSs1, "eta_lag")) < 1e-6)
    for (.evid in c(5, 6)) {
      .e <- .base |> et(amt = 0.5, cmt = "central", evid = .evid, time = 10) |>
        et(seq(0.1, 24, 0.5))
      .s <- rxSolve(.m, .e, params = .p)
      expect_true(all(is.na(.s$d9)), label = paste("evid", .evid))
    }
    # ... but a replace on an ODE compartment is none of its business
    .mOde <- rxode2({
      cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
      lag <- 2 * exp(eta_lag)
      alag(depot) <- lag
      d/dt(eff) <- -0.1 * eff
      cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      d9 <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -9, 7, 1, cl, v, 0, 0, 0, 0, ka)
    })
    .eOde <- et(amt = 100, cmt = "depot") |>
      et(amt = 1, cmt = "eff", evid = 5, time = 10) |> et(seq(0.1, 24, 0.5))
    .s <- rxSolve(.mOde, .eOde, params = .p, inits = c(eff = 5))
    expect_false(anyNA(.s$d9))
  })

  test_that("linCmtB(-9) is per-individual", {
    .m <- .rxOriginModel(2L, 1L, 0L)
    .e <- do.call(rbind, lapply(1:5, function(i) {
      .d <- as.data.frame(et(amt = 100, cmt = "depot") |>
                            et(amt = 50, cmt = "central", time = 0) |>
                            et(seq(0.1, 24, 1)))
      .d$id <- i
      .d
    }))
    .s <- rxSolve(.m, .e, params = .p)
    .f <- .fd(.m, .e, "eta_lag")
    expect_true(.rel(.s$d9, .f) < 1e-6)
    for (.i in 2:5) {
      expect_equal(.s$d9[.s$id == 1], .s$d9[.s$id == .i])
    }
  })
})
