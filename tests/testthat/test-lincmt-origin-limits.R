rxTest({
  # What the linCmtB(which1 = -9 / -10) per-origin decomposition REFUSES to
  # answer, and the one case where its answer is an exact zero rather than a
  # derivative.  Every refusal here is NA on purpose: a wrong number that
  # looks like a real sensitivity is worse than no number at all.  The
  # finite-difference accuracy cases are in test-lincmt-origin-sens.R; the
  # shared helpers are in helper-lincmt-origin.R.

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
})
