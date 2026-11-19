rxTest({
  # linCmtB(which1 = -3) (the dose-time / moving-boundary sensitivity) assumes
  # every dose feeding the linear system shares one alag() -- see the header
  # comment on linCmtB in src/linCmt.cpp.  A model that lags more than one
  # linCmt() compartment differently violates that assumption and would get a
  # silently wrong answer instead of an error (nlmixr2/rxode2#1237); rxode2()
  # refuses to build such a model instead.

  test_that("linCmtB(-3) with distinct alag() on 2 linCmt compartments errors", {
    expect_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        lag1 <- 2 * exp(eta1); lag2 <- 1 * exp(eta2)
        alag(depot) <- lag1
        alag(central) <- lag2
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
        dcp <- lag1 * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
      }),
      "1237"
    )
  })

  test_that("linCmtB(-3) with the SAME alag() shared by 2 linCmt compartments is allowed", {
    expect_no_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        lag <- 2 * exp(eta1)
        alag(depot) <- lag
        alag(central) <- lag
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
        dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
      })
    )
  })

  test_that("distinct alag() on 2 linCmt compartments without which1=-3 is allowed", {
    expect_no_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        lag1 <- 2 * exp(eta1); lag2 <- 1 * exp(eta2)
        alag(depot) <- lag1
        alag(central) <- lag2
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
      })
    )
  })

  test_that("which1=-3 with a single lagged linCmt compartment is allowed", {
    expect_no_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        lag <- 2 * exp(eta1)
        alag(depot) <- lag
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
        dcp <- lag * linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
      })
    )
  })

  test_that("which1=-3 with no modeled alag() at all is allowed", {
    expect_no_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
        dcp <- linCmtB(rx__PTR__, t, 2, 1, 1, -3, -3, 1, cl, v, 0, 0, 0, 0, ka)
      })
    )
  })

  test_that("which1=-3 reached indirectly through a constant variable still errors", {
    # linCmtB()'s which1/which2 selectors are always written as literals in
    # every call this package generates or that its tests write by hand, but
    # nothing in the grammar enforces that -- confirm the guard still catches
    # `w <- -3; ... linCmtB(..., w, w, ...)`.
    expect_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv); ka <- exp(tka)
        lag1 <- 2; lag2 <- 3
        alag(depot) <- lag1
        alag(central) <- lag2
        w <- -3
        cp <- linCmtB(rx__PTR__, t, 2, 1, 1, -1, -1, 1, cl, v, 0, 0, 0, 0, ka)
        dcp <- linCmtB(rx__PTR__, t, 2, 1, 1, w, w, 1, cl, v, 0, 0, 0, 0, ka)
      }),
      "1237"
    )
  })

  test_that("a conditionally re-assigned alag() on ONE compartment is allowed", {
    # alag(central) taking a different expression per if/else branch does not
    # violate the shared-delay assumption by itself -- there is still only one
    # lagged compartment. Only a MISMATCH ACROSS compartments is the
    # violation.
    expect_no_error(
      rxode2({
        cl <- exp(tcl); v <- exp(tv)
        lag1 <- 2; lag2 <- 3
        if (wt > 70) {
          alag(central) <- lag1
        } else {
          alag(central) <- lag2
        }
        cp <- linCmtB(rx__PTR__, t, 1, 1, 0, -1, -1, 1, cl, v, 0, 0, 0, 0, 0)
        dcp <- linCmtB(rx__PTR__, t, 1, 1, 0, -3, -3, 1, cl, v, 0, 0, 0, 0, 0)
      })
    )
  })
})
