rxTest({
  # Regression test for a heap-buffer-overflow in syncIdx()
  # (inst/include/rxode2parseHandleEvid.h).  handle_evid() advances ind->ixds
  # past the last dose and syncIdx() dereferenced ind->idose[ind->ixds] before
  # bounds-checking ixds against ind->ndoses, reading one past the idose array.
  #
  # The corruption is silent under a normal build (the solve still returns the
  # right values -- the out-of-bounds value is discarded by the re-sync), so
  # this test cannot fail on the value alone; it exists to exercise the exact
  # solve path under the package's ASAN / valgrind CI, where the overflow is
  # caught.  An oral two-compartment model with a modeled-duration infusion
  # (rate = -2 -> dur()), a lag time (alag()) and a bioavailability split
  # (f(A1)/f(A2)) dosed into two compartments drives ixds past ndoses.  Model
  # taken from the RxODE#435 regression.
  test_that("syncIdx does not read idose out of bounds (modeled dur + alag, multi-cmt dosing)", {
    rx <- rxode2({
      K20 <- CL / VC
      S2 <- VC
      d / dt(A1) <- -KA * A1
      d / dt(A2) <- KA * A1 - K20 * A2
      dur(A2) <- D2
      alag(A2) <- ALAG2
      f(A1) <- F1
      f(A2) <- 1 - F1
      CP <- A2 / S2
    })

    theta <- c(KA = 3, CL = 5, VC = 100, D2 = 4, F1 = 0, ALAG2 = 4)

    ev <- et(amt = 100000, rate = -2, cmt = 2) |>
      et(seq(0, 24, length.out = 100)) |>
      et(amt = 100000, cmt = 1)

    d <- suppressWarnings(rxSolve(rx, ev, theta, returnType = "data.frame"))

    expect_equal(nrow(d), 100L)
    expect_false(anyNA(d$CP))
    expect_true(all(is.finite(d$CP)))
    # lock in the known-good steady value so a regression that corrupts the
    # dose index (and therefore the solve) is also caught
    expect_equal(max(d$CP), 906.346235, tolerance = 1e-4)
  })
})
