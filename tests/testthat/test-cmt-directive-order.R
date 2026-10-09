rxTest({
  # An x(0) line parses its state first, so the parse order differs from the
  # compartment order; alag() and split directives must still name the
  # compartment, not the parse index (rxode2#1452).

  test_that("alag() compartment survives an x(0) statement (#1452)", {
    mIni <- rxode2({
      R(0) <- 100
      d/dt(depot) <- -1.2 * depot
      d/dt(central) <- 1.2 * depot - 0.1 * central
      d/dt(R) <- 0
      alag(depot) <- 2
    })
    mNoIni <- rxode2({
      d/dt(depot) <- -1.2 * depot
      d/dt(central) <- 1.2 * depot - 0.1 * central
      alag(depot) <- 2
    })
    expect_equal(rxModelVars(mIni)$alag, 1L)
    expect_equal(rxModelVars(mNoIni)$alag, 1L)

    e <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
      et(c(0, 1, 2, 3, 12))
    for (.m in c("liblsoda", "lsoda", "dop853")) {
      sIni <- rxSolve(mIni, e, method = .m)
      sNoIni <- rxSolve(mNoIni, e, method = .m)
      expect_equal(sIni$depot, sNoIni$depot, tolerance = 1e-5)
      expect_equal(sIni$central, sNoIni$central, tolerance = 1e-5)
      expect_true(all(sIni$central > 40))
      expect_equal(sIni$R, rep(100, 5))
    }
  })

  test_that("split directive compartments survive an x(0) statement (#1452)", {
    m <- rxode2({
      R(0) <- 0
      d/dt(depot) <- -ka * depot
      d/dt(gut2) <- -ka * gut2
      d/dt(central) <- ka * depot + ka * gut2 - 0.1 * central
      d/dt(R) <- 0
      splitBolus(depot, depot, gut2)
    })
    expect_equal(rxModelVars(m)$state, c("depot", "gut2", "central", "R"))
    expect_equal(rxModelVars(m)$splitBolus, c(1L, 1L, 2L))
  })
})
