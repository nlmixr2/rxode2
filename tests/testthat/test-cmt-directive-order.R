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

    for (.split in c("splitInfusion", "splitInfusionBolus", "splitBolusInfusion")) {
      .m <- rxode2(paste0(
        "R(0) <- 0\n",
        "d/dt(depot) <- -ka * depot\n",
        "d/dt(gut2) <- -ka * gut2\n",
        "d/dt(central) <- ka * depot + ka * gut2 - 0.1 * central\n",
        "d/dt(R) <- 0\n",
        .split, "(depot, depot, gut2)\n"
      ))
      expect_equal(rxModelVars(.m)[[.split]], c(1L, 1L, 2L))
    }

    mNoIni <- rxode2({
      d/dt(depot) <- -ka * depot
      d/dt(gut2) <- -ka * gut2
      d/dt(central) <- ka * depot + ka * gut2 - 0.1 * central
      d/dt(R) <- 0
      splitBolus(depot, depot, gut2)
    })
    e <- et(amt = 100, cmt = "depot") |> et(c(0, 1, 2, 6))
    s <- rxSolve(m, e, params = c(ka = 1.2))
    sNoIni <- rxSolve(mNoIni, e, params = c(ka = 1.2))
    expect_equal(s$depot, sNoIni$depot)
    expect_equal(s$gut2, sNoIni$gut2)
    expect_equal(s$central, sNoIni$central)
    expect_true(s$gut2[1] > 0)
  })

  test_that("linCmt() alag() compartment survives an x(0) statement (#1452)", {
    m <- rxode2({
      eff(0) <- 10
      C2 <- linCmt(cl, v, ka)
      d/dt(eff) <- -0.1 * eff + C2
      alag(depot) <- 2
    })
    m0 <- rxode2({
      C2 <- linCmt(cl, v, ka)
      d/dt(eff) <- -0.1 * eff + C2
      alag(depot) <- 2
    })
    expect_equal(rxModelVars(m)$state, c("eff", "depot", "central"))
    expect_equal(rxModelVars(m)$alag, 2L)
    e <- et(amt = 100, ii = 12, ss = 1, cmt = "depot") |>
      et(c(0, 1, 2, 3, 12))
    p <- c(cl = 1, v = 10, ka = 1.2)
    s <- rxSolve(m, e, p)
    s0 <- rxSolve(m0, e, p, inits = c(eff = 10))
    expect_equal(s$C2, s0$C2)
    expect_equal(s$eff, s0$eff)
    expect_true(s$C2[1] > 5)
  })

  test_that("parser tables grow past their first block", {
    txt <- paste0(sprintf("A%d <- 1\nstr%d <- \"a\"", 1:5200, 1:5200),
                  collapse = "\n")
    mv <- rxModelVars(txt)
    expect_length(mv$lhs, 5200)
    expect_length(mv$strAssign, 5200)
    txt <- paste0(c(sprintf("d/dt(c%d) <- -c%d", 1:5100, 1:5100),
                    "x(0) <- 1", "d/dt(x) <- 0", "alag(c3) <- 2"),
                  collapse = "\n")
    mv <- rxModelVars(txt)
    expect_length(mv$state, 5101)
    expect_length(mv$extraState, 0)
    expect_equal(mv$alag, 3L)
  })
})
