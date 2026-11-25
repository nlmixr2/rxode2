rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    # locking tests

    ode <- rxode2({
      a <- -8 / 3
      b <- -10
      c <- -1
      d / dt(X) <- a * X + Y * Z
      d / dt(Y) <- b * (Y - Z)
      d / dt(Z) <- -X * Y + c * Y - Z
    })

    test_that("Unlocked when simply loading", {
      expect_true(rxModels_()[[rxDll(ode)]] == 0L)
    })

    et <- eventTable(time.units = "hr") # default time units
    et$add.sampling(seq(from = 0, to = 100, by = 0.01))

    out <- rxSolve(ode, events = et, inits = c(X = 1, Y = 1, Z = 1), .setupOnly = TRUE)

    test_that("Locked after .setupOnly", {
      expect_true(rxModels_()[[rxDll(ode)]] == 1L)
    })

    out <- rxSolve(ode, events = et, inits = c(X = 1, Y = 1, Z = 1))

    test_that("Unlocked after other solve", {
      expect_true(rxModels_()[[rxDll(ode)]] == 0L)
    })

    test_that("a solve that fails during setup releases its lock (#1761)", {
      m <- rxode2({
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - cl / v * central
      })
      ev <- et(amt = 100) |> et(0:4)
      .lk <- function() rxModels_()[[rxDll(m)]]
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev)
      expect_equal(.lk(), 0L)
      for (i in 1:3) {
        expect_error(rxSolve(m, c(ka = 1, cl = 4), ev))
      }
      expect_equal(.lk(), 0L)
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev)
      expect_equal(.lk(), 0L)
      # an outer lock survives solves (no double unlock)
      rxLock(m)
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev)
      expect_error(rxSolve(m, c(ka = 1, cl = 4), ev))
      rxSolveFree()
      expect_equal(.lk(), 1L)
      # serializing a solve and replaying it keep the outer lock
      .f <- tempfile(fileext = ".rxbin")
      on.exit(unlink(.f), add = TRUE)
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev, serializeFile = .f)
      rxSolveFree()
      expect_equal(.lk(), 1L)
      rxSolve(m, .f)
      rxSolveFree()
      expect_equal(.lk(), 1L)
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev, serializeFile = TRUE)
      rxSolveFree()
      expect_equal(.lk(), 1L)
      rxUnlock(m)
      # repeated .setupOnly solves hold one lock
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev, .setupOnly = TRUE)
      rxSolve(m, c(ka = 1, cl = 4, v = 25), ev, .setupOnly = TRUE)
      expect_equal(.lk(), 1L)
      rxSolveFree()
      expect_equal(.lk(), 0L)
      expect_true(rxUnload(m))
    })
  }
})
