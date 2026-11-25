rxTest({
  # rxSsAdvance is 1 while the solver advances to steady state (#1797)
  .ssAucMod <- function() {
    ini({
      ka <- 1
      cl <- 1
      v <- 10
    })
    model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      d / dt(auc) <- (1 - rxSsAdvance) * central / v
      flag <- rxSsAdvance
    })
  }

  test_that("rxSsAdvance holds an AUC compartment during an ss=1 bolus advance", {
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 1))
    .s <- rxSolve(.ssAucMod, .ev)
    # the AUC over one steady-state interval is dose/cl
    expect_equal(.s$auc[.s$time == 0], 0)
    expect_equal(.s$auc[.s$time == 24], 100, tolerance = 1e-4)
    expect_true(all(.s$flag == 0))
  })

  test_that("rxSsAdvance holds an AUC compartment during an ss=1 infusion advance", {
    .ev <- et(amt = 100, ii = 24, ss = 1, rate = 50) |> et(seq(0, 24, by = 1))
    .s <- rxSolve(.ssAucMod, .ev)
    expect_equal(.s$auc[.s$time == 0], 0)
    expect_equal(.s$auc[.s$time == 24], 100, tolerance = 1e-4)
  })

  test_that("rxSsAdvance keeps the accumulated AUC through an ss=2 advance", {
    .ev <- et(amt = 100, ii = 24, ss = 1) |>
      et(time = 24, amt = 50, ii = 24, ss = 2) |>
      et(seq(0, 48, by = 1))
    .s <- rxSolve(.ssAucMod, .ev)
    .auc24 <- .s$auc[.s$time == 24]
    expect_equal(.auc24, 100, tolerance = 1e-4)
    # the next interval adds the 50 regimen's dose/cl and the decaying trough
    # of the first dose
    .c0 <- .s$central[.s$time == 0]
    expect_equal(.s$auc[.s$time == 48] - .auc24, 50 + .c0 * (1 - exp(-2.4)), tolerance = 1e-4)
  })

  test_that("ss=2 adds nothing to a state its advance leaves unchanged", {
    .mod <- function() {
      ini({
        ka <- 1
        cl <- 1
        v <- 10
      })
      model({
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - cl / v * central
        d / dt(z) <- 0
        z(0) <- 5
      })
    }
    .ev <- et(amt = 100, ii = 24, ss = 1) |>
      et(time = 24, amt = 50, ii = 24, ss = 2) |>
      et(c(0, 24, 48))
    .s <- rxSolve(.mod, .ev)
    expect_equal(.s$z, rep(5, 3))
  })

  test_that("without rxSsAdvance the AUC includes the steady-state history", {
    .mod <- .ssAucMod |>
      model(d / dt(auc) <- central / v)
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 1))
    .s <- rxSolve(.mod, .ev, minSS = 10, maxSS = 10)
    expect_gt(.s$auc[.s$time == 0], 100)
  })
})
