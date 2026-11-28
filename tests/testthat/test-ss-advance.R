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

  test_that("ss=2 of a regimen already at steady state still superposes", {
    .ev1 <- et(amt = 100, ii = 24, addl = 4, ss = 1) |> et(c(48, 60))
    .ev2 <- et(amt = 100, ii = 24, addl = 4, ss = 1) |>
      et(time = 48, amt = 100, ii = 24, ss = 2) |>
      et(c(48, 60))
    .s1 <- rxSolve(.ssAucMod, .ev1)
    .s2 <- rxSolve(.ssAucMod, .ev2)
    expect_equal(.s2$central, 2 * .s1$central, tolerance = 1e-4)
    expect_equal(.s2$auc[1], .s1$auc[1], tolerance = 1e-4)
  })

  test_that("ss=2 keeps an AUC whose integrand is 0 at the dose time", {
    .mod <- function() {
      ini({
        cl <- 1
        v <- 10
      })
      model({
        d / dt(central) <- -cl / v * central
        ce <- central / v - 0.5
        if (ce < 0) ce <- 0
        d / dt(auc) <- (1 - rxSsAdvance) * ce
      })
    }
    .ev <- et(amt = 100) |>
      et(time = 48, amt = 100, ii = 24, ss = 2) |>
      et(c(47.99, 48))
    .s <- rxSolve(.mod, .ev)
    expect_gt(.s$auc[1], 0)
    expect_equal(.s$auc[2], .s$auc[1], tolerance = 1e-4)
  })

  test_that("without rxSsAdvance the AUC includes the steady-state history", {
    .mod <- .ssAucMod |>
      model(d / dt(auc) <- central / v)
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 1))
    .s <- rxSolve(.mod, .ev, minSS = 10, maxSS = 10)
    expect_gt(.s$auc[.s$time == 0], 100)
  })
})
