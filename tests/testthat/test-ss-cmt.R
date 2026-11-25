rxTest({
  # rxSolve(ssCmt=) leaves compartments out of the steady-state advance (#1797)
  .ssCmtMod <- function() {
    ini({
      ka <- 1
      cl <- 1
      v <- 10
    })
    model({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      d / dt(auc) <- central / v
    })
  }

  test_that("ssCmt = '-auc' gives the AUC from an ss=1 bolus onwards", {
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 1))
    expect_no_warning(.s <- rxSolve(.ssCmtMod, .ev, ssCmt = "-auc"))
    # the AUC over one steady-state interval is dose/cl
    expect_equal(.s$auc[.s$time == 0], 0)
    expect_equal(.s$auc[.s$time == 24], 100, tolerance = 1e-4)
    # naming the advanced compartments is the same
    .s2 <- rxSolve(.ssCmtMod, .ev, ssCmt = c("depot", "central"))
    expect_equal(.s2$auc, .s$auc)
    expect_equal(.s2$central, .s$central)
    # the other compartments match the default steady state
    .s0 <- rxSolve(.ssCmtMod, .ev, minSS = 10, maxSS = 10)
    expect_equal(.s$central, .s0$central, tolerance = 1e-4)
  })

  test_that("ssCmt = '-auc' gives the AUC from an ss=1 infusion onwards", {
    .ev <- et(amt = 100, ii = 24, ss = 1, rate = 50) |> et(seq(0, 24, by = 1))
    expect_no_warning(.s <- rxSolve(.ssCmtMod, .ev, ssCmt = "-auc"))
    expect_equal(.s$auc[.s$time == 0], 0)
    expect_equal(.s$auc[.s$time == 24], 100, tolerance = 1e-4)
  })

  test_that("ssCmt = '-auc' keeps the accumulated AUC through ss=1 and ss=2", {
    .ev <- et(amt = 100) |>
      et(time = 24, amt = 50, ii = 24, ss = 2) |>
      et(time = 48, amt = 100, ii = 24, ss = 1) |>
      et(c(0, 23.99, 24, 47.99, 48, 72))
    expect_no_warning(.s <- rxSolve(.ssCmtMod, .ev, ssCmt = "-auc"))
    expect_equal(.s$auc[.s$time == 24], .s$auc[.s$time == 23.99], tolerance = 1e-3)
    expect_equal(.s$auc[.s$time == 48], .s$auc[.s$time == 47.99], tolerance = 1e-3)
    expect_equal(.s$auc[.s$time == 72] - .s$auc[.s$time == 48], 100, tolerance = 1e-4)
  })

  test_that("ssCmt leaves out a compartment's sensitivities too", {
    .mod <- rxode2({
      d / dt(depot) <- -ka * depot
      d / dt(central) <- ka * depot - cl / v * central
      d / dt(auc) <- central / v
    }, calcSens = "ka")
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(c(0, 24))
    expect_no_warning(
      .s <- rxSolve(.mod, .ev, c(ka = 1, cl = 1, v = 10), ssCmt = "-auc")
    )
    expect_equal(.s$auc[1], 0)
    expect_equal(.s$rx__sens_auc_BY_ka__[1], 0)
    expect_error(
      rxSolve(.mod, .ev, c(ka = 1, cl = 1, v = 10), ssCmt = "rx__sens_auc_BY_ka__"),
      "sensitivities"
    )
  })

  test_that("ssCmt works with a linCmt() model and refuses its compartments", {
    .mod <- rxode2({
      ka <- 1
      cl <- 1
      v <- 10
      cp <- linCmt()
      d / dt(auc) <- cp
    })
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(seq(0, 24, by = 1))
    expect_no_warning(.s <- rxSolve(.mod, .ev, ssCmt = "-auc"))
    expect_equal(.s$auc[.s$time == 0], 0)
    expect_equal(.s$auc[.s$time == 24], 100, tolerance = 1e-4)
    expect_error(rxSolve(.mod, .ev, ssCmt = "-central"), "linCmt")
    # the positive form has to name every linCmt() compartment
    expect_error(rxSolve(.mod, .ev, ssCmt = "depot"), "linCmt")
    # naming the advanced linCmt() compartments leaves out the AUC
    .s2 <- rxSolve(.mod, .ev, ssCmt = c("depot", "central"))
    expect_equal(.s2$auc, .s$auc)
  })

  test_that("ssCmt matches a compartment whose name contains _BY_", {
    .mod <- rxode2(
      {
        d / dt(depot) <- -ka * depot
        d / dt(central) <- ka * depot - cl / v * central
        d / dt(auc) <- central / v
        d / dt(auc_BY_x) <- central / v
      },
      calcSens = "ka"
    )
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(c(0, 24))
    .p <- c(ka = 1, cl = 1, v = 10)
    .s <- rxSolve(.mod, .ev, .p, ssCmt = "-auc_BY_x", minSS = 10, maxSS = 10)
    expect_equal(.s$auc_BY_x[1], 0)
    expect_equal(.s$rx__sens_auc_BY_x_BY_ka__[1], 0)
    expect_gt(.s$auc[1], 100)
    # leaving out auc keeps auc_BY_x and its sensitivity
    .s <- rxSolve(.mod, .ev, .p, ssCmt = "-auc", minSS = 10, maxSS = 10)
    expect_equal(.s$auc[1], 0)
    expect_equal(.s$rx__sens_auc_BY_ka__[1], 0)
    expect_gt(.s$auc_BY_x[1], 100)
  })

  test_that("ss=2 of a regimen already at steady state still superposes", {
    .ev1 <- et(amt = 100, ii = 24, addl = 4, ss = 1) |> et(c(48, 60))
    .ev2 <- et(amt = 100, ii = 24, addl = 4, ss = 1) |>
      et(time = 48, amt = 100, ii = 24, ss = 2) |>
      et(c(48, 60))
    .s1 <- rxSolve(.ssCmtMod, .ev1, ssCmt = "-auc")
    .s2 <- rxSolve(.ssCmtMod, .ev2, ssCmt = "-auc")
    expect_equal(.s2$central, 2 * .s1$central, tolerance = 1e-4)
    expect_equal(.s2$depot, 2 * .s1$depot, tolerance = 1e-4)
  })

  test_that("ssCmt is checked", {
    .ev <- et(amt = 100, ii = 24, ss = 1) |> et(c(0, 24))
    expect_error(rxSolve(.ssCmtMod, .ev, ssCmt = "-foo"), "not a compartment")
    expect_error(rxSolve(.ssCmtMod, .ev, ssCmt = c("-auc", "depot")), "not both")
    expect_error(rxSolve(.ssCmtMod, .ev, ssCmt = NA_character_), "ssCmt")
  })
})
