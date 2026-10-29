rxTest({
  if (!.Call(`_rxode2_isIntel`)) {
    # test newind flag
    ode.1c <- rxode2({
      V <- 20
      Cl <- 1
      fc <- 1
      C2 <- center / V
      ni <- newind
      ni2 <- NEWIND
      d / dt(center) ~ -Cl * C2
      f(center) <- fc
    })

    et <- eventTable() |>
      add.dosing(dose = 3, nbr.doses = 6, dosing.interval = 8) |>
      add.sampling(0:4)

    et2 <- eventTable() |>
      add.dosing(start.time = 0.5, dose = 3, nbr.doses = 6, dosing.interval = 8) |>
      add.sampling(0:4)

    ms <- .methods0

    for (m in ms) {
      s <- solve(ode.1c, et, addDosing = TRUE, method = m)
      test_that("ode newind works with dose first", {
        expect_equal(s$ni[1], 1L)
        expect_equal(s$ni2[1], 1L)
        expect_true(all(s$ni[-1] == 2))
        expect_true(all(s$ni2[-1] == 2))
      })

      s2 <- solve(ode.1c, et2, addDosing = TRUE, method = m)

      test_that("ode newind works with dose first", {
        expect_equal(s2$ni[1], 1L)
        expect_equal(s2$ni2[1], 1L)
        expect_true(all(s2$ni[-1] == 2))
        expect_true(all(s2$ni2[-1] == 2))
      })
    }
  }
})

rxTest({
  # newind during the solve (#1526): 1 for the subject's first record's own dose
  # functions and evid_() pushes, 2 for the integration after it
  .newindMethods <- c("dop853", "dop853+ros4", "t54+sdirk43")
  .newindDense <- c("dop853", "dop853+ros4")

  .newindRhs <- rxode2({
    d / dt(central) <- -0.2 * central * ifelse(newind <= 1, 2, 1)
  })
  .newindF <- rxode2({
    d / dt(central) <- -0.2 * central
    f(central) <- ifelse(newind <= 1, 0.5, 1)
  })
  .newindGuard <- rxode2({
    d / dt(central) <- -0.2 * central
    if (newind <= 1) bolus(100, 1, 0, 0, 0)
  })
  .newindSticky <- rxode2({
    d / dt(central) <- -0.2 * central
    if (newind <= 1) {
      n <- 0
    }
    if (t >= 1 && n < 1) {
      bolus(100, 1, 0, 0, 0)
      n <- n + 1
    }
  })

  .newindCheck <- function(method, dense = FALSE) {
    .lab <- paste0(method, if (dense) " (dense)" else "")
    .solve <- function(m, ev, ...) {
      suppressMessages(rxSolve(m, ev, method = method, dense = dense, ...))
    }
    test_that(paste("newind during the solve matches the table for", .lab), {
      .s <- .solve(.newindRhs, et(amt = 100) |> et(c(1, 2, 4)))
      expect_equal(.s$central, 100 * exp(-0.2 * c(1, 2, 4)), tolerance = 1e-4)

      .s <- .solve(
        .newindF,
        et(amt = 100, time = 0) |> et(amt = 100, time = 2) |> et(c(1, 1.999, 2.001, 4))
      )
      expect_equal(.s$central, c(40.9365, 33.5227, 133.489, 89.4985), tolerance = 1e-4)

      # dense output falls back to standard output for evid_() models
      if (dense) {
        return(invisible())
      }
      .s <- .solve(.newindGuard, et(c(0, 1, 2, 4)), addDosing = TRUE)
      expect_equal(.s$central[.s$time > 0], 100 * exp(-0.2 * c(1, 2, 4)), tolerance = 1e-4)
      expect_equal(sum(.s$evid != 0), 1L)

      .s <- .solve(.newindSticky, et(c(0, 0.5, 1, 2, 4)))
      expect_equal(.s$central[.s$time > 1], 100 * exp(-0.2 * c(1, 3)), tolerance = 1e-4)
    })
  }

  for (.m in .newindMethods) .newindCheck(.m)
  for (.m in .newindDense) .newindCheck(.m, dense = TRUE)
})
