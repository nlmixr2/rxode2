rxode2Test(
  {
    context("Bad solve")

    model <- rxode2({
      C2 <- centr / V2
      C3 <- peri / V3
      #
      d / dt(depot) <- -KA * depot
      d / dt(centr) <- KA * depot - CL * C2 - Q * C2 + Q * C3
      d / dt(peri) <- Q * C2 - Q * C3
      d / dt(eff) <- Kin - Kout * (1 - C2 / (EC50 + C2)) * eff
      ##
      eff(0) <- 1
      ##
      ## central
      KA <- 2.94E-01
      CL <- 1.86E+01
      V2 <- 4.02E+01
      ## peripheral
      Q <- 1.05E+01
      V3 <- 2.97E+02
      ##
      ## effects
      Kin <- 1
      Kout <- 1
      EC50 <- 200
    })

    event_table <- rxode2::eventTable() %>%
      add.dosing(dose = 10000, nbr.doses = 10, dosing.interval = 12) %>%
      add.sampling(0:240)

    test_that("Bad solve raises error", {
      tf <- tempfile()
      sink(tf)
      expect_error(
        model %>% solve(c(KA = -10), event_table, maxsteps = 5000),
        rex::rex("could not solve the system")
      )
      sink()
      unlink(tf)
    })
  },
  silent = TRUE,
  test = "cran"
)
