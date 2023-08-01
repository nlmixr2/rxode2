rxTest({
  test_that("Test ui solve", {

    # Step 1 - Create a model specification
    f <- function() {
      ini({
        KA <- .291
        CL <- 18.6
        V2 <- 40.2
        Q <- 10.5
        V3 <- 297.0
        Kin <- 1.0
        Kout <- 1.0
        EC50 <- 200.0
      })
      model({
        # A 4-compartment model, 3 PK and a PD (effect) compartment
        # (notice state variable names 'depot', 'centr', 'peri', 'eff')
        C2 <- centr/V2
        C3 <- peri/V3
        d/dt(depot) <- -KA*depot
        d/dt(centr) <- KA*depot - CL*C2 - Q*C2 + Q*C3
        d/dt(peri)  <-                    Q*C2 - Q*C3
        d/dt(eff)   <- Kin - Kout*(1-C2/(EC50+C2))*eff
        eff(0) <- 1
      })
    }

    u <- f()

    # this pre-compiles and displays the simulation model
    u$simulationModel

    # Step 2 - Create the model input as an EventTable,
    # including dosing and observation (sampling) events

    # QD (once daily) dosing for 5 days.

    qd <- eventTable(amount.units = "ug", time.units = "hours")
    qd$add.dosing(dose = 10000, nbr.doses = 5, dosing.interval = 24)

    # Sample the system hourly during the first day, every 8 hours
    # then after

    qd$add.sampling(0:24)
    qd$add.sampling(seq(from = 24 + 8, to = 5 * 24, by = 8))

    # Step 3 - set starting parameter estimates and initial
    # values of the state


    # Step 4 - Fit the model to the data
    expect_error(solve(u, qd), NA)

    u1 <- u$simulationModel

    expect_error(solve(u1, qd), NA)

    u2 <- u$simulationIniModel

    expect_error(solve(u2, qd), NA)

  })
})
