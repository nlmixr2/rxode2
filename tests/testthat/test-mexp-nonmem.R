rxTest({
  test_that("NONMEM-like matrix exponential syntax errors are caught", {
    # 1. matExp() with ODEs throws error
    expect_error(rxode2({
      matExp()
      d/dt(depot) = -k_depot_central * depot
      d/dt(central) = k_depot_central * depot - k_central_output * central
    }))

    # 2. indLin() without matExp() throws error
    expect_error(rxode2({
      cmt(depot)
      cmt(central)
      indLin(central) <- 1.5
      k_depot_central = 0.5
    }))

    # 3. self-transfer k_cmt1_cmt1 throws error
    expect_error(rxode2({
      matExp()
      cmt(depot)
      k_depot_depot = 0.1
    }))

    expect_error(rxode2({
      matExp()
      cmt(depot)
      k.depot.depot = 0.1
    }))
  })

  test_that("NONMEM-like matrix exponential compiles and solves correctly", {
    # Test a simple 2-compartment model
    mod1 <- rxode2({
      matExp()
      k_depot_central = 0.5
      k_central_output = 0.2
      # CP calculation using compartment amount
      cp = central / 10.0
    })

    # Verify that depot and central are state variables
    expect_true(any(mod1$state == "depot"))
    expect_true(any(mod1$state == "central"))

    # Solve it with some doses
    et1 <- eventTable() %>%
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) %>%
      add.sampling(seq(0, 10, by = 1))

    res1 <- rxSolve(mod1, et1, method = "indLin")

    # Solve the analytical/ode version to compare
    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central
      cp = central / 10.0
    })
    resOde <- rxSolve(modOde, et1)

    expect_equal(res1$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res1$central, resOde$central, tolerance = 1e-4)
    expect_equal(res1$cp, resOde$cp, tolerance = 1e-4)
  })

  test_that("NONMEM-like matrix exponential works with dot notation and indLin prop", {
    mod2 <- rxode2({
      matExp()
      k.depot.central = 0.5
      k.central.output = 0.2
      indLin(central) <- 1.5
    })

    et2 <- eventTable() %>%
      add.dosing(dose = 100, nbr.doses = 1, start.time = 0) %>%
      add.sampling(seq(0, 10, by = 1))

    res2 <- rxSolve(mod2, et2, method = "indLin")

    # Solve the ODE version to compare (since indLin(central) <- 1.5 adds a constant infusion/rate of 1.5 to central)
    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central + 1.5
    })
    resOde <- rxSolve(modOde, et2)

    expect_equal(res2$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res2$central, resOde$central, tolerance = 1e-4)
  })

  test_that("NONMEM-like matrix exponential works with infusions", {
    mod3 <- rxode2({
      matExp()
      k_depot_central = 0.5
      k_central_output = 0.2
    })

    et3 <- eventTable() %>%
      add.dosing(dose = 100, rate = 10, start.time = 0) %>% # 10 hour infusion
      add.sampling(seq(0, 15, by = 1))

    res3 <- rxSolve(mod3, et3, method = "indLin")

    modOde <- rxode2({
      d/dt(depot) = -0.5 * depot
      d/dt(central) = 0.5 * depot - 0.2 * central
    })
    resOde <- rxSolve(modOde, et3)

    expect_equal(res3$depot, resOde$depot, tolerance = 1e-4)
    expect_equal(res3$central, resOde$central, tolerance = 1e-4)
  })
})
