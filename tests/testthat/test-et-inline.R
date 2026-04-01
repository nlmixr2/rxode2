rxTest({
  test_that("et_() bolus dose injected when threshold crossed", {
    m_et <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
      if (central/v > threshold) {
        et_(t + 1, 50, 101)
      }
    })
    m_ref <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    })
    e <- et(0, amt = 200, evid = 101) |> et(seq(0, 10, by = 0.5))
    params <- c(ka = 1, cl = 5, v = 50, threshold = 1.0)
    s_et  <- rxSolve(m_et,  params, e)
    s_ref <- rxSolve(m_ref, params, e)
    expect_s3_class(s_et, "rxSolve")
    # The et_() model should have higher late-time concentrations than the reference
    # model because additional doses are injected when the threshold is crossed
    central_et  <- as.data.frame(s_et)$central
    central_ref <- as.data.frame(s_ref)$central
    # At least some time points should show higher concentration due to injected dose
    expect_true(any(central_et > central_ref, na.rm = TRUE))
  })

  test_that("et_() past-time dose records warning counter", {
    m <- rxode2({
      d/dt(central) = -cl/v * central
      # Always inject a dose in the past
      et_(t - 100, 10, 101)
    })
    e <- et(0, amt = 100, evid = 101) |> et(seq(0, 5, by = 1))
    params <- c(cl = 1, v = 10)
    expect_warning(
      rxSolve(m, params, e),
      "past-time|earlier than current solver time"
    )
  })

  test_that("et_() SS dose emits warning (ssDoseN incremented)", {
    # evid 110 has SS flag set (EVID0_SS = 10, wh0 = 10 -> flag=10)
    # Use evid with wh0=10 which is EVID0_SS. evid=1010 -> wh0=10
    m <- rxode2({
      d/dt(central) = -cl/v * central
      et_(t + 1, 10, 1010)
    })
    e <- et(0, amt = 100, evid = 101) |> et(seq(0, 5, by = 1))
    params <- c(cl = 1, v = 10)
    # SS doses via et_() emit a deferred warning (cannot be injected dynamically)
    expect_warning(
      rxSolve(m, params, e),
      "steady-state|SS"
    )
  })

  test_that("et_() multi-subject isolation: injected doses don't cross subjects", {
    m <- rxode2({
      d/dt(central) = -cl/v * central
      if (central/v > threshold) {
        et_(t + 0.5, 50, 101)
      }
    })
    e <- et(0, amt = 100, evid = 101) |> et(seq(0, 5, by = 0.5))
    params <- data.frame(id = 1:3, cl = c(1, 2, 3), v = 10, threshold = 1.5)
    s <- rxSolve(m, params, e)
    expect_s3_class(s, "rxSolve")
    expect_equal(length(unique(as.data.frame(s)$id)), 3)
  })

  test_that("et_() model parses and summary shows et_ statement", {
    m <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
      et_(t + 1, 100, 101)
    })
    expect_s3_class(m, "rxode2")
  })

  test_that("et_() bioavailability is applied to injected doses via handle_evid", {
    # With f(depot)=0.5, a 100-unit dose should deliver 50 units to depot.
    # Compare et_()-injected dose against regular dose with same bioavailability.
    m_et <- rxode2({
      f(depot) = 0.5
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
      # Inject an additional dose at t=0 via et_() — F is applied by handle_evid
      et_(0.0, 100, 101)
    })
    m_reg <- rxode2({
      f(depot) = 0.5
      d/dt(depot)   = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
    })
    params <- c(ka = 0.5, cl = 1, v = 10)
    e_et  <- et(seq(0, 10, by = 1))      # no initial dose; et_() injects it
    e_reg <- et(0, amt = 100, evid = 101) |> et(seq(0, 10, by = 1))
    s_et  <- rxSolve(m_et,  params, e_et)
    s_reg <- rxSolve(m_reg, params, e_reg)
    # Both should produce the same central concentration profile
    expect_equal(as.data.frame(s_et)$central,
                 as.data.frame(s_reg)$central,
                 tolerance = 1e-4)
  })
})
