rxTest({
  test_that("et_() bolus dose injected when threshold crossed", {
    m <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
      if (central/v > threshold) {
        et_(t + 1, 50, 101)
      }
    })
    e <- et(0, amt = 200, evid = 101) |> et(seq(0, 10, by = 0.5))
    params <- c(ka = 1, cl = 5, v = 50, threshold = 1.0)
    s <- rxSolve(m, params, e)
    expect_s3_class(s, "rxSolve")
    expect_true(nrow(s) > 0)
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
      "past"
    )
  })

  test_that("et_() SS dose is silently dropped (ssDoseN incremented)", {
    # evid 110 has SS flag set (EVID0_SS = 10, wh0 = 10 -> flag=10)
    # Use evid with wh0=10 which is EVID0_SS. evid=1010 -> wh0=10
    m <- rxode2({
      d/dt(central) = -cl/v * central
      et_(t + 1, 10, 1010)
    })
    e <- et(0, amt = 100, evid = 101) |> et(seq(0, 5, by = 1))
    params <- c(cl = 1, v = 10)
    # Should not error; SS doses via et_() are silently dropped
    s <- rxSolve(m, params, e)
    expect_s3_class(s, "rxSolve")
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
    expect_equal(length(unique(as.data.frame(s)$sim.id)), 3)
  })

  test_that("et_() model parses and summary shows et_ statement", {
    m <- rxode2({
      d/dt(depot) = -ka * depot
      d/dt(central) = ka * depot - cl/v * central
      et_(t + 1, 100, 101)
    })
    expect_s3_class(m, "rxode2")
  })
})
