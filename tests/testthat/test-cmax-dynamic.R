rxTest({
  test_that("dynamic Cmax/Cmin accumulation works correctly for IV bolus", {
    # One-compartment PK model with IV bolus (direct central compartment dosing).
    # Using max(Cmax, CP) should track the running maximum of CP over time.
    # Using min(Cmin, CP) should track the running minimum of CP over time.
    mod <- rxode2({
      cl <- 0.5
      v  <- 10.0
      ke <- cl / v

      d/dt(center) <- -ke * center

      CP <- center / v

      Cmax <- max(Cmax, CP)
      Cmin <- min(Cmin, CP)
    })

    # Observations only at positive times; dose goes directly into center
    et <- et(amt = 100, time = 0, cmt = "center") |>
      et(seq(0.5, 24, by = 0.5))

    sol <- rxSolve(mod, et)

    # Cmax should be monotonically non-decreasing
    expect_true(all(diff(sol$Cmax) >= -1e-10),
                info = "Cmax should be non-decreasing over time")

    # Cmin should be monotonically non-increasing
    expect_true(all(diff(sol$Cmin) <= 1e-10),
                info = "Cmin should be non-increasing over time")

    # Cmax should be >= all observed CP values (it is the running maximum)
    expect_true(all(sol$Cmax >= sol$CP - 1e-10),
                info = "Cmax should be >= CP at every time point")

    # Cmin should be <= all observed CP values (it is the running minimum)
    expect_true(all(sol$Cmin <= sol$CP + 1e-10),
                info = "Cmin should be <= CP at every time point")

    # The last Cmin value should equal the minimum CP at observation times
    # (for IV bolus, CP declines monotonically, so Cmin tracks it down)
    expect_equal(tail(sol$Cmin, 1), min(sol$CP),
                 tolerance = 1e-6,
                 label = "final Cmin equals min(CP) at observation times")
  })

  test_that("Cmax tracks running maximum for absorption model", {
    # Two-compartment absorption model: CP rises then falls.
    # Cmax should track the running maximum.
    # At t=0 (dose into depot), center=0 and CP=0, so the initial Cmax
    # is 0 and will be updated as CP rises during absorption.
    mod <- rxode2({
      ka <- 1.0
      cl <- 0.5
      v  <- 10.0
      ke <- cl / v

      d/dt(depot)  <- -ka * depot
      d/dt(center) <- ka * depot - ke * center

      CP <- center / v

      Cmax <- max(Cmax, CP)
    })

    et <- et(amt = 100, time = 0) |>
      et(seq(0.5, 24, by = 0.5))

    sol <- rxSolve(mod, et)

    # Cmax should be monotonically non-decreasing
    expect_true(all(diff(sol$Cmax) >= -1e-10),
                info = "Cmax should be non-decreasing over time")

    # The last Cmax should equal the maximum CP observed
    expect_equal(tail(sol$Cmax, 1), max(sol$CP),
                 tolerance = 1e-6,
                 label = "final Cmax equals max(CP)")

    # During the rising phase, Cmax should equal CP
    # (each new observation is larger than all previous ones)
    peak_time <- sol$time[which.max(sol$CP)]
    rising <- sol$time <= peak_time
    expect_true(all(abs(sol$Cmax[rising] - sol$CP[rising]) < 1e-6),
                info = "Cmax equals CP during rising phase")

    # During the falling phase, Cmax should remain constant at the peak value
    falling <- sol$time > peak_time
    peak_cp <- max(sol$CP)
    expect_true(all(abs(sol$Cmax[falling] - peak_cp) < 1e-6),
                info = "Cmax stays at peak value during falling phase")
  })
})
