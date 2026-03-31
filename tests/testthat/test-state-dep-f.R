rxTest({
  # ── Analytical background ──────────────────────────────────────────────────
  # f(depot) <- f0 + state scales the bioavailable fraction of each dose.
  # For a bolus dose of amount `amt` at time t_dose into a depot that drains
  # with first-order rate constant ka:
  #
  #   depot(t) = amt * (f0 + S0) * exp(-ka * (t - t_dose))   for t >= t_dose
  #
  # Multiple doses contribute additively.  Constant state (d/dt(state)=0)
  # keeps S0 fixed, making the expected trajectory fully analytical.

  # ── Test 1 ─────────────────────────────────────────────────────────────────
  # state = 0 throughout  →  f(depot) = f0 + 0 = f0 exactly.
  # Effective bioavailable amount = amt * f0; trajectory is mono-exponential.
  test_that("state-dep f with state=0 uses exactly f0 (no state shift)", {
    mod <- rxode2({
      # Bioavailability depends on a state variable.
      # With state held at 0, this reduces to the fixed-f0 case.
      f(depot) <- f0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0         # constant state; initial value is the only value
    })

    amt  <- 100
    f0   <- 0.8
    ka   <- 0.5
    times <- seq(0, 20, by = 0.5)

    et <- eventTable() |>
      add.dosing(dose = amt, nbr.doses = 1) |>
      add.sampling(times)

    s <- solve(mod, et,
               inits  = c(state = 0, depot = 0),
               params = c(f0 = f0, ka = ka))

    # Expected: depot(t) = amt * f0 * exp(-ka * t)
    expected <- amt * f0 * exp(-ka * times)

    # A mismatch would mean the bioavailable fraction was not f0.
    expect_equal(s$depot[s$time %in% times], expected,
                 tolerance = 1e-4,
                 label = "depot matches amt*f0*exp(-ka*t) when state=0")
  })

  # ── Test 2 ─────────────────────────────────────────────────────────────────
  # state = S0 (constant, various values)  →  f(depot) = f0 + S0 exactly.
  # Effective bioavailable amount = amt * (f0 + S0); different for each S0.
  test_that("state-dep f shifts bioavailable amount by state value: f = f0 + state_at_dose", {
    mod <- rxode2({
      # f(depot) is evaluated at the moment the dose fires.
      # With constant state the evaluation time doesn't matter.
      f(depot) <- f0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0
    })

    amt  <- 100
    f0   <- 0.8
    ka   <- 0.5
    times <- seq(0, 20, by = 0.5)

    for (S0 in c(0.1, 0.5, -0.2)) {
      et <- eventTable() |>
        add.dosing(dose = amt, nbr.doses = 1) |>
        add.sampling(times)

      s <- solve(mod, et,
                 inits  = c(state = S0, depot = 0),
                 params = c(f0 = f0, ka = ka))

      F_eff    <- f0 + S0                          # effective bioavailability
      expected <- amt * F_eff * exp(-ka * times)   # analytical trajectory

      # Full trajectory must match the analytical formula for f = f0 + S0.
      expect_equal(s$depot[s$time %in% times], expected,
                   tolerance = 1e-4,
                   label = paste0("depot matches amt*(f0+S0)*exp(-ka*t) for S0=", S0))

      # Also confirm that this differs from the S0=0 baseline,
      # i.e., the state contribution is not silently ignored.
      expected_s0 <- amt * f0 * exp(-ka * times)
      expect_false(isTRUE(all.equal(expected, expected_s0)),
                   label = paste0("S0=", S0, " gives a different trajectory than state=0"))
    }
  })

  # ── Test 3 ─────────────────────────────────────────────────────────────────
  # Multiple doses: each dose's bioavailable amount = dose * (f0 + state_at_dose_time).
  # With a decaying state, consecutive doses get decreasing f values and
  # therefore decreasing effective amounts.
  # depot(t) = sum over fired doses of:  amt*(f0+state(t_d)) * exp(-ka*(t-t_d))
  test_that("state-dep f: each dose evaluated at its own firing-time state", {
    mod <- rxode2({
      f(depot)    <- f0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- -kstate * state   # state decays so each dose sees a different f
    })

    f0     <- 0.8
    ka     <- 0.5
    kstate <- 0.3
    amt    <- 100

    dose_times <- c(0, 5)
    times      <- seq(0, 15, by = 1)

    et <- et() |>
      et(amt = amt, time = dose_times[1], cmt = "depot") |>
      et(amt = amt, time = dose_times[2], cmt = "depot") |>
      et(times)

    for (S0 in c(0, 0.2)) {
      s <- rxSolve(mod, et,
                   params = c(f0 = f0, ka = ka, kstate = kstate),
                   inits  = c(depot = 0, state = S0))

      # Analytical: state(t) = S0 * exp(-kstate * t)
      # f at each dose time:  f_d = f0 + S0*exp(-kstate*t_d)
      # contribution of dose d at observation time t:
      #   amt * f_d * exp(-ka*(t - t_d))  [if t >= t_d]
      expected_depot <- vapply(times, function(t) {
        contrib <- 0
        for (td in dose_times) {
          if (t >= td) {
            f_at_td  <- f0 + S0 * exp(-kstate * td)
            contrib  <- contrib + amt * f_at_td * exp(-ka * (t - td))
          }
        }
        contrib
      }, numeric(1))

      obs <- s$depot[s$time %in% times]
      expect_equal(obs, expected_depot,
                   tolerance = 1e-3,
                   label = paste0("two-dose depot matches analytic f=f0+state(t_dose), S0=", S0))
    }
  })

  # ── Test 4 ─────────────────────────────────────────────────────────────────
  # Robustness: no NAs produced when state is non-zero and f varies between
  # subjects (two-row data frame with different initial state values).
  test_that("state-dep f: no NAs across subjects with different initial state", {
    mod <- rxode2({
      f(depot) <- f0 + state
      d/dt(depot) <- -ka * depot
      d/dt(state) <- 0
    })

    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1) |>
      add.sampling(seq(0, 10, by = 1))

    s0 <- solve(mod, et, inits = c(state = 0,   depot = 0),
                params = c(f0 = 0.8, ka = 0.5))
    s1 <- solve(mod, et, inits = c(state = 0.3, depot = 0),
                params = c(f0 = 0.8, ka = 0.5))

    expect_false(any(is.na(s0$depot)), label = "no NAs when state=0")
    expect_false(any(is.na(s1$depot)), label = "no NAs when state=0.3")
    # Different initial f → different trajectories
    expect_false(isTRUE(all.equal(s0$depot, s1$depot)),
                 label = "different state gives different f and depot")
  })
})
