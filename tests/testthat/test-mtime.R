rxTest({
  test_that("state-dep mtime with state=0 fires at exactly mt1", {
    # mtime(t1) <- mt1 + state; state is constant at 0
    # => t1 fires at mt1 + 0 = mt1
    mod <- rxode2({
      mtime(t1) <- mt1 + offset
      d/dt(offset) <- 0
      d/dt(y) <- -kel * y
    })
    # Sparse sampling (by=3): 5 and 7 are NOT in the grid.
    # mtime events (evid 10-99) are observations that add their own time point.
    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1) |>
      add.sampling(seq(0, 21, by = 3))
    s <- solve(mod, et, inits = c(offset = 0, y = 0),
               params = c(mt1 = 5, kel = 0.1))
    expect_true(any(abs(s$time - 5) < 1e-6),
                label = "mtime fires at mt1 when state=0")
    expect_false(any(abs(s$time - 7) < 1e-6),
                 label = "mtime does not fire at mt1+2 when state=0")
  })

  test_that("state-dep mtime shifts by state value: mtime = mt1 + state_at_t1", {
    # mtime(t1) <- mt1 + offset; offset is constant at initial value S0
    # => t1 fires at mt1 + S0
    mod <- rxode2({
      mtime(t1) <- mt1 + offset
      d/dt(offset) <- 0
      d/dt(y) <- -kel * y
    })
    # Sparse sampling (by=3): 5, 7, and 3.5 are NOT in the grid.
    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1) |>
      add.sampling(seq(0, 21, by = 3))

    # S0 = 2: fires at 5 + 2 = 7, not at 5
    s2 <- solve(mod, et, inits = c(offset = 2, y = 0),
                params = c(mt1 = 5, kel = 0.1))
    expect_true(any(abs(s2$time - 7) < 1e-6),
                label = "mtime fires at mt1 + state (=7) when state=2")
    expect_false(any(abs(s2$time - 5) < 1e-6),
                 label = "mtime does not fire at bare mt1 when state=2")

    # S0 = -1.5: fires at 5 + (-1.5) = 3.5, not at 5
    s3 <- solve(mod, et, inits = c(offset = -1.5, y = 0),
                params = c(mt1 = 5, kel = 0.1))
    expect_true(any(abs(s3$time - 3.5) < 1e-6),
                label = "mtime fires at mt1 + state (=3.5) when state=-1.5")
    expect_false(any(abs(s3$time - 5) < 1e-6),
                 label = "mtime does not fire at bare mt1 when state=-1.5")
  })

  test_that("state-dep mtime: changing initial state shifts event time by same amount", {
    # Verify linearity: firing_time - mt1 == initial_state exactly
    mod <- rxode2({
      mtime(t1) <- mt1 + offset
      d/dt(offset) <- 0
      d/dt(y) <- -kel * y
    })
    mt1_val <- 3
    kel_val <- 0.2
    # Sampling by=0.7: none of the expected fire times (2.5, 3, 4, 5.5) fall on
    # the grid, so the mtime event adds its own time point for each S0.
    for (S0 in c(0, 1, 2.5, -0.5)) {
      et <- eventTable() |>
        add.dosing(dose = 100, nbr.doses = 1) |>
        add.sampling(seq(0, 10, by = 0.7))
      s <- solve(mod, et, inits = c(offset = S0, y = 0),
                 params = c(mt1 = mt1_val, kel = kel_val))
      expected_time <- mt1_val + S0
      expect_true(any(abs(s$time - expected_time) < 1e-6),
                  label = paste0("mtime fires at mt1+S0=", expected_time,
                                 " when S0=", S0))
    }
  })

  test_that("state-dep mtime with decaying state: fires at T0 (initial sort), not fixed-point", {
    # mtime(t1) <- mt1 + state; state decays: state(t) = S0*exp(-ka*t)
    #
    # T0 = mt1 + S0  (initial sort time, using initial state)
    # At T0, re-evaluate: T1 = mt1 + S0*exp(-ka*T0) < T0  (state has decayed)
    # Because T1 < T0, the solver cannot go back: mtime fires at T0.
    #
    # Old broken behaviour: continuous re-evaluation converges to fixed-point
    # Tfp = mt1 + S0*exp(-ka*Tfp), which is strictly less than T0.
    # Fixed: mtime fires at T0 (not at Tfp).
    mod <- rxode2({
      mtime(t1) <- mt1 + state
      d/dt(state) <- -ka * state
      d/dt(y) <- -kel * y
    })
    mt1_val <- 5
    ka_val  <- 0.3
    kel_val <- 0.1
    S0      <- 1.5

    T0  <- mt1_val + S0  # = 6.5
    Tfp <- uniroot(function(T) T - mt1_val - S0 * exp(-ka_val * T),
                   interval = c(mt1_val - 2, mt1_val + S0 + 1))$root

    # Sparse sampling (by=2): 6.5 and Tfp≈5.3 are NOT in the grid;
    # the mtime event adds its own time point at T0.
    et <- eventTable() |>
      add.dosing(dose = 100, nbr.doses = 1) |>
      add.sampling(seq(0, 15, by = 2))
    s <- solve(mod, et, inits = c(state = S0, y = 100),
               params = c(mt1 = mt1_val, ka = ka_val, kel = kel_val))

    expect_true(any(abs(s$time - T0) < 1e-4),
                label = paste0("mtime fires at initial sort time T0=", round(T0, 4)))
    if (abs(T0 - Tfp) > 0.05) {
      expect_false(any(abs(s$time - Tfp) < 1e-4),
                   label = paste0("mtime does NOT fire at fixed-point Tfp=",
                                  round(Tfp, 4)))
    }
  })

  test_that("state-dependent mtime is parsed and solves without error", {
    mod <- rxode2({
      ka <- 0.04
      kel <- 0.6
      # mtime depends on state variable 'intestine'
      mtime(t1) <- intestine * 0.01
      d / dt(intestine) <- -ka * intestine
      d / dt(blood) <- ka * intestine - kel * blood
    })
    et <- eventTable() |>
      add.dosing(dose = 3, nbr.doses = 1) |>
      add.sampling(0:48)
    # Uses initial condition of intestine (100) to compute mtime at solve init
    expect_no_error(solve(mod, et, inits = c(intestine = 100, blood = 0)))
  })

  if (!.Call(`_rxode2_isIntel`)) {
    mod <- rxode2({
      ka <- 0.04
      kel <- 0.6
      mt1 <- 0.04
      mt2 <- 0.6
      mtime(t1) <- mt1
      mtime(t2) <- mt2
      if (t >= t1) {
        ka <- 0.4
      }
      if (t >= t2) {
        ka <- 4
      }
      d / dt(intestine) <- -ka * intestine
      d / dt(blood) <- ka * intestine - kel * blood
    })

    et <- eventTable() |>
      add.dosing(dose = 3, nbr.doses = 1) |>
      add.sampling(0:48)


    test_that("Solved model contains the model times", {
      s <- solve(mod, et)
      expect_true(any(s$time == 0.04))
      expect_true(any(s$time == 0.6))
      expect_false(any(s$time == 1.977))
      expect_false(any(s$time == 10.99))
      s <- solve(mod, et, c(mt1 = 1.977, mt2 = 10.09))
      expect_true(any(s$time == 1.977))
      expect_true(any(s$time == 10.09))
      expect_false(any(s$time == 0.04))
      expect_false(any(s$time == 0.6))
    })
  }
})

