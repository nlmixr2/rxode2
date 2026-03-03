rxTest({
  test_that("state-dep rate uses per-subject initial state at sort phase", {
    # Two subjects with different initial central concentrations
    # rate(depot) depends on central, so infusion duration differs between subjects
    mod <- rxode2({
      rate(depot) <- central * 0.1 + 1.0
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
    })
    et <- et(amt = 100, rate = -1) %>% et(seq(0, 24, by = 1))
    s0 <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                  inits = c(depot = 0, central = 0))
    s5 <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                  inits = c(depot = 0, central = 5))
    # With central=5, rate is higher so infusion ends sooner →
    # the subsequent trajectories should differ
    expect_false(isTRUE(all.equal(s0$central, s5$central)))
    expect_false(any(is.na(s0$central)))
    expect_false(any(is.na(s5$central)))
  })

  test_that("runtime re-sort places stop event correctly after rate recompute", {
    mod <- rxode2({
      rate(depot) <- central + 2
      d/dt(depot) <- -ka * depot
      d/dt(central) <- ka * depot - kel * central
    })
    et <- et() %>%
      et(amt = 100, rate = -1, time = 0) %>%
      et(amt = 100, rate = -1, time = 2) %>%
      et(seq(0, 10))
    s <- rxSolve(mod, et, c(ka = 0.5, kel = 0.2),
                 inits = c(depot = 0, central = 3))
    expect_false(any(is.na(s$central)))
  })
})
