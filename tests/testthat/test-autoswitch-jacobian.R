rxTest({
  # AutoSwitch composite "primary+stiff": the stiff secondary's analytical
  # Jacobian must be generated and used whenever the secondary is an implicit
  # (Rosenbrock / implicit-RK) method.  These methods are exactly the ones for
  # which rxIsImplicit() is TRUE and which the C solver's _jacAvailable check
  # expects: ros4(13), iem(14), ros43(31), ros6(32), backwardEuler(33),
  # gauss6(34), iiic6(35), radauiia5(36), geng5(37), sdirk43(38).

  ## A well-conditioned stiff linear system (eigenvalues -100, -1).
  .stiff <- rxode2({
    d/dt(a) <- -100 * a + 99 * b
    d/dt(b) <- a - b
    a(0) <- 1
    b(0) <- 0
  })
  .ev <- et(seq(0, 3, by = 0.5))
  .ref <- rxSolve(.stiff, .ev, method = "lsoda", atol = 1e-10, rtol = 1e-10)

  .implicit <- c("ros4", "iem", "ros43", "ros6", "backwardEuler",
                 "gauss6", "iiic6", "radauiia5", "geng5", "sdirk43")

  test_that("rxIsImplicit() flags exactly the Jacobian-needing methods", {
    expect_true(all(rxIsImplicit(.implicit)))
    ## solvers that supply their own Jacobian internally are NOT flagged
    expect_false(any(rxIsImplicit(c("dop853", "lsoda", "liblsoda", "cvode", "bdf"))))
  })

  test_that("dop853 + <implicit> composites generate and use the Jacobian", {
    for (.s in .implicit) {
      .m <- paste0("dop853+", .s)
      .x <- rxSolve(.stiff, .ev, method = .m, atol = 1e-8, rtol = 1e-8)
      expect_false(any(is.na(.x$a)),
                   info = paste(.m, "produced NA (Jacobian not hooked up?)"))
      expect_true(max(abs(.x$a - .ref$a)) < 1e-5,
                  info = paste(.m, "did not match the reference solution"))
    }
  })

  test_that("the dense dop853+ros4 composite generates and uses the Jacobian", {
    .x <- rxSolve(.stiff, .ev, method = "dop853+ros4", dense = TRUE,
                  atol = 1e-8, rtol = 1e-8)
    expect_false(any(is.na(.x$a)))
    expect_true(max(abs(.x$a - .ref$a)) < 1e-5)
  })

  test_that("the dense dop853+ros4 composite solves steady state (SS-path regression)", {
    ## Regression for the SS-path AutoSwitch gotcha.  solveWith1Pt -- the
    ## single-interval solver that steady-state dosing advances repeatedly --
    ## once kept an interval-length Gershgorin stiffness pre-check after the
    ## main-solve paths had dropped it.  On the long tau-sized SS intervals
    ## that check over-estimated the spectral radius and spuriously toggled
    ## ind->autoMethod; the toggled state leaked into the dense main solve and
    ## forced ros4 segments that returned stale (unchanged) state, so every
    ## observation after the first SS dose was corrupted.  Making solveWith1Pt
    ## reactive (like the main-solve paths) fixes it.  This pins the behavior
    ## for the dense composite, which is otherwise only covered indirectly --
    ## and was mislabeled -- in the large nmtest suite.
    .ssEv <- et() |>
      et(amt = 10, ii = 1, ss = 1, cmt = "a") |>
      et(seq(0, 1, by = 0.1))
    .refss <- rxSolve(.stiff, .ssEv, method = "lsoda", atol = 1e-10, rtol = 1e-10)
    .xss <- rxSolve(.stiff, .ssEv, method = "dop853+ros4", dense = TRUE,
                    atol = 1e-8, rtol = 1e-8)
    expect_false(any(is.na(.xss$a)))
    ## not stale: the steady-state trajectory genuinely varies across the
    ## interval (the bug pinned every post-dose observation to one value).
    expect_true(stats::sd(.xss$a) > 1e-6)
    ## and it matches the reference steady-state solution.
    expect_true(max(abs(.xss$a - .refss$a)) < 1e-4)
  })

  test_that("the non-dense dop853+ros4 composite switches to ros4 mid-solve", {
    ## A stiff Robertson problem with widely spaced output times that overwhelms
    ## the non-stiff dop853 primary.  The composite must switch to ros4 per
    ## interval and solve it; pure dop853 cannot.
    .rob <- rxode2({
      d/dt(a)  <- -0.04 * a + 1e4 * b * cc
      d/dt(b)  <-  0.04 * a - 1e4 * b * cc - 3e7 * b * b
      d/dt(cc) <-  3e7 * b * b
      a(0) <- 1
      b(0) <- 0
      cc(0) <- 0
    })
    .evr <- et(c(0.1, 1, 10, 100))
    .refr <- rxSolve(.rob, .evr, method = "lsoda", atol = 1e-10, rtol = 1e-10)

    ## pure dop853 cannot solve it ...
    expect_error(rxSolve(.rob, .evr, method = "dop853", atol = 1e-8, rtol = 1e-8))

    ## ... but the non-dense composite (no dense=TRUE) does, matching lsoda.
    .xr <- rxSolve(.rob, .evr, method = "dop853+ros4", atol = 1e-8, rtol = 1e-8)
    expect_false(any(is.na(.xr$a)))
    expect_true(max(abs(.xr$a - .refr$a)) < 1e-5)
  })
})
