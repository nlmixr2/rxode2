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

  test_that("the Jacobian-augmented model is compiled once, not once per solve", {
    ## nlmixr2/rxode2#1307: rxSolve.default() cached the augmented model's TEXT
    ## and then re-ran rxode2() on it every call -- a full parse of a model with
    ## one df()/dy() line per Jacobian entry.  That, not the switching, was the
    ## whole of the reported 2-28x slowdown, and pure ros4 paid it too.
    .key <- paste0(rxModelVars(.stiff)$md5["parsed_md5"], "_jac")
    rm(list = ls(envir = rxSolveCacheEnv, all.names = TRUE), envir = rxSolveCacheEnv)
    rxSolveCacheEnv$.order <- character()
    expect_null(.rxSolveCacheGet(.key))
    invisible(rxSolve(.stiff, .ev, method = "ros4"))
    .c1 <- .rxSolveCacheGet(.key)
    expect_true(is.list(.c1))
    expect_false(is.null(.c1$obj))
    invisible(rxSolve(.stiff, .ev, method = "ros4"))
    ## the SAME compiled object, not an equal one rebuilt from the text
    expect_identical(.rxSolveCacheGet(.key)$obj, .c1$obj)
    ## and the composite shares the entry rather than making its own
    invisible(rxSolve(.stiff, .ev, method = "dop853+ros4"))
    expect_identical(.rxSolveCacheGet(.key)$obj, .c1$obj)
  })

  test_that("the composite does not switch on a non-stiff model", {
    ## The #1307 regression target: a 1-cmt oral model is not stiff, so
    ## "dop853+ros4" must be dop853 throughout.  Asserted as bit-identity with
    ## plain dop853 rather than as a wall time, which is what actually holds --
    ## a single ros4 interval anywhere would change the trajectory.
    .oral <- rxode2({
      d/dt(depot)  <- -ka*depot
      d/dt(center) <-  ka*depot - (cl/v)*center
      cp <- center/v
    })
    .p <- c(ka = 1, cl = 1, v = 20)
    .oev <- et(amt = 100, cmt = "depot", ii = 24, addl = 6) |> et(seq(0, 168, by = 0.5))
    .d <- rxSolve(.oral, .oev, params = .p, method = "dop853")
    .c <- rxSolve(.oral, .oev, params = .p, method = "dop853+ros4")
    expect_identical(.c$cp, .d$cp)
  })

  test_that("the composite switches on a stiff model without grinding dop853 to mxstep", {
    ## Full TMDD: fast binding against slow turnover.  dop853 solves it, but
    ## slowly; the composite must actually switch (so its trajectory differs
    ## from plain dop853) and still match lsoda.  Before the detector's state was
    ## carried across intervals this switched zero times -- its output was
    ## bit-identical to dop853 -- because a switch needed ~64 accepted steps
    ## inside one observation interval.
    .tmdd <- rxode2({
      d/dt(depot) <- -ka*depot
      d/dt(L)     <-  ka*depot - kel*L - kon*L*R + koff*RL
      d/dt(R)     <-  ksyn - kdeg*R - kon*L*R + koff*RL
      d/dt(RL)    <-  kon*L*R - koff*RL - kint*RL
    })
    .p <- c(ka = 0.5, kel = 0.1, kon = 100, koff = 1, ksyn = 1, kdeg = 0.5, kint = 0.2)
    .tev <- et(amt = 50, cmt = "depot", ii = 24, addl = 6) |> et(seq(0, 168, by = 0.5))
    .ref <- rxSolve(.tmdd, .tev, params = .p, method = "lsoda", atol = 1e-12, rtol = 1e-12)
    .d <- rxSolve(.tmdd, .tev, params = .p, method = "dop853")
    .c <- rxSolve(.tmdd, .tev, params = .p, method = "dop853+ros4")
    expect_false(identical(.c$L, .d$L))
    expect_true(max(abs(.c$L - .ref$L)) < 1e-5)
  })

  test_that("a composite whose primary is not dop853 switches too", {
    ## dop5 and bs cannot solve stiff Robertson on their own; paired with a
    ## stiff secondary they must.  Before, their drivers ignored op->stiff2 and
    ## the composite silently ran as the plain primary on the main timeline.
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
    for (.p in c("dop5", "bs")) {
      expect_error(suppressWarnings(rxSolve(.rob, .evr, method = .p, atol = 1e-8, rtol = 1e-8)),
                   info = paste("plain", .p, "was expected to fail on Robertson"))
      .x <- suppressWarnings(rxSolve(.rob, .evr, method = paste0(.p, "+ros4"),
                                     atol = 1e-8, rtol = 1e-8))
      expect_false(any(is.na(.x$a)), info = paste0(.p, "+ros4 produced NA"))
      expect_true(max(abs(.x$a - .refr$a)) < 1e-4,
                  info = paste0(.p, "+ros4 did not match the reference solution"))
    }
  })

  test_that("the autoSwitch controls reach the composite", {
    ## They were documented, parsed, stored on op, and read by nothing.
    .ref2 <- rxSolve(.stiff, .ev, method = "lsoda", atol = 1e-10, rtol = 1e-10)
    .base <- rxSolve(.stiff, .ev, method = "dop853+ros4", atol = 1e-8, rtol = 1e-8)
    .first <- rxSolve(.stiff, .ev, method = "dop853+ros4", atol = 1e-8, rtol = 1e-8,
                      autoSwitchStiffFirst = TRUE)
    .eager <- rxSolve(.stiff, .ev, method = "dop853+ros4", atol = 1e-8, rtol = 1e-8,
                      autoSwitchNonstifftol = 0.05)
    ## each control changes which method runs where ...
    expect_false(identical(.first$a, .base$a))
    expect_false(identical(.eager$a, .base$a))
    ## ... and none of them changes the answer
    for (.x in list(.base, .first, .eager)) {
      expect_true(max(abs(.x$a - .ref2$a)) < 1e-5)
    }
    ## autoSwitchDtfac is accepted and inert
    expect_identical(rxSolve(.stiff, .ev, method = "dop853+ros4", atol = 1e-8,
                             rtol = 1e-8, autoSwitchDtfac = 4)$a, .base$a)
  })

  test_that("dop853+ros4 composite solves a STIFF FORWARD-SENSITIVITY system with the analytic Jacobian", {
    withr::local_options(rxode2.useLinCmt = FALSE)
    # Robertson with free rate constants + first-order sensitivities: the
    # augmented (states + rx__sens_*) system is stiff, so pure dop853 cannot solve
    # it, and the ros4 secondary is only viable because the FULL forward-sensitivity
    # Jacobian is emitted (calcJac=TRUE) -- without it boost's rosenbrock4 could not
    # step the wide augmented system.  This exercises the composite ACTUALLY
    # switching to the stiff secondary on a forward-sens model (not just the primal).
    .rob <- "d/dt(a) = -k1*a + k2*b*cc\nd/dt(b) = k1*a - k2*b*cc - k3*b*b\nd/dt(cc) = k3*b*b"
    .cs <- c("k1", "k2", "k3"); .p <- c(k1 = 0.04, k2 = 1e4, k3 = 3e7); .ini <- c(a = 1, b = 0, cc = 0)
    .ev <- et(c(0.1, 1, 10, 100))
    .scol <- as.vector(outer(c("a", "b", "cc"), .cs, function(s, pn) sprintf("rx__sens_%s_BY_%s__", s, pn)))
    .ref <- as.data.frame(suppressWarnings(rxSolve(rxode2(.rob, calcSens = .cs), .ev,
                                                   params = .p, inits = .ini, method = "lsoda", atol = 1e-10, rtol = 1e-10)))
    .mj <- rxode2(.rob, calcSens = .cs, calcJac = TRUE)
    ## pure dop853 fails on the stiff augmented system ...
    expect_error(suppressWarnings(rxSolve(.mj, .ev, params = .p, inits = .ini, method = "dop853", atol = 1e-8, rtol = 1e-8)))
    ## ... but the composite switches to ros4 (analytic Jacobian) and matches lsoda.
    .x <- as.data.frame(suppressWarnings(rxSolve(.mj, .ev, params = .p, inits = .ini,
                                                 method = "dop853+ros4", atol = 1e-8, rtol = 1e-8)))
    expect_false(any(is.na(.x$a)))
    .sref <- max(1, max(abs(unlist(.ref[.scol])), na.rm = TRUE)); .mx <- 0
    for (cn in .scol) .mx <- max(.mx, max(abs(.x[[cn]] - .ref[[cn]]), na.rm = TRUE))
    expect_lt(.mx / .sref, 1e-3)
  })
})
