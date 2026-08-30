rxTest({
  # When the AutoSwitch composite "primary+stiff" switches, and when it must
  # not.  The Jacobian plumbing the stiff secondary needs is
  # test-autoswitch-jacobian.R.

  ## Robertson: stiff enough that the non-stiff primaries below cannot solve it
  ## on their own, so solving it at all is proof of a switch.
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

  ## Full TMDD: fast binding against slow turnover.  dop853 solves it unaided,
  ## just slowly, so here a switch shows up as a difference from dop853 rather
  ## than as the difference between solving and not.
  .tmdd <- rxode2({
    d/dt(depot) <- -ka*depot
    d/dt(L)     <-  ka*depot - kel*L - kon*L*R + koff*RL
    d/dt(R)     <-  ksyn - kdeg*R - kon*L*R + koff*RL
    d/dt(RL)    <-  kon*L*R - koff*RL - kint*RL
  })
  .tmddP <- c(ka = 0.5, kel = 0.1, kon = 100, koff = 1,
              ksyn = 1, kdeg = 0.5, kint = 0.2)
  .tmddEv <- et(amt = 50, cmt = "depot", ii = 24, addl = 6) |> et(seq(0, 168, by = 0.5))

  test_that("the non-dense dop853+ros4 composite switches to ros4 mid-solve", {
    ## Widely spaced output times overwhelm the non-stiff dop853 primary, so it
    ## must switch to ros4 per interval to solve this at all.
    ## pure dop853 cannot solve it ...
    expect_error(rxSolve(.rob, .evr, method = "dop853", atol = 1e-8, rtol = 1e-8))

    ## ... but the non-dense composite (no dense=TRUE) does, matching lsoda.
    .xr <- rxSolve(.rob, .evr, method = "dop853+ros4", atol = 1e-8, rtol = 1e-8)
    expect_false(any(is.na(.xr$a)))
    expect_true(max(abs(.xr$a - .refr$a)) < 1e-5)
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
    .ref <- rxSolve(.tmdd, .tmddEv, params = .tmddP, method = "lsoda",
                    atol = 1e-12, rtol = 1e-12)
    .d <- rxSolve(.tmdd, .tmddEv, params = .tmddP, method = "dop853")
    .c <- rxSolve(.tmdd, .tmddEv, params = .tmddP, method = "dop853+ros4")
    expect_false(identical(.c$L, .d$L))
    expect_true(max(abs(.c$L - .ref$L)) < 1e-5)
  })

  test_that("a composite whose primary is not dop853 switches too", {
    ## dop5 and bs cannot solve stiff Robertson on their own; paired with a
    ## stiff secondary they must.  Before, their drivers ignored op->stiff2 and
    ## the composite silently ran as the plain primary on the main timeline.
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

  test_that("a dense composite hands the segment over without losing observations", {
    ## The dense path is the subtle one: dopDenseSolout fills observations from
    ## the interpolant and advances a cursor as it goes, so when ros4 takes over
    ## mid-segment the state, the delay history and that cursor all have to
    ## agree.  The fallback used to rewind the first two and not the third, so a
    ## segment it believed it had re-solved kept the abandoned attempt's
    ## observation values -- and nothing asserted on them, because the dense
    ## composite was only ever checked on its final state.
    ##
    ## Observations are dense inside each segment (the segment runs dose to
    ## dose, so every interior point is filled by interpolation, not by
    ## stepping to it) and the model is stiff enough to force the hand-over.
    .dev <- et(amt = 50, cmt = "depot", ii = 24, addl = 2) |> et(seq(0, 72, by = 0.1))
    .ref <- rxSolve(.tmdd, .dev, params = .tmddP, method = "lsoda",
                    atol = 1e-12, rtol = 1e-12)
    .dd <- rxSolve(.tmdd, .dev, params = .tmddP, method = "dop853",
                   dense = TRUE, atol = 1e-10, rtol = 1e-10)
    .dn <- rxSolve(.tmdd, .dev, params = .tmddP, method = "dop853+ros4",
                   dense = TRUE, atol = 1e-10, rtol = 1e-10)
    expect_false(any(is.na(.dn$L)))
    ## a hand-over actually happened -- otherwise this asserts nothing
    expect_false(identical(.dn$L, .dd$L))
    ## every observation, not just the segment ends
    expect_true(max(abs(.dn$L - .ref$L)) < 1e-6)
    ## and the interior points are genuinely filled, not held at a segment value
    expect_true(all(diff(.dn$L[.dn$time > 0 & .dn$time < 24]) != 0))
  })

  test_that("the autoSwitch controls reach the composite", {
    ## Every one of these was documented, parsed, stored on op, and read by
    ## nothing.  Each is checked the same way: it has to change which method
    ## runs where -- otherwise it is still dead -- without changing the answer.
    .go <- function(...) {
      rxSolve(.tmdd, .tmddEv, params = .tmddP, method = "dop853+ros4",
              atol = 1e-8, rtol = 1e-8, ...)
    }
    .ref2 <- rxSolve(.tmdd, .tmddEv, params = .tmddP, method = "lsoda",
                     atol = 1e-12, rtol = 1e-12)
    .base <- .go()
    .live <- list(autoSwitchNonstifftol = 0.05,   # trip the detector sooner
                  autoSwitchStifftol = 0.05,      # ... on the re-probe after a switch
                  autoSwitchStiffFirst = TRUE,    # start on the secondary
                  autoSwitchMaxStiff = 1L,        # stick to it after one stiff interval
                  autoSwitchMaxNonstiff = 50L,    # stay on it far longer
                  autoSwitchSwitchMax = 200L)     # ... and refuse to come back sooner
    for (.nm in names(.live)) {
      .x <- do.call(.go, stats::setNames(list(.live[[.nm]]), .nm))
      expect_false(identical(.x$L, .base$L),
                   info = paste(.nm, "had no effect on the solve"))
      expect_true(max(abs(.x$L - .ref2$L)) < 1e-5,
                  info = paste(.nm, "changed the answer, not just the method mix"))
    }
    ## autoSwitchDtfac is kept for compatibility and documented as inert
    expect_identical(.go(autoSwitchDtfac = 4)$L, .base$L)
  })
})
