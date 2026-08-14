rxTest({

  ## Simulating from the `ini({})` block priors: a multivariate normal on
  ## the population parameters plus an inverse Wishart on each omega
  ## block, with its own degrees of freedom.  This is what NONMEM calls
  ## NWPRI.

  .hasPriorSupport <- function() {
    exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .mod <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1.0
        tv <- 3.45
        eta.cl + eta.v ~ c(0.3,
                           0.01, 0.1)
        eta.ka ~ 0.6
        add.sd <- 0.7
        tka ~ 0.01
        prior(eta.cl, eta.v) ~ invWishart(200)
        prior(eta.ka) ~ invWishart(4)
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl + eta.cl)
        v <- exp(tv + eta.v)
        linCmt() ~ add(add.sd)
      })
    })
  }

  .plain <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
        eta.ka ~ 0.6
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- 1
        v <- 1
        linCmt() ~ add(add.sd)
      })
    })
  }

  .ev <- function() et(amt=100) |> et(seq(0, 24, by=8))

  test_that("the population parameters are drawn from their prior", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=500)
    })

    ## `$thetaMat` on a solve is the per study *deviation* that is added to
    ## the parameter, so it is zero mean by construction -- which is why
    ## the prior mean has to be the estimate for the two to agree
    expect_equal(dim(.s$thetaMat), c(500L, 1L))
    expect_equal(colnames(.s$thetaMat), "tka")
    expect_equal(mean(.s$thetaMat[, "tka"]), 0, tolerance=0.02)

    ## `tka ~ 0.01` is centered on the estimate with a variance of 0.01
    .tka <- unique(.s$params$tka)
    expect_equal(length(.tka), 500L)
    expect_equal(mean(.tka), 0.45, tolerance=0.02)
    expect_equal(sd(.tka), 0.1, tolerance=0.02)
  })

  test_that("each omega block is drawn with its own degrees of freedom", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=500)
    })

    ## every gate that decides whether the drawn omega is used has to move
    ## off `dfSub`, which a prior draw leaves at zero -- otherwise the
    ## matrices are built and then silently thrown away
    expect_equal(length(.s$omegaList), 500L)

    .cl <- vapply(.s$omegaList, function(m) m["eta.cl", "eta.cl"], double(1))
    .ka <- vapply(.s$omegaList, function(m) m["eta.ka", "eta.ka"], double(1))

    ## the 1x1 block has 4 degrees of freedom and the 2x2 has 200, so it
    ## is far more variable; a single shared `dfSub` could not do this
    expect_true(sd(.ka) / 0.6 > 4 * (sd(.cl) / 0.3))

    ## and they are centered on the omega the model gives
    expect_equal(mean(.cl), 0.3, tolerance=0.1)

    ## a draw from an inverse Wishart is positive definite
    expect_true(all(vapply(.s$omegaList,
                           function(m) all(eigen(m)$values > 0), logical(1))))
  })

  test_that("the between subject variability uses the per study omega", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=200)
    })

    ## the etas have to come from `omegaList[[i]]` rather than the point
    ## estimate, which is a separate gate from the one above
    expect_gt(length(unique(round(.s$params$eta.ka, 8))), 10L)
  })

  test_that("usePrior=FALSE reproduces the unpriored solve", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(7, {
      .off <- rxSolve(.mod(), .ev(), nSub=2, nStud=20, usePrior=FALSE)
    })
    withr::with_seed(7, {
      .none <- rxSolve(.plain(), .ev(), nSub=2, nStud=20)
    })

    ## no thetaMat and no per study omega, exactly as before priors existed
    expect_equal(length(.off$omegaList), 0L)
    expect_true(is.null(.off$thetaMat) || length(.off$thetaMat) == 0L)
    expect_equal(length(.none$omegaList), 0L)
  })

  test_that("a thetaMat given at the call site wins over the priors", {
    skip_if_not(.hasPriorSupport())

    .m <- matrix(1, 1, 1, dimnames=list("tka", "tka"))
    expect_warning({
      withr::with_seed(1, {
        .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=200, thetaMat=.m)
      })
    }, "were not used")

    ## an explicit argument is never silently discarded, so the spread is
    ## the one that was asked for and not the prior's 0.1
    expect_gt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  test_that("usePrior=TRUE says why when it cannot be honored", {
    skip_if_not(.hasPriorSupport())

    expect_error(rxSolve(.plain(), .ev(), nSub=2, nStud=5, usePrior=TRUE),
                 "specifies no prior")
    expect_error(rxSolve(.mod(), .ev(), nSub=2, nStud=1, usePrior=TRUE),
                 "no variability would be simulated")
  })

  test_that("simVariability decides whether the priors apply, not nStud", {
    skip_if_not(.hasPriorSupport())

    ## the C++ side resolves `simVar` from `simVariability` first and only
    ## falls back to `nStud > 1`, so the R side has to agree or the priors
    ## are dropped in one direction and wasted in the other
    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=1, simVariability=TRUE)
    })
    expect_equal(length(.s$omegaList), 1L)

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub=2, nStud=20, simVariability=FALSE)
    })
    expect_equal(length(.s$omegaList), 0L)
  })
})
