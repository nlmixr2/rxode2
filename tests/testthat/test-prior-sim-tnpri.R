rxTest({

  ## One multivariate normal over the population parameters *and* the omega
  ## values, which is what NONMEM calls TNPRI.  Drawn that way an omega is
  ## not guaranteed positive definite, so the draw retries.

  .hasPriorSupport <- function() {
    exists("lotriPriorDists", envir=asNamespace("lotri"), inherits=FALSE)
  }

  .ev <- function() et(amt=100) |> et(seq(0, 24, by=8))

  ## A joint block over `tcl` and the omega element of `eta.cl`, with a
  ## covariance between them -- the thing a block that is all one kind
  ## cannot express.
  .joint <- function() {
    rxode2(function() {
      ini({
        tka <- 0.45
        tcl <- 1.0
        add.sd <- 0.7
        eta.cl ~ 0.3
        eta.v ~ 0.1
        tcl + om.eta.cl ~ c(0.02,
                            0.004, 0.005)
      })
      model({
        ka <- exp(tka)
        cl <- exp(tcl + eta.cl)
        v <- exp(1 + eta.v)
        linCmt() ~ add(add.sd)
      })
    })
  }

  test_that("a joint prior draws the thetas and the omega values together", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(11, {
      .s <- rxSolve(.joint(), .ev(), nSub=2, nStud=400)
    })

    expect_equal(length(.s$omegaList), 400L)

    .cl <- vapply(.s$omegaList, function(m) m["eta.cl", "eta.cl"], double(1))
    .v <- vapply(.s$omegaList, function(m) m["eta.v", "eta.v"], double(1))
    .tcl <- unique(.s$params$tcl)

    ## the omega element carries the prior's spread, centered on the omega
    ## value the model gives
    expect_equal(mean(.cl), 0.3, tolerance=0.02)
    expect_equal(sd(.cl), sqrt(0.005), tolerance=0.02)

    ## the element that has no prior is left at its point estimate
    expect_equal(unique(.v), 0.1)

    ## and the theta is drawn with it, correlated as the block says:
    ## 0.004 / sqrt(0.02 * 0.005) = 0.4
    expect_equal(sd(.tcl), sqrt(0.02), tolerance=0.02)
    expect_equal(cor(.tcl, .cl), 0.4, tolerance=0.12)
  })

  test_that("every drawn omega is positive definite", {
    skip_if_not(.hasPriorSupport())

    withr::with_seed(11, {
      .s <- rxSolve(.joint(), .ev(), nSub=2, nStud=200)
    })

    expect_true(all(vapply(.s$omegaList,
                           function(m) all(eigen(m)$values > 0), logical(1))))
  })

  test_that("the retry falls back to the nearest positive definite omega", {
    skip_if_not(.hasPriorSupport())

    ## A strongly correlated block whose variance carries a wide prior:
    ## the drawn variance stays positive but the block stops being
    ## positive definite, which is exactly the case the projection can
    ## repair.  It has to say so -- those studies are not draws from the
    ## stated prior.
    .wide <- rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
        eta.cl + eta.v ~ c(0.3,
                           0.29, 0.3)
        ## sd 0.03 on a variance of 0.3: the block loses positive
        ## definiteness (it needs the variance above 0.29^2/0.3 = 0.2803)
        ## well before the variance itself could go negative, which is the
        ## regime the projection can repair
        om.eta.cl ~ 0.0009
      })
      model({
        ka <- exp(tka)
        cl <- exp(1 + eta.cl)
        v <- exp(1 + eta.v)
        linCmt() ~ add(add.sd)
      })
    })

    expect_warning({
      withr::with_seed(5, {
        .s <- rxSolve(.wide, .ev(), nSub=2, nStud=100, priorPdRetry=1)
      })
    }, "nearest positive definite")

    ## whatever route it took, what comes back is usable
    expect_true(all(vapply(.s$omegaList,
                           function(m) all(eigen(m)$values > 0), logical(1))))
  })

  test_that("a prior that can never be made positive definite is an error", {
    skip_if_not(.hasPriorSupport())

    ## a 1x1 omega drawn negative cannot be projected back onto the cone --
    ## its nearest positive definite matrix is the boundary, which is not
    ## positive definite.  That is an error rather than a silent zero.
    .neg <- rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
        eta.cl ~ 0.3
        om.eta.cl ~ 9
      })
      model({
        ka <- exp(tka)
        cl <- exp(1 + eta.cl)
        v <- 1
        linCmt() ~ add(add.sd)
      })
    })

    expect_error({
      withr::with_seed(5, {
        rxSolve(.neg, .ev(), nSub=2, nStud=50, priorPdRetry=2)
      })
    }, "positive definite")
  })

  test_that("priorPdRetry=1 does not retry", {
    skip_if_not(.hasPriorSupport())

    ## with a single try every non positive definite draw goes straight to
    ## the fallback, so the warning is the same but it fires more often
    expect_warning({
      withr::with_seed(5, {
        .s <- rxSolve(.joint(), .ev(), nSub=2, nStud=50, priorPdRetry=1)
      })
    }, NA)
  })

  test_that("a joint prior and a block degrees of freedom cannot be mixed", {
    skip_if_not(.hasPriorSupport())

    ## 'lotri' rejects the combination when the model is written, since the
    ## two are alternative ways of saying the same thing
    expect_error(rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
        eta.cl ~ 0.3
        eta.v ~ 0.1
        prior(eta.cl) ~ invWishart(4)
        om.eta.v ~ 0.01
      })
      model({
        ka <- exp(tka)
        cl <- exp(1 + eta.cl)
        v <- exp(1 + eta.v)
        linCmt() ~ add(add.sd)
      })
    }), "alternatives, not additions")

    ## and on the same omega it is a duplicate, which is rejected too
    expect_error(rxode2(function() {
      ini({
        tka <- 0.45
        add.sd <- 0.7
        eta.cl ~ 0.3
        prior(eta.cl) ~ invWishart(4)
        om.eta.cl ~ 0.01
      })
      model({
        ka <- exp(tka)
        cl <- exp(1 + eta.cl)
        v <- 1
        linCmt() ~ add(add.sd)
      })
    }), "more than one prior")
  })
})
