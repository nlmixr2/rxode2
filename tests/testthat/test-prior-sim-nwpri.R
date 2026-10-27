rxTest({
  ## Simulating from the `ini({})` block priors: a multivariate normal on
  ## the population parameters plus an inverse Wishart on each omega
  ## block, with its own degrees of freedom.  This is what NONMEM calls
  ## NWPRI.

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

  .ev <- function() et(amt = 100) |> et(seq(0, 24, by = 8))

  test_that("the population parameters are drawn from their prior", {
    skipIfOldLotri()

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 500)
    })

    ## `$thetaMat` on a solve is the per study *deviation* that is added to
    ## the parameter, so it is zero mean by construction -- which is why
    ## the prior mean has to be the estimate for the two to agree
    expect_equal(dim(.s$thetaMat), c(500L, 1L))
    expect_equal(colnames(.s$thetaMat), "tka")
    expect_equal(mean(.s$thetaMat[, "tka"]), 0, tolerance = 0.02)

    ## `tka ~ 0.01` is centered on the estimate with a variance of 0.01
    .tka <- unique(.s$params$tka)
    expect_equal(length(.tka), 500L)
    expect_equal(mean(.tka), 0.45, tolerance = 0.02)
    expect_equal(sd(.tka), 0.1, tolerance = 0.02)
  })

  test_that("each omega block is drawn with its own degrees of freedom", {
    skipIfOldLotri()

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 500)
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
    expect_equal(mean(.cl), 0.3, tolerance = 0.1)

    ## a draw from an inverse Wishart is positive definite
    expect_true(all(vapply(.s$omegaList, function(m) all(eigen(m)$values > 0), logical(1))))
  })

  test_that("the between subject variability uses the per study omega", {
    skipIfOldLotri()

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 200)
    })

    ## the etas have to come from `omegaList[[i]]` rather than the point
    ## estimate, which is a separate gate from the one above
    expect_gt(length(unique(round(.s$params$eta.ka, 8))), 10L)
  })

  test_that("usePrior=FALSE reproduces the unpriored solve", {
    skipIfOldLotri()

    withr::with_seed(7, {
      .off <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 20, usePrior = FALSE)
    })
    withr::with_seed(7, {
      .none <- rxSolve(.plain(), .ev(), nSub = 2, nStud = 20)
    })

    ## no thetaMat and no per study omega, exactly as before priors existed
    expect_equal(length(.off$omegaList), 0L)
    expect_true(is.null(.off$thetaMat) || length(.off$thetaMat) == 0L)
    expect_equal(length(.none$omegaList), 0L)
  })

  test_that("a thetaMat given at the call site wins over the priors", {
    skipIfOldLotri()

    .m <- matrix(1, 1, 1, dimnames = list("tka", "tka"))
    ## it covers the only prior parameter, so there is nothing to say
    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 200, thetaMat = .m)
      }),
      message = "prior|thetaMat"
    )

    ## an explicit argument is never silently discarded, so the spread is
    ## the one that was asked for and not the prior's 0.1
    expect_gt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  ## a MAP estimate: the prior pulls the estimate but is not centered on it
  .map <- function(meta = FALSE) {
    .f <- function() {
      ini({
        tka <- 0.43
        prior(tka) ~ dnorm(log(3), 0.25)
        tcl <- -3.2
        eta.ka ~ 0.4
        add.sd <- 0.7
      })
      model({
        ka <- exp(tka + eta.ka)
        cl <- exp(tcl)
        d / dt(depot) <- -ka * depot
        d / dt(center) <- ka * depot - cl / 30 * center
        cp <- center / 30
        cp ~ add(add.sd)
      })
    }
    .ui <- rxode2(.f)
    if (meta) {
      assign("thetaMat", .mapThetaMat(), envir = .ui$meta)
    }
    .ui
  }
  .mapThetaMat <- function() {
    lotri::lotri(tka + tcl ~ c(0.01, 0.001, 0.02))
  }
  .mapEv <- function() et(amt = 320) |> et(seq(0, 24, by = 4))

  test_that("a call site thetaMat wins over a prior not centered on the estimate (#1502)", {
    skipIfOldLotri()

    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.map(), .mapEv(), nSub = 2, nStud = 200, thetaMat = .mapThetaMat())
      }),
      message = "prior|thetaMat"
    )
    expect_equal(sort(colnames(.s$thetaMat)), c("tcl", "tka"))
    expect_equal(sd(.s$thetaMat[, "tka"]), 0.1, tolerance = 0.2)
  })

  test_that("a meta thetaMat wins over the priors without a warning (#1502)", {
    skipIfOldLotri()

    .ui <- .map(meta = TRUE)
    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.ui, .mapEv(), nSub = 2, nStud = 200)
      }),
      message = "prior"
    )
    expect_equal(sort(colnames(.s$thetaMat)), c("tcl", "tka"))
    expect_equal(sd(.s$thetaMat[, "tcl"]), sqrt(0.02), tolerance = 0.2)
  })

  test_that("usePrior=TRUE uses the priors over a given thetaMat (#1502)", {
    skipIfOldLotri()

    ## the prior is still required to be centered on the estimate
    expect_error(
      rxSolve(.map(), .mapEv(), nSub = 2, nStud = 5, thetaMat = .mapThetaMat(), usePrior = TRUE),
      "is not the initial estimate"
    )

    .m <- matrix(1, 1, 1, dimnames = list("tka", "tka"))
    expect_warning(
      {
        withr::with_seed(1, {
          .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 200, thetaMat = .m, usePrior = TRUE)
        })
      },
      "replace the 'thetaMat'"
    )
    expect_lt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  test_that("a meta thetaMat wins over a prior centered on the estimate (#1502)", {
    skipIfOldLotri()

    .ui <- .mod()
    assign("thetaMat", lotri::lotri(tka ~ 1), envir = .ui$meta)
    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.ui, .ev(), nSub = 2, nStud = 200)
      }),
      message = "prior|thetaMat"
    )
    expect_gt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  test_that("a meta thetaMat that misses a prior parameter says so (#1502)", {
    skipIfOldLotri()

    .ui <- .map(meta = TRUE)
    assign("thetaMat", lotri::lotri(tcl ~ 0.02), envir = .ui$meta)
    expect_warning(
      rxSolve(.ui, .mapEv(), nSub = 2, nStud = 5),
      "does not draw 'tka'"
    )
  })

  test_that("a winning thetaMat says which omega element priors it leaves undrawn (#1502)", {
    skipIfOldLotri()

    .joint <- function() {
      rxode2(function() {
        ini({
          tcl <- 1.0
          add.sd <- 0.7
          eta.cl ~ 0.3
          tcl + om.eta.cl ~ c(0.02, 0.004, 0.005)
        })
        model({
          cl <- exp(tcl + eta.cl)
          v <- 1
          linCmt() ~ add(add.sd)
        })
      })
    }
    .m <- lotri::lotri(tcl + om.eta.cl ~ c(0.02, 0.004, 0.005))
    ## the om. column is pruned unless the separation draws it
    expect_warning(
      rxSolve(.joint(), .ev(), nSub = 2, nStud = 5, thetaMat = .m),
      "does not draw 'om.eta.cl'"
    )
    expect_warning(
      rxSolve(.joint(), .ev(), nSub = 2, nStud = 5, thetaMat = lotri::lotri(tcl ~ 0.02)),
      "does not draw 'om.eta.cl'"
    )
    ## under tnpri any spelling of the element is drawn
    for (.nm in c("om.eta.cl", "eta.cl", "omega1.1")) {
      .t <- lotri::lotri(tcl ~ 0.02)
      .t <- rbind(cbind(.t, 0), c(0, 0.005))
      dimnames(.t) <- list(c("tcl", .nm), c("tcl", .nm))
      expect_no_warning(
        withr::with_seed(3, {
          .s <- rxSolve(.joint(), .ev(), nSub = 2, nStud = 50, thetaMat = .t, omegaSeparation = "tnpri")
        }),
        message = "does not draw"
      )
      ## and the element really is drawn
      .cl <- vapply(.s$omegaList, function(m) m["eta.cl", "eta.cl"], double(1))
      expect_gt(sd(.cl), 0.01)
    }
    ## the thetaMat is read by its column names
    .t <- matrix(0.02, 1, 1, dimnames = list(NULL, "tcl"))
    expect_warning(
      rxSolve(.joint(), .ev(), nSub = 2, nStud = 5, thetaMat = .t),
      "does not draw 'om.eta.cl' from"
    )
  })

  test_that("a fit's covariance wins over a prior not centered on the estimate (#1502)", {
    skipIfOldLotri()

    ## a fit's covariance is added to the control only after the priors are
    ## applied, so the priors have to step aside for it up front
    .ui <- .map()
    .env <- new.env(parent = emptyenv())
    .env$cov <- .mapThetaMat()
    .fit <- structure(list(env = .env, iniDf = .ui$iniDf), class = "nlmixr2FitCore")
    .ctl <- rxControl(nStud = 5)
    expect_no_warning(.ret <- .rxPriorApplyControl(.fit, .ctl))
    expect_null(.ret$thetaMat)

    ## and a covariance that misses a prior parameter says so
    .env$cov <- lotri::lotri(tcl ~ 0.02)
    expect_warning(.rxPriorApplyControl(.fit, .ctl), "does not draw 'tka'")

    ## no covariance step: the prior is used, and has to be centered
    rm("cov", envir = .env)
    expect_error(.rxPriorApplyControl(.fit, .ctl), "is not the initial estimate")
  })

  test_that("usePrior=TRUE says why when it cannot be honored", {
    skipIfOldLotri()

    expect_error(rxSolve(.plain(), .ev(), nSub = 2, nStud = 5, usePrior = TRUE), "specifies no prior")
    expect_error(rxSolve(.mod(), .ev(), nSub = 2, nStud = 1, usePrior = TRUE), "no variability would be simulated")
  })

  test_that("simVariability decides whether the priors apply, not nStud", {
    skipIfOldLotri()

    ## the C++ side resolves `simVar` from `simVariability` first and only
    ## falls back to `nStud > 1`, so the R side has to agree or the priors
    ## are dropped in one direction and wasted in the other
    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 1, simVariability = TRUE)
    })
    expect_equal(length(.s$omegaList), 1L)

    withr::with_seed(7, {
      .s <- rxSolve(.mod(), .ev(), nSub = 2, nStud = 20, simVariability = FALSE)
    })
    expect_equal(length(.s$omegaList), 0L)
  })
})
