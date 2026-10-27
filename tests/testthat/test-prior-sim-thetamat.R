rxTest({
  ## Which uncertainty a study draws when the model has priors and a
  ## covariance of its estimates is also available (#1502)

  test_that("a call site thetaMat wins over a prior not centered on the estimate (#1502)", {
    skipIfOldLotri()

    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.priorMapMod(), .priorMapEv(), nSub = 2, nStud = 200, thetaMat = .priorMapThetaMat())
      }),
      message = "prior|thetaMat"
    )
    expect_equal(sort(colnames(.s$thetaMat)), c("tcl", "tka"))
    expect_equal(sd(.s$thetaMat[, "tka"]), 0.1, tolerance = 0.2)
  })

  test_that("a meta thetaMat wins over the priors without a warning (#1502)", {
    skipIfOldLotri()

    .ui <- .priorMapMod(meta = TRUE)
    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.ui, .priorMapEv(), nSub = 2, nStud = 200)
      }),
      message = "prior"
    )
    expect_equal(sort(colnames(.s$thetaMat)), c("tcl", "tka"))
    expect_equal(sd(.s$thetaMat[, "tcl"]), sqrt(0.02), tolerance = 0.2)
  })

  test_that("a meta thetaMat wins over a prior centered on the estimate (#1502)", {
    skipIfOldLotri()

    .ui <- .priorMapMod(centered = TRUE)
    assign("thetaMat", lotri::lotri(tka ~ 1), envir = .ui$meta)
    expect_no_warning(
      withr::with_seed(1, {
        .s <- rxSolve(.ui, .priorMapEv(), nSub = 2, nStud = 200)
      }),
      message = "prior|thetaMat"
    )
    expect_gt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  test_that("usePrior=TRUE uses the priors over a given thetaMat (#1502)", {
    skipIfOldLotri()

    ## the prior is still required to be centered on the estimate
    expect_error(
      rxSolve(.priorMapMod(), .priorMapEv(), nSub = 2, nStud = 5, thetaMat = .priorMapThetaMat(), usePrior = TRUE),
      "is not the initial estimate"
    )

    .m <- matrix(1, 1, 1, dimnames = list("tka", "tka"))
    expect_warning(
      {
        withr::with_seed(1, {
          .s <- rxSolve(
            .priorMapMod(centered = TRUE),
            .priorMapEv(),
            nSub = 2,
            nStud = 200,
            thetaMat = .m,
            usePrior = TRUE
          )
        })
      },
      "replace the 'thetaMat'"
    )
    expect_lt(sd(.s$thetaMat[, "tka"]), 0.5)
  })

  test_that("a fit's covariance wins over a prior not centered on the estimate (#1502)", {
    skipIfOldLotri()

    ## a fit's covariance is added to the control only after the priors are
    ## applied, so the priors have to step aside for it up front
    .ui <- .priorMapMod()
    .env <- new.env(parent = emptyenv())
    .env$cov <- .priorMapThetaMat()
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
})
