rxTest({

  .rx <- loadNamespace("rxode2")

  ## `omegaSeparation="tnpri"` -- the omega (and sigma) entries are carried
  ## in the `thetaMat` and drawn from it jointly with the thetas, rather
  ## than having their correlations redrawn by the separation strategy.
  ## That is what a covariance step from NONMEM or nlmixr2 gives.

  .mod <- function() {
    rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv)
      cp <- linCmt()
    })
  }

  .ev <- function() et(amt=100) %>% et(seq(0, 24, by=8))

  .omega <- function() {
    lotri::lotri(eta.ka + eta.cl ~ c(0.3,
                                     0.05, 0.2))
  }

  ## the uncertainty of the omega entries themselves: a variance for each
  ## of omega[1,1], omega[2,1] and omega[2,2]
  .thetaMat <- function(nm) {
    .m <- diag(c(0.01, 0.002, 0.0004, 0.002))
    dimnames(.m) <- list(c("tka", nm), c("tka", nm))
    .m
  }

  .params <- c(tka=0.45, tcl=1, tv=3.45)

  test_that("the nonmem2rx spelling is drawn jointly", {
    ## `nonmem2rx` names a diagonal by the eta and an off diagonal
    ## `omega<i>.<j>`
    .tm <- .thetaMat(c("eta.ka", "omega2.1", "eta.cl"))

    withr::with_seed(3, {
      .s <- rxSolve(.mod(), .ev(), params=.params, omega=.omega(),
                    thetaMat=.tm, nSub=2, nStud=300,
                    omegaSeparation="tnpri")
    })

    expect_equal(length(.s$omegaList), 300L)

    .d1 <- vapply(.s$omegaList, function(m) m[1, 1], double(1))
    .off <- vapply(.s$omegaList, function(m) m[2, 1], double(1))
    .d2 <- vapply(.s$omegaList, function(m) m[2, 2], double(1))

    ## Each entry is centered on the omega the model gives and spread by
    ## the thetaMat variance for it.  These are Monte Carlo estimates from
    ## 300 draws, so the tolerances are sized to the sampling error rather
    ## than to the number of digits: a mean is +/- sd/sqrt(n) and an sd is
    ## +/- sd/sqrt(2n).
    expect_equal(mean(.d1), 0.3, tolerance=0.05)
    expect_equal(sd(.d1), sqrt(0.002), tolerance=0.15)

    ## the off diagonal is drawn too -- the separation strategy would have
    ## thrown it away and redrawn the correlation from LKJ
    expect_equal(mean(.off), 0.05, tolerance=0.15)
    expect_equal(sd(.off), sqrt(0.0004), tolerance=0.15)
    expect_gt(length(unique(.off)), 100L)

    expect_equal(mean(.d2), 0.2, tolerance=0.05)

    ## and what comes back is usable
    expect_true(all(vapply(.s$omegaList,
                           function(m) all(eigen(m)$values > 0), logical(1))))
  })

  test_that("the nlmixr2est spelling gives the same draws", {
    ## `.foceiOmegaCovNames()` writes `om.<eta>` and `cov.<eta1>.<eta2>`
    .nm <- c("om.eta.ka", "cov.eta.cl.eta.ka", "om.eta.cl")

    withr::with_seed(3, {
      .a <- rxSolve(.mod(), .ev(), params=.params, omega=.omega(),
                    thetaMat=.thetaMat(.nm), nSub=2, nStud=50,
                    omegaSeparation="tnpri")
    })
    withr::with_seed(3, {
      .b <- rxSolve(.mod(), .ev(), params=.params, omega=.omega(),
                    thetaMat=.thetaMat(c("eta.ka", "omega2.1", "eta.cl")),
                    nSub=2, nStud=50, omegaSeparation="tnpri")
    })

    ## the same matrix by either spelling
    expect_equal(.a$omegaList, .b$omegaList)
  })

  test_that("tnpri keeps the theta and omega draws correlated", {
    ## the point of drawing them jointly: a covariance between a theta and
    ## an omega entry survives, which the separation strategy cannot carry
    .nm <- c("tka", "om.eta.ka", "om.eta.cl")
    .tm <- matrix(c(0.01,  0.004, 0,
                    0.004, 0.002, 0,
                    0,     0,     0.002), 3, 3,
                  dimnames=list(.nm, .nm))

    withr::with_seed(4, {
      .s <- rxSolve(.mod(), .ev(), params=.params, omega=.omega(),
                    thetaMat=.tm, nSub=2, nStud=400,
                    omegaSeparation="tnpri")
    })

    .d1 <- vapply(.s$omegaList, function(m) m[1, 1], double(1))
    ## 0.004 / sqrt(0.01 * 0.002) = 0.894
    expect_equal(cor(unique(.s$params$tka), .d1), 0.894, tolerance=0.06)
  })

  test_that("tnpri says what is missing rather than ignoring it", {
    ## a thetaMat that names no omega entry cannot be what was meant
    .tm <- matrix(0.01, 1, 1, dimnames=list("tka", "tka"))
    expect_error(rxSolve(.mod(), .ev(), params=.params, omega=.omega(),
                         thetaMat=.tm, nSub=2, nStud=10,
                         omegaSeparation="tnpri"),
                 "no 'thetaMat' column names a omega entry")

    ## and the omega has to be a matrix, since the draws are added to it
    expect_error(rxSolve(.mod(), .ev(), params=.params,
                         omega=c("eta.ka", "eta.cl"),
                         thetaMat=.thetaMat(c("eta.ka", "omega2.1", "eta.cl")),
                         nSub=2, nStud=10, omegaSeparation="tnpri"),
                 "needs 'omega' to be a matrix")
  })

  test_that("the separation strategy is unchanged", {
    ## The same eta-named thetaMat columns still mean that eta's variance
    ## under the existing strategy, which needs `dfSub` and redraws the
    ## correlations.  `thetaLower=0` keeps the drawn variances positive,
    ## which that strategy requires -- see `test-nearpd.R`.
    .nm <- c("eta.ka", "eta.cl")
    .tm <- diag(c(0.01, 0.002, 0.002))
    dimnames(.tm) <- list(c("tka", .nm), c("tka", .nm))

    withr::with_seed(3, {
      .s <- rxSolve(.mod(), .ev(),
                    params=c(.params, eta.ka=0.3, eta.cl=0.2),
                    omega=.nm, thetaMat=.tm, thetaLower=0,
                    nSub=2, nStud=20, dfSub=10)
    })
    expect_equal(length(.s$omegaList), 20L)

    ## and nothing routed it through the joint draw
    .ctl <- rxControl(omega=.nm, thetaMat=.tm, omegaSeparation="auto")
    expect_null(.rx$.rxTnpriApplyControl(.ctl)$priorOmegaEl)
  })
})
