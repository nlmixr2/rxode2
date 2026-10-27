rxTest({
  ## `omegaSeparation="invchol"` -- the omega (and sigma) entries carried in
  ## the `thetaMat` are drawn jointly with the thetas on the inverse
  ## Cholesky scale, so every drawn matrix is positive definite (#1486)

  .mod <- function() {
    rxode2({
      ka <- exp(tka + eta.ka)
      cl <- exp(tcl + eta.cl)
      v <- exp(tv + eta.v)
      cp <- linCmt()
    })
  }

  .ev <- function() et(amt = 100) |> et(seq(0, 24, by = 8))

  .omega <- function() {
    lotri::lotri(eta.ka + eta.cl ~ c(0.3, 0.05, 0.2), eta.v ~ 0.1)
  }

  .params <- c(tka = 0.45, tcl = 1, tv = 3.45)

  .nm <- c("tka", "om.eta.ka", "cov.eta.cl.eta.ka", "om.eta.cl")

  .thetaMat <- function(v) {
    .m <- diag(v)
    dimnames(.m) <- list(.nm, .nm)
    .m
  }

  .solve <- function(tm, sep, nStud, seed = 3, ...) {
    withr::with_seed(seed, {
      rxSolve(
        .mod(),
        .ev(),
        params = .params,
        omega = .omega(),
        thetaMat = tm,
        nSub = 2,
        nStud = nStud,
        omegaSeparation = sep,
        ...
      )
    })
  }

  .pd <- function(lst) {
    all(vapply(lst, function(m) all(eigen(m, symmetric = TRUE)$values > 0), logical(1)))
  }

  test_that("a small uncertainty gives the same spread as tnpri", {
    .tm <- .thetaMat(c(0.01, 0.0004, 0.0001, 0.0004))
    .s <- .solve(.tm, "invchol", 400)
    expect_equal(length(.s$omegaList), 400L)

    .d1 <- vapply(.s$omegaList, function(m) m[1, 1], double(1))
    .off <- vapply(.s$omegaList, function(m) m[2, 1], double(1))
    .d2 <- vapply(.s$omegaList, function(m) m[2, 2], double(1))
    ## Monte Carlo tolerances: a mean is +/- sd/sqrt(n), an sd +/- sd/sqrt(2n)
    expect_equal(mean(.d1), 0.3, tolerance = 0.02)
    expect_equal(sd(.d1), 0.02, tolerance = 0.15)
    expect_equal(mean(.off), 0.05, tolerance = 0.1)
    expect_equal(sd(.off), 0.01, tolerance = 0.15)
    expect_equal(mean(.d2), 0.2, tolerance = 0.02)
    expect_equal(sd(.d2), 0.02, tolerance = 0.15)
    expect_true(.pd(.s$omegaList))
  })

  test_that("a large uncertainty never gives a matrix that is not positive definite", {
    ## the off diagonal sd is as large as it can be and stay positive
    ## definite, so tnpri has to fall back to the nearest one or give up
    .tm <- .thetaMat(c(0.01, 0.01, 0.06, 0.01))
    .cnd <- tryCatch(
      .solve(.tm, "tnpri", 200, priorPdRetry = 1),
      warning = function(w) w,
      error = function(e) e
    )
    expect_match(conditionMessage(.cnd), "positive definite")
    expect_no_warning(.s <- .solve(.tm, "invchol", 200))
    expect_true(.pd(.s$omegaList))
    .off <- vapply(.s$omegaList, function(m) m[2, 1], double(1))
    expect_gt(length(unique(.off)), 150L)
  })

  test_that("a block without a drawn entry stays at its estimate", {
    .s <- .solve(.thetaMat(c(0.01, 0.0004, 0.0001, 0.0004)), "invchol", 20)
    for (.m in .s$omegaList) {
      expect_identical(.m[3, 3], 0.1)
      expect_identical(.m[3, 1:2], c(eta.ka = 0, eta.cl = 0))
    }
  })

  test_that("the theta and omega draws stay correlated", {
    .tm <- .thetaMat(c(0.01, 0.0004, 0.0001, 0.0004))
    .tm["tka", "om.eta.ka"] <- .tm["om.eta.ka", "tka"] <- 0.0016
    .s <- .solve(.tm, "invchol", 400, seed = 4)
    .d1 <- vapply(.s$omegaList, function(m) m[1, 1], double(1))
    ## 0.0016 / sqrt(0.01 * 0.0004) = 0.8
    expect_equal(cor(unique(.s$params$tka), .d1), 0.8, tolerance = 0.06)
  })

  test_that("a Cholesky thetaMat gives the same draws", {
    .tm <- .thetaMat(c(0.01, 0.0004, 0.0001, 0.0004))
    .tm["tka", "om.eta.ka"] <- .tm["om.eta.ka", "tka"] <- 0.0016
    .a <- .solve(.tm, "invchol", 5)
    .ch <- chol(.tm)
    .b <- .solve(.ch, "invchol", 5, thetaIsChol = TRUE)
    expect_equal(.a$omegaList, .b$omegaList)
  })

  test_that("dfSub is not used", {
    .tm <- .thetaMat(c(0.01, 0.0004, 0.0001, 0.0004))
    expect_equal(
      .solve(.tm, "invchol", 5)$omegaList,
      .solve(.tm, "invchol", 5, dfSub = 100)$omegaList
    )
  })

  test_that("the omega estimate has to be positive definite", {
    .om <- lotri::lotri(eta.ka + eta.cl ~ c(0.3, 0.5, 0.2), eta.v ~ 0.1)
    expect_error(
      suppressWarnings(withr::with_seed(3, {
        rxSolve(
          .mod(),
          .ev(),
          params = .params,
          omega = .om,
          thetaMat = .thetaMat(c(0.01, 0.0004, 0.0001, 0.0004)),
          nSub = 2,
          nStud = 5,
          omegaSeparation = "invchol"
        )
      })),
      "needs a positive definite 'omega'"
    )
  })

  test_that("the missing pieces are named as for tnpri", {
    .tm <- matrix(0.01, 1, 1, dimnames = list("tka", "tka"))
    expect_error(
      .solve(.tm, "invchol", 5),
      "'omegaSeparation=\"invchol\"' was given but no 'thetaMat' column"
    )
  })

  test_that("sigma is drawn the same way", {
    .m <- rxode2({
      ka <- exp(tka)
      cl <- exp(tcl)
      v <- exp(tv)
      cp <- linCmt() + add.eps + add2.eps
    })
    .sg <- lotri::lotri(add.eps + add2.eps ~ c(0.4, 0.05, 0.3))
    .n <- c("tka", "add.eps", "sigma2.1", "add2.eps")
    .tm <- diag(c(0.01, 0.0004, 0.0001, 0.0004))
    dimnames(.tm) <- list(.n, .n)
    withr::with_seed(9, {
      .s <- suppressWarnings(rxSolve(
        .m,
        .ev(),
        params = .params,
        sigma = .sg,
        thetaMat = .tm,
        nSub = 2,
        nStud = 300,
        sigmaSeparation = "invchol"
      ))
    })
    expect_equal(length(.s$sigmaList), 300L)
    .d1 <- vapply(.s$sigmaList, function(x) x[1, 1], double(1))
    .off <- vapply(.s$sigmaList, function(x) x[2, 1], double(1))
    expect_equal(mean(.d1), 0.4, tolerance = 0.02)
    expect_equal(sd(.d1), 0.02, tolerance = 0.15)
    expect_equal(sd(.off), 0.01, tolerance = 0.15)
    expect_true(.pd(.s$sigmaList))
  })
})
