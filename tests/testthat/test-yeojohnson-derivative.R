rxTest({

  # The Yeo-Johnson transform is monotone increasing, so rxTBSd() must be
  # positive everywhere.  On the negative branch lambda == 2 is a special case
  # (yj(x) = -log(1 - x)); it used to return the derivative with the wrong sign,
  # which also made it disagree with the general formula in the limit.

  .yjModel <- function() {
    model({
      v <- rxTBS(xv, lam, 1, 1, 0)
      d1 <- rxTBSd(xv, lam, 1, 1, 0)
      d2 <- rxTBSd2(xv, lam, 1, 1, 0)
    })
  }

  .yj <- function(x, lambda) {
    .et <- et(seq_along(x))
    .et$xv <- x
    .et$lam <- lambda
    as.data.frame(rxSolve(.yjModel, .et, cores = 1L))[, c("v", "d1", "d2")]
  }

  test_that("yeoJohnson derivatives at lambda = 2 have the right sign (negative branch)", {
    x <- c(-3, -1, -0.5)
    r <- .yj(x, 2.0)
    # yj(x) = -log(1 - x); yj'(x) = 1/(1 - x); yj''(x) = 1/(1 - x)^2
    expect_equal(r$v, -log1p(-x))
    expect_equal(r$d1, 1 / (1 - x))
    expect_equal(r$d2, 1 / ((1 - x) * (1 - x)))
    expect_true(all(r$d1 > 0))
    expect_true(all(r$d2 > 0))
  })

  test_that("yeoJohnson derivatives are continuous in lambda at lambda = 2", {
    x <- c(-3, -1, -0.5)
    at2 <- .yj(x, 2.0)
    lo <- .yj(x, 2.0 - 1e-6)
    hi <- .yj(x, 2.0 + 1e-6)
    expect_equal(at2$d1, lo$d1, tolerance = 1e-5)
    expect_equal(at2$d1, hi$d1, tolerance = 1e-5)
    expect_equal(at2$d2, lo$d2, tolerance = 1e-5)
    expect_equal(at2$d2, hi$d2, tolerance = 1e-5)
  })

  test_that("yeoJohnson derivatives match finite differences on both branches", {
    x <- c(-3, -1, -0.5, 0.5, 2)
    h <- 1e-4
    for (lambda in c(2.0, 0.5, 1.5, 0)) {
      r <- .yj(x, lambda)
      fp <- .yj(x + h, lambda)$v
      fm <- .yj(x - h, lambda)$v
      expect_equal(r$d1, (fp - fm) / (2 * h), tolerance = 1e-5)
      expect_equal(r$d2, (fp - 2 * r$v + fm) / (h * h), tolerance = 1e-3)
    }
  })

  # General transform model: yj code and bounds as covariates.  Codes: 4 logit,
  # 5 logit + yeoJohnson, 6 probit, 7 probit + yeoJohnson; x in (lo, hi).  For
  # yj 5/7, x below the midpoint gives a negative inner logit/probit value, so
  # these also exercise the negative Yeo-Johnson branch (including lambda == 2).
  .tbsModel <- function() {
    model({
      v <- rxTBS(xv, lam, yjc, lo, hi)
      d1 <- rxTBSd(xv, lam, yjc, lo, hi)
      d2 <- rxTBSd2(xv, lam, yjc, lo, hi)
      vi <- rxTBSi(v, lam, yjc, lo, hi)
    })
  }

  .tbs <- function(x, lambda, yjc, lo = 0, hi = 1) {
    .et <- et(seq_along(x))
    .et$xv <- x
    .et$lam <- lambda
    .et$yjc <- yjc
    .et$lo <- lo
    .et$hi <- hi
    as.data.frame(rxSolve(.tbsModel, .et, cores = 1L))[, c("v", "d1", "d2", "vi")]
  }

  test_that("logit/probit (+ yeoJohnson) derivatives match finite differences", {
    x <- c(0.15, 0.35, 0.5, 0.75, 0.9)
    h <- 1e-6
    for (yjc in c(4, 5, 6, 7)) {
      for (lambda in c(2.0, 1.0, 0.5, 0)) {
        r <- .tbs(x, lambda, yjc)
        fp <- .tbs(x + h, lambda, yjc)
        fm <- .tbs(x - h, lambda, yjc)
        expect_equal(r$d1, (fp$v - fm$v) / (2 * h), tolerance = 1e-4)
        expect_equal(r$d2, (fp$d1 - fm$d1) / (2 * h), tolerance = 1e-4)
      }
    }
  })

  test_that("rxTBSi() inverts rxTBS() for the composed transforms", {
    x <- c(0.15, 0.35, 0.5, 0.75, 0.9)
    for (yjc in c(4, 5, 6, 7)) {
      for (lambda in c(2.0, 1.0, 0.5, 0)) {
        r <- .tbs(x, lambda, yjc)
        expect_equal(r$vi, x, tolerance = 1e-6)
      }
    }
  })

  # boxCox (yj = 0) and lnorm (yj = 3) clamp x at sqrt(.Machine$double.eps)
  # for stability; the clamped x is then run through the usual derivative
  # formula, so the derivatives are continuous at the clamp boundary (they
  # used to return the clamp constant ~1.5e-8 itself below the boundary).
  test_that("boxCox/lnorm derivatives clamp x and evaluate the formula", {
    .eps <- sqrt(.Machine$double.eps)
    x <- c(0, .eps, 0.5, 2)
    xc <- pmax(x, .eps)
    r <- .tbs(x, 1.0, 3)
    expect_equal(r$d1, 1 / xc)
    expect_equal(r$d2, -1 / (xc * xc))
    for (lambda in c(0, 0.5, 2.0)) {
      r <- .tbs(x, lambda, 0)
      if (lambda == 0) {
        expect_equal(r$d1, 1 / xc)
        expect_equal(r$d2, -1 / (xc * xc))
      } else {
        expect_equal(r$d1, xc^(lambda - 1))
        expect_equal(r$d2, (lambda - 1) * xc^(lambda - 2))
      }
    }
  })

  test_that("clamped boxCox/lnorm derivatives are continuous at the boundary", {
    .eps <- sqrt(.Machine$double.eps)
    x <- c(0, .eps * (1 + 1e-8))
    r <- .tbs(x, 0.5, 0)
    expect_equal(r$d1[1], r$d1[2], tolerance = 1e-6)
    expect_equal(r$d2[1], r$d2[2], tolerance = 1e-6)
    r <- .tbs(x, 1.0, 3)
    expect_equal(r$d1[1], r$d1[2], tolerance = 1e-6)
    expect_equal(r$d2[1], r$d2[2], tolerance = 1e-6)
  })

  # powerL (log-Jacobian) and powerDL (its lambda gradient) are only shared
  # via R_RegisterCCallable (nlmixr2est), so they are pinned through the
  # internal .rxTransformL() test hook.
  test_that("powerL/powerDL log-Jacobian and lambda gradient", {
    .pL <- function(x, lambda, yj) {
      rxode2:::.rxTransformL(x, lambda, transform = yj)
    }
    .pDL <- function(x, lambda, yj) {
      rxode2:::.rxTransformL(x, lambda, transform = yj, dLambda = TRUE)
    }
    # yeoJohnson log-Jacobian: (lambda-1)*log1p(x) for x >= 0,
    # (1-lambda)*log1p(-x) for x < 0
    expect_equal(.pL(c(2, -0.5, -2), 0.5, 1),
                 c(-0.5 * log1p(2), 0.5 * log1p(0.5), 0.5 * log1p(2)))
    # lambda gradient log1p(x) / -log1p(-x), including at exactly lambda == 1
    # (used to return 0 there) and x < -1 (used to be NaN)
    for (lam in c(0.25, 1, 1.75)) {
      expect_equal(.pDL(c(2, -0.5, -2), lam, 1),
                   c(log1p(2), -log1p(0.5), -log1p(2)))
    }
    # boxCox gradient log(x), including at exactly lambda == 1
    expect_equal(.pDL(2, 1, 0), log(2))
    # lambda-free transforms have a zero gradient (norm, lnorm, logit, probit)
    for (yj in c(2, 3, 4, 6)) {
      expect_equal(.pDL(0.5, 0.7, yj), 0)
    }
    # composed transforms chain through the inner logit/probit value
    # (yj = 7 used to return NA)
    expect_equal(.pDL(0.25, 0.5, 5), -log1p(log(1 / 0.25 - 1)))
    expect_equal(.pDL(0.25, 0.5, 7), -log1p(-qnorm(0.25)))
    # powerDL is the lambda derivative of powerL
    h <- 1e-6
    for (yj in c(0, 1, 5, 7)) {
      x <- c(2, -0.5, 0.35, 0.35)[match(yj, c(0, 1, 5, 7))]
      fd <- (.pL(x, 0.75 + h, yj) - .pL(x, 0.75 - h, yj)) / (2 * h)
      expect_equal(.pDL(x, 0.75, yj), fd, tolerance = 1e-6)
    }
    # logit log-Jacobian is finite at the upper bound (clamped)
    expect_true(is.finite(.pL(1, 1, 4)))
    expect_error(rxode2:::.rxTransformL(0.5, low = 1, high = 0, transform = 4L),
                 "'high' must be greater than 'low'")
  })

})
