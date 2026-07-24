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
    for (yjc in c(4, 5, 7)) {
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

})
