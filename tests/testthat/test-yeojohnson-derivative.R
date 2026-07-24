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

})
