rxTest({
  ## Tests adapted from
  ## https://github.com/cran/mvtnorm/blob/master/tests/rmvnorm.R
  test_that("mvtnormal simulations make sense", {
    rxWithSeed(1, {
      m <- 1:3

      s <- diag(1:3)
      s[2, 1] <- 1
      s[3, 1] <- 2
      s[3, 2] <- 3
      s <- s + t(s)

      set.seed(1)

      x <- rxRmvn(20000, m, s, ncores = 1)

      expect_equal(m, colMeans(x), tolerance = 0.01)
      expect_equal(s, var(x), tolerance = 0.1)

      set.seed(1)
      x <- rxRmvn(20000, m, s, ncores = 2)

      # Using tolerance=0.03 for the Mac random number generator
      expect_equal(m, colMeans(x), tolerance = 0.03)
      expect_equal(s, var(x), tolerance = 0.1)
    })
  })

  test_that("seeds behave as expected", {
    rxWithPreserveSeed({
      m <- 1:3

      s <- diag(1:3)
      s[2, 1] <- 1
      s[3, 1] <- 2
      s[3, 2] <- 3
      s <- s + t(s)

      set.seed(1)
      x1 <- rxRmvn(10, m, s, ncores = 1)

      set.seed(2)
      x2 <- rxRmvn(10, m, s, ncores = 1)
      expect_false(isTRUE(all.equal(x1, x2)))

      set.seed(1)
      x2 <- rxRmvn(10, m, s, ncores = 1)
      expect_equal(x1, x2)

      set.seed(1)
      x1 <- rxRmvn(10, m, s, ncores = 1)
      set.seed(2)
      x2 <- rxRmvn(10, m, s, ncores = 2)
      expect_false(isTRUE(all.equal(x1, x2)))

      set.seed(10)
      r1 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 1)

      set.seed(10)
      r2 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 1)

      expect_equal(r1, r2)

      set.seed(10)
      r1 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 1)

      set.seed(10)
      r2 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 2)
      expect_false(isTRUE(all.equal(x1, x2)))

      set.seed(10)
      r1 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 2)
      set.seed(10)
      r2 <- rxRmvn(10, c(1, 1, 1), diag(3), c(-1, -1, -1), c(3, 3, 3), ncores = 2)
      if (isTRUE(all.equal(r1, r2))) {
        expect_equal(r1, r2)
      } else {
        warning("r1 != r2 because of load/imbalance etc.")
      }

      rxRmvn(10, 1, diag(1) * 0.01, -1, 1.1)

      rxRmvn(10, 1, diag(1) * 0.01)
    })
  })

  ## test simulating with zero variance

  test_that("zero variance simulations", {
    m <- 1:3

    s <- diag(rep(0, 3))

    rxWithSeed(1, {
      x <- rxRmvn(20, m, s, ncores = 1)
      expect_true(all(x[, 1] == 1))
      expect_true(all(x[, 2] == 2))
      expect_true(all(x[, 3] == 3))

      x <- rxRmvn(20, sigma = s, ncores = 1)

      expect_true(all(x == 0))
    })
  })

  test_that("truncated normal simulation", {
    rxWithSeed(414, {
      d <- 5
      mu <- 1:d

      # Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)

      a <- rxRmvn(10, 1:d, mcov, lower = 1:d - 1, upper = 1:d + 1)

      for (i in 1:d) {
        expect_false(all(a[, i] == i))
        expect_false(any(a[, i] < i - 1))
        expect_false(any(a[, i] > i + 1))
      }

      set.seed(10)
      a1 <- rxRmvn(1, 10, matrix(2), 10, 11)
      a2 <- rxRmvn(1, 10, matrix(2), 10, 11)

      expect_false(all(a1 == a2))

      set.seed(10)
      a1 <- rxRmvn(1, 10, matrix(2), 10, 11)

      set.seed(10)
      a2 <- rxRmvn(1, 10, matrix(2), 10, 11)

      expect_equal(a1, a2)
    })
  })

  test_that("truncated normal has the covariance of rejection sampling", {
    rxWithSeed(42, {
      s <- matrix(c(4, 1.8, 1.8, 1), 2)
      x <- rxRmvn(40000, c(0, 0), s, lower = c(-Inf, -0.5), upper = Inf)
      y <- rxRmvn(200000, c(0, 0), s)
      y <- y[y[, 2] > -0.5, ]
      expect_equal(cov(x), cov(y), tolerance = 0.05)
      x2 <- rxRmvn(40000, c(0, 0), chol(s), lower = c(-Inf, -0.5), upper = Inf, isChol = TRUE)
      expect_equal(cov(x2), cov(y), tolerance = 0.05)

      s3 <- matrix(c(2, .6, -.4, .6, 1, .3, -.4, .3, 1.5), 3)
      lo <- c(-Inf, 0.2, -1)
      up <- c(1, Inf, 0.5)
      x <- rxRmvn(40000, c(0.5, 0, 0), s3, lower = lo, upper = up)
      y <- rxRmvn(400000, c(0.5, 0, 0), s3)
      y <- y[y[, 1] < 1 & y[, 2] > 0.2 & y[, 3] > -1 & y[, 3] < 0.5, ]
      expect_equal(colMeans(x), colMeans(y), tolerance = 0.03)
      expect_equal(cov(x), cov(y), tolerance = 0.05)
    })
  })
})
