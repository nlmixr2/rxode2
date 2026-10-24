rxTest({
  test_that("untruncated multivariate t has the t covariance", {
    rxWithSeed(42, {
      s <- matrix(c(4, 1.8, 1.8, 1), 2)
      x <- rxRmvt(100000, c(1, 2), s, df = 10)
      expect_equal(colMeans(x), c(1, 2), tolerance = 0.02)
      expect_equal(cov(x), s * 10 / 8, tolerance = 0.03)
    })
  })

  test_that("df = Inf matches rxRmvn()", {
    s <- matrix(c(4, 1.8, 1.8, 1), 2)
    x <- rxWithSeed(42, rxRmvt(10, c(1, 2), s, df = Inf))
    y <- rxWithSeed(42, rxRmvn(10, c(1, 2), s))
    expect_equal(x, y)
    x <- rxWithSeed(42, rxRmvt(10, c(1, 2), s, df = Inf, lower = 0, upper = 3))
    y <- rxWithSeed(42, rxRmvn(10, c(1, 2), s, lower = 0, upper = 3))
    expect_equal(x, y)
  })

  test_that("truncated t agrees with rejection sampling", {
    rxWithSeed(42, {
      s <- matrix(c(4, 1.8, 1.8, 1), 2)
      x <- rxRmvt(40000, c(0, 0), s, df = 5, lower = c(-Inf, -0.5), upper = Inf)
      expect_true(all(x[, 2] > -0.5))
      y <- rxRmvt(300000, c(0, 0), s, df = 5)
      y <- y[y[, 2] > -0.5, ]
      expect_equal(colMeans(x), colMeans(y), tolerance = 0.03)
      expect_equal(cov(x), cov(y), tolerance = 0.05)

      s3 <- matrix(c(2, .6, -.4, .6, 1, .3, -.4, .3, 1.5), 3)
      lo <- c(-Inf, 0.2, -1)
      up <- c(1, Inf, 0.5)
      mu <- c(0.5, 0, 0)
      x <- rxRmvt(40000, mu, s3, df = 10, lower = lo, upper = up)
      for (i in 1:3) {
        expect_true(all(x[, i] >= lo[i] & x[, i] <= up[i]))
      }
      y <- rxRmvt(600000, mu, s3, df = 10)
      y <- y[y[, 1] < 1 & y[, 2] > 0.2 & y[, 3] > -1 & y[, 3] < 0.5, ]
      expect_equal(colMeans(x), colMeans(y), tolerance = 0.03)
      expect_equal(cov(x), cov(y), tolerance = 0.05)
    })
  })

  test_that("univariate truncated t matches the analytic mean", {
    rxWithSeed(42, {
      x <- rxRmvt(50000, 1, matrix(4), df = 4, lower = 0, upper = 3)
      expect_true(all(x >= 0 & x <= 3))
      .f <- function(y) dt((y - 1) / 2, 4) / 2
      .m <- integrate(function(y) y * .f(y), 0, 3)$value / integrate(.f, 0, 3)$value
      expect_equal(mean(x), .m, tolerance = 0.01)
    })
  })

  test_that("tilting bound matches Botev and L'Ecuyer (2015) tables 1 and 2", {
    .psi <- function(d, lower) {
      s <- solve(diag(d) / 2 + matrix(1, d, d) / 2)
      exp(rxRmvtTilt_(s, rep(lower, d), rep(Inf, d), 10)$psistar)
    }
    expect_equal(.psi(5, -1), 0.33, tolerance = 0.01)
    expect_equal(.psi(10, -1), 0.063, tolerance = 0.02)
    expect_equal(.psi(5, 0), 0.0030, tolerance = 0.02)
    expect_equal(.psi(10, 0), 2.67e-7, tolerance = 0.01)
  })

  test_that("rxRmvt() seeds, zero scale and errors", {
    s <- matrix(c(4, 1.8, 1.8, 1), 2)
    a1 <- rxWithSeed(10, rxRmvt(5, c(0, 0), s, df = 3, lower = -1, upper = 1))
    a2 <- rxWithSeed(10, rxRmvt(5, c(0, 0), s, df = 3, lower = -1, upper = 1, ncores = 2))
    expect_equal(a1, a2)
    a3 <- rxWithSeed(11, rxRmvt(5, c(0, 0), s, df = 3, lower = -1, upper = 1))
    expect_false(isTRUE(all.equal(a1, a3)))
    expect_true(all(rxRmvt(4, c(1, 2), matrix(0, 2, 2), df = 3) == rep(c(1, 2), each = 4)))
    u <- chol(s)
    x <- rxWithSeed(10, rxRmvt(5, c(0, 0), u, df = 3, lower = -1, upper = 1, isChol = TRUE))
    expect_equal(x, a1)
    expect_error(rxRmvt(4, 0, matrix(0), df = 3, lower = 1, upper = 2), "upper")
    # a loose nlTol is tightened so the envelope stays valid (half-Cauchy median 1)
    x <- rxWithSeed(3, rxRmvt(20000, 0, matrix(1), df = 1, lower = 0, upper = Inf, nlTol = 2))
    expect_equal(median(x), 1, tolerance = 0.05)
    expect_error(rxRmvt(5, c(0, 0), s, df = 0.5, lower = -1, upper = 1), "df")
    expect_error(rxRmvt(5, c(0, 0), s, df = -1))
    expect_error(rxRmvt(5, c(0, 0), s, df = 3, lower = 1, upper = 1))
    expect_error(rxRmvt(5, c(0, 0), matrix(c(1, 2, 2, 1), 2), df = 3, lower = -1, upper = 1),
                 "positive")
  })

  test_that("C handle draws are thread safe and reproducible per id", {
    rxSeedEng(2L)
    s <- matrix(c(4, 1.8, 1.8, 1), 2)
    a1 <- rxRmvtTeamDraw_(c(0, 0), s, 5, c(-Inf, -0.5), c(Inf, Inf), 10L, 8L, 1L, 42)
    a2 <- rxRmvtTeamDraw_(c(0, 0), s, 5, c(-Inf, -0.5), c(Inf, Inf), 10L, 8L, 2L, 42)
    expect_identical(a1, a2)
    expect_true(all(a1[, 2, ] > -0.5))
    expect_false(isTRUE(all.equal(a1[, , 1], a1[, , 2])))
  })
})
