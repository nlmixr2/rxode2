if (!.Call(`_rxode2_isIntel`)) {
  ## Adapted from https://github.com/biobakery/banocc/blob/master/tests/testthat/test_utils_rlkj.R
  # context("Utilities - rLKJ1")
  d_vals <- c(2, 5) # seq(2, 200, 10)
  eta_vals <- c(1, 50) # seq(1, 200, 10)

  test_that("rLKJ1 returns a matrix", {
    for (d in d_vals) {
      for (eta in eta_vals) {
        eval(bquote(expect_true(is.matrix(rLKJ1(d = .(d), eta = .(eta))))))
        eval(bquote(
          expect_true(is.matrix(rLKJ1(d = .(d), eta = .(eta), cholesky = TRUE)))
        ))
      }
    }
  })

  test_that("rLKJ1 returns square matrix", {
    test_square <- function(d, e, ch) {
      r <- rLKJ1(d = d, eta = e, cholesky = ch)
      expect_equal(nrow(r), ncol(r))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_square(d, eta, FALSE)
        test_square(d, eta, TRUE)
      }
    }
  })

  test_that("rLKJ1 returns matrix with d rows", {
    test_nrow <- function(d, e, ch) {
      eval(bquote(
        expect_equal(nrow(rLKJ1(d = .(d), eta = .(e), cholesky = .(ch))), .(d))
      ))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_nrow(d, eta, TRUE)
        test_nrow(d, eta, FALSE)
      }
    }
  })

  test_that("rLKJ1 returns positive definite matrix if cholesky=FALSE", {
    test_pd <- function(d, e) {
      eval(bquote(expect_true(all(eigen(rLKJ1(d = .(d), eta = .(e)))$values > 0))))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_pd(d, eta)
      }
    }
  })

  test_that("rLKJ1 returns symmetric matrix if cholesky=FALSE", {
    get_tri <- function(mat, type) {
      if (type == "lower") {
        t(mat)[upper.tri(mat)]
      } else {
        mat[upper.tri(mat)]
      }
    }
    test_symmetric <- function(d, e) {
      l <- rLKJ1(d = d, eta = e)
      expect_equal(
        get_tri(l, "lower"),
        get_tri(l, "upper")
      )
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_symmetric(d, eta)
      }
    }
  })

  test_that("rLKJ1 returns matrix with diagonal elts 1 if cholesky=FALSE", {
    test_diag <- function(d, e) {
      eval(bquote(
        expect_equal(diag(rLKJ1(d = .(d), eta = .(e))), rep(1, .(d)))
      ))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_diag(d, eta)
      }
    }
  })

  test_that("rLKJ1 returns matrix with elts between -1 and 1 if cholesky=FALSE", {
    test_elts <- function(d, e) {
      r <- rLKJ1(d = d, eta = e)
      r[upper.tri(r)]
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        expect_true(all(test_elts(d, eta) >= -1))
        expect_true(all(test_elts(d, eta) <= 1))
      }
    }
  })

  test_that("rLKJ1 returns matrix with upper triangle of 0 if cholesky=TRUE", {
    get_uppertri <- function(mat) {
      mat[upper.tri(mat)]
    }
    test_uppertri <- function(d, e) {
      eval(bquote(
        expect_equal(
          get_uppertri(rLKJ1(d = .(d), eta = .(e), cholesky = TRUE)),
          rep(0, choose(.(d), 2))
        )
      ))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_uppertri(d, eta)
      }
    }
  })

  test_that("rLKJ1 corr matrix is positive definite if cholesky=TRUE", {
    test_pd <- function(d, e) {
      l <- rLKJ1(d = d, eta = e, cholesky = TRUE)
      eigen(l %*% t(l))$values
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        expect_true(all(test_pd(d, eta) > 0))
      }
    }
  })

  test_that("rLKJ1 corr matrix is symmetric if cholesky=TRUE", {
    get_tri <- function(mat, type) {
      if (type == "lower") {
        t(mat)[upper.tri(mat)]
      } else {
        mat[upper.tri(mat)]
      }
    }
    test_symmetric <- function(d, e) {
      l <- rLKJ1(d = d, eta = e, cholesky = TRUE)
      r <- l %*% t(l)
      expect_equal(get_tri(r, "lower"), get_tri(r, "upper"))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        test_symmetric(d, eta)
      }
    }
  })

  test_that("rLKJ1 corr matrix has diagonal elements of 1 if cholesky=TRUE", {
    test_diag <- function(d, e) {
      l <- rLKJ1(d = d, eta = e, cholesky = TRUE)
      diag(l %*% t(l))
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        expect_equal(test_diag(d, eta), rep(1, d))
      }
    }
  })

  test_that("rLKJ1 corr matrix has elts between -1 and 1 if cholesky=TRUE", {
    test_elts <- function(d, e) {
      l <- rLKJ1(d = d, eta = e, cholesky = TRUE)
      (l %*% t(l))[upper.tri(l)]
    }
    for (d in d_vals) {
      for (eta in eta_vals) {
        expect_true(all(test_elts(d, eta) >= -1))
        expect_true(all(test_elts(d, eta) <= 1))
      }
    }
  })

  test_that("rLKJ1 gives error if eta < 1", {
    err_string <- "must be >= 1"
    for (d in d_vals) {
      for (eta in c(-1, 0)) {
        expect_error(rLKJ1(d = d, eta = eta, cholesky = TRUE), err_string)
        expect_error(rLKJ1(d = d, eta = eta, cholesky = FALSE), err_string)
      }
    }
  })

  test_that("rLKJ1 gives error if d < 2", {
    err_string <- "must be > 1"
    for (d in c(-1, 0, 1)) {
      for (eta in eta_vals) {
        expect_error(rLKJ1(d = d, eta = eta, cholesky = TRUE), err_string)
        expect_error(rLKJ1(d = d, eta = eta, cholesky = FALSE), err_string)
      }
    }
  })

  # context("Utilities - invWR1d")
  nu_vals <- c(4.5, 50)
  d_vals <- c(2, 5)

  test_that("invWR1d returns a matrix", {
    for (d in d_vals) {
      for (nu in nu_vals) {
        expect_true(is.matrix(eval(bquote(invWR1d(d = .(d), nu = .(nu))))))
        eval(bquote(
          expect_true(is.matrix(invWR1d(d = .(d), nu = .(nu))))
        ))
      }
    }
  })

  test_that("invWR1d returns square matrix", {
    test_square <- function(d, e) {
      r <- invWR1d(d = d, nu = e)
      expect_equal(nrow(r), ncol(r))
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_square(d, nu)
      }
    }
  })

  test_that("invWR1d returns matrix with d rows", {
    test_nrow <- function(d, e) {
      eval(bquote(
        expect_equal(nrow(invWR1d(d = .(d), nu = .(e))), .(d))
      ))
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_nrow(d, nu)
      }
    }
  })

  test_that("invWR1d returns positive definite matrix if cholesky=FALSE", {
    test_pd <- function(d, e) {
      eval(bquote(expect_true(all(eigen(invWR1d(d = .(d), nu = .(e)))$values > 0))))
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_pd(d, nu)
      }
    }
  })

  test_that("invWR1d returns symmetric matrix", {
    get_tri <- function(mat, type) {
      if (type == "lower") {
        t(mat)[upper.tri(mat)]
      } else {
        mat[upper.tri(mat)]
      }
    }
    test_symmetric <- function(d, e) {
      l <- invWR1d(d = d, nu = e)
      expect_equal(
        get_tri(l, "lower"),
        get_tri(l, "upper")
      )
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_symmetric(d, nu)
      }
    }
  })

  test_that("invWR1d returns matrix with diagonal elts 1", {
    test_diag <- function(d, e) {
      eval(bquote(
        expect_equal(diag(invWR1d(d = .(d), nu = .(e))), rep(1, .(d)))
      ))
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_diag(d, nu)
      }
    }
  })

  test_that("invWR1d returns matrix with elts between -1 and 1", {
    test_elts <- function(d, e) {
      r <- invWR1d(d = d, nu = e)
      r[upper.tri(r)]
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        expect_true(all(test_elts(d, nu) >= -1))
        expect_true(all(test_elts(d, nu) <= 1))
      }
    }
  })

  test_that("invWR1d corr matrix is positive definite", {
    test_pd <- function(d, e) {
      l <- invWR1d(d = d, nu = e)
      eigen(l %*% t(l))$values
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        expect_true(all(test_pd(d, nu) > 0))
      }
    }
  })

  test_that("invWR1d corr matrix is symmetric", {
    get_tri <- function(mat, type) {
      if (type == "lower") {
        t(mat)[upper.tri(mat)]
      } else {
        mat[upper.tri(mat)]
      }
    }
    test_symmetric <- function(d, e) {
      l <- invWR1d(d = d, nu = e)
      r <- l %*% t(l)
      expect_equal(get_tri(r, "lower"), get_tri(r, "upper"))
    }
    for (d in d_vals) {
      for (nu in nu_vals) {
        test_symmetric(d, nu)
      }
    }
  })

  test_that("invWR1d gives error if nu < d-1", {
    err_string <- "'nu' must be greater than 'd'-1"
    for (d in d_vals) {
      for (nu in c(-1, 1)) {
        expect_error(invWR1d(d = d, nu = nu), err_string)
      }
    }
  })
}
