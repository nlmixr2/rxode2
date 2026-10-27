rxTest({
  ## the inverse Cholesky parameterization of a covariance matrix
  ## (`src/invChol.cpp`) used to draw an omega that is always positive
  ## definite (#1486)
  .par <- function(m) rxInvChol_(m, 0L)
  .mat <- function(p) rxInvChol_(p, 1L)
  .jac <- function(m) rxInvChol_(m, 2L)

  .om <- matrix(c(0.3, 0.05, 0.02, 0.05, 0.2, -0.03, 0.02, -0.03, 0.4), 3, 3)

  test_that("the parameters are the inverse Cholesky with a log diagonal", {
    .p <- .par(.om)
    .r <- chol(solve(.om))
    expect_equal(length(.p), 6L)
    expect_equal(
      .p,
      c(log(.r[1, 1]), .r[1, 2], .r[1, 3], log(.r[2, 2]), .r[2, 3], log(.r[3, 3])),
      ignore_attr = TRUE
    )
  })

  test_that("the matrix round trips", {
    expect_equal(.mat(.par(.om)), .om)
    expect_equal(.mat(.par(matrix(0.7))), matrix(0.7))
  })

  test_that("any parameter vector gives a positive definite matrix", {
    withr::with_seed(42, {
      for (.i in 1:20) {
        .m <- .mat(rnorm(6, sd = 3))
        expect_true(isSymmetric(.m))
        expect_true(all(eigen(.m, symmetric = TRUE)$values > 0))
      }
    })
  })

  test_that("the Jacobian matches finite differences", {
    .p <- .par(.om)
    .h <- 1e-6
    .num <- vapply(
      seq_along(.p),
      function(k) {
        .up <- .p
        .dn <- .p
        .up[k] <- .up[k] + .h
        .dn[k] <- .dn[k] - .h
        .a <- .mat(.up)
        .b <- .mat(.dn)
        (.a[lower.tri(.a, diag = TRUE)] - .b[lower.tri(.b, diag = TRUE)]) / (2 * .h)
      },
      double(6)
    )
    expect_equal(.jac(.om), .num, tolerance = 1e-6)
  })

  test_that("a diagonal that underflows gives an unusable matrix, not an error", {
    expect_true(all(is.nan(.mat(-1000))))
  })

  test_that("a matrix that is not positive definite is refused", {
    .bad <- matrix(c(1, 2, 2, 1), 2, 2)
    expect_error(.par(.bad), "positive definite")
    expect_error(.jac(.bad), "positive definite")
    expect_error(.mat(c(1, 2)), "lower triangle")
  })
})
