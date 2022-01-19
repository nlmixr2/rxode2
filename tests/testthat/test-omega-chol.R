rxode2Test(
  {
    set.seed(42)
    dgs <- c("sqrt", "log", "identity")
    for (dg in dgs) {
      for (d in seq(1, rxSymInvCholN())) {
        test_that(sprintf("Omega Cholesky %sx%s, %s", d, d, dg), {
          ## Creating covariance matrix
          tmp <- matrix(rnorm(d^2), d, d)
          mcov <- tcrossprod(tmp, tmp)
          v <- rxSymInvCholCreate(mcov, dg)
          expect_equal(v$ntheta, sum((lower.tri(mcov, TRUE)) * 1))
          expect_equal(length(v$xType), sum((lower.tri(mcov, TRUE)) * 1))
          expect_equal(v$omega, mcov, tolerance = 1e-4)
          expect_equal(v$omegaInv, solve(mcov), tolerance = 1e-4)
          expect_equal(v$chol.omegaInv, chol(solve(mcov)), tolerance = 1e-4)
          expect_equal(v$chol.omega, chol(mcov), tolerance = 1e-4)
          expect_equal(v$log.det.OMGAinv.5, 0.5 * log(det(solve(mcov))), tolerance = 1e-4)
          expect_equal(length(v$d.omegaInv), v$ntheta, tolerance = 1e-4)
          expect_equal(length(v$d.D.omegaInv), v$ntheta, tolerance = 1e-4)
          ## This is to make sure there is no run-time error in calculation
          expect_true(inherits(v$tr.28, "numeric"))
          expect_true(inherits(v$omega.47, "list"))
          if (d != 1) {
            expect_error(v$theta <- 3)
          } else {
            v$theta <- 3 # Should work
          }
        })
      }

      test_that("diagonal indicator give correct values", {
        tmp <- rxSymInvCholCreate(matrix(c(1, 0.9, 0, 0.9, 1, 0, 0, 0, 1), ncol = 3))
        expect_equal(tmp$theta.diag, c(TRUE, FALSE, TRUE, TRUE))
        tmp <- rxSymInvCholCreate(matrix(c(1, 0.9, 0.9, 1), ncol = 2))
        expect_equal(tmp$theta.diag, c(TRUE, FALSE, TRUE))
      })
    }
  },
  test = "lvl2"
)
