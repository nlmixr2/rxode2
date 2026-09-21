rxTest({
  rxWithSeed(
    42,
    {
      dgs <- c("sqrt", "log", "identity")
      for (dg in dgs) {
        for (d in seq(1, rxSymInvCholN())) {
          test_that(sprintf("Omega Cholesky %sx%s, %s", d, d, dg), {
            ## Creating covariance matrix
            tmp <- matrix(rnorm(d^2), d, d)
            mcov <- tcrossprod(tmp, tmp)
            v <- suppressMessages(rxSymInvCholCreate(mcov, dg))
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
            expect_type(v$tr.28, "double")
            expect_type(v$omega.47, "list")
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
    }
  )
  test_that("an off-diagonal zero outside a block structure is a free parameter (#1365)", {
    .chk <- function(m, dg) {
      v <- suppressMessages(rxSymInvCholCreate(mat = m, diag.xform = dg))
      .n <- sum(lower.tri(m, TRUE))
      expect_equal(v$ntheta, .n)
      expect_length(v$theta, .n)
      if (dg == "sqrt") expect_length(v$theta.diag, .n)
      expect_equal(v$omega, m, ignore_attr = TRUE, tolerance = 1e-8)
      expect_equal(v$omegaInv, solve(m), ignore_attr = TRUE, tolerance = 1e-8)
      expect_length(v$d.omegaInv, .n)
      expect_length(v$d.D.omegaInv, .n)
    }
    m <- matrix(c(1, 0.1, 0.1, 0.1, 1, 0, 0.1, 0, 1), 3, 3)
    dimnames(m) <- list(paste0("e", 1:3), paste0("e", 1:3))
    for (dg in c("sqrt", "log", "identity")) .chk(m, dg)
    ## a permuted block pattern is not contiguous either
    .chk(matrix(c(1, 0, 0.1, 0, 1, 0, 0.1, 0, 1), 3, 3), "sqrt")
    ## near-singular SAEM Omega with one exact zero
    m <- matrix(c(2.6563e-02, 2.3205e-02, 7.5781e-15, 7.5791e-15,
                  2.3205e-02, 3.2003e-02, -3.0305e-14, -4.5475e-14,
                  7.5781e-15, -3.0305e-14, 5.2647e-10, 0,
                  7.5791e-15, -4.5475e-14, 0, 5.2647e-10), 4, 4)
    v <- suppressMessages(rxSymInvCholCreate(mat = m, diag.xform = "sqrt"))
    expect_equal(v$ntheta, 10L)
    expect_equal(v$omega, m, ignore_attr = TRUE, tolerance = 1e-6)
    ## a zero inside one block of a block diagonal Omega
    m <- lotri::lotri(a + b + c ~ c(1, 0.1, 1, 0.1, 0, 1), d ~ 2)
    v <- suppressMessages(rxSymInvCholCreate(mat = m, diag.xform = "sqrt"))
    expect_equal(v$ntheta, 7L)
    expect_equal(v$omega, unclass(m), ignore_attr = TRUE, tolerance = 1e-8)
  })
})
