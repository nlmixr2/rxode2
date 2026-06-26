rxTest({
  # Non-Cholesky (variance-covariance) Omega derivatives must match finite
  # differences, including off-diagonal (block) elements.
  L <- matrix(c(0.6, 0, 0,  0.1, 0.4, 0,  -0.05, 0.08, 0.3), 3, byrow = TRUE)
  Om <- L %*% t(L)
  r <- rxOmegaVarCovDeriv(Om, order = 2L)
  np <- nrow(r$elements)
  perturb <- function(O, k, hh) { a <- r$elements[k, 1]; b <- r$elements[k, 2]
    O[a, b] <- O[a, b] + hh; if (a != b) O[b, a] <- O[b, a] + hh; O }
  h <- 1e-5

  test_that("first derivatives of Omega^-1 and log|Omega| match FD", {
    e1Oi <- 0; e1ld <- 0
    for (k in seq_len(np)) {
      fdOi <- (solve(perturb(Om, k, h)) - solve(perturb(Om, k, -h))) / (2 * h)
      e1Oi <- max(e1Oi, max(abs(r$dOmegaInv[[k]] - fdOi)))
      fdld <- (determinant(perturb(Om, k, h), logarithm = TRUE)$modulus -
               determinant(perturb(Om, k, -h), logarithm = TRUE)$modulus) / (2 * h)
      e1ld <- max(e1ld, abs(r$dLogDet[k] - as.numeric(fdld)))
    }
    expect_lt(e1Oi, 1e-4)
    expect_lt(e1ld, 1e-4)
  })

  test_that("second derivative of log|Omega| matches FD", {
    e2 <- 0
    for (j in seq_len(np)) for (k in seq_len(np)) {
      pp <- perturb(perturb(Om, j, h), k, h); pm <- perturb(perturb(Om, j, h), k, -h)
      mp <- perturb(perturb(Om, j, -h), k, h); mm <- perturb(perturb(Om, j, -h), k, -h)
      fd <- (as.numeric(determinant(pp, logarithm = TRUE)$modulus) -
             as.numeric(determinant(pm, logarithm = TRUE)$modulus) -
             as.numeric(determinant(mp, logarithm = TRUE)$modulus) +
             as.numeric(determinant(mm, logarithm = TRUE)$modulus)) / (4 * h * h)
      e2 <- max(e2, abs(r$d2LogDet[j, k] - fd))
    }
    expect_lt(e2, 1e-3)
  })
})
