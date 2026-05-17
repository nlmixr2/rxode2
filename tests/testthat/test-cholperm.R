rxTest({
  # These functions are taken from TruncatedNormal for testing
  .rx <- loadNamespace("rxode2")

  lnNpr <-
    function(a, b) { ## computes ln(P(a<Z<b))
      ## where Z~N(0,1) very accurately for any 'a', 'b'
      p <- rep(0, length(a))
      ## case b>a>0
      I <- a > 0
      if (any(I)) {
        pa <- pnorm(a[I], lower.tail = FALSE, log.p = TRUE)
        pb <- pnorm(b[I], lower.tail = FALSE, log.p = TRUE)
        p[I] <- pa + log1p(-exp(pb - pa))
      }
      ## case a<b<0
      idx <- b < 0
      if (any(idx)) {
        pa <- pnorm(a[idx], log.p = TRUE)
        pb <- pnorm(b[idx], log.p = TRUE)
        p[idx] <- pb + log1p(-exp(pa - pb))
      }
      ## case a<0<b
      I <- !I & !idx
      if (any(I)) {
        pa <- pnorm(a[I])
        pb <- pnorm(b[I], lower.tail = FALSE)
        p[I] <- log1p(-pa - pb)
      }
      return(p)
    }

  cholperm <-
    function(Sig, l, u) {
      ##  Computes permuted lower Cholesky factor L for Sig
      ##  by permuting integration limit vectors l and u.
      ## Outputs perm, such that Sig(perm,perm)=L%*%t(L).
      ##
      ## Reference:
      ##  Gibson G. J., Glasbey C. A., Elston D. A. (1994),
      ##  "Monte Carlo evaluation of multivariate normal integrals and
      ##  sensitivity to variate ordering",
      ##  In: Advances in Numerical Methods and Applications, pages 120--126
      eps <- 10^-10 # round-off error tolerance
      d <- length(l)
      perm <- 1:d # keep track of permutation
      L <- matrix(0, d, d)
      z <- rep(0, d)
      for (j in 1:d) {
        pr <- rep(Inf, d) # compute marginal prob.
        I <- j:d # search remaining dimensions
        D <- diag(Sig)
        if (j > 2) {
          s <- D[I] - L[I, 1:(j - 1)]^2 %*% rep(1, j - 1)
        } else if (j == 2) {
          s <- D[I] - L[I, 1]^2
        } else {
          s <- D[I]
        }
        s[s < 0] <- eps
        s <- sqrt(s)
        if (j > 2) {
          cols <- L[I, 1:(j - 1)] %*% z[1:(j - 1)]
        } else if (j == 2) {
          cols <- L[I, 1] * z[1]
        } else {
          cols <- 0
        }
        tl <- (l[I] - cols) / s
        tu <- (u[I] - cols) / s
        pr[I] <- lnNpr(tl, tu)
        # find smallest marginal dimension
        k <- which.min(pr)
        # flip dimensions k-->j
        jk <- c(j, k)
        kj <- c(k, j)
        Sig[jk, ] <- Sig[kj, ]
        Sig[, jk] <- Sig[, kj] # update rows and cols of Sig
        L[jk, ] <- L[kj, ] # update only rows of L
        l[jk] <- l[kj]
        u[jk] <- u[kj] # update integration limits
        perm[jk] <- perm[kj] # keep track of permutation
        # construct L sequentially via Cholesky computation
        s <- Sig[j, j] - sum(L[j, 1:(j - 1)]^2)
        if (s < (-0.001)) {
          stop("Sigma is not positive semi-definite")
        }
        s[s < 0] <- eps
        L[j, j] <- sqrt(s)
        if (j < d) {
          if (j > 2) {
            L[(j + 1):d, j] <- (Sig[(j + 1):d, j] - L[(j + 1):d, 1:(j - 1)] %*% L[j, 1:(j - 1)]) / L[j, j]
          } else if (j == 2) {
            L[(j + 1):d, j] <- (Sig[(j + 1):d, j] - L[(j + 1):d, 1] * L[j, 1]) / L[j, j]
          } else if (j == 1) {
            L[(j + 1):d, j] <- Sig[(j + 1):d, j] / L[j, j]
          }
        }
        ## find mean value, z(j), of truncated normal:
        tl <- (l[j] - L[j, 1:j] %*% z[1:j]) / L[j, j]
        tu <- (u[j] - L[j, 1:j] %*% z[1:j]) / L[j, j]
        w <- lnNpr(tl, tu) # aids in computing expected value of trunc. normal
        z[j] <- (exp(-.5 * tl^2 - w) - exp(-.5 * tu^2 - w)) / sqrt(2 * pi)
      }
      return(list(L = L, l = l, u = u, perm = perm))
    }

  gradpsi <-
    function(y, L, l, u) { # implements grad_psi(x) to find optimal exponential twisting;
      # assume scaled 'L' with zero diagonal;
      d <- length(u)
      c <- rep(0, d)
      x <- c
      mu <- c
      x[1:(d - 1)] <- y[1:(d - 1)]
      mu[1:(d - 1)] <- y[d:(2 * d - 2)]
      # compute now ~l and ~u
      c[-1] <- L[-1, ] %*% x
      lt <- l - mu - c
      ut <- u - mu - c
      # compute gradients avoiding catastrophic cancellation
      w <- lnNpr(lt, ut)
      pl <- exp(-0.5 * lt^2 - w) / sqrt(2 * pi)
      pu <- exp(-0.5 * ut^2 - w) / sqrt(2 * pi)
      P <- pl - pu
      # output the gradient
      dfdx <- -mu[-d] + t(t(P) %*% L[, -d])
      dfdm <- mu - x + P
      grad <- c(dfdx, dfdm[-d])
      # here compute Jacobian matrix
      lt[is.infinite(lt)] <- 0
      ut[is.infinite(ut)] <- 0
      dP <- (-P^2) + lt * pl - ut * pu # dPdm
      DL <- rep(dP, 1, d) * L
      mx <- -diag(d) + DL
      xx <- t(L) %*% DL
      mx <- mx[1:(d - 1), 1:(d - 1)]
      xx <- xx[1:(d - 1), 1:(d - 1)]
      if (d > 2) {
        Jac <- rbind(cbind(xx, t(mx)), cbind(mx, diag(1 + dP[1:(d - 1)])))
      } else {
        Jac <- rbind(cbind(xx, t(mx)), cbind(mx, 1 + dP[1:(d - 1)]))
        dimnames(Jac) <- NULL
      }
      f <- list(grad = grad, Jac = Jac)
    }

  nleq <-
    function(l, u, L) {
      d <- length(l)
      x <- rep(0, 2 * d - 2) # initial point for Newton iteration
      err <- Inf
      iter <- 0
      while (err > 10^-10) {
        f <- gradpsi(x, L, l, u)
        Jac <- f$Jac
        grad <- f$grad
        del <- solve(Jac, -grad) # Newton correction
        x <- x + del
        err <- sum(grad^2)
        iter <- iter + 1
        if (iter > 100) {
          stop("Covariance matrix is ill-conditioned and method failed")
        }
      }
      return(x)
    }

  test_that("cholperm", {

    rxWithSeed(12, {
      d <- 5

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)

      r1 <- cholperm(mcov, -(1:5), 1:5)
      r2 <- .rx$rxCholperm(mcov, -(1:5), 1:5)

      expect_equal(r1$L, r2$L)
      expect_equal(r1$l, r2$l)
      expect_equal(r1$u, r2$u)
      expect_equal(r1$perm, r2$perm + 1)

      r1 <- cholperm(mcov, 1:5, 2 * (1:5))
      r2 <- .rx$rxCholperm(mcov, 1:5, 2 * 1:5)

      expect_equal(r1$L, r2$L)
      expect_equal(r1$l, r2$l)
      expect_equal(r1$u, r2$u)
      expect_equal(r1$perm, r2$perm + 1)

      r1 <- cholperm(mcov, -2 * (1:5), -(1:5))
      r2 <- .rx$rxCholperm(mcov, -2 * (1:5), -(1:5))

      expect_equal(r1$L, r2$L)
      expect_equal(r1$l, r2$l)
      expect_equal(r1$u, r2$u)
      expect_equal(r1$perm, r2$perm + 1)

      ## microbenchmark::microbenchmark(cholperm(mcov, -2 * (1:5), -(1:5)), rxCholperm(mcov, -2 * (1:5), -(1:5)))
      ## microbenchmark::microbenchmark(microbenchmark::cholperm(mcov, -2 * (1:5), -(1:5)), rxCholperm(mcov, -2 * (1:5), -(1:5)))
    })

  })

  test_that("gradpsi", {

    rxWithSeed(12, {
      d <- 5

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)
      r2 <- .rx$rxCholperm(mcov, -(1:5), 1:5)

      d <- length(r2$l)
      y <- seq(0, 2 * d - 2)
      l <- r2$l
      u <- r2$u
      L <- r2$L

      r1 <- gradpsi(y, L, l, u)
      r2 <- .rx$rxGradpsi(y, L, l, u)

      expect_equal(r1$Jac, r2$Jac)
      expect_equal(r1$grad, r2$grad)

      r1 <- gradpsi(rep(1, 2 * d - 2), L, l, u)
      r2 <- .rx$rxGradpsi(rep(1, 2 * d - 2), L, l, u)

      expect_equal(r1$Jac, r2$Jac)
      expect_equal(r1$grad, r2$grad)

      r1 <- gradpsi(rep(-1, 2 * d - 2), L, l, u)
      r2 <- .rx$rxGradpsi(rep(-1, 2 * d - 2), L, l, u)

      expect_equal(r1$Jac, r2$Jac)
      expect_equal(r1$grad, r2$grad)

      ## microbenchmark::microbenchmark(gradpsi(rep(-1,2*d-2), L, l, u), .rx$rxGradpsi(rep(-1,2*d-2), L, l, u));

      d <- 2

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)
      r2 <- .rx$rxCholperm(mcov, -(1:d), 1:d)

      d <- length(r2$l)
      y <- seq(0, 2 * d - 2)
      l <- r2$l
      u <- r2$u
      L <- r2$L


      r1 <- gradpsi(rep(-1, 2 * d - 2), L, l, u)
      r2 <- .rx$rxGradpsi(rep(-1, 2 * d - 2), L, l, u)

      expect_equal(r1$Jac, r2$Jac)
      expect_equal(r1$grad, r2$grad)
    })

  })


  test_that("nleq", {

    rxWithSeed(12, {
      d <- 5

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)
      r2 <- .rx$rxCholperm(mcov, -3 * (1:d), 2 * (1:d))

      expect_equal(.rx$rxNleq(r2$l, r2$u, r2$L), nleq(r2$l, r2$u, r2$L))
      ## microbenchmark::microbenchmark(rxNleq(r2$l, r2$u, r2$L), nleq(r2$l, r2$u, r2$L))

      d <- 2

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)
      r2 <- .rx$rxCholperm(mcov, -2 * (1:d), 3 * 1:d)

      expect_equal(.rx$rxNleq(r2$l, r2$u, r2$L), nleq(r2$l, r2$u, r2$L))
    })

  })

  test_that("rxMvnrnd", {

    rxWithSeed(12, {
      d <- 5

      mu <- 1:d

      ## Creating covariance matrix
      tmp <- matrix(rnorm(d^2), d, d)
      mcov <- tcrossprod(tmp, tmp)

      out <- .rx$rxCholperm(mcov, -2 * (1:5), 1:5)

      Lfull <- out$L
      l <- out$l
      u <- out$u
      D <- diag(Lfull)
      perm <- out$perm
      if (any(D < 10^-10)) {
        warning("Method may fail as covariance matrix is singular!")
      }
      L <- Lfull / D
      u <- u / D
      l <- l / D # rescale
      L <- L - diag(d) # remove diagonal
      # find optimal tilting parameter via non-linear equation solver
      xmu <- nleq(l, u, L) # nonlinear equation solver
      x <- xmu[1:(d - 1)]
      mu <- xmu[d:(2 * d - 2)] # assign saddlepoint x* and mu*

      fun <- function(n) {
        r1 <- .rx$rxMvnrnd(5, L, l, u, mu)
        expect_equal(length(r1$logpr), 5)
        expect_true(all(!duplicated(r1$logpr)))
        expect_equal(length(r1$Z[1, ]), 5)
        expect_true(all(!duplicated(r1$Z[1, ])))
      }
      fun(2)
      fun(5)
      fun(10)
    })

  })
})
