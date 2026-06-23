#' Variance-covariance (non-Cholesky) Omega parameterization derivatives
#'
#' rxode2's default random-effects parameterization for estimation is a Cholesky
#' decomposition (see [rxSymInvCholCreate()]), whose parameters are Cholesky
#' factors rather than interpretable variances/covariances.  To report standard
#' errors on the natural variance-covariance scale -- or to build an analytic
#' covariance matrix over the `Omega` elements -- a *non-Cholesky* path is needed
#' that differentiates with respect to the variance-covariance entries directly.
#'
#' This returns `Omega^{-1}`, `log|Omega|`, and their first (and optionally
#' second) derivatives with respect to each free lower-triangular
#' variance-covariance element `omega_{ab}`, using the closed forms
#'
#' \deqn{\partial \Omega^{-1}/\partial \omega_{ab} = -\Omega^{-1} E_{ab} \Omega^{-1}}
#' \deqn{\partial \log|\Omega|/\partial \omega_{ab} = \mathrm{tr}(\Omega^{-1} E_{ab})}
#'
#' where \eqn{E_{ab}} is the symmetric single-entry basis matrix.  These are the
#' pieces a FOCEI/FOCE observed-information covariance contracts against for the
#' `Omega` block.
#'
#' @param omega symmetric positive-definite random-effects covariance matrix.
#' @param order integer; `1` for first derivatives only, `2` (default) to also
#'   return the second derivatives needed for the covariance Hessian.
#' @return a list with `omegaInv`, `logDet`, the free-element index matrix
#'   `elements` (each row `c(a, b)`, `a >= b`), first derivatives
#'   `dOmegaInv` / `dLogDet`, and (when `order = 2`) second derivatives
#'   `d2OmegaInv` / `d2LogDet`.
#' @author Matthew Fidler
#' @export
rxOmegaVarCovDeriv <- function(omega, order = 2L) {
  omega <- as.matrix(omega)
  if (nrow(omega) != ncol(omega) || max(abs(omega - t(omega))) > 1e-8) {
    stop("'omega' must be a symmetric matrix", call. = FALSE)
  }
  n <- nrow(omega)
  Oi <- solve(omega)
  idx <- which(lower.tri(omega, diag = TRUE), arr.ind = TRUE)
  idx <- idx[order(idx[, "col"], idx[, "row"]), , drop = FALSE]
  np <- nrow(idx)
  Eb <- lapply(seq_len(np), function(k) {
    E <- matrix(0, n, n); a <- idx[k, 1]; b <- idx[k, 2]; E[a, b] <- 1; E[b, a] <- 1; E
  })
  OiE <- lapply(Eb, function(E) Oi %*% E)              # Omega^{-1} E_k
  ret <- list(
    omegaInv = Oi,
    logDet = as.numeric(determinant(omega, logarithm = TRUE)$modulus),
    elements = idx,
    dOmegaInv = lapply(OiE, function(M) -(M %*% Oi)),
    dLogDet = vapply(OiE, function(M) sum(diag(M)), numeric(1))
  )
  if (order >= 2L) {
    d2OmegaInv <- vector("list", np)
    d2LogDet <- matrix(0, np, np)
    for (j in seq_len(np)) {
      d2OmegaInv[[j]] <- vector("list", np)
      for (k in seq_len(np)) {
        # d2 Omega^{-1} = Oi (E_j Oi E_k + E_k Oi E_j) Oi
        d2OmegaInv[[j]][[k]] <- Oi %*% (Eb[[j]] %*% OiE[[k]] + Eb[[k]] %*% OiE[[j]]) %*% Oi
        # d2 log|Omega| = -tr(Oi E_j Oi E_k)
        d2LogDet[j, k] <- -sum(diag(OiE[[j]] %*% OiE[[k]]))
      }
    }
    ret$d2OmegaInv <- d2OmegaInv
    ret$d2LogDet <- d2LogDet
  }
  ret
}
