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
#' @author Hidde van de Beek
#' @export
rxOmegaVarCovDeriv <- function(omega, order = 2L) {
  omega <- as.matrix(omega)
  storage.mode(omega) <- "double"
  # Heavy lifting (the E-basis matrix calculus) is done in C++/RcppArmadillo so
  # this can be called from the analytic-covariance C path; see src/omegaVarCov.cpp.
  rxOmegaVarCovDeriv_(omega, as.integer(order))
}
