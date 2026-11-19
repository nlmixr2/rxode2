// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define ARMA_WARN_LEVEL 1
#define STRICT_R_HEADERS
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

//' Variance-covariance (non-Cholesky) Omega parameterization derivatives (C++)
//'
//' C++/RcppArmadillo implementation of the non-Cholesky Omega derivatives used
//' to build a FOCEI/FOCE observed-information covariance over the natural
//' variance-covariance scale.  Returns \eqn{\Omega^{-1}}, \eqn{\log|\Omega|},
//' and their first (and optionally second) derivatives with respect to each free
//' lower-triangular variance-covariance element \eqn{\omega_{ab}}, using
//'
//' \deqn{\partial \Omega^{-1}/\partial \omega_{ab} = -\Omega^{-1} E_{ab} \Omega^{-1}}
//' \deqn{\partial \log|\Omega|/\partial \omega_{ab} = \mathrm{tr}(\Omega^{-1} E_{ab})}
//'
//' where \eqn{E_{ab}} is the symmetric single-entry basis matrix.
//'
//' @param omega symmetric positive-definite random-effects covariance matrix.
//' @param order integer; `1` for first derivatives only, `2` (default) to also
//'   return the second derivatives needed for the covariance Hessian.
//' @return a list with `omegaInv`, `logDet`, the free-element index matrix
//'   `elements` (each row `c(a, b)`, `a >= b`), first derivatives
//'   `dOmegaInv` / `dLogDet`, and (when `order = 2`) second derivatives
//'   `d2OmegaInv` / `d2LogDet`.
//' @author Hidde van de Beek
//' @keywords internal
//' @export
//[[Rcpp::export]]
List rxOmegaVarCovDeriv_(arma::mat omega, int order = 2) {
  unsigned int n = omega.n_rows;
  if (omega.n_cols != n) {
    Rcpp::stop("'omega' must be a symmetric matrix");
  }
  if (n > 0 && (abs(omega - omega.t())).max() > 1e-8) {
    Rcpp::stop("'omega' must be a symmetric matrix");
  }
  arma::mat Oi = arma::inv_sympd(omega);

  // Free lower-triangular elements ordered by column then row -- matches the R
  // `which(lower.tri(., diag=TRUE), arr.ind=TRUE)` ordered by (col, row): for
  // column b, rows a = b..n (each pair a >= b, 1-based).
  std::vector<unsigned int> ai, bi;
  for (unsigned int b = 0; b < n; ++b) {
    for (unsigned int a = b; a < n; ++a) {
      ai.push_back(a); bi.push_back(b);
    }
  }
  unsigned int np = ai.size();

  // Symmetric single-entry basis matrices E_k and Oi %*% E_k.
  std::vector<arma::mat> Eb(np), OiE(np);
  for (unsigned int k = 0; k < np; ++k) {
    arma::mat E(n, n, arma::fill::zeros);
    E(ai[k], bi[k]) = 1.0;
    E(bi[k], ai[k]) = 1.0;          // a==b just rewrites the same cell to 1
    Eb[k] = E;
    OiE[k] = Oi * E;
  }

  IntegerMatrix elements(np, 2);
  List dOmegaInv(np);
  NumericVector dLogDet(np);
  for (unsigned int k = 0; k < np; ++k) {
    elements(k, 0) = (int)ai[k] + 1;  // a (row), 1-based
    elements(k, 1) = (int)bi[k] + 1;  // b (col), 1-based
    dOmegaInv[k] = wrap(-(OiE[k] * Oi));
    dLogDet[k] = arma::trace(OiE[k]);
  }
  colnames(elements) = CharacterVector::create("row", "col");

  List ret = List::create(
    _["omegaInv"]  = wrap(Oi),
    _["logDet"]    = arma::log_det_sympd(omega),
    _["elements"]  = elements,
    _["dOmegaInv"] = dOmegaInv,
    _["dLogDet"]   = dLogDet);

  if (order >= 2) {
    List d2OmegaInv(np);
    arma::mat d2LogDet(np, np, arma::fill::zeros);
    for (unsigned int j = 0; j < np; ++j) {
      List d2j(np);
      for (unsigned int k = 0; k < np; ++k) {
        // d2 Omega^{-1} = Oi (E_j Oi E_k + E_k Oi E_j) Oi
        d2j[k] = wrap(Oi * (Eb[j] * OiE[k] + Eb[k] * OiE[j]) * Oi);
        // d2 log|Omega| = -tr(Oi E_j Oi E_k)
        d2LogDet(j, k) = -arma::trace(OiE[j] * OiE[k]);
      }
      d2OmegaInv[j] = d2j;
    }
    ret["d2OmegaInv"] = d2OmegaInv;
    ret["d2LogDet"] = wrap(d2LogDet);
  }
  return ret;
}
