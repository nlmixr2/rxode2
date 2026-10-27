#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define ARMA_WARN_LEVEL 1
#define STRICT_R_HEADER
#include <RcppArmadillo.h>
#include "invChol.h"
#define _(String) (String)

using namespace Rcpp;

int rxInvCholIdx(int i, int j, int dim) {
  if (i < j) std::swap(i, j);
  return j * dim - j * (j - 1) / 2 + i - j;
}

// R = chol(inv(omega)), upper triangular
static bool rxInvCholR(arma::mat &R, const arma::mat &omega) {
  arma::mat inv;
  if (!arma::inv_sympd(inv, 0.5 * (omega + omega.t()))) return false;
  return arma::chol(R, inv);
}

bool rxInvCholPar(arma::vec &par, const arma::mat &omega) {
  arma::mat R;
  if (!rxInvCholR(R, omega)) return false;
  int dim = omega.n_rows;
  par.set_size(dim * (dim + 1) / 2);
  for (int j = 0; j < dim; ++j) {
    for (int i = j; i < dim; ++i) {
      par[rxInvCholIdx(i, j, dim)] = (i == j) ? std::log(R(i, i)) : R(j, i);
    }
  }
  return true;
}

static arma::mat rxInvCholParR(const arma::vec &par, int dim) {
  arma::mat R(dim, dim, arma::fill::zeros);
  for (int j = 0; j < dim; ++j) {
    for (int i = j; i < dim; ++i) {
      double v = par[rxInvCholIdx(i, j, dim)];
      R(j, i) = (i == j) ? std::exp(v) : v;
    }
  }
  return R;
}

arma::mat rxInvCholMat(const arma::vec &par, int dim) {
  arma::mat Ri;
  // a diagonal that under- or overflows has no inverse; NaN marks the
  // matrix unusable so the draw is redrawn
  if (!arma::inv(Ri, arma::trimatu(rxInvCholParR(par, dim)))) {
    Ri.set_size(dim, dim);
    Ri.fill(arma::datum::nan);
  }
  arma::mat ret = Ri * Ri.t();
  return 0.5 * (ret + ret.t());
}

bool rxInvCholJac(arma::mat &jac, const arma::mat &omega) {
  arma::mat R;
  if (!rxInvCholR(R, omega)) return false;
  int dim = omega.n_rows;
  int n = dim * (dim + 1) / 2;
  arma::mat om = 0.5 * (omega + omega.t());
  jac.zeros(n, n);
  for (int b = 0; b < dim; ++b) {
    for (int a = b; a < dim; ++a) {
      // d inv(omega) = dR' R + R' dR, so d omega = -omega (dR' R + R' dR) omega
      arma::mat dR(dim, dim, arma::fill::zeros);
      dR(b, a) = (a == b) ? R(a, a) : 1.0;
      arma::mat dOm = -om * (dR.t() * R + R.t() * dR) * om;
      int k = rxInvCholIdx(a, b, dim);
      for (int j = 0; j < dim; ++j) {
        for (int i = j; i < dim; ++i) {
          jac(rxInvCholIdx(i, j, dim), k) = dOm(i, j);
        }
      }
    }
  }
  return true;
}

// Test hook: type 0 gives the parameters of the matrix `x`, 1 the matrix
// of the parameter vector `x`, 2 the Jacobian at the matrix `x`
//[[Rcpp::export]]
SEXP rxInvChol_(SEXP x, int type) {
  if (type == 1) {
    int n = Rf_length(x);
    int dim = (int)((std::sqrt(8.0 * n + 1.0) - 1.0) / 2.0 + 0.5);
    if (dim * (dim + 1) / 2 != n) {
      stop(_("'x' is not the lower triangle of a square matrix"));
    }
    return wrap(rxInvCholMat(as<arma::vec>(x), dim));
  }
  arma::mat omega = as<arma::mat>(x);
  if (type == 0) {
    arma::vec par;
    if (!rxInvCholPar(par, omega)) stop(_("'x' is not positive definite"));
    return wrap(par);
  }
  arma::mat jac;
  if (!rxInvCholJac(jac, omega)) stop(_("'x' is not positive definite"));
  return wrap(jac);
}
