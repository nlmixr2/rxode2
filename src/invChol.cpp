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
  return arma::chol(R, 0.5 * (inv + inv.t()));
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
  return 0.5 * ret + 0.5 * ret.t();
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

static int rxInvCholFind(std::vector<int> &up, int i) {
  while (up[i] != i) i = up[i] = up[up[i]];
  return i;
}

// The rows/columns `d` of the blocks the drawn entries touch, where a block
// links entries by a nonzero estimate or a drawn entry; `pos` gives each
// row's place in `d` and `use` marks the drawn entries (-1 when not drawn)
static void rxInvCholBlocks(std::vector<arma::uword> &d, std::vector<int> &pos,
                            std::vector<int> &use, const arma::mat &mat,
                            const IntegerMatrix &el) {
  int dim = mat.n_rows;
  std::vector<int> up(dim);
  for (int i = 0; i < dim; ++i) up[i] = i;
  for (int j = 0; j < dim; ++j) {
    for (int i = j + 1; i < dim; ++i) {
      if (mat(i, j) != 0.0) up[rxInvCholFind(up, i)] = rxInvCholFind(up, j);
    }
  }
  std::vector<bool> drawn(dim, false);
  for (int r = 0; r < el.nrow(); ++r) {
    int a = el(r, 0) - 1, b = el(r, 1) - 1;
    if (a < 0 || b < 0 || a >= dim || b >= dim) use[r] = -1;
    if (use[r] < 0) continue;
    up[rxInvCholFind(up, a)] = rxInvCholFind(up, b);
  }
  for (int r = 0; r < el.nrow(); ++r) {
    if (use[r] >= 0) drawn[rxInvCholFind(up, el(r, 0) - 1)] = true;
  }
  pos.assign(dim, -1);
  for (int i = 0; i < dim; ++i) {
    if (drawn[rxInvCholFind(up, i)]) {
      pos[i] = d.size();
      d.push_back(i);
    }
  }
}

// Set up `ic` from the estimate `mat` and move the covariance `cov` of the
// thetaMat columns onto the parameter scale by the delta method.  With the
// other parameters held at their estimates, d(entries) = Jcc d(par), so
// the drawn entries keep their stated covariance to first order.
bool rxInvCholJointSetup(rxInvCholJoint &ic, arma::mat &cov,
                                const arma::mat &mat, const IntegerMatrix &el,
                                const std::vector<int> &col) {
  std::vector<arma::uword> d;
  std::vector<int> pos, use(col);
  rxInvCholBlocks(d, pos, use, mat, el);
  // nothing is drawn, so the estimate is kept as it is
  if (d.empty()) return true;
  ic.dims = arma::uvec(d);
  arma::mat sub = mat(ic.dims, ic.dims);
  arma::mat jac;
  if (!rxInvCholPar(ic.base, sub) || !rxInvCholJac(jac, sub)) return false;
  int n = d.size();
  ic.parIdx.assign(el.nrow(), -1);
  std::vector<arma::uword> pi, ci;
  for (int r = 0; r < el.nrow(); ++r) {
    if (use[r] < 0) continue;
    ic.parIdx[r] = rxInvCholIdx(pos[el(r, 0) - 1], pos[el(r, 1) - 1], n);
    pi.push_back(ic.parIdx[r]);
    ci.push_back(col[r]);
  }
  arma::uvec pv(pi), cv(ci);
  arma::mat jinv;
  if (!arma::inv(jinv, jac(pv, pv))) return false;
  arma::mat A = arma::eye(cov.n_rows, cov.n_cols);
  A(cv, cv) = jinv;
  cov = A * cov * A.t();
  cov = 0.5 * (cov + cov.t());
  ic.on = true;
  return true;
}

// One candidate: `cur` with the drawn parameters set to the estimate's
// plus the deviation
arma::mat rxInvCholJointCand(const rxInvCholJoint &ic,
                                    const arma::mat &cur,
                                    const NumericVector &dev,
                                    const std::vector<int> &col) {
  arma::vec par;
  if (!rxInvCholPar(par, cur(ic.dims, ic.dims))) par = ic.base;
  for (size_t r = 0; r < ic.parIdx.size(); ++r) {
    if (ic.parIdx[r] < 0) continue;
    par[ic.parIdx[r]] = ic.base[ic.parIdx[r]] + dev[col[r]];
  }
  arma::mat cand = cur;
  cand(ic.dims, ic.dims) = rxInvCholMat(par, ic.dims.n_elem);
  return cand;
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
