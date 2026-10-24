#define USE_FC_LEN_T
#define STRICT_R_HEADERS
// Truncated multivariate t by exponential tilting and acceptance-rejection:
// Botev and L'Ecuyer (2015), "Efficient estimation and simulation of the
// truncated multivariate student-t distribution", WSC 2015.
//
// The setup/draw core uses no R API besides the (pure) Rmath functions and
// reports errors as RXMVT_* status codes, so it may run on any thread.  A
// handle is read-only after setup, so threads may draw from it concurrently.
#include <algorithm>
#include <limits>
#include <new>
#include <RcppArmadillo.h>
#include "../inst/include/rxode2.h"
#include <threefry.h>
#include "rxomp.h"
#include "rxtrunc.h"
#include <boost/random/normal_distribution.hpp>
#include <boost/random/chi_squared_distribution.hpp>

#define _(String) (String)

using namespace Rcpp;

extern "C" {
  extern rx_solving_options op_global;
  uint32_t getRxSeed1(int ncores);
  void setSeedEng1(uint32_t seed);
  void setRxThreadId(int id);
}
extern std::vector<sitmo::threefry> _eng;

// Maximum proposals per accepted draw before RXMVT_ACCEPT_FAIL
#define RXMVT_MAX_TRIES 10000000

struct rxRmvtHandle {
  int d;
  double nu;
  bool zero;          // zero scale matrix: every draw is mu
  bool trunc;         // at least one finite bound
  double a, tol;      // trandn() tuning
  arma::vec mu, lower, upper;
  arma::mat Lfull;    // lower Cholesky factor (permuted when trunc)
  arma::mat L;        // Lfull scaled by its diagonal, diagonal removed
  arma::vec l, u;     // permuted standardized bounds scaled by diag(Lfull)
  arma::uvec perm;
  arma::vec tmu;      // tilting mean, tmu[d-1] = 0
  double eta;         // tilting mean of the chi variable
  double psistar;     // log upper bound of the likelihood ratio
  double lnPhiEta, cnst;
};

// log(f_nu(r)) - (-r^2/2 + (nu-1) log r): the chi density normalizing constant
// plus the log(sqrt(2 pi)) from the normal proposal of r.
static inline double rxRmvtConst(double nu) {
  return 0.5*log(2.0*M_PI) - (0.5*nu - 1.0)*M_LN2 - R::lgammafn(0.5*nu);
}

static inline double rxMills(double eta) {
  return exp(R::dnorm(eta, 0.0, 1.0, 1) - R::pnorm(eta, 0.0, 1.0, 1, 1));
}

// Gradient (eq. 7) and optionally the Jacobian of psi with respect to
// x = [z_1..z_{d-1}, r, mu_1..mu_{d-1}, eta].
static void rxRmvtGrad(const arma::vec& x, const arma::mat& L, const arma::vec& l,
                       const arma::vec& u, double nu, arma::vec& g, arma::mat* J) {
  int d = l.n_elem, dm = d - 1, n2 = 2*d;
  double sn = sqrt(nu);
  double r = x[dm], eta = x[n2 - 1];
  arma::vec z(d, arma::fill::zeros), m(d, arma::fill::zeros);
  for (int i = 0; i < dm; ++i) {
    z[i] = x[i];
    m[i] = x[d + i];
  }
  arma::vec c = L * z;
  arma::vec Psi(d), rho(d), Dk(d), Ek(d), Fk(d);
  for (int k = 0; k < d; ++k) {
    double lt = r*l[k]/sn - m[k] - c[k];
    double ut = r*u[k]/sn - m[k] - c[k];
    double w = lnNpr(lt, ut);
    bool fl = R_FINITE(lt), fu = R_FINITE(ut);
    double pl = fl ? exp(-0.5*lt*lt - w)*M_1_SQRT_2PI : 0.0;
    double pu = fu ? exp(-0.5*ut*ut - w)*M_1_SQRT_2PI : 0.0;
    double lpl = fl ? l[k]*pl : 0.0;
    double upu = fu ? u[k]*pu : 0.0;
    Psi[k] = pl - pu;
    rho[k] = (upu - lpl)/sn;
    if (J != NULL) {
      double ltpl = fl ? lt*pl : 0.0;
      double utpu = fu ? ut*pu : 0.0;
      Dk[k] = ltpl - utpu - Psi[k]*Psi[k];
      Ek[k] = ((fu ? ut*upu : 0.0) - (fl ? lt*lpl : 0.0))/sn - Psi[k]*rho[k];
      Fk[k] = ((fl ? l[k]*lt*lpl : 0.0) - (fu ? u[k]*ut*upu : 0.0))/nu - rho[k]*rho[k];
    }
  }
  double mills = rxMills(eta);
  g.set_size(n2);
  for (int i = 0; i < dm; ++i) {
    double acc = -m[i];
    for (int k = i + 1; k < d; ++k) acc += L.at(k, i)*Psi[k];
    g[i] = acc;
    g[d + i] = m[i] - z[i] + Psi[i];
  }
  g[dm] = (nu - 1.0)/r - eta + arma::accu(rho);
  g[n2 - 1] = eta - r + mills;
  if (J == NULL) return;
  arma::mat& H = *J;
  H.zeros(n2, n2);
  for (int i = 0; i < dm; ++i) {
    for (int j = 0; j < dm; ++j) {
      double acc = 0.0;
      for (int k = std::max(i, j) + 1; k < d; ++k) acc += L.at(k, i)*Dk[k]*L.at(k, j);
      H.at(i, j) = acc;
      // z_i, mu_j
      double zm = Dk[j]*L.at(j, i) - (i == j ? 1.0 : 0.0);
      H.at(i, d + j) = zm;
      H.at(d + j, i) = zm;
    }
    H.at(d + i, d + i) = 1.0 + Dk[i];
    double zr = 0.0;
    for (int k = i + 1; k < d; ++k) zr += L.at(k, i)*Ek[k];
    H.at(i, dm) = H.at(dm, i) = zr;
    H.at(d + i, dm) = H.at(dm, d + i) = Ek[i];
  }
  H.at(dm, dm) = -(nu - 1.0)/(r*r) + arma::accu(Fk);
  H.at(dm, n2 - 1) = H.at(n2 - 1, dm) = -1.0;
  H.at(n2 - 1, n2 - 1) = 1.0 - mills*(eta + mills);
}

// psi(r, z; eta, mu) including the chi density constant
static double rxRmvtPsi(const arma::vec& x, const arma::mat& L, const arma::vec& l,
                        const arma::vec& u, double nu) {
  int d = l.n_elem, dm = d - 1;
  double sn = sqrt(nu);
  double r = x[dm], eta = x[2*d - 1];
  arma::vec z(d, arma::fill::zeros), m(d, arma::fill::zeros);
  for (int i = 0; i < dm; ++i) {
    z[i] = x[i];
    m[i] = x[d + i];
  }
  arma::vec c = L * z;
  double p = 0.5*eta*eta - r*eta + (nu - 1.0)*log(r) +
    R::pnorm(eta, 0.0, 1.0, 1, 1) + rxRmvtConst(nu);
  for (int k = 0; k < d; ++k) {
    p += lnNpr(r*l[k]/sn - m[k] - c[k], r*u[k]/sn - m[k] - c[k]) +
      0.5*m[k]*m[k] - z[k]*m[k];
  }
  return p;
}

// Newton solve of the nonlinear system (7); returns an RXMVT_* status
static int rxRmvtNleq(arma::vec& x, const arma::mat& L, const arma::vec& l,
                      const arma::vec& u, double nu, double nlTol, int nlMaxiter) {
  int d = l.n_elem, dm = d - 1, n2 = 2*d;
  x.zeros(n2);
  x[dm] = sqrt(nu);
  x[n2 - 1] = sqrt(nu);
  arma::vec g, gn, del, xn;
  arma::mat J;
  rxRmvtGrad(x, L, l, u, nu, g, &J);
  double err = arma::accu(g % g);
  int iter = 0;
  while (!(err < nlTol)) {
    if (!std::isfinite(err) || ++iter > nlMaxiter) return RXMVT_NLEQ_FAIL;
    if (!arma::solve(del, J, -g, arma::solve_opts::no_approx)) {
      if (!arma::solve(del, J, -g)) return RXMVT_NLEQ_FAIL;
    }
    double t = 1.0;
    while (x[dm] + t*del[dm] <= 0.0 && t > 1e-12) t *= 0.5;
    double errn = R_PosInf;
    for (int ls = 0; ls < 40; ++ls) {
      xn = x + t*del;
      rxRmvtGrad(xn, L, l, u, nu, gn, NULL);
      errn = arma::accu(gn % gn);
      if (std::isfinite(errn) && errn < err) break;
      t *= 0.5;
    }
    if (!std::isfinite(errn) || xn[dm] <= 0.0) return RXMVT_NLEQ_FAIL;
    x = xn;
    rxRmvtGrad(x, L, l, u, nu, g, &J);
    err = arma::accu(g % g);
  }
  return RXMVT_OK;
}

static int rxRmvtSetupCore(rxRmvtHandle& h, int d, const double* mu,
                           const double* sigma, int isChol, const double* lower,
                           const double* upper, double nu, double a, double tol,
                           double nlTol, int nlMaxiter) {
  if (d < 1 || sigma == NULL || ISNAN(nu) || !(nu > 0.0) || nlMaxiter < 1 ||
      ISNAN(nlTol)) {
    return RXMVT_BAD_ARGS;
  }
  h.d = d;
  h.nu = nu;
  h.a = a;
  h.tol = tol;
  h.mu.set_size(d);
  h.lower.set_size(d);
  h.upper.set_size(d);
  h.trunc = false;
  for (int j = 0; j < d; ++j) {
    h.mu[j] = (mu == NULL) ? 0.0 : mu[j];
    h.lower[j] = (lower == NULL) ? R_NegInf : lower[j];
    h.upper[j] = (upper == NULL) ? R_PosInf : upper[j];
    if (!R_FINITE(h.mu[j])) return RXMVT_BAD_ARGS;
    if (ISNAN(h.lower[j]) || ISNAN(h.upper[j]) || !(h.upper[j] > h.lower[j])) {
      return RXMVT_BAD_BOUNDS;
    }
    if (R_FINITE(h.lower[j]) || R_FINITE(h.upper[j])) h.trunc = true;
  }
  arma::mat S(d, d);
  for (int i = 0; i < d*d; ++i) {
    if (!R_FINITE(sigma[i])) return RXMVT_BAD_ARGS;
    S[i] = sigma[i];
  }
  if (isChol) {
    arma::mat U = arma::trimatu(S);
    S = U.t() * U;
  }
  h.zero = S.is_zero();
  if (h.zero) {
    for (int j = 0; j < d; ++j) {
      if (h.mu[j] < h.lower[j] || h.mu[j] > h.upper[j]) return RXMVT_BAD_BOUNDS;
    }
    return RXMVT_OK;
  }
  if (!h.trunc) {
    if (!arma::chol(h.Lfull, S, "lower")) return RXMVT_NOT_PD;
    return RXMVT_OK;
  }
  if (!R_FINITE(nu) || nu < 1.0) return RXMVT_BAD_DF;
  int ret = RXMVT_OK;
  int status = 0;
  rx_cholperms cp = cholpermCore(S, h.lower - h.mu, h.upper - h.mu, 1e-10, status);
  if (status != 0) return RXMVT_NOT_PD;
  h.Lfull = cp.L;
  h.perm = cp.perm;
  arma::vec D = h.Lfull.diag();
  if (any(D < 1e-10)) ret = RXMVT_WARN_SINGULAR;
  h.L = h.Lfull.each_col() / D;
  h.L.diag().zeros();
  h.l = cp.l / D;
  h.u = cp.u / D;
  arma::vec x;
  // psistar must be the supremum of psi, so a loose tolerance is tightened
  int st = rxRmvtNleq(x, h.L, h.l, h.u, nu, std::min(nlTol, 1e-6), nlMaxiter);
  if (st != RXMVT_OK) return st;
  int dm = d - 1;
  double r = x[dm], sn = sqrt(nu);
  h.eta = x[2*d - 1];
  h.tmu.zeros(d);
  arma::vec z(d, arma::fill::zeros);
  for (int i = 0; i < dm; ++i) {
    z[i] = x[i];
    h.tmu[i] = x[d + i];
  }
  // The saddle point solves (6) only when it lies in the region R
  arma::vec c = h.L * z;
  for (int k = 0; k < dm; ++k) {
    if (z[k] < r*h.l[k]/sn - c[k] || z[k] > r*h.u[k]/sn - c[k]) {
      ret = RXMVT_WARN_INFEASIBLE;
    }
  }
  h.psistar = rxRmvtPsi(x, h.L, h.l, h.u, nu);
  h.lnPhiEta = R::pnorm(h.eta, 0.0, 1.0, 1, 1);
  h.cnst = rxRmvtConst(nu);
  if (!std::isfinite(h.psistar)) return RXMVT_NLEQ_FAIL;
  return ret;
}

// Draws n vectors into the column-major n x d matrix out; returns
// RXMVT_WARN_INFEASIBLE when a proposal exceeded the psistar envelope.
static int rxRmvtDrawCore(const rxRmvtHandle& h, double* out, int n,
                          sitmo::threefry& eng) {
  int d = h.d;
  if (n < 0 || (n > 0 && out == NULL)) return RXMVT_BAD_ARGS;
  if (h.zero) {
    for (int j = 0; j < d; ++j) {
      for (int i = 0; i < n; ++i) out[i + n*j] = h.mu[j];
    }
    return RXMVT_OK;
  }
  std::vector<double> Z(d), Y(d);
  boost::random::normal_distribution<double> snorm(0.0, 1.0);
  if (!h.trunc) {
    // mu + Lfull z sqrt(nu/W), W ~ chi^2_nu
    boost::random::chi_squared_distribution<double> chisq(R_FINITE(h.nu) ? h.nu : 1.0);
    for (int i = 0; i < n; ++i) {
      for (int k = 0; k < d; ++k) Z[k] = snorm(eng);
      double s = R_FINITE(h.nu) ? sqrt(h.nu/chisq(eng)) : 1.0;
      for (int j = 0; j < d; ++j) {
        double acc = 0.0;
        for (int k = 0; k <= j; ++k) acc += h.Lfull.at(j, k)*Z[k];
        out[i + n*j] = h.mu[j] + s*acc;
      }
    }
    return RXMVT_OK;
  }
  // Algorithm 2
  std::uniform_real_distribution<double> unif(0.0, 1.0);
  double sn = sqrt(h.nu);
  int ret = RXMVT_OK;
  for (int i = 0; i < n; ++i) {
    int tries = 0;
    while (true) {
      if (++tries > RXMVT_MAX_TRIES) return RXMVT_ACCEPT_FAIL;
      double R = h.eta + trandn(-h.eta, R_PosInf, eng, h.a, h.tol);
      if (!(R > 0.0) || !R_FINITE(R)) continue;
      double lr = 0.5*h.eta*h.eta - R*h.eta + (h.nu - 1.0)*log(R) + h.lnPhiEta + h.cnst;
      for (int k = 0; k < d; ++k) {
        double c = 0.0;
        for (int j = 0; j < k; ++j) c += h.L.at(k, j)*Z[j];
        double lt = R*h.l[k]/sn - c - h.tmu[k];
        double ut = R*h.u[k]/sn - c - h.tmu[k];
        Z[k] = h.tmu[k] + trandn(lt, ut, eng, h.a, h.tol);
        lr += lnNpr(lt, ut) + 0.5*h.tmu[k]*h.tmu[k] - h.tmu[k]*Z[k];
      }
      if (lr > h.psistar + 1e-8) ret = RXMVT_WARN_INFEASIBLE;
      if (-log(unif(eng)) <= h.psistar - lr) continue;
      rxTruncBackTransform(h.Lfull, Z.data(), h.perm, sn/R, Y.data());
      bool ok = true;
      for (int j = 0; j < d; ++j) {
        Y[j] += h.mu[j];
        if (!(Y[j] >= h.lower[j] && Y[j] <= h.upper[j])) {
          ok = false;
          break;
        }
      }
      if (!ok) continue;
      for (int j = 0; j < d; ++j) out[i + n*j] = Y[j];
      break;
    }
  }
  return ret;
}

// Thread-safe C interface (exported through the rxode2 function-pointer table)
extern "C" int rxRmvtSetup(void** handle, int d, const double* mu,
                           const double* sigma, int isChol, const double* lower,
                           const double* upper, double nu, double a, double tol,
                           double nlTol, int nlMaxiter) {
  if (handle == NULL) return RXMVT_BAD_ARGS;
  *handle = NULL;
  rxRmvtHandle* h = new (std::nothrow) rxRmvtHandle();
  if (h == NULL) return RXMVT_ALLOC_FAIL;
  int ret;
  try {
    ret = rxRmvtSetupCore(*h, d, mu, sigma, isChol, lower, upper, nu, a, tol,
                          nlTol, nlMaxiter);
  } catch (...) {
    ret = RXMVT_ALLOC_FAIL;
  }
  if (ret < 0) {
    delete h;
    return ret;
  }
  *handle = (void*)h;
  return ret;
}

extern "C" int rxRmvtDraw(void* handle, double* out, int n) {
  if (handle == NULL) return RXMVT_BAD_ARGS;
  int thread = rx_get_thread(op_global.cores);
  if (thread < 0 || thread >= (int)_eng.size()) return RXMVT_NO_ENGINE;
  try {
    return rxRmvtDrawCore(*((rxRmvtHandle*)handle), out, n, _eng[thread]);
  } catch (...) {
    return RXMVT_ALLOC_FAIL;
  }
}

extern "C" void rxRmvtFree(void* handle) {
  if (handle != NULL) delete (rxRmvtHandle*)handle;
}

static void rxRmvtStop(int status) {
  switch (status) {
  case RXMVT_BAD_ARGS:
    Rcpp::stop(_("invalid arguments for the multivariate t"));
  case RXMVT_BAD_BOUNDS:
    Rcpp::stop(_("'upper' must be greater than 'lower'"));
  case RXMVT_NOT_PD:
    Rcpp::stop(_("'sigma' is not positive definite"));
  case RXMVT_BAD_DF:
    Rcpp::stop(_("truncated multivariate t requires a finite 'df' >= 1"));
  case RXMVT_NLEQ_FAIL:
    Rcpp::stop(_("covariance matrix is ill-conditioned and method failed (truncated mvt)"));
  case RXMVT_ACCEPT_FAIL:
    Rcpp::stop(_("could not sample from truncated multivariate t"));
  case RXMVT_NO_ENGINE:
    Rcpp::stop(_("threefry engine not seeded"));
  default:
    Rcpp::stop(_("could not allocate the multivariate t sampler"));
  }
}

static void rxRmvtWarn(int status) {
  if (status == RXMVT_WARN_SINGULAR) {
    Rf_warningcall(R_NilValue, "%s", _("truncated multivariate t may fail as covariance matrix is singular"));
  } else if (status == RXMVT_WARN_INFEASIBLE) {
    Rf_warningcall(R_NilValue, "%s", _("truncated multivariate t tilting envelope may be invalid; draws may not be exact"));
  }
}

void rxRmvt0(arma::mat& A, const arma::rowvec& mu, const arma::mat& sigma,
             const arma::vec& lower, const arma::vec& upper, double df,
             bool isChol, double a, double tol, double nlTol, int nlMaxiter) {
  int n = A.n_rows, d = mu.n_elem;
  if (n < 1) Rcpp::stop(_("n should be a positive integer"));
  if (d != (int)sigma.n_rows || d != (int)sigma.n_cols || d != (int)A.n_cols) {
    Rcpp::stop("length(mu) != ncol(sigma)");
  }
  rxRmvtHandle h;
  int st = rxRmvtSetupCore(h, d, mu.memptr(), sigma.memptr(), isChol ? 1 : 0,
                           lower.memptr(), upper.memptr(), df, a, tol, nlTol,
                           nlMaxiter);
  if (st < 0) rxRmvtStop(st);
  rxRmvtWarn(st);
  // One serial stream, so the draws do not depend on ncores (#1376)
  sitmo::threefry eng;
  eng.seed(getRxSeed1(1));
  st = rxRmvtDrawCore(h, A.memptr(), n, eng);
  if (st < 0) rxRmvtStop(st);
  rxRmvtWarn(st);
}

// Exported for testing: the tilting solution
//[[Rcpp::export]]
List rxRmvtTilt_(arma::mat sigma, arma::vec lower, arma::vec upper, double df) {
  int d = sigma.n_rows;
  rxRmvtHandle h;
  arma::vec mu(d, arma::fill::zeros);
  int st = rxRmvtSetupCore(h, d, mu.memptr(), sigma.memptr(), 0, lower.memptr(),
                           upper.memptr(), df, 0.4, 2.05, 1e-10, 100);
  if (st < 0) rxRmvtStop(st);
  if (!h.trunc || h.zero) {
    return List::create(_["status"] = st);
  }
  return List::create(_["psistar"] = h.psistar, _["eta"] = h.eta,
                      _["mu"] = wrap(h.tmu), _["status"] = st);
}

// Exported for testing: sets up one handle, then draws n vectors per id in an
// OpenMP team through the C interface, seeding each id with setSeedEng1(seed + id).
//[[Rcpp::export]]
NumericVector rxRmvtTeamDraw_(arma::rowvec mu, arma::mat sigma, double df,
                              arma::vec lower, arma::vec upper, int n, int nid,
                              int nthreads, double seed) {
  int d = mu.n_elem;
  void* h = NULL;
  int st = rxRmvtSetup(&h, d, mu.memptr(), sigma.memptr(), 0, lower.memptr(),
                       upper.memptr(), df, 0.4, 2.05, 1e-10, 100);
  if (st < 0) rxRmvtStop(st);
  NumericVector ret(n*nid*d);
  double* retD = ret.begin();
  uint32_t seedBase = (uint32_t)seed;
  int oldCores = op_global.cores;
  if ((int)_eng.size() < nthreads) {
    rxRmvtFree(h);
    Rcpp::stop("rxSeedEng(nthreads) first");
  }
  op_global.cores = nthreads;
  int bad = 0;
#ifdef _OPENMP
#pragma omp parallel num_threads(nthreads)
  {
    setRxThreadId(omp_get_thread_num());
#pragma omp for schedule(static)
#endif
    for (int id = 0; id < nid; ++id) {
      setSeedEng1(seedBase + (uint32_t)id);
      if (rxRmvtDraw(h, retD + (ptrdiff_t)id*n*d, n) < 0) {
#ifdef _OPENMP
#pragma omp atomic write
#endif
        bad = 1;
      }
    }
#ifdef _OPENMP
    setRxThreadId(-1);
  }
#endif
  op_global.cores = oldCores;
  rxRmvtFree(h);
  if (bad) Rcpp::stop(_("could not sample from truncated multivariate t"));
  ret.attr("dim") = IntegerVector::create(n, d, nid);
  return ret;
}
