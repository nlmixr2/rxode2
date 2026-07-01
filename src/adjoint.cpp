// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
// [[Rcpp::interfaces(r,cpp)]]
#include <Rcpp.h>
using namespace Rcpp;

// Backward adjoint sweep for the functional-gradient objective, in C++.
//
// Given the forward trajectory sampled on a fine increasing grid `tg` and, at
// every grid point, the full Jacobian J (df_i/dy_j) and forcing df_i/dtheta_p,
// integrate the costate and quadrature backward in one pass:
//
//   d lambda / dt = -J(t)^T lambda            (costate)
//   d mu_p    / dt = -lambda^T (df/dtheta_p)   (running gradient quadrature)
//
// lambda picks up the per-observation covector `cover` as a jump at each
// observation grid index `obsK` (processed in decreasing time).  mu at the
// start of the grid is the trajectory part of dG/dtheta.  Integration uses RK4
// on the fine grid (J / df-dp linearly interpolated at step midpoints); on the
// dissipative PK grids this matches an adaptive backward solve to ~1e-6 while
// avoiding per-segment solver calls and any symbolic work.
//
// Layout: J(k, i*ns + j) = df_i/dy_j at tg[k];  dP(k, i*np + p) = df_i/dtheta_p.
// cover has one row per observation (length ns), obsK the 0-based grid index.
//
//[[Rcpp::export]]
NumericVector rxAdjointSweepC(NumericVector tg, NumericMatrix J, NumericMatrix dP,
                             NumericMatrix cover, IntegerVector obsK, int ns, int np) {
  int nt = tg.size();
  std::vector<double> lam(ns, 0.0), mu(np, 0.0);
  // map grid index -> observation row (or -1)
  std::vector<int> obsRow(nt, -1);
  for (int o = 0; o < obsK.size(); ++o) obsRow[obsK[o]] = o;

  // Jt^T * v  ->  out[j] = sum_i J[i][j] * v[i]   (transpose-Jacobian x vector)
  auto JTv = [&](int k, const std::vector<double>& v, std::vector<double>& out) {
    for (int j = 0; j < ns; ++j) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += J(k, i * ns + j) * v[i];
      out[j] = s;
    }
  };
  // dP^T * v  ->  out[p] = sum_i dP[i][p] * v[i]
  auto dPTv = [&](int k, const std::vector<double>& v, std::vector<double>& out) {
    for (int p = 0; p < np; ++p) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += dP(k, i * np + p) * v[i];
      out[p] = s;
    }
  };

  std::vector<double> l1(ns), l2(ns), l3(ns), l4(ns), tmp(ns);
  std::vector<double> m1(np), m2(np), m3(np), m4(np);
  // midpoint (interpolated) Jacobian / forcing buffers as flat access via avg
  auto stepJT = [&](double a, double b, int ka, int kb, // rows for endpoints
                    const std::vector<double>& v, std::vector<double>& out) {
    // out = ((J[ka]+J[kb])/2)^T v  (midpoint interpolation)
    for (int j = 0; j < ns; ++j) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += 0.5 * (J(ka, i * ns + j) + J(kb, i * ns + j)) * v[i];
      out[j] = s;
    }
    (void)a; (void)b;
  };
  auto stepPT = [&](int ka, int kb, const std::vector<double>& v, std::vector<double>& out) {
    for (int p = 0; p < np; ++p) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += 0.5 * (dP(ka, i * np + p) + dP(kb, i * np + p)) * v[i];
      out[p] = s;
    }
  };

  for (int k = nt - 1; k >= 0; --k) {
    // observation covector jump (lambda(tau-) = lambda(tau+) + cover)
    if (obsRow[k] >= 0) {
      int o = obsRow[k];
      for (int i = 0; i < ns; ++i) lam[i] += cover(o, i);
    }
    if (k == 0) break;
    double h = tg[k - 1] - tg[k];   // negative (stepping backward)
    // RK4 for d/dt (lambda, mu) = (-J^T lambda, -dP^T lambda)
    JTv(k, lam, l1);            dPTv(k, lam, m1);
    for (int i = 0; i < ns; ++i) { l1[i] = -l1[i]; }
    for (int p = 0; p < np; ++p) { m1[p] = -m1[p]; }

    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + 0.5 * h * l1[i];
    stepJT(0, 0, k, k - 1, tmp, l2);  stepPT(k, k - 1, tmp, m2);
    for (int i = 0; i < ns; ++i) l2[i] = -l2[i];
    for (int p = 0; p < np; ++p) m2[p] = -m2[p];

    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + 0.5 * h * l2[i];
    stepJT(0, 0, k, k - 1, tmp, l3);  stepPT(k, k - 1, tmp, m3);
    for (int i = 0; i < ns; ++i) l3[i] = -l3[i];
    for (int p = 0; p < np; ++p) m3[p] = -m3[p];

    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + h * l3[i];
    JTv(k - 1, tmp, l4);       dPTv(k - 1, tmp, m4);
    for (int i = 0; i < ns; ++i) l4[i] = -l4[i];
    for (int p = 0; p < np; ++p) m4[p] = -m4[p];

    for (int i = 0; i < ns; ++i)
      lam[i] += (h / 6.0) * (l1[i] + 2 * l2[i] + 2 * l3[i] + l4[i]);
    for (int p = 0; p < np; ++p)
      mu[p]  += (h / 6.0) * (m1[p] + 2 * m2[p] + 2 * m3[p] + m4[p]);
  }
  NumericVector out(np);
  for (int p = 0; p < np; ++p) out[p] = mu[p];
  return out;
}
