// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <vector>

// Backward adjoint sweep for the functional-gradient objective.
//
// Given the forward trajectory sampled on a fine increasing grid `tg` and, at
// every grid point, the full Jacobian J (df_i/dy_j) and forcing df_i/dtheta_p,
// integrate the costate and quadrature backward in one pass:
//
//   d lambda / dt = -J(t)^T lambda            (costate)
//   d mu_p    / dt = -lambda^T (df/dtheta_p)   (running gradient quadrature)
//
// lambda picks up the per-observation covector `cover` as a jump at each
// observation grid index `obsK` (processed in decreasing time).  `out` receives
// the trajectory part of dG/dtheta.  Integration uses RK4 on the fine grid
// (J / df-dp linearly interpolated at step midpoints); on the dissipative PK
// grids this matches an adaptive backward solve to ~1e-6 while avoiding
// per-segment solver calls and any symbolic work.
//
// Data layout (column-major, as passed straight from R matrices):
//   J[k + (i*ns + j)*nt] = df_i/dy_j        at tg[k]   (nt x ns*ns)
//   dP[k + (i*np + p)*nt] = df_i/dtheta_p    at tg[k]   (nt x ns*np)
//   cover[o + i*nobs]     = covector row o, state i     (nobs x ns)
//   obsK[o]               = 0-based grid index of observation o
//
// Exposed with C linkage and registered via R_RegisterCCallable so downstream
// packages can obtain it with R_GetCCallable("rxode2", "rxode2AdjointSweep")
// -- the CRAN-preferred cross-package interface (no ABI coupling).
extern "C" void rxode2AdjointSweep(double *tg, double *J, double *dP,
                                   double *cover, int *obsK, int ns, int np,
                                   int nt, int nobs, double *out) {
  std::vector<double> lam(ns, 0.0), mu(np, 0.0);
  std::vector<int> obsRow(nt, -1);
  for (int o = 0; o < nobs; ++o) obsRow[obsK[o]] = o;

  // (J[k])^T v  ->  o[j] = sum_i J[k, i*ns+j] v[i]
  auto JTv = [&](int k, const std::vector<double> &v, std::vector<double> &o) {
    for (int j = 0; j < ns; ++j) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += J[k + (i * ns + j) * nt] * v[i];
      o[j] = -s;
    }
  };
  auto dPTv = [&](int k, const std::vector<double> &v, std::vector<double> &o) {
    for (int p = 0; p < np; ++p) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += dP[k + (i * np + p) * nt] * v[i];
      o[p] = -s;
    }
  };
  // midpoint (0.5*(J[ka]+J[kb]))^T v
  auto JTvMid = [&](int ka, int kb, const std::vector<double> &v, std::vector<double> &o) {
    for (int j = 0; j < ns; ++j) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i)
        s += 0.5 * (J[ka + (i * ns + j) * nt] + J[kb + (i * ns + j) * nt]) * v[i];
      o[j] = -s;
    }
  };
  auto dPTvMid = [&](int ka, int kb, const std::vector<double> &v, std::vector<double> &o) {
    for (int p = 0; p < np; ++p) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i)
        s += 0.5 * (dP[ka + (i * np + p) * nt] + dP[kb + (i * np + p) * nt]) * v[i];
      o[p] = -s;
    }
  };

  std::vector<double> l1(ns), l2(ns), l3(ns), l4(ns), tmp(ns);
  std::vector<double> m1(np), m2(np), m3(np), m4(np);
  for (int k = nt - 1; k >= 0; --k) {
    if (obsRow[k] >= 0) {                 // observation covector jump
      int o = obsRow[k];
      for (int i = 0; i < ns; ++i) lam[i] += cover[o + i * nobs];
    }
    if (k == 0) break;
    double h = tg[k - 1] - tg[k];         // negative (stepping backward)
    JTv(k, lam, l1);       dPTv(k, lam, m1);
    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + 0.5 * h * l1[i];
    JTvMid(k, k - 1, tmp, l2);  dPTvMid(k, k - 1, tmp, m2);
    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + 0.5 * h * l2[i];
    JTvMid(k, k - 1, tmp, l3);  dPTvMid(k, k - 1, tmp, m3);
    for (int i = 0; i < ns; ++i) tmp[i] = lam[i] + h * l3[i];
    JTv(k - 1, tmp, l4);   dPTv(k - 1, tmp, m4);
    for (int i = 0; i < ns; ++i)
      lam[i] += (h / 6.0) * (l1[i] + 2 * l2[i] + 2 * l3[i] + l4[i]);
    for (int p = 0; p < np; ++p)
      mu[p]  += (h / 6.0) * (m1[p] + 2 * m2[p] + 2 * m3[p] + m4[p]);
  }
  for (int p = 0; p < np; ++p) out[p] = mu[p];
}

// .Call wrapper (registered in init.c) so R can invoke the same C core.
extern "C" SEXP _rxode2_rxAdjointSweep(SEXP tgS, SEXP JS, SEXP dPS, SEXP coverS,
                                       SEXP obsKS, SEXP nsS, SEXP npS) {
  int ns = INTEGER(nsS)[0], np = INTEGER(npS)[0];
  int nt = LENGTH(tgS), nobs = LENGTH(obsKS);
  SEXP outS = PROTECT(Rf_allocVector(REALSXP, np));
  rxode2AdjointSweep(REAL(tgS), REAL(JS), REAL(dPS), REAL(coverS),
                     INTEGER(obsKS), ns, np, nt, nobs, REAL(outS));
  UNPROTECT(1);
  return outS;
}
