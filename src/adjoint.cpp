// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <vector>
#include <algorithm>

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
// Discrete dosing-parameter duals are applied at their grid index during the
// same backward pass, all in the uniform form
//   out[p] += (lambda . dualW[e]) * dualC[e][p]     (dose duals)
//   lambda[cjCmt[e]] *= cjAlpha[e]                    (costate jumps)
// (F: w = amt*e_c, c = dF/dtheta;  modeled lag: w = f(y-)-f(y+), c = d(alag)/dtheta;
//  infusion boundary: w = R*e_c, c = d(tau2)/dtheta;  replace: alpha=0; multiply:
//  alpha=multiplier).  The infusion continuous forcing is folded into dP by the
//  caller.  All jump/dual arrays are column-major: dualW[e + i*nDual],
//  dualC[e + p*nDual].
extern "C" void rxode2AdjointSweep(double *tg, double *J, double *dP,
                                   double *cover, int *obsK, int ns, int np,
                                   int nt, int nobs, double *out,
                                   int nCj, int *cjK, int *cjCmt, double *cjAlpha,
                                   int nDual, int *dualK, double *dualW, double *dualC) {
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
    // dose duals (F / lag / infusion boundary), captured with lambda(tau+)
    for (int e = 0; e < nDual; ++e) if (dualK[e] == k) {
      double s = 0.0;
      for (int i = 0; i < ns; ++i) s += lam[i] * dualW[e + i * nDual];
      for (int p = 0; p < np; ++p) out[p] += s * dualC[e + p * nDual];
    }
    // costate jumps (replace -> alpha 0, multiply -> alpha)
    for (int e = 0; e < nCj; ++e) if (cjK[e] == k) lam[cjCmt[e]] *= cjAlpha[e];
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
  // out already holds the accumulated dose-dual contributions; add the
  // trajectory quadrature (caller zeroes out before the call).
  for (int p = 0; p < np; ++p) out[p] += mu[p];
}

// Full-trajectory adjoint sweep: dy_k(t_i)/dp for EVERY state of interest k,
// EVERY requested output time t_i, and EVERY parameter p -- the adjoint
// counterpart of forward sensitivity's rx__sens_<state>_BY_<param>__ columns.
//
// IMPORTANT: dy_k(t_i)/dp requires the costate reset to e_k AT t_i and
// integrated ALL THE WAY BACK to t0 -- it is NOT a snapshot of a shared
// running accumulator taken mid-sweep (that would conflate contributions
// belonging to a LATER output time's own reset point with an EARLIER one's,
// since the linear costate dynamics do not compose that way for distinct
// reset vectors).  So this performs one INDEPENDENT backward sweep per
// output time (looping o = 0..nOut-1), each running from grid index outK[o]
// down to 0, with one costate/quadrature block per state of interest reset
// at the start of that sweep.  Cost is O(nOut * nStates) sweeps of the grid
// -- the same asymptotic cost as [.rxAdjointSolve()]'s R loop of independent
// solves, just without the per-call R/solver overhead.  (This is why the
// adjoint method's real speed win over forward sensitivity is the SCALAR
// objective gradient in `rxode2AdjointSweep` above, not full-trajectory
// reconstruction -- see the plan's "honest scoping note".)
//
// result[o + s*nOut + p*nOut*nStates] = dy_{stateIdx[s]}(t_{outK[o]}) / dp_p
// (column-major, nOut x nStates x np).  Dose duals / costate jumps apply
// identically to every block (the jump/forcing math does not depend on which
// output state a block is tracking).
extern "C" void rxode2AdjointTrajSweep(double *tg, double *J, double *dP,
                                       int ns, int np, int nt,
                                       int *outK, int nOut,
                                       int *stateIdx, int nStates,
                                       double *result,
                                       int nCj, int *cjK, int *cjCmt, double *cjAlpha,
                                       int nDual, int *dualK, double *dualW, double *dualC) {
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
  std::vector<std::vector<double>> lam(nStates, std::vector<double>(ns, 0.0));
  std::vector<std::vector<double>> mu(nStates, std::vector<double>(np, 0.0));

  for (int o = 0; o < nOut; ++o) {
    int k0 = outK[o];
    for (int s = 0; s < nStates; ++s) {
      std::fill(lam[s].begin(), lam[s].end(), 0.0);
      lam[s][stateIdx[s]] = 1.0;
      std::fill(mu[s].begin(), mu[s].end(), 0.0);
    }
    for (int k = k0; k >= 0; --k) {
      // dose duals -- apply to every block independently (each has its own lambda)
      for (int e = 0; e < nDual; ++e) if (dualK[e] == k) {
        for (int s = 0; s < nStates; ++s) {
          double dot = 0.0;
          for (int i = 0; i < ns; ++i) dot += lam[s][i] * dualW[e + i * nDual];
          for (int p = 0; p < np; ++p) mu[s][p] += dot * dualC[e + p * nDual];
        }
      }
      // costate jumps -- apply to every block
      for (int e = 0; e < nCj; ++e) if (cjK[e] == k) {
        for (int s = 0; s < nStates; ++s) lam[s][cjCmt[e]] *= cjAlpha[e];
      }
      if (k == 0) break;
      double h = tg[k - 1] - tg[k];
      for (int s = 0; s < nStates; ++s) {
        JTv(k, lam[s], l1);        dPTv(k, lam[s], m1);
        for (int i = 0; i < ns; ++i) tmp[i] = lam[s][i] + 0.5 * h * l1[i];
        JTvMid(k, k - 1, tmp, l2); dPTvMid(k, k - 1, tmp, m2);
        for (int i = 0; i < ns; ++i) tmp[i] = lam[s][i] + 0.5 * h * l2[i];
        JTvMid(k, k - 1, tmp, l3); dPTvMid(k, k - 1, tmp, m3);
        for (int i = 0; i < ns; ++i) tmp[i] = lam[s][i] + h * l3[i];
        JTv(k - 1, tmp, l4);      dPTv(k - 1, tmp, m4);
        for (int i = 0; i < ns; ++i)
          lam[s][i] += (h / 6.0) * (l1[i] + 2 * l2[i] + 2 * l3[i] + l4[i]);
        for (int p = 0; p < np; ++p)
          mu[s][p]  += (h / 6.0) * (m1[p] + 2 * m2[p] + 2 * m3[p] + m4[p]);
      }
    }
    for (int s = 0; s < nStates; ++s)
      for (int p = 0; p < np; ++p)
        result[o + s * nOut + p * nOut * (long)nStates] = mu[s][p];
  }
}

// .Call wrapper for rxode2AdjointTrajSweep (registered in init.c).
extern "C" SEXP _rxode2_rxAdjointTrajSweep(SEXP tgS, SEXP JS, SEXP dPS, SEXP nsS,
                                           SEXP npS, SEXP outKS, SEXP stateIdxS,
                                           SEXP cjS, SEXP dualS) {
  int ns = INTEGER(nsS)[0], np = INTEGER(npS)[0];
  int nt = LENGTH(tgS), nOut = LENGTH(outKS), nStates = LENGTH(stateIdxS);
  int nCj = LENGTH(VECTOR_ELT(cjS, 0));
  int nDual = LENGTH(VECTOR_ELT(dualS, 0));
  SEXP resS = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)nOut * nStates * np));
  rxode2AdjointTrajSweep(REAL(tgS), REAL(JS), REAL(dPS), ns, np, nt,
                         INTEGER(outKS), nOut, INTEGER(stateIdxS), nStates, REAL(resS),
                         nCj, INTEGER(VECTOR_ELT(cjS, 0)), INTEGER(VECTOR_ELT(cjS, 1)),
                         REAL(VECTOR_ELT(cjS, 2)),
                         nDual, INTEGER(VECTOR_ELT(dualS, 0)), REAL(VECTOR_ELT(dualS, 1)),
                         REAL(VECTOR_ELT(dualS, 2)));
  UNPROTECT(1);
  return resS;
}

// .Call wrapper (registered in init.c) so R can invoke the same C core.
// cjS = list(K, Cmt, Alpha); dualS = list(K, W, C).
extern "C" SEXP _rxode2_rxAdjointSweep(SEXP tgS, SEXP JS, SEXP dPS, SEXP coverS,
                                       SEXP obsKS, SEXP nsS, SEXP npS,
                                       SEXP cjS, SEXP dualS) {
  int ns = INTEGER(nsS)[0], np = INTEGER(npS)[0];
  int nt = LENGTH(tgS), nobs = LENGTH(obsKS);
  int nCj = LENGTH(VECTOR_ELT(cjS, 0));
  int nDual = LENGTH(VECTOR_ELT(dualS, 0));
  SEXP outS = PROTECT(Rf_allocVector(REALSXP, np));
  double *out = REAL(outS);
  for (int p = 0; p < np; ++p) out[p] = 0.0;
  rxode2AdjointSweep(REAL(tgS), REAL(JS), REAL(dPS), REAL(coverS),
                     INTEGER(obsKS), ns, np, nt, nobs, out,
                     nCj, INTEGER(VECTOR_ELT(cjS, 0)), INTEGER(VECTOR_ELT(cjS, 1)),
                     REAL(VECTOR_ELT(cjS, 2)),
                     nDual, INTEGER(VECTOR_ELT(dualS, 0)), REAL(VECTOR_ELT(dualS, 1)),
                     REAL(VECTOR_ELT(dualS, 2)));
  UNPROTECT(1);
  return outS;
}
