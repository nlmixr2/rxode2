// -*- mode: c++; c-basic-offset: 2; tab-width: 2; indent-tabs-mode: t; -*-
#ifndef R_NO_REMAP
#define R_NO_REMAP
#endif
#define USE_FC_LEN_T
#define STRICT_R_HEADERS
#include <R.h>
#include <Rinternals.h>
#include <cmath>
#include <vector>
#include "../inst/include/rxode2prior.h"

#ifndef M_SQRT1_2
#define M_SQRT1_2 0.70710678118654752440
#endif

// ---------------------------------------------------------------------------
// Pure math: no R/Rcpp symbol below this line touches the R API, so
// rxPriorLogDensityEval() is safe to call from any OpenMP thread
// (nlmixr2/nlmixr2est#929 -- "called in an OpenMP thread safe environment").
// ---------------------------------------------------------------------------

// log(1 - Phi(x)) for x >= 0, via erfc() directly (never via 1 - erf()) so
// it stays accurate deep into the tail instead of rounding to log(0).
static inline double logUpperTailNorm(double x) {
  return std::log(0.5 * std::erfc(x * M_SQRT1_2));
}
// log(Phi(x)), any sign of x, via the same tail-accurate erfc() path.
static inline double logLowerTailNorm(double x) {
  return logUpperTailNorm(-x);
}

// log(F(upper) - F(lower)) for N(mean, sd), stable even when the window
// sits deep in one tail: plain pnorm(upper)-pnorm(lower) can round both
// calls to the same double there (subtracting to exactly 0, so log() of it
// is -Inf and the *density* -- which subtracts that -Inf back off --
// becomes +Inf). Picking whichever tail keeps both log-probabilities away
// from log(1) and subtracting in log space (log1p(-exp(lb-la))) avoids
// that; a window straddling the mean has no cancellation risk either way.
static double logNormCdfDiff(double lower, double upper, double mean, double sd) {
  if (!std::isfinite(lower) && !std::isfinite(upper)) return 0.0;
  if (!std::isfinite(lower)) return logLowerTailNorm((upper - mean) / sd);
  if (!std::isfinite(upper)) return logUpperTailNorm((lower - mean) / sd);
  double zl = (lower - mean) / sd, zu = (upper - mean) / sd;
  if (zu <= 0) {
    double la = logLowerTailNorm(zu), lb = logLowerTailNorm(zl);
    return la + std::log1p(-std::exp(lb - la));
  } else if (zl >= 0) {
    double la = logUpperTailNorm(zl), lb = logUpperTailNorm(zu);
    return la + std::log1p(-std::exp(lb - la));
  }
  double Fu = 0.5 * std::erfc(-zu * M_SQRT1_2);
  double Fl = 0.5 * std::erfc(-zl * M_SQRT1_2);
  return std::log(Fu - Fl);
}

// log(1 - F(z)) for a standard Cauchy, z > 0: 1 - F(z) = atan(1/z)/pi (the
// atan(z)+atan(1/z)=pi/2 identity), which -- unlike 0.5 - atan(z)/pi --
// never cancels near 0.5, so it stays accurate for any z.
static inline double logUpperTailCauchy(double z) {
  return std::log(std::atan(1.0 / z) / M_PI);
}
static inline double logLowerTailCauchy(double z) {
  return logUpperTailCauchy(-z);
}

// log(F(upper) - F(lower)) for Cauchy(location, scale); same tail-selection
// structure as logNormCdfDiff(), even though Cauchy's polynomial tail makes
// the cancellation this guards against far less likely in practice.
static double logCauchyCdfDiff(double lower, double upper, double location, double scale) {
  if (!std::isfinite(lower) && !std::isfinite(upper)) return 0.0;
  if (!std::isfinite(lower)) return logLowerTailCauchy((upper - location) / scale);
  if (!std::isfinite(upper)) return logUpperTailCauchy((lower - location) / scale);
  double zl = (lower - location) / scale, zu = (upper - location) / scale;
  if (zu <= 0) {
    double la = logLowerTailCauchy(zu), lb = logLowerTailCauchy(zl);
    return la + std::log1p(-std::exp(lb - la));
  } else if (zl >= 0) {
    double la = logUpperTailCauchy(zl), lb = logUpperTailCauchy(zu);
    return la + std::log1p(-std::exp(lb - la));
  }
  double Fu = 0.5 + std::atan(zu) / M_PI;
  double Fl = 0.5 + std::atan(zl) / M_PI;
  return std::log(Fu - Fl);
}

// Cholesky factor L (row-major, lower triangular) of a symmetric n x n
// matrix A (row-major), A = L L^T. false when A is not positive definite.
static bool cholesky(const double *A, int n, std::vector<double> &L) {
  L.assign((size_t)n * n, 0.0);
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j <= i; ++j) {
      double s = A[i * n + j];
      for (int k = 0; k < j; ++k) s -= L[i * n + k] * L[j * n + k];
      if (i == j) {
        if (s <= 0.0) return false;
        L[i * n + i] = std::sqrt(s);
      } else {
        L[i * n + j] = s / L[j * n + j];
      }
    }
  }
  return true;
}

static inline double cholLogDet(const std::vector<double> &L, int n) {
  double s = 0.0;
  for (int i = 0; i < n; ++i) s += std::log(L[i * n + i]);
  return 2.0 * s;
}

// Solve A x = b given A's Cholesky factor (forward + back substitution).
static void cholSolve(const std::vector<double> &L, int n, const double *b,
                      std::vector<double> &x) {
  std::vector<double> y(n);
  for (int i = 0; i < n; ++i) {
    double s = b[i];
    for (int k = 0; k < i; ++k) s -= L[i * n + k] * y[k];
    y[i] = s / L[i * n + i];
  }
  x.assign(n, 0.0);
  for (int i = n - 1; i >= 0; --i) {
    double s = y[i];
    for (int k = i + 1; k < n; ++k) s -= L[k * n + i] * x[k]; // (L^T)[i,k] = L[k,i]
    x[i] = s / L[i * n + i];
  }
}

// Dense A^{-1} from its Cholesky factor (needed for the invWishart gradient).
static void cholInverse(const std::vector<double> &L, int n, std::vector<double> &Ainv) {
  Ainv.assign((size_t)n * n, 0.0);
  std::vector<double> e(n, 0.0), col;
  for (int j = 0; j < n; ++j) {
    std::fill(e.begin(), e.end(), 0.0);
    e[j] = 1.0;
    cholSolve(L, n, e.data(), col);
    for (int i = 0; i < n; ++i) Ainv[i * n + j] = col[i];
  }
}

// log-gamma, for a positive argument only, via the standard g=7/n=9 Lanczos
// approximation. std::lgamma() (POSIX libm) writes the sign of Gamma(x) to
// the global `signgam` on every call -- unused here, but a genuine data
// race under concurrent calls from multiple OpenMP threads regardless, so
// it cannot be used inside rxPriorLogDensityEval(). x is always positive
// here (nu/2 - (p-1)/2 > 0, guaranteed by the nu > p-1 check
// R/priorDensity.R makes when the spec is built), so the reflection
// formula for non-positive arguments is not needed.
static double lgammaPositive(double x) {
  static const double g = 7.0;
  static const double c[9] = {
    0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  };
  double xx = x - 1.0;
  double t = xx + g + 0.5;
  double a = c[0];
  for (int i = 1; i < 9; ++i) a += c[i] / (xx + i);
  return 0.5 * std::log(2.0 * M_PI) + (xx + 0.5) * std::log(t) - t + std::log(a);
}

// log of the multivariate gamma function, log Gamma_p(a).
static double logMvGamma(double a, int p) {
  double s = (double)p * (p - 1) / 4.0 * std::log(M_PI);
  for (int i = 1; i <= p; ++i) s += lgammaPositive(a + (1 - i) / 2.0);
  return s;
}

// Gather term k's current value from the caller's theta/omega arrays.
static inline double termValue(const rx_prior_term_t &term, int k,
                               const double *theta, const double *omega, int omegaDim) {
  if (term.thetaIdx[k] > 0) return theta[term.thetaIdx[k] - 1];
  int e = term.etaIdx[k] - 1;
  return omega[(size_t)e * omegaDim + e];
}

// Scatter a scalar gradient contribution for term member k.
static inline void addGrad(const rx_prior_term_t &term, int k, double g,
                           double *gradTheta, double *gradOmega, int omegaDim) {
  if (term.thetaIdx[k] > 0) {
    gradTheta[term.thetaIdx[k] - 1] += g;
  } else {
    int e = term.etaIdx[k] - 1;
    gradOmega[(size_t)e * omegaDim + e] += g;
  }
}

extern "C" double rxPriorLogDensityEval(const rx_prior_spec_t *spec,
                                        const double *theta, int thetaLen,
                                        const double *omega, int omegaDim,
                                        double *gradTheta, double *gradOmega) {
  double val = 0.0;
  (void)thetaLen;
  for (int t = 0; t < spec->nTerms; ++t) {
    const rx_prior_term_t &term = spec->terms[t];
    if (term.type == 0 || term.type == 1) {
      double x = termValue(term, 0, theta, omega, omegaDim);
      double mean = term.mu[0], sd = term.scale[0];
      double z = (x - mean) / sd;
      double v, g;
      if (term.type == 0) {
        v = -std::log(sd) - 0.5 * std::log(2.0 * M_PI) - 0.5 * z * z;
        g = -(x - mean) / (sd * sd);
        if (std::isfinite(term.lower) || std::isfinite(term.upper)) {
          v -= logNormCdfDiff(term.lower, term.upper, mean, sd);
        }
      } else {
        v = -std::log(M_PI) - std::log(sd) - std::log1p(z * z);
        g = -2.0 * (x - mean) / (sd * sd * (1.0 + z * z));
        if (std::isfinite(term.lower) || std::isfinite(term.upper)) {
          v -= logCauchyCdfDiff(term.lower, term.upper, mean, sd);
        }
      }
      val += v;
      addGrad(term, 0, g, gradTheta, gradOmega, omegaDim);
    } else if (term.type == 2) {
      int n = term.n;
      std::vector<double> L;
      if (!cholesky(term.scale, n, L)) { val = -INFINITY; continue; }
      std::vector<double> d(n);
      for (int k = 0; k < n; ++k) d[k] = termValue(term, k, theta, omega, omegaDim) - term.mu[k];
      std::vector<double> s;
      cholSolve(L, n, d.data(), s); // Sigma^{-1} d
      double quad = 0.0;
      for (int k = 0; k < n; ++k) quad += d[k] * s[k];
      val += -0.5 * n * std::log(2.0 * M_PI) - 0.5 * cholLogDet(L, n) - 0.5 * quad;
      for (int k = 0; k < n; ++k) addGrad(term, k, -s[k], gradTheta, gradOmega, omegaDim);
    } else if (term.type == 3 || term.type == 4) {
      int p = term.n;
      std::vector<double> Om((size_t)p * p);
      for (int i = 0; i < p; ++i) {
        int ei = term.etaIdx[i] - 1;
        for (int j = 0; j < p; ++j) {
          int ej = term.etaIdx[j] - 1;
          Om[i * p + j] = omega[(size_t)ei * omegaDim + ej];
        }
      }
      std::vector<double> Lom, Lpsi;
      if (!cholesky(Om.data(), p, Lom) || !cholesky(term.scale, p, Lpsi)) {
        val = -INFINITY; continue;
      }
      double logdetOm = cholLogDet(Lom, p), logdetPsi = cholLogDet(Lpsi, p);
      std::vector<double> Oi;
      cholInverse(Lom, p, Oi); // Omega^{-1}
      double tr = 0.0; // tr(Psi * Omega^{-1})
      for (int i = 0; i < p; ++i)
        for (int j = 0; j < p; ++j) tr += term.scale[i * p + j] * Oi[j * p + i];
      std::vector<double> OiPsi((size_t)p * p, 0.0), OiPsiOi((size_t)p * p, 0.0);
      for (int i = 0; i < p; ++i)
        for (int j = 0; j < p; ++j) {
          double sum = 0.0;
          for (int k = 0; k < p; ++k) sum += Oi[i * p + k] * term.scale[k * p + j];
          OiPsi[i * p + j] = sum;
        }
      for (int i = 0; i < p; ++i)
        for (int j = 0; j < p; ++j) {
          double sum = 0.0;
          for (int k = 0; k < p; ++k) sum += OiPsi[i * p + k] * Oi[k * p + j];
          OiPsiOi[i * p + j] = sum;
        }
      if (term.type == 3) {
        // Textbook inverse-Wishart(nu, Psi) log density (the "general" method).
        double logNC = (term.nu * p / 2.0) * std::log(2.0) + logMvGamma(term.nu / 2.0, p);
        val += (term.nu / 2.0) * logdetPsi - ((term.nu + p + 1) / 2.0) * logdetOm -
          0.5 * tr - logNC;
        for (int i = 0; i < p; ++i) {
          int ei = term.etaIdx[i] - 1;
          for (int j = 0; j < p; ++j) {
            int ej = term.etaIdx[j] - 1;
            double g = -((term.nu + p + 1) / 2.0) * Oi[i * p + j] + 0.5 * OiPsiOi[i * p + j];
            gradOmega[(size_t)ei * omegaDim + ej] += g;
          }
        }
      } else {
        // NONMEM $PRIOR NWPRI, modal (non-BAYES) convention: NONMEM7
        // Technical Guide eq. 1.157/1.159/1.170. term.nu is NONMEM's own
        // "rho" (the invWishart(rho) argument); d_W = rho+n+1 collapses
        // (d_W-n-1) to rho, giving the closed forms below (see the term-type
        // comment in rxode2prior.h for the derivation notes).
        double rho = term.nu;
        double d_W = rho + p + 1.0;
        val += -0.5 * (rho * tr + rho * logdetOm - d_W * logdetPsi - d_W * p * std::log(rho));
        for (int i = 0; i < p; ++i) {
          int ei = term.etaIdx[i] - 1;
          for (int j = 0; j < p; ++j) {
            int ej = term.etaIdx[j] - 1;
            double g = 0.5 * rho * (OiPsiOi[i * p + j] - Oi[i * p + j]);
            gradOmega[(size_t)ei * omegaDim + ej] += g;
          }
        }
      }
    }
  }
  return val;
}

// Frees a spec built by _rxode2_rxPriorBuildSpec(). Internal linkage only
// (called exclusively by the R external-pointer finalizer below, in this
// same translation unit) -- deliberately NOT part of the downstream C API:
// the finalizer is the ONLY thing that may free a spec, since a raw
// pointer obtained by a downstream caller and freed independently would
// race the finalizer into a double free once R's GC runs. A caller that
// wants the spec to outlive one R call must instead keep the R external
// pointer object itself alive (referenced from its own fit state).
static void rxPriorFreeSpec(void *specPtr) {
  if (specPtr == NULL) return;
  rx_prior_spec_t *spec = (rx_prior_spec_t *)specPtr;
  for (int t = 0; t < spec->nTerms; ++t) {
    delete[] spec->terms[t].thetaIdx;
    delete[] spec->terms[t].etaIdx;
    delete[] spec->terms[t].mu;
    delete[] spec->terms[t].scale;
  }
  delete[] spec->terms;
  delete spec;
}

// ---------------------------------------------------------------------------
// R-facing glue below this line: builds a spec from the flat R
// representation R/priorDensity.R assembles (one-time, at fit setup, on the
// main R thread -- never inside the OpenMP-parallel evaluation the pure
// functions above are for) and wraps rxPriorLogDensityEval() for direct
// use from R (rxPriorLogDensity()'s R-level convenience shim).
// ---------------------------------------------------------------------------

static void _rxode2_rxPriorFreeSpecFinalizer(SEXP specSEXP) {
  rxPriorFreeSpec(R_ExternalPtrAddr(specSEXP));
  R_ClearExternalPtr(specSEXP);
}

// specList: type, n (integer, one per term), thetaIdx, etaIdx (integer,
// concatenated across terms, length sum(n)), mu, scale (numeric,
// concatenated; scale is n*n per term, row-major, back to back), lower,
// upper, nu (numeric, one per term).
extern "C" SEXP _rxode2_rxPriorBuildSpec(SEXP specList) {
  SEXP typeS = VECTOR_ELT(specList, 0), nS = VECTOR_ELT(specList, 1),
    thetaIdxS = VECTOR_ELT(specList, 2), etaIdxS = VECTOR_ELT(specList, 3),
    muS = VECTOR_ELT(specList, 4), scaleS = VECTOR_ELT(specList, 5),
    lowerS = VECTOR_ELT(specList, 6), upperS = VECTOR_ELT(specList, 7),
    nuS = VECTOR_ELT(specList, 8);
  int nTerms = LENGTH(typeS);
  rx_prior_spec_t *spec = new rx_prior_spec_t;
  spec->nTerms = nTerms;
  spec->terms = new rx_prior_term_t[nTerms];
  int memberOff = 0, scaleOff = 0;
  for (int t = 0; t < nTerms; ++t) {
    rx_prior_term_t &term = spec->terms[t];
    term.type = INTEGER(typeS)[t];
    term.n = INTEGER(nS)[t];
    term.thetaIdx = new int[term.n];
    term.etaIdx = new int[term.n];
    term.mu = new double[term.n];
    for (int k = 0; k < term.n; ++k) {
      term.thetaIdx[k] = INTEGER(thetaIdxS)[memberOff + k];
      term.etaIdx[k] = INTEGER(etaIdxS)[memberOff + k];
      term.mu[k] = REAL(muS)[memberOff + k];
    }
    int nScale = term.n * term.n;
    term.scale = new double[nScale];
    for (int k = 0; k < nScale; ++k) term.scale[k] = REAL(scaleS)[scaleOff + k];
    term.lower = REAL(lowerS)[t];
    term.upper = REAL(upperS)[t];
    term.nu = REAL(nuS)[t];
    memberOff += term.n;
    scaleOff += nScale;
  }
  SEXP ret = PROTECT(R_MakeExternalPtr(spec, R_NilValue, R_NilValue));
  R_RegisterCFinalizerEx(ret, _rxode2_rxPriorFreeSpecFinalizer, TRUE);
  UNPROTECT(1);
  return ret;
}

// specSEXP: external pointer from _rxode2_rxPriorBuildSpec(). thetaS: full
// theta vector (length thetaLen). omegaS: full omega matrix (omegaDim x
// omegaDim). Returns list(value, gradTheta, gradOmega).
extern "C" SEXP _rxode2_rxPriorLogDensity(SEXP specSEXP, SEXP thetaS, SEXP omegaS) {
  rx_prior_spec_t *spec = (rx_prior_spec_t *)R_ExternalPtrAddr(specSEXP);
  int thetaLen = LENGTH(thetaS);
  int omegaDim = (int)std::sqrt((double)LENGTH(omegaS));
  SEXP gradThetaS = PROTECT(Rf_allocVector(REALSXP, thetaLen));
  SEXP gradOmegaS = PROTECT(Rf_allocVector(REALSXP, (R_xlen_t)omegaDim * omegaDim));
  double *gradTheta = REAL(gradThetaS), *gradOmega = REAL(gradOmegaS);
  for (int i = 0; i < thetaLen; ++i) gradTheta[i] = 0.0;
  for (int i = 0; i < omegaDim * omegaDim; ++i) gradOmega[i] = 0.0;
  double value = rxPriorLogDensityEval(spec, REAL(thetaS), thetaLen,
                                       REAL(omegaS), omegaDim, gradTheta, gradOmega);
  SEXP valueS = PROTECT(Rf_allocVector(REALSXP, 1));
  REAL(valueS)[0] = value;
  SEXP ret = PROTECT(Rf_allocVector(VECSXP, 3));
  SET_VECTOR_ELT(ret, 0, valueS);
  SET_VECTOR_ELT(ret, 1, gradThetaS);
  SET_VECTOR_ELT(ret, 2, gradOmegaS);
  UNPROTECT(4);
  return ret;
}
